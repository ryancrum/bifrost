%%%-------------------------------------------------------------------
%%% File    : bifrost.erl
%%% Author  : Ryan Crum <ryan@ryancrum.org>
%%% Description : Pluggable FTP Server gen_server
%%%-------------------------------------------------------------------

-module(bifrost).

-behaviour(gen_server).
-include("bifrost.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/2, establish_control_connection/2, await_connections/2, supervise_connections/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(FEATURES, [ "UTF8" ]).

default(Expr, Default) ->
    case Expr of
        undefined ->
            Default;
        _ ->
            Expr
    end.

-spec ucs2_to_utf8(string()) -> string().
ucs2_to_utf8(String) ->
    erlang:binary_to_list(unicode:characters_to_binary(String, utf8)).

start_link(HookModule, Opts) ->
    gen_server:start_link(?MODULE, [HookModule, Opts], []).

%% gen_server callbacks implementation
init([HookModule, Opts]) ->
    Port = default(proplists:get_value(port, Opts), 21),
    Ssl = default(proplists:get_value(ssl, Opts), false),
    SslKey = proplists:get_value(ssl_key, Opts),
    SslCert = proplists:get_value(ssl_cert, Opts),
    CaSslCert = proplists:get_value(ca_ssl_cert, Opts),
    UTF8 = proplists:get_value(utf8, Opts),
    case listen_socket(Port, [{active, false}, {reuseaddr, true}, list]) of
        {ok, Listen} ->
            IpAddress = default(proplists:get_value(ip_address, Opts), get_socket_addr(Listen)),
            InitialState = #connection_state{module=HookModule,
                                             ip_address=IpAddress,
                                             ssl_allowed=Ssl,
                                             ssl_key=SslKey,
                                             ssl_cert=SslCert,
                                             ssl_ca_cert=CaSslCert,
                                             utf8=UTF8},
            Supervisor = proc_lib:spawn_link(?MODULE,
                                             supervise_connections,
                                             [HookModule:init(InitialState, Opts)]),
            proc_lib:spawn_link(?MODULE,
                                await_connections,
                                [Listen, Supervisor]),
            {ok, {listen_socket, Listen}};
        {error, Error} ->
            {stop, Error}
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, {listen_socket, Socket}) ->
    gen_tcp:close(Socket);
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_socket_addr(Socket) ->
    case inet:sockname(Socket) of
        {ok, {Addr, _}} ->
            Addr
    end.

listen_socket(Port, TcpOpts) ->
    gen_tcp:listen(Port, TcpOpts).

await_connections(Listen, Supervisor) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            Supervisor ! {new_connection, self(), Socket},
            receive
                {ack, Worker} ->
                    %% ssl:ssl_accept/2 will return {error, not_owner} otherwise
                    ok = gen_tcp:controlling_process(Socket, Worker)
            end;
        _Error ->
            exit(bad_accept)
    end,
    await_connections(Listen, Supervisor).

supervise_connections(InitialState) ->
    process_flag(trap_exit, true),
    receive
        {new_connection, Acceptor, Socket} ->
            Worker = proc_lib:spawn_link(?MODULE,
                                         establish_control_connection,
                                         [Socket, InitialState]),
            Acceptor ! {ack, Worker};
        {'EXIT', _Pid, normal} -> % not a crash
            ok;
        {'EXIT', _Pid, shutdown} -> % manual termination, not a crash
            ok;
        {'EXIT', Pid, Info} ->
            error_logger:error_msg("Control connection ~p crashed: ~p~n", [Pid, Info]);
        _ ->
            ok
    end,
    supervise_connections(InitialState).

establish_control_connection(Socket, InitialState) ->
    respond({gen_tcp, Socket}, 220, "FTP Server Ready"),
    IpAddress = case InitialState#connection_state.ip_address of
                    undefined -> get_socket_addr(Socket);
                    {0, 0, 0, 0} -> get_socket_addr(Socket);
                    {0, 0, 0, 0, 0, 0} -> get_socket_addr(Socket);
                    Ip -> Ip
                end,
    control_loop(none,
                 {gen_tcp, Socket},
                 InitialState#connection_state{control_socket=Socket, ip_address=IpAddress}).

control_loop(HookPid, {SocketMod, RawSocket} = Socket, State) ->
    case SocketMod:recv(RawSocket, 0) of
        {ok, Input} ->
            {Command, Arg} = parse_input(Input),
            case ftp_command(Socket, State, Command, Arg) of
                {ok, NewState} ->
                    if is_pid(HookPid) ->
                            HookPid ! {new_state, self(), NewState},
                            receive
                                {ack, HookPid} ->
                                    control_loop(HookPid, Socket, NewState);
                                {done, HookPid} ->
                                    {error, closed}
                            end;
                       true ->
                            control_loop(HookPid, Socket, NewState)
                    end;
                {new_socket, NewState, NewSock} ->
                    control_loop(HookPid, NewSock, NewState);
                {error, timeout} ->
                    respond(Socket, 412, "Timed out. Closing control connection."),
                    SocketMod:close(RawSocket),
                    {error, timeout};
                {error, closed} ->
                    {error, closed};
                quit ->
                    SocketMod:close(RawSocket),
                    {ok, quit}
            end;
        {error, _Reason} ->
            error_logger:warning_report({bifrost, connection_terminated})
    end.

respond(Socket, ResponseCode) ->
    respond(Socket, ResponseCode, response_code_string(ResponseCode)).

respond({SocketMod, Socket}, ResponseCode, Message) ->
    Line = integer_to_list(ResponseCode) ++ " " ++ ucs2_to_utf8(Message) ++ "\r\n",
    SocketMod:send(Socket, Line).

respond_raw({SocketMod, Socket}, Line) ->
    SocketMod:send(Socket, ucs2_to_utf8(Line) ++ "\r\n").

ssl_options(State) ->
    [{keyfile, State#connection_state.ssl_key},
     {certfile, State#connection_state.ssl_cert},
     {cacertfile, State#connection_state.ssl_ca_cert}].

data_connection(ControlSocket, State) ->
    respond(ControlSocket, 150),
    case establish_data_connection(State) of
        {ok, DataSocket} ->
            case State#connection_state.protection_mode of
                clear ->
                    {gen_tcp, DataSocket};
                private ->
                    case ssl:ssl_accept(DataSocket,
                                        ssl_options(State)) of
                        {ok, SslSocket} ->
                            {ssl, SslSocket};
                        E ->
                            respond(ControlSocket, 425),
                            throw({error, E})
                    end
            end;
        {error, Error} ->
            respond(ControlSocket, 425),
            throw(Error)
    end.


%% passive -- accepts an inbound connection
establish_data_connection(#connection_state{pasv_listen={passive, Listen, _}}) ->
    gen_tcp:accept(Listen);

%% active -- establishes an outbound connection
establish_data_connection(#connection_state{data_port={active, Addr, Port}}) ->
    gen_tcp:connect(Addr, Port, [{active, false}, binary]).

pasv_connection(ControlSocket, State) ->
    case State#connection_state.pasv_listen of
        {passive, PasvListen, _} ->
                                                % We should only have one passive socket open at a time, so close the current one
                                                % and open a new one.
            gen_tcp:close(PasvListen),
            pasv_connection(ControlSocket, State#connection_state{pasv_listen=undefined});
        undefined ->
            case listen_socket(0, [{active, false}, binary]) of
                {ok, Listen} ->
                    {ok, {_, Port}} = inet:sockname(Listen),
                    Ip = State#connection_state.ip_address,
                    PasvSocketInfo = {passive,
                                      Listen,
                                      {Ip, Port}},
                    {P1,P2} = format_port(Port),
                    {S1,S2,S3,S4} = Ip,
                    respond(ControlSocket,
                            227,
                            lists:flatten(
                              io_lib:format("Entering Passive Mode (~p,~p,~p,~p,~p,~p)",
                                            [S1,S2,S3,S4,P1,P2]))),

                    {ok,
                     State#connection_state{pasv_listen=PasvSocketInfo}};
                {error, _} ->
                    respond(ControlSocket, 425),
                    {ok, State}
            end
    end.

%% FTP COMMANDS

ftp_command(Socket, State, Command, RawArg) ->
    Mod = State#connection_state.module,
    case unicode:characters_to_list(erlang:list_to_binary(RawArg), utf8) of
        { error, List, _RestData } ->
            error_logger:warning_report({bifrost, invalid_utf8, List}),
            respond(Socket, 501),
            {ok, State};
        { incomplete, List, _Binary } ->
            error_logger:warning_report({bifrost, incomplete_utf8, List}),
            respond(Socket, 501),
            {ok, State};
        Arg ->
            ftp_command(Mod, Socket, State, Command, Arg)
    end.

ftp_command(Mod, Socket, State, quit, _) ->
    respond(Socket, 200, "Goodbye."),
    Mod:disconnect(State),
    quit;

ftp_command(_, Socket, State, pasv, _) ->
    pasv_connection(Socket, State);

ftp_command(_, {_, RawSocket} = Socket, State, auth, Arg) ->
    if State#connection_state.ssl_allowed =:= false ->
            respond(Socket, 500),
            {ok, State};
       true ->
            case string:to_lower(Arg) of
                "tls" ->
                    respond(Socket, 234, "Command okay."),
                    case ssl:ssl_accept(RawSocket,
                                        ssl_options(State)) of
                        {ok, SslSocket} ->
                            {new_socket,
                             State#connection_state{ssl_socket=SslSocket},
                             {ssl, SslSocket}};
                        _ ->
                            respond(Socket, 500),
                            {ok, State}
                    end;
                _ ->
                    respond(Socket, 502, "Unsupported security extension."),
                    {ok, State}
            end
    end;

ftp_command(_, Socket, State, prot, Arg) ->
    ProtMode = case string:to_lower(Arg) of
                   "c" -> clear;
                   _ -> private
               end,
    respond(Socket, 200),
    {ok, State#connection_state{protection_mode=ProtMode}};

ftp_command(_, Socket, State, pbsz, "0") ->
    respond(Socket, 200),
    {ok, State};

ftp_command(_, Socket, State, user, Arg) ->
    respond(Socket, 331),
    {ok, State#connection_state{user_name=Arg}};

ftp_command(_, Socket, State, port, Arg) ->
    case parse_address(Arg) of
        {ok, {Addr, Port}} ->
            respond(Socket, 200),
            {ok, State#connection_state{data_port = {active, Addr, Port}}};
        _ ->
            respond(Socket, 452, "Error parsing address.")
    end;

ftp_command(Mod, Socket, State, pass, Arg) ->
    case Mod:login(State, State#connection_state.user_name, Arg) of
        {true, NewState} ->
            respond(Socket, 230),
            {ok, NewState#connection_state{authenticated_state=authenticated}};
        _ ->
            respond(Socket, 530, "Login incorrect."),
            quit
    end;

%% based of rfc2389
ftp_command(_Mod, Socket, State, feat, _Arg) ->
    respond_raw(Socket, "211- Extensions supported:"),
    lists:map(	fun	({Feature, FeatureParams}) -> respond_raw(Socket, " " ++ Feature ++ " " ++ FeatureParams);
                      (Feature) -> respond_raw(Socket, " " ++ Feature)
                end,
                ?FEATURES),
    respond(Socket, 211, "End"),
    {ok, State};

%% ^^^ from this point down every command requires authentication ^^^

ftp_command(_, Socket, State=#connection_state{authenticated_state=unauthenticated}, _, _) ->
    respond(Socket, 530),
    {ok, State};

ftp_command(_, Socket, State, rein, _) ->
    respond(Socket, 200),
    {ok,
     State#connection_state{user_name=none,authenticated_state=unauthenticated}};

ftp_command(Mod, Socket, State, pwd, _) ->
    respond(Socket, 257, "\"" ++ Mod:current_directory(State) ++ "\""),
    {ok, State};

ftp_command(Mod, Socket, State, cdup, _) ->
    ftp_command(Mod, Socket, State, cwd, "..");

ftp_command(Mod, Socket, State, cwd, Arg) ->
    case Mod:change_directory(State, Arg) of
        {ok, NewState} ->
            respond(Socket, 250, "directory changed to \"" ++ Mod:current_directory(NewState) ++ "\""),
            {ok, NewState};
        {error, _} ->
            respond(Socket, 550, "Unable to change directory"),
            {ok, State}
    end;

ftp_command(Mod, Socket, State, mkd, Arg) ->
    case Mod:make_directory(State, Arg) of
        {ok, NewState} ->
            respond(Socket, 250, "\"" ++ Arg ++ "\" directory created."),
            {ok, NewState};
        {error, _} ->
            respond(Socket, 550, "Unable to create directory"),
            {ok, State}
    end;

ftp_command(Mod, Socket, State, nlst, Arg) ->
    case Mod:list_files(State, Arg) of
        {error, NewState} ->
            respond(Socket, 451),
            {ok, NewState};
        Files ->
            DataSocket = data_connection(Socket, State),
            list_file_names_to_socket(DataSocket, Files),
            respond(Socket, 226),
            bf_close(DataSocket),
            {ok, State}
    end;

ftp_command(Mod, Socket, State, list, Arg) ->
    case Mod:list_files(State, Arg) of
        {error, _} ->
            respond(Socket, 451),
            {ok, State};
        Files ->
            DataSocket = data_connection(Socket, State),
            list_files_to_socket(DataSocket, Files),
            respond(Socket, 226),
            bf_close(DataSocket),
            {ok, State}
    end;

ftp_command(Mod, Socket, State, rmd, Arg) ->
    case Mod:remove_directory(State, Arg) of
        {ok, NewState} ->
            respond(Socket, 200),
            {ok, NewState};
        {error, _} ->
            respond(Socket, 550),
            {ok, State}
    end;

ftp_command(_, Socket, State, syst, _) ->
    respond(Socket, 215, "UNIX Type: L8"),
    {ok, State};

ftp_command(Mod, Socket, State, dele, Arg) ->
    case Mod:remove_file(State, Arg) of
        {ok, NewState} ->
            respond(Socket, 200),
            {ok, NewState};
        {error, _} ->
            respond(Socket, 450),
            {ok, State}
    end;

ftp_command(Mod, Socket, State, stor, Arg) ->
    DataSocket = data_connection(Socket, State),
    Fun = fun() ->
                  case bf_recv(DataSocket) of
                      {ok, Data} ->
                          {ok, Data, size(Data)};
                      {error, closed} ->
                          done
                  end
          end,
    RetState = case Mod:put_file(State, Arg, write, Fun) of
                   {ok, NewState} ->
                       respond(Socket, 226),
                       NewState;
                   {error, Info} ->
                       respond(Socket, 451, io_lib:format("Error ~p when storing a file.", [Info])),
                       State
               end,
    bf_close(DataSocket),
    {ok, RetState};

ftp_command(_, Socket, State, type, Arg) ->
    case Arg of
        "I" ->
            respond(Socket, 200);
        "A" ->
            respond(Socket, 200);
        _->
            respond(Socket, 501, "Only TYPE I or TYPE A may be used.")
    end,
    {ok, State};

ftp_command(Mod, Socket, State, site, Arg) ->
    [Command | Sargs] = string:tokens(Arg, " "),
    case Mod:site_command(State, list_to_atom(string:to_lower(Command)), string:join(Sargs, " ")) of
        {ok, NewState} ->
            respond(Socket, 200),
            {ok, NewState};
        {error, not_found} ->
            respond(Socket, 500),
            {ok, State};
        {error, _} ->
            respond(Socket, 501, "Error completing command."),
            {ok, State}
    end;

ftp_command(Mod, Socket, State, site_help, _) ->
    case Mod:site_help(State) of
        {ok, []} ->
            respond(Socket, 500);
        {error, _} ->
            respond(Socket, 500);
        {ok, Commands} ->
            respond_raw(Socket, "214-The following commands are recognized"),
            lists:map(fun({CmdName, Descr}) ->
                              respond_raw(Socket, CmdName ++ " : " ++ Descr)
                      end,
                      Commands),
            respond(Socket, 214, "Help OK")
    end,
    {ok, State};

ftp_command(Mod, Socket, State, help, Arg) ->
    LowerArg =  string:to_lower(Arg),
    case LowerArg of
        "site" ->
            ftp_command(Mod, Socket, State, site_help, undefined);
        _ ->
            respond(Socket, 500),
            {ok, State}
    end;

ftp_command(Mod, Socket, State, retr, Arg) ->
    try
        case Mod:get_file(State, Arg) of
            {ok, Fun} ->
                DataSocket = data_connection(Socket, State),
                {ok, NewState} = write_fun(DataSocket, Fun),
                respond(Socket, 226),
                bf_close(DataSocket),
                {ok, NewState};
            error ->
                respond(Socket, 550),
                {ok, State}
        end
        catch
            _ ->
                                                   respond(Socket, 550),
                                                   {ok, State}
                                           end;

ftp_command(Mod, Socket, State, mdtm, Arg) ->
    case Mod:file_info(State, Arg) of
        {ok, FileInfo} ->
            respond(Socket,
                    213,
                    format_mdtm_date(FileInfo#file_info.mtime));
        _ ->
            respond(Socket, 550)
    end,
    {ok, State};

ftp_command(_, Socket, State, rnfr, Arg) ->
    respond(Socket, 350, "Ready for RNTO."),
    {ok, State#connection_state{rnfr=Arg}};

ftp_command(Mod, Socket, State, rnto, Arg) ->
    case State#connection_state.rnfr of
        undefined ->
            respond(Socket, 503, "RNFR not specified."),
            {ok, State};
        Rnfr ->
            case Mod:rename_file(State, Rnfr, Arg) of
                {error, _} ->
                    respond(Socket, 550),
                    {ok, State};
                {ok, NewState} ->
                    respond(Socket, 250, "Rename successful."),
                    {ok, NewState#connection_state{rnfr=undefined}}
            end
    end;

ftp_command(Mod, Socket, State, xcwd, Arg) ->
    ftp_command(Mod, Socket, State, cwd, Arg);

ftp_command(Mod, Socket, State, xcup, Arg) ->
    ftp_command(Mod, Socket, State, cdup, Arg);

ftp_command(Mod, Socket, State, xmkd, Arg) ->
    ftp_command(Mod, Socket, State, mkd, Arg);

ftp_command(Mod, Socket, State, xpwd, Arg) ->
    ftp_command(Mod, Socket, State, pwd, Arg);

ftp_command(Mod, Socket, State, xrmd, Arg) ->
    ftp_command(Mod, Socket, State, rmd, Arg);

ftp_command(_Mod, Socket, State, feat, _Arg) ->
    respond_raw(Socket, "211-Features"),
    case State#connection_state.utf8 of
        true ->
            respond_raw(Socket, " UTF8");
        _ ->
            ok
    end,
    respond(Socket, 211, "End"),
    {ok, State};

ftp_command(_Mod, Socket, State, opts, Arg) ->
    case string:to_upper(Arg) of
        "UTF8 ON" when State#connection_state.utf8 =:= true ->
            respond(Socket, 200, "Accepted");
        _ ->
            respond(Socket, 501)
    end,
    {ok, State};

ftp_command(_Mod, Socket, State, size, _Arg) ->
    respond(Socket, 550),
    {ok, State};

ftp_command(_, Socket, State, Command, _Arg) ->
    error_logger:warning_report({bifrost, unrecognized_command, Command}),
    respond(Socket, 500),
    {ok, State}.

write_fun(Socket, Fun) ->
    case Fun(1024) of
        {ok, Bytes, NextFun} ->
            bf_send(Socket, Bytes),
            write_fun(Socket, NextFun);
        {done, NewState} ->
            {ok, NewState}
    end.

strip_newlines(S) ->
    lists:foldr(fun(C, A) ->
                        string:strip(A, right, C) end,
                S,
                "\r\n").

parse_input(Input) ->
    Tokens = string:tokens(Input, " "),
    [Command | Args] = lists:map(fun(S) -> strip_newlines(S) end,
                                 Tokens),
    {list_to_atom(string:to_lower(Command)), string:join(Args, " ")}.

list_files_to_socket(DataSocket, Files) ->
    lists:map(fun(Info) ->
                      bf_send(DataSocket,
                              ucs2_to_utf8(file_info_to_string(Info)) ++ "\r\n") end,
              Files),
    ok.

list_file_names_to_socket(DataSocket, Files) ->
    lists:map(fun(Info) ->
                      bf_send(DataSocket, ucs2_to_utf8(Info#file_info.name)++"\r\n") end,
              Files),
    ok.

bf_send({SockMod, Socket}, Data) ->
    SockMod:send(Socket, Data).

bf_close({SockMod, Socket}) ->
    SockMod:close(Socket).

bf_recv({SockMod, Socket}) ->
    SockMod:recv(Socket, 0).

%% Adapted from jungerl/ftpd.erl
response_code_string(110) -> "MARK yyyy = mmmm";
response_code_string(120) -> "Service ready in nnn minutes.";
response_code_string(125) -> "Data connection alredy open; transfere starting.";
response_code_string(150) -> "File status okay; about to open data connection.";
response_code_string(200) -> "Command okay.";
response_code_string(202) -> "Command not implemented, superfluous at this site.";
response_code_string(211) -> "System status, or system help reply.";
response_code_string(212) -> "Directory status.";
response_code_string(213) -> "File status.";
response_code_string(214) -> "Help message.";
response_code_string(215) -> "UNIX system type";
response_code_string(220) -> "Service ready for user.";
response_code_string(221) -> "Service closing control connection.";
response_code_string(225) -> "Data connection open; no transfere in progress";
response_code_string(226) -> "Closing data connection.";
response_code_string(227) -> "Entering Passive Mode (h1,h2,h3,h4,p1,p2).";
response_code_string(230) -> "User logged in, proceed.";
response_code_string(250) -> "Requested file action okay, completed.";
response_code_string(257) -> "PATHNAME created.";
response_code_string(331) -> "User name okay, need password.";
response_code_string(332) -> "Need account for login.";
response_code_string(350) -> "Requested file action pending further information.";
response_code_string(421) -> "Service not available, closing control connection.";
response_code_string(425) -> "Can't open data connection.";
response_code_string(426) -> "Connection closed; transfere aborted.";
response_code_string(450) -> "Requested file action not taken.";
response_code_string(451) -> "Requested action not taken: local error in processing.";
response_code_string(452) -> "Requested action not taken.";
response_code_string(500) -> "Syntax error, command unrecognized.";
response_code_string(501) -> "Syntax error in parameters or arguments.";
response_code_string(502) -> "Command not implemented.";
response_code_string(503) -> "Bad sequence of commands.";
response_code_string(504) -> "Command not implemented for that parameter.";
response_code_string(530) -> "Not logged in.";
response_code_string(532) -> "Need account for storing files.";
response_code_string(550) -> "Requested action not taken.";
response_code_string(551) -> "Requested action aborted: page type unkown.";
response_code_string(552) -> "Requested file action aborted.";
response_code_string(553) -> "Requested action not taken.";
response_code_string(_) -> "N/A".

%% Taken from jungerl/ftpd

file_info_to_string(Info) ->
    format_type(Info#file_info.type) ++
        format_access(Info#file_info.mode) ++ " " ++
        format_number(type_num(Info#file_info.type), 2, $ ) ++ " " ++
        format_number(Info#file_info.uid,5,$ ) ++ " " ++
        format_number(Info#file_info.gid,5,$ ) ++ " "  ++
        format_number(Info#file_info.size,8,$ ) ++ " " ++
        format_date(Info#file_info.mtime) ++ " " ++
        Info#file_info.name.

format_mdtm_date({{Year, Month, Day}, {Hours, Mins, Secs}}) ->
    lists:flatten(io_lib:format("~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B",
                                [Year, Month, Day, Hours, Mins, erlang:trunc(Secs)])).

format_date({Date, Time}) ->
    {Year, Month, Day} = Date,
    {Hours, Min, _} = Time,
    {LDate, _LTime} = calendar:local_time(),
    {LYear, _, _} = LDate,
    format_month_day(Month, Day) ++
        if LYear > Year ->
                format_year(Year);
           true ->
                format_time(Hours, Min)
        end.

format_month_day(Month, Day) ->
    io_lib:format("~s ~2.2w", [month(Month), Day]).

format_year(Year) ->
    io_lib:format(" ~5.5w", [Year]).

format_time(Hours, Min) ->
    io_lib:format(" ~2.2.0w:~2.2.0w", [Hours, Min]).

format_type(file) -> "-";
format_type(dir) -> "d";
format_type(_) -> "?".

type_num(file) ->
    1;
type_num(dir) ->
    4;
type_num(_) ->
    0.

format_access(Mode) ->
    format_rwx(Mode bsr 6) ++ format_rwx(Mode bsr 3) ++ format_rwx(Mode).

format_rwx(Mode) ->
    [if Mode band 4 == 0 -> $-; true -> $r end,
     if Mode band 2 == 0 -> $-; true -> $w end,
     if Mode band 1 == 0 -> $-; true -> $x end].

format_number(X, N, LeftPad) when X >= 0 ->
    Ls = integer_to_list(X),
    Len = length(Ls),
    if Len >= N -> Ls;
       true ->
            lists:duplicate(N - Len, LeftPad) ++ Ls
    end.

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%% parse address on form:
%% d1,d2,d3,d4,p1,p2  => { {d1,d2,d3,d4}, port} -- ipv4
%% h1,h2,...,h32,p1,p2 => {{n1,n2,..,n8}, port} -- ipv6
%% Taken from jungerl/ftpd
parse_address(Str) ->
    paddr(Str, 0, []).

paddr([X|Xs],N,Acc) when X >= $0, X =< $9 -> paddr(Xs, N*10+(X-$0), Acc);
paddr([X|Xs],_N,Acc) when X >= $A, X =< $F -> paddr(Xs,(X-$A)+10, Acc);
paddr([X|Xs],_N,Acc) when X >= $a, X =< $f -> paddr(Xs, (X-$a)+10, Acc);
paddr([$,,$,|_Xs], _N, _Acc) -> error;
paddr([$,|Xs], N, Acc) -> paddr(Xs, 0, [N|Acc]);
paddr([],P2,[P1,D4,D3,D2,D1]) -> {ok,{{D1,D2,D3,D4}, P1*256+P2}};
paddr([],P2,[P1|As]) when length(As) == 32 ->
    case addr6(As,[]) of
        {ok,Addr} -> {ok, {Addr, P1*256+P2}};
        error -> error
    end;
paddr(_, _, _) -> error.

addr6([H4,H3,H2,H1|Addr],Acc) when H4<16,H3<16,H2<16,H1<16 ->
    addr6(Addr, [H4 + H3*16 + H2*256 + H1*4096 |Acc]);
addr6([], Acc) -> {ok, list_to_tuple(Acc)};
addr6(_, _) -> error.

format_port(PortNumber) ->
    [A,B] = binary_to_list(<<PortNumber:16>>),
    {A, B}.

-ifdef(TEST).

%% EUNIT TESTS %%

%% Testing Utility Functions

setup() ->
    meck:new(gen_tcp, [unstick]),
    meck:new(inet, [unstick, passthrough]),
    meck:new(fake_server, [non_strict]).

execute(ListenerPid) ->
    receive
        go ->
            control_loop(ListenerPid,
                         {gen_tcp, socket},
                         #connection_state{module=fake_server,ip_address={127,0,0,1}}),
            meck:validate(fake_server),
            meck:validate(gen_tcp),
            meck:unload(fake_server),
            meck:unload(inet),
            meck:unload(gen_tcp)
    end.

-define(dataSocketTest(TEST_NAME),
        TEST_NAME() ->
               TEST_NAME(active),
               TEST_NAME(passive)).

%% Awkward, monadic interaction sequence testing
script_dialog([]) ->
    meck:expect(gen_tcp,
                recv,
                fun(_, _) -> {error, closed} end);
script_dialog([{Request, Response} | Rest]) ->
    meck:expect(gen_tcp,
                recv,
                fun(Socket, _) ->
                        script_dialog([{resp, Socket, Response}] ++ Rest),
                        {ok, Request}
                end);
script_dialog([{resp, Socket, Response} | Rest]) ->
    meck:expect(gen_tcp,
                send,
                fun(S, C) ->
                        ?assertEqual(Socket, S),
                        ?assertEqual(Response, unicode:characters_to_list(C)),
                        script_dialog(Rest)
                end);
script_dialog([{resp_bin, Socket, Response} | Rest]) ->
    meck:expect(gen_tcp,
                send,
                fun(S, C) ->
                        ?assertEqual(Socket, S),
                        ?assertEqual(Response, C),
                        script_dialog(Rest)
                end);
script_dialog([{req, Socket, Request} | Rest]) ->
    meck:expect(gen_tcp,
                recv,
                fun(S, _) ->
                        ?assertEqual(S, Socket),
                        script_dialog(Rest),
                        {ok, Request}
                end).

%% executes the next step in the test script
step(Pid) ->
    Pid ! {ack, self()},
    receive
        {new_state, _, State} ->
            State;
        _ ->
            ?assert(fail)
    end.

%% stops the script
finish(Pid) ->
    Pid ! {done, self()}.


%% Unit Tests

strip_newlines_test() ->
    "testing 1 2 3" = strip_newlines("testing 1 2 3\r\n"),
    "testing again" = strip_newlines("testing again").

parse_input_test() ->
    {test, "1 2 3"} = parse_input("TEST 1 2 3"),
    {test, ""} = parse_input("Test\r\n"),
    {test, "awesome"} = parse_input("Test awesome\r\n").

format_access_test() ->
    "rwxrwxrwx" = format_access(8#0777),
    "rw-rw-rw-" = format_access(8#0666),
    "r--rwxrwx" = format_access(8#0477),
    "---------" = format_access(0).

format_number_test() ->
    "005" = format_number(5, 3, $0),
    "500" = format_number(500, 2, $0),
    "500" = format_number(500, 3, $0).

parse_address_test() ->
    {ok, {{127,0,0,1}, 2000}} = parse_address("127,0,0,1,7,208"),
    error = parse_address("MEAT MEAT").


%% Functional/Integration Tests

authenticate_state(State) ->
    State#connection_state{authenticated_state=authenticated}.

login_test_user(SocketPid) ->
    login_test_user(SocketPid, []).

login_test_user(SocketPid, Script) ->
    script_dialog([{req, socket, "USER meat"},
                   {resp, socket, "331 User name okay, need password.\r\n"},
                   {req, socket, "PASS meatmeat"},
                   {resp, socket, "230 User logged in, proceed.\r\n"}] ++ Script),
    SocketPid ! go,
    receive
        {new_state, _, _} ->
            ok
    end,
    SocketPid ! {ack, self()},

    meck:expect(fake_server,
                login,
                fun(St, "meat", "meatmeat") ->
                        {true, authenticate_state(St)}
                end),
    receive
        {new_state, _, _} ->
            ok
    end.

authenticate_successful_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(ControlPid),
                      ControlPid ! {ack, self()},
                      finish(ControlPid)
              end),
    execute(Child).

authenticate_failure_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      script_dialog([{"USER meat", "331 User name okay, need password.\r\n"},
                                     {"PASS meatmeat", "530 Login incorrect.\r\n"}]),
                      ok = meck:expect(gen_tcp, close, fun(socket) -> ok end),
                      ok = meck:expect(fake_server, login, fun(_, "meat", "meatmeat") -> {error} end),
                      ControlPid ! go,
                      ?assertMatch(#connection_state{authenticated_state=unauthenticated}, step(ControlPid)),
                      finish(ControlPid)
              end),

    execute(Child).

unauthenticated_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      script_dialog([  {"CWD /hamster", "530 Not logged in.\r\n"},
                                       {"MKD /unicorns", "530 Not logged in.\r\n"}]),

                      ControlPid ! go,
                      ?assertMatch(#connection_state{authenticated_state=unauthenticated}, step(ControlPid)),
                      ?assertMatch(#connection_state{authenticated_state=unauthenticated}, step(ControlPid)),
                      finish(ControlPid)
              end),
    execute(Child).

mkdir_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  make_directory,
                                  fun(State, _) ->
                                          {ok, State}
                                  end),
                      login_test_user(ControlPid,
                                      [{"MKD test_dir", "250 \"test_dir\" directory created.\r\n"},
                                       {"MKD test_dir_2", "550 Unable to create directory\r\n"}]),
                      step(ControlPid),

                      meck:expect(fake_server,
                                  make_directory,
                                  fun(_, _) ->
                                          {error, error}
                                  end),
                      step(ControlPid),

                      finish(ControlPid)
              end),
    execute(Child).

cwd_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  change_directory,
                                  fun(State, "/meat/bovine/bison") ->
                                          {ok, State}
                                  end),
                      meck:expect(fake_server,
                                  current_directory,
                                  fun(_) -> "/meat/bovine/bison" end),
                      login_test_user(ControlPid,
                                      [{"CWD /meat/bovine/bison", "250 directory changed to \"/meat/bovine/bison\"\r\n"},
                                       {"CWD /meat/bovine/auroch", "550 Unable to change directory\r\n"}]),

                      step(ControlPid),

                      meck:expect(fake_server,
                                  change_directory,
                                  fun(State, "/meat/bovine/auroch") ->
                                          {error, State}
                                  end),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

cdup_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  change_directory,
                                  fun(State, "..") -> {ok, State} end),
                      meck:expect(fake_server,
                                  current_directory,
                                  fun(_) -> "/meat" end),
                      login_test_user(ControlPid,
                                      [{"CDUP", "250 directory changed to \"/meat\"\r\n"},
                                       {"CDUP", "250 directory changed to \"/\"\r\n"},
                                       {"CDUP", "250 directory changed to \"/\"\r\n"}]),
                      step(ControlPid),

                      meck:expect(fake_server,
                                  current_directory,
                                  fun(_) -> "/" end),

                      step(ControlPid),

                      meck:expect(fake_server,
                                  current_directory,
                                  fun(_) -> "/" end),
                      step(ControlPid),

                      finish(ControlPid)
              end),
    execute(Child).

pwd_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  current_directory,
                                  fun(_) -> "/meat/bovine/bison" end),

                      login_test_user(ControlPid, [{"PWD", "257 \"/meat/bovine/bison\"\r\n"}]),

                      step(ControlPid),

                      finish(ControlPid)
              end),
    execute(Child).

login_test_user_with_data_socket(ControlPid, Script, passive) ->
    meck:expect(gen_tcp,
                listen,
                fun(0, _) ->
                        {ok, listen_socket}
                end),

    meck:expect(gen_tcp,
                accept,
                fun(listen_socket) ->
                        {ok, data_socket}
                end),

    meck:expect(inet,
                sockname,
                fun(listen_socket) ->
                        {ok, {{127, 0, 0, 1}, 2000}}
                end),

    login_test_user(ControlPid, [{"PASV", "227 Entering Passive Mode (127,0,0,1,7,208)\r\n"}] ++ Script),
    ControlPid ! {ack, self()},
    receive
        {new_state, _, #connection_state{pasv_listen={passive, listen_socket, {{127,0,0,1}, 2000}}}} ->
            ok;
        _ ->
            ?assert(bad_value)
    end;

login_test_user_with_data_socket(ControlPid, Script, active) ->
    meck:expect(gen_tcp,
                connect,
                fun(_, _, _) ->
                        {ok, data_socket}
                end),
    login_test_user(ControlPid, [{"PORT 127,0,0,1,7,208", "200 Command okay.\r\n"}] ++ Script),
    ControlPid ! {ack, self()},
    receive
        {new_state, _, #connection_state{data_port={active, {127,0,0,1}, 2000}}} ->
            ok
    end.

?dataSocketTest(nlst_test).
nlst_test(Mode) ->
    setup(),
    meck:expect(fake_server,
                list_files,
                fun(_, _) ->
                        [#file_info{type=file, name="edward"},
                         #file_info{type=dir, name="Aethelred"}]
                end),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(gen_tcp, close, fun(data_socket) -> ok end),
                      login_test_user_with_data_socket(ControlPid,
                                                       [{"NLST", "150 File status okay; about to open data connection.\r\n"},
                                                        {resp, data_socket, "edward\r\n"},
                                                        {resp, data_socket, "Aethelred\r\n"},
                                                        {resp, socket, "226 Closing data connection.\r\n"}],
                                                       Mode),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

?dataSocketTest(list_test).
list_test(Mode) ->
    setup(),
    meck:expect(fake_server,
                list_files,
                fun(_, _) ->
                        [#file_info{type=file,
                                    name="edward",
                                    mode=511,
                                    gid=0,
                                    uid=0,
                                    mtime={{3019,12,12},{12,12,12}},
                                    size=512},
                         #file_info{type=dir,
                                    name="Aethelred",
                                    mode=200,
                                    gid=0,
                                    uid=0,
                                    mtime={{3019,12,12},{12,12,12}},
                                    size=0}]
                end),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      Script = [{"LIST", "150 File status okay; about to open data connection.\r\n"},
                                {resp, data_socket, "-rwxrwxrwx  1     0     0      512 Dec 12 12:12 edward\r\n"},
                                {resp, data_socket, "d-wx--x---  4     0     0        0 Dec 12 12:12 Aethelred\r\n"},
                                {resp, socket, "226 Closing data connection.\r\n"}],

                      meck:expect(gen_tcp, close, fun(data_socket) -> ok end),

                      login_test_user_with_data_socket(ControlPid,
                                                       Script,
                                                       Mode),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

remove_directory_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  remove_directory,
                                  fun(St, "/bison/burgers") ->
                                          {ok, St}
                                  end),

                      login_test_user(ControlPid, [{"RMD /bison/burgers", "200 Command okay.\r\n"},
                                                   {"RMD /bison/burgers", "550 Requested action not taken.\r\n"}]),
                      step(ControlPid),

                      meck:expect(fake_server,
                                  remove_directory,
                                  fun(_, "/bison/burgers") ->
                                          {error, error}
                                  end),
                      step(ControlPid),

                      finish(ControlPid)
              end),
    execute(Child).

remove_file_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  remove_file,
                                  fun(St, "cheese.txt") ->
                                          {ok, St}
                                  end),

                      login_test_user(ControlPid, [{"DELE cheese.txt", "200 Command okay.\r\n"},
                                                   {"DELE cheese.txt", "450 Requested file action not taken.\r\n"}]),
                      step(ControlPid),

                      meck:expect(fake_server,
                                  remove_file,
                                  fun(_, "cheese.txt") ->
                                          {error, error}
                                  end),

                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

?dataSocketTest(stor_test).
stor_test(Mode) ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      Script = [{"STOR bologna.txt", "150 File status okay; about to open data connection.\r\n"},
                                {req, data_socket, <<"SOME DATA HERE">>},
                                {resp, socket, "226 Closing data connection.\r\n"}
                               ],
                      meck:expect(fake_server,
                                  put_file,
                                  fun(S, "bologna.txt", write, F) ->
                                          {ok, Data, DataSize} = F(),
                                          BinData = <<"SOME DATA HERE">>,
                                          ?assertEqual(Data, BinData),
                                          ?assertEqual(DataSize, size(BinData)),
                                          {ok, S}
                                  end),

                      meck:expect(gen_tcp, close, fun(data_socket) -> ok end),

                      login_test_user_with_data_socket(ControlPid, Script, Mode),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

?dataSocketTest(stor_failure_test).
stor_failure_test(Mode) ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      Script = [{"STOR bologna.txt", "150 File status okay; about to open data connection.\r\n"},
                                {req, data_socket, <<"SOME DATA HERE">>},
                                {resp, socket, "451 Error access_denied when storing a file.\r\n"}
                               ],
                      meck:expect(fake_server,
                                  put_file,
                                  fun(_, "bologna.txt", write, F) ->
                                          F(),
                                          {error, access_denied}
                                  end),

                      meck:expect(gen_tcp, close, fun(data_socket) -> ok end),

                      login_test_user_with_data_socket(ControlPid, Script, Mode),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

?dataSocketTest(retr_test).
retr_test(Mode) ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      Script = [{"RETR bologna.txt", "150 File status okay; about to open data connection.\r\n"},
                                {resp, data_socket, "SOME DATA HERE"},
                                {resp, data_socket, "SOME MORE DATA"},
                                {resp, socket, "226 Closing data connection.\r\n"}],
                      meck:expect(fake_server,
                                  get_file,
                                  fun(State, "bologna.txt") ->
                                          {ok,
                                           fun(1024) ->
                                                   {ok,
                                                    list_to_binary("SOME DATA HERE"),
                                                    fun(1024) ->
                                                            {ok,
                                                             list_to_binary("SOME MORE DATA"),
                                                             fun(1024) -> {done, State} end}
                                                    end}
                                           end}
                                  end),

                      meck:expect(gen_tcp, close, fun(data_socket) -> ok end),
                      login_test_user_with_data_socket(ControlPid, Script, Mode),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

rein_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(ControlPid, [{"REIN", "200 Command okay.\r\n"}]),
                      ControlPid ! {ack, self()},
                      receive
                          {new_state, _, #connection_state{authenticated_state=unauthenticated}} ->
                              ok;
                          _ ->
                              ?assert(fail)
                      end,
                      finish(ControlPid)
              end),
    execute(Child).

mdtm_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  file_info,
                                  fun(_, "cheese.txt") ->
                                          {ok,
                                           #file_info{type=file,
                                                      mtime={{2012,2,3},{16,3,12}}}}
                                  end),
                      login_test_user(ControlPid, [{"MDTM cheese.txt", "213 20120203160312\r\n"}]),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

mdtm_truncate_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  file_info,
                                  fun(_, "mould.txt") ->
                                          {ok,
                                           #file_info{type=file,
                                                      mtime={{2012,2,3},{16,3,11.933844}}}}
                                  end),
                      login_test_user(ControlPid, [{"MDTM mould.txt", "213 20120203160311\r\n"}]),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

rnfr_rnto_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(ControlPid,
                                      [{"RNTO mushrooms.txt", "503 RNFR not specified.\r\n"},
                                       {"RNFR cheese.txt", "350 Ready for RNTO.\r\n"},
                                       {"RNTO mushrooms.txt", "250 Rename successful.\r\n"}]),
                      step(ControlPid),

                      meck:expect(fake_server,
                                  rename_file,
                                  fun(S, "cheese.txt", "mushrooms.txt") ->
                                          {ok, S}
                                  end),
                      step(ControlPid),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

type_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(ControlPid,
                                      [{"TYPE I", "200 Command okay.\r\n"},
                                       {"TYPE X", "501 Only TYPE I or TYPE A may be used.\r\n"}]),
                      step(ControlPid),
                      step(ControlPid),
                      finish(ControlPid)
              end
             ),
    execute(Child).

site_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  site_command,
                                  fun(S, monkey, "cheese bits") ->
                                          {ok, S}
                                  end),
                      login_test_user(ControlPid,
                                      [{"SITE MONKEY cheese bits", "200 Command okay.\r\n"},
                                       {"SITE GORILLA cheese", "500 Syntax error, command unrecognized.\r\n"}]),
                      step(ControlPid),

                      meck:expect(fake_server,
                                  site_command,
                                  fun(_, gorilla, "cheese") ->
                                          {error, not_found}
                                  end),
                      step(ControlPid),
                      finish(ControlPid)
              end
             ),
    execute(Child).

help_site_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      meck:expect(fake_server,
                                  site_help,
                                  fun (_) ->
                                          {ok, [{"MEAT", "devour the flesh of beasts."}]}
                                  end),
                      Script = [{"HELP SITE", "214-The following commands are recognized\r\n"},
                                {resp, socket, "MEAT : devour the flesh of beasts.\r\n"},
                                {resp, socket, "214 Help OK\r\n"}],
                      login_test_user(ControlPid, Script),
                      step(ControlPid),
                      finish(ControlPid)
              end
             ),
    execute(Child).

unrecognized_command_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(ControlPid, [{"FEED buffalo", "500 Syntax error, command unrecognized.\r\n"}]),
                      step(ControlPid),
                      finish(ControlPid)
              end
             ),
    execute(Child).

quit_test() ->
    setup(),
    meck:expect(gen_tcp,
                close,
                fun(socket) -> ok end),
    ControlPid = self(),
    Child = spawn_link(
              fun () ->
                      meck:expect(fake_server,
                                  disconnect,
                                  fun(_) ->
                                          ok
                                  end),
                      login_test_user(ControlPid,
                                      [{"QUIT", "200 Goodbye.\r\n"}]),
                      step(ControlPid),
                      finish(ControlPid)
              end
             ),
    execute(Child).

feat_test() ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(ControlPid,
                                      [{"FEAT", "211- Extensions supported:\r\n"},
                                       {resp, socket, " UTF8\r\n" },
                                       {resp, socket, "211 End\r\n" }]),
                      step(ControlPid),
                      finish(ControlPid)
              end
             ),
    execute(Child).

?dataSocketTest(utf8_success_test).
utf8_success_test(Mode) ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      FileName = "Молоко-Яйки", % milk-eggs
                      UtfFileName = ucs2_to_utf8(FileName), % milk-eggs
                      FileContent = <<"SOME DATA HERE">>,

                      Script =[{"PWD " ++ UtfFileName, "257 \""++ UtfFileName ++"\"\r\n"},
                               {"CWD " ++ UtfFileName, "250 directory changed to \""++ UtfFileName ++"\"\r\n"},
                               {"STOR " ++ UtfFileName, "150 File status okay; about to open data connection.\r\n"},
                               {req, data_socket, FileContent },
                               {resp, socket, "226 Closing data connection.\r\n"},
                               {"LIST", "150 File status okay; about to open data connection.\r\n"},
                               {resp, data_socket, "d-wx--x---  4     0     0        0 Dec 12 12:12 "++UtfFileName++"\r\n"},
                               {resp, socket, "226 Closing data connection.\r\n"},
                               {"STOR " ++ UtfFileName, "150 File status okay; about to open data connection.\r\n"}
                              ],

                      meck:expect(fake_server,
                                  current_directory,
                                  fun(_) -> FileName end),

                      login_test_user_with_data_socket(ControlPid, Script, Mode),
                      step(ControlPid),

                      meck:expect(fake_server,
                                  change_directory,
                                  fun(State, InFileName) ->
                                          ?assertEqual(InFileName, FileName),
                                          {ok, State}
                                  end),
                      step(ControlPid),

                      meck:expect(fake_server,
                                  put_file,
                                  fun(S, InFileName, write, F) ->
                                          ?assertEqual(InFileName, FileName),
                                          {ok, Data, DataSize} = F(),
                                          ?assertEqual(Data, FileContent),
                                          ?assertEqual(DataSize, size(FileContent)),
                                          {ok, S}
                                  end),

                      meck:expect(gen_tcp, close, fun(data_socket) -> ok end),

                      step(ControlPid),

                      meck:expect(fake_server,
                                  list_files,
                                  fun(_, _) ->
                                          [#file_info{type=dir,name=FileName,mode=200,gid=0,uid=0,mtime={{3019,12,12},{12,12,12}},size=0}]
                                  end),

                      meck:expect(gen_tcp, close, fun(data_socket) -> ok end),

                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

?dataSocketTest(utf8_failure_test).
utf8_failure_test(Mode) ->
    setup(),
    ControlPid = self(),
    Child = spawn_link(
              fun() ->
                      FileName = "Молоко-Яйки", % milk-eggs
                      UtfFileNameOk = ucs2_to_utf8(FileName), % milk-eggs
                      { UtfFileName, _ } = lists:split(length(UtfFileNameOk)-1, UtfFileNameOk),

                      Script =[{"CWD " ++ UtfFileName, "501 Syntax error in parameters or arguments.\r\n"}],

                      meck:expect(gen_tcp, close, fun(data_socket) -> ok end),

                      login_test_user_with_data_socket(ControlPid, Script, Mode),
                      step(ControlPid),
                      finish(ControlPid)
              end),
    execute(Child).

-endif.
