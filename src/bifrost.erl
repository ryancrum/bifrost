-module(bifrost).

-behaviour(gen_server).

-include("bifrost.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/1, start_link/0, init_sync/1, establish_control_connection/3, await_connections/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, 
        {
          listen,
          accept
         }).

start_link() ->
    start_link(memory_server).

start_link(HookModule) ->
    gen_server:start_link(?MODULE, [HookModule], []).

init([HookModule]) ->
    case listen_socket(5000, []) of
        {ok, Listen} ->
            proc_lib:spawn_link(?MODULE,
                                await_connections,
                                [Listen, HookModule]),
            {ok, done};
        {error, Error} ->
            {stop, Error}
    end.

init_sync(HookModule) ->
    case listen_socket(5000, []) of
        {ok, Listen} ->
            await_connections_sync(Listen, HookModule),
            {ok, done};
        {error, Error} ->
            {stop, Error}
    end.
    
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

listen_socket(Port, TcpOpts) ->
    gen_tcp:listen(Port, TcpOpts).

await_connections_sync(Listen, Mod) ->
    establish_control_connection(self(), Listen, Mod),
    await_connections_sync(Listen, Mod).

await_connections(Listen, Mod) ->
    proc_lib:spawn_link(?MODULE,
                        establish_control_connection,
                        [self(), Listen, Mod]),
    receive
        {accepted, ChildPid} -> await_connections(Listen, Mod);
        Error -> await_connections(Listen, Mod)
    end.

establish_control_connection(SrvPid, Listen, Mod) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            SrvPid ! {accepted, self()},
            respond(Socket, 220, "FTP Server Ready"),
            control_loop(SrvPid, none, Socket, #connection_state{module=Mod});
        _Error ->
            exit(bad_accept)
    end.

control_loop(SrvPid, HookPid, Socket, State) ->
    receive
        {tcp, Socket, Input} ->
            {Command, Arg} = parse_input(Input),
            case ftp_command(Socket, State, Command, Arg) of
                {ok, NewState} ->
                    if is_pid(HookPid) ->
                            HookPid ! {new_state, self(), NewState};
                       true ->
                            ok
                    end,
                    control_loop(SrvPid, HookPid, Socket, NewState);
                {error, timeout} ->
                    respond(Socket, 412, "Timed out. Closing control connection."),
                    {error, timeout};
                {error, closed} ->
                    {error, closed};
                quit ->
                    {ok, quit}
            end;
        {tcp_closed, Socket} ->
            io:format("DONE BRO~n")
    end.

respond(Socket, ResponseCode) ->
    respond(Socket, ResponseCode, response_code_string(ResponseCode)).

respond(Socket, ResponseCode, Message) ->
    gen_tcp:send(Socket, 
                 integer_to_list(ResponseCode) ++ " " ++ Message ++ "\r\n").

data_connection(ControlSocket, State) ->
    respond(ControlSocket, 150),
    {Addr, Port} = State#connection_state.data_port,
    case gen_tcp:connect(Addr, Port, [{active, false}, binary]) of
        {ok, DataSocket} ->
            DataSocket;
        {error, Error} ->
            respond(ControlSocket, 425, "Can't open data connection"),
            throw(failed)
    end.

%% FTP COMMANDS

ftp_command(Socket, State, Command, Arg) ->
    Mod = State#connection_state.module,
    ftp_command(Mod, Socket, State, Command, Arg).

ftp_command(_, Socket, _, quit, _) ->
    respond(Socket, 200, "Goodbye."),
    quit;
ftp_command(_, Socket, State, user, Arg) ->
    respond(Socket, 331),
    {ok, State#connection_state{user_name=Arg}};
ftp_command(_, Socket, State, port, Arg) ->
    case parse_address(Arg) of
        {ok, {Addr, Port} = AddrPort} ->
            respond(Socket, 200),
            io:format("Data port set to ~p~n", [AddrPort]),
            {ok, State#connection_state{data_port = AddrPort}};
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
            {error, closed}
     end;

%% from this point on every command requires authentication
ftp_command(_, Socket, State=#connection_state{authenticated_state=unauthenticated}, Command, _) ->
    respond(Socket, 530),
    {ok, State};

ftp_command(Mod, Socket, State, pwd, _) ->
    respond(Socket, 257, Mod:current_directory(State)),
    {ok, State};

ftp_command(Mod, Socket, State, cdup, Arg) ->
    ftp_command(Mod, Socket, State, cwd, "..");

ftp_command(Mod, Socket, State, cwd, Arg) ->
    case Mod:change_directory(State, Arg) of
        {ok, NewState} ->
            respond(Socket, 250, "directory changed to \"" ++ Mod:current_directory(NewState) ++ "\""),
            {ok, NewState};
        {error, _} ->
            respond(Socket, 450, "Unable to change directory"),
            {ok, State}
    end;

ftp_command(Mod, Socket, State, mkd, Arg) ->
    case Mod:make_directory(State, Arg) of
        {ok, NewState} ->
            respond(Socket, 250, "\"" ++ Arg ++ "\" directory created."),
            {ok, NewState};
        {error, _} ->
            respond(Socket, 450, "Unable to create directory"),
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
            gen_tcp:close(DataSocket),
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
            gen_tcp:close(DataSocket),
            {ok, State}
    end;

ftp_command(Mod, Socket, State, rmd, Arg) ->
    case Mod:remove_directory(State, Arg) of
        {ok, NewState} ->
            respond(Socket, 200),
            {ok, NewState};
        {error, _} ->
            respond(Socket, 450),
            {ok, State}
        end;

ftp_command(Mod, Socket, State, syst, _) ->
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
    Fun = fun(Size) ->
                  case gen_tcp:recv(DataSocket, 0) of
                      {ok, Data} ->
                          {ok, Data, size(Data)};
                      {error, closed} ->
                          done
                  end
          end,
    {ok, NewState} = Mod:put_file(State, Arg, write, Fun),
    respond(Socket, 226),
    gen_tcp:close(DataSocket),
    {ok, NewState};

ftp_command(_, Socket, State, type, Arg) ->
    case Arg of
        "I" ->
            respond(Socket, 200);
        _ ->
            respond(Socket, 501, "Only TYPE I may be used.")
    end,
    {ok, State};

ftp_command(Mod, Socket, State, retr, Arg) ->
    DataSocket = data_connection(Socket, State),
    case Mod:get_file(State, Arg) of
        {ok, Fun} ->
            write_fun(DataSocket, Fun),
            respond(Socket, 226);
        error ->
            respond(Socket, 550)            
    end,
    gen_tcp:close(DataSocket),
    {ok, State};

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

    
ftp_command(Mod, Socket, State, Command, _) ->
    io:format("Unrecognized command ~p~n", [Command]),
    respond(Socket, 500),
    {ok, State}.

write_fun(Socket, Fun) ->
    case Fun(1024) of 
        {ok, Bytes, NextFun} ->
            gen_tcp:send(Socket, Bytes),
            write_fun(Socket, NextFun);
        done ->
            ok
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
    io:format("Listing files ~p~n", [Files]),
    lists:map(fun(Info) -> 
                      gen_tcp:send(DataSocket, 
                                   file_info_to_string(Info) ++ "\r\n") end,
              Files),
    io:format("Done listing~n"),
    ok.

list_file_names_to_socket(DataSocket, Files) ->
    io:format("Listing files ~p~n", [Files]),
    lists:map(fun(Info) -> 
                      gen_tcp:send(DataSocket, 
                                   Info#file_info.name ++ "\r\n") end,
              Files),
    io:format("Done listing~n"),
    ok.

%% OUTPUT FORMATTING
%% Most of this code has been shamelessly/fully 
%% yanked from jungerl/ftpd.erl
%% FTP code strings 
response_code_string(110) -> "MARK yyyy = mmmm";             %% ARGS
response_code_string(120) -> "Service ready in nnn minutes.";  %% ARG
response_code_string(125) -> "Data connection alredy open; transfere starting.";
response_code_string(150) -> "File status okay; about to open data connection.";
response_code_string(200) -> "Command okay.";
response_code_string(202) -> "Command not implemented, superfluous at this site.";
response_code_string(211) -> "System status, or system help reply.";
response_code_string(212) -> "Directory status.";
response_code_string(213) -> "File status.";
response_code_string(214) -> "Help message.";     %% ADD HELP
response_code_string(215) -> "UNIX system type";  %% set NAME
response_code_string(220) -> "Service ready for user.";
response_code_string(221) -> "Service closing control connection.";
response_code_string(225) -> "Data connection open; no transfere in progress";    
response_code_string(226) -> "Closing data connection.";  %% ADD INFO
response_code_string(227) -> "Entering Passive Mode (h1,h2,h3,h4,p1,p2).";  %% ARGS
response_code_string(230) -> "User logged in, proceed.";
response_code_string(250) -> "Requested file action okay, completed.";
response_code_string(257) -> "PATHNAME created.";  %% ARG
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

% printing functions ripped from jungerl/ftpd
file_info_to_string(Info) ->
    format_type(Info#file_info.type) ++
        format_access(Info#file_info.mode) ++ " " ++
        format_number(type_num(Info#file_info.type), 2, $ ) ++ " " ++
        format_number(Info#file_info.uid,5,$ ) ++ " " ++
        format_number(Info#file_info.gid,5,$ ) ++ " "  ++
        format_number(Info#file_info.size,8,$ ) ++ " " ++
        format_date(Info#file_info.mtime) ++ " " ++
        Info#file_info.name.

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
%%
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

% TESTS
-ifdef(TEST).

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

mock_socket_response(S, R) ->
    meck:expect(gen_tcp, 
                send, 
                fun(S2, R2) ->
                        S2 = S,
                        R2 = R,
                        ok
                end).

% FUNCTIONAL TESTS

control_connection_establishment_test() ->
    meck:new(gen_tcp, [unstick]),
    meck:expect(gen_tcp, accept, fun(some_socket) -> {ok, another_socket} end),
    mock_socket_response(another_socket, "220 FTP Server Ready\r\n"),
    Myself = self(),
    Child = spawn_link(fun() ->
                               establish_control_connection(Myself, some_socket, memory_server)
                       end),
    receive
        {accepted, Child} ->
            ok
    end,
    Child ! {tcp_closed, another_socket},
    meck:validate(gen_tcp),
    meck:unload(gen_tcp).

login_test_user(SocketPid) ->
    mock_socket_response(socket, "331 User name okay, need password.\r\n"),
    SocketPid ! {tcp, socket, "USER meat"},
    receive
        {new_state, _, #connection_state{user_name="meat"}} ->
            ok
    end,
    
    mock_socket_response(socket, "230 User logged in, proceed.\r\n"),
    SocketPid ! {tcp, socket, "PASS meatmeat"},
    receive
        {new_state, _, #connection_state{authenticated_state=authenticated}} ->
            ok
    end.

authenticate_successful_test() ->
    meck:new(gen_tcp, [unstick]),
    Myself = self(),
    Child = spawn_link(
              fun() ->
                      login_test_user(Myself),
                      Myself ! {tcp_closed, socket}
              end),
    control_loop(Child, Child, socket, #connection_state{module=memory_server}),
    meck:validate(gen_tcp),
    meck:unload(gen_tcp).

authenticate_failure_test() ->
    meck:new(gen_tcp, [unstick]),
    meck:new(memory_server, [unstick, passthrough]),
    meck:expect(memory_server, 
                login, 
                fun(_, "meat", "meatmeat") ->
                        {error}
                end),
    Myself = self(),
    Child = spawn_link(
              fun() ->
                      mock_socket_response(socket, "331 User name okay, need password.\r\n"),
                      Myself ! {tcp, socket, "USER meat"},
                      receive
                          {new_state, _, #connection_state{user_name="meat"}} ->
                              ok
                      end,

                      mock_socket_response(socket, "530 Login incorrect.\r\n"),
                      Myself ! {tcp, socket, "PASS meatmeat"},
                      receive
                          {new_state, _, #connection_state{authenticated_state=authenticated}} ->
                              ok
                      end
              end),
    {error, closed} = control_loop(Child, Child, socket, #connection_state{module=memory_server}),
    meck:validate(gen_tcp),
    meck:unload(memory_server),
    meck:unload(gen_tcp).

-endif.
