%%%-------------------------------------------------------------------
%%% File    : bifrost.erl
%%% Author  : Ryan Crum <ryan@ryancrum.org>
%%% Description : 
%%%
%%% Created :  3 Mar 2012 by Ryan Crum <ryan@bismarck>
%%%-------------------------------------------------------------------
-module(bifrost).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("bifrost.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/1, start_link/0]).
-export([establish_control_connection/3]).
-export([await_connections/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, 
        {
          listen,
          accept
         }).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    start_link(memory_server).

start_link(HookModule) ->
    gen_server:start_link(?MODULE, [HookModule], []).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

listen_socket(Port, TcpOpts) ->
    gen_tcp:listen(Port, TcpOpts).

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
            control_loop(SrvPid, Socket, #connection_state{module=Mod});
        _Error ->
            exit(bad_accept)
    end.

control_loop(SrvPid, Socket, State) ->
    receive
        {tcp, Socket, Input} ->
            {Command, Arg} = parse_input(Input),
            case ftp_command(Socket, State, Command, Arg) of
                {ok, NewState} ->
                    control_loop(SrvPid, Socket, NewState);
                {error, timeout} ->
                    respond(Socket, 412, "Timed out. Closing control connection."),
                    true;
                {error, closed} ->
                    true;
                quit -> true
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

ftp_command(Socket, _, quit, _) ->
    respond(Socket, 200, "Goodbye."),
    quit;
ftp_command(Socket, State, user, Arg) ->
    respond(Socket, 331),
    {ok, State#connection_state{user_name=Arg}};
ftp_command(Socket, State, port, Arg) ->
    case parse_address(Arg) of
        {ok, {Addr, Port} = AddrPort} ->
            respond(Socket, 200),
            io:format("Data port set to ~p~n", [AddrPort]),
            {ok, State#connection_state{data_port = AddrPort}};
         _ ->
            respond(Socket, 452, "Error parsing address.")
        end;
ftp_command(Socket, State, pass, Arg) ->
    Mod = State#connection_state.module,
    case Mod:login(State, State#connection_state.user_name, Arg) of
        {true, NewState} -> 
            respond(Socket, 230), 
            {ok, NewState#connection_state{authenticated_state=authenticated}};
        _ ->
            respond(Socket, 530, "Login incorrect."),
            {error, closed}
     end;

%% from this point on every command requires authentication
ftp_command(Socket, State=#connection_state{authenticated_state=unauthenticated}, Command, _) ->
    respond(Socket, 530),
    {ok, State};

ftp_command(Socket, State, pwd, _) ->
    Mod = State#connection_state.module,
    respond(Socket, 257, Mod:current_directory(State)),
    {ok, State};

ftp_command(Socket, State, cwd, Arg) ->
    Mod = State#connection_state.module,
    case Mod:change_directory(State, Arg) of
        {ok, NewState} ->
            respond(Socket, 250, "directory changed to \"" ++ Mod:current_directory(NewState) ++ "\""),
            {ok, NewState};
        {error, _} ->
            respond(Socket, 450, "Unable to change directory"),
            {ok, State}
    end;

ftp_command(Socket, State, mkd, Arg) ->
    Mod = State#connection_state.module,
    case Mod:make_directory(State, Arg) of
        {ok, NewState} ->
            respond(Socket, 250, "\"" ++ Arg ++ "\" directory created."),
            {ok, NewState};
        {error, _} ->
            respond(Socket, 450, "Unable to create directory"),
            {ok, State}
    end;

ftp_command(Socket, State, list, Arg) ->
    Mod = State#connection_state.module,
    DataSocket = data_connection(Socket, State),
    case Mod:list_files(State, Arg) of
        not_found ->
            respond(Socket, 451);
        Files ->
            list_files_to_socket(DataSocket, Files),
            respond(Socket, 226),
            gen_tcp:close(DataSocket),
            {ok, State}
    end;

ftp_command(Socket, State, rmd, Arg) ->
    Mod = State#connection_state.module,
    case Mod:remove_directory(State, Arg) of
        {ok, NewState} ->
            respond(Socket, 200),
            {ok, NewState};
        {error, _} ->
            respond(Socket, 450),
            {ok, State}
        end;


ftp_command(Socket, State, dele, Arg) ->
    Mod = State#connection_state.module,
    case Mod:remove_file(State, Arg) of
        {ok, NewState} ->
            respond(Socket, 200),
            {ok, NewState};
        {error, _} ->
            respond(Socket, 450),
            {ok, State}
        end;
    
ftp_command(Socket, State, Command, _) ->
    io:format("Unrecognized command ~p~n", [Command]),
    respond(Socket, 500),
    {ok, State}.

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
%% OUTPUT FORMATTING
%% Most of this code has been shamelessly/fully 
%% yanked from jungerl/ftpd.erl
%% FTP code strings 
response_code_string(110) -> "MARK yyyy = mmmm";             %% ARGS
response_code_string(120) -> "Service ready in nnn minutes.";  %% ARG
response_code_string(125) -> "Data connection alredy open; transfere starting.";
response_code_string(150) -> "File status okay; about to open data connection.";
response_code_string(200) -> "Command okay.";
response_code_string(202) -> "Command not implemented, superfluos at this site.";
response_code_string(211) -> "System status, or system help reply.";
response_code_string(212) -> "Directory status.";
response_code_string(213) -> "File status.";
response_code_string(214) -> "Help message.";     %% ADD HELP
response_code_string(215) -> "NAME system type";  %% set NAME
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
response_code_string(500) -> "Syntax error, command unrecognized.";  %% ADD INFO
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
