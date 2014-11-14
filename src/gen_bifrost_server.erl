%%%-------------------------------------------------------------------
%%% File    : gen_bifrost_server.erl
%%% Author  : Ryan Crum <ryan.j.crum@gmail.com>
%%% Description : Behavior for a Bifrost FTP server.
%%%-------------------------------------------------------------------

-module(gen_bifrost_server).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
	% FileInfo :: include/bifrost.hrl:file_info()
	% State, NewState :: include/bifrost.hrl:connection_state()
    % Path      :: String
    % File Name :: String
	%
    % StateChangeOK     :: {ok, NewState}
    % StateChangeError  :: {error, Reason, NewState}
	% 						for compatibility {error, Reason} and {error, NewState} also supported
	%
	% StateChange :: StateChangeOK | StateChangeError
	%
    % HelpInfo :: {Name, Description}
	%
	% GetFun(ByteCount) -> {ok, Bytes, NextGetFun} | {done, NewState} | StateChangeError
	%
    [{init, 2}, % State, PropList (options) -> State
     {login, 3}, % State, Username, Password -> {true OR false, State} | 'quit'(disconnect client)
     {current_directory, 1}, % State -> Path
     {make_directory, 2}, % State, Path -> StateChange
     {change_directory, 2}, % State, Path -> StateChange
     {list_files, 2}, % State, Path -> [FileInfo] | StateChangeError

     {remove_directory, 2}, % State, Path -> StateChange
     {remove_file, 2}, % State, Path -> StateChange
     {put_file, 4}, % State, File Name, (append | write), Fun(Byte Count) -> StateChange
	 				% State, File Name, notification, 	done (next command is arrived) |
					% 									timeout (control_timeout is passed, but connection works)|
					% 									terminated (control connection error) -> StateChange
     {get_file, 2}, % State, Path -> {ok, GetFun, NewState} | StateChangeError
	 				%      for compatibility {ok, GetFun} | error also supported

     {file_info, 2}, % State, Path -> {ok, FileInfo} | StateChangeError
     {rename_file, 3}, % State, From Path, To Path -> StateChange
     {site_command, 3}, % State, CommandNameString, CommandArgsString -> StateChange
     {site_help, 1}, % State -> {ok, [HelpInfo]} | StateChangeError

     {disconnect, 2}]; % State, exit (QUIT command from client) or {error, Reason}  -> *unused* State Change

behaviour_info(_) ->
    undefined.
