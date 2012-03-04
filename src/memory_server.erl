-module(memory_server).
-include("bifrost.hrl").

-export([login/3, init/1, current_directory/1, make_directory/2, change_directory/2, list_files/2]).

-define(debug, true).
-ifdef(debug).
-compile(export_all).
-endif.

-record(msrv_state, 
        {
          current_dir = [[]],
          fs = new_directory("/")
         }).

init(_) ->
    {}.

login(State, Username, Password) ->
    {true, initialize_state(State)}.

current_directory(State) ->
    case current_directory_list(State) of
        [[]] ->
            "/";
        Path ->
            string:join(Path, "/")
    end.

make_directory(State, Directory) ->
    Target = absolute_directory(State, Directory),
    Fs = get_fs(get_module_state(State)),
    case fetch_path(Fs, Target) of
        not_found ->
            {ok, set_state_fs(State, 
                              set_path(Fs, 
                                       Target, 
                                       new_directory(lists:last(Target))))};
        _ ->
            {error, State}
    end.

change_directory(State, Directory) ->
    Target = absolute_directory(State, Directory),
    Fs = get_fs(get_module_state(State)),
    case fetch_path(Fs, Target) of
        not_found ->
            {error, State};
        {file, _, _} ->
            {error, State};
        {dir, _, _} ->
            ModState = get_module_state(State),
            NewModState = ModState#msrv_state{current_dir=Target},
            {ok, set_module_state(State, NewModState)};
        _ ->
            {error, State}
    end.

list_files(State, "") ->
    list_files(State, current_directory(State));
list_files(State, Directory) ->
    Target = absolute_directory(State, Directory),
    Fs = get_fs(get_module_state(State)),
    case fetch_path(Fs, Target) of
        not_found ->
            {error, State};
        {dir, Dict, DirInfo} ->
            {dir, _, ParentInfo} = fetch_parent(Fs, Target),
            lists:map(fun({_,{_, _, Info}}) -> Info end, 
                      dict:to_list(Dict)) ++
                [DirInfo#file_info{name = "."}, 
                 ParentInfo#file_info{name = ".."}];
        {file, _, Info} ->
            [Info];
        _ ->
            {error, State}
    end.

%% priv
get_module_state(State) ->
    State#connection_state.module_state.

get_fs(ModState) ->
    ModState#msrv_state.fs.

split_directory(DirString) ->
    string:tokens(DirString, "/").

absolute_directory(State, Directory=[FirstChar | _]) ->
    Path = case FirstChar of
               $/ ->
                   [[]] ++ split_directory(Directory);
               _ ->
                   current_directory_list(State) ++ split_directory(Directory)
           end,
    resolve_path(State, Path).

resolve_path(State, Path) ->
    resolve_path(State, Path, []).

resolve_path(State, [], []) ->
    [[]]; % back to the root
resolve_path(State, [], R) ->
    R;
resolve_path(State, [H|T], R) ->
    case H of
        "." ->
            resolve_path(State, T, R);
        ".." ->
            % drop the last element of R
            [_|Rem] = lists:reverse(R),
            resolve_path(State, T, lists:reverse(Rem));
        P ->
            resolve_path(State, T, R ++ [P])
    end.

set_module_state(State, ModState) ->
    State#connection_state{module_state=ModState}.

set_state_fs(State, Fs) ->
    ModState = get_module_state(State),
    NewModState = ModState#msrv_state{fs=Fs},
    set_module_state(State, NewModState).

current_directory_list(State) ->
    ModState = State#connection_state.module_state,
    ModState#msrv_state.current_dir.

initialize_state(State) ->
    State#connection_state{module_state=#msrv_state{current_dir=[[]]}}.

fetch_parent(Root, [[]]) ->
    Root;
fetch_parent(Root, Path) ->
    [_ | T] = lists:reverse(Path),
    fetch_path(Root, lists:reverse(T)).

fetch_path(F, []) ->
    F;
fetch_path(F, [[] | T]) ->
    fetch_path(F, T);
fetch_path({file, _, _}, [_ | _]) ->
    not_found;
fetch_path({_, Root, _}, [Current]) ->
    case dict:is_key(Current, Root) of
        true ->
            dict:fetch(Current, Root);
        _ ->
            not_found
    end;
fetch_path({dir, Root, _}, [Current | Rest]) ->
    case dict:is_key(Current, Root) of 
        true ->
            fetch_path(dict:fetch(Current, Root), Rest);
        _ ->
            not_found
    end.

new_directory(Name) ->
    {dir, dict:new(), #file_info{name=Name, 
                                 mtime=erlang:localtime(),
                                 type=dir,
                                 mode=511, % 0777
                                 gid=0,
                                 uid=0,
                                 size=0}}.

set_path(F, [[]|T], V) ->
    set_path(F, T, V);
set_path({dir, Root, FileInfo}, [Current], Val) ->
    {dir, dict:store(Current, Val, Root), FileInfo};
set_path({dir, Root, FileInfo}, [Current | Rest], Val) ->
    case dict:is_key(Current, Root) of
        true ->
            {dir, 
             dict:store(Current, 
                        set_path(dict:fetch(Current, Root), Rest, Val), 
                        Root),
            FileInfo};
        _ ->
            {dir, 
             dict:store(Current, 
                        set_path(new_directory(Current), Rest, Val),
                        Root),
            FileInfo}
    end.

wrap_fs(Fs) ->
    #connection_state{module_state=#msrv_state{fs=Fs}}.
