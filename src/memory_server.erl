%%%-------------------------------------------------------------------
%%% File    : memory_server.erl
%%% Author  : Ryan Crum <ryan.j.crum@gmail.com>
%%% Description : A simplistic and useless implementation of a
%%%               bifrost server.
%%%-------------------------------------------------------------------

-module(memory_server).
-include("bifrost.hrl").
-include_lib("eunit/include/eunit.hrl").

-behavior(gen_bifrost_server).

-export([login/3, 
         init/2, 
         current_directory/1, 
         make_directory/2, 
         change_directory/2, 
         list_files/2, 
         remove_directory/2, 
         remove_file/2, 
         put_file/4, 
         get_file/2, 
         file_info/2,
         rename_file/3,
         site_command/3,
         site_help/1,
         disconnect/1]).

-ifdef(debug).
-compile(export_all).
-endif.

-ifdef(TEST).
-export([fs_with_paths/1, fs_with_paths/2, add_file/3]).
-endif.

-record(msrv_state, 
        {
          current_dir = [[]],
          fs = new_directory("/")
         }).

init(InitialState, _) ->
    InitialState.

login(State, _Username, _Password) ->
    {true, initialize_state(State)}.

current_directory(State) ->
    case current_directory_list(State) of
        [[]] ->
            "/";
        Path ->
            string:join(Path, "/")
    end.

make_directory(State, Directory) ->
    Target = absolute_path(State, Directory),
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
    Target = absolute_path(State, Directory),
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

disconnect(_) ->
    ok.

remove_file(State, File) ->
    Target = absolute_path(State, File),
    ModState = get_module_state(State),
    Fs = get_fs(ModState),
    case fetch_path(Fs, Target) of
        not_found ->
            {error, not_found};
        {dir, _, _} ->
            {error, not_file};
        {file, _, _} ->
            NewModState = ModState#msrv_state{fs=set_path(Fs, 
                                                          Target, 
                                                          remove)},
            {ok, set_module_state(State, NewModState)};
        _ ->
            {error, unknown}
    end.

rename_file(_State, _FromPath, _ToPath) ->
    {error, not_supported}.

remove_directory(State, Directory) ->
    Target = absolute_path(State, Directory),
    ModState = get_module_state(State),
    Fs = get_fs(ModState),
    case fetch_path(Fs, Target) of
        not_found ->
            {error, not_found};
        {file, _, _} ->
            {error, not_directory};
        {dir, Dict, _} ->
            DictSize = dict:size(Dict),
            if DictSize > 0 ->
                    {error, not_empty};
               true ->
                    NewModState = ModState#msrv_state{fs=set_path(Fs, 
                                                                  Target, 
                                                                  remove)},
                    {ok, set_module_state(State, NewModState)}
            end;
        _ ->
            {error, unknown}
    end.

list_files(State, "") ->
    list_files(State, current_directory(State));
list_files(State, Directory) ->
    Target = absolute_path(State, Directory),
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

% mode could be append or write, but we're only supporting
% write.
% FileRetrievalFun is fun(ByteCount) and retrieves up to ByteCount bytes,
% or as many bytes as are available if ByteCount == 0
%  and returns {ok, Bytes, Count} or done
put_file(State, ProvidedFileName, _Mode, FileRetrievalFun) ->
    FileName = lists:last(string:tokens(ProvidedFileName, "/")),
    Target = absolute_path(State, FileName),
    ModState = get_module_state(State),
    Fs = get_fs(ModState),
    {ok, FileBytes, FileSize} = read_from_fun(FileRetrievalFun),
    NewFs= set_path(Fs, Target, {file,
                                 FileBytes,
                                 new_file_info(FileName, file, FileSize)}),
    NewModState = ModState#msrv_state{fs=NewFs},
    {ok, set_module_state(State, NewModState)}.

get_file(State, Path) ->
    Target = absolute_path(State, Path),
    ModState = get_module_state(State),
    Fs = get_fs(ModState),
    case fetch_path(Fs, Target) of
        {file, Contents, _} ->
            {ok, reading_fun(State, Contents)};
        _ ->
            error
    end.

file_info(State, Path) ->
    Target = absolute_path(State, Path),
    ModState = get_module_state(State),
    Fs = get_fs(ModState),
    case fetch_path(Fs, Target) of
        {file, _, Info} ->
            {ok, Info};
        _ ->
            {error, not_found}
    end.

read_from_fun(Fun) ->
    read_from_fun([], 0, Fun).
read_from_fun(Buffer, Count, Fun) ->
    case Fun(1024) of
        {ok, Bytes, ReadCount} ->
            read_from_fun([Buffer, Bytes], Count + ReadCount, Fun);
        done ->
            io:format("DONE!"),
            {ok, Buffer, Count}
    end.

reading_fun(State, Bytes) ->
    reading_fun(State, 1, Bytes).
reading_fun(State, Pos, Bytes) ->
    TotalSize = length(Bytes),
    fun(ByteCount) ->
            Window = list_to_binary(lists:sublist(Bytes, Pos, ByteCount)),
            ReadCount = size(Window),
            if Pos >= TotalSize ->
                    {done, State};
               true ->
                    {ok, Window, reading_fun(State, Pos + ReadCount, Bytes)}
            end
    end.

site_command(_, _, _) ->
    {error, not_found}.

site_help(_) ->
    {error, not_found}.

%% priv
get_module_state(State) ->
    State#connection_state.module_state.

get_fs(ModState) ->
    ModState#msrv_state.fs.

split_directory(DirString) ->
    string:tokens(DirString, "/").

absolute_path(State, Directory=[FirstChar | _]) ->
    Path = case FirstChar of
               $/ ->
                   [[]] ++ split_directory(Directory);
               _ ->
                   current_directory_list(State) ++ split_directory(Directory)
           end,
    resolve_path(State, Path).

resolve_path(State, Path) ->
    resolve_path(State, Path, []).

resolve_path(_, [], []) ->
    [[]]; % back to the root
resolve_path(_, [], R) ->
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

new_file_info(Name, Type, Size) ->
    #file_info{name=Name, 
               mtime=erlang:localtime(),
               type=Type,
               mode=511, % 0777
               gid=0,
               uid=0,
               size=Size}.

new_directory(Name) ->
    {dir, dict:new(), new_file_info(Name, dir, 0)}.

set_path(F, [[]|T], V) ->
    set_path(F, T, V);
set_path({dir, Root, FileInfo}, [Current], Val) ->
    case Val of
        remove ->
            {dir, dict:erase(Current, Root), FileInfo};
        _ ->
            {dir, dict:store(Current, Val, Root), FileInfo}
    end;
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

%% Tests
-ifdef(TEST).

fs_with_paths([], State) ->
    State;
fs_with_paths([Path | Paths], State) ->
    {ok, NewState} = make_directory(State, Path),
    fs_with_paths(Paths, NewState).

fs_with_paths(Paths) ->
    fs_with_paths(Paths, wrap_fs(create_fs())).

add_file(State, Path, Contents) ->
    put_file(State, 
             Path, 
             image, 
             fun(_) ->
                     {ok, Contents, size(Contents)}
             end).

wrap_fs(Fs) ->
    #connection_state{module_state=#msrv_state{fs=Fs}, module=?MODULE}.

create_fs() ->
    new_directory("").

new_file_info_test() ->
    FileInfo = #file_info{name="Test",
                          mtime=erlang:localtime(),
                          type=file,
                          mode=0511,
                          gid=0,
                          uid=0,
                          size=20},
    FileInfo = new_file_info("Test", file, 20).

new_directory_test() ->
    EmptyDictionary = dict:new(),
    NewFileInfo = new_file_info("Test", dir, 0),
    {dir, EmptyDictionary, NewFileInfo} = new_directory("Test").

login_test() ->
    OldState = #connection_state{},
    NewState = #connection_state{module_state=#msrv_state{current_dir=[[]]}},
    {true, NewState} = login(OldState, "a", "b").

current_directory_test() ->
    ModState1 = #msrv_state{current_dir = [[]]},
    ModState2 = #msrv_state{current_dir = [[], "testing", "123"]},
    State1 = #connection_state{module_state=ModState1},
    State2 = #connection_state{module_state=ModState2},
    "/" = current_directory(State1),
    "/testing/123" = current_directory(State2).

change_directory_test() ->
    FS = set_path(create_fs(), ["testing", "123"], new_directory("123")),
    ModStateBefore = #msrv_state{current_dir = [[], "testing", "123"], fs=FS},
    ModStateAfter = #msrv_state{current_dir = [[], "testing"], fs=FS},
    StateBefore = #connection_state{module_state=ModStateBefore},
    StateAfter = #connection_state{module_state=ModStateAfter},
    {ok, StateBefore} = change_directory(StateBefore, "."),
    {ok, StateAfter} = change_directory(StateBefore, ".."),
    {ok, StateAfter} = change_directory(StateBefore, "/testing"),
    {error, StateBefore} = change_directory(StateBefore, "/magical/unicorn").

remove_directory_test() ->
    FSBefore = set_path(create_fs(), ["testing", "123"], new_directory("123")),
    FSAfter = set_path(create_fs(), ["testing"], new_directory("testing")),
    FSWithFile = set_path(FSBefore, ["testing", "123", "cheese"], {file, contents, new_file_info("cheese", file, 0)}),
    ModStateBefore = #msrv_state{current_dir = [[], "testing"], fs=FSBefore},
    ModStateAfter = #msrv_state{current_dir = [[], "testing"], fs=FSAfter},
    ModStateWithFile = #msrv_state{current_dir = [[], "testing"], fs=FSWithFile},
    StateBefore = #connection_state{module_state=ModStateBefore},
    StateAfter = #connection_state{module_state=ModStateAfter},
    StateWithFile = #connection_state{module_state=ModStateWithFile},
    {ok, StateAfter} = remove_directory(StateBefore, "123"),
    {ok, StateAfter} = remove_directory(StateBefore, "/testing/123"),
    {error, not_found} = remove_directory(StateBefore, "monkey"),
    {error, not_directory} = remove_directory(StateWithFile, "/testing/123/cheese"),
    {error, not_empty} = remove_directory(StateWithFile, "/testing/123").

-endif.
