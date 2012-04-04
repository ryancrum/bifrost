-module(gen_bifrost_server).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{login, 3}, 
     {init, 1}, 
     {current_directory, 1}, 
     {make_directory, 2}, 
     {change_directory, 2}, 
     {list_files, 2}, 
     {remove_directory, 2}, 
     {remove_file, 2}, 
     {put_file, 4}, 
     {get_file, 2}, 
     {file_info, 2},
     {rename_file, 3},
     {site_command, 3}];
behaviour_info(_) ->
    undefined.
