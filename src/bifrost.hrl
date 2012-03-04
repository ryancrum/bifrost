
-record(connection_state,
        {
          authenticated_state = unauthenticated,
          user_name,
          ip,
          data_port,
          pasv_listen,
          module,
          module_state
         }).

-record(file_info,
        {
          type, % dir or file
          name,
          mode,
          uid,
          gid,
          size,
          mtime,
          module_info
         }).
