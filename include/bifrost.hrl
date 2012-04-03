
-record(connection_state,
        {
          authenticated_state = unauthenticated,
          user_name,
          ip,
          data_port = undefined,
          pasv_listen = undefined,
          rnfr = undefined,
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
