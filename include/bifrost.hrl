
-record(connection_state,
        {
          authenticated_state = unauthenticated,
          user_name,
          ip,
          data_port = undefined,
          pasv_listen = undefined,
          ip_address = undefined,
          rnfr = undefined,
          module,
          module_state,
          ssl_allowed = false,
          ssl_cert = undefined,
          protection_mode = clear, % can also be `private'
          pb_size = 0,
          control_socket = undefined,
          ssl_socket = undefined
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
