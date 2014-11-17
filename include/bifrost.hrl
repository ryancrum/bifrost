
-record(connection_state,
        {
          authenticated_state = unauthenticated,
          user_name,
          data_port = undefined,
          pasv_listen = undefined,
          ip_address = undefined,
          rnfr = undefined,
          module,
          module_state,
		  ssl_mode = disabled,	% 'disabled' - NO SSL
								% 'enabled'  - allowed SSL and FTP
								% 'only'     - non secured FTP is not allowed
								% old true and false also supported
          ssl_cert = undefined,
          ssl_key = undefined,
          ssl_ca_cert = undefined,
          protection_mode = clear,
          pb_size = 0,
          control_socket = undefined,
          ssl_socket = undefined,
          utf8 = true,
		  recv_block_size = 64*1024,
		  send_block_size = 64*1024,
		  prev_cmd_notify = undefined, % previous command notification data { command, Arguments } | undefined
		  control_timeout = infinity % control connection timeout for prev-command notification = tcp_gen:timeout()
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
