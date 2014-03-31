# Bifröst
Erlang FTP Server Framework

[![Build Status](https://travis-ci.org/thorstadt/bifrost.png?branch=master)](https://travis-ci.org/thorstadt/bifrost)

<img src="http://ryancrum.com/bifrost.jpg" alt="Bifröst" />

> Then spoke Gangleri: "Does fire burn over Bifröst?"
> High said: "The red you see in the rainbow is burning fire. The
>             frost-giants and mountain-giants would go up into
>             heaven if Bifröst was crossable by everyone that
>             wanted to go."

## Description

Bifröst is an implementation of the FTP protocol that enables you to create an FTP server without worrying about the protocol details. Many legacy business systems still use FTP heavily for data transmission, and Bifröst can help bridge the gap between them and modern distributed systems. For example, using Bifröst you can pretty easily write an FTP server that serves files from Amazon's S3 and a Postgres SQL server instead of a filesystem.

Bifröst also includes FTP/SSL support, if you supply a certificate.

## TODO

- Document interface

## Usage

See `memory_server.erl`, a somewhat-feature-complete-but-completely-useless in-memory FTP server for an example implementation.

Create a module with the `gen_bifrost_server` behaviour and implement all of the callbacks. Documentation is on its way, but until then `memory_server` will unfortunately have to suffice.

### gen_bifrost_server

bifrost retains state for each connection which is stored in a record type `connection_state`. This state is passed into every `gen_bifrost_server` callback, and many of the callbacks should return a state.

```erlang

-record(connection_state,
        {
          authenticated_state = AuthState,
          user_name = String,
          data_port = DataPort,
          pasv_listen = PasvPort,
          ip_address = IpAddress, % FTP server IP
          rnfr = String, % used by RNFR, RNTO
          module = GenBifröstServerModule,
          module_state = ModuleInternalState, % can be anything
          ssl_allowed = Boolean,
          ssl_cert = Path,
          ssl_key = Path,
          ssl_ca_cert = Path,
          protection_mode = ProtectionMode,
          pb_size = 0,
          control_socket = Socket,
          ssl_socket = Socket
         }).
```

* `AuthState` = authenticated | unauthenticated
* `DataPort` = {active, IpAddress, Port}
* `PasvPort` = {passive, Socket, {IpAddress, Port}}
* `ProtectionMode` = clear | private

In your application supervisor add a bifrost child:

`my_supervisor.erl`

```erlang
init([]) ->
  GenBifröstServerModule = some_module,
  ExternalIpAddress = {127,0,0,1},
  Port = 21,
  {ok, { {one_for_one, 5, 10},
         [{bifrost,
           {bifrost,
            start_link,
            [GenBifröstServerModule, [{ip_address, ExternalIpAddress}, {port, Port}]]},
           permanent,
           5000,
           worker,
           [bifrost]}]}}.
```

### SSL

Bifröst includes implicit FTP/SSL support. To use this functionality, just add some additional startup options to the start_link call.

- `ssl` should be `true`.
- `ssl_key` should be the path to the PEM-encoded private key file.
- `ssl_cert` should be the path to the PEM-encoded certificate file.
- `ssl_ca_cert` should be the path to the CA's PEM-encoded certificate file.

### Contributions

Bifröst is being used in production environments, and has proven itself reliable. That being said, there are undoubtedly bugs and things that can be improved. Please feel free to submit issues (and pull requests!).

## License

Copyright (C) 2012-2014 Ryan Crum, Phil Calvin, Boris Resnick, madRat

Distributed under the MIT License.
