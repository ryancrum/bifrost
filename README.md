# Bifrost
Pluggable Erlang FTP Server

> Then spoke Gangleri: "Does fire burn over Bifrost?"
> High said: "The red you see in the rainbow is burning fire. The
>             frost-giants and mountain-giants would go up into 
>             heaven if Bifrost was crossable by everyone that
>             wanted to go."
>  -- from the Gylfaginning (Snorri Sturluson)

### Warning: Bifrost is still in very early active development, is not yet documented and the interface is prone to be subject to dramatic changes.

## TODO

- Document interface
- Include real erlang application with included supervisor, not just a gen_server.

## Description
Bifrost is a server implementation of the FTP protocol that allows you to define your own backend (i.e. a filesystem, webservice, etc.). It includes implicit FTP/SSL support.

## Usage

See `memory_server.erl`, a somewhat-feature-complete-but-completely-useless in-memory FTP server for an example implementation.

Create a module with the `gen_bifrost_server` behaviour and implement all of the callbacks. memory_server will have to suffice for documentation until the bifrost interface is firmed up. Sorry.
In your application supervisor add a bifrost child:

`my_supervisor.erl`

```erlang
init([]) ->
  GenBifrostServerModule = some_module,
  ExternalIpAddress = {127,0,0,1},
  Port = 21,
  {ok, { {one_for_one, 5, 10},
         [{bifrost,
           {bifrost,
            start_link,
            [GenBifrostServerModule, ExternalIpAddress, [{port, Port}]]},
           permanent,
           5000,
           worker,
           [bifrost]}]}}.
```

### SSL

Bifrost includes implicit FTP/SSL support. To use this functionality, just add some additional startup options to the start_link call.

`ssl` should be `true`.
`ssl_key` should be the path to the PEM-encoded private key file.
`ssl_cert` should be the path to the PEM-encoded certificate file.
`ssl_ca_cert` should be the path to the CA's PEM-encoded certificate file.

## License

Copyright (C) 2012 Ryan Crum

Distributed under the MIT License.
