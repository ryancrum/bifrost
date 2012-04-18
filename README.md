# Bifrost
Pluggable Erlang FTP Server

> Then spoke Gangleri: "Does fire burn over Bifrost?"
> High said: "The red you see in the rainbow is burning fire. The
>             frost-giants and mountain-giants would go up into 
>             heaven if Bifrost was crossable by everyone that
>             wanted to go."
>  -- from the Gylfaginning (Snorri Sturluson)

### Warning: Bifrost is still in very early active development, is not yet documented and the interface is prone to be subject to dramatic changes. There is one known production implementation of bifrost, however, so is complete enough to work.

## TODO

- Document interface
- Implement FTP/SSL (see `ssl` branch for current work on this)
- Include real erlang application with included supervisor, not just a gen_server.

## Description
Bifrost is a server implementation of the FTP protocol that allows you to define your own backend (i.e. a filesystem, webservice, etc.).

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
            [GenBifrostServerModule, ExternalIpAddress, Port]},
           permanent,
           5000,
           worker,
           [bifrost]}]}}.
```

## License

Copyright (C) 2012 Ryan Crum

Distributed under the MIT License.
