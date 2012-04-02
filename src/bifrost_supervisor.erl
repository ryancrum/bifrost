-module(bifrost_supervisor).

-behavior(supervisor).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 3, 10},
          [{bifrost,
            {bifrost, start_link, []},
            permanent,
            2000,
            worker,
            [bifrost]}]}}.

