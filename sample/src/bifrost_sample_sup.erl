%%%-------------------------------------------------------------------
%%% File    : bifrost_sup.erl
%%% Author  : Boris Resnick <boris@resnick.ru>
%%% Description : Stub supervisor for the library
%%%-------------------------------------------------------------------
-module(bifrost_sample_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10},
           [{bifrost,
             {bifrost, start_link, [bifrost_memory_server,
                                    [{port, 2121}]]},
             permanent,
             5000,
             worker,
             [bifrost]}
           ]} }.

