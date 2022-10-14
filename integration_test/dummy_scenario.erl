-module(dummy_scenario).
-behaviour(amoc_scenario).

%% amoc_scenario behaviour
-export([init/0, start/1]).

-spec init() -> ok.
init() ->
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(_Id) ->
    %%sleep 15 minutes
    timer:sleep(1000 * 60 * 15),
    amoc_user:stop().
