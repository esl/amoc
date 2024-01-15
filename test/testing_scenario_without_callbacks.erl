-module(testing_scenario_without_callbacks).

-behaviour(amoc_scenario).

-required_variable(#{name => testing_var1, description => "description1",
                     verification => [def1, another_value]}).

%% amoc_scenario behaviour
-export([init/0, terminate/0]).

-spec init() -> ok.
init() ->
    ok.

-spec terminate() -> term().
terminate() ->
    ok.
