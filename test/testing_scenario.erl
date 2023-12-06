-module(testing_scenario).

-behaviour(amoc_scenario).

-required_variable(#{name => testing_var1, description => "description1",
                     verification => [def1, another_value]}).

%% amoc_scenario behaviour
-export([init/0, start/1, terminate/0]).

-spec init() -> ok.
init() ->
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(_Id) ->
    %% Wait for any message to be send
    receive
        Msg ->
            ct:pal("Msg ~p~n", [Msg]),
            timer:sleep(100),
            Msg
    end.

-spec terminate() -> term().
terminate() ->
    ok.
