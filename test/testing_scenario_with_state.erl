-module(testing_scenario_with_state).

-behaviour(amoc_scenario).

-required_variable(#{name => testing_state_var1, description => "description1",
                     verification => [def1, another_value]}).

%% amoc_scenario behaviour
-export([init/0, start/2, terminate/1]).

-type state() :: #{_ := _}.
-export_type([state/0]).

-spec init() -> {ok, state()}.
init() ->
    {ok, #{some_state => this_has_state}}.

-spec start(amoc_scenario:user_id(), state()) -> any().
start(Id, #{some_state := this_has_state} = State) ->
    %% Wait for anymessage to be send
    receive
        {'EXIT', _, _} ->
            start(Id, State);
        Msg ->
            ct:pal("Msg ~p~n", [Msg]),
            timer:sleep(100),
            Msg
    end.

-spec terminate(state()) -> any().
terminate(#{some_state := GiveState}) ->
    throw(GiveState).
