-module(test_helpers).

-compile([export_all, nowarn_export_all]).

wait_until_scenario_has_users(Scenario, Current, HighestId) ->
    wait_until_scenario_has_users(Scenario, Current, HighestId, #{}).

wait_until_scenario_has_users(Scenario, Current, HighestId, ExtraConfig) ->
    WaitUntilFun = fun amoc_controller:get_status/0,
    WaitUntilValue = {running, #{scenario => Scenario,
                                 currently_running_users => Current,
                                 highest_user_id => HighestId}},
    wait_helper:wait_until(WaitUntilFun, WaitUntilValue, ExtraConfig).

all_vars() ->
    [{interarrival, 1}, {testing_var1, def1},
     {config_scenario_var1, unused_value}].

regular_vars() ->
    [{interarrival, 1}, {testing_var1, def1}].

regular_vars_with_state() ->
    [{interarrival, 1}, {testing_state_var1, def1}].

other_vars_to_keep_quiet() ->
    [{config_scenario_var1, unused_value}].
