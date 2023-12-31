-module(amoc_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

all() ->
    [
     start_with_bad_config,
     start_with_no_users,
     start_with_some_users,
     start_and_add_some_users,
     start_and_then_force_remove_some_users,
     start_and_then_soft_remove_some_users,
     start_and_then_stop_cannot_rerun,
     start_and_then_stop_can_rerun
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    application:ensure_all_started(amoc),
    Config.

end_per_testcase(_, _Config) ->
    application:stop(amoc),
    ok.

%%-----------------------------------------------------------------------------------
%% test cases
%%-----------------------------------------------------------------------------------

start_with_bad_config(_) ->
    Ret = amoc_do(testing_scenario, 0, []),
    ?assertMatch({{error, _}, 0}, Ret).

start_with_no_users(_) ->
    Ret = amoc_do(testing_scenario, 0),
    ?assertEqual(ok, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 0, 0).

start_with_some_users(_) ->
    Ret = amoc_do(testing_scenario, 1),
    ?assertEqual(ok, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 1, 1).

start_and_add_some_users(_) ->
    Ret = amoc_do(testing_scenario, 0),
    ?assertEqual(ok, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 0, 0),
    amoc:add(1),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 1, 1).

start_and_then_force_remove_some_users(_) ->
    Ret = amoc_do(testing_scenario, 2),
    ?assertEqual(ok, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 2, 2),
    amoc:remove(1, true),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 1, 2).

start_and_then_soft_remove_some_users(_) ->
    Ret = amoc_do(testing_scenario, 2),
    ?assertEqual(ok, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 2, 2),
    amoc:remove(1, false),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 1, 2).

start_and_then_stop_cannot_rerun(_) ->
    Ret = amoc_do(testing_scenario, 1),
    ?assertEqual(ok, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 1, 1),
    Status = amoc:stop(),
    ?assertMatch(ok, Status),
    Retry = amoc_do(testing_scenario, 1),
    ?assertMatch({{error, {invalid_status, _}}, 1}, Retry).

start_and_then_stop_can_rerun(_) ->
    Ret = amoc_do(testing_scenario, 1),
    ?assertEqual(ok, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 1, 1),
    Status = amoc:reset(),
    ?assertMatch(ok, Status),
    Retry = amoc_do(testing_scenario, 1),
    ?assertMatch(ok, Retry).

%% Helpers
amoc_do(Scenario) ->
    amoc_do(Scenario, 0, test_helpers:all_vars()).

amoc_do(Scenario, Count) ->
    amoc_do(Scenario, Count, test_helpers:all_vars()).

amoc_do(Scenario, Count, Config) ->
    amoc:do(Scenario, Count, Config).
