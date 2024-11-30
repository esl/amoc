-module(amoc_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

all() ->
    [
     bad_config_fails_to_start,
     start_with_no_users,
     start_with_some_users,
     start_and_add_some_users,
     start_and_then_force_remove_some_users,
     start_and_then_soft_remove_some_users,
     start_and_then_force_remove_more_users_than_running,
     force_remove_more_users_with_no_running,
     start_and_then_soft_remove_users_that_ignore_the_error,
     start_and_then_stop_cannot_rerun,
     after_reset_can_run_again
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

bad_config_fails_to_start(_) ->
    Ret = amoc_do(testing_scenario, 0, []),
    ?assertMatch({error, _}, Ret).

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
    Removed = amoc:remove(1, true),
    ?assertEqual({ok, 1}, Removed),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 1, 2).

start_and_then_soft_remove_some_users(_) ->
    Ret = amoc_do(testing_scenario, 2),
    ?assertEqual(ok, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 2, 2),
    Removed = amoc:remove(1, false),
    ?assertEqual({ok, 1}, Removed),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 1, 2).

start_and_then_force_remove_more_users_than_running(_) ->
    Ret = amoc_do(testing_scenario, 2),
    ?assertEqual(ok, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 2, 2),
    Removed = amoc:remove(10, true),
    ?assertEqual({ok, 2}, Removed),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 0, 2).

force_remove_more_users_with_no_running(_) ->
    Ret = amoc_do(testing_scenario, 0),
    ?assertEqual(ok, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 0, 0),
    Removed = amoc:remove(10, true),
    ?assertEqual({ok, 0}, Removed),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 0, 0).

start_and_then_soft_remove_users_that_ignore_the_error(_) ->
    Ret = amoc_do(testing_scenario_with_state, 2, test_helpers:all_vars_with_state()),
    ?assertEqual(ok, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario_with_state, 2, 2),
    Removed = amoc:remove(10, false),
    ?assertEqual({ok, 2}, Removed),
    timer:sleep(100),
    Status = amoc_controller:get_status(),
    ?assertMatch({running, #{scenario := testing_scenario_with_state,
                             currently_running_users := 2,
                             highest_user_id := 2}}, Status).

start_and_then_stop_cannot_rerun(_) ->
    Ret = amoc_do(testing_scenario, 1),
    ?assertEqual(ok, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 1, 1),
    Status = amoc:stop(),
    ?assertMatch(ok, Status),
    Retry = amoc_do(testing_scenario, 1),
    ?assertMatch({error, {invalid_status, _}}, Retry).

after_reset_can_run_again(_) ->
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
