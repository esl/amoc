-module(controller_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").

all() ->
    [
     {group, all_tests}
    ].

groups() ->
    [
     {all_tests, [], all_tests()}
    ].

all_tests() ->
    [
     no_scenario_running_status_is_idle,
     disable_controller_returns_status_disable,
     disable_controller_then_cant_start_scenario,
     start_non_existing_scenario_fails,
     start_scenario_without_vars_fails,
     start_scenario_runs_fine,
     start_scenario_check_status,
     add_users_to_started_scenario,
     add_bad_range_of_users_to_started_scenario_fails,
     add_users_scenario_not_started_fails,
     remove_users_scenario_not_started_fails,
     check_status_with_running_users_is_correct,
     check_status_after_killing_one_user_is_correct,
     killing_more_users_than_available_is_correct,
     killing_many_users_in_a_big_test_is_correct,
     stop_non_running_scenario_fails,
     stop_running_scenario_with_no_users_immediately_terminates,
     stop_running_scenario_with_users_stays_in_finished,
     stop_running_scenario_with_users_eventually_terminates,
     interarrival_equal_zero_starts_all_users_at_once,
     scenario_with_state_and_crashing_in_terminate_run_fine,
     scenario_missing_start_callback_fails,
     scenario_with_failing_init_fails
    ].


%% setup and teardown
init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    application:ensure_all_started(amoc),
    Config.

end_per_testcase(_TestCase, Config) ->
    application:stop(amoc),
    Config.

%% test cases
no_scenario_running_status_is_idle(_) ->
    Status = amoc_controller:get_status(),
    ?assertMatch(idle, Status).

disable_controller_returns_status_disable(_) ->
    amoc_controller:disable(),
    Status = amoc_controller:get_status(),
    ?assertMatch(disabled, Status).

disable_controller_then_cant_start_scenario(_) ->
    amoc_controller:disable(),
    Ret = do_start_scenario(testing_scenario),
    ?assertMatch({error, {invalid_status, disabled}}, Ret).

start_non_existing_scenario_fails(_) ->
    Scenario = 'non_existing_come_on_dont_do_this_#(*&$%*(@_))',
    Ret = do_start_scenario(Scenario),
    ?assertMatch({error, {no_such_scenario, Scenario}}, Ret).

start_scenario_without_vars_fails(_) ->
    Ret = do_start_scenario(testing_scenario),
    ?assertMatch({error, {parameters_verification_failed, [{testing_var1, _, _}]}}, Ret).

start_scenario_runs_fine(_) ->
    Ret = do_start_scenario(testing_scenario, test_helpers:regular_vars()),
    ?assertMatch(ok, Ret).

start_scenario_check_status(_) ->
    do_start_scenario(testing_scenario, test_helpers:regular_vars()),
    Status = amoc_controller:get_status(),
    ?assertMatch(
       {running, #{scenario := testing_scenario,
                   currently_running_users := 0,
                   highest_user_id := 0}},
       Status).

add_users_to_started_scenario(_) ->
    do_start_scenario(testing_scenario, test_helpers:regular_vars()),
    Ret = amoc_controller:add_users(1, 10),
    ?assertMatch(ok, Ret).

add_bad_range_of_users_to_started_scenario_fails(_) ->
    do_start_scenario(testing_scenario, test_helpers:regular_vars()),
    Ret = amoc_controller:add_users(10, 1),
    ?assertMatch({error, invalid_range}, Ret).

add_users_scenario_not_started_fails(_) ->
    Ret = amoc_controller:add_users(1, 10),
    ?assertMatch({error, {invalid_status, idle}}, Ret).

remove_users_scenario_not_started_fails(_) ->
    Ret = amoc_controller:add_users(1, 10),
    ?assertMatch({error, {invalid_status, idle}}, Ret).

check_status_with_running_users_is_correct(_) ->
    do_start_scenario(testing_scenario, test_helpers:regular_vars()),
    StartId = 6,
    EndId = 10,
    amoc_controller:add_users(6, 10),
    test_helpers:wait_until_scenario_has_users(testing_scenario, EndId - StartId + 1, EndId).

check_status_after_killing_one_user_is_correct(_) ->
    do_start_scenario(testing_scenario, test_helpers:regular_vars()),
    NumOfUsers = 10,
    amoc_controller:add_users(1, NumOfUsers),
    test_helpers:wait_until_scenario_has_users(testing_scenario, NumOfUsers, NumOfUsers),
    Ret = amoc_controller:remove_users(1, true),
    ?assertMatch({ok, 1}, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario, NumOfUsers - 1, NumOfUsers).

killing_more_users_than_available_is_correct(_) ->
    do_start_scenario(testing_scenario, test_helpers:regular_vars()),
    NumOfUsers = 10,
    amoc_controller:add_users(1, NumOfUsers),
    test_helpers:wait_until_scenario_has_users(testing_scenario, NumOfUsers, NumOfUsers),
    Ret = amoc_controller:remove_users(999, true),
    ?assertMatch({ok, 10}, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 0, NumOfUsers).

killing_many_users_in_a_big_test_is_correct(_) ->
    do_start_scenario(testing_scenario, test_helpers:regular_vars()),
    NumOfUsers = 200,
    amoc_controller:add_users(1, NumOfUsers),
    test_helpers:wait_until_scenario_has_users(testing_scenario, NumOfUsers, NumOfUsers),
    Ret = amoc_controller:remove_users(120, true),
    ?assertMatch({ok, 120}, Ret),
    test_helpers:wait_until_scenario_has_users(testing_scenario, 80, NumOfUsers).

stop_non_running_scenario_fails(_) ->
    Ret = amoc_controller:stop_scenario(),
    ?assertMatch({error, {invalid_status, idle}}, Ret).

stop_running_scenario_with_no_users_immediately_terminates(_) ->
    do_start_scenario(testing_scenario, test_helpers:regular_vars()),
    Ret = amoc_controller:stop_scenario(),
    ?assertMatch(ok, Ret),
    Status = amoc_controller:get_status(),
    ?assertMatch({finished, testing_scenario}, Status).

stop_running_scenario_with_users_stays_in_finished(_) ->
    do_start_scenario(testing_scenario, test_helpers:regular_vars()),
    NumOfUsers = 10,
    amoc_controller:add_users(1, NumOfUsers),
    test_helpers:wait_until_scenario_has_users(testing_scenario, NumOfUsers, NumOfUsers),
    Ret = amoc_controller:stop_scenario(),
    Status = amoc_controller:get_status(),
    ?assertMatch(ok, Ret),
    ?assertMatch({terminating, testing_scenario}, Status).

stop_running_scenario_with_users_eventually_terminates(_) ->
    do_start_scenario(testing_scenario, test_helpers:regular_vars()),
    NumOfUsers = 10,
    amoc_controller:add_users(1, NumOfUsers),
    test_helpers:wait_until_scenario_has_users(testing_scenario, NumOfUsers, NumOfUsers),
    amoc_controller:stop_scenario(),
    WaitUntilFun = fun amoc_controller:get_status/0,
    WaitUntilValue = {finished, testing_scenario},
    wait_helper:wait_until(WaitUntilFun, WaitUntilValue).

interarrival_equal_zero_starts_all_users_at_once(_) ->
    Vars = [{interarrival, 0}, {testing_var1, def1} | test_helpers:other_vars_to_keep_quiet()],
    do_start_scenario(testing_scenario, Vars),
    NumOfUsers = 1000,
    amoc_controller:add_users(1, NumOfUsers),
    Extra = #{time_left => 25, sleep_time => 5},
    test_helpers:wait_until_scenario_has_users(testing_scenario, NumOfUsers, NumOfUsers, Extra).

scenario_with_state_and_crashing_in_terminate_run_fine(_) ->
    do_start_scenario(testing_scenario_with_state, test_helpers:regular_vars_with_state()),
    NumOfUsers = 10,
    amoc_controller:add_users(1, NumOfUsers),
    test_helpers:wait_until_scenario_has_users(testing_scenario_with_state, NumOfUsers, NumOfUsers),
    amoc_controller:stop_scenario(),
    WaitUntilFun = fun amoc_controller:get_status/0,
    WaitUntilValue = {finished, testing_scenario_with_state},
    wait_helper:wait_until(WaitUntilFun, WaitUntilValue).

scenario_missing_start_callback_fails(_) ->
    Ret = do_start_scenario(testing_scenario_without_callbacks, test_helpers:regular_vars()),
    ?assertMatch({error, _}, Ret).

scenario_with_failing_init_fails(_) ->
    Ret = do_start_scenario(testing_scenario_with_error_in_init, test_helpers:regular_vars()),
    ?assertMatch({error, _}, Ret).

%% helpers
do_start_scenario(Scenario) ->
    do_start_scenario(Scenario, []).

do_start_scenario(Scenario, Config) ->
    Vars = test_helpers:other_vars_to_keep_quiet() ++ Config,
    amoc_controller:start_scenario(Scenario, Vars).
