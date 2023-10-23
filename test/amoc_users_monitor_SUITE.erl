-module(amoc_users_monitor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() ->
    [
        error_when_amoc_users_monitor_is_disabled,
        amoc_users_monitor_keeps_track_of_users
    ].

%% Setup and teardown

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(error_when_amoc_users_monitor_is_disabled, Config) ->
    application:set_env(amoc, amoc_users_monitor, disabled),
    common_init_per_testcase(Config);
init_per_testcase(amoc_users_monitor_keeps_track_of_users, Config) ->
    application:set_env(amoc, amoc_users_monitor, enabled),
    common_init_per_testcase(Config).

common_init_per_testcase(Config) ->
    application:ensure_all_started(amoc),
    meck:new(amoc_dist, []),
    Config.

end_per_testcase(_TC, _Config) ->
    meck:unload(amoc_dist),
    application:stop(amoc).

%% Test cases

error_when_amoc_users_monitor_is_disabled(_Config) ->
    ?assertEqual(disabled, amoc_users_monitor:is_users_monitor_enabled()),
    ?assertEqual("Amoc users monitor is disabled.", amoc_users_monitor:get_all_users_pids()).

amoc_users_monitor_keeps_track_of_users(_Config) ->
    ?assertEqual(enabled, amoc_users_monitor:is_users_monitor_enabled()),
    ok = amoc:do(dummy_scenario_test, 10, []),

    timer:sleep(1_000),

    #{monitored_users := MonitoredUsers,
      children_which_died_normally := DiedNormally,
      children_which_died_unnormaly := DiedUnnormally} =
        amoc_users_monitor:get_all_users_pids(),
    ?assertEqual(6, sets:size(MonitoredUsers)),
    ?assertEqual(2, length(DiedNormally)),
    ?assertEqual(2, length(DiedUnnormally)),
    ?assertMatch([{_, test_crush_reason} | _], DiedUnnormally).
