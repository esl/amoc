-module(amoc_api_execution_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("scenario_template.hrl").

-compile(export_all).

all() ->
    [start_scenario,
     start_scenario_with_users_and_settings,
     fail_to_start_non_existing_scenario,
     fail_to_start_scenario_without_name,
     stop_scenario,
     fail_to_stop_scenario_when_not_running,
     add_users,
     add_users_on_nodes,
     fail_to_add_users_when_not_running,
     remove_users,
     remove_users_on_nodes,
     fail_to_remove_users_when_not_running,
     update_settings,
     update_settings_on_nodes,
     fail_to_update_settings_when_not_running].

%% Setup and teardown

init_per_suite(Config) ->
    amoc_api_helper:start_amoc(),
    ok = amoc_scenario:install_module(sample_test, dummy_scenario_module()),
    Config.

end_per_suite(_Config) ->
    amoc_api_helper:remove_module(sample_test),
    amoc_api_helper:stop_amoc().

init_per_testcase(TC, Config) ->
    meck:new(amoc_dist, [passthrough]),
    setup_meck(TC),
    Config.

end_per_testcase(_TC, _Config) ->
    meck:unload(amoc_dist).

setup_meck(start_scenario) ->
    meck:expect(amoc_dist, do, fun(sample_test, 0, []) -> {ok, mocked} end);
setup_meck(start_scenario_with_users_and_settings) ->
    meck:expect(amoc_dist, do, fun(sample_test, 10, Settings) ->
                                       ?assertEqual(lists:sort(Settings), lists:sort(settings())),
                                       {ok, mocked}
                               end);
setup_meck(stop_scenario) ->
    meck:expect(amoc_dist, stop, fun() -> {ok, mocked} end);
setup_meck(add_users) ->
    meck:expect(amoc_dist, add, fun(10) -> {ok, mocked} end);
setup_meck(add_users_on_nodes) ->
    meck:expect(amoc_dist, add, fun(10, [node1@host1, node2@host2]) -> {ok, mocked} end);
setup_meck(remove_users) ->
    meck:expect(amoc_dist, remove, fun(10, false) -> {ok, mocked} end);
setup_meck(remove_users_on_nodes) ->
    meck:expect(amoc_dist, remove, fun(10, false, [node1@host1, node2@host2]) -> {ok, mocked} end);
setup_meck(update_settings) ->
    meck:expect(amoc_dist, update_settings,
                fun(Settings) ->
                        ?assertEqual(lists:sort(Settings), lists:sort(settings())),
                        {ok, mocked}
                end);
setup_meck(update_settings_on_nodes) ->
    meck:expect(amoc_dist, update_settings,
                fun(Settings, [node1@host1, node2@host2]) ->
                        ?assertEqual(lists:sort(Settings), lists:sort(settings())),
                        {ok, mocked}
                end);
setup_meck(_TC) ->
    ok.

%% Test cases

start_scenario(_Config) ->
    JsonBody = #{scenario => sample_test},
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/start", JsonBody),
    ?assertEqual(200, HttpCode).

start_scenario_with_users_and_settings(_Config) ->
    JsonBody = #{scenario => sample_test, users => 10, settings => settings_for_json()},
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/start", JsonBody),
    ?assertEqual(200, HttpCode).

fail_to_start_non_existing_scenario(_Config) ->
    JsonBody = #{scenario => bad_scenario},
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/start", JsonBody),
    ?assertEqual(409, HttpCode).

fail_to_start_scenario_without_name(_Config) ->
    JsonBody = #{users=> 10},
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/start", JsonBody),
    ?assertEqual(400, HttpCode).

stop_scenario(_Config) ->
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/stop"),
    ?assertEqual(200, HttpCode).

fail_to_stop_scenario_when_not_running(_Config) ->
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/stop"),
    ?assertEqual(409, HttpCode).

add_users(_Config) ->
    JsonBody = #{users=> 10},
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/add_users", JsonBody),
    ?assertEqual(200, HttpCode).

add_users_on_nodes(_Config) ->
    JsonBody = #{users=> 10, nodes=> [node1@host1, node2@host2]},
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/add_users", JsonBody),
    ?assertEqual(200, HttpCode).

fail_to_add_users_when_not_running(_Config) ->
    JsonBody = #{users=> 10},
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/add_users", JsonBody),
    ?assertEqual(409, HttpCode).

remove_users(_Config) ->
    JsonBody = #{users=> 10},
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/remove_users", JsonBody),
    ?assertEqual(200, HttpCode).

remove_users_on_nodes(_Config) ->
    JsonBody = #{users=> 10, nodes=> [node1@host1, node2@host2]},
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/remove_users", JsonBody),
    ?assertEqual(200, HttpCode).

fail_to_remove_users_when_not_running(_Config) ->
    JsonBody = #{users=> 10},
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/remove_users", JsonBody),
    ?assertEqual(409, HttpCode).

update_settings(_Config) ->
    JsonBody = #{settings=> settings_for_json()},
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/update_settings", JsonBody),
    ?assertEqual(200, HttpCode).

update_settings_on_nodes(_Config) ->
    JsonBody = #{settings=> settings_for_json(), nodes=>[node1@host1, node2@host2]},
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/update_settings", JsonBody),
    ?assertEqual(200, HttpCode).

fail_to_update_settings_when_not_running(_Config) ->
    JsonBody = #{settings=> settings_for_json()},
    {HttpCode, _Body} = amoc_api_helper:patch("/execution/update_settings", JsonBody),
    ?assertEqual(409, HttpCode).

%% Test helpers

sample_scenario_path() ->
    "/execution/start/sample_test".

dummy_scenario_module() ->
    ?DUMMY_SCENARIO_MODULE(sample_test).

settings_for_json() ->
    #{some_map => <<"#{a=>b}">>,
      some_list => <<"[a, b, c]">>,
      some_tuple => <<"{a, b, c}">>,
      some_string => <<"\"aaa\"">>,
      some_binary => <<"<<\"bbb\">>">>,
      some_atom => <<"'ATOM'">>,
      some_int => <<"4">>,
      some_float => <<"4.6">>}.

settings() ->
    [{some_map, #{a => b}},
     {some_list, [a, b, c]},
     {some_tuple, {a, b, c}},
     {some_string, "aaa"},
     {some_binary, <<"bbb">>},
     {some_atom, 'ATOM'},
     {some_int, 4},
     {some_float, 4.6}].
