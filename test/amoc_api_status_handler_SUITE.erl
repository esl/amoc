-module(amoc_api_status_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([returns_up_when_amoc_up_and_idle/1,
         returns_up_when_amoc_up_and_disabled/1,
         returns_up_when_amoc_up_and_in_error/1,
         returns_up_when_amoc_up_and_running/1,
         returns_up_when_amoc_up_and_terminating/1,
         returns_up_when_amoc_up_and_finished/1,
         returns_down_when_api_up_and_amoc_down/1,
         returns_nodes_list_when_amoc_up/1]).

-define(STATUS_PATH, "/status").
-define(NODES_PATH, "/nodes").

all() ->
    [returns_up_when_amoc_up_and_idle,
     returns_up_when_amoc_up_and_disabled,
     returns_up_when_amoc_up_and_in_error,
     returns_up_when_amoc_up_and_running,
     returns_up_when_amoc_up_and_terminating,
     returns_up_when_amoc_up_and_finished,
     returns_down_when_api_up_and_amoc_down,
     returns_nodes_list_when_amoc_up].

init_per_testcase(TC, Config) ->
    amoc_api_helper:start_amoc(),
    meck:new(amoc_controller, []),
    meck:new(amoc_config_scenario, []),
    meck:new(amoc_cluster, []),
    Config.

end_per_testcase(_, _Config) ->
    meck:unload(),
    amoc_api_helper:stop_amoc().

returns_up_when_amoc_up_and_idle(_Config) ->
    %% given
    EnvMap = given_amoc_envs_are_set(),
    ControllerMap = given_amoc_controller_is_mocked(idle),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?STATUS_PATH),
    %% then
    ExpectedBody = #{<<"amoc_status">> => <<"up">>,
                     <<"controller">> => ControllerMap,
                     <<"env">> => EnvMap},
    ?assertEqual(200, CodeHttp),
    ?assertEqual(ExpectedBody, Body).

returns_up_when_amoc_up_and_disabled(_Config) ->
    %% given
    EnvMap = given_amoc_envs_are_set(),
    ControllerMap = given_amoc_controller_is_mocked(disabled),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?STATUS_PATH),
    %% then
    ExpectedBody = #{<<"amoc_status">> => <<"up">>,
                     <<"controller">> => ControllerMap,
                     <<"env">> => EnvMap},
    ?assertEqual(200, CodeHttp),
    ?assertEqual(ExpectedBody, Body).

returns_up_when_amoc_up_and_in_error(_Config) ->
    %% given
    EnvMap = given_amoc_envs_are_set(),
    ControllerMap = given_amoc_controller_is_mocked(error),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?STATUS_PATH),
    %% then
    ExpectedBody = #{<<"amoc_status">> => <<"up">>,
                     <<"controller">> => ControllerMap,
                     <<"env">> => EnvMap},
    ?assertEqual(200, CodeHttp),
    ?assertEqual(ExpectedBody, Body).

returns_up_when_amoc_up_and_running(_Config) ->
    %% given
    EnvMap = given_amoc_envs_are_set(),
    ControllerMap = given_amoc_controller_is_mocked(running),
    SettingsMap = given_amoc_config_scenario_is_mocked(),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?STATUS_PATH),
    %% then
    ControllerStatus = ControllerMap#{<<"settings">> => SettingsMap},
    ExpectedBody = #{<<"amoc_status">> => <<"up">>,
                     <<"controller">> => ControllerStatus,
                     <<"env">> => EnvMap},
    ?assertEqual(200, CodeHttp),
    ?assertEqual(ExpectedBody, Body).

returns_up_when_amoc_up_and_terminating(_Config) ->
    %% given
    EnvMap = given_amoc_envs_are_set(),
    ControllerMap = given_amoc_controller_is_mocked(terminating),
    SettingsMap = given_amoc_config_scenario_is_mocked(),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?STATUS_PATH),
    %% then
    ControllerStatus = ControllerMap#{<<"settings">> => SettingsMap},
    ExpectedBody = #{<<"amoc_status">> => <<"up">>,
                     <<"controller">> => ControllerStatus,
                     <<"env">> => EnvMap},
    ?assertEqual(200, CodeHttp),
    ?assertEqual(ExpectedBody, Body).

returns_up_when_amoc_up_and_finished(_Config) ->
    %% given
    EnvMap = given_amoc_envs_are_set(),
    ControllerMap = given_amoc_controller_is_mocked(finished),
    SettingsMap = given_amoc_config_scenario_is_mocked(),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?STATUS_PATH),
    %% then
    ControllerStatus = ControllerMap#{<<"settings">> => SettingsMap},
    ExpectedBody = #{<<"amoc_status">> => <<"up">>,
                     <<"controller">> => ControllerStatus,
                     <<"env">> => EnvMap},
    ?assertEqual(200, CodeHttp),
    ?assertEqual(ExpectedBody, Body).

returns_down_when_api_up_and_amoc_down(_Config) ->
    %% given
    given_amoc_app_is_down(),
    EnvMap = given_amoc_envs_are_set(),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?STATUS_PATH),
    %% then
    ExpectedBody = #{<<"amoc_status">> => <<"down">>,
                     <<"env">> => EnvMap},
    ?assertEqual(200, CodeHttp),
    ?assertEqual(ExpectedBody, Body).

returns_nodes_list_when_amoc_up(_Config) ->
    %% given
    NodesMap = given_prepared_nodes(),
    %% when
    {CodeHttp, JSON} = amoc_api_helper:get(?NODES_PATH),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertEqual( #{<<"nodes">> =>NodesMap}, JSON).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec given_amoc_app_is_down() -> any().
given_amoc_app_is_down() ->
    application:stop(amoc).

given_amoc_envs_are_set() ->
    [amoc_config_helper:set_os_env(K, V) || {K, V} <- settings()],
    AmocEnvs = [begin
                    EnvName = amoc_config_helper:env_name(K),
                    EnvValue = amoc_config_helper:format_value(V),
                    {list_to_binary(EnvName), list_to_binary(EnvValue)}
                end || {K, V} <- settings()],
    maps:from_list(AmocEnvs).

given_prepared_nodes() ->
    ConnectionStatus = #{connected => [test1],
                         failed_to_connect => [test2],
                         connection_lost => [test3, test2]},
    meck:expect(amoc_cluster, get_status, fun() -> ConnectionStatus end),
    #{atom_to_binary(node(), utf8) => <<"up">>, <<"test1">> => <<"up">>,
      <<"test2">> => <<"down">>, <<"test3">> => <<"down">>}.

settings() ->
    [{some_map, #{a => b}},
     {some_list, [a, b, c]},
     {some_tuple, {a, b, c}},
     {some_string, "aaa"},
     {some_binary, <<"bbb">>},
     {some_atom, 'ATOM'},
     {some_int, 4},
     {some_float, 4.6}].

given_amoc_controller_is_mocked(idle) ->
    meck:expect(amoc_controller, get_status, fun() -> idle end),
    #{<<"status">> => <<"idle">>};
given_amoc_controller_is_mocked(disabled) ->
    meck:expect(amoc_controller, get_status, fun() -> disabled end),
    #{<<"status">> => <<"disabled">>};
given_amoc_controller_is_mocked(error) ->
    meck:expect(amoc_controller, get_status, fun() -> {error, mocked} end),
    #{<<"status">> => <<"error">>, <<"error">> => <<"mocked">>};
given_amoc_controller_is_mocked(running) ->
    Status = {running, test_scenario, 10, 10},
    meck:expect(amoc_controller, get_status, fun() -> Status end),
    #{<<"status">> => <<"running">>, <<"scenario">> => <<"test_scenario">>,
      <<"number_of_users">> => 10};
given_amoc_controller_is_mocked(terminating) ->
    meck:expect(amoc_controller, get_status, fun() -> {terminating, test_scenario} end),
    #{<<"status">> => <<"terminating">>, <<"scenario">> => <<"test_scenario">>};
given_amoc_controller_is_mocked(finished) ->
    meck:expect(amoc_controller, get_status, fun() -> {finished, test_scenario} end),
    #{<<"status">> => <<"finished">>, <<"scenario">> => <<"test_scenario">>}.

given_amoc_config_scenario_is_mocked()->
    ConfigurationMap = maps:from_list([{K, #{value => V}} || {K, V} <- settings()]),
    meck:expect(amoc_config_scenario, get_current_configuration,
                fun() -> {ok, ConfigurationMap} end),
    Settings = [begin
                    EnvName = atom_to_binary(K, utf8),
                    EnvValue = amoc_config_helper:format_value(V),
                    {EnvName, list_to_binary(EnvValue)}
                end || {K, V} <- settings()],
    maps:from_list(Settings).
