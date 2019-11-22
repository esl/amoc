-module(amoc_api_scenario_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("scenario_template.hrl").


-define(SCENARIOS_DIR_S, "scenarios").
-define(SAMPLE_SCENARIO_S, "sample_test1.erl").
-define(SAMPLE_SCENARIO_A, sample_test1).
-define(SAMPLE_GOOD_SCENARIO_PATH, "/scenarios/sample_test1").
-define(SAMPLE_BAD_SCENARIO_PATH, "/scenarios/non_existing_scenario").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
         get_scenario_status_returns_200_when_scenario_exists/1,
         get_scenario_status_returns_404_when_scenario_not_exists/1,
         get_scenario_status_returns_running_when_scenario_is_running/1,
         get_scenario_status_returns_finished_when_scenario_is_ended/1,
         get_scenario_status_returns_loaded_when_scenario_is_not_running/1,
         patch_scenario_returns_404_when_scenario_not_exists/1,
         patch_scenario_returns_400_when_malformed_request/1,
         patch_scenario_returns_200_when_request_ok_and_module_exists/1,
         patch_scenario_returns_200_when_request_with_user_batches_ok_and_module_exists/1
        ]).


all() ->
    [
     get_scenario_status_returns_200_when_scenario_exists,
     get_scenario_status_returns_404_when_scenario_not_exists,
     get_scenario_status_returns_running_when_scenario_is_running,
     get_scenario_status_returns_finished_when_scenario_is_ended,
     get_scenario_status_returns_loaded_when_scenario_is_not_running,
     patch_scenario_returns_404_when_scenario_not_exists,
     patch_scenario_returns_400_when_malformed_request,
     patch_scenario_returns_200_when_request_ok_and_module_exists,
     patch_scenario_returns_200_when_request_with_user_batches_ok_and_module_exists
    ].

init_per_testcase(
  patch_scenario_returns_200_when_request_ok_and_module_exists,
  Config) ->
    mock_amoc_dist_do(),
    create_env(Config),
    Config;

init_per_testcase(
  patch_scenario_returns_200_when_request_with_user_batches_ok_and_module_exists,
  Config) ->
    mock_amoc_dist_do(),
    mock_amoc_controller_add_batches(),
    create_env(Config),
    Config;

init_per_testcase(_, Config) ->
    create_env(Config),
    Config.

end_per_testcase(
  patch_scenario_returns_200_when_request_ok_and_module_exists,
  _Config) ->
    ok = meck:unload(amoc_dist),
    destroy_env();

end_per_testcase(
  patch_scenario_returns_200_when_request_with_user_batches_ok_and_module_exists,
  _Config) ->
    ok = meck:unload(amoc_dist),
    ok = meck:unload(amoc_controller),
    destroy_env();

end_per_testcase(_, _Config) ->
    destroy_env().

get_scenario_status_returns_200_when_scenario_exists(_Config) ->
    %% given
    given_amoc_dist_mocked_with_test_status(true),
    %% when
    {CodeHttp, _Body} = amoc_api_helper:get(?SAMPLE_GOOD_SCENARIO_PATH),
    %% then
    %% Maybe check Body, as answer format will be ready
    ?assertEqual(200, CodeHttp),
    %% cleanup
    cleanup_amoc_dist().

get_scenario_status_returns_404_when_scenario_not_exists(_Config) ->
    %% when
    {CodeHttp, _Body} = amoc_api_helper:get(?SAMPLE_BAD_SCENARIO_PATH),
    %% then
    %% Maybe check Body, as answer format will be ready
    ?assertEqual(404, CodeHttp).

get_scenario_status_returns_running_when_scenario_is_running(_Config) ->
    %% given
    given_amoc_dist_mocked_with_test_status(running),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?SAMPLE_GOOD_SCENARIO_PATH),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch({[{<<"scenario_status">>, <<"running">>}]}, Body),
    %% cleanup
    cleanup_amoc_dist().

get_scenario_status_returns_finished_when_scenario_is_ended(_Config) ->
    %% given
    given_amoc_dist_mocked_with_test_status(finished),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?SAMPLE_GOOD_SCENARIO_PATH),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch({[{<<"scenario_status">>, <<"finished">>}]}, Body),
    %% cleanup
    cleanup_amoc_dist().

get_scenario_status_returns_loaded_when_scenario_is_not_running(_Config) ->
    %% given
    given_amoc_dist_mocked_with_test_status(loaded),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?SAMPLE_GOOD_SCENARIO_PATH),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch({[{<<"scenario_status">>, <<"loaded">>}]}, Body),
    %% cleanup
    cleanup_amoc_dist().

patch_scenario_returns_404_when_scenario_not_exists(_Config) ->
    %% given
    RequestBody = jiffy:encode({[{users,30}]}),
    %% when
    {CodeHttp, _Body} = amoc_api_helper:patch(
                            ?SAMPLE_BAD_SCENARIO_PATH, RequestBody),
    %% then
    %% Maybe check Body, as answer format will be ready
    ?assertEqual(404, CodeHttp).

patch_scenario_returns_400_when_malformed_request(_Config) ->
    %% given
    RequestBody = jiffy:encode({[{bad_key, bad_value}]}),
    %% when
    {CodeHttp, _Body} = amoc_api_helper:patch(
                            ?SAMPLE_GOOD_SCENARIO_PATH, RequestBody),
    %% then
    %% Maybe check Body, as answer format will be ready
    ?assertEqual(400, CodeHttp).


patch_scenario_returns_200_when_request_ok_and_module_exists(_Config) ->
    %% given
    RequestBody = jiffy:encode({[{users, 10}]}),
    %% when
    {CodeHttp, _Body} = amoc_api_helper:patch(
                            ?SAMPLE_GOOD_SCENARIO_PATH, RequestBody),
    %% then
    %% Maybe check Body, as answer format will be ready
    meck:wait(amoc_dist, do, ['sample_test1', 1, 10], 2000),
    ?assertEqual(200, CodeHttp).

patch_scenario_returns_200_when_request_with_user_batches_ok_and_module_exists(_Config) ->
    %% given
    RequestBody = jiffy:encode({[{users, 10}, {batches, 10}]}),
    %% when
    {CodeHttp, _Body} = amoc_api_helper:patch(
                          ?SAMPLE_GOOD_SCENARIO_PATH, RequestBody),
    %% then
    %% Maybe check Body, as answer format will be ready
    meck:wait(amoc_dist, do, ['sample_test1', 1, 10], 2000),
    meck:wait(amoc_controller, add_batches, [10, 'sample_test1'], 2000),
    ?assertEqual(200, CodeHttp).

%% Helpers

create_env(Config) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(amoc),
    ScenarioContent = ?DUMMY_SCENARIO_MODULE(?SAMPLE_SCENARIO_A),
    ok = amoc_scenario:install_scenario(?SAMPLE_SCENARIO_A,ScenarioContent).

destroy_env() ->
    ok.


-spec given_amoc_dist_mocked_with_test_status(
        amoc_controller:scenario_status()) -> ok.
given_amoc_dist_mocked_with_test_status(Value) ->
    meck:new(amoc_dist, [passtrough]),
    meck:expect(amoc_dist, test_status, fun(_) -> Value end).

-spec mock_amoc_dist_do() -> ok.
mock_amoc_dist_do() ->
    ok = meck:new(amoc_dist, [unstick]),
    Fun = fun(_,1,_) -> [ok] end,
    ok = meck:expect(amoc_dist, do, Fun).

-spec mock_amoc_controller_add_batches() -> ok.
mock_amoc_controller_add_batches() ->
    ok = meck:new(amoc_controller, [unstick]),
    Fun = fun(BatchCount, Scenario)
                when is_integer(BatchCount) and is_atom(Scenario) -> ok
          end,
    ok = meck:expect(amoc_controller, add_batches, Fun).

-spec cleanup_amoc_dist() -> ok.
cleanup_amoc_dist() ->
    meck:unload(amoc_dist).
