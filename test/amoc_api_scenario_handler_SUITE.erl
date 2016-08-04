-module(amoc_api_scenario_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SCENARIOS_DIR_S, "scenarios").
-define(SAMPLE_SCENARIO_S, "sample_test.erl").
-define(SAMPLE_SCENARIO_A, sample_test).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
         get_scenario_status_returns_200_when_scenario_exists/1,
         get_scenario_status_returns_404_when_scenario_not_exists/1,
         get_scenario_status_returns_running_when_scenario_is_running/1,
         get_scenario_status_returns_finished_when_scenario_is_ended/1,
         get_scenario_status_returns_loaded_when_scenario_is_not_running/1,
         patch_scenario_returns_404_when_scenario_not_exists/1,
         patch_scenario_returns_400_when_malformed_request/1,
         patch_scenario_returns_200_when_request_ok_and_module_exists/1
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
     patch_scenario_returns_200_when_request_ok_and_module_exists
    ].

init_per_testcase(
  patch_scenario_returns_200_when_request_ok_and_module_exists,
  Config) ->
    ok = meck:new(amoc_dist, [unstick]),
    Fun = fun(_,1,_) -> ok end,
    ok = meck:expect(amoc_dist, do, Fun),
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

end_per_testcase(_, _Config) ->
    destroy_env().

get_scenario_status_returns_200_when_scenario_exists(_Config) ->
    %% given
    given_amoc_dist_mocked_with_test_status(true),
    URL = get_url() ++ "/scenarios/sample_test",
    %% when
    {CodeHttp, _Body} = get_request(URL),
    %% then
    %% Maybe check Body, as answer format will be ready
    ?assertEqual(200, CodeHttp),
    %% cleanup
    cleanup_amoc_dist().

get_scenario_status_returns_404_when_scenario_not_exists(_Config) ->
    %% given
    URL = get_url() ++ "/scenarios/non_existing_scenario",
    %% when
    {CodeHttp, _Body} = get_request(URL),
    %% then
    %% Maybe check Body, as answer format will be ready
    ?assertEqual(404, CodeHttp).

get_scenario_status_returns_running_when_scenario_is_running(_Config) ->
    %% given
    given_amoc_dist_mocked_with_test_status(running),
    URL = get_url() ++ "/scenarios/sample_test",
    %% when
    {CodeHttp, Body} = get_request(URL),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch([{<<"scenario_status">>, <<"running">>}], Body),
    %% cleanup
    cleanup_amoc_dist().

get_scenario_status_returns_finished_when_scenario_is_ended(_Config) ->
    %% given
    given_amoc_dist_mocked_with_test_status(finished),
    URL = get_url() ++ "/scenarios/sample_test",
    %% when
    {CodeHttp, Body} = get_request(URL),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch([{<<"scenario_status">>, <<"finished">>}], Body),
    %% cleanup
    cleanup_amoc_dist().

get_scenario_status_returns_loaded_when_scenario_is_not_running(_Config) ->
    %% given
    given_amoc_dist_mocked_with_test_status(loaded),
    URL = get_url() ++ "/scenarios/sample_test",
    %% when
    {CodeHttp, Body} = get_request(URL),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch([{<<"scenario_status">>, <<"loaded">>}], Body),
    %% cleanup
    cleanup_amoc_dist().

patch_scenario_returns_404_when_scenario_not_exists(_Config) ->
    %% given
    URL = get_url() ++ "/scenarios/non_existing_scenario",
    RequestBody = jsx:encode([{users,30}]),
    %% when
    {CodeHttp, _Body} = patch_request(URL, RequestBody),
    %% then
    %% Maybe check Body, as answer format will be ready
    ?assertEqual(404, CodeHttp).

patch_scenario_returns_400_when_malformed_request(_Config) ->
    %% given
    URL = get_url() ++ "/scenarios/sample_test",
    RequestBody = jsx:encode([{bad_key, bad_value}]),
    %% when
    {CodeHttp, _Body} = patch_request(URL, RequestBody),
    %% then
    %% Maybe check Body, as answer format will be ready
    ?assertEqual(400, CodeHttp).


patch_scenario_returns_200_when_request_ok_and_module_exists(_Config) ->
    %% given
    URL = get_url() ++ "/scenarios/sample_test",
    RequestBody = jsx:encode([{users, 10}]),
    %% when
    {CodeHttp, _Body} = patch_request(URL, RequestBody),
    %% then
    %% Maybe check Body, as answer format will be ready
    meck:wait(amoc_dist, do, ['sample_test', 1, 10], 2000),
    ?assertEqual(200, CodeHttp).


%% Helpers

create_env(Config) ->
    ok = file:make_dir(?SCENARIOS_DIR_S),
    SampleScenario = filename:join([?SCENARIOS_DIR_S,
                                    ?SAMPLE_SCENARIO_S]),
    copy(data(Config, ?SAMPLE_SCENARIO_S), SampleScenario),
    {ok, _} = compile:file(SampleScenario),
    {module, _} = code:load_file(?SAMPLE_SCENARIO_A),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(amoc).

destroy_env() ->
    ok = file:delete(filename:join([?SCENARIOS_DIR_S,
                               ?SAMPLE_SCENARIO_S])),
    ok = file:del_dir(?SCENARIOS_DIR_S),
    code:purge(?SAMPLE_SCENARIO_A).

copy(Src, Dst) ->
    {ok, _} = file:copy(Src, Dst).

data(Config, Path) ->
    Dir = proplists:get_value(data_dir, Config),
    filename:join([Dir, Path]).

-spec get_url() -> string().
get_url() ->
    Port = amoc_config:get(api_port, 4000),
    "http://localhost:" ++ erlang:integer_to_list(Port).

-spec get_request(string()) -> 
    {integer(), jsx:json_term()}.
get_request(URL) ->
    Header = [],
    HTTPOpts = [],
    Opts = [],
    Result = httpc:request(get,
                           {URL, Header},
                           HTTPOpts,
                           Opts),

    {ok, {{_HttpVsn, CodeHttp, _Status}, _, Body}} = Result,
    BodyErl = case Body of
                  [] -> 
                      [];
                  _ ->
                      jsx:decode(erlang:list_to_bitstring(Body))
              end,
    {CodeHttp, BodyErl}.

-spec patch_request(string(), string()) ->
    {integer(), jsx:json_term()}.
patch_request(URL, RequestBody) ->
    Header = "",
    Type = "application/json",
    HTTPOpts = [],
    Opts = [],
    Result = httpc:request(patch,
                           {URL, Header, Type, RequestBody},
                           HTTPOpts,
                           Opts),
    {ok, {{_HttpVsn, CodeHttp, _Status},_, Body}} = Result, 
    BodyErl = case Body of
                  [] -> 
                      [];
                  _ ->
                      jsx:decode(erlang:list_to_bitstring(Body))
              end,
    {CodeHttp, BodyErl}.

-spec given_amoc_dist_mocked_with_test_status(amoc_controller:scenario_status()) -> ok.
given_amoc_dist_mocked_with_test_status(Value) ->
    meck:new(amoc_dist, [passtrough]),
    meck:expect(amoc_dist, test_status, fun(_) -> Value end).

-spec cleanup_amoc_dist() -> ok.
cleanup_amoc_dist() ->
    meck:unload(amoc_dist).
