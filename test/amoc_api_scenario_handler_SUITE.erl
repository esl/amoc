-module(amoc_api_scenario_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("scenario_template.hrl").



-define(SAMPLE_SCENARIO, sample_test).

-define(SAMPLE_GOOD_SCENARIO_PATH, "/scenarios/" ++ atom_to_list(?SAMPLE_SCENARIO)).
-define(SAMPLE_BAD_SCENARIO_PATH, "/scenarios/non_existing_scenario").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
         get_scenario_status_returns_404_when_scenario_not_exists/1,
         get_scenario_status_returns_running_when_scenario_is_running/1,
         get_scenario_status_returns_finished_when_scenario_is_ended/1,
         get_scenario_status_returns_loaded_when_scenario_exists_but_not_running/1,
         patch_scenario_returns_404_when_scenario_not_exists/1,
         patch_scenario_returns_400_when_malformed_request/1,
         patch_scenario_returns_200_when_request_ok_and_module_exists/1,
         patch_scenario_returns_200_when_request_ok_and_module_exists_w_settings/1
        ]).


all() ->
    [
     get_scenario_status_returns_404_when_scenario_not_exists,
     get_scenario_status_returns_running_when_scenario_is_running,
     get_scenario_status_returns_finished_when_scenario_is_ended,
     get_scenario_status_returns_loaded_when_scenario_exists_but_not_running,
     patch_scenario_returns_404_when_scenario_not_exists,
     patch_scenario_returns_400_when_malformed_request,
     patch_scenario_returns_200_when_request_ok_and_module_exists,
     patch_scenario_returns_200_when_request_ok_and_module_exists_w_settings
    ].

init_per_testcase(Mod, Config)
    when Mod =:= patch_scenario_returns_200_when_request_ok_and_module_exists;
         Mod =:= patch_scenario_returns_200_when_request_ok_and_module_exists_w_settings ->
    mock_amoc_dist_do(),
    create_env(Config),
    Config;

init_per_testcase(_, Config) ->
    create_env(Config),
    Config.

end_per_testcase(patch_scenario_returns_200_when_request_ok_and_module_exists, _Config) ->
    ok = meck:unload(amoc_dist),
    destroy_env();
end_per_testcase(_, _Config) ->
    destroy_env().

get_scenario_status_returns_404_when_scenario_not_exists(_Config) ->
    %% when
    {CodeHttp, _Body} = amoc_api_helper:get(?SAMPLE_BAD_SCENARIO_PATH),
    %% then
    %% Maybe check Body, as answer format will be ready
    ?assertEqual(404, CodeHttp).

get_scenario_status_returns_running_when_scenario_is_running(_Config) ->
    %% given
    given_test_status_mocked({running, ?SAMPLE_SCENARIO}),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?SAMPLE_GOOD_SCENARIO_PATH),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch({[{<<"scenario_status">>, <<"running">>}]}, Body),
    %% cleanup
    cleanup_test_status_mock().

get_scenario_status_returns_finished_when_scenario_is_ended(_Config) ->
    %% given
    given_test_status_mocked({finished, ?SAMPLE_SCENARIO}),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?SAMPLE_GOOD_SCENARIO_PATH),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch({[{<<"scenario_status">>, <<"finished">>}]}, Body),
    %% cleanup
    cleanup_test_status_mock().

get_scenario_status_returns_loaded_when_scenario_exists_but_not_running(_Config) ->
    %% given
    given_test_status_mocked({loaded, ?SAMPLE_SCENARIO}),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?SAMPLE_GOOD_SCENARIO_PATH),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch({[{<<"scenario_status">>, <<"loaded">>}]}, Body),
    %% cleanup
    cleanup_test_status_mock().

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


patch_scenario_returns_200_when_request_ok_and_module_exists_w_settings(_Config) ->
    %% given
    RequestBody = jiffy:encode({[{users, 10},
                                 {settings, {[
                                                 {some_map, <<"#{a=>b}">>},
                                                 {some_list, <<"[a, b, c]">>},
                                                 {some_tuple, <<"{a, b, c}">>},
                                                 {some_string, <<"\"aaa\"">>},
                                                 {some_binary, <<"<<\"bbb\">>">>},
                                                 {some_atom, <<"'ATOM'">>},
                                                 {some_int, <<"4">>},
                                                 {some_float, <<"4.6">>}
                                             ]}}]}),
    %% when
    {CodeHttp, _Body} = amoc_api_helper:patch(
        ?SAMPLE_GOOD_SCENARIO_PATH, RequestBody),

    Predicate = fun(S) ->
                    Settings = [{some_map, #{a => b}},
                                {some_list, [a, b, c]},
                                {some_tuple, {a, b, c}},
                                {some_string, "aaa"},
                                {some_binary, <<"bbb">>},
                                {some_atom, 'ATOM'},
                                {some_int, 4},
                                {some_float, 4.6}],
                    ?assertEqual(lists:sort(S), lists:sort(Settings)),
                    true
                end,
    SettingsMatcher = meck_matcher:new(Predicate),
    %% then
    %% Maybe check Body, as answer format will be ready
    meck:wait(amoc_dist, do, [?SAMPLE_SCENARIO, 10, SettingsMatcher], 2000),
    ?assertEqual(200, CodeHttp).

patch_scenario_returns_200_when_request_ok_and_module_exists(_Config) ->
    %% given
    RequestBody = jiffy:encode({[{users, 10}]}),
    %% when
    {CodeHttp, _Body} = amoc_api_helper:patch(
        ?SAMPLE_GOOD_SCENARIO_PATH, RequestBody),

    %% then
    %% Maybe check Body, as answer format will be ready
    meck:wait(amoc_dist, do, [?SAMPLE_SCENARIO, 10, []], 2000),
    ?assertEqual(200, CodeHttp).


%% Helpers

create_env(Config) ->
    amoc_api_helper:start_amoc(),
    ScenarioContent = ?DUMMY_SCENARIO_MODULE(?SAMPLE_SCENARIO),
    ok = amoc_scenario:install_module(?SAMPLE_SCENARIO, ScenarioContent).

destroy_env() ->
    amoc_api_helper:remove_module(?SAMPLE_SCENARIO),
    amoc_api_helper:stop_amoc().


-spec given_test_status_mocked(atom()) -> ok.
given_test_status_mocked(Value) ->
    meck:new(amoc_api_scenario_status, []),
    meck:expect(amoc_api_scenario_status, test_status, fun(_) -> Value end).

-spec mock_amoc_dist_do() -> ok.
mock_amoc_dist_do() ->
    ok = meck:new(amoc_dist, []),
    Fun = fun(_,_,_) -> {ok,anything} end,
    ok = meck:expect(amoc_dist, do, Fun).

-spec cleanup_test_status_mock() -> ok.
cleanup_test_status_mock() ->
    meck:unload(amoc_api_scenario_status).
