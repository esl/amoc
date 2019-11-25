-module(amoc_api_scenarios_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("scenario_template.hrl").

-define(SCENARIOS_URL_S, "/scenarios").
-define(SCENARIOS_DIR_S, filename:join(code:priv_dir(amoc), "scenarios")).
-define(SCENARIOS_EBIN_DIR_S, filename:join(code:priv_dir(amoc), "scenarios_ebin")).
-define(SAMPLE_SCENARIO_S, "sample_test.erl").
-define(SAMPLE_SCENARIO_BEAM_S, "sample_test.beam").
-define(SAMPLE_SCENARIO_A, sample_test).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
         get_scenarios_returns_200_and_scenarios_list_when_requested/1,
         post_scenarios_returns_400_when_malformed_request/1,
         post_scenarios_returns_200_and_compile_error_when_scenario_source_not_valid/1,
         post_scenarios_returns_200_and_when_scenario_valid/1
        ]).


all() ->
    [
     get_scenarios_returns_200_and_scenarios_list_when_requested,
     post_scenarios_returns_400_when_malformed_request,
     post_scenarios_returns_200_and_compile_error_when_scenario_source_not_valid,
     post_scenarios_returns_200_and_when_scenario_valid
    ].


init_per_testcase(_, Config) ->
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(amoc),
    Config.

end_per_testcase(post_scenarios_returns_200_and_when_scenario_valid, _Config) ->
    ok = file:delete(filename:join([?SCENARIOS_EBIN_DIR_S,
                                    ?SAMPLE_SCENARIO_BEAM_S])),
    ok = file:delete(filename:join([?SCENARIOS_DIR_S,
                                    ?SAMPLE_SCENARIO_S]));

end_per_testcase(_, _Config) ->
    ok.

get_scenarios_returns_200_and_scenarios_list_when_requested(_Config) ->
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?SCENARIOS_URL_S),
    %% then
    {[{Key, Scenarios}]} = Body,
    ?assertEqual(200, CodeHttp),
    ?assertEqual(<<"scenarios">>, Key),
    ?assert(is_list(Scenarios)).

post_scenarios_returns_400_when_malformed_request(_Config) ->
    %% given
    RequestBody = jiffy:encode({[{keyname_typo, ?SAMPLE_SCENARIO_A}]}),
    %% when
    {CodeHttp, Body} = amoc_api_helper:post(?SCENARIOS_URL_S, RequestBody),
    %% then
    ?assertEqual(400, CodeHttp),
    ?assertEqual({[{<<"error">>, <<"wrong_json">>}]},
                 Body).

post_scenarios_returns_200_and_compile_error_when_scenario_source_not_valid(_Config) ->
    %% given
    RequestBody = jiffy:encode({[
                              {scenario, ?SAMPLE_SCENARIO_A},
                              {module_source, invalid_source}
                              ]}),
    %% when
    {CodeHttp, Body} = amoc_api_helper: post(?SCENARIOS_URL_S, RequestBody),
    ScenarioFileSource = filename:join([?SCENARIOS_DIR_S,
                                        ?SAMPLE_SCENARIO_S]),
    %% then
    ?assertNot(filelib:is_regular(ScenarioFileSource)),
    ?assertEqual(200, CodeHttp),
    Error = <<"[{\"", (list_to_binary(ScenarioFileSource))/binary, "\",\n  [{1,erl_parse,[\"",
              "syntax error before: \",[]]}]},\n {\"", (list_to_binary(ScenarioFileSource))/binary,
              "\",\n  [{1,erl_lint,undefined_module}]}]">>,
    ?assertEqual({[{<<"compile">>,Error}]},Body).

post_scenarios_returns_200_and_when_scenario_valid(_Config) ->
    %% given
    ScenarioContent = ?DUMMY_SCENARIO_MODULE(?SAMPLE_SCENARIO_A),
    RequestBody = jiffy:encode({[
                              {scenario, ?SAMPLE_SCENARIO_A},
                              {module_source, ScenarioContent}
                               ]}),
    %% when
    {CodeHttp, Body} = amoc_api_helper:post(?SCENARIOS_URL_S, RequestBody),
    ScenarioFileSource = filename:join([?SCENARIOS_DIR_S,
                                        ?SAMPLE_SCENARIO_S]),
    ScenarioFileBeam = filename:join([?SCENARIOS_EBIN_DIR_S,
                                      ?SAMPLE_SCENARIO_BEAM_S]),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertEqual({[{<<"compile">>, <<"ok">>}]}, Body),
    ?assert(filelib:is_regular(ScenarioFileSource)),
    ?assert(filelib:is_regular(ScenarioFileBeam)).

%% Helpers


