-module(amoc_api_scenarios_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("scenario_template.hrl").

-define(SCENARIOS_URL_S, "/scenarios").
-define(SCENARIOS_URL_U, "/scenarios/upload").

-define(SAMPLE_SCENARIO, sample_test).
-define(SAMPLE_SCENARIO_DECLARATION,
        "-module(" ++ atom_to_list(?SAMPLE_SCENARIO) ++ ").").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
         get_scenarios_returns_200_and_scenarios_list_when_requested/1,
         put_scenarios_returns_400_and_error_when_scenario_is_not_valid/1,
         put_scenarios_returns_200_and_compile_error_when_scenario_source_not_valid/1,
         put_scenarios_returns_200_when_scenario_valid/1
        ]).


all() ->
    [
     get_scenarios_returns_200_and_scenarios_list_when_requested,
     put_scenarios_returns_400_and_error_when_scenario_is_not_valid,
     put_scenarios_returns_200_and_compile_error_when_scenario_source_not_valid,
     put_scenarios_returns_200_when_scenario_valid
    ].


init_per_testcase(_, Config) ->
    amoc_api_helper:start_amoc(),
    Config.

end_per_testcase(put_scenarios_returns_200_when_scenario_valid, _Config) ->
    amoc_api_helper:remove_module(?SAMPLE_SCENARIO),
    amoc_api_helper:stop_amoc();
end_per_testcase(_, _Config) ->
    amoc_api_helper:stop_amoc().

get_scenarios_returns_200_and_scenarios_list_when_requested(_Config) ->
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?SCENARIOS_URL_S),
    %% then
    {[{Key, Scenarios}]} = Body,
    ?assertEqual(200, CodeHttp),
    ?assertEqual(<<"scenarios">>, Key),
    ?assert(is_list(Scenarios)).

put_scenarios_returns_400_and_error_when_scenario_is_not_valid(_Config) ->
    %% given
    ScenarioContent = "invalid_source",
    %% when
    {CodeHttp, Body} = amoc_api_helper:put(?SCENARIOS_URL_U, ScenarioContent),
    ScenarioFileSource = amoc_api_helper:module_src(?SAMPLE_SCENARIO),
    %% then
    ?assertNot(filelib:is_regular(ScenarioFileSource)),
    ?assertEqual(400, CodeHttp),
    ?assertEqual({[{<<"error">>, <<"invalid module">>}]}, Body).

put_scenarios_returns_200_and_compile_error_when_scenario_source_not_valid(_Config) ->
    %% given
    ScenarioContent = ?SAMPLE_SCENARIO_DECLARATION ++ "\ninvalid_source",
    %% when
    {CodeHttp, Body} = amoc_api_helper:put(?SCENARIOS_URL_U, ScenarioContent),
    ScenarioFileSource = amoc_api_helper:module_src(?SAMPLE_SCENARIO),
    %% then
    ?assertNot(filelib:is_regular(ScenarioFileSource)),
    ?assertEqual(200, CodeHttp),
    Error = <<"compilation errors: [{\"", (list_to_binary(ScenarioFileSource))/binary, "\","
              "\n                      [{2,erl_parse,[\"syntax error before: \",[]]}]}]\n">>,
    ?assertEqual({[{<<"compile">>, Error}]}, Body).

put_scenarios_returns_200_when_scenario_valid(_Config) ->
    %% given
    ScenarioContent = ?DUMMY_SCENARIO_MODULE(?SAMPLE_SCENARIO),
    %% when
    {CodeHttp, Body} = amoc_api_helper:put(?SCENARIOS_URL_U, ScenarioContent),
    ScenarioFileSource = amoc_api_helper:module_src(?SAMPLE_SCENARIO),
    ScenarioFileBeam = amoc_api_helper:module_beam(?SAMPLE_SCENARIO),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertEqual({[{<<"compile">>, <<"ok">>}]}, Body),
    ?assert(filelib:is_regular(ScenarioFileSource)),
    ?assert(filelib:is_regular(ScenarioFileBeam)).
