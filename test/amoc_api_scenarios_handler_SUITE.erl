-module(amoc_api_scenarios_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SCENARIOS_URL_S, "/scenarios").
-define(SCENARIOS_DIR_S, "scenarios").
-define(SCENARIOS_EBIN_DIR_S, "scenarios_ebin").
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
    ok = file:make_dir(?SCENARIOS_EBIN_DIR_S),
    ok = file:make_dir(?SCENARIOS_DIR_S),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(amoc),
    Config.

end_per_testcase(post_scenarios_returns_200_and_when_scenario_valid, _Config) ->
    ok = file:delete(filename:join([?SCENARIOS_EBIN_DIR_S,
                                    ?SAMPLE_SCENARIO_BEAM_S])),
    ok = file:delete(filename:join([?SCENARIOS_DIR_S,
                                    ?SAMPLE_SCENARIO_S])),
    ok = file:del_dir(?SCENARIOS_EBIN_DIR_S),
    ok = file:del_dir(?SCENARIOS_DIR_S);

end_per_testcase(_, _Config) ->
    ok = file:del_dir(?SCENARIOS_EBIN_DIR_S),
    ok = file:del_dir(?SCENARIOS_DIR_S).

get_scenarios_returns_200_and_scenarios_list_when_requested(_Config) ->
    %% given
    URL = get_url() ++ ?SCENARIOS_URL_S,
    %% when
    {CodeHttp, Body} = get_request(URL),
    Scenarios = proplists:get_value(<<"scenarios">>, Body),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assert(is_list(Scenarios)).

post_scenarios_returns_400_when_malformed_request(_Config) ->
    %% given
    URL = get_url() ++ ?SCENARIOS_URL_S,
    RequestBody = jsx:encode([{keyname_typo, ?SAMPLE_SCENARIO_A}]),
    %% when
    {CodeHttp, Body} = post_request(URL, RequestBody),
    %% then
    ?assertEqual(400, CodeHttp),
    ?assertEqual([{<<"error">>, <<"wrong_json">>}],
                 Body).

post_scenarios_returns_200_and_compile_error_when_scenario_source_not_valid(_Config) ->
    %% given
    URL = get_url() ++ ?SCENARIOS_URL_S,
    RequestBody = jsx:encode([
                              {scenario, ?SAMPLE_SCENARIO_A},
                              {module_source, invalid_source}
                             ]),
    %% when
    {CodeHttp, Body} = post_request(URL, RequestBody),
    ScenarioFileSource = filename:join([?SCENARIOS_DIR_S,
                                        ?SAMPLE_SCENARIO_S]),
    %% then
    ?assertNot(filelib:is_regular(ScenarioFileSource)),
    ?assertEqual(200, CodeHttp),
    ?assertEqual([{<<"compile">>, 
                   <<"[{\"scenarios/sample_test.erl\",[{1,erl_parse,"
                     "[\"syntax error before: \",[]]}]},\n "
                     "{\"scenarios/sample_test.erl\",["
                     "{1,erl_lint,undefined_module}"
                     "]}]">>}],
                 Body).

post_scenarios_returns_200_and_when_scenario_valid(Config) ->
    %% given
    URL = get_url() ++ ?SCENARIOS_URL_S,
    ScenarioFile = data(Config, ?SAMPLE_SCENARIO_S),
    {ok, ScenarioContent} = file:read_file(ScenarioFile),
    RequestBody = jsx:encode([
                              {scenario, ?SAMPLE_SCENARIO_A},
                              {module_source, ScenarioContent}
                             ]),
    %% when
    {CodeHttp, Body} = post_request(URL, RequestBody),
    ScenarioFileSource = filename:join([?SCENARIOS_DIR_S,
                                        ?SAMPLE_SCENARIO_S]),
    ScenarioFileBeam = filename:join([?SCENARIOS_EBIN_DIR_S,
                                      ?SAMPLE_SCENARIO_BEAM_S]),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assert(filelib:is_regular(ScenarioFileSource)),
    ?assert(filelib:is_regular(ScenarioFileBeam)),
    ?assertEqual([{<<"compile">>, <<"ok">>}],
                 Body).

%% Helpers

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

-spec post_request(string(), string()) ->
    {integer(), jsx:json_term()}.
post_request(URL, RequestBody) ->
    Header = "",
    Type = "application/json",
    HTTPOpts = [],
    Opts = [],
    Result = httpc:request(post,
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

