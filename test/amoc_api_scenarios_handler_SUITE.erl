-module(amoc_api_scenarios_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
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
    file:make_dir("ebin"),
    file:make_dir("scenarios"),
    
    application:ensure_all_started(inets),
    application:ensure_all_started(amoc),
    Config.

end_per_testcase(post_scenarios_returns_200_and_when_scenario_valid, _Config) ->
    file:delete("ebin/sample_test.beam"),
    file:delete("scenarios/sample_test.erl"),
    
    file:del_dir("ebin"),
    file:del_dir("scenarios"),

    application:stop(inets),
    application:stop(amoc);

end_per_testcase(_, _Config) ->
    application:stop(inets),
    application:stop(amoc).

get_scenarios_returns_200_and_scenarios_list_when_requested(_Config) ->
    %% given
    URL = get_url() ++ "/scenarios", 
    %% when
    {CodeHttp, Body} = get_request(URL),
    Scenarios = proplists:get_value(<<"scenarios">>, Body),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assert(is_list(Scenarios)).

post_scenarios_returns_400_when_malformed_request(_Config) ->
    %% given
    URL = get_url() ++ "/scenarios",
    RequestBody = jsx:encode([{keyname_typo, sample_test}]),
    %% when
    {CodeHttp, Body} = post_request(URL, RequestBody),
    %% then
    ?assertEqual(400, CodeHttp),
    ?assertEqual([{<<"error">>, <<"bad_request">>}],
                 Body).

post_scenarios_returns_200_and_compile_error_when_scenario_source_not_valid(_Config) ->
    %% given
    URL = get_url() ++ "/scenarios",
    RequestBody = jsx:encode([
                              {scenario, sample_test},
                              {module_source, invalid_source}
                             ]),
    %% when
    {CodeHttp, Body} = post_request(URL, RequestBody),
    %% then
    ?assertNot(filelib:is_regular("scenarios/sample_test.erl")),
    ?assertEqual(200, CodeHttp),
    ?assertEqual([{<<"compile">>, <<"error">>}],
                 Body).

post_scenarios_returns_200_and_when_scenario_valid(Config) ->
    %% given
    URL = get_url() ++ "/scenarios",
    ScenarioFile = data(Config, "sample_test.erl"),
    {ok, ScenarioContent} = file:read_file(ScenarioFile),
    RequestBody = jsx:encode([
                              {scenario, sample_test},
                              {module_source, ScenarioContent}
                             ]),
    %% when
    {CodeHttp, Body} = post_request(URL, RequestBody),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assert(filelib:is_regular("scenarios/sample_test.erl")),
    ?assert(filelib:is_regular("ebin/sample_test.beam")),
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

