-module(amoc_api_scenario_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
         get_scenario_status_returns_200_when_scenario_exists/1,
         get_scenario_status_returns_404_when_scenario_not_exists/1,
         patch_scenario_returns_404_when_scenario_not_exists/1,
         patch_scenario_returns_400_when_malformed_request/1,
         patch_scenario_returns_200_when_request_ok_and_module_exists/1
        ]).


all() ->
    [
     get_scenario_status_returns_200_when_scenario_exists,
     get_scenario_status_returns_404_when_scenario_not_exists,
     patch_scenario_returns_404_when_scenario_not_exists,
     patch_scenario_returns_400_when_malformed_request,
     patch_scenario_returns_200_when_request_ok_and_module_exists
    ].

init_per_testcase(
  patch_scenario_returns_200_when_request_ok_and_module_exists,
  Config) ->
    meck:new(amoc_dist, [unstick]),
    Fun = fun(_,1,_) -> ok end,
    meck:expect(amoc_dist, do, Fun),
    ok = create_env(Config),
    Config;

init_per_testcase(_, Config) ->
    ok = create_env(Config),
    Config.

end_per_testcase(
  patch_scenario_returns_200_when_request_ok_and_module_exists,
  _Config) ->
    meck:unload(amoc_dist),
    ok = destroy_env();

end_per_testcase(_, _Config) ->
    ok = destroy_env().

get_scenario_status_returns_200_when_scenario_exists(_Config) ->
    %% given
    URL = get_url() ++ "/scenarios/sample_test",
    %% when
    {CodeHttp, _Body} = get_request(URL),
    %% then
    %% Maybe check Body, as answer format will be ready
    ?assertEqual(200, CodeHttp).

get_scenario_status_returns_404_when_scenario_not_exists(_Config) ->
    %% given
    URL = get_url() ++ "/scenarios/non_existing_scenario",
    %% when
    {CodeHttp, _Body} = get_request(URL),
    %% then
    %% Maybe check Body, as answer format will be ready
    ?assertEqual(404, CodeHttp).

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
    {CodeHttp, Body} = patch_request(URL, RequestBody),
    ct:print("~n ~n CODE ~p, Body: ~p ~n ~n", [CodeHttp, RequestBody]),
    %% then
    %% Maybe check Body, as answer format will be ready
    ?assertEqual(400, CodeHttp).


patch_scenario_returns_200_when_request_ok_and_module_exists(_Config) ->
    %% given
    URL = get_url() ++ "/scenarios/sample_test",
    RequestBody = jsx:encode([{users, 10}]),
    %% when
    {CodeHttp, Body} = patch_request(URL, RequestBody),
    %% then
    %% Maybe check Body, as answer format will be ready
    meck:wait(amoc_dist, do, ['sample_test', 1, 10], 2000),
    ?assertEqual(200, CodeHttp).


%% Helpers

create_env(Config) ->
    file:make_dir("scenarios"),
    copy(
      data(Config, "sample_test.erl"),
      "scenarios/sample_test.erl"),
    compile:file("scenarios/sample_test.erl"),
    code:load_file(sample_test),
    {ok, _} = application:ensure_all_started(inets),
    {ok, _} = application:ensure_all_started(amoc),
    ok.
 

destroy_env() ->
    file:delete("scenarios/sample_test.erl"),
    file:del_dir("scenarios"),
    ok.

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

