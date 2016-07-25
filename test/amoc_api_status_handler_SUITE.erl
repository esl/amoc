-module(amoc_api_status_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([works_when_amoc_running_offline/1,
         works_when_amoc_running_online/1,
         returns_false_when_api_up_and_amoc_down_offline/1,
         returns_false_when_api_up_and_amoc_down_online/1]).

-record(state, {action}).

all() ->
    [works_when_amoc_running_offline,
     works_when_amoc_running_online,
     returns_false_when_api_up_and_amoc_down_offline,
     returns_false_when_api_up_and_amoc_down_online].

init_per_testcase(_, Config) ->
    application:ensure_all_started(inets),
    Config.

end_per_testcase(_, _Config) ->
    application:stop(inets),
    application:stop(amoc),
    amoc_api:stop().

works_when_amoc_running_offline(_Config) ->
    %% given
    given_applications_started(),
    %% when
    Status = amoc_api_status_handler:get_status(),
    %% then
    ?assert(Status).

works_when_amoc_running_online(_Config) ->
    %% given
    given_applications_started(),
    %% when
    Result = httpc:request(get, {get_url() ++  "/status", [{"Accept", "application/json"}]}, [], []),
    {ok, {{_HttpVsn, CodeHttp, _Status}, _, Body}} = Result,
    BodyErl = jsx:decode(erlang:list_to_bitstring(Body)),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch([{<<"result">>, true}], BodyErl).


returns_false_when_api_up_and_amoc_down_offline(_Config) ->
    %% given
    given_http_api_started(),
    %% when
    Status = amoc_api_status_handler:get_status(),
    %% then
    ?assertNot(Status).
 
returns_false_when_api_up_and_amoc_down_online(_Config) ->
    %% given
    given_http_api_started(),
    %% when
    Result = httpc:request(get_url() ++ "/status"),
    {ok, {{_HttpVsn, CodeHttp, _Status}, _, Body}} = Result, 
    BodyErl = jsx:decode(erlang:list_to_bitstring(Body)),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch([{<<"result">>, false}], BodyErl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
given_applications_started() ->
    application:ensure_all_started(amoc).

given_http_api_started() ->
    amoc_api:start_listener().

get_url() ->
    Port = amoc_config:get(api_port, 4000),
    "http://localhost:" ++ erlang:integer_to_list(Port).
