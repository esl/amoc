-module(amoc_api_status_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([returns_true_when_amoc_up_offline/1,
         returns_true_when_amoc_up_online/1,
         returns_false_when_api_up_and_amoc_down_offline/1,
         returns_false_when_api_up_and_amoc_down_online/1]).

all() ->
    [returns_true_when_amoc_up_offline,
     returns_true_when_amoc_up_online,
     returns_false_when_api_up_and_amoc_down_offline,
     returns_false_when_api_up_and_amoc_down_online].

init_per_testcase(_, Config) ->
    application:ensure_all_started(inets),
    Config.

end_per_testcase(_, _Config) ->
    application:stop(inets),
    application:stop(amoc),
    amoc_api:stop().

returns_true_when_amoc_up_offline(_Config) ->
    %% given
    given_applications_started(),
    %% when
    Status = amoc_api_status_handler:get_status(),
    %% then
    ?assert(Status).

returns_true_when_amoc_up_online(_Config) ->
    %% given
    given_applications_started(),
    %% when
    {CodeHttp, Body} = send_request(),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch([{<<"result">>, true}], Body).


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
    {CodeHttp, Body} = send_request(),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch([{<<"result">>, false}], Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec given_applications_started() -> {ok, [atom()]} | {error, term()}.
given_applications_started() ->
    application:ensure_all_started(amoc).

-spec given_http_api_started() -> {ok, pid()}.
given_http_api_started() ->
    amoc_api:start_listener().

-spec get_url() -> string().
get_url() ->
    Port = amoc_config:get(api_port, 4000),
    "http://localhost:" ++ erlang:integer_to_list(Port).

-spec send_request() -> {integer(), jsx:json_term()}.
send_request() ->
    Result = httpc:request(get_url() ++ "/status"),
    {ok, {{_HttpVsn, CodeHttp, _Status}, _, Body}} = Result, 
    BodyErl = jsx:decode(erlang:list_to_bitstring(Body)),
    {CodeHttp, BodyErl}.
