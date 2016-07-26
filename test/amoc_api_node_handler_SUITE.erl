-module(amoc_api_node_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([returns_empty_list_when_amoc_up/1]).

all() ->
    [returns_empty_list_when_amoc_up].

init_per_testcase(_, Config) ->
    application:ensure_all_started(inets),
    Config.

end_per_testcase(_, _Config) ->
    application:stop(inets),
    application:stop(amoc),
    amoc_api:stop().

returns_empty_list_when_amoc_up(_Config) ->
    %% given
    given_applications_started(),
    %% when
    {CodeHttp,JSON} = send_request(),
    [{Answer,List}] = JSON,
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertEqual(<<"nodes">>, Answer),
    ?assert(is_list(List)),
    ?assertEqual(0, erlang:length(List)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec given_applications_started() -> {ok, [atom()]} | {error, term()}.
given_applications_started() ->
    application:ensure_all_started(amoc).

-spec get_url() -> string().
get_url() ->
    Port = amoc_config:get(api_port, 4000),
    "http://localhost:" ++ erlang:integer_to_list(Port).

-spec send_request() -> {integer(), jsx:json_term()}.
send_request() ->
    Result = httpc:request(get_url() ++ "/nodes"),
    {ok, {{_HttpVsn, CodeHttp, _Status}, _, Body}} = Result, 
    BodyErl = jsx:decode(erlang:list_to_bitstring(Body)),
    {CodeHttp, BodyErl}.
