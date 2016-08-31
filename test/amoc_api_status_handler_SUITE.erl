-module(amoc_api_status_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([returns_up_when_amoc_up_offline/1,
         returns_up_when_amoc_up_online/1,
         returns_down_when_api_up_and_amoc_down_offline/1,
         returns_down_when_api_up_and_amoc_down_online/1]).

-define(PATH, "/status").

all() ->
    [returns_up_when_amoc_up_offline,
     returns_up_when_amoc_up_online,
     returns_down_when_api_up_and_amoc_down_offline,
     returns_down_when_api_up_and_amoc_down_online].

init_per_testcase(_, Config) ->
    application:ensure_all_started(inets),
    Config.

end_per_testcase(_, _Config) ->
    application:stop(inets),
    application:stop(amoc),
    amoc_api:stop().

returns_up_when_amoc_up_offline(_Config) ->
    %% given
    given_applications_started(),
    %% when
    Status = amoc_api_status_handler:get_status(),
    %% then
    ?assertEqual(up, Status).

returns_up_when_amoc_up_online(_Config) ->
    %% given
    given_applications_started(),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?PATH),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch({[{<<"node_status">>, <<"up">>}]}, Body).


returns_down_when_api_up_and_amoc_down_offline(_Config) ->
    %% given
    given_http_api_started(),
    %% when
    Status = amoc_api_status_handler:get_status(),
    %% then
    ?assertEqual(down, Status).
 
returns_down_when_api_up_and_amoc_down_online(_Config) ->
    %% given
    given_http_api_started(),
    %% when
    {CodeHttp, Body} = amoc_api_helper:get(?PATH),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch({[{<<"node_status">>, <<"down">>}]}, Body).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec given_applications_started() -> {ok, [atom()]} | {error, term()}.
given_applications_started() ->
    application:ensure_all_started(amoc).

-spec given_http_api_started() -> {ok, pid()}.
given_http_api_started() ->
    amoc_api:start_listener().
