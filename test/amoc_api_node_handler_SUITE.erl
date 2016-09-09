-module(amoc_api_node_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([returns_empty_list_when_amoc_up/1,
         returns_nodes_list_when_amoc_up/1]).

-define(PATH, "/nodes").

all() ->
    [returns_empty_list_when_amoc_up,
     returns_nodes_list_when_amoc_up].

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
    {CodeHttp,JSON} = amoc_api_helper:get(?PATH),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch({[{<<"nodes">>, {[]}}]}, JSON).

returns_nodes_list_when_amoc_up(_Config) ->
    %% given
    given_applications_started(),
    given_prepared_nodes(),
    %% when
    {CodeHttp, JSON} = amoc_api_helper:get(?PATH),
    %% then
    ?assertEqual(200, CodeHttp),
    ?assertMatch(
        {[{<<"nodes">>, {[{<<"test1">>, <<"up">>}, {<<"test2">>, <<"down">>}]}}]},
        JSON),
    %% cleanup
    clean_nodes().
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec given_applications_started() -> {ok, [atom()]} | {error, term()}.
given_applications_started() ->
    application:ensure_all_started(amoc).

-spec given_prepared_nodes() -> ok.
given_prepared_nodes() ->
    meck:new(amoc_dist, [unstick]),
    meck:expect(amoc_dist, ping_nodes, fun() -> [{test1, pong}, {test2, pang}] end).

-spec clean_nodes() -> ok.
clean_nodes() ->
    meck:unload(amoc_dist).
