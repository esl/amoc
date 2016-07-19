-module(amoc_api_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_success/1, test_fail/1]).

all() ->
	[test_success, test_fail].

init_per_testcase(_, Config) ->
	application:ensure_all_started(inets),
	Config.

end_per_testcase(_, _Config) ->
	application:stop(inets),
	application:stop(amoc),
	amoc_api:stop().

test_success(_Config) ->
	%% given
	given_applications_started(),
	%% when
	Result = httpc:request("http://localhost:4000/status"),
	{ok, {{_HttpVsn, CodeHtml, _Status}, _, _}} = Result,
    %% then
	200 = CodeHtml.	

test_fail(_Config) ->
	%% given
	given_http_api_started(),
	%% when
	Result = httpc:request("http://localhost:4000/status"),
	{ok, {{_HttpVsn, CodeHtml, _Status}, _, _}} = Result, 
	%% then
	503 = CodeHtml.

given_applications_started() ->
	application:ensure_all_started(amoc).

given_http_api_started() ->
	amoc_api:start_listener().
