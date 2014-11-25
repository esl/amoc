%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(http_req).

-export([start/0,stop/0]).
-export([request/4]).
-export([get_request/2,
         get_request/3,
         post_request/3,
         post_request/4]).
-define(TIMEOUT, 12000).

start() ->
    error_logger:tty(false),
    start_fusco().

stop() ->
    stop_fusco().

request(Host, Path, Method, Body) ->
    fusco_request(Host, Path, Method, [], Body, ?TIMEOUT).

post_request(Host, Path, PostBody) ->
    post_request(Host, Path, [], PostBody).

post_request(Host, Path, Headers, PostBody) ->
    fusco_request(Host, Path, <<"POST">>, Headers, PostBody, ?TIMEOUT).

get_request(Host, Path) ->
    get_request(Host, [], Path).
get_request(Host, Path, Headers) ->
    fusco_request(Host, Path, <<"GET">>, Headers, [], ?TIMEOUT).

%%% Internal
start_fusco() ->
    %% In R16B01 and above, we could use application:ensure_started/1
    %% instead.
    lists:foreach(fun(App) ->
                          case application:start(App) of
                              ok -> ok;
                              {error, {already_started, App}} -> ok;
                              {error, E} -> error({cannot_start, E}, [App])
                          end end,
                  [asn1,crypto,public_key,ssl,fusco]).
stop_fusco() ->
    [ok,ok,ok,ok,ok] = lists:map(fun application:stop/1,
                                 [fusco,ssl,public_key,crypto,asn1]).

fusco_request(URL, Path, Method, Hdrs, Body, Timeout) ->
    {ok, P}=fusco:start(URL,[]),
    ok = fusco:connect(P),
    Result = fusco:request(P, Path, Method, Hdrs, Body, Timeout),
    ok = fusco:disconnect(P),
    Result.
