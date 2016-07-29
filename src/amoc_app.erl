%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    ok = amoc_config:start(),
    Ret = amoc_sup:start_link(),
    amoc_dist:start_nodes(),
    ok = amoc_event:add_handler(amoc_annotations, []),
    Ret.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
