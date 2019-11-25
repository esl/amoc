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
    AdditionalCodePaths = amoc_config_env:get(extra_code_paths, []),
    code:add_pathsz(AdditionalCodePaths),
    Ret = amoc_sup:start_link(),
    amoc_api:start_listener(),
    amoc_metrics:start(),
    Ret.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
