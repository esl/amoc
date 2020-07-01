%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api).

-export([start/0, stop/0]).

-spec start() -> {ok, pid()} | {error, any()}.
start() ->
    LogicHandler = amoc_api_logic_handler,
    Port = amoc_config_env:get(amoc, api_port, 4000),
    ServerParams = #{ip => {0, 0, 0, 0}, port => Port, net_opts => [],
                     logic_handler => LogicHandler},
    amoc_rest_server:start(http_server, ServerParams).

-spec stop() -> ok | {error, not_found}.
stop() ->
    cowboy:stop_listener(http_server).
