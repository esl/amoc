-module(telemetry_helpers).

-define(HANDLER, telemetry_handler).
-define(CONFIG, #{dummy_config => true}).
-define(CONFIG_MATCH, #{dummy_config := true}).

-export([start/1, reset/0, stop/0, get_calls/1]).

start(TelemetryEvents) ->
    meck:new(?HANDLER, [non_strict, no_link]),
    meck:expect(?HANDLER, handler, ['_', '_', '_', '_'], ok),
    application:start(telemetry),
    TelemetryHandler = fun ?HANDLER:handler/4,
    telemetry:attach_many(?HANDLER, TelemetryEvents, TelemetryHandler, ?CONFIG).

reset() ->
    meck:reset(?HANDLER).

stop() ->
    meck:unload(?HANDLER).

get_calls(Prefix) ->
    History = meck:history(?HANDLER),
    Filter = fun({_Pid, Call, _Ret}) ->
                     {?HANDLER, handler,
                      [EventName, Measurements, Metadata, ?CONFIG_MATCH]} = Call,
                     lists:prefix(Prefix, EventName)
                     andalso {true, {EventName, Measurements, Metadata}}
             end,
    lists:filtermap(Filter, History).
