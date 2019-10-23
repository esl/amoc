-module(amoc_metrics).

-export([start/0, init/2]).
-export([update_time/2, update_counter/2, update_counter/1, update_gauge/2]).
-export([messages_spiral_name/0, message_ttd_histogram_name/0]).

-type simple_name() :: atom() | [atom()].

-type name() :: simple_name() | {strict, simple_name()}.

-type type() :: counters | times | gauge.

-define(AMOC_DEFAULT_METRICS_REPORTER, exometer_report_graphite).
-define(AMOC_METRICS_REPORTING_INTERVAL, timer:seconds(10)).

-spec start() -> ok.
start() ->
    maybe_add_reporter(),
    subsribe_default_metrics().

-spec init(type(), name()) -> ok.
init(Type, Name) ->
    ExName = make_name(Type, Name),
    ExType = exometer_metric_type(Type),
    create_metric_and_maybe_subscribe(ExName, ExType).


-spec update_time(name(), integer()) -> ok.
update_time(Name, Value) ->
    ExName = make_name(times, Name),
    exometer:update(ExName, Value).

-spec update_counter(name()) -> ok.
update_counter(Name) ->
    ExName = make_name(counters, Name),
    exometer:update(ExName, 1).

-spec update_counter(name(), integer()) -> ok.
update_counter(Name, Value) ->
    ExName = make_name(counters, Name),
    exometer:update(ExName, Value).

-spec update_gauge(name(), integer()) -> ok.
update_gauge(Name, Value) ->
    ExName = make_name(gauge, Name),
    exometer:update(ExName, Value).

-spec messages_spiral_name() -> name().
messages_spiral_name() ->
    messages_sent.

-spec message_ttd_histogram_name() -> name().
message_ttd_histogram_name() ->
    message_ttd.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_name(_, {strict, Name}) ->
    add_name_prefix([amoc], Name);
make_name(Type, Name) ->
    add_name_prefix([amoc, Type], Name).

add_name_prefix(Prefix, Name) when is_atom(Name) ->
    Prefix ++ [Name];
add_name_prefix(Prefix, Name) when is_list(Name) ->
    Prefix ++ Name.

metric_report_datapoints(gauge)     -> [value];
metric_report_datapoints(spiral)    -> [count, one];
metric_report_datapoints(histogram) -> [mean, min, max, median, 95, 99, 999].

exometer_metric_type(counters) -> spiral;
exometer_metric_type(times)    -> histogram;
exometer_metric_type(gauge)    -> gauge.

create_metric_and_maybe_subscribe(ExName, ExType) ->
    ok = exometer:re_register(ExName, ExType, []),
    Datapoints = metric_report_datapoints(ExType),
    maybe_subscribe(ExName, Datapoints).

maybe_add_reporter() ->
    Reporter = get_reporter(),
    case lists:keyfind(Reporter, 1, exometer_report:list_reporters()) of
        {Reporter, _} -> ok;
        _->
            case amoc_config:get(graphite_host) of
                undefined -> ok;
                Host ->
                    Prefix =amoc_config:get(graphite_prefix,net_adm:localhost()),
                    Port = amoc_config:get(graphite_port,2003),
                    Options = [{module, exometer_report_graphite},
                               {prefix, Prefix},
                               {host, Host},
                               {port, Port},
                               {api_key, ""}],
                    exometer_report:add_reporter(Reporter,Options)
            end
    end.

subsribe_default_metrics() ->
    maybe_subscribe([amoc, users], [size]),
    maybe_subscribe([erlang, system_info], [port_count, process_count]),
    maybe_subscribe([erlang, memory], [total, processes, processes_used, system, binary, ets]),
    maybe_subscribe([amoc, times, connection], metric_report_datapoints(histogram)),
    maybe_subscribe([amoc, counters, connections], metric_report_datapoints(spiral)),
    maybe_subscribe([amoc, counters, connection_failures], metric_report_datapoints(spiral)).

get_reporter() ->
    amoc_config:get(metrics_reporter, ?AMOC_DEFAULT_METRICS_REPORTER).

maybe_subscribe(ExName, Datapoints) ->
    Reporter = get_reporter(),
    Interval = ?AMOC_METRICS_REPORTING_INTERVAL,
    case lists:keyfind(Reporter, 1, exometer_report:list_reporters()) of
        {Reporter, _} ->
            exometer_report:unsubscribe(Reporter, ExName, Datapoints),
            ok = exometer_report:subscribe(Reporter, ExName, Datapoints, Interval);
        _ ->
            lager:warning("Reporter=~p not_enbled", [Reporter])
    end.

