-module(amoc_metrics).

-export([default_reporting_opts/0]).
-export([init/2]).
-export([update_time/2, update_counter/2, update_counter/1]).
-export([messages_spiral_name/0, message_ttd_histogram_name/0]).

-type name() :: atom().

-type reporting_opts() :: #{reporter := atom(),
                            interval := non_neg_integer(),
                            report => [atom()]}.

-type type() :: counters | times.

-spec init(type(), name()) -> ok.
init(Type, Name) ->
    init(Type, Name, default_reporting_opts()).

init(Type, Name, Opts) ->
    ExName = make_name(Type, Name),
    ExType = exometer_metric_type(Type),
    #{reporter := Reporter, interval := Interval} = Opts,
    Datapoints = metric_report_datapoints(ExType),
    create_metric_and_maybe_subscribe(ExName, ExType, Reporter, Interval, Datapoints).


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

-spec messages_spiral_name() -> name().
messages_spiral_name() ->
    messages_sent.

-spec message_ttd_histogram_name() -> name().
message_ttd_histogram_name() ->
    message_ttd.

-spec default_reporting_opts() -> reporting_opts().
default_reporting_opts() ->
    #{reporter => exometer_report_graphite,
      interval => timer:seconds(10)}.

make_name(Type, Name) when is_atom(Name) ->
    [amoc, Type, Name].

metric_report_datapoints(spiral) -> [count, one];
metric_report_datapoints(histogram) -> [mean, min, max, median, 95, 99, 999].

create_metric_and_maybe_subscribe(ExName, Type, Reporter, Interval, Datapoints) ->
    ok = exometer:re_register(ExName, Type, []),
    case lists:keyfind(Reporter, 1, exometer_report:list_reporters()) of
        {Reporter, _} ->
            exometer_report:unsubscribe(Reporter, ExName, Datapoints),
            ok = exometer_report:subscribe(Reporter, ExName, Datapoints, Interval);
        _ ->
            lager:warning("Reporter=~p not_enbled")
    end.

exometer_metric_type(counters) -> spiral;
exometer_metric_type(times) -> histogram.
