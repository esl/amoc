-module(amoc_metrics).

-export([default_reporting_opts/0]).
-export([new_histogram/1, new_counter/1, new_spiral/1]).
-export([new_histogram/2, new_counter/2, new_spiral/2]).
-export([update_hist/2, update_counter/2, update_spiral/2]).
-export([messages_spiral_name/0, message_ttd_histogram_name/0]).

-type name() :: atom() | [atom()].

-type reporting_opts() :: #{reporter := atom(),
                            interval := non_neg_integer(),
                            report => [atom()]}.

-spec new_histogram(name()) -> ok.
new_histogram(Name) ->
    new_histogram(Name, default_reporting_opts()).

-spec new_histogram(name(), reporting_opts()) -> ok.
new_histogram(Name, Opts) ->
    ExName = make_name(histogram, Name),
    #{reporter := Reporter, interval := Interval} = Opts,
    HistProps = [mean, min, max, median, 95, 99, 999],
    Datapoints = maps:get(report, Opts, HistProps),
    create_metric_and_maybe_subscribe(ExName, histogram, Reporter, Interval, Datapoints).

-spec new_spiral(name()) -> ok.
new_spiral(Name) ->
    new_spiral(Name, default_reporting_opts()).

-spec new_spiral(name(), reporting_opts()) -> ok.
new_spiral(Name, Opts) ->
    ExName = make_name(spiral, Name),
    #{reporter := Reporter, interval := Interval} = Opts,
    Datapoints = maps:get(report, Opts, [one, count]),
    create_metric_and_maybe_subscribe(ExName, spiral, Reporter, Interval, Datapoints).

-spec new_counter(name()) -> ok.
new_counter(Name) when is_atom(Name) ->
    new_counter(Name, default_reporting_opts()).

-spec new_counter(name(), reporting_opts()) -> ok.
new_counter(Name, Opts) when is_atom(Name) ->
    ExName = [amoc, counters, Name],
    #{reporter := Reporter, interval := Interval} = Opts,
    Datapoints = maps:get(report, Opts, [value]),
    create_metric_and_maybe_subscribe(ExName, counter, Reporter, Interval, Datapoints).

-spec update_hist(name(), integer()) -> ok.
update_hist(Name, Value) ->
    ExName = make_name(histogram, Name),
    exometer:update(ExName, Value).

-spec update_spiral(name(), integer()) -> ok.
update_spiral(Name, Value) ->
    ExName = make_name(spiral, Name),
    exometer:update(ExName, Value).

-spec update_counter(name(), integer()) -> ok.
update_counter(Name, Value) ->
    ExName = make_name(counter, Name),
    exometer:update(ExName, Value).

-spec messages_spiral_name() -> name().
messages_spiral_name() ->
    [counters, messages_sent].

-spec message_ttd_histogram_name() -> name().
message_ttd_histogram_name() ->
    [times, message_ttd].

-spec default_reporting_opts() -> reporting_opts().
default_reporting_opts() ->
    #{reporter => exometer_report_graphite,
      interval => timer:seconds(10)}.

make_name(Type, Name) when is_atom(Name) ->
    [amoc, name_by_type(Type), Name];
make_name(_, Name) when is_list(Name) ->
    [amoc | Name].

name_by_type(histogram) -> histograms;
name_by_type(spiral) -> spirals;
name_by_type(counter) -> counters.


create_metric_and_maybe_subscribe(ExName, Type, Reporter, Interval, Datapoints) ->
    ok = exometer:re_register(ExName, Type, []),
    case lists:keyfind(Reporter, 1, exometer_report:list_reporters()) of
        {Reporter, _} ->
            exometer_report:unsubscribe(Reporter, ExName, Datapoints),
            ok = exometer_report:subscribe(Reporter, ExName, Datapoints, Interval);
        _ ->
            lager:warning("Reporter=~p not_enbled")
    end.

