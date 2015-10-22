-module(amoc_metrics).

-export([new_histogram/1, new_counter/1, new_spiral/1]).
-export([new_histogram/2, new_counter/2, new_spiral/2]).
-export([update_hist/2, update_counter/2, update_spiral/2]).

-type name() :: atom().

-spec new_histogram(name()) -> ok.
new_histogram(Name) when is_atom(Name) ->
    new_histogram(Name, []).

-spec new_histogram(name(), [proplists:property()]) -> ok.
new_histogram(Name, Opts) when is_atom(Name) ->
    ExName = [amoc, histograms, Name],
    {Reporter, Interval} = reporter_and_interval(Opts),
    HistProps = [mean, min, max, median, 95, 99, 999],
    ReportProps = proplists:get_value(report, Opts, HistProps),
    ok = exometer:new(ExName, histogram),
    ok = exometer_report:subscribe(Reporter, ExName, ReportProps, Interval).

-spec new_spiral(name()) -> ok.
new_spiral(Name) when is_atom(Name) ->
    new_spiral(Name, []).

-spec new_spiral(name(), [proplists:property()]) -> ok.
new_spiral(Name, Opts) when is_atom(Name) ->
    ExName = [amoc, spirals, Name],
    {Reporter, Interval} = reporter_and_interval(Opts),
    ReportProps = proplists:get_value(report, Opts, [one, count]),
    ok = exometer:new(ExName, spiral),
    ok = exometer_report:subscribe(Reporter, ExName, ReportProps, Interval).

-spec new_counter(name()) -> ok.
new_counter(Name) when is_atom(Name) ->
    new_counter(Name, []).

-spec new_counter(name(), [proplists:property()]) -> ok.
new_counter(Name, Opts) when is_atom(Name) ->
    ExName = [amoc, counters, Name],
    {Reporter, Interval} = reporter_and_interval(Opts),
    ReportProps = proplists:get_value(report, Opts, [value]),
    ok = exometer:new(ExName, counter),
    ok = exometer_report:subscribe(Reporter, ExName, ReportProps, Interval).

-spec update_hist(name(), integer()) -> ok.
update_hist(Name, Value) when is_atom(Name) ->
    ExName = [amoc, histograms, Name],
    exometer:update(ExName, Value).

-spec update_spiral(name(), integer()) -> ok.
update_spiral(Name, Value) when is_atom(Name) ->
    ExName = [amoc, spirals, Name],
    exometer:update(ExName, Value).

-spec update_counter(name(), integer()) -> ok.
update_counter(Name, Value) when is_atom(Name) ->
    ExName = [amoc, counters, Name],
    exometer:update(ExName, Value).

-spec reporter_and_interval([proplists:property()]) ->
    {module(), non_neg_integer()}.
reporter_and_interval(Opts) ->
    Reporter = proplists:get_value(reporter, Opts, exometer_report_graphite),
    Interval = proplists:get_value(interval, Opts, 5000),
    {Reporter, Interval}.
