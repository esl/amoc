%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_throttle).

%% API
-export([start/2,
         start/3,
         start/4,
         send/3,
         send/2,
         send_and_wait/2,
         run/2,
         pause/1,
         resume/1,
         change_rate/3,
         change_rate_gradually/6,
         stop/1]).

-define(DEFAULT_INTERVAL, 60000).%% one minute

-define(RATE(Name), [throttle, Name, rate]).
-define(EXEC_RATE(Name), [throttle, Name, exec_rate]).
-define(REQ_RATE(Name), [throttle, Name, req_rate]).

-type name() :: atom().

-spec start(name(), pos_integer()) -> ok | {error, any()}.
start(Name, Rate) ->
    start(Name, Rate, ?DEFAULT_INTERVAL).

-spec start(name(), pos_integer(), non_neg_integer()) -> ok | {error, any()}.
start(Name, Rate, Interval) ->
    start(Name, Rate, Interval, 10).

-spec start(name(), pos_integer(), non_neg_integer(), pos_integer()) -> ok | {error, any()}.
start(Name, Rate, Interval, NoOfProcesses) ->
    case pg2:get_members(Name) of
        {error, {no_such_group, Name}} ->
            pg2:create(Name),
            amoc_metrics:init(gauge, ?RATE(Name)),
            [amoc_metrics:init(counters, E) || E <- [?EXEC_RATE(Name), ?REQ_RATE(Name)]],
            RatePerMinute = rate_per_minute(Rate, Interval),
            amoc_metrics:update_gauge(?RATE(Name), RatePerMinute),
            RealNoOfProcesses = min(Rate, NoOfProcesses),
            start_throttle_processes(Name, Interval, Rate, RealNoOfProcesses),
            ok;
        List when is_list(List) ->
            {error, {name_is_already_used, Name}}
    end.

-spec pause(name()) -> ok | {error, any()}.
pause(Name) ->
    all_processes(Name, pause).

-spec resume(name()) -> ok | {error, any()}.
resume(Name) ->
    all_processes(Name, resume).

-spec change_rate(name(), pos_integer(), non_neg_integer()) -> ok | {error, any()}.
change_rate(Name, Rate, Interval) ->
    case pg2:get_members(Name) of
        {error, Err} -> {error, Err};
        List when is_list(List) ->
            RatePerMinute = rate_per_minute(Rate, Interval),
            amoc_metrics:update_gauge(?RATE(Name), RatePerMinute),
            update_throttle_processes(List, Interval, Rate, length(List))
    end.

-spec change_rate_gradually(name(), pos_integer(), pos_integer(),
                            non_neg_integer(), pos_integer(), pos_integer()) -> ok | {error, any()}.
change_rate_gradually(Name, _, HighRate, RateInterval, _, 1) ->
    change_rate(Name, HighRate, RateInterval), ok;
change_rate_gradually(Name, LowRate, HighRate, RateInterval, StepInterval, NoOfSteps) ->
    change_rate(Name, LowRate, RateInterval),
    Step = (HighRate - LowRate) div (NoOfSteps - 1),
    NewLowRate = LowRate + Step,
    spawn(
        fun() ->
            timer:sleep(StepInterval),
            change_rate_gradually(Name, NewLowRate, HighRate, RateInterval, StepInterval, NoOfSteps - 1)
        end),
    ok.

-spec run(name(), fun(()-> any())) -> ok | {error, any()}.
run(Name, Fn) ->
    case get_throttle_process(Name) of
        {ok, Pid} ->
            amoc_metrics:update_counter(?REQ_RATE(Name)),
            Fun =
                fun() ->
                    amoc_metrics:update_counter(?EXEC_RATE(Name)),
                    Fn()
                end,
            amoc_throttle_process:run(Pid, Fun),
            ok;
        Error -> Error
    end.

-spec send(name(), pid(), any()) -> ok | {error, any()}.
send(Name, Pid, Msg) ->
    run(Name, fun() -> Pid ! Msg end).

-spec send(name(), any()) -> ok | {error, any()}.
send(Name, Msg) ->
    send(Name, self(), Msg).

-spec send_and_wait(name(), any()) -> ok | {error, any()}.
send_and_wait(Name, Msg) ->
    send(Name, Msg),
    receive
        Msg -> ok
    end.

-spec stop(name()) -> ok | {error, any()}.
stop(Name) ->
    all_processes(Name, stop),
    pg2:delete(Name),
    ok.

all_processes(Name, Cmd) ->
    case pg2:get_members(Name) of
        List when is_list(List) ->
            [run_cmd(P, Cmd) || P <- List], ok;
        Error -> Error
    end.

run_cmd(Pid, stop) ->
    amoc_throttle_process:stop(Pid);
run_cmd(Pid, pause) ->
    amoc_throttle_process:pause(Pid);
run_cmd(Pid, resume) ->
    amoc_throttle_process:resume(Pid).

rate_per_minute(_, 0) -> 0;
rate_per_minute(Rate, Interval) ->
    (Rate * 60000) div Interval.

start_throttle_processes(Name, Interval, Rate, 1) ->
    start_throttle_process(Name, Interval, Rate);
start_throttle_processes(Name, Interval, Rate, N) when is_integer(N), N > 1 ->
    ProcessRate = Rate div N,
    start_throttle_process(Name, Interval, ProcessRate),
    start_throttle_processes(Name, Interval, Rate - ProcessRate, N - 1).

start_throttle_process(Name, Interval, Rate) ->
    {ok, Pid} = amoc_throttle_process:start(Interval, Rate),
    pg2:join(Name, Pid).

update_throttle_processes([Pid], Interval, Rate, 1) ->
    amoc_throttle_process:update(Pid, Interval, Rate);
update_throttle_processes([Pid | Tail], Interval, Rate, N) when N > 1 ->
    ProcessRate = Rate div N,
    amoc_throttle_process:update(Pid, Interval, ProcessRate),
    update_throttle_processes(Tail, Interval, Rate - ProcessRate, N - 1).

get_throttle_process(Name) ->
    case pg2:get_members(Name) of
        [] ->
            {error, {no_trottle_process_registered, Name}};
        {error, Error} ->
            {error, Error};
        List -> %% nonempty list
            N = rand:uniform(length(List)),
            {ok, lists:nth(N, List)}
    end.
