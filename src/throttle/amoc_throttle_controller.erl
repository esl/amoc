%% @private
%% @see amoc_throttle
%% @copyright 2023 Erlang Solutions Ltd.
-module(amoc_throttle_controller).

-behaviour(gen_server).

%% API
-export([start_link/0,
         ensure_throttle_processes_started/4,
         pause/1, resume/1, stop/1,
         change_rate/3, change_rate_gradually/6,
         run/2, telemetry_event/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-define(PG_SCOPE, amoc_throttle).
-define(SERVER, ?MODULE).
-define(MASTER_SERVER, {?SERVER, amoc_cluster:master_node()}).

-record(throttle_info, {
    rate :: amoc_throttle:rate(),
    interval :: amoc_throttle:interval(),
    no_of_procs :: pos_integer(),
    active :: boolean(),
    change_plan :: change_rate_plan() | undefined
}).

-record(change_rate_plan, {
    high_rate :: pos_integer(),
    no_of_steps :: non_neg_integer(),
    timer :: timer:tref()}).

-type name() :: amoc_throttle:name().
-type change_rate_plan() :: #change_rate_plan{}.
-type throttle_info() :: #throttle_info{}.
-type state() :: #{name() => throttle_info()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(ensure_throttle_processes_started(name(), amoc_throttle:rate(),
                                        amoc_throttle:interval(), pos_integer()) ->
    {ok, started | already_started} |
    {error, wrong_reconfiguration | wrong_no_of_procs}).
ensure_throttle_processes_started(Name, Rate, Interval, NoOfProcesses) ->
    maybe_raise_event(Name, init),
    gen_server:call(?MASTER_SERVER, {start_processes, Name, Rate, Interval, NoOfProcesses}).

-spec run(name(), fun(() -> any())) -> ok | {error, any()}.
run(Name, Fn) ->
    case get_throttle_process(Name) of
        {ok, Pid} ->
            maybe_raise_event(Name, request),
            Fun =
                fun() ->
                    maybe_raise_event(Name, execute),
                    Fn()
                end,
            amoc_throttle_process:run(Pid, Fun),
            ok;
        Error -> Error
    end.

-spec pause(name()) -> ok | {error, any()}.
pause(Name) ->
    gen_server:call(?MASTER_SERVER, {pause, Name}).

-spec resume(name()) -> ok | {error, any()}.
resume(Name) ->
    gen_server:call(?MASTER_SERVER, {resume, Name}).

-spec change_rate(name(), amoc_throttle:rate(), amoc_throttle:interval()) -> ok | {error, any()}.
change_rate(Name, Rate, Interval) ->
    gen_server:call(?MASTER_SERVER, {change_rate, Name, Rate, Interval}).

-spec change_rate_gradually(name(), amoc_throttle:rate(), amoc_throttle:rate(),
                            amoc_throttle:interval(), pos_integer(), pos_integer()) ->
    ok | {error, any()}.
change_rate_gradually(Name, LowRate, HighRate, RateInterval, StepInterval, NoOfSteps) ->
    gen_server:call(?MASTER_SERVER, {change_rate_gradually, Name, LowRate, HighRate,
                                     RateInterval, StepInterval, NoOfSteps}).

-spec stop(name()) -> ok | {error, any()}.
stop(Name) ->
    gen_server:call(?MASTER_SERVER, {stop, Name}).

-spec telemetry_event(name(), request | execute) -> ok.
telemetry_event(Name, Event) when Event =:= request; Event =:= execute ->
    raise_event(Name, Event).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, #{}}.
init([]) ->
    {ok, #{}}.

-spec handle_call({start_processes, name(), pos_integer(), amoc_throttle:interval(), pos_integer()},
                  From :: {pid(), Tag :: term()}, state()) ->
                     {reply, {ok, started | already_started}, state()} |
                     {reply, {error, wrong_reconfiguration | wrong_no_of_procs}, state()};
                 ({pause | resume | stop}, From :: {pid(), Tag :: term()}, state()) ->
                     {reply, ok, state()} |
                     {reply, Error :: any(), state()};
                 ({change_rate, name(), amoc_throttle:rate(), amoc_throttle:interval()},
                  From :: {pid(), Tag :: term()}, state()) ->
                     {reply, ok, state()} |
                     {reply, {error, any()}, state()};
                 ({change_rate_gradually, name(), amoc_throttle:rate(), amoc_throttle:rate(),
                   amoc_throttle:interval(), pos_integer(), pos_integer()},
                  From :: {pid(), Tag :: term()}, state()) ->
                     {reply, ok, state()} |
                     {reply, {error, any()}, state()}.
handle_call({start_processes, Name, Rate, Interval, NoOfProcesses}, _From, State) ->
    case pg:get_members(?PG_SCOPE, Name) of
        [] ->
            RealNoOfProcesses = start_processes(Name, Rate, Interval, NoOfProcesses),
            {reply, {ok, started},
             State#{Name => #throttle_info{rate = Rate, interval = Interval, active = true,
                                           no_of_procs = RealNoOfProcesses}}};
        Group when is_list(Group) ->
            verify_new_start_matches_running(Name, Rate, Interval, NoOfProcesses, Group, State)
    end;
handle_call({pause, Name}, _From, State) ->
    case all_processes(Name, pause) of
        ok ->
            Info = maps:get(Name, State),
            {reply, ok, State#{Name => Info#throttle_info{active = false}}};
        Error -> {reply, Error, State}
    end;
handle_call({resume, Name}, _From, State) ->
    case all_processes(Name, resume) of
        ok ->
            Info = maps:get(Name, State),
            {reply, ok, State#{Name => Info#throttle_info{active = true}}};
        Error -> {reply, Error, State}
    end;
handle_call({change_rate, Name, Rate, Interval}, _From, State) ->
    case State of
        #{Name := Info} ->
            Info = maps:get(Name, State),
            case maybe_change_rate(Name, Rate, Interval, Info) of
                {ok, Rate} -> UpdatedInfo = Info#throttle_info{rate = Rate, interval = Interval},
                              {reply, ok, State#{Name => UpdatedInfo}};
                Error -> {reply, Error, State}
            end;
        _ -> {reply, {error, {no_throttle_by_name, Name}}, State}
    end;
handle_call({change_rate_gradually, Name, LowRate, HighRate,
             RateInterval, StepInterval, NoOfSteps},
            _From, State) ->
    case State of
        #{Name := Info} ->
            case Info#throttle_info.change_plan of
                undefined ->
                    NewInfo = start_gradual_rate_change(Name, LowRate, HighRate, RateInterval,
                                                        StepInterval, NoOfSteps, Info),
                    {reply, ok, State#{Name => NewInfo}};
                _ -> {reply, {error, cannot_change_rate}, State}
            end;
        _ -> {reply, {error, {no_throttle_by_name, Name}}, State}
    end;
handle_call({stop, Name}, _From, State) ->
    case all_processes(Name, stop) of
        ok ->
            {reply, ok, maps:remove(Name, State)};
        Error -> {reply, Error, State}
    end.

-spec(handle_cast(any(), state()) -> {noreply, state()}).
handle_cast(_, State) ->
    {noreply, State}.

-spec(handle_info({change_plan, name()}, state()) ->
    {noreply, state()}).
handle_info({change_plan, Name}, State) ->
    Info = maps:get(Name, State),
    Plan = Info#throttle_info.change_plan,
    case Plan#change_rate_plan.no_of_steps of
        1 -> NewState = change_rate_and_stop_plan(Name, State),
            {noreply, NewState};
        N when N > 1 -> NewState = continue_plan(Name, State),
            {noreply, NewState}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_raise_event(Name, Event) ->
    case amoc_cluster:master_node() =:= node() of
        true -> ok;
        _ -> raise_event(Name, Event)
    end.

raise_event(Name, Event) when Event =:= request; Event =:= execute; Event =:= init ->
    amoc_telemetry:execute([throttle, Event], #{count => 1}, #{name => Name}).

report_rate(Name, RatePerMinute) ->
    amoc_telemetry:execute([throttle, rate], #{rate => RatePerMinute}, #{name => Name}).

-spec change_rate_and_stop_plan(name(), state()) -> state().
change_rate_and_stop_plan(Name, State) ->
    Info = maps:get(Name, State),
    Plan = Info#throttle_info.change_plan,
    Interval = Info#throttle_info.interval,
    TRef = Plan#change_rate_plan.timer,
    HighRate = Plan#change_rate_plan.high_rate,

    {ok, _} = do_change_rate(Name, HighRate, Interval),
    {ok, cancel} = timer:cancel(TRef),
    consume_all_timer_ticks({change_plan, Name}),
    State#{Name => Info#throttle_info{rate = HighRate, change_plan = undefined}}.

consume_all_timer_ticks(Msg) ->
    receive
        Msg -> consume_all_timer_ticks(Msg)
    after 0 -> ok
    end.

-spec continue_plan(name(), state()) -> state().
continue_plan(Name, State) ->
    Info = maps:get(Name, State),
    Plan = Info#throttle_info.change_plan,
    LowRate = Info#throttle_info.rate,
    HighRate = Plan#change_rate_plan.high_rate,
    NoOfSteps = Plan#change_rate_plan.no_of_steps,

    Step = (HighRate - LowRate) div (NoOfSteps),
    NewRate = LowRate + Step,
    {ok, _} = do_change_rate(Name, NewRate, Info#throttle_info.interval),

    NewPlan = Plan#change_rate_plan{no_of_steps = NoOfSteps - 1},
    State#{Name => Info#throttle_info{rate = NewRate, change_plan = NewPlan}}.

-spec rate_per_minute(amoc_throttle:rate(), amoc_throttle:interval()) -> amoc_throttle:rate().
rate_per_minute(_, 0) -> 0;
rate_per_minute(Rate, Interval) ->
    (Rate * 60000) div Interval.

-spec start_processes(name(), amoc_throttle:rate(), amoc_throttle:interval(), pos_integer()) ->
    pos_integer().
start_processes(Name, Rate, Interval, NoOfProcesses) ->
    raise_event(Name, init),
    RatePerMinute = rate_per_minute(Rate, Interval),
    report_rate(Name, RatePerMinute),
    RealNoOfProcesses = max(1, min(Rate, NoOfProcesses)),
    start_throttle_processes(Name, Interval, Rate, RealNoOfProcesses),
    RealNoOfProcesses.

-spec get_throttle_process(name()) -> {error, {no_throttle_process_registered, name()}} |
                                      {error, any()} | {ok, pid()}.
get_throttle_process(Name) ->
    case pg:get_members(?PG_SCOPE, Name) of
        [] ->
            {error, {no_throttle_process_registered, Name}};
        List -> %% nonempty list
            N = rand:uniform(length(List)),
            {ok, lists:nth(N, List)}
    end.

-spec maybe_change_rate(name(), amoc_throttle:rate(), amoc_throttle:interval(), throttle_info()) ->
    {ok, non_neg_integer()} | {error, any()}.
maybe_change_rate(Name, Rate, Interval, Info) ->
    CurrentRatePerMin = rate_per_minute(Info#throttle_info.rate, Info#throttle_info.interval),
    ReqRatePerMin = rate_per_minute(Rate, Interval),
    case {CurrentRatePerMin, Info#throttle_info.change_plan} of
        {ReqRatePerMin, _} -> {ok, ReqRatePerMin};
        {_, undefined} -> do_change_rate(Name, Rate, Interval);
        _ -> {error, cannot_change_rate}
    end.

-spec do_change_rate(name(), amoc_throttle:rate(), amoc_throttle:interval()) ->
    {ok, non_neg_integer()} | {error, any()}.
do_change_rate(Name, Rate, Interval) ->
    case pg:get_members(?PG_SCOPE, Name) of
        [] -> {error, no_processes_in_group};
        List when is_list(List) ->
            RatePerMinute = rate_per_minute(Rate, Interval),
            report_rate(Name, RatePerMinute),
            update_throttle_processes(List, Interval, Rate, length(List)),
            {ok, RatePerMinute}
    end.

-spec start_gradual_rate_change(
        name(), amoc_throttle:rate(), amoc_throttle:rate(),
        amoc_throttle:interval(), pos_integer(), pos_integer(), throttle_info()) ->
    throttle_info().
start_gradual_rate_change(Name, LowRate, HighRate, RateInterval, StepInterval, NoOfSteps, Info) ->
    {ok, _} = do_change_rate(Name, LowRate, RateInterval),
    {ok, Timer} = timer:send_interval(StepInterval, {change_plan, Name}),
    Plan = #change_rate_plan{high_rate = HighRate, no_of_steps = NoOfSteps, timer = Timer},
    Info#throttle_info{rate = LowRate, interval = RateInterval, change_plan = Plan}.

start_throttle_processes(Name, Interval, Rate, N) ->
    ok = amoc_throttle_pool:start_process_pool(Name, Interval, Rate, N).

update_throttle_processes([Pid], Interval, Rate, 1) ->
    amoc_throttle_process:update(Pid, Interval, Rate);
update_throttle_processes([Pid | Tail], Interval, Rate, N) when N > 1 ->
    ProcessRate = Rate div N,
    amoc_throttle_process:update(Pid, Interval, ProcessRate),
    update_throttle_processes(Tail, Interval, Rate - ProcessRate, N - 1).

all_processes(Name, Cmd) ->
    case pg:get_members(?PG_SCOPE, Name) of
        [] -> {error, no_processes_in_group};
        Ps -> [run_cmd(P, Cmd) || P <- Ps], ok
    end.

verify_new_start_matches_running(Name, Rate, Interval, NoOfProcesses, Group, State) ->
    ExpectedNoOfProcesses = min(Rate, NoOfProcesses),
    case {length(Group), State} of
        {ExpectedNoOfProcesses, #{Name := #throttle_info{rate = Rate, interval = Interval}}} ->
            {reply, {ok, already_started}, State};
        {ExpectedNoOfProcesses, #{Name := #throttle_info{}}} ->
            {reply, {error, wrong_reconfiguration}, State};
        _ ->
            {reply, {error, wrong_no_of_procs}, State}
    end.

run_cmd(Pid, stop) ->
    amoc_throttle_process:stop(Pid);
run_cmd(Pid, pause) ->
    amoc_throttle_process:pause(Pid);
run_cmd(Pid, resume) ->
    amoc_throttle_process:resume(Pid).
