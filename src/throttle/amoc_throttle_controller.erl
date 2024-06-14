%% @private
%% @see amoc_throttle
%% @copyright 2024 Erlang Solutions Ltd.
%% @doc Manages throttle processes and rate changes.
-module(amoc_throttle_controller).

-behaviour(gen_server).

%% API
-export([start_link/0,
         ensure_throttle_processes_started/2,
         pause/1, resume/1, stop/1,
         change_rate/3, change_rate_gradually/2,
         raise_event_on_slave_node/2, telemetry_event/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-ifdef(TEST).
-export([verify_config/1]).
-endif.

-define(SERVER, ?MODULE).
-define(MASTER_SERVER, {?SERVER, amoc_cluster:master_node()}).
-define(DEFAULT_STEP_SIZE, 1).
-define(DEFAULT_INTERVAL, 60000). %% one minute
-define(DEFAULT_NO_PROCESSES, 10).
-define(NONNEG_INT(N), (is_integer(N) andalso N >= 0)).
-define(POS_INT(N), (is_integer(N) andalso N > 0)).

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
    step_size :: pos_integer(),
    timer :: timer:tref()}).

-type name() :: amoc_throttle:name().
-type change_rate_plan() :: #change_rate_plan{}.
-type throttle_info() :: #throttle_info{}.
-type state() :: #{name() => throttle_info()}.
-type event() :: init | execute | request.

-type config() :: #{rate := amoc_throttle:rate(),
                    interval := amoc_throttle:interval(),
                    parallelism := non_neg_integer()}.
-type gradual_rate_change() :: #{from_rate := amoc_throttle:rate(),
                                 to_rate := amoc_throttle:rate(),
                                 interval := amoc_throttle:interval(),
                                 step_interval := pos_integer(),
                                 step_size := pos_integer(),
                                 step_count := pos_integer()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec ensure_throttle_processes_started(name(), amoc_throttle:config()) ->
    {ok, started | already_started} |
    {error, invalid_throttle | wrong_reconfiguration | wrong_no_of_procs}.
ensure_throttle_processes_started(
  Name, #{interarrival := EveryMs} = Config)
  when is_atom(Name), ?NONNEG_INT(EveryMs) ->
    raise_event_on_slave_node(Name, init),
    Config1 = #{rate => ?DEFAULT_INTERVAL div EveryMs, interval => ?DEFAULT_INTERVAL},
    Config2 = Config1#{parallelism => maps:get(parallelism, Config, ?DEFAULT_NO_PROCESSES)},
    gen_server:call(?MASTER_SERVER, {start_processes, Name, Config2});
ensure_throttle_processes_started(
  Name, #{rate := Rate, interval := Interval, parallelism := NoOfProcesses} = Config)
  when is_atom(Name), ?POS_INT(Rate), ?NONNEG_INT(Interval), ?POS_INT(NoOfProcesses) ->
    raise_event_on_slave_node(Name, init),
    gen_server:call(?MASTER_SERVER, {start_processes, Name, Config});
ensure_throttle_processes_started(
  Name, #{rate := Rate, interval := Interval} = Config)
  when is_atom(Name), ?POS_INT(Rate), ?NONNEG_INT(Interval) ->
    raise_event_on_slave_node(Name, init),
    Config1 = Config#{parallelism => ?DEFAULT_NO_PROCESSES},
    gen_server:call(?MASTER_SERVER, {start_processes, Name, Config1});
ensure_throttle_processes_started(
  Name, #{rate := Rate} = Config)
  when is_atom(Name), ?POS_INT(Rate) ->
    raise_event_on_slave_node(Name, init),
    Config1 = Config#{interval => ?DEFAULT_INTERVAL, parallelism => ?DEFAULT_NO_PROCESSES},
    gen_server:call(?MASTER_SERVER, {start_processes, Name, Config1});
ensure_throttle_processes_started(_Name, _Config) ->
    {error, invalid_throttle}.

-spec pause(name()) -> ok | {error, any()}.
pause(Name) ->
    gen_server:call(?MASTER_SERVER, {pause, Name}).

-spec resume(name()) -> ok | {error, any()}.
resume(Name) ->
    gen_server:call(?MASTER_SERVER, {resume, Name}).

-spec change_rate(name(), amoc_throttle:rate(), amoc_throttle:interval()) -> ok | {error, any()}.
change_rate(Name, Rate, Interval) ->
    gen_server:call(?MASTER_SERVER, {change_rate, Name, Rate, Interval}).

-spec change_rate_gradually(name(), amoc_throttle:gradual_rate_config()) -> ok | {error, any()}.
change_rate_gradually(Name, GradualChangeRate) ->
    case verify_config(GradualChangeRate) of
        {error, _} = Error ->
            Error;
        Config ->
            gen_server:call(?MASTER_SERVER, {change_rate_gradually, Name, Config})
    end.

-spec stop(name()) -> ok | {error, any()}.
stop(Name) ->
    gen_server:call(?MASTER_SERVER, {stop, Name}).

-spec telemetry_event(name(), request | execute) -> ok.
telemetry_event(Name, Event) when Event =:= request; Event =:= execute ->
    raise_event(Name, Event).

%% The purpose of this function is to ensure that there are no event duplicates if we are running in
%% a single (non-distributed) node, as the throttle process will already raise this event.
-spec raise_event_on_slave_node(name(), event()) -> ok.
raise_event_on_slave_node(Name, Event) ->
    case amoc_cluster:master_node() =:= node() of
        true -> ok;
        _ -> raise_event(Name, Event)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, #{}}.
init([]) ->
    {ok, #{}}.

-spec handle_call({start_processes, name(), config()},
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
                 ({change_rate_gradually, name(), gradual_rate_change()},
                  From :: {pid(), Tag :: term()}, state()) ->
                     {reply, ok, state()} |
                     {reply, {error, any()}, state()}.
handle_call({start_processes, Name, Config}, _From, State) ->
    case amoc_throttle_process:get_throttle_processes(Name) of
        {error, no_throttle_process_registered} ->
            Info = start_processes(Name, Config),
            NewState = State#{Name => Info},
            {reply, {ok, started}, NewState};
        {ok, Group} ->
            verify_new_start_matches_running(Name, Config, Group, State)
    end;
handle_call({pause, Name}, _From, State) ->
    case run_in_all_processes(Name, pause) of
        ok ->
            Info = maps:get(Name, State),
            {reply, ok, State#{Name => Info#throttle_info{active = false}}};
        Error ->
            {reply, Error, State}
    end;
handle_call({resume, Name}, _From, State) ->
    case run_in_all_processes(Name, resume) of
        ok ->
            Info = maps:get(Name, State),
            {reply, ok, State#{Name => Info#throttle_info{active = true}}};
        Error -> {reply, Error, State}
    end;
handle_call({change_rate, Name, Rate, Interval}, _From, State) ->
    case State of
        #{Name := Info} ->
            case maybe_change_rate(Name, Rate, Interval, Info) of
                ok ->
                    UpdatedInfo = Info#throttle_info{rate = Rate, interval = Interval},
                    {reply, ok, State#{Name => UpdatedInfo}};
                Error ->
                    {reply, Error, State}
            end;
        _ ->
            {reply, {error, {no_throttle_by_name, Name}}, State}
    end;
handle_call({change_rate_gradually, Name, GradualChangeRate}, _From, State) ->
    case State of
        #{Name := #throttle_info{change_plan = undefined} = Info} ->
            NewInfo = start_gradual_rate_change(Name, Info, GradualChangeRate),
            {reply, ok, State#{Name => NewInfo}};
        #{Name := _} ->
            {reply, {error, cannot_change_rate}, State};
        _ ->
            {reply, {error, {no_throttle_by_name, Name}}, State}
    end;
handle_call({stop, Name}, _From, State) ->
    case run_in_all_processes(Name, stop) of
        ok ->
            {reply, ok, maps:remove(Name, State)};
        Error ->
            {reply, Error, State}
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
        1 -> NewState = change_rate_and_stop_plan(Name, State, Info, Plan),
            {noreply, NewState};
        N when N > 1 -> NewState = continue_plan(Name, State, Info, Plan),
            {noreply, NewState}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

raise_event(Name, Event) when Event =:= request; Event =:= execute; Event =:= init ->
    amoc_telemetry:execute([throttle, Event], #{count => 1}, #{name => Name}).

report_rate(Name, RatePerMinute) ->
    amoc_telemetry:execute([throttle, rate], #{rate => RatePerMinute}, #{name => Name}).

-spec change_rate_and_stop_plan(name(), state(), throttle_info(), change_rate_plan()) -> state().
change_rate_and_stop_plan(Name, State, Info, Plan) ->
    Interval = Info#throttle_info.interval,
    TRef = Plan#change_rate_plan.timer,
    HighRate = Plan#change_rate_plan.high_rate,
    ok = do_change_rate(Name, HighRate, Interval),
    {ok, cancel} = timer:cancel(TRef),
    consume_all_timer_ticks({change_plan, Name}),
    State#{Name => Info#throttle_info{rate = HighRate, change_plan = undefined}}.

consume_all_timer_ticks(Msg) ->
    receive
        Msg -> consume_all_timer_ticks(Msg)
    after 0 -> ok
    end.

-spec continue_plan(name(), state(), throttle_info(), change_rate_plan()) -> state().
continue_plan(Name, State, Info, Plan) ->
    LowRate = Info#throttle_info.rate,
    NoOfSteps = Plan#change_rate_plan.no_of_steps,
    StepSize = Plan#change_rate_plan.step_size,
    NewRate = LowRate + StepSize,
    ok = do_change_rate(Name, NewRate, Info#throttle_info.interval),
    NewPlan = Plan#change_rate_plan{no_of_steps = NoOfSteps - 1},
    State#{Name => Info#throttle_info{rate = NewRate, change_plan = NewPlan}}.

-spec rate_per_minute(amoc_throttle:rate(), amoc_throttle:interval()) -> amoc_throttle:rate().
rate_per_minute(_, 0) -> 0;
rate_per_minute(Rate, Interval) ->
    (Rate * 60000) div Interval.

-spec start_processes(name(), config()) -> throttle_info().
start_processes(Name, #{rate := Rate, interval := Interval, parallelism := NoOfProcesses}) ->
    raise_event(Name, init),
    RatePerMinute = rate_per_minute(Rate, Interval),
    report_rate(Name, RatePerMinute),
    RealNoOfProcs = min(Rate, NoOfProcesses),
    start_throttle_processes(Name, Interval, Rate, RealNoOfProcs),
    #throttle_info{rate = Rate, interval = Interval, active = true, no_of_procs = RealNoOfProcs}.

-spec maybe_change_rate(name(), amoc_throttle:rate(), amoc_throttle:interval(), throttle_info()) ->
    ok | {error, any()}.
maybe_change_rate(Name, Rate, Interval, Info) ->
    CurrentRatePerMin = rate_per_minute(Info#throttle_info.rate, Info#throttle_info.interval),
    ReqRatePerMin = rate_per_minute(Rate, Interval),
    case {CurrentRatePerMin, Info#throttle_info.change_plan} of
        {ReqRatePerMin, _} -> ok;
        {_, undefined} -> do_change_rate(Name, Rate, Interval);
        _ -> {error, cannot_change_rate}
    end.

-spec do_change_rate(name(), amoc_throttle:rate(), amoc_throttle:interval()) -> ok | {error, any()}.
do_change_rate(Name, Rate, Interval) ->
    case amoc_throttle_process:get_throttle_processes(Name) of
        {ok, List} ->
            RatePerMinute = rate_per_minute(Rate, Interval),
            report_rate(Name, RatePerMinute),
            update_throttle_processes(List, Interval, Rate, length(List)),
            ok;
        Error ->
            Error
    end.

-spec start_gradual_rate_change(name(), throttle_info(), gradual_rate_change()) ->
    throttle_info().
start_gradual_rate_change(Name, Info,
  #{from_rate := LowRate, to_rate := HighRate, interval := RateInterval,
    step_interval := StepInterval, step_count := StepCount, step_size := StepSize}) ->
    ok = do_change_rate(Name, LowRate, RateInterval),
    {ok, Timer} = timer:send_interval(StepInterval, {change_plan, Name}),
    Plan = #change_rate_plan{high_rate = HighRate, timer = Timer,
                             no_of_steps = StepCount, step_size = StepSize},
    Info#throttle_info{rate = LowRate, interval = RateInterval, change_plan = Plan}.

start_throttle_processes(Name, Interval, Rate, N) ->
    ok = amoc_throttle_pool:start_process_pool(Name, Interval, Rate, N).

update_throttle_processes([Pid], Interval, Rate, 1) ->
    amoc_throttle_process:update(Pid, Interval, Rate);
update_throttle_processes([Pid | Tail], Interval, Rate, N) when N > 1 ->
    ProcessRate = Rate div N,
    amoc_throttle_process:update(Pid, Interval, ProcessRate),
    update_throttle_processes(Tail, Interval, Rate - ProcessRate, N - 1).

run_in_all_processes(Name, Cmd) ->
    case amoc_throttle_process:get_throttle_processes(Name) of
        {ok, List} ->
            [run_cmd(P, Cmd) || P <- List],
            ok;
        Error ->
            Error
    end.

verify_new_start_matches_running(Name, Config, Group, State) ->
    #{rate := Rate, interval := Interval, parallelism := NoOfProcesses} = Config,
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

-spec verify_config(amoc_throttle:gradual_rate_config()) -> gradual_rate_change() | {error, any()}.
verify_config(Config) ->
    try do_verify_config(Config) of
        Change -> Change
    catch error:Reason ->
              {error, Reason}
    end.

step_size_sign(From, To) when From =< To -> 1;
step_size_sign(From, To) when From > To -> -1.

check_step_size_with_from_to_rate(From, To, StepSize) when From =< To, StepSize >= 0 -> ok;
check_step_size_with_from_to_rate(From, To, StepSize) when From > To, StepSize < 0 -> ok.

check_step_parameters(StepSize, StepSize) -> ok.

-spec do_verify_config(amoc_throttle:gradual_rate_config()) -> gradual_rate_change().
do_verify_config(
  #{from_rate := FromRate, to_rate := ToRate,
    step_interval := StepInterval, step_count := StepCount, step_size := StepSize} = Config) ->
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    check_step_size_with_from_to_rate(FromRate, ToRate, StepSize),
    check_step_parameters((ToRate - FromRate) div StepCount, StepSize),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_config(
  #{from_rate := FromRate, to_rate := ToRate, duration := Duration}) ->
    StepSize = ?DEFAULT_STEP_SIZE * step_size_sign(FromRate, ToRate),
    StepCount = abs((ToRate - FromRate) div StepSize),
    StepInterval = abs(Duration div (ToRate - FromRate)),
    #{from_rate => FromRate, to_rate => ToRate, interval => ?DEFAULT_INTERVAL,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_config(
  #{from_rate := FromRate, to_rate := ToRate,
    step_interval := StepInterval, step_size := StepSize} = Config) ->
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    check_step_size_with_from_to_rate(FromRate, ToRate, StepSize),
    StepCount = abs((ToRate - FromRate) div StepSize),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_config(
  #{from_rate := FromRate, to_rate := ToRate, step_interval := StepInterval} = Config) ->
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    StepSize = ?DEFAULT_STEP_SIZE * step_size_sign(FromRate, ToRate),
    StepCount = abs((ToRate - FromRate) div StepSize),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_config(
  #{from_rate := FromRate, to_rate := ToRate, step_count := StepCount} = Config) ->
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    StepSize = ?DEFAULT_STEP_SIZE * step_size_sign(FromRate, ToRate),
    StepInterval = (ToRate - FromRate) div (StepSize * StepCount),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_config(
  #{from_rate := FromRate, to_rate := ToRate, step_size := StepSize} = Config) ->
    check_step_size_with_from_to_rate(FromRate, ToRate, StepSize),
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    StepCount = abs((ToRate - FromRate) div StepSize),
    StepInterval = abs((ToRate - FromRate) div StepCount),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_config(
  #{from_rate := FromRate, to_rate := ToRate} = Config) ->
    StepSize = ?DEFAULT_STEP_SIZE * step_size_sign(FromRate, ToRate),
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    StepCount = abs((ToRate - FromRate) div StepSize),
    StepInterval = abs((ToRate - FromRate) div StepCount),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize}.
