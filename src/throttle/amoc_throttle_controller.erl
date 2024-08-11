%% @private
%% @see amoc_throttle
%% @copyright 2024 Erlang Solutions Ltd.
%% @doc Manages throttle processes and rate changes.
-module(amoc_throttle_controller).

-behaviour(gen_server).

%% API
-export([start_link/0,
         ensure_throttle_processes_started/2,
         pause/1, resume/1, unlock/1, stop/1, get_info/1,
         change_rate/2, change_rate_gradually/2,
         pg_scope/0,
         get_throttle_process/1,
         raise_event_on_slave_node/2, telemetry_event/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-ifdef(TEST).
-export([verify_gradual_config/1]).
-endif.

-define(PG_SCOPE, amoc_throttle).
-define(SERVER, ?MODULE).
-define(MASTER_SERVER, {?SERVER, amoc_cluster:master_node()}).
-define(DEFAULT_STEP_SIZE, 1).
-define(DEFAULT_INTERVAL, 60000). %% one minute
-define(TIMEOUT(N), (infinity =:= N orelse is_integer(N) andalso N >= 0)).
-define(POS_INT(N), (is_integer(N) andalso N > 0)).

-record(throttle_info, {
    pool_sup :: pid(),
    pool_config :: pool_config(),
    rate :: amoc_throttle:rate(),
    interval :: amoc_throttle:interval(),
    active = true :: boolean(),
    change_plan :: undefined | change_rate_plan()
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
                    interval := amoc_throttle:interval()}.
-type gradual() :: #{from_rate := non_neg_integer(),
                     to_rate := non_neg_integer(),
                     interval := amoc_throttle:interval(),
                     step_interval := pos_integer(),
                     step_size := pos_integer(),
                     step_count := pos_integer()}.
-type pool_config() :: #{ProcessN :: non_neg_integer() :=
                         #{max_n := infinity | non_neg_integer(),
                           delay := non_neg_integer(),
                           status := active | inactive,
                           pid => undefined | pid()}}.
-export_type([pool_config/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec pg_scope() -> atom().
pg_scope() ->
    ?PG_SCOPE.

-spec no_of_processes() -> non_neg_integer().
no_of_processes() ->
    3 * erlang:system_info(schedulers_online).

-spec get_throttle_process(amoc_throttle:name()) ->
    {error, no_throttle_process_registered} | {ok, pid()}.
get_throttle_process(Name) ->
    case pg:get_members(?PG_SCOPE, Name) of
        [] ->
            {error, no_throttle_process_registered};
        List -> %% nonempty list
            N = rand:uniform(length(List)),
            {ok, lists:nth(N, List)}
    end.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec ensure_throttle_processes_started(name(), amoc_throttle:t()) ->
    {ok, started | already_started} |
    {error, invalid_throttle | wrong_reconfiguration | error_starting_pool}.
ensure_throttle_processes_started(Name, Config) when is_atom(Name) ->
    case verify_config(Config) of
        {error, invalid_throttle} ->
            {error, invalid_throttle};
        VerifiedConfig ->
            raise_event_on_slave_node(Name, init),
            gen_server:call(?MASTER_SERVER, {start_throttle, Name, VerifiedConfig})
    end.

-spec change_rate(name(), amoc_throttle:t()) -> ok | {error, any()}.
change_rate(Name, Config) ->
    case verify_config(Config) of
        {error, invalid_throttle} ->
            {error, invalid_throttle};
        VerifiedConfig ->
            gen_server:call(?MASTER_SERVER, {change_rate, Name, VerifiedConfig})
    end.

-spec change_rate_gradually(name(), amoc_throttle:gradual_rate_config()) -> ok | {error, any()}.
change_rate_gradually(Name, GradualChangeRate) ->
    case verify_gradual_config(GradualChangeRate) of
        {error, _} = Error ->
            Error;
        VerifiedConfig ->
            gen_server:call(?MASTER_SERVER, {change_rate_gradually, Name, VerifiedConfig})
    end.

-spec pause(name()) -> ok | {error, any()}.
pause(Name) ->
    gen_server:call(?MASTER_SERVER, {pause, Name}).

-spec resume(name()) -> ok | {error, any()}.
resume(Name) ->
    gen_server:call(?MASTER_SERVER, {resume, Name}).

-spec unlock(name()) -> ok | {error, any()}.
unlock(Name) ->
    gen_server:call(?MASTER_SERVER, {unlock, Name}).

-spec get_info(name()) -> #{_ := _} | {error, any()}.
get_info(Name) ->
    gen_server:call(?MASTER_SERVER, {get_info, Name}).

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

-spec handle_call({start_throttle, name(), config()}, gen_server:from(), state()) ->
                     {reply, {ok, started | already_started}, state()} |
                     {reply, {error, wrong_reconfiguration | error_starting_pool}, state()};
                 ({change_rate, name(), config()}, gen_server:from(), state()) ->
                     {reply, ok, state()} |
                     {reply, {error, any()}, state()};
                 ({change_rate_gradually, name(), gradual()}, gen_server:from(), state()) ->
                     {reply, ok, state()} |
                     {reply, {error, any()}, state()};
                 ({pause | resume | unlock | stop}, gen_server:from(), state()) ->
                     {reply, ok, state()} |
                     {reply, Error :: any(), state()};
                 ({get_info, name()}, gen_server:from(), state()) ->
                     {reply, #{_ := _}, state()} |
                     {reply, {error, any()}, state()}.
handle_call({start_throttle, Name, #{rate := Rate, interval := Interval}}, _From, State) ->
    case State of
        #{Name := #throttle_info{rate = Rate, interval = Interval}} ->
            {reply, {ok, already_started}, State};
        #{Name := #throttle_info{}} ->
            {reply, {error, wrong_reconfiguration}, State};
        _ ->
            do_start_throttle(Name, Rate, Interval, State)
    end;
handle_call({change_rate, Name, #{rate := Rate, interval := Interval}}, _From, State) ->
    case State of
        #{Name := #throttle_info{rate = Rate, interval = Interval}} ->
            {reply, ok, State};
        #{Name := #throttle_info{change_plan = undefined} = Info} ->
            do_change_rate(Name, Rate, Interval, Info, State);
        #{Name := #throttle_info{}} ->
            {reply, {error, cannot_change_rate}, State};
        _ ->
            {reply, {error, {no_throttle_by_name, Name}}, State}
    end;
handle_call({change_rate_gradually, Name, GradualChangeRate}, _From, State) ->
    case State of
        #{Name := #throttle_info{change_plan = undefined} = Info} ->
            do_gradual_change_rate(Name, Info, GradualChangeRate, State);
        #{Name := _} ->
            {reply, {error, cannot_change_rate}, State};
        _ ->
            {reply, {error, {no_throttle_by_name, Name}}, State}
    end;
handle_call({Op, Name}, _From, State)
  when stop =:= Op; pause =:= Op; unlock =:= Op; resume =:= Op ->
    case State of
        #{Name := Info} ->
            do_run_op(Op, Name, Info, State);
        _ ->
            {reply, {error, {no_throttle_by_name, Name}}, State}
    end;
handle_call({get_info, Name}, _From, State) ->
    Info = maps:get(Name, State),
    Fields = record_info(fields, throttle_info),
    [_ | Values] = tuple_to_list(Info),
    Ret = maps:from_list(lists:zip(Fields, Values)),
    {reply, Ret, State}.

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

report_rate(Name, infinity, _) ->
    amoc_telemetry:execute([throttle, rate], #{rate_per_minute => infinity}, #{name => Name});
report_rate(Name, Rate, Interval) ->
    RatePerMinute = (Rate * 60000) div Interval,
    amoc_telemetry:execute([throttle, rate], #{rate_per_minute => RatePerMinute}, #{name => Name}).

-spec do_start_throttle(name(), amoc_throttle:rate(), amoc_throttle:interval(), state()) ->
    {reply, {ok, started}, state()} |
    {reply, {error, error_starting_pool}, state()}.
do_start_throttle(Name, Rate, Interval, State) ->
    PoolConfig = pool_config(Rate, Interval),
    case amoc_throttle_pooler:start_pool(Name, PoolConfig) of
        {ok, PoolSup} when is_pid(PoolSup) ->
            PoolConfig1 = process_config(Name, PoolSup, PoolConfig),
            raise_event(Name, init),
            report_rate(Name, Rate, Interval),
            Info = #throttle_info{pool_sup = PoolSup, pool_config = PoolConfig1,
                                  rate = Rate, interval = Interval},
            NewState = State#{Name => Info},
            {reply, {ok, started}, NewState};
        _ ->
            {reply, {error, error_starting_pool}, State}
    end.

-spec do_change_rate(
        name(), amoc_throttle:rate(), amoc_throttle:interval(), throttle_info(), state()) ->
    {reply, ok, state()} |
    {reply, {error, any()}, state()}.
do_change_rate(Name, Rate, Interval, Info, State) ->
    NewInfo = do_change_rate(Name, Rate, Interval, Info),
    {reply, ok, State#{Name => NewInfo}}.

-spec do_change_rate(name(), amoc_throttle:rate(), amoc_throttle:interval(), throttle_info()) ->
    throttle_info().
do_change_rate(Name, Rate, Interval, #throttle_info{pool_config = OldPoolConfig} = Info) ->
    NewPoolConfig = pool_config(Rate, Interval),
    report_rate(Name, Rate, Interval),
    PoolConfig1 = update_throttle_processes(Name, OldPoolConfig, NewPoolConfig),
    Info#throttle_info{rate = Rate, interval = Interval, pool_config = PoolConfig1}.

-spec change_rate_and_stop_plan(name(), state(), throttle_info(), change_rate_plan()) -> state().
change_rate_and_stop_plan(Name, State, Info, Plan) ->
    Interval = Info#throttle_info.interval,
    TRef = Plan#change_rate_plan.timer,
    HighRate = Plan#change_rate_plan.high_rate,
    Info1 = do_change_rate(Name, HighRate, Interval, Info),
    {ok, cancel} = timer:cancel(TRef),
    consume_all_timer_ticks({change_plan, Name}),
    State#{Name => Info1#throttle_info{change_plan = undefined}}.

do_run_op(stop, Name, #throttle_info{pool_sup = PoolSup}, State) ->
    ok = amoc_throttle_pooler:stop_pool(PoolSup),
    {reply, ok, maps:remove(Name, State)};
do_run_op(pause, Name, #throttle_info{pool_config = PoolConfig} = Info, State) ->
    Fun = fun(_, #{pid := Pid}) ->
                  amoc_throttle_process:update(Pid, 0, infinity)
          end,
    maps:foreach(Fun, PoolConfig),
    {reply, ok, State#{Name => Info#throttle_info{active = false}}};
do_run_op(unlock, Name, #throttle_info{pool_config = PoolConfig} = Info, State) ->
    Fun = fun(_, #{pid := Pid}) ->
                  amoc_throttle_process:update(Pid, infinity, 0)
          end,
    maps:foreach(Fun, PoolConfig),
    {reply, ok, State#{Name => Info#throttle_info{active = true}}};
do_run_op(resume, Name, #throttle_info{pool_config = PoolConfig} = Info, State) ->
    Fun = fun(_, #{max_n := MaxN, delay := Delay, pid := Pid}) ->
                  amoc_throttle_process:update(Pid, MaxN, Delay)
          end,
    maps:foreach(Fun, PoolConfig),
    {reply, ok, State#{Name => Info#throttle_info{active = true}}}.

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
    Info1 = do_change_rate(Name, NewRate, Info#throttle_info.interval, Info),
    NewPlan = Plan#change_rate_plan{no_of_steps = NoOfSteps - 1},
    State#{Name => Info1#throttle_info{change_plan = NewPlan}}.

%% Helpers

-spec pool_config(amoc_throttle:rate(), amoc_throttle:interval()) -> pool_config().
pool_config(infinity, _) ->
    Config = #{max_n => infinity, delay => 0, status => active, pid => undefined},
    maps:from_keys(lists:seq(1, no_of_processes()), Config);
pool_config(0, _) ->
    Config = #{max_n => 0, delay => infinity, status => active, pid => undefined},
    maps:from_keys(lists:seq(1, no_of_processes()), Config);
pool_config(Rate, Interval) ->
    NoOfProcesses = no_of_processes(),
    RatePerMinutePerProcess = (60000 * Rate div Interval) div NoOfProcesses,
    DelayPerProcess = (NoOfProcesses * Interval) div Rate,
    Rem = ((60000 * Rate div Interval) rem NoOfProcesses)
            + ((NoOfProcesses * Interval) rem Rate),
    Fun = fun(N, {Acc, R}) ->
                  case {RatePerMinutePerProcess < NoOfProcesses, R} of
                      {true, 0} ->
                          Config = #{max_n => RatePerMinutePerProcess,
                                     delay => DelayPerProcess + 1,
                                     status => inactive, pid => undefined},
                          {Acc#{N => Config}, R};
                      {true, R} ->
                          Config = #{max_n => RatePerMinutePerProcess,
                                     delay => DelayPerProcess,
                                     status => active, pid => undefined},
                          {Acc#{N => Config}, R - 1};
                      {false, 0} ->
                          Config = #{max_n => RatePerMinutePerProcess,
                                     delay => DelayPerProcess,
                                     status => active, pid => undefined},
                          {Acc#{N => Config}, R};
                      {false, R} ->
                          Config = #{max_n => RatePerMinutePerProcess,
                                     delay => DelayPerProcess + 1,
                                     status => active, pid => undefined},
                          {Acc#{N => Config}, R - 1}
                  end
          end,
    {PoolConfig, _} = lists:foldl(Fun, {#{}, Rem}, lists:seq(1, NoOfProcesses)),
    PoolConfig.

-spec process_config(amoc_throttle:name(), pid(), pool_config()) -> pool_config().
process_config(Name, PoolSup, PoolConfig) ->
    Processes = supervisor:which_children(PoolSup),
    Workers = [ {N, Pid} || {{amoc_throttle_process, N}, Pid, _, _} <- Processes, is_pid(Pid) ],
    Fun1 = fun(N, Config) ->
                   {_, Pid} = lists:keyfind(N, 1, Workers),
                   Config#{pid => Pid}
           end,
    PoolConfig1 = maps:map(Fun1, PoolConfig),
    process_pool(Name, PoolConfig1).

-spec process_pool(amoc_throttle:name(), pool_config()) -> pool_config().
process_pool(Name, PoolConfig1) ->
    Fun2 = fun({_, #{status := active, pid := Pid}}) ->
                   {true, Pid};
              (_) ->
                   false
           end,
    Pids = lists:filtermap(Fun2, maps:to_list(PoolConfig1)),
    pg:join(?PG_SCOPE, Name, Pids),
    PoolConfig1.

-spec do_gradual_change_rate(name(), throttle_info(), gradual(), state()) ->
    {reply, ok, state()}.
do_gradual_change_rate(Name, Info,
  #{from_rate := LowRate, to_rate := HighRate, interval := RateInterval,
    step_interval := StepInterval, step_count := StepCount, step_size := StepSize}, State) ->
    Info1 = do_change_rate(Name, LowRate, RateInterval, Info),
    {ok, Timer} = timer:send_interval(StepInterval, {change_plan, Name}),
    Plan = #change_rate_plan{high_rate = HighRate, timer = Timer,
                             no_of_steps = StepCount, step_size = StepSize},
    NewInfo = Info1#throttle_info{rate = LowRate, interval = RateInterval, change_plan = Plan},
    {reply, ok, State#{Name => NewInfo}}.

-spec update_throttle_processes(amoc_throttle:name(), pool_config(), pool_config()) ->
    pool_config().
update_throttle_processes(Name, OldPoolConfig, NewPoolConfig) ->
    Fun = fun(N, #{status := Status, delay := Delay, max_n := MaxN} = V, {C, J, L}) ->
                  #{status := OldStatus, pid := Pid} = maps:get(N, C),
                  amoc_throttle_process:update(Pid, MaxN, Delay),
                  case {Status, OldStatus} of
                      {active, inactive} ->
                          {C#{N := V#{pid := Pid}}, [Pid | J], L};
                      {inactive, active} ->
                          {C#{N := V#{pid := Pid}}, J, [Pid | L]};
                      {Same, Same} ->
                          {C#{N := V#{pid := Pid}}, J, L}
                  end
          end,
    {PoolConfig, Join, Leave} = maps:fold(Fun, {OldPoolConfig, [], []}, NewPoolConfig),
    pg:join(?PG_SCOPE, Name, Join),
    pg:leave(?PG_SCOPE, Name, Leave),
    PoolConfig.

-spec verify_config(amoc_throttle:t()) -> config() | {error, any()}.
verify_config(#{interarrival := infinity} = Config)
  when 1 =:= map_size(Config) ->
    #{rate => 0, interval => ?DEFAULT_INTERVAL};
verify_config(#{interarrival := 0} = Config)
  when 1 =:= map_size(Config) ->
    #{rate => infinity, interval => ?DEFAULT_INTERVAL};
verify_config(#{interarrival := Interarrival} = Config)
  when 1 =:= map_size(Config), ?POS_INT(Interarrival) ->
    #{rate => ?DEFAULT_INTERVAL div Interarrival, interval => ?DEFAULT_INTERVAL};
verify_config(#{rate := Rate, interval := Interval} = Config)
  when 2 =:= map_size(Config), ?TIMEOUT(Rate), ?POS_INT(Interval) ->
    Config;
verify_config(#{rate := Rate} = Config)
  when 1 =:= map_size(Config), ?TIMEOUT(Rate) ->
    Config#{interval => ?DEFAULT_INTERVAL};
verify_config(_Config) ->
    {error, invalid_throttle}.

-spec verify_gradual_config(amoc_throttle:gradual_rate_config()) ->
    gradual() | {error, any()}.
verify_gradual_config(Config) ->
    try do_verify_gradual_config(Config) of
        Change -> Change
    catch error:Reason ->
              {error, Reason}
    end.

step_size_sign(From, To) when From =< To -> 1;
step_size_sign(From, To) when From > To -> -1.

check_step_size_with_from_to_rate(From, To, StepSize) when From =< To, StepSize >= 0 -> ok;
check_step_size_with_from_to_rate(From, To, StepSize) when From > To, StepSize < 0 -> ok.

check_step_parameters(StepSize, StepSize) -> ok.

-spec do_verify_gradual_config(amoc_throttle:gradual_rate_config()) -> gradual().
do_verify_gradual_config(
  #{from_interarrival := FromInterarrival, to_interarrival := ToInterarrival} = Config0) ->
    FromRate = ?DEFAULT_INTERVAL div FromInterarrival,
    ToRate = ?DEFAULT_INTERVAL div ToInterarrival,
    Config1 = Config0#{from_rate => FromRate, to_rate => ToRate},
    do_verify_gradual_config(maps:without([from_interarrival, to_interarrival], Config1));
do_verify_gradual_config(
  #{from_rate := FromRate, to_rate := ToRate,
    step_interval := StepInterval, step_count := StepCount, step_size := StepSize} = Config) ->
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    check_step_size_with_from_to_rate(FromRate, ToRate, StepSize),
    check_step_parameters((ToRate - FromRate) div StepCount, StepSize),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_gradual_config(
  #{from_rate := FromRate, to_rate := ToRate, duration := Duration}) ->
    StepSize = ?DEFAULT_STEP_SIZE * step_size_sign(FromRate, ToRate),
    StepCount = abs((ToRate - FromRate) div StepSize),
    StepInterval = abs(Duration div (ToRate - FromRate)),
    #{from_rate => FromRate, to_rate => ToRate, interval => ?DEFAULT_INTERVAL,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_gradual_config(
  #{from_rate := FromRate, to_rate := ToRate,
    step_interval := StepInterval, step_size := StepSize} = Config) ->
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    check_step_size_with_from_to_rate(FromRate, ToRate, StepSize),
    StepCount = abs((ToRate - FromRate) div StepSize),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_gradual_config(
  #{from_rate := FromRate, to_rate := ToRate, step_interval := StepInterval} = Config) ->
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    StepSize = ?DEFAULT_STEP_SIZE * step_size_sign(FromRate, ToRate),
    StepCount = abs((ToRate - FromRate) div StepSize),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_gradual_config(
  #{from_rate := FromRate, to_rate := ToRate, step_count := StepCount} = Config) ->
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    StepSize = ?DEFAULT_STEP_SIZE * step_size_sign(FromRate, ToRate),
    StepInterval = (ToRate - FromRate) div (StepSize * StepCount),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_gradual_config(
  #{from_rate := FromRate, to_rate := ToRate, step_size := StepSize} = Config) ->
    check_step_size_with_from_to_rate(FromRate, ToRate, StepSize),
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    StepCount = abs((ToRate - FromRate) div StepSize),
    StepInterval = abs((ToRate - FromRate) div StepCount),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize};
do_verify_gradual_config(
  #{from_rate := FromRate, to_rate := ToRate} = Config) ->
    StepSize = ?DEFAULT_STEP_SIZE * step_size_sign(FromRate, ToRate),
    RateInterval = maps:get(interval, Config, ?DEFAULT_INTERVAL),
    StepCount = abs((ToRate - FromRate) div StepSize),
    StepInterval = abs((ToRate - FromRate) div StepCount),
    #{from_rate => FromRate, to_rate => ToRate, interval => RateInterval,
      step_interval => StepInterval, step_count => StepCount, step_size => StepSize}.
