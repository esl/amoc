%% @private
%% @see amoc_throttle
%% @copyright 2024 Erlang Solutions Ltd.
%% @doc Manages throttle processes and rate changes.
-module(amoc_throttle_controller).

-behaviour(gen_server).

%% API
-export([start_link/0,
         ensure_throttle_processes_started/2,
         pause/1, resume/1, stop/1, get_info/1,
         change_rate/2, change_rate_gradually/2,
         pg_scope/0,
         get_throttle_process/1,
         raise_event_on_slave_node/2, telemetry_event/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(PG_SCOPE, amoc_throttle).
-define(SERVER, ?MODULE).
-define(MASTER_SERVER, {?SERVER, amoc_cluster:master_node()}).

-record(throttle_info, {
    pool_sup :: pid(),
    pool_config :: amoc_throttle_config:pool_config(),
    rate :: amoc_throttle:rate(),
    interval :: amoc_throttle:interval(),
    active = true :: boolean(),
    change_plan :: undefined | change_rate_plan()
}).

-record(change_rate_plan, {
    rates :: [non_neg_integer()],
    timer :: timer:tref()
}).

-type name() :: amoc_throttle:name().
-type change_rate_plan() :: #change_rate_plan{}.
-type throttle_info() :: #throttle_info{}.
-type state() :: #{name() => throttle_info()}.
-type event() :: init | execute | request.

-type start_throttle() :: {start_throttle, name(), amoc_throttle_config:config()}.
-type change_rate() :: {change_rate, name(), amoc_throttle_config:config()}.
-type change_rate_gradually() ::
    {change_rate_gradually, name(), amoc_throttle_config:gradual_plan()}.
-type operation() :: {pause | resume | stop, name()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec pg_scope() -> atom().
pg_scope() ->
    ?PG_SCOPE.

-spec get_throttle_process(amoc_throttle:name()) ->
    {error, no_throttle_process_registered} | {ok, pid()}.
get_throttle_process(Name) ->
    case pg:get_members(?PG_SCOPE, Name) of
        [_ | _] = List -> %% nonempty list
            N = rand:uniform(length(List)),
            {ok, lists:nth(N, List)};
        [] ->
            {error, no_throttle_process_registered}
    end.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec ensure_throttle_processes_started(name(), amoc_throttle_config:config()) ->
    {ok, started | already_started} |
    {error, invalid_throttle | wrong_reconfiguration | error_starting_pool}.
ensure_throttle_processes_started(Name, Config) when is_atom(Name) ->
    raise_event_on_slave_node(Name, init),
    gen_server:call(?MASTER_SERVER, {start_throttle, Name, Config}).

-spec change_rate(name(), amoc_throttle_config:config()) -> ok | {error, any()}.
change_rate(Name, Config) ->
    gen_server:call(?MASTER_SERVER, {change_rate, Name, Config}).

-spec change_rate_gradually(name(), amoc_throttle_config:gradual_plan()) -> ok | {error, any()}.
change_rate_gradually(Name, Config) ->
    gen_server:call(?MASTER_SERVER, {change_rate_gradually, Name, Config}).

-spec pause(name()) -> ok | {error, any()}.
pause(Name) ->
    gen_server:call(?MASTER_SERVER, {pause, Name}).

-spec resume(name()) -> ok | {error, any()}.
resume(Name) ->
    gen_server:call(?MASTER_SERVER, {resume, Name}).

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

-spec init([]) -> {ok, state()}.
init([]) ->
    {ok, #{}}.

-spec handle_call(start_throttle(), gen_server:from(), state()) ->
                     {reply, {ok, started | already_started}, state()} |
                     {reply, {error, wrong_reconfiguration | error_starting_pool}, state()};
                 (change_rate(), gen_server:from(), state()) ->
                     {reply, ok, state()} |
                     {reply, {error, any()}, state()};
                 (change_rate_gradually(), gen_server:from(), state()) ->
                     {reply, ok, state()} |
                     {reply, {error, any()}, state()};
                 (operation(), gen_server:from(), state()) ->
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
            do_gradual_change_rate(Name, Info, State, GradualChangeRate);
        #{Name := _} ->
            {reply, {error, cannot_change_rate}, State};
        _ ->
            {reply, {error, {no_throttle_by_name, Name}}, State}
    end;
handle_call({Op, Name}, _From, State)
  when stop =:= Op; pause =:= Op; resume =:= Op ->
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
    NewState = continue_plan(Name, State, Info, Plan),
    {noreply, NewState}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

raise_event(Name, Event) when Event =:= request; Event =:= execute; Event =:= init ->
    amoc_telemetry:execute([throttle, Event], #{count => 1}, #{name => Name}).

report_rate(Name, infinity, _) ->
    amoc_telemetry:execute([throttle, rate], #{rate => infinity}, #{name => Name});
report_rate(Name, Rate, Interval) ->
    Measurements = #{rate => Rate, interval => Interval},
    amoc_telemetry:execute([throttle, rate], Measurements, #{name => Name}).

-spec do_start_throttle(name(), amoc_throttle:rate(), amoc_throttle:interval(), state()) ->
    {reply, {ok, started}, state()} |
    {reply, {error, error_starting_pool}, state()}.
do_start_throttle(Name, Rate, Interval, State) ->
    PoolConfig = amoc_throttle_config:pool_config(Rate, Interval),
    case amoc_throttle_pooler:start_pool(Name, PoolConfig) of
        {ok, PoolSup} when is_pid(PoolSup) ->
            PoolConfig1 = amoc_throttle_config:process_pool_config(PoolSup, PoolConfig),
            process_pool(Name, PoolConfig1),
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
    NewPoolConfig = amoc_throttle_config:pool_config(Rate, Interval),
    report_rate(Name, Rate, Interval),
    PoolConfig1 = update_throttle_processes(Name, OldPoolConfig, NewPoolConfig),
    Info#throttle_info{rate = Rate, interval = Interval, pool_config = PoolConfig1}.

-spec do_gradual_change_rate(
        name(), throttle_info(), state(), amoc_throttle_config:gradual_plan()) ->
    {reply, ok, state()}.
do_gradual_change_rate(
  Name, Info, State,
  #{rates := [FirstRate | Rates], interval := Interval, step_interval := StepInterval}) ->
    Info1 = do_change_rate(Name, FirstRate, Interval, Info),
    {ok, Timer} = timer:send_interval(StepInterval, {change_plan, Name}),
    Plan = #change_rate_plan{rates = Rates, timer = Timer},
    NewInfo = Info1#throttle_info{rate = FirstRate, interval = Interval, change_plan = Plan},
    {reply, ok, State#{Name => NewInfo}}.

-spec continue_plan(name(), state(), throttle_info(), change_rate_plan()) -> state().
continue_plan(Name, State, Info, #change_rate_plan{rates = [Rate]} = Plan) ->
    Interval = Info#throttle_info.interval,
    TRef = Plan#change_rate_plan.timer,
    Info1 = do_change_rate(Name, Rate, Interval, Info),
    {ok, cancel} = timer:cancel(TRef),
    consume_all_timer_ticks({change_plan, Name}),
    State#{Name => Info1#throttle_info{change_plan = undefined}};
continue_plan(Name, State, Info, #change_rate_plan{rates = [Rate | Rates]} = Plan) ->
    Info1 = do_change_rate(Name, Rate, Info#throttle_info.interval, Info),
    NewPlan = Plan#change_rate_plan{rates = Rates},
    State#{Name => Info1#throttle_info{change_plan = NewPlan}}.

consume_all_timer_ticks(Msg) ->
    receive
        Msg -> consume_all_timer_ticks(Msg)
    after 0 -> ok
    end.

do_run_op(stop, Name, #throttle_info{pool_sup = PoolSup}, State) ->
    ok = amoc_throttle_pooler:stop_pool(PoolSup),
    {reply, ok, maps:remove(Name, State)};
do_run_op(pause, Name, #throttle_info{pool_config = PoolConfig, active = true} = Info, State) ->
    Fun = fun(_, #{pid := Pid}) ->
                  amoc_throttle_process:update(Pid, 0, infinity)
          end,
    maps:foreach(Fun, PoolConfig),
    {reply, ok, State#{Name => Info#throttle_info{active = false}}};

do_run_op(resume, Name, #throttle_info{pool_config = PoolConfig, active = false} = Info, State) ->
    Fun = fun(_, #{max_n := MaxN, delay := Delay, pid := Pid}) ->
                  amoc_throttle_process:update(Pid, MaxN, Delay)
          end,
    maps:foreach(Fun, PoolConfig),
    {reply, ok, State#{Name => Info#throttle_info{active = true}}}.

-spec process_pool(amoc_throttle:name(), amoc_throttle_config:pool_config()) ->
    amoc_throttle_config:pool_config().
process_pool(Name, PoolConfig1) ->
    Fun2 = fun({_, #{status := active, pid := Pid}}) ->
                   {true, Pid};
              (_) ->
                   false
           end,
    Pids = lists:filtermap(Fun2, maps:to_list(PoolConfig1)),
    pg:join(?PG_SCOPE, Name, Pids),
    PoolConfig1.

-spec update_throttle_processes(
        amoc_throttle:name(),
        amoc_throttle_config:pool_config(),
        amoc_throttle_config:pool_config()) ->
    amoc_throttle_config:pool_config().
update_throttle_processes(Name, OldPoolConfig, NewPoolConfig) ->
    Fun = fun(N, #{status := Status, delay := Delay, max_n := MaxN} = V, {C, J, L}) ->
                  #{status := OldStatus, pid := Pid} = maps:get(N, C),
                  case {Status, OldStatus} of
                      {active, inactive} ->
                          %% we let the disabling process drain, without possibly setting its delay
                          %% to infinity and blocking all other processes.
                          {C#{N := V#{pid := Pid}}, [Pid | J], L};
                      {inactive, active} ->
                          amoc_throttle_process:update(Pid, MaxN, Delay),
                          {C#{N := V#{pid := Pid}}, J, [Pid | L]};
                      {Same, Same} ->
                          amoc_throttle_process:update(Pid, MaxN, Delay),
                          {C#{N := V#{pid := Pid}}, J, L}
                  end
          end,
    {PoolConfig, Join, Leave} = maps:fold(Fun, {OldPoolConfig, [], []}, NewPoolConfig),
    pg:join(?PG_SCOPE, Name, Join),
    pg:leave(?PG_SCOPE, Name, Leave),
    PoolConfig.
