%% @private
%% @see amoc_throttle
%% @copyright 2024 Erlang Solutions Ltd.
%% @doc This process's only responsibility is to notify runners that
%% they can run exactly when allowed by the throttling mechanism.
-module(amoc_throttle_process).
-behaviour(gen_server).

%% API
-export([run/2, update/3]).

%% gen_server behaviour
-export([start_link/3,
         init/1,
         handle_call/3,
         handle_info/2,
         handle_cast/2,
         handle_continue/2,
         terminate/2,
         format_status/1]).

-define(DEFAULT_MSG_TIMEOUT, 60000). %% one minute

-record(state, {name :: atom(),
                delay_between_executions :: timeout(),  %% ms
                max_n :: infinity | non_neg_integer(),
                n = 0 :: non_neg_integer(),
                can_run_fn = true :: boolean(),
                tref :: timer:tref() | undefined,
                schedule = [] :: [AmocThrottleRunnerProcess :: pid()],
                schedule_reversed = [] :: [AmocThrottleRunnerProcess :: pid()]
               }).

-type state() :: #state{}.
%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------

-spec start_link(atom(), amoc_throttle:rate(), timeout()) -> gen_server:start_ret().
start_link(Name, MaxN, Delay) ->
    gen_server:start_link(?MODULE, {Name, MaxN, Delay}, []).

-spec run(pid(), pid()) -> ok.
run(Pid, RunnerPid) ->
    gen_server:cast(Pid, {schedule, RunnerPid}).

%% @doc See `initial_state/1'.
%%
%% Setting the delay to infinity results in the effective pausing of the process.
-spec update(pid(), amoc_throttle:rate(), timeout()) -> ok.
update(Pid, MaxN, Delay) ->
    gen_server:cast(Pid, {update, MaxN, Delay}).

%%------------------------------------------------------------------------------
%% gen_server behaviour
%%------------------------------------------------------------------------------

%% We're assuming that this throttle process is getting configured sensible values
%% for Delay and MaxN, and it is the responsibility of the controller
%% to give all workers values that aggregate to the desired throttling.
-spec init({amoc_throttle:name(), amoc_throttle:rate(), timeout()}) ->
    {ok, state(), timeout()}.
init({Name, MaxN, Delay}) ->
    InitialState = initial_state(Name, MaxN, Delay),
    StateWithTimer = maybe_start_timer(InitialState),
    {ok, StateWithTimer, timeout(InitialState)}.

-spec handle_info(Req, state()) ->
    {noreply, state(), {continue, maybe_run_fn}}
      when Req :: {'DOWN', reference(), process, pid(), term()}
           | delay_between_executions
           | timeout.
handle_info({'DOWN', _, process, _, _}, State) ->
    {noreply, dec_n(State), {continue, maybe_run_fn}};
handle_info(delay_between_executions, State) ->
    {noreply, State#state{can_run_fn = true}, {continue, maybe_run_fn}};
handle_info(timeout, State) ->
    internal_event(<<"is inactive">>, State),
    {noreply, State, {continue, maybe_run_fn}}.

-spec handle_cast(Req, state()) ->
    {noreply, state(), {continue, maybe_run_fn}}
      when Req :: {update, amoc_throttle:rate(), timeout()}
           | {schedule, pid()}.
handle_cast({schedule, RunnerPid}, #state{schedule_reversed = SchRev, name = Name} = State) ->
    amoc_throttle_controller:telemetry_event(Name, request),
    {noreply, State#state{schedule_reversed = [RunnerPid | SchRev]}, {continue, maybe_run_fn}};
handle_cast({update, MaxN, Delay}, #state{name = Name} = State) ->
    NewState = merge_state(initial_state(Name, MaxN, Delay), State),
    internal_event(<<"state update">>, NewState),
    {noreply, NewState, {continue, maybe_run_fn}}.

-spec handle_call(any(), gen_server:from(), state()) ->
    {reply, {error, not_implemented}, state(), {continue, maybe_run_fn}}.
handle_call(_, _, State) ->
    {reply, {error, not_implemented}, State, {continue, maybe_run_fn}}.

-spec handle_continue(maybe_run_fn, state()) -> {noreply, state(), timeout()}.
handle_continue(maybe_run_fn, State) ->
    NewState = maybe_run_fn(State),
    {noreply, NewState, timeout(NewState)}.

-spec terminate(term(), state()) -> ok.
terminate(_, State) -> %% Flush all pending actions
    maybe_run_fn(State#state{can_run_fn = true, max_n = infinity}),
    ok.

-spec format_status(gen_server:format_status()) -> gen_server:format_status().
format_status(#{state := State} = FormatStatus) ->
    FormatStatus#{state := printable_state(State)}.

%%------------------------------------------------------------------------------
%% internal functions
%%------------------------------------------------------------------------------

%% - If `Delay' is infinity, we mean to pause the process, see how at `maybe_start_timer/1'
%%      a delay of infinity will set `can_run_fn = false'.
%%
%% - If `MaxN' is infinity and `Delay' is a number, we mean no limits to throttling,
%%      see how `maybe_start_timer/1' will not actually start any timer
%%      and `maybe_run_fn/1' with `max_n = infinity' will loop without pause.
%%
%% - If both `MaxN' and `Delay' are numbers, this will be the actual rate/interval.
%%      Note however that if delay is zero, we effectively limit parallelism to `MaxN'.
-spec initial_state(Name :: atom(), MaxN :: amoc_throttle:rate(), Delay :: timeout()) -> state().
initial_state(Name, MaxN, Delay) ->
    #state{name = Name, max_n = MaxN, delay_between_executions = Delay}.

merge_state(#state{delay_between_executions = D, max_n = MaxN}, #state{} = OldState) ->
    maybe_stop_timer(OldState),
    NewState = OldState#state{delay_between_executions = D, max_n = MaxN, tref = undefined},
    maybe_start_timer(NewState).

maybe_start_timer(#state{delay_between_executions = infinity, tref = undefined} = State) ->
    State#state{can_run_fn = false};
maybe_start_timer(#state{delay_between_executions = 0, tref = undefined} = State) ->
    State#state{can_run_fn = true};
maybe_start_timer(#state{delay_between_executions = D, tref = undefined} = State) ->
    {ok, TRef} = timer:send_interval(D, delay_between_executions),
    State#state{can_run_fn = false, tref = TRef}.

maybe_stop_timer(#state{tref = undefined}) ->
    ok;
maybe_stop_timer(#state{tref = TRef}) ->
    {ok, cancel} = timer:cancel(TRef),
    amoc_throttle_controller:consume_all_timer_ticks(delay_between_executions).

timeout(#state{delay_between_executions = infinity}) ->
    infinity;
timeout(#state{delay_between_executions = Delay}) ->
    Delay + ?DEFAULT_MSG_TIMEOUT.

maybe_run_fn(#state{schedule = [], schedule_reversed = []} = State) ->
    State;
maybe_run_fn(#state{schedule = [], schedule_reversed = SchRev} = State) ->
    NewSchedule = lists:reverse(SchRev),
    NewState = State#state{schedule = NewSchedule, schedule_reversed = []},
    maybe_run_fn(NewState);
maybe_run_fn(#state{can_run_fn = true, max_n = infinity} = State) ->
    NewState = run_fn(State),
    maybe_run_fn(NewState);
maybe_run_fn(#state{can_run_fn = true, n = N, max_n = MaxN} = State) when N < MaxN ->
    NewState = run_fn(State),
    NewState#state{can_run_fn = false};
maybe_run_fn(State) ->
    State.

run_fn(#state{schedule = [RunnerPid | T], name = Name, n = N} = State) ->
    erlang:monitor(process, RunnerPid),
    amoc_throttle_runner:run(RunnerPid),
    amoc_throttle_controller:telemetry_event(Name, execute),
    State#state{schedule = T, n = N + 1}.

dec_n(#state{name = Name, n = 0} = State) ->
    PrintableState = printable_state(State),
    Msg = <<"throttle proccess has invalid N">>,
    Metadata = #{name => Name, n => 0, state => PrintableState},
    amoc_telemetry:execute_log(error, [throttle, process], Metadata, Msg),
    State;
dec_n(#state{n = N} = State) ->
    State#state{n = N - 1}.

-spec internal_event(binary(), state()) -> any().
internal_event(Msg, #state{name = Name} = State) ->
    PrintableState = printable_state(State),
    amoc_telemetry:execute_log(
      debug, [throttle, process], #{self => self(), name => Name, state => PrintableState}, Msg).

printable_state(#state{} = State) ->
    Fields = record_info(fields, state),
    [_ | Values] = tuple_to_list(State#state{schedule = [], schedule_reversed = []}),
    StateMap = maps:from_list(lists:zip(Fields, Values)),
    StateMap#{
        schedule := length(State#state.schedule),
        schedule_reversed := length(State#state.schedule_reversed)}.
