%% @private
%% @see amoc_throttle
%% @copyright 2024 Erlang Solutions Ltd.
%% @doc This process's only responsibility is to notify runners that
%% they can run exactly when allowed by the throttling mechanism.
-module(amoc_throttle_process).
-behaviour(gen_server).

%% API
-export([stop/1,
         run/2,
         update/3,
         pause/1,
         resume/1,
         unlock/1,
         get_state/1,
         get_throttle_process/1,
         get_throttle_processes/1
        ]).

%% gen_server behaviour
-export([start_link/3,
         init/1,
         handle_call/3,
         handle_info/2,
         handle_cast/2,
         handle_continue/2,
         format_status/1]).

-define(PG_SCOPE, amoc_throttle).
-define(DEFAULT_MSG_TIMEOUT, 60000).%% one minute

-record(state, {can_run_fn = true :: boolean(),
                status = running :: running | paused | unlocked,
                max_n :: infinity | non_neg_integer(),
                name :: atom(),
                n :: infinity | non_neg_integer(),
                interval = 0 :: amoc_throttle:interval(),  %%ms
                delay_between_executions = 0 :: timeout(),  %%ms
                tref :: timer:tref() | undefined,
                schedule = [] :: [AmocThrottleRunnerProcess :: pid()],
                schedule_reversed = [] :: [AmocThrottleRunnerProcess :: pid()]}).

-type state() :: #state{}.
%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------

-spec start_link(atom(), amoc_throttle:interval(), amoc_throttle:rate()) -> {ok, pid()}.
start_link(Name, Interval, Rate) ->
    gen_server:start_link(?MODULE, {Name, Interval, Rate}, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop_process).

-spec run(pid(), pid()) -> ok.
run(Pid, RunnerPid) ->
    gen_server:cast(Pid, {schedule, RunnerPid}).

-spec update(pid(), amoc_throttle:interval(), amoc_throttle:rate()) -> ok.
update(Pid, Interval, Rate) ->
    gen_server:cast(Pid, {update, Interval, Rate}).

-spec pause(pid()) -> ok.
pause(Pid) ->
    gen_server:cast(Pid, pause_process).

-spec resume(pid()) -> ok.
resume(Pid) ->
    gen_server:cast(Pid, resume_process).

-spec unlock(pid()) -> ok.
unlock(Pid) ->
    gen_server:cast(Pid, unlock_process).

-spec get_state(pid()) -> map().
get_state(Pid) ->
    gen_server:call(Pid, get_state).

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

-spec get_throttle_processes(amoc_throttle:name()) ->
    {error, no_throttle_process_registered} | {ok, [pid()]}.
get_throttle_processes(Name) ->
    case pg:get_members(?PG_SCOPE, Name) of
        [] ->
            {error, no_throttle_process_registered};
        List ->
            {ok, List}
    end.

%%------------------------------------------------------------------------------
%% gen_server behaviour
%%------------------------------------------------------------------------------

-spec init({amoc_throttle:name(), amoc_throttle:interval(), amoc_throttle:rate()}) ->
    {ok, state(), timeout()}.
init({Name, Interval, Rate}) ->
    pg:join(?PG_SCOPE, Name, self()),
    InitialState = initial_state(Name, Interval, Rate),
    StateWithTimer = maybe_start_timer(InitialState),
    {ok, StateWithTimer#state{name = Name}, timeout(InitialState)}.

-spec handle_info(term(), state()) -> {noreply, state(), {continue, maybe_run_fn}}.
handle_info({'DOWN', _, process, _, _}, State) ->
    {noreply, inc_n(State), {continue, maybe_run_fn}};
handle_info(delay_between_executions, State) ->
    {noreply, State#state{can_run_fn = true}, {continue, maybe_run_fn}};
handle_info(timeout, State) ->
    internal_event(<<"is inactive">>, State),
    {noreply, State, {continue, maybe_run_fn}}.

-spec handle_cast(term(), state()) ->
    {noreply, state(), {continue, maybe_run_fn}} | {stop, normal, state()}.
handle_cast(stop_process, State) ->
    {stop, normal, State};
handle_cast(pause_process, State) ->
    {noreply, State#state{status = paused}, {continue, maybe_run_fn}};
handle_cast(resume_process, State) ->
    {noreply, State#state{status = running}, {continue, maybe_run_fn}};
handle_cast(unlock_process, State) ->
    {noreply, State#state{status = unlocked}, {continue, maybe_run_fn}};
handle_cast({schedule, RunnerPid}, #state{schedule_reversed = SchRev, name = Name} = State) ->
    amoc_throttle_controller:telemetry_event(Name, request),
    {noreply, State#state{schedule_reversed = [RunnerPid | SchRev]}, {continue, maybe_run_fn}};
handle_cast({update, Interval, Rate}, #state{name = Name} = State) ->
    NewState = merge_state(initial_state(Name, Interval, Rate), State),
    internal_event(<<"state update">>, NewState),
    {noreply, NewState, {continue, maybe_run_fn}}.

-spec handle_call(term(), term(), state()) ->
    {reply, {error, not_implemented} | state(), state(), {continue, maybe_run_fn}}.
handle_call(get_state, _, State) ->
    {reply, printable_state(State), State, {continue, maybe_run_fn}};
handle_call(_, _, State) ->
    {reply, {error, not_implemented}, State, {continue, maybe_run_fn}}.

-spec handle_continue(maybe_run_fn, state()) -> {noreply, state(), timeout()}.
handle_continue(maybe_run_fn, State) ->
    NewState = maybe_run_fn(State),
    {noreply, NewState, timeout(NewState)}.

-spec format_status(gen_server:format_status()) -> gen_server:format_status().
format_status(#{state := State} = FormatStatus) ->
    FormatStatus#{state := printable_state(State)}.

%%------------------------------------------------------------------------------
%% internal functions
%%------------------------------------------------------------------------------

initial_state(_Name, Interval, infinity) ->
    #state{interval = Interval, n = infinity, max_n = infinity, delay_between_executions = 0};
initial_state(_Name, Interval, 0) ->
    #state{interval = Interval, n = 0, max_n = 0, delay_between_executions = infinity};
initial_state(Name, Interval, Rate) when Rate > 0 ->
    case Rate < 5 of
        true ->
            Msg = <<"too low rate, please reduce NoOfProcesses">>,
            internal_warning(Msg, Name, Rate, Interval);
        false ->
            ok
    end,
    Delay = case {Interval, Interval div Rate, Interval rem Rate} of
                {0, _, _} -> 0; %% limit only No of simultaneous executions
                {_, I, _} when I < 10 ->
                    Message = <<"too high rate, please increase NoOfProcesses">>,
                    internal_warning(Message, Name, Rate, Interval),
                    10;
                {_, DelayBetweenExecutions, 0} -> DelayBetweenExecutions;
                {_, DelayBetweenExecutions, _} -> DelayBetweenExecutions + 1
            end,
    #state{interval = Interval, n = Rate, max_n = Rate, delay_between_executions = Delay}.

merge_state(#state{interval = I, delay_between_executions = D, n = N, max_n = MaxN},
            #state{n = OldN, max_n = OldMaxN} = OldState) ->
    maybe_stop_timer(OldState),
    NewN = N - (OldMaxN - OldN),
    NewState = OldState#state{interval = I, delay_between_executions = D, n = NewN,
                              max_n = MaxN, tref = undefined},
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
    consume_all_timer_ticks(delay_between_executions).

consume_all_timer_ticks(Msg) ->
    receive
        Msg -> consume_all_timer_ticks(Msg)
    after 0 -> ok
    end.

maybe_run_fn(#state{schedule = [], schedule_reversed = []} = State) ->
    State;
maybe_run_fn(#state{schedule = [], schedule_reversed = SchRev} = State) ->
    NewSchedule = lists:reverse(SchRev),
    NewState = State#state{schedule = NewSchedule, schedule_reversed = []},
    maybe_run_fn(NewState);
maybe_run_fn(#state{interval = _, status = unlocked, n = N} = State) when N > 0 ->
    NewState = run_fn(State),
    maybe_run_fn(NewState);
maybe_run_fn(#state{interval = 0, status = running, n = N} = State) when N > 0 ->
    NewState = run_fn(State),
    maybe_run_fn(NewState);
maybe_run_fn(#state{can_run_fn = true, status = running, n = N} = State) when N > 0 ->
    NewState = run_fn(State),
    NewState#state{can_run_fn = false};
maybe_run_fn(State) ->
    State.

run_fn(#state{schedule = [RunnerPid | T], name = Name, n = infinity} = State) ->
    erlang:monitor(process, RunnerPid),
    amoc_throttle_runner:run(RunnerPid),
    amoc_throttle_controller:telemetry_event(Name, execute),
    State#state{schedule = T};
run_fn(#state{schedule = [RunnerPid | T], name = Name, n = N} = State) ->
    erlang:monitor(process, RunnerPid),
    amoc_throttle_runner:run(RunnerPid),
    amoc_throttle_controller:telemetry_event(Name, execute),
    State#state{schedule = T, n = N - 1}.

timeout(State) ->
    State#state.interval + ?DEFAULT_MSG_TIMEOUT.

inc_n(#state{name = Name, n = N, max_n = MaxN} = State) ->
    NewN = N + 1,
    case MaxN < NewN of
        true ->
            PrintableState = printable_state(State),
            Msg = <<"throttle proccess has invalid N">>,
            amoc_telemetry:execute_log(
              error, [throttle, process], #{name => Name, n => NewN, state => PrintableState}, Msg),
            State#state{n = MaxN};
        false ->
            State#state{n = NewN}
    end.

-spec internal_event(binary(), state()) -> any().
internal_event(Msg, #state{name = Name} = State) ->
    PrintableState = printable_state(State),
    amoc_telemetry:execute_log(
      debug, [throttle, process], #{self => self(), name => Name, state => PrintableState}, Msg).

-spec internal_warning(binary(), atom(), amoc_throttle:rate(), amoc_throttle:interval()) -> any().
internal_warning(Msg, Name, Rate, Interval) ->
    amoc_telemetry:execute_log(
      warning, [throttle, process], #{name => Name, rate => Rate, interval => Interval}, Msg).

printable_state(#state{} = State) ->
    Fields = record_info(fields, state),
    [_ | Values] = tuple_to_list(State#state{schedule = [], schedule_reversed = []}),
    StateMap = maps:from_list(lists:zip(Fields, Values)),
    StateMap#{
        schedule := length(State#state.schedule),
        schedule_reversed := length(State#state.schedule_reversed)}.
