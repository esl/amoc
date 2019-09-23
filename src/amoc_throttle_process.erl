%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_throttle_process).
-behaviour(gen_server).

%% API
-export([start/3,
         stop/1,
         run/2,
         update/3,
         pause/1,
         resume/1,
         get_state/1]).

%% gen_server behaviour
-export([init/1,
         handle_call/3,
         handle_info/2,
         handle_cast/2,
         handle_continue/2,
         format_status/2]).

-define(DEFAULT_MSG_TIMEOUT, 60000).%% one minute

-record(state, {can_run_fn = true :: boolean(),
                pause = false :: boolean(),
                max_n :: non_neg_integer(),
                name :: atom(),
                n :: integer(),
                interval = 0 :: non_neg_integer(),  %%ms
                delay_between_executions = 0 :: non_neg_integer(),  %%ms
                tref :: timer:tref() | undefined,
                schedule = [] :: [pid()],
                schedule_reversed = [] :: [pid()]}).

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------

-spec start(atom(), non_neg_integer(), pos_integer()) -> {ok, pid()}.
start(Name, Interval, Rate) ->
    gen_server:start(?MODULE, [Name, Interval, Rate], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop_process).

-spec run(pid(), fun(()-> any())) -> ok.
run(Pid, Fun) ->
    RunnerPid = spawn(fun() -> async_runner(Fun) end),
    gen_server:cast(Pid, {schedule, RunnerPid}).

-spec update(pid(), non_neg_integer(), pos_integer()) -> ok.
update(Pid, Interval, Rate) ->
    gen_server:cast(Pid, {update, Interval, Rate}).

-spec pause(pid()) -> ok.
pause(Pid) ->
    gen_server:cast(Pid, pause_process).

-spec resume(pid()) -> ok.
resume(Pid) ->
    gen_server:cast(Pid, resume_process).

-spec get_state(pid()) -> #state{}.
get_state(Pid) ->
    gen_server:call(Pid, get_state).

%%------------------------------------------------------------------------------
%% gen_server behaviour
%%------------------------------------------------------------------------------
-spec init(list()) -> {ok, #state{}, timeout()}.
init([Name, Interval, Rate]) ->
    InitialState = initial_state(Interval, Rate),
    StateWithTimer = maybe_start_timer(InitialState),
    {ok, StateWithTimer#state{name = Name}, timeout(InitialState)}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}, {continue, maybe_run_fn}}.
handle_info({'DOWN', _, process, _, _}, State) ->
    {noreply, inc_n(State), {continue, maybe_run_fn}};
handle_info(delay_between_executions, State) ->
    {noreply, State#state{can_run_fn = true}, {continue, maybe_run_fn}};
handle_info(timeout, State) ->
    log_state("is inactive", State),
    {noreply, State, {continue, maybe_run_fn}}.

-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}, {continue, maybe_run_fn}} | {stop, normal, #state{}}.
handle_cast(stop_process, State) ->
    {stop, normal, State};
handle_cast(pause_process, State) ->
    {noreply, State#state{pause = true}, {continue, maybe_run_fn}};
handle_cast(resume_process, State) ->
    {noreply, State#state{pause = false}, {continue, maybe_run_fn}};
handle_cast({schedule, RunnerPid}, #state{schedule_reversed = SchRev, name = Name} = State) ->
    amoc_throttle_controller:update_metric(Name, request),
    {noreply, State#state{schedule_reversed = [RunnerPid | SchRev]}, {continue, maybe_run_fn}};
handle_cast({update, Interval, Rate}, State) ->
    NewState = merge_state(initial_state(Interval, Rate), State),
    log_state("state update", NewState),
    {noreply, NewState, {continue, maybe_run_fn}}.

-spec handle_call(term(), term(), #state{}) ->
    {reply, {error, not_implemented} | #state{}, #state{}, {continue, maybe_run_fn}}.
handle_call(get_state, _, State) ->
    {reply, printable_state(State), State, {continue, maybe_run_fn}};
handle_call(_, _, State) ->
    {reply, {error, not_implemented}, State, {continue, maybe_run_fn}}.

-spec handle_continue(maybe_run_fn, #state{}) -> {noreply, #state{}, timeout()}.
handle_continue(maybe_run_fn, State) ->
    NewState = maybe_run_fn(State),
    {noreply, NewState, timeout(NewState)}.

format_status(_Opt, [_PDict, State]) ->
    ScheduleLen=length(State#state.schedule),
    ScheduleRevLen=length(State#state.schedule_reversed),
    State1=setelement(#state.schedule,State,ScheduleLen),
    setelement(#state.schedule_reversed,State1,ScheduleRevLen).
%%------------------------------------------------------------------------------
%% internal functions
%%------------------------------------------------------------------------------
initial_state(Interval, 0) ->
    lager:error("invalid rate, must be higher than zero"),
    initial_state(Interval, 1);
initial_state(Interval, Rate) when Rate > 0 ->
    if
        Rate < 5 -> lager:error("too low rate, please reduce NoOfProcesses");
        true -> ok
    end,
    Delay = case {Interval, Interval div Rate, Interval rem Rate} of
                {0, _, _} -> 0; %% limit only No of simultaneous executions
                {_, I, _} when I < 10 ->
                    lager:error("too high rate, please increase NoOfProcesses"),
                    10;
                {_, DelayBetweenExecutions, 0} -> DelayBetweenExecutions;
                {_, DelayBetweenExecutions, _} -> DelayBetweenExecutions + 1
            end,
    #state{interval = Interval, n = Rate, max_n = Rate, delay_between_executions = Delay}.

merge_state(#state{interval = I, delay_between_executions = D, n = N, max_n = MaxN},
            #state{n = OldN, max_n = OldMaxN} = OldState) ->
    maybe_stop_timer(OldState),
    NewN = N - (OldMaxN - OldN),
    NewState = OldState#state{interval = I, delay_between_executions = D, n = NewN, max_n = MaxN, tref = undefined},
    maybe_start_timer(NewState).

maybe_start_timer(#state{delay_between_executions = 0, tref = undefined} = State) ->
    State#state{can_run_fn = true};
maybe_start_timer(#state{delay_between_executions = D, tref = undefined} = State) ->
    {ok, TRef} = timer:send_interval(D, delay_between_executions),
    State#state{can_run_fn = false, tref = TRef}.

maybe_stop_timer(#state{tref = undefined}) ->
    ok;
maybe_stop_timer(#state{tref = TRef}) ->
    {ok, cancel} = timer:cancel(TRef).

maybe_run_fn(#state{schedule = [], schedule_reversed = []} = State) ->
    State;
maybe_run_fn(#state{schedule = [], schedule_reversed = SchRev} = State) ->
    NewSchedule = lists:reverse(SchRev),
    NewState = State#state{schedule = NewSchedule, schedule_reversed = []},
    maybe_run_fn(NewState);
maybe_run_fn(#state{interval = 0, pause = false, n = N} = State) when N > 0 ->
    NewState = run_fn(State),
    maybe_run_fn(NewState);
maybe_run_fn(#state{can_run_fn = true, pause = false, n = N} = State) when N > 0 ->
    NewState = run_fn(State),
    NewState#state{can_run_fn = false};
maybe_run_fn(State) ->
    State.

run_fn(#state{schedule = [RunnerPid | T], name = Name, n = N} = State) ->
    erlang:monitor(process, RunnerPid),
    RunnerPid ! scheduled,
    amoc_throttle_controller:update_metric(Name, execution),
    State#state{schedule = T, n = N - 1}.

async_runner(Fun) ->
    receive
        scheduled -> Fun()
    end.

timeout(State) ->
    State#state.interval + ?DEFAULT_MSG_TIMEOUT.

inc_n(#state{n = N, max_n = MaxN} = State) ->
    NewN = N + 1,
    if
        MaxN < NewN ->
            PrintableState = printable_state(State),
            lager:error("~nthrottle process ~p: invalid N (~p)~n", [self(), PrintableState]),
            State#state{n = MaxN};
        true ->
            State#state{n = NewN}
    end.

log_state(Msg, State) ->
    PrintableState = printable_state(State),
    lager:debug("~nthrottle process ~p: ~s (~p)~n", [self(), Msg, PrintableState]).

printable_state(#state{} = State) ->
    Fields = record_info(fields, state),
    [_ | Values] = tuple_to_list(State#state{schedule = [], schedule_reversed = []}),
    StateMap = maps:from_list(lists:zip(Fields, Values)),
    StateMap#{
        schedule:=length(State#state.schedule),
        schedule_reversed:=length(State#state.schedule_reversed)}.
