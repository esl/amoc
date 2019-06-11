%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_throttle_process).
-behaviour(gen_server).

%% API
-export([start/2,
         stop/1,
         run/2,
         update/3,
         pause/1,
         resume/1]).

%% gen_server behaviour
-export([init/1,
         handle_call/3,
         handle_info/2,
         handle_cast/2,
         handle_continue/2]).

-define(DEFAULT_MSG_TIMEOUT, 60000).%% one minute


-record(state, {can_run_fn = true :: boolean(),
                pause = false :: boolean(),
                n :: non_neg_integer(),
                interval = 0 :: non_neg_integer(),  %%ms
                delay_between_executions = 0 :: non_neg_integer(),  %%ms
                schedule = [] :: [fun(()-> any())],
                schedule_reversed = [] :: [fun(()-> any())]}).

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------

-spec start(non_neg_integer(), pos_integer()) -> {ok, pid()}.
start(Interval, Rate) ->
    gen_server:start(?MODULE, [Interval, Rate], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop_process).

-spec run(pid(), fun(()-> any())) -> ok.
run(Pid, Fun) ->
    gen_server:cast(Pid, {run, Fun}).

-spec update(pid(), non_neg_integer(), pos_integer()) -> ok.
update(Pid, Interval, Rate) ->
    gen_server:cast(Pid, {update, Interval, Rate}).

-spec pause(pid()) -> ok.
pause(Pid) ->
    gen_server:cast(Pid, pause_process).

-spec resume(pid()) -> ok.
resume(Pid) ->
    gen_server:cast(Pid, resume_process).


%%------------------------------------------------------------------------------
%% gen_server behaviour
%%------------------------------------------------------------------------------
-spec init(list()) -> {ok, #state{}, timeout()}.
init([Interval, Rate]) ->
    InitialState = initial_state(Interval, Rate),
    {ok, InitialState, timeout(InitialState)}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}, {continue, maybe_run_fn}}.
handle_info(increase_n, #state{n = N} = State) ->
    {noreply, State#state{n = N + 1}, {continue, maybe_run_fn}};
handle_info(delay_between_executions, State) ->
    {noreply, State#state{can_run_fn = true}, {continue, maybe_run_fn}};
handle_info(timeout, State) ->
    lager:debug("throttle process is inactive (n=~p)", [State#state.n]),
    {noreply, State, {continue, maybe_run_fn}}.

-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}, {continue, maybe_run_fn}} | {stop, normal, #state{}}.
handle_cast(stop_process, State) ->
    {stop, normal, State};
handle_cast(pause_process, State) ->
    {noreply, State#state{pause = true}, {continue, maybe_run_fn}};
handle_cast(resume_process, State) ->
    {noreply, State#state{pause = false}, {continue, maybe_run_fn}};
handle_cast({run, Fn}, #state{schedule_reversed = SchRev} = State) ->
    {noreply, State#state{schedule_reversed = [Fn | SchRev]}, {continue, maybe_run_fn}};
handle_cast({update, Interval, Rate}, State) ->
    NewState = merge_state(initial_state(Interval, Rate), State),
    {noreply, NewState, {continue, maybe_run_fn}}.

-spec handle_call(term(), term(), #state{}) ->
    {reply, {error, not_implemented}, #state{}, {continue, maybe_run_fn}}.
handle_call(_, _, State) ->
    {reply, {error, not_implemented}, State, {continue, maybe_run_fn}}.

-spec handle_continue(maybe_run_fn, #state{}) -> {noreply, #state{}, timeout()}.
handle_continue(maybe_run_fn, State) ->
    NewState = maybe_run_fn(State),
    {noreply, NewState, timeout(NewState)}.
%%------------------------------------------------------------------------------
%% internal functions
%%------------------------------------------------------------------------------
initial_state(Interval, Rate) ->
    if
        Rate < 5 -> lager:error("too low rate, please reduce NoOfProcesses");
        true -> ok
    end,
    case {Interval, Interval div Rate} of
        {0, 0} -> %% limit only No of simultaneous executions
            #state{interval                 = Interval,
                   delay_between_executions = 0,
                   n                        = Rate};
        {_, I} when I < 10 ->
            lager:error("too high rate, please increase NoOfProcesses"),
            #state{interval                 = Interval,
                   delay_between_executions = 10,
                   n                        = Rate};
        {_, DelayBetweenExecutions} ->
            #state{interval                 = Interval,
                   delay_between_executions = DelayBetweenExecutions,
                   n                        = Rate}
    end.

merge_state(#state{interval = I, delay_between_executions = D, n = N}, OldState) ->
    OldState#state{interval = I, delay_between_executions = D, n = N}.

maybe_run_fn(#state{schedule = [], schedule_reversed = []} = State) ->
    State;
maybe_run_fn(#state{schedule = [], schedule_reversed = SchRev} = State) ->
    NewSchedule = lists:reverse(SchRev),
    NewState = State#state{schedule = NewSchedule, schedule_reversed = []},
    maybe_run_fn(NewState);
maybe_run_fn(#state{can_run_fn = true, pause = false, n = N} = State) when N > 0 ->
    NewState = run_fn(State),
    NewState#state{can_run_fn = false, n = N - 1};
maybe_run_fn(State) ->
    State.

run_fn(#state{schedule = [Fn | T], delay_between_executions = Delay, interval = Interval} = State) ->
    Self = self(),
    spawn(fun() -> run_and_wait(Fn, Interval), Self ! increase_n end),
    erlang:send_after(Delay, self(), delay_between_executions),
    State#state{schedule = T}.

run_and_wait(Fn, Interval) ->
    {TimeUs, _} = timer:tc(Fn),
    Time = erlang:convert_time_unit(TimeUs, microsecond, millisecond),
    case Interval - Time of
        SleepTime when SleepTime > 0 -> timer:sleep(SleepTime);
        _ -> ok
    end.

timeout(State) ->
    State#state.interval + ?DEFAULT_MSG_TIMEOUT.