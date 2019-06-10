%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_throttle_process).

%% API
-export([start/2,
         stop/1,
         run/2,
         update/3,
         pause/1,
         resume/1]).

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

start(Interval, Rate) ->
    spawn(fun() -> init_throttle_process(Interval, Rate) end).

stop(Pid) ->
    Pid ! stop_process.

run(Pid, Fun) ->
    Pid ! {run, Fun}.

update(Pid, Interval, Rate) ->
    NewState = initial_state(Interval, Rate),
    Pid ! {update, NewState}.

pause(Pid) ->
    Pid ! pause_process.

resume(Pid) ->
    Pid ! resume_process.

%%------------------------------------------------------------------------------
%% internal functions
%%------------------------------------------------------------------------------
init_throttle_process(Interval, Rate) ->
    InitialState = initial_state(Interval, Rate),
    throttle_process(InitialState).

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

throttle_process(State) ->
    NewState = maybe_run_fn(State),
    wait_for_msg(NewState).

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

wait_for_msg(#state{n = N, schedule_reversed = SchRev} = State) ->
    receive
        stop_process -> ok;
        pause_process ->
            throttle_process(State#state{pause = true});
        resume_process ->
            throttle_process(State#state{pause = false});
        {run, Fn} ->
            throttle_process(State#state{schedule_reversed = [Fn | SchRev]});
        increase_n ->
            throttle_process(State#state{n = N + 1});
        delay_between_executions ->
            throttle_process(State#state{can_run_fn = true});
        {update, NewState} ->
            throttle_process(merge_state(NewState, State))
    after State#state.interval + ?DEFAULT_MSG_TIMEOUT ->
        lager:debug("throttle process is inactive (n=~p)", [State#state.n]),
        throttle_process(State)
    end.

