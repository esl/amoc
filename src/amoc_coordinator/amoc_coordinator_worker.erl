%%==============================================================================
%% Copyright 2023 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_coordinator_worker).

-behaviour(gen_server).

%% API
-export([start_link/1, add/2,
         stop/1, reset/1,
         timeout/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2]).

-type event_type() :: amoc_coordinator:coordination_event_type().
-type event() :: amoc_coordinator:coordination_event().
-type action() :: amoc_coordinator:coordination_action().
-type data() :: amoc_coordinator:coordination_data().

-record(state, {required_n = all :: pos_integer() | all,
                n = 0 :: non_neg_integer(),
                actions = [] :: [action()],
                collect_data = true :: boolean(),
                accumulator = [] :: [data()]}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(amoc_coordinator:normalized_coordination_item()) -> {ok, Pid :: pid()}.
start_link(CoordinationItem) -> gen_server:start_link(?MODULE, CoordinationItem, []).

-spec reset(pid()) -> ok.
reset(Pid) -> gen_server:call(Pid, {reset, reset}).

-spec timeout(pid()) -> ok.
timeout(Pid) -> gen_server:call(Pid, {reset, timeout}).

-spec stop(pid()) -> ok.
stop(Pid) -> gen_server:call(Pid, {reset, stop}).

-spec add(pid(), data()) -> ok.
add(Pid, Data) -> gen_server:cast(Pid, {add, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(amoc_coordinator:normalized_coordination_item()) -> {ok, state()}.
init({NoOfUsers, Actions}) ->
    State = #state{required_n = NoOfUsers, actions = Actions},
    {ok, State#state{collect_data = is_acc_required(Actions)}}.


-spec handle_call({reset, reset | timeout | stop}, term(), state()) ->
    {reply, ok, state()} | {stop, normal, ok, state()}.
handle_call({reset, Event}, _, State) ->
    NewState = reset_state(Event, State),
    case Event of
        stop -> {stop, normal, ok, NewState};
        _ ->
            {reply, ok, NewState}
    end.

-spec handle_cast({add, data()}, state()) -> {noreply, state()}.
handle_cast({add, Data}, State) ->
    NewState = add_data(Data, State),
    {noreply, NewState}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec is_acc_required([action()]) -> boolean().
is_acc_required(Actions) ->
    lists:any(fun(F) when is_function(F, 1) -> false;
                 (_) -> true
              end, Actions).

-spec add_data(data(), state()) -> state().
add_data(Data, #state{n = N, accumulator = Acc} = State) ->
    NewState = case State#state.collect_data of
                   false ->
                       State#state{n = N + 1};
                   true ->
                       State#state{n = N + 1, accumulator = [Data | Acc]}
               end,
    maybe_reset_state(NewState).

-spec maybe_reset_state(state()) -> state().
maybe_reset_state(#state{n = N, required_n = N} = State) ->
    reset_state(coordinate, State);
maybe_reset_state(State) ->
    State.

-spec reset_state(event_type(), state()) -> state().
reset_state(Event, #state{actions = Actions, accumulator = Acc, n = N} = State) ->
    [execute_action(Action, {Event, N}, Acc) || Action <- Actions],
    State#state{accumulator = [], n = 0}.

-spec execute_action(action(), event(), [data()]) -> any().
execute_action(Action, Event, _) when is_function(Action, 1) ->
    safe_executions(Action, [Event]);
execute_action(Action, Event, Acc) when is_function(Action, 2) ->
    safe_executions(Action, [Event, Acc]);
execute_action(Action, Event, Acc) when is_function(Action, 3) ->
    Fun = fun(A, B) -> safe_executions(Action, [Event, A, B]) end,
    distinct_pairs(Fun, Acc).

-spec safe_executions(function(), [any()]) -> any().
safe_executions(Fun, Args) ->
    try
        erlang:apply(Fun, Args)
    catch
        _:_ -> ok
    end.

-spec distinct_pairs(fun((data(), data()) -> any()), [data()]) -> any().
distinct_pairs(Fun, []) ->
    Fun(undefined, undefined);
distinct_pairs(Fun, [OneElement]) ->
    Fun(OneElement, undefined);
distinct_pairs(Fun, [Element1, Element2]) ->
    Fun(Element1, Element2);
distinct_pairs(Fun, [Element1 | Tail]) -> %% length(Tail) >= 2
    [Fun(Element1, Element2) || Element2 <- Tail],
    distinct_pairs(Fun, Tail).
