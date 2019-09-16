-module(amoc_coordinator_worker).

-behaviour(gen_server).

%% API
-export([start/1, add/2,
         stop/1, reset/1,
         timeout/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-type event() :: amoc_coordinator:coordination_event().
-type action() :: amoc_coordinator:coordination_action().
-type data() :: amoc_coordinator:coordination_data().

-record(state, {required_n = 1 :: pos_integer(),
                n = 0 :: non_neg_integer(),
                timeout = infinity :: amoc_coordinator:coordination_timeout(),
                actions = [] :: [action()],
                collect_data = true :: boolean(),
                accumulator = [] :: [data()]}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start(amoc_coordinator:coordination_item_with_timeout()) -> {ok, Pid :: pid()}).
start(CoordinationItem) -> gen_server:start(?MODULE, CoordinationItem, []).

-spec(reset(pid()) -> ok).
reset(Pid) -> gen_server:cast(Pid, reset).

-spec(timeout(pid()) -> ok).
timeout(Pid) -> erlang:send(Pid, timeout).

-spec(stop(pid()) -> ok).
stop(Pid) -> gen_server:cast(Pid, stop).

-spec(add(pid(), data()) -> ok).
add(Pid, Data) -> gen_server:cast(Pid, {add, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(amoc_coordinator:coordination_item_with_timeout()) -> {ok, #state{}}).
init({NoOfUsers, Actions, Timeout}) ->
    State = #state{required_n = NoOfUsers, actions = Actions, timeout = Timeout},
    {ok, State#state{collect_data = is_acc_required(Actions)}}.


-spec(handle_call(term(), term(), #state{}) -> {reply, {error, not_implemented}, #state{}, timeout()}).
handle_call(_Request, _From, #state{timeout = Timeout} = State) ->
    {reply, {error, not_implemented}, State, Timeout}.

-spec(handle_cast(reset | stop | {add, any()}, #state{}) ->
    {stop, normal, #state{}} |
    {noreply, #state{}, timeout()} |
    {noreply, #state{}}).
handle_cast(reset, State) ->
    NewState = reset_state(reset, State),
    {noreply, NewState};
handle_cast({add, Data}, #state{timeout = Timeout} = State) ->
    NewState = add_data(Data, State),
    {noreply, NewState, Timeout};
handle_cast(stop, State) ->
    NewState = reset_state(stop, State),
    {stop, normal, NewState}.

-spec(handle_info(timeout | any(), #state{}) ->
    {noreply, NewState :: #state{}, timeout()} |
    {noreply, NewState :: #state{}}).
handle_info(timeout, State) ->
    NewState = reset_state(timeout, State),
    {noreply, NewState};
handle_info(_, #state{timeout = Timeout} = State) ->
    {noreply, State, Timeout}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(is_acc_required([action()]) -> boolean()).
is_acc_required(Actions) ->
    lists:any(fun(F) when is_function(F, 1) -> false;
                 (_) -> true
              end, Actions).

-spec(add_data(data(), #state{}) -> #state{}).
add_data(Data, #state{n = N, accumulator = Acc} = State) ->
    NewState = case State#state.collect_data of
                   false ->
                       State#state{n = N + 1};
                   true ->
                       State#state{n = N + 1, accumulator = [Data | Acc]}
               end,
    maybe_reset_state(NewState).

-spec(maybe_reset_state(#state{}) -> #state{}).
maybe_reset_state(#state{n = N, required_n = N} = State) ->
    reset_state(coordinate, State);
maybe_reset_state(State) ->
    State.

-spec(reset_state(event(), #state{}) -> #state{}).
reset_state(Event, #state{actions = Actions, accumulator = Acc} = State) ->
    [execute_action(Action, Event, Acc) || Action <- Actions],
    State#state{accumulator = [], n = 0}.

-spec(execute_action(action(), event(), [data()]) -> any()).
execute_action(Action, Event, _) when is_function(Action, 1) ->
    safe_executions(Action, [Event]);
execute_action(Action, Event, Acc) when is_function(Action, 2) ->
    safe_executions(Action, [Event, Acc]);
execute_action(Action, Event, Acc) when is_function(Action, 3) ->
    Fun = fun(A, B) -> safe_executions(Action, [Event, A, B]) end,
    distinct_pairs(Fun, Acc).

-spec(safe_executions(function(), [any()]) -> any()).
safe_executions(Fun, Args) ->
    try
        erlang:apply(Fun, Args)
    catch
        _:_ -> ok
    end.

-spec(distinct_pairs(fun((data(), data()) -> any()), [data()]) -> any()).
distinct_pairs(Fun, []) ->
    Fun(undefined, undefined);
distinct_pairs(Fun, [OneElement]) ->
    Fun(OneElement, undefined);
distinct_pairs(Fun, [Element1, Element2]) ->
    Fun(Element1, Element2);
distinct_pairs(Fun, [Element1 | Tail]) -> %% length(Tail) >= 2
    [Fun(Element1, Element2) || Element2 <- Tail],
    distinct_pairs(Fun, Tail).

