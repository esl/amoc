%%==============================================================================
%% @doc This module allows to synchronize the users and act on groups of them.
%% @copyright 2023 Erlang Solutions Ltd.
%%==============================================================================
-module(amoc_coordinator).

-behaviour(gen_event).

%% API
-export([start/3, start/2,
         add/2, add/3,
         stop/1, reset/1]).

%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         terminate/2]).

-define(DEFAULT_TIMEOUT, 30). %% 30 seconds

-define(IS_POS_INT(Integer), (is_integer(Integer) andalso Integer > 0)).
-define(IS_N_OF_USERS(N), (?IS_POS_INT(N) orelse N =:= all)).
-define(IS_TIMEOUT(Timeout), (?IS_POS_INT(Timeout) orelse Timeout =:= infinity)).

-type name() :: atom().
-type state() :: {worker, pid()} | {timeout, name(), pid()}.

-type coordination_data() :: {pid(), Data :: any()}.

-type maybe_coordination_data() :: coordination_data() | undefined.

-type coordination_event_type() :: coordinate | timeout | stop | reset.

-type coordination_event() :: {coordination_event_type(), non_neg_integer()}.

-type coordination_action() ::
    fun((coordination_event(), [coordination_data()]) -> any()) |
    fun((coordination_event(), maybe_coordination_data(), maybe_coordination_data()) -> any()) |
    fun((coordination_event()) -> any()).

-type coordination_actions() :: [coordination_action()] | coordination_action().

-type coordination_item() :: {NoOfUsers :: pos_integer() | all,
                              coordination_actions()}.

-type normalized_coordination_item() :: {NoOfUsers :: pos_integer() | all,
                                         [coordination_action()]}.

-type coordination_plan() :: [coordination_item()] | coordination_item().

%% timeout in seconds
-type coordination_timeout_in_sec() :: pos_integer() | infinity.

-export_type([coordination_event_type/0,
              coordination_event/0,
              coordination_action/0,
              coordination_data/0,
              coordination_plan/0,
              normalized_coordination_item/0]).

%%%===================================================================
%%% Api
%%%===================================================================

%% @see start/3
-spec start(name(), coordination_plan()) -> ok | error.
start(Name, CoordinationPlan) ->
    start(Name, CoordinationPlan, ?DEFAULT_TIMEOUT).

%% @doc Starts a coordinator. Usually called in the init callback of an amoc scenario.
-spec start(name(), coordination_plan(), coordination_timeout_in_sec()) -> ok | error.
start(Name, CoordinationPlan, Timeout) when ?IS_TIMEOUT(Timeout) ->
    Plan = normalize_coordination_plan(CoordinationPlan),
    case gen_event:start({local, Name}) of
        {ok, _} ->
            telemetry:execute([amoc, coordinator, start], #{count => 1}, #{name => Name}),
            %% according to gen_event documentation:
            %%
            %%    When the event is received, the event manager calls
            %%    handle_event(Event, State) for each installed event
            %%    handler, in the same order as they were added.
            %%
            %% in reality the order is reversed, the last added handler
            %% is executed at first. so to ensure that all the items in
            %% the plan with NoOfUsers =:= all are executed in the very
            %% end, we need to add them first. Also, the timeout handler
            %% must be added last, so gen_event triggers it first.
            AllItemsHandlers = lists:reverse([Item || {all, _} = Item <- Plan]),
            [gen_event:add_handler(Name, ?MODULE, Item) || Item <- AllItemsHandlers],
            [gen_event:add_handler(Name, ?MODULE, Item) || {N, _} = Item <- Plan,
                                                           is_integer(N)],
            gen_event:add_handler(Name, ?MODULE, {timeout, Name, Timeout}),
            ok;
        {error, _} -> error
    end.

%% @doc Stops a coordinator.
-spec stop(name()) -> ok.
stop(Name) ->
    gen_event:stop(Name),
    telemetry:execute([amoc, coordinator, stop], #{count => 1}, #{name => Name}).

%% @see add/3
-spec add(name(), any()) -> ok.
add(Name, Data) ->
    add(Name, self(), Data).

%% @doc Adds the current process data. Usually called in the `start/2' callback of an amoc scenario.
-spec add(name(), pid(), any()) -> ok.
add(Name, Pid, Data) ->
    gen_event:notify(Name, {coordinate, {Pid, Data}}).

%% @doc Resets a coordinator, that is, calls all coordination actions with `reset' as the coordination data.
-spec reset(name()) -> ok.
reset(Name) ->
    gen_event:notify(Name, reset_coordinator).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec init({timeout, name(), coordination_timeout_in_sec()} | normalized_coordination_item()) ->
    {ok, state()}.
init({timeout, Name, Timeout}) ->
    Pid = spawn(fun() ->
                    case Timeout of
                        infinity ->
                            timeout_fn(Name, infinity, infinity);
                        Int when is_integer(Int), Int > 0 ->
                            timeout_fn(Name, timer:seconds(Timeout), infinity)
                    end
                end),
    {ok, {timeout, Name, Pid}};
init(CoordinationItem) ->
    {ok, Pid} = amoc_coordinator_worker:start_link(CoordinationItem),
    {ok, {worker, Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_event(Event :: term(), state()) -> {ok, state()}.
handle_event(Event, {timeout, Name, Pid}) ->
    %% there's only one "timeout" event handler for coordinator,
    %% so calling telemetry:execute/3 here to ensure that it's
    %% triggered just once per event.
    TelemetryEvent = case Event of
                         {coordinate, _} -> add;
                         reset_coordinator -> reset;
                         coordinator_timeout -> timeout
                     end,
    telemetry:execute([amoc, coordinator, TelemetryEvent],
                       #{count => 1}, #{name => Name}),
    erlang:send(Pid, Event),
    {ok, {timeout, Name, Pid}};
handle_event(Event, {worker, WorkerPid}) ->
    case Event of
        coordinator_timeout -> %% synchronous notification
            amoc_coordinator_worker:timeout(WorkerPid);
        reset_coordinator -> %% synchronous notification
            amoc_coordinator_worker:reset(WorkerPid);
        {coordinate, {Pid, Data}} when is_pid(Pid) -> %% async notification
            amoc_coordinator_worker:add(WorkerPid, {Pid, Data})
    end,
    {ok, {worker, WorkerPid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), state()) -> {ok, {error, not_implemented}, state()}.
handle_call(_Request, State) ->
    {ok, {error, not_implemented}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(any(), state()) -> ok.
terminate(_, {timeout, _Name, Pid}) ->
    erlang:send(Pid, terminate), ok;
terminate(_, {worker, Pid}) ->
    %% synchronous notification
    amoc_coordinator_worker:stop(Pid), ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec normalize_coordination_plan(coordination_plan()) -> [normalized_coordination_item()].
normalize_coordination_plan(CoordinationPlan) when is_tuple(CoordinationPlan) ->
    normalize_coordination_plan([CoordinationPlan]);
normalize_coordination_plan(CoordinationPlan) ->
    [normalize_coordination_item(I) || I <- CoordinationPlan].

normalize_coordination_item({NoOfUsers, Action}) when is_function(Action) ->
    normalize_coordination_item({NoOfUsers, [Action]});
normalize_coordination_item({NoOfUsers, Actions}) when ?IS_N_OF_USERS(NoOfUsers),
                                                       is_list(Actions) ->
    [assert_action(NoOfUsers, A) || A <- Actions],
    {NoOfUsers, Actions}.

assert_action(all, Action) when is_function(Action, 1);
                                is_function(Action, 2) ->
    ok;
assert_action(N, Action) when is_integer(N),
                              (is_function(Action, 1) orelse
                               is_function(Action, 2) orelse
                               is_function(Action, 3)) ->
    ok.

timeout_fn(Name, CoordinationTimeout, Timeout) ->
    receive
        terminate -> ok;
        {coordinate, _} ->
            timeout_fn(Name, CoordinationTimeout, CoordinationTimeout);
        _ -> %% coordinator_timeout or reset_coordinator
            timeout_fn(Name, CoordinationTimeout, infinity)
    after Timeout ->
        gen_event:notify(Name, coordinator_timeout),
        timeout_fn(Name, CoordinationTimeout, infinity)
    end.
