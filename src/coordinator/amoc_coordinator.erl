%%==============================================================================
%% @doc This module allows to synchronize the users and act on groups of them.
%% @copyright 2023 Erlang Solutions Ltd.
%%==============================================================================
-module(amoc_coordinator).

%% API
-export([start/3, start/2,
         add/2, add/3,
         stop/1, reset/1,
         notify/2]).
-ifdef(TEST).
-export([normalize_coordination_plan/1]).
-endif.

-define(DEFAULT_TIMEOUT, 30). %% 30 seconds

-define(IS_POS_INT(Integer), (is_integer(Integer) andalso Integer > 0)).
-define(IS_N_OF_USERS(N), (?IS_POS_INT(N) orelse N =:= all)).
-define(IS_TIMEOUT(Timeout), (?IS_POS_INT(Timeout) orelse Timeout =:= infinity)).

-type name() :: atom().

-type coordination_data() :: {pid(), Data :: any()}.

-type maybe_coordination_data() :: coordination_data() | undefined.

-type event() :: coordinator_timeout | reset_coordinator | {coordinate, {pid(), term()}}.
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

-type plan() :: [coordination_item()] | coordination_item().

%% timeout in seconds
-type coordination_timeout_in_sec() :: pos_integer() | infinity.

-export_type([name/0,
              event/0,
              plan/0,
              coordination_event_type/0,
              coordination_event/0,
              coordination_action/0,
              coordination_data/0,
              normalized_coordination_item/0]).

%%%===================================================================
%%% Api
%%%===================================================================

%% @see start/3
-spec start(name(), plan()) -> ok | error.
start(Name, CoordinationPlan) ->
    start(Name, CoordinationPlan, ?DEFAULT_TIMEOUT).

%% @doc Starts a coordinator. Usually called in the init callback of an amoc scenario.
-spec start(name(), plan(), coordination_timeout_in_sec()) -> ok | error.
start(Name, CoordinationPlan, Timeout) when ?IS_TIMEOUT(Timeout) ->
    Plan = normalize_coordination_plan(CoordinationPlan),
    case amoc_coordinator_sup:start_coordinator(Name, Plan, Timeout) of
        {ok, _} ->
            amoc_telemetry:execute([coordinator, start], #{count => 1}, #{name => Name}),
            ok;
        {error, _} -> error
    end.

%% @doc Stops a coordinator.
-spec stop(name()) -> ok.
stop(Name) ->
    amoc_coordinator_sup:stop_coordinator(Name),
    amoc_telemetry:execute([coordinator, stop], #{count => 1}, #{name => Name}).

%% @see add/3
-spec add(name(), any()) -> ok.
add(Name, Data) ->
    add(Name, self(), Data).

%% @doc Adds the current process data. Usually called in the `start/2' callback of an amoc scenario.
-spec add(name(), pid(), any()) -> ok.
add(Name, Pid, Data) ->
    notify(Name, {coordinate, {Pid, Data}}).

%% @doc Resets a coordinator, that is, calls all coordination actions with `reset' as the coordination data.
-spec reset(name()) -> ok.
reset(Name) ->
    notify(Name, reset_coordinator).

-spec notify(name(), coordinator_timeout | reset_coordinator | {coordinate, {pid(), term()}}) -> ok.
notify(Name, coordinator_timeout) when is_atom(Name) ->
    do_notify(Name, coordinator_timeout);
notify(Name, reset_coordinator) when is_atom(Name) ->
    do_notify(Name, reset_coordinator);
notify(Name, {coordinate, _} = Event) when is_atom(Name) ->
    do_notify(Name, Event).

do_notify(Name, Event) ->
    raise_telemetry_event(Name, Event),
    case amoc_coordinator_sup:get_workers(Name) of
        {ok, TimeoutWorker, Workers} ->
            amoc_coordinator_timeout:notify(TimeoutWorker, Event),
            [ notify_worker(Worker, Event) || Worker <- Workers ],
            ok;
        {error, not_found} ->
            ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
notify_worker(WorkerPid, coordinator_timeout) -> %% synchronous notification
    amoc_coordinator_worker:timeout(WorkerPid);
notify_worker(WorkerPid, reset_coordinator) -> %% synchronous notification
    amoc_coordinator_worker:reset(WorkerPid);
notify_worker(WorkerPid, {coordinate, {Pid, Data}}) when is_pid(Pid) -> %% async notification
    amoc_coordinator_worker:add(WorkerPid, {Pid, Data}).

raise_telemetry_event(Name, Event) ->
    TelemetryEvent = case Event of
                         {coordinate, _} -> add;
                         reset_coordinator -> reset;
                         coordinator_timeout -> timeout
                     end,
    amoc_telemetry:execute([coordinator, TelemetryEvent], #{count => 1}, #{name => Name}).

-spec normalize_coordination_plan(plan()) -> [normalized_coordination_item()].
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
