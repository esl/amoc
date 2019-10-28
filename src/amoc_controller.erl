%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_controller).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(INTERARRIVAL_DEFAULT, 50).
-define(CHECKING_INTERVAL_DEFAULT, 60*1000).
-define(ADD_BATCH_INTERVAL_DEFAULT, 5*60*1000).
-define(TABLE, amoc_users).

-record(state, {scenario :: amoc:scenario() | undefined,
                scenario_state :: any(),
                nodes ::  non_neg_integer() | undefined,
                node_id :: node_id() | undefined}).


-type state() :: #state{}.
-type node_id() :: non_neg_integer().
-type handle_call_res() :: ok | {ok, term()} | {error, term()}.
-type scenario_status() :: error | running | finished | loaded.
-type user_count() :: non_neg_integer().
-type interarrival() :: non_neg_integer().
-type user_batch_strategy() :: [{node(), user_count(), interarrival()}].

-include_lib("kernel/include/logger.hrl").

%% ------------------------------------------------------------------
%% Types Exports
%% ------------------------------------------------------------------

-export_type([scenario_status/0,
              user_count/0,
              interarrival/0,
              user_batch_strategy/0]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0,
         do/3,
         do/7,
         add/1,
         add/2,
         add/3,
         remove/1,
         remove/2,
         remove/3,
         users/0,
         test_status/1,
         start_scenario_checking/1,
         add_batches/2]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec do(amoc:scenario(), amoc_scenario:user_id(), amoc_scenario:user_id()) ->
    ok | {error, term()}.
do(Scenario, Start, End) ->
    gen_server:call(?SERVER, {do, Scenario, Start, End}).

-spec do(node(), amoc:scenario(), amoc_scenario:user_id(),
         amoc_scenario:user_id(), non_neg_integer(), node_id(),
         amoc:do_opts()) -> ok | {error, term()}.
do(Node, Scenario, Start, End, NodesCount, NodeId, Opts) ->
    Req = {do, Scenario, Start, End, NodesCount, NodeId, Opts},
    gen_server:call({?SERVER, Node}, Req).

-spec add(user_count()) -> ok.
add(Count) ->
    gen_server:cast(?SERVER, {add, Count}).

-spec add(node(), user_count()) -> ok.
add(Node, Count) ->
    gen_server:cast({?SERVER, Node}, {add, Count}).

-spec add(node(), user_count(), proplists:proplist()) -> ok.
add(Node, Count, Opts) ->
    gen_server:cast({?SERVER, Node}, {add, Count, Opts}).

-spec remove(user_count()) -> ok.
remove(Count) ->
    remove(Count, []).

-spec remove(user_count(), amoc:remove_opts()) ->ok.
remove(Count, Opts) ->
    gen_server:cast(?SERVER, {remove, Count, Opts}).

-spec remove(node(), user_count(), amoc:remove_opts()) ->ok.
remove(Node, Count, Opts) ->
    gen_server:cast({?SERVER, Node}, {remove, Count, Opts}).

-spec users() -> [proplists:property()].
users() ->
    {ok, U} = gen_server:call(?SERVER, users),
    U.

-spec test_status(atom()) -> {ok, scenario_status()} | {error, term()}.
test_status(ScenarioName) ->
    gen_server:call(?SERVER, {status, ScenarioName}).

-spec start_scenario_checking(amoc:scenario()) -> ok | skip | {error, term()}.
start_scenario_checking(Scenario) ->
    gen_server:call(?SERVER, {start_scenario_checking, Scenario}).

-spec add_batches(non_neg_integer(), amoc:scenario()) -> ok | skip |
                                                         {error, term()}.
add_batches(BatchCount, Scenario) ->
    gen_server:call(?SERVER, {add_batches, BatchCount, Scenario}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(priority, max),
    State = #state{scenario = undefined,
                   nodes = undefined},
    {ok, State}.

-spec handle_call(any(), any(), state()) -> {reply, handle_call_res(), state()}.
handle_call({do, Scenario, Start, End}, _From, State) ->
    handle_local_do(Scenario, Start, End, State);
handle_call({do, Scenario, Start, End, Nodes, NodeId, Opts}, _From, State) ->
    handle_dist_do(Scenario, Start, End, Nodes, NodeId, Opts, State);
handle_call(users, _From, State) ->
    Reply = [{count, ets:info(?TABLE, size)},
             {last, ets:last(?TABLE)}],
    {reply, {ok, Reply}, State};
handle_call({start_scenario_checking, Scenario}, _From, State) ->
    Reply = maybe_start_scenario_checking(Scenario),
    {reply, Reply, State};
handle_call({add_batches, BatchCount, Scenario}, _From, State) ->
    Reply = maybe_add_batches(Scenario, BatchCount, 1, 0),
    {reply, Reply, State};
handle_call({status, Scenario}, _From, State) ->
    Res = case does_scenario_exist(Scenario) of
        true -> check_test(Scenario, State#state.scenario);
        false -> error
    end,
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({add, Count}, State) ->
    handle_add(Count, State, []),
    {noreply, State};
handle_cast({add, Count, Opts}, State) ->
    handle_add(Count, State, Opts),
    {noreply, State};
handle_cast({remove, Count, Opts}, State) ->
    handle_remove(Count, Opts, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({start_scenario, Scenario, UserIds, ScenarioState}, State) ->
    start_scenario(Scenario, UserIds, ScenarioState),
    {noreply, State};
handle_info({check, Scenario, Interval}, State) ->
    handle_check(Scenario, Interval),
    {noreply, State};
handle_info({add_batch, {Scenario, BatchCount, CurrentBatch, PrevUserCount},
             Interval}, State) ->
    handle_add_batch(Scenario, BatchCount, CurrentBatch, PrevUserCount,
                     Interval),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% callbacks handlers
%% ------------------------------------------------------------------
-spec handle_add(user_count(), state(), proplists:proplist()) ->
    ok | list({ok, pid()}).
handle_add(_Count, #state{scenario=undefined}, _Opts) ->
    ?LOG_ERROR("add users invoked, but no scenario defined");
handle_add(Count, #state{scenario=Scenario,
                         scenario_state=State,
                         nodes = Nodes,
                         node_id = NodeId}, Opts) when
      is_integer(Count), Count > 0 ->
    Last = case ets:last(?TABLE) of
               '$end_of_table' -> 0;
               Other -> Other
           end,
    UserIds = node_userids(Last+1, Last+Count, Nodes, NodeId),
    Interarrival = proplists:get_value(interarrival, Opts, interarrival()),
    start_users(Scenario, UserIds, Interarrival, State).

-spec handle_remove(user_count(), amoc:remove_opts(), state()) -> ok.
handle_remove(_Count, _Opts, #state{scenario=undefined}) ->
    ?LOG_ERROR("remove users invoked, but no scenario defined");
handle_remove(Count, Opts, _State) when
      is_integer(Count), Count > 0 ->
    ForceRemove = proplists:get_value(force, Opts, false),
    Users = last_users(Count),
    stop_users(Users, ForceRemove).

-spec handle_local_do(amoc:scenario(), amoc_scenario:user_id(),
                      amoc_scenario:user_id(), state()) ->
    {reply, ok | {error, term()}, state()}.
handle_local_do(Scenario, Start, End, State) ->
    handle_do(Scenario, lists:seq(Start, End), State).

-spec handle_dist_do(amoc:scenario(), amoc_scenario:user_id(),
                     amoc_scenario:user_id(), non_neg_integer(),
                     node_id(), amoc:do_opts(), state())->
    {reply, ok | {error, term()}, state()}.
handle_dist_do(Scenario, Start, End, NodesCount, NodeId, _Opts, State) ->
    UserIds = node_userids(Start, End, NodesCount, NodeId),
    State1 = State#state{nodes = NodesCount,
                         node_id = NodeId},
    handle_do(Scenario, UserIds, State1).

-spec handle_do(amoc:scenario(), [amoc_scenario:user_id()], state()) ->
    {reply, ok | {error, term()}, state()}.
handle_do(Scenario, UserIds, State) ->
    case code:ensure_loaded(Scenario) of
        {module, Scenario} ->
            case init_scenario(Scenario) of
                {ok, ScenarioState} ->
                    self() ! {start_scenario, Scenario, UserIds, ScenarioState},
                    State1 = State#state{scenario       = Scenario,
                                         scenario_state = ScenarioState},
                    {reply, ok, State1};
                {error, Reason} ->
                    ?LOG_ERROR("scenario  ~p initialisation failed, reason: ~p",
                                [Scenario, Reason]),
                    {reply, {error, Reason}, State}
            end;
        Error ->
            ?LOG_ERROR("scenario module ~p cannot be found, reason: ~p",
                        [Scenario, Error]),
            {reply, {error, Error}, State}
    end.

-spec handle_check(amoc:scenario(), non_neg_integer()) -> any().
handle_check(Scenario, Interval) ->
    case apply_safely(Scenario, continue, []) of
        {ok, continue} ->
            schedule_scenario_checking(Scenario, Interval),
            continue;
        {ok, {stop, Reason}} ->
            ?LOG_INFO("scenario should be terminated, reason=~p", [Reason]),
            try_terminate_scenario(Scenario, Reason);
        {error, Error, Reason} ->
            ?LOG_ERROR("continue/1 callback failed, scenario should be"
                       " terminated, error=~p, reason=~p", [Error, Reason]),
            try_terminate_scenario(Scenario, {Error, Reason})
    end.

-spec handle_add_batch(amoc:scenario(), non_neg_integer(), non_neg_integer(),
                       non_neg_integer(), non_neg_integer()) ->
   any().
handle_add_batch(Scenario, BatchCount, CurrentBatch, PrevUserCount, Interval) ->
    case apply_safely(Scenario, next_user_batch,
                      [CurrentBatch, PrevUserCount]) of
        {ok, BatchStrategy} ->
            add_batch(BatchStrategy),
            TotalUserCount = total_users_in_batch(BatchStrategy),
            maybe_schedule_add_batch({Scenario, BatchCount - 1, CurrentBatch + 1,
                                      TotalUserCount}, Interval);
        {error, Error, Reason} ->
            ?LOG_ERROR("next_user_batch/2 callback failed, scenario should be"
                        " terminated, error=~p, reason=~p", [Error, Reason]),
            try_terminate_scenario(Scenario, {Error, Reason})
    end.

%% ------------------------------------------------------------------
%% helpers
%% ------------------------------------------------------------------

-spec start_scenario(amoc:scenario(), [amoc_scenario:user_id()], state()) ->
    [term()].
start_scenario(Scenario, UserIds, State) ->
    Start = lists:min(UserIds),
    End = lists:max(UserIds),
    Length = erlang:length(UserIds),
    ?LOG_INFO("starting scenario begin_id=~p, end_id=~p, length=~p",
               [Start, End, Length]),
    start_users(Scenario, UserIds, interarrival(), State).

-spec init_scenario(amoc:scenario()) -> any().
init_scenario(Scenario) ->
    case erlang:function_exported(Scenario, init, 0) of
        true ->
            case Scenario:init() of
                ok -> {ok, ok};
                RetValue -> RetValue
            end;
        false ->
            {ok, skip}
    end.

-spec maybe_start_scenario_checking(amoc:scenario()) -> ok | skip.
maybe_start_scenario_checking(Scenario) ->
    Callbacks = [{continue, 0}, {terminate, 1}],
    case are_callbacks_exported(Scenario, Callbacks) of
        true ->
            schedule_scenario_checking(Scenario, checking_interval()),
            ?LOG_INFO("checking has been started", []),
            ok;
        false ->
            ?LOG_INFO("checking is not started as callbacks=~p are not"
                       " exported", [Callbacks]),
            skip
    end.

-spec maybe_add_batches(amoc:scenario(), non_neg_integer(), non_neg_integer(),
                        non_neg_integer()) -> ok | skip.
maybe_add_batches(Scenario, BatchCount, CurrentBatch, PrevUserCount) ->
    Callbacks = [{next_user_batch, 2}],
    case are_callbacks_exported(Scenario, Callbacks) of
        true ->
            maybe_schedule_add_batch({Scenario, BatchCount, CurrentBatch,
                                      PrevUserCount}, add_batch_interval()),
            ok;
        false ->
            ?LOG_INFO("add batches is not possible as callbacks=~p are not"
                       " exported", [Callbacks]),
            skip
    end.

-spec add_batch(user_batch_strategy()) -> any().
add_batch(BatchStrategy) ->
    [amoc_controller:add(Node, UserCount, [{interarrival, Interarrival}])
     || {Node, UserCount, Interarrival} <- BatchStrategy].

-spec are_callbacks_exported(amoc:scenario(), proplists:proplist()) -> boolean().
are_callbacks_exported(Scenario, Callbacks) ->
    case code:ensure_loaded(Scenario) of
        {module, Scenario} ->
            lists:all(fun({Func, Arity}) ->
                              erlang:function_exported(Scenario, Func, Arity)
                      end, Callbacks);
        Error ->
            ?LOG_ERROR("scenario module ~p cannot be found, reason: ~p",
                        [Scenario, Error]),
            false
    end.

-spec schedule_scenario_checking(amoc:scenario(), non_neg_integer()) ->
    reference().
schedule_scenario_checking(Scenario, Interval) ->
    schedule_periodic_action(Scenario, Interval, check).

-spec maybe_schedule_add_batch(term(), non_neg_integer()) -> reference().
maybe_schedule_add_batch({_, BatchCount, _, _}, _) when BatchCount == 0 ->
    ok;
maybe_schedule_add_batch(Data, Interval) ->
    schedule_periodic_action(Data, Interval, add_batch).

-spec schedule_periodic_action(term(), non_neg_integer(), atom()) -> reference().
schedule_periodic_action(Data, Interval, Action) ->
    erlang:send_after(Interval, ?SERVER, {Action, Data, Interval}).

-spec try_terminate_scenario(amoc:scenario(), term()) -> term().
try_terminate_scenario(Scenario, Reason) ->
    case apply_safely(Scenario, terminate, [Reason]) of
        {ok, _} ->
            ok;
        {error, E, R} ->
            ?LOG_ERROR("terminate/1 callback failed, scenario cannot be"
                       " terminated, error=~p, reason=~p", [E, R]),
            application:stop(amoc)
    end.

-spec start_users(amoc:scenario(), [amoc_scenario:user_id()], interarrival(),
                  state()) -> [term()].
start_users(Scenario, UserIds, Interarrival, State) ->
    [ start_user(Scenario, Id, Interarrival, State) || Id <- UserIds ].

-spec start_user(amoc:scenario(), amoc_scenario:user_id(), interarrival(),
                 state()) -> supervisor:startchild_ret().
start_user(Scenario, Id, Interarrival, State) ->
    R = supervisor:start_child(amoc_users_sup, [Scenario, Id, State]),
    timer:sleep(Interarrival),
    R.

-spec stop_users([amoc_scenario:user_id()], boolean()) -> [true | stop].
stop_users(Users, _ForceRemove=true) ->
    [ begin
          ets:delete(?TABLE, Id),
          exit(Pid, shutdown)
      end || {Id, Pid} <- Users ];
stop_users(Users, _ForceRemove=false) ->
    [ Pid ! stop || {_, Pid} <- Users ].

-spec last_users(user_count()) -> [{amoc_scenario:user_id(), pid()}].
last_users(Count) ->
    [ User || User <- last_users(Count, ets:last(?TABLE), []) ].

-spec last_users(user_count(), amoc_scenario:user_id() | '$end_of_table',
                 [{amoc_scenario:user_id(), pid()}]) ->
    [{amoc_scenario:user_id(), pid()}].
last_users(0, _, Acc) ->
    Acc;
last_users(_, '$end_of_table', Acc) ->
    Acc;
last_users(Count, Current, Acc) ->
    Prev = ets:prev(?TABLE, Current),
    [User] = ets:lookup(?TABLE, Current),
    last_users(Count-1, Prev, [ User | Acc ]).

-spec node_userids(amoc_scenario:user_id(), amoc_scenario:user_id(),
                   undefined | non_neg_integer(),
                   undefined | node_id()) ->[non_neg_integer()].
%% amoc_local
node_userids(Start, End, undefined, undefined) ->
    lists:seq(Start, End);
%% amoc_dist
node_userids(Start, End, Nodes, NodeId) when is_integer(Nodes), Nodes > 0,
                                             is_integer(NodeId), NodeId > 0 ->
    F = fun(Id) when Id rem Nodes + 1 =:= NodeId ->
                true;
           (_) ->
                false
        end,
    lists:filter(F, lists:seq(Start, End)).

-spec total_users_in_batch(user_batch_strategy()) ->
    non_neg_integer().
total_users_in_batch(BatchStrategy) ->
    lists:foldl(fun({_, UserCount, _}, Acc) -> Acc + UserCount end, 0,
                BatchStrategy).

-spec interarrival() -> interarrival().
interarrival() ->
    amoc_config:get(interarrival, ?INTERARRIVAL_DEFAULT).

-spec checking_interval() -> integer().
checking_interval() ->
    amoc_config:get(scenario_checking_interval, ?CHECKING_INTERVAL_DEFAULT).

-spec add_batch_interval() -> integer().
add_batch_interval() ->
    amoc_config:get(add_batch_interval, ?ADD_BATCH_INTERVAL_DEFAULT).

-spec get_test_status() -> scenario_status().
get_test_status() ->
    case supervisor:which_children(amoc_users_sup) of
        [] -> finished;
        _Children -> running
    end.

-spec does_scenario_exist(atom()) -> boolean().
does_scenario_exist(Scenario) ->
    {Status, Result} = file:list_dir("scenarios/"),
    case Status of
        ok ->
            lists:member(erlang:atom_to_list(Scenario) ++ ".erl", Result);
        error -> false
    end.
-spec check_test(atom(), amoc:scenario()) -> scenario_status().
check_test(Scenario, CurrentScenario) ->
    case Scenario =:= CurrentScenario of
        true -> get_test_status();
        false -> loaded
    end.

-spec apply_safely(atom(), atom(), [term()]) -> term().
apply_safely(M, F, A) ->
    try erlang:apply(M, F, A) of
        Result ->
            {ok, Result}
    catch
        Error:Reason ->
            {error, Error, Reason}
    end.
