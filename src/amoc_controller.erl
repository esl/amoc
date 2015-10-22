%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_controller).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(INTERARRIVAL_DEFAULT, 50).
-define(INTERARRIVAL,
        application:get_env(amoc, interarrival, ?INTERARRIVAL_DEFAULT)).
-record(state, {scenario :: amoc:scenario(),
                scenario_state :: any(),
                nodes ::  non_neg_integer(),
                node_id :: node_id()}).


-type state() :: #state{}.
-type node_id() :: non_neg_integer().
-type handle_call_res() :: ok | {ok, term()} | {error, term()}.

%% ------------------------------------------------------------------
%% Types Exports
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0,
         do/3,
         do/7,
         add/1,
         add/2,
         remove/1,
         remove/2,
         remove/3,
         users/0]).
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

-spec add(non_neg_integer()) -> ok.
add(Count) ->
    gen_server:cast(?SERVER, {add, Count}).

-spec add(node(), non_neg_integer()) -> ok.
add(Node, Count) ->
    gen_server:cast({?SERVER, Node}, {add, Count}).

-spec remove(non_neg_integer()) -> ok.
remove(Count) ->
    remove(Count, []).

-spec remove(non_neg_integer(), amoc:remove_opts()) ->ok.
remove(Count, Opts) ->
    gen_server:cast(?SERVER, {remove, Count, Opts}).

-spec remove(node(), non_neg_integer(), amoc:remove_opts()) ->ok.
remove(Node, Count, Opts) ->
    gen_server:cast({?SERVER, Node}, {remove, Count, Opts}).

-spec users() -> [proplists:property()].
users() ->
    {ok, U} = gen_server:call(?SERVER, users),
    U.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(priority, max),
    State = #state{scenario = undefined},
    {ok, State}.

-spec handle_call(any(), any(), state()) -> {reply, handle_call_res(), state()}.
handle_call({do, Scenario, Start, End}, _From, State) ->
    handle_local_do(Scenario, Start, End, State);
handle_call({do, Scenario, Start, End, Nodes, NodeId, Opts}, _From, State) ->
    handle_dist_do(Scenario, Start, End, Nodes, NodeId, Opts, State);
handle_call(users, _From, State) ->
    Reply = [{count, amoc_user_registry:count()},
             {last,  amoc_user_registry:last_id()}],
    {reply, {ok, Reply}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({add, Count}, State) ->
    handle_add(Count, State),
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
-spec handle_add(non_neg_integer(), state()) -> ok | list({ok, pid()}).
handle_add(_Count, #state{scenario=undefined}) ->
    lager:error("add users invoked, but no scenario defined");
handle_add(Count, #state{scenario=Scenario,
                         scenario_state=State,
                         nodes = Nodes,
                         node_id = NodeId}) when
      is_integer(Count), Count > 0 ->
    Last = amoc_user_registry:last_id(),
    UserIds = node_userids(Last+1, Last+Count, Nodes, NodeId),
    start_users(Scenario, UserIds, State).

-spec handle_remove(non_neg_integer(), amoc:remove_opts(), state()) -> ok.
handle_remove(_Count, _Opts, #state{scenario=undefined}) ->
    lager:error("remove users invoked, but no scenario defined");
handle_remove(Count, Opts, _State) when
      is_integer(Count), Count > 0 ->
    ForceRemove = proplists:get_value(force, Opts, false),
    Users = amoc_user_registry:last_users(Count),
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
            ScenarioState = init_scenario(Scenario),
            self() ! {start_scenario, Scenario, UserIds, ScenarioState},
            State1 = State#state{scenario = Scenario,
                                 scenario_state = ScenarioState},
            {reply, ok, State1};
        Error ->
            lager:error("scenario module ~p cannot be found, reason: ~p",
                        [Scenario, Error]),
            {reply, {error, Error}, State}
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
    lager:info("starting scenario begin_id=~p, end_id=~p, length=~p",
               [Start, End, Length]),
    start_users(Scenario, UserIds, State).

-spec init_scenario(amoc:scenario()) -> any().
init_scenario(Scenario) ->
    case erlang:function_exported(Scenario, init, 0) of
        true ->
            Scenario:init();
        false ->
            skip
    end.

-spec start_users(amoc:scenario(), [amoc_scenario:user_id()], state()) ->
    [term()].
start_users(Scenario, UserIds, State) ->
    [ start_user(Scenario, Id, State) || Id <- UserIds ].

-spec start_user(amoc:scenario(), amoc_scenario:user_id(), state()) ->
    supervisor:startchild_ret().
start_user(Scenario, Id, State) ->
    R = supervisor:start_child(amoc_users_sup, [Scenario, Id, State]),
    timer:sleep(?INTERARRIVAL),
    R.

-spec stop_users([amoc_scenario:user_id()], boolean()) -> [true | stop].
stop_users(Users, _ForceRemove=true) ->
    [ begin
          amoc_user_registry:delete(Id),
          exit(Pid, shutdown)
      end || {Id, Pid} <- Users ];
stop_users(Users, _ForceRemove=false) ->
    [ Pid ! stop || {_, Pid} <- Users ].

-spec node_userids(amoc_scenario:user_id(), amoc_scenario:user_id(),
                   non_neg_integer(), node_id()) ->[non_neg_integer()].
node_userids(Start, End, Nodes, NodeId) ->
    F = fun(Id) when Id rem Nodes + 1 =:= NodeId ->
                true;
           (_) ->
                false
        end,
    lists:filter(F, lists:seq(Start, End)).
