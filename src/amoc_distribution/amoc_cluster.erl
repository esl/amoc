%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_cluster).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("kernel/include/logger.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0,
         connect_nodes/1,
         ping/1,
         set_master_node/1,
         on_new_connection/1,
         get_status/0,
         all_nodes/0,
         slave_nodes/0,
         master_node/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-record(state, {to_ack = [] :: [to_ack()], %% sorted by nodes
                failed_to_connect = [] :: [node()],
                connection_lost = [] :: [node()],
                connected = [] :: [node()],
                slave = [] :: [node()],
                master :: node() | undefined,
                new_connection_action :: new_connection_handler() | undefined}).

-define(NUMBER_OF_RETRIES, 30).

-type state() :: #state{}.
-type to_ack() :: {node(), no_retries()}.
-type no_retries() :: non_neg_integer().

-type new_connection_handler() :: fun((node()) -> ok).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    Nodes = amoc_config_env:get(nodes, []),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Nodes, []).

-spec connect_nodes([node()]) -> ok.
connect_nodes(Nodes) ->
    connect_nodes(node(), Nodes).

-spec ping(node()) -> pong | pang.
ping(Node) ->
    try gen_server:call({?SERVER, Node}, {ping, node()}) of
        {pong, Node} ->
            pong
    catch _:_ ->
        pang
    end.

-spec set_master_node(node()) -> ok | {error, any()}.
set_master_node(Node) when Node =:= node() ->
    gen_server:call(?SERVER, {set_master_node, node()});
set_master_node(Node) ->
    case get_status() of
        #{new_connection_action:=undefined} ->
            {error, action_is_undefined};
        #{new_connection_action:=Action} ->
            set_master_node(Node, Action)
    end.

-spec get_status() -> #{atom()=>any()}.
get_status() ->
    gen_server:call(?SERVER, get_status).

-spec on_new_connection(new_connection_handler()) -> {ok, [node()]} | {error, any()}.
on_new_connection(Action) when is_function(Action, 1) ->
    %% sets action for the connections and returns
    %% the list of all the currently connected nodes.
    gen_server:call(?SERVER, {on_new_connection, Action}).


-spec all_nodes() -> [node()].
all_nodes() ->
    Status = get_status(),
    [node() | maps:get(connected, Status, [])].

-spec slave_nodes() -> [node()].
slave_nodes() ->
    case master_node() of
        undefined -> [];
        Master ->
            Status = gen_server:call({?SERVER, Master}, get_status),
            maps:get(slave, Status, [])
    end.

-spec master_node() -> node().
master_node() ->
    Status = get_status(),
    maps:get(master, Status, undefined).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init(Nodes) ->
    NewState = handle_connect_nodes(Nodes, #state{}),
    schedule_timer(NewState),
    {ok, NewState}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({set_master_node, Node}, _From, #state{master    = MasterNode,
                                                   connected = Connected} = State) ->
    {RetValue, NewState} = case MasterNode of
                               undefined ->
                                   KnownNodes = [node() | Connected],
                                   case lists:member(Node, KnownNodes) of
                                       true -> {ok, State#state{master = Node}};
                                       false -> {{error, not_connected}, State}
                                   end;
                               Node -> %% the same master as before
                                   {ok, State};
                               _NewMaster ->
                                   {{error, master_is_already_set}, State}
                           end,
    {reply, RetValue, NewState};
handle_call({ping, Node}, _From, State) ->
    {reply, {pong, node()}, merge(connected, [Node], State)};
handle_call(get_status, _From, State) ->
    {reply, state_to_map(State), State};
handle_call({on_new_connection, Action}, _From, #state{master = MasterNode} = State) ->
    {RetValue, NewState} = case {node(), State#state.new_connection_action} of
                               {MasterNode, undefined} ->
                                   Connected = State#state.connected,
                                   S = State#state{new_connection_action = Action},
                                   {{ok, Connected}, S};
                               {MasterNode, _} ->
                                   {{error, handler_is_already_set}, State};
                               _ ->
                                   {{error, not_a_master}, State}
                           end,
    {reply, RetValue, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({connect_nodes, Nodes}, State) ->
    ?LOG_INFO("{connect_nodes, ~p}, state: ~p", [Nodes, state_to_map(State)]),
    NewState = handle_connect_nodes(Nodes, State),
    schedule_timer(NewState),
    {noreply, NewState};
handle_cast({add_slave, Node}, State) ->
    {noreply, merge(slave, [Node], State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(timeout, State) ->
    NewState = ping_nodes(State),
    schedule_timer(NewState),
    {noreply, NewState};
handle_info({nodedown, Node}, #state{master = Node} = State) ->
    ?LOG_ERROR("Master node ~p is down. Halting.", [Node]),
    erlang:halt(),
    {noreply, State};
handle_info({nodedown, Node}, State) ->
    ?LOG_ERROR("node ~p is down.", [Node]),
    {noreply, merge(connection_lost, [Node], State)};
handle_info(_Info, State) ->
    {noreply, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec connect_nodes(node(), [node()]) -> ok.
connect_nodes(Node, Nodes) ->
    gen_server:cast({?SERVER, Node}, {connect_nodes, Nodes}).

-spec set_master_node(node(), new_connection_handler()) -> ok | {error, any()}.
set_master_node(Node, Action) ->
    case gen_server:call({?SERVER, Node}, {set_master_node, node()}) of
        ok ->
            case catch apply(Action, [Node]) of
                ok -> gen_server:cast(?SERVER, {add_slave, Node});
                RetValue -> {error, {invalid_action_ret_value, RetValue}}
            end;
        Error -> Error
    end.

-spec handle_connect_nodes([node()], state()) -> state().
handle_connect_nodes(Nodes, #state{to_ack = Ack, connected = Connected} = State) ->
    NodesToConnect = lists:usort(Nodes) -- [node() | Connected],
    NewAck = [{Node, ?NUMBER_OF_RETRIES} || Node <- NodesToConnect],
    State#state{to_ack = lists:ukeymerge(1, NewAck, Ack)}.

-spec ping_nodes(state()) -> state().
ping_nodes(#state{to_ack = Ack} = State) ->
    {NewConnected, NewFailedToConnect, ReversedNewAck} =
        lists:foldl(fun ping_node/2, {[], [], []}, Ack),
    NewAck = lists:reverse(ReversedNewAck),
    merge([{failed_to_connect, NewFailedToConnect}, {connected, NewConnected}],
          State#state{to_ack = NewAck}).

-spec ping_node(to_ack(), {[node()], [node()], [to_ack()]}) ->
    {[node()], [node()], [to_ack()]}.
ping_node({Node, Retries}, {Connected, FailedToConnect, Ack}) when is_integer(Retries),
                                                                   Retries > 0 ->
    case {ping(Node), Retries} of
        {pong, _} ->
            {[Node | Connected], FailedToConnect, Ack};
        {pang, 1} -> %% that was the last try
            {Connected, [Node | FailedToConnect], Ack};
        {pang, _} ->
            {Connected, FailedToConnect, [{Node, Retries - 1} | Ack]}
    end.

-spec schedule_timer(state()) -> any().
schedule_timer(#state{to_ack = []}) -> ok;
schedule_timer(#state{to_ack = [_ | _]}) ->
    erlang:send_after(1000, self(), timeout).

-spec merge([{connected | slave | failed_to_connect | connection_lost, [node()]}], state()) -> state().
merge([], State) -> State;
merge([{Type, Nodes} | Tail], State) ->
    NewState = merge(Type, Nodes, State),
    merge(Tail, NewState).

-spec merge(connected | slave | failed_to_connect | connection_lost, [node()], state()) -> state().
merge(connected, Nodes, #state{failed_to_connect     = FailedToConnect,
                               connection_lost       = ConnectionLost,
                               connected             = Connected} = State) ->
    NewConnected = lists:usort(Nodes ++ Connected),
    NewNodes = NewConnected -- Connected,
    [begin
         erlang:monitor_node(Node, true),
         maybe_set_master(Node,State),
         %% connect_nodes is based on cast,
         %% so it won't cause the deadlock.
         connect_nodes(Node, NewConnected)
     end || Node <- NewNodes],
    State#state{connected         = NewConnected,
                failed_to_connect = FailedToConnect -- Nodes,
                connection_lost   = ConnectionLost -- Nodes};
merge(connection_lost, Nodes, #state{connected       = Connected, slave = Slave,
                                     connection_lost = ConnectionLost} = State) ->
    State#state{connection_lost = lists:usort(Nodes ++ ConnectionLost),
                connected       = Connected -- Nodes,
                slave           = Slave-- Nodes};
merge(failed_to_connect, Nodes, #state{failed_to_connect = FailedToConnect} = State) ->
    State#state{failed_to_connect = lists:usort(Nodes ++ FailedToConnect)};
merge(slave, Nodes, #state{slave = Slave} = State) ->
    State#state{slave = lists:usort(Nodes ++ Slave)}.

-spec state_to_map(#state{}) -> #{}.
state_to_map(#state{} = State) ->
    Fields = record_info(fields, state),
    [state | Values] = tuple_to_list(State),
    KVList = lists:zip(Fields, Values),
    maps:from_list(KVList).

-spec maybe_set_master(node(), state()) -> any().
maybe_set_master(_, #state{new_connection_action = undefined}) ->
    ok;
maybe_set_master(Node, #state{new_connection_action = Action}) ->
    %% to avoid a possibility of the amoc_cluster deadlock while
    %% running the Action call set_master_node/2 asynchronously
    spawn(fun() -> set_master_node(Node, Action) end).
