%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_slave).
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
         get_master_node/0,
         get_status/0]).

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
                master :: node()}).

-define(NUMBER_OF_RETRIES, 30).

-type state() :: #state{}.
-type to_ack() :: {node(), no_retries()}.
-type no_retries() :: non_neg_integer().

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    Nodes = amoc_config:get(nodes, []),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Nodes, []).

-spec connect_nodes([node()]) -> ok.
connect_nodes(Nodes) ->
    connect_nodes(node(), Nodes).

-spec ping(node()) -> pong | pang.
ping(Node) ->
    try gen_server:call({?SERVER, Node}, ping) of
        pong ->
            pong
    catch _:_ ->
        pang
    end.

-spec get_master_node() -> node().
get_master_node() ->
    case gen_server:call(?SERVER, get_master_node) of
        undefined -> node();
        MasterNode -> MasterNode
    end.

-spec set_master_node(node()) -> ok.
set_master_node(Node) ->
    gen_server:call({?SERVER, Node}, {set_master_node, node()}).

-spec get_status() -> #{}.
get_status() ->
    gen_server:call(?SERVER, get_status).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init(Nodes) ->
    NewState = handle_connect_nodes(Nodes, #state{}),
    schedule_timer(NewState),
    {ok, NewState}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call(get_master_node, _From, #state{master = Node} = State) ->
    {reply, Node, State};
handle_call({set_master_node, Node}, _From, #state{master = MasterNode} = State) ->
    {NewState, RetValue} = case MasterNode of
                               undefined ->
                                   {State#state{master = Node}, ok};
                               Node -> %% the same master as before
                                   {State, ok};
                               _NewMaster ->
                                   {State, {error, master_is_already_set}}
                           end,
    {reply, RetValue, NewState};
handle_call(ping, _From, State) ->
    {reply, pong, State};
handle_call(get_status, _From, State) ->
    {reply, state_to_map(State), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({connect_nodes, Nodes}, State) ->
    ?LOG_INFO("{connect_nodes, ~p}, state: ~p", [Nodes, state_to_map(State)]),
    NewState = handle_connect_nodes(Nodes, State),
    schedule_timer(NewState),
    {noreply, NewState};
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


-spec merge([{connected | failed_to_connect | connection_lost, [node()]}], state()) -> state().
merge([], State) -> State;
merge([{Type, Nodes} | Tail], State) ->
    NewState = merge(Type, Nodes, State),
    merge(Tail, NewState).


-spec merge(connected | failed_to_connect | connection_lost, [node()], state()) -> state().
merge(connected, Nodes, #state{failed_to_connect = FailedToConnect,
                               connection_lost   = ConnectionLost,
                               connected         = Connected} = State) ->
    NewConnected = lists:usort(Nodes ++ Connected),
    NewNodes = NewConnected -- Connected,
    [begin
         erlang:monitor_node(Node, true),
         connect_nodes(Node, [node() | NewConnected])
     end || Node <- NewNodes],
    State#state{connected         = NewConnected,
                failed_to_connect = FailedToConnect -- Nodes,
                connection_lost   = ConnectionLost -- Nodes};
merge(connection_lost, Nodes, #state{connection_lost = ConnectionLost,
                                     connected = Connected} = State) ->
    State#state{connection_lost = lists:usort(Nodes ++ ConnectionLost),
                connected       = Connected -- Nodes};
merge(failed_to_connect, Nodes, #state{failed_to_connect = FailedToConnect} = State) ->
    State#state{failed_to_connect = lists:usort(Nodes ++ FailedToConnect)}.

-spec state_to_map(#state{}) -> #{}.
state_to_map(#state{} = State) ->
    Fields = record_info(fields, state),
    [state | Values] = tuple_to_list(State),
    KVList = lists:zip(Fields, Values),
    maps:from_list(KVList).
