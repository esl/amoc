%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_slave).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0,
         start/2,
         monitor_master/1,
         ping/1,
         node_name/1,
         get_master_node/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {to_ack :: [{node(), no_retries()}],
                master :: node()}).
-define(DEFAULT_RETRIES, 10).

-type state() :: #state{}.
-type command() :: {start, string(), file:filename()} |
                   {monitor_master, node()}.
-type no_retries() :: non_neg_integer().

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec ping(node()) -> pong | pang.
ping(Node) ->
    try gen_server:call({?SERVER, Node}, ping) of
        pong ->
            pong
    catch _:_ ->
              pang
    end.

-spec start(string(), file:filename()) -> ok.
start(Host, Directory) ->
    gen_server:call(?SERVER, {start, Host, Directory}).

-spec get_master_node() -> node().
get_master_node() ->
    case gen_server:call(?SERVER, get_master_node) of
        undefined -> node();
        MasterNode -> MasterNode
    end.

-spec monitor_master(node()) -> ok.
monitor_master(Node) ->
    gen_server:call({?SERVER, Node}, {monitor_master, node()}).

-spec node_name(string()) -> node().
node_name(Host) ->
    list_to_atom("amoc@" ++ Host).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
    schedule_timer(),
    {ok, #state{to_ack = []}}.

-spec handle_call(command(), {pid(), any()}, state()) -> {reply, ok |
                                                          pong, state()}.
handle_call({start, Host, Directory}, _From, State) ->
    State1 = handle_start(Host, Directory, State),
    {reply, ok, State1};
handle_call(get_master_node, _From, #state{master = Master} = State) ->
    {reply, Master, State};
handle_call({monitor_master, Master}, _From, State) ->
    true = erlang:monitor_node(Master, true),
    {reply, ok, State#state{master = Master}};
handle_call(ping, _From, State) ->
    {reply, pong, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(timeout, State) ->
    State1 = ping_slave_nodes(State),
    {noreply, State1};
handle_info({nodedown, Master}, #state{master=Master}=State) ->
    lager:error("Master node ~p is down. Halting.", [Master]),
    erlang:halt(),
    {noreply, State};
handle_info({nodedown, Node}, State) ->
    lager:info("Node ~p is down", [Node]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec handle_start(string(), file:filename(), state()) -> state().
handle_start(Host, Directory, #state{to_ack=Ack}=State) ->
    create_status_file(<<"connecting">>),
    _Port = start_slave_node(Host, Directory),
    Node = node_name(Host),
    Ack1 = [{Node, ?DEFAULT_RETRIES} | Ack],
    State#state{to_ack = Ack1}.

-spec start_slave_node(string(), file:filename()) -> port().
start_slave_node(Host, Directory) ->
    Cmd = "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no "
          ++ Host ++ " " ++ Directory ++ "/bin/amoc start",
    erlang:open_port({spawn, Cmd}, [stream]).

-spec ping_slave_nodes(state()) -> state().
ping_slave_nodes(#state{to_ack=Ack}=State) ->
    Ack1 = lists:filtermap(fun ping_slave_node/1, Ack),
    case Ack1 of
        []  -> create_status_file(<<"ready">>);
        _   -> schedule_timer()
    end,
    State#state{to_ack=Ack1}.

-spec ping_slave_node({node(), no_retries()}) -> false |
                                                 {true, {node(), no_retries()}}.
ping_slave_node({Node, 0}) ->
    lager:error("Limit of retries exceeded for node ~p", [Node]),
    false;
ping_slave_node({Node, Retries}) ->
    case ping(Node) of
        pong ->
            ok = monitor_master(Node),
            true = erlang:monitor_node(Node, true),
            lager:info("Node ~p successfully connected", [Node]),
            false;
        pang ->
            {true, {Node, Retries-1}}
    end.

-spec schedule_timer() -> reference().
schedule_timer() ->
    erlang:send_after(1000, self(), timeout).

-spec create_status_file(binary()) -> ok.
create_status_file(Status) ->
    Path = application:get_env(amoc, status_file, ".amoc.status"),
    ok = file:write_file(Path, Status).
