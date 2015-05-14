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
         monitor_master/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {to_ack, master}).

-define(DEFAULT_RETRIES, 10).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

ping(Node) ->
    try gen_server:call({?SERVER, Node}, ping) of
        pong ->
            pong
    catch _:_ ->
              pang
    end.

start(Host, Directory) ->
    gen_server:call(?SERVER, {start, Host, Directory}).

monitor_master(Node) ->
    gen_server:call({?SERVER, Node}, {monitor_master, node()}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([]) ->
    schedule_timer(),
    {ok, #state{to_ack = []}}.

handle_call({start, Host, Directory}, _From, State) ->
    State1 = handle_start(Host, Directory, State),
    {reply, ok, State1}; 
handle_call({monitor_master, Master}, _From, State) ->
    true = erlang:monitor_node(Master, true),
    {reply, ok, State#state{master = Master}};
handle_call(ping, _From, State) ->
    {reply, pong, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
handle_start(Host, Directory, #state{to_ack=Ack}=State) ->
    _Port = start_slave_node(Host, Directory),
    Node = node_name(Host),
    Ack1 = [{Node, ?DEFAULT_RETRIES}|Ack],
    State#state{to_ack = Ack1}.

start_slave_node(Host, Directory) ->
    Cmd = "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no "
          ++ Host ++ " " ++ Directory ++ "/bin/amoc start",
    erlang:open_port({spawn, Cmd}, [stream]).

ping_slave_nodes(#state{to_ack=Ack}=State) ->
    Ack1 = lists:filtermap(fun ping_slave_node/1, Ack),
    case Ack1 of
        []  -> ok;
        _   -> schedule_timer()
    end,
    State#state{to_ack=Ack1}.

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

node_name(Host) ->
    list_to_atom("amoc@" ++ Host).

schedule_timer() ->
    erlang:send_after(1000, self(), timeout).
