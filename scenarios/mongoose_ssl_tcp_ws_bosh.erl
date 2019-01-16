%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(mongoose_ssl_tcp_ws_bosh).

-behaviour(amoc_scenario).

-define(HOST, <<"localhost">>).

-export([start/1]).
-export([init/0]).

-type transport() :: tcp | bosh | ws.

-spec init() -> ok.
init() ->
    lager:info("init the scenario").

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Transport = choose_transport(MyId),
    ExtraProps = secure_transport(Transport),

    {ok, Client, _} = amoc_xmpp:connect_or_exit(MyId, ExtraProps),

    %%Allow presence stanza only
    AllowPresence = fun escalus_pred:is_presence/1,

    escalus_connection:set_filter_predicate(Client, AllowPresence),
    %%Drop all stanzas
    %escalus_connection:set_filter_predicate(Client, none),

    amoc_xmpp:send_presence_available(Client),

    lager:info("~p presence resp ~p", [Transport, escalus_client:wait_for_stanza(Client)]),
    timer:sleep(5000),

    NeighbourIds = lists:delete(MyId, lists:seq(max(1,MyId-4),MyId+4)),
    send_messages_many_times(Client, 20000, NeighbourIds),

    timer:sleep(10*1000),
    amoc_xmpp:send_presence_unavailable(Client),
    escalus_connection:stop(Client).

-spec send_messages_many_times(escalus:client(), timeout(), [amoc_scenario:user_id()]) -> ok.
send_messages_many_times(Client, MessageInterval, NeighbourIds) ->
    S = fun(_) ->
                send_messages_to_neighbors(Client, NeighbourIds, MessageInterval)
        end,
    lists:foreach(S, lists:seq(1, 5)).


-spec send_messages_to_neighbors(escalus:client(), [amoc_scenario:user_id()], timeout()) -> list().
send_messages_to_neighbors(Client,TargetIds, SleepTime) ->
    [send_message(Client, TargetId, SleepTime)
     || TargetId <- TargetIds].

-spec send_message(escalus:client(), amoc_scenario:user_id(), timeout()) -> ok.
send_message(Client, ToId, SleepTime) ->
    Msg = amoc_xmpp_stanza:make_message(ToId),
    escalus_connection:send(Client, Msg),
    timer:sleep(SleepTime).

-spec choose_transport(amoc_scenario:user_id()) -> transport().
choose_transport(Id) ->
    Transports = {tcp, bosh, ws},
    TSize = erlang:size(Transports),
    TId = Id rem TSize + 1,
    element(TId, Transports).

-spec secure_transport(transport()) -> [proplists:property()].
secure_transport(tcp) ->
    [{starttls, required}];
secure_transport(ws) ->
    [{transport, ws}, {ssl, true}, {port, 5285}];
secure_transport(bosh) ->
    [{transport, bosh}, {ssl, true}, {port, 5285}].
