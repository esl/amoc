%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(mongoose_ssl_tcp_ws_bosh).

-behaviour(amoc_scenario).

-define(HOST, <<"localhost">>).

-export([start/1]).
-export([init/0]).

-type binjid() :: binary().
-type transport() :: tcp | bosh | ws.


-spec init() -> ok.
init() ->
    lager:info("init the scenario").

-spec user_spec(binary(), binary(), binary()) -> escalus_users:user_spec().
user_spec(ProfileId, XMPPToken, Res) ->
    [ {username, ProfileId},
      {server, ?HOST},
      {host, <<"127.0.0.1">>},
      {password, XMPPToken},
      {carbons, false},
      {stream_management, false},
      {resource, Res}
    ].

-spec make_user_cfg(amoc_scenario:user_id(), binary(), transport()) ->
    escalus_users:user_spec().
make_user_cfg(AmocId, R, Transport) ->
    BinId = integer_to_binary(AmocId),
    ProfileId = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    user_spec(ProfileId, Password, R) ++ secure_transport(Transport).

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Transport = choose_transport(MyId),
    Cfg = make_user_cfg(MyId, <<"res1">>, Transport),

    {ok, Client, _EscalusSessionFeatures} = escalus_connection:start(Cfg),

    %%Allow presence stanza only
    AllowPresence = fun escalus_pred:is_presence/1,

    escalus_connection:set_filter_predicate(Client, AllowPresence),
    %%Drop all stanzas
    %escalus_connection:set_filter_predicate(Client, none),

    send_presence_available(Client),

    lager:info("~p presence resp ~p", [Transport, escalus_client:wait_for_stanza(Client)]),
    timer:sleep(5000),

    NeighbourIds = lists:delete(MyId, lists:seq(max(1,MyId-4),MyId+4)),
    send_messages_many_times(Client, 20000, NeighbourIds),

    timer:sleep(10*1000),
    send_presence_unavailable(Client),
    escalus_connection:stop(Client).

-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

-spec send_presence_unavailable(escalus:client()) -> ok.
send_presence_unavailable(Client) ->
    Pres = escalus_stanza:presence(<<"unavailable">>),
    escalus_connection:send(Client, Pres).

-spec send_messages_many_times(escalus:client(), timeout(), [binjid()]) -> ok.
send_messages_many_times(Client, MessageInterval, NeighbourIds) ->
    S = fun(_) ->
                send_messages_to_neighbors(Client, NeighbourIds, MessageInterval)
        end,
    lists:foreach(S, lists:seq(1, 5)).


-spec send_messages_to_neighbors(escalus:client(), [binjid()], timeout()) -> list().
send_messages_to_neighbors(Client,TargetIds, SleepTime) ->
    [send_message(Client, make_jid(TargetId), SleepTime)
     || TargetId <- TargetIds].

-spec send_message(escalus:client(), binjid(), timeout()) -> ok.
send_message(Client, ToId, SleepTime) ->
    Msg = make_message(ToId),
    escalus_connection:send(Client, Msg),
    timer:sleep(SleepTime).

-spec make_message(binjid()) -> exml:element().
make_message(ToId) ->
    Body = <<"hello sir, you are a gentelman and a scholar.">>,
    Id = escalus_stanza:id(),
    escalus_stanza:set_id(escalus_stanza:chat_to(ToId, Body), Id).

-spec make_jid(amoc_scenario:user_id()) -> binjid().
make_jid(Id) ->
    BinInt = integer_to_binary(Id),
    ProfileId = <<"user_", BinInt/binary>>,
    Host = ?HOST,
    << ProfileId/binary, "@", Host/binary >>.

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
