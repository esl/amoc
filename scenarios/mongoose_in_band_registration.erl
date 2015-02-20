%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(mongoose_in_band_registration).

-define(HOST, <<"localhost">>).
-define(ADDR,  <<"127.0.0.1">>).

-export([start/1]).
-export([init/0]).

init() ->
    lager:info("init the scenario").

user_spec(ProfileId, XMPPToken, Res) ->
    [ {username, ProfileId},
      {server, ?HOST},
      {host, ?ADDR},
      {password, XMPPToken},
      {carbons, false},
      {stream_management, false},
      {starttls, optional},
      {resource, Res}
    ].

make_user_cfg(AmocId, R) ->
    BinId = integer_to_binary(AmocId),
    ProfileId = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    user_spec(ProfileId, Password, R).

start(MyId) ->
    Cfg = make_user_cfg(MyId, <<"res1">>),

    maybe_register(Cfg),

    {ok, Client, _, _} = escalus_connection:start(Cfg),

    %%Allow presence stanza only
    AllowPresence = fun escalus_pred:is_presence/1,

    escalus_connection:set_filter_predicate(Client, AllowPresence),
    %%Drop all stanzas
    %escalus_connection:set_filter_predicate(Client, none),

    send_presence_available(Client),
    lager:info("presence resp ~p", [escalus_client:wait_for_stanza(Client)]),

    timer:sleep(10000),

    NeighbourIds = lists:delete(MyId, lists:seq(max(1,MyId-4),MyId+4)),
    send_messages_many_times(Cfg, Client, 20000, NeighbourIds),

    timer:sleep(10000),
    send_presence_unavailable(Client),
    escalus_connection:stop(Client).

maybe_register(Cfg) ->
    case erlang:get(registered) of
        undefined ->
            R = escalus_users:create_user(Cfg),
            erlang:put(registered, true),
            lager:info("~p", [R]),
            R;
        _ ->
            already_registered
    end.

send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

send_presence_unavailable(Client) ->
    Pres = escalus_stanza:presence(<<"unavailable">>),
    escalus_connection:send(Client, Pres).

send_messages_many_times(Cfg, Client, MessageInterval, NeighbourIds) ->
    S = fun(_) ->
                send_messages_to_neighbors(Client, NeighbourIds, MessageInterval)
        end,
    lists:foreach(S, lists:seq(1, 5)).


send_messages_to_neighbors(Client,TargetIds, SleepTime) ->
    [send_message(Client, make_jid(TargetId), SleepTime)
     || TargetId <- TargetIds].

send_message(Client, ToId, SleepTime) ->
    Msg = make_message(ToId),
    escalus_connection:send(Client, Msg),
    timer:sleep(SleepTime).

make_message(ToId) ->
    Body = <<"hello sir, you are a gentelman and a scholar.">>,
    Id = escalus_stanza:id(),
    escalus_stanza:set_id(escalus_stanza:chat_to(ToId, Body), Id).

make_jid(Id) ->
    BinInt = integer_to_binary(Id),
    ProfileId = <<"user_", BinInt/binary>>,
    Host = ?HOST,
    << ProfileId/binary, "@", Host/binary >>.

