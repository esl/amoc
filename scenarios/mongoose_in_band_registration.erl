%%==============================================================================
%% Copyright 2019 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(mongoose_in_band_registration).

-define(HOST, <<"localhost">>).
-define(ADDR,  <<"127.0.0.1">>).

-export([start/1]).
-export([init/0]).

-spec init() -> ok.
init() ->
    lager:info("init the scenario").

-spec user_spec(binary(), binary(), binary()) -> escalus_users:user_spec().
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

-spec make_user_cfg(amoc_scenario:user_id(), binary()) -> escalus_users:user_spec().
make_user_cfg(AmocId, R) ->
    BinId = integer_to_binary(AmocId),
    ProfileId = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    user_spec(ProfileId, Password, R).

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Cfg = make_user_cfg(MyId, <<"res1">>),

    maybe_register(Cfg),

    {ok, Client, _} = escalus_connection:start(Cfg),

    %%Allow presence stanza only
    AllowPresence = fun escalus_pred:is_presence/1,

    escalus_connection:set_filter_predicate(Client, AllowPresence),
    %%Drop all stanzas
    %escalus_connection:set_filter_predicate(Client, none),

    escalus_session:send_presence_available(Client),
    lager:info("presence resp ~p", [escalus_client:wait_for_stanza(Client)]),

    timer:sleep(10000),

    NeighbourIds = lists:delete(MyId, lists:seq(max(1,MyId-4),MyId+4)),
    send_messages_many_times(Client, 20000, NeighbourIds),

    timer:sleep(10000),
    escalus_session:send_presence_unavailable(Client),
    escalus_connection:stop(Client).

-spec maybe_register(escalus_users:user_spec()) -> ok | already_registered.
maybe_register(Cfg) ->
    case erlang:get(registered) of
        undefined ->
            R = escalus_users:create_user(Cfg, {dummy, []}),
            erlang:put(registered, true),
            lager:info("~p", [R]),
            ok;
        _ ->
            already_registered
    end.

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
    Msg = amoc_xmpp_stanzas:make_message(ToId),
    escalus_connection:send(Client, Msg),
    timer:sleep(SleepTime).

