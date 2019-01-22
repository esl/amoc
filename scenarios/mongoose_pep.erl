-module(mongoose_pep).

-include_lib("exml/include/exml.hrl").

-behaviour(amoc_scenario).

-export([start/1]).
-export([init/0]).

-type binjid() :: binary().

-spec init() -> ok.
init() ->
    lager:info("init metrics"),
    amoc_metrics:init(counters, amoc_metrics:messages_spiral_name()),
    amoc_metrics:init(times, amoc_metrics:message_ttd_histogram_name()),
    amoc_metrics:init(counters, stanzas_skipped),
    amoc_metrics:init(counters, iqs_received),
    amoc_metrics:init(counters, iqs_sent),
    amoc_metrics:init(times, response),
    amoc_metrics:init(counters, timeouts),
    amoc_metrics:init(counters, presences_sent),
    amoc_metrics:init(counters, presences_received),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Cfg = make_user(MyId, <<"res1">>),
    {ok, Client, _} = amoc_xmpp:connect_or_exit(Cfg),

    send_presence_available_with_caps(Client),
    %% note: clients may get 'probe' before the response if they are in other client's roster
    %% and the other client connects in "the same" time

    DeviceId = base16:encode(crypto:strong_rand_bytes(6)),
    initialize_omemo(Client, DeviceId),

    NeighbourIds = neighbours(MyId),
    NeighbourJIDs = [make_jid(UserId) || UserId <- NeighbourIds],

    befriend_neighbours(Client, NeighbourJIDs),

    timer:sleep(10000),

    lager:info("Time to send messages"),

    send_messages_to_neighbours(Client, DeviceId, 10000, 100, NeighbourJIDs),

    escalus_connection:wait_forever(Client).

-spec user_spec(binary(), binary(), binary()) -> escalus_users:user_spec().
user_spec(ProfileId, Password, Res) ->
    [ {username, ProfileId},
      {server, <<"localhost">>},
      {password, Password},
      {carbons, false},
      {stream_management, false},
      {resource, Res},
      {sent_stanza_handlers, prepare_handlers(sent_stanza_handlers())},
      {received_stanza_handlers, prepare_handlers(received_stanza_handlers())}
      | pick_server()
    ].

-spec make_user(amoc_scenario:user_id(), binary()) -> escalus_users:user_spec().
make_user(Id, R) ->
    BinId = integer_to_binary(Id),
    ProfileId = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    user_spec(ProfileId, Password, R).


send_presence_available_with_caps(Client) ->
    Pres = escalus_stanza:presence(<<"available">>, [caps()]),
    PresWithId = escalus_stanza:set_id(Pres, escalus_stanza:id()),
    send_request_and_get_response(Client, PresWithId, fun is_own_presence_response/2).

initialize_omemo(Client, DeviceId) ->
    set_omemo_devicelist(Client, DeviceId),
    configure_omemo_devicelist(Client),

    set_omemo_bundle(Client, DeviceId),
    configure_omemo_bundle(Client, DeviceId).

set_omemo_devicelist(Client, DeviceId) ->
    Content = #xmlel{name = <<"list">>,
                     attrs = [{<<"xmlns">>, <<"eu.siacs.conversations.axolotl">>}],
                     children = [#xmlel{name = <<"device">>,
                                        attrs = [{<<"id">>, DeviceId}]}]
                    },
    Req = escalus_pubsub_stanza:publish(Client, Content, escalus_stanza:id(),
                                        {escalus_client:short_jid(Client), ns(devicelist)}),
    send_iq_and_get_result(Client, Req).

ns(devicelist) -> <<"eu.siacs.conversations.axolotl.devicelist">>.

ns(bundle, DeviceId) -> <<"eu.siacs.conversations.axolotl.bundles:", DeviceId/binary>>.

send_iq_and_get_result(Client, Req) ->
    Resp = send_request_and_get_response(Client, Req, fun escalus_pred:is_iq_result/2),
    amoc_metrics:update_counter(iqs_received),
    Resp.

send_iq_and_get_result_or_error(Client, Req) ->
    Resp = send_request_and_get_response(Client, Req, fun escalus_pred:is_iq_result_or_error/2),
    amoc_metrics:update_counter(iqs_received),
    Resp.

send_request_and_get_response(Client, Req, Pred) ->
    send_request_and_get_response(Client, Req, Pred, response).

send_request_and_get_response(Client, Req, Pred, TimeMetric) ->
    escalus_client:send(Client, Req),
    get_response(Client, fun(Stanza) -> Pred(Req, Stanza) end, TimeMetric).

get_response(Client, Pred, TimeMetric) ->
    {Time, Resp} = timer:tc(fun do_get_response/2, [Client, Pred]),
    amoc_metrics:update_time(TimeMetric, Time),
    Resp.

do_get_response(Client, Pred) ->
    case escalus_connection:get_stanza_safe(Client, 10000, Pred) of
        {error, timeout} ->
            amoc_metrics:update_counter(timeouts),
            error(timeout_when_waiting_for_response, [Client, Pred]);
        {Stanza, _} ->
            Stanza
    end.

-spec pick_server() -> [proplists:property()].
pick_server() ->
    Servers = amoc_config:get(xmpp_servers),
    verify(Servers),
    S = length(Servers),
    N = erlang:phash2(self(), S) + 1,
    lists:nth(N, Servers).

verify(Servers) ->
    lists:foreach(
      fun(Proplist) ->
              true = proplists:is_defined(host, Proplist)
      end,
      Servers
     ).

caps() ->
    #xmlel{name = <<"c">>,
           attrs = [{<<"xmlns">>, <<"http://jabber.org/protocol/caps">>},
                    {<<"hash">>, <<"sha-1">>},
                    {<<"node">>, <<"http://www.igniterealtime.org/projects/smack">>},
                    {<<"ver">>, <<"9bNu1aO14ui+PgM1A3v1bi77FrA=">>}]}.

is_own_presence_response(OrigStanza, Stanza) ->
    OrigId = exml_query:attr(OrigStanza, <<"id">>),
    Id = exml_query:attr(Stanza, <<"id">>),
    escalus_pred:is_presence(Stanza) andalso OrigId =:= Id.

configure_omemo_devicelist(Client) ->
    configure_pubsub_node(Client, ns(devicelist)).

configure_omemo_bundle(Client, DeviceId) ->
    configure_pubsub_node(Client, ns(bundle, DeviceId)).

configure_pubsub_node(Client, Node) ->
    Fields = pubsub_config_fields(),
    Req = escalus_pubsub_stanza:set_configuration(Client, <<"configure1">>,
                                                  {escalus_client:short_jid(Client), Node},
                                                  Fields),
    send_iq_and_get_result(Client, Req).

pubsub_config_fields() ->
    [{<<"pubsub#access_model">>, <<"open">>},
     {<<"pubsub#deliver_payloads">>, <<"1">>},
     {<<"pubsub#notify_retract">>, <<"0">>},
     {<<"pubsub#notify_config">>, <<"0">>},
     {<<"pubsub#notify_delete">>, <<"0">>},
     {<<"pubsub#purge_offline">>, <<"0">>},
     {<<"pubsub#persist_items">>, <<"1">>},
     {<<"pubsub#max_items">>, <<"1">>},
     {<<"pubsub#subscribe">>, <<"1">>},
     {<<"pubsub#publish_model">>, <<"publishers">>},
     {<<"pubsub#notification_type">>, <<"headline">>},
     {<<"pubsub#max_payload_size">>, <<"60000">>},
     {<<"pubsub#send_last_published_item">>, <<"on_sub">>},
     {<<"pubsub#deliver_notifications">>, <<"1">>},
     {<<"pubsub#presence_based_delivery">>, <<"1">>},
     {<<"pubsub#itemreply">>, <<"none">>}].

set_omemo_bundle(Client, DeviceId) ->
    Content = #xmlel{name = <<"bundle">>,
                     attrs = [{<<"xmlns">>, <<"eu.siacs.conversations.axolotl">>}],
                     children = [#xmlel{name = <<"signedPreKeyPublic">>,
                                        attrs = [{<<"signedPreKeyId">>, <<"1">>}],
                                        children = [#xmlcdata{content = base64:encode(crypto:strong_rand_bytes(31))}]},
                                 #xmlel{name = <<"signedPreKeySignature">>,
                                        children = [#xmlcdata{content = base64:encode(crypto:strong_rand_bytes(64))}]},
                                 #xmlel{name = <<"identityKey">>,
                                        children = [#xmlcdata{content = base64:encode(crypto:strong_rand_bytes(31))}]},
                                 #xmlel{name = <<"prekeys">>,
                                        children = [pre_key(integer_to_binary(Id)) || Id <- lists:seq(1, 100)]}]},
    Req = escalus_pubsub_stanza:publish(Client, Content, escalus_stanza:id(),
                                        {escalus_client:short_jid(Client), ns(bundle, DeviceId)}),
    send_iq_and_get_result(Client, Req).

pre_key(Id) ->
    #xmlel{name = <<"preKeyPublic">>,
           attrs = [{<<"preKeyId">>, Id}],
           children = [#xmlcdata{content = <<"BcyxFZKc083H46z1xVagNBajs27jNHv9jMW7tJ6WRm4l">>}]}.

neighbours(MyId) ->
    %lager:info("users: ~p", [amoc_controller:users()]),
    lists:delete(MyId, lists:seq(max(1, MyId - number_of_prev_neighbours()),
                                 MyId + number_of_next_neighbours())).

number_of_prev_neighbours() ->
    5.
number_of_next_neighbours() ->
    5.

-spec make_jid(amoc_scenario:user_id()) -> binjid().
make_jid(Id) ->
    BinInt = integer_to_binary(Id),
    ProfileId = <<"user_", BinInt/binary>>,
    Host = <<"localhost">>,
    << ProfileId/binary, "@", Host/binary >>.

befriend_neighbours(Client, NeighbourJids) ->
    RosterJids = get_roster(Client),
    MissingJids = NeighbourJids -- RosterJids,
    case MissingJids of
        [] ->
            done;
        _ ->
            [request_presence_subscription(Client, UserJid) || UserJid <- MissingJids],
            timer:sleep(10000),
            befriend_neighbours(Client, MissingJids)
    end.

get_roster(Client) ->
    Req = escalus_stanza:roster_get(),
    Result = send_iq_and_get_result(Client, Req),
    escalus:assert(is_iq_with_ns, [<<"jabber:iq:roster">>], Result),
    Query = exml_query:subelement(Result, <<"query">>),
    [escalus_utils:jid_to_lower(exml_query:attr(Element, <<"jid">>)) || Element <- Query#xmlel.children,
                                                                        is_subscribed_to(Element)
    ].

is_subscribed_to(RosterItem) ->
    Subscription = exml_query:attr(RosterItem, <<"subscription">>),
    lists:member(Subscription, [<<"to">>, <<"both">>]).

request_presence_subscription(Client, UserJID) ->
    Req = escalus_stanza:presence_direct(UserJID, <<"subscribe">>, []),
    PushReq = send_request_and_get_response(Client, Req, fun(_Req, Stanza) -> escalus_pred:is_roster_set(Stanza) end),
    amoc_metrics:update_counter(iqs_received, 1),
    escalus_client:send(Client, escalus_stanza:iq_result(PushReq)).

-spec send_messages_to_neighbours(escalus:client(), binary(), timeout(), non_neg_integer(), [{binjid(), binary()}]) -> ok.
send_messages_to_neighbours(Client, DeviceId, MessageInterval, Count, [Neighbour | RemNeighbours])
  when Count > 0 ->
    {ToJID, ToDevId} = ensure_omemo_session(Client, Neighbour),
    send_message(Client, DeviceId, <<"chat">>, ToJID, [ToDevId]),
    escalus_connection:wait(Client, MessageInterval),
    send_messages_to_neighbours(Client, DeviceId, MessageInterval, Count - 1, RemNeighbours ++ [{ToJID, ToDevId}]);
send_messages_to_neighbours(_Client, _DeviceId, _Messageinterval, 0, _Neighbours) -> ok.

ensure_omemo_session(_Client, {_ToJID, _DeviceId} = Neighbour) -> Neighbour;
ensure_omemo_session(Client, ToJID) ->
    ToDevId = case amoc_config:get(skip_omemo_setup) of
                  true -> undefined;
                  _ -> get_omemo_device_id(Client, ToJID)
              end,
    case ToDevId of
        undefined -> lager:info("Undefined device ID for ~p", [ToJID]);
        _ -> get_omemo_bundle(Client, ToJID, ToDevId)
    end,
    {ToJID, ToDevId}.

-spec send_message(escalus:client(), binary(), binary(), binjid(), list()) -> ok.
send_message(Client, DeviceId, Type, ToJID, ToDevIds) ->
    Message = omemo_message(DeviceId, Type, ToJID, [ToDevId || ToDevId <- ToDevIds, ToDevId =/= undefined]),
    escalus_client:send(Client, Message).

omemo_message(Sid, Type, ToJID, Rids) ->
    TimeStamp = integer_to_binary(os:system_time(microsecond)), % Inject timestamp as body to measure TTD
    Msg = escalus_stanza:set_id(escalus_stanza:message(undefined, ToJID, Type, TimeStamp),
                                escalus_stanza:id()),
    Children = Msg#xmlel.children ++
        [encrypted_element(Sid, Rids),
         encryption_element()],
    escalus_stanza:markable(Msg#xmlel{children = Children}).

encrypted_element(Sid, Rids) ->
    #xmlel{name = <<"encrypted">>,
           attrs = [{<<"xmlns">>, <<"urn:xmpp:eme:0' namespace='eu.siacs.conversations.axolotl">>}],
           children = [encrypted_header_element(Sid, Rids),
                       encrypted_payload_element()]}.

encrypted_header_element(Sid, Rids) ->
    #xmlel{name = <<"header">>,
           attrs = [{<<"sid">>, Sid}],
           children = [omemo_key_element(Rid) || Rid <- Rids] ++ [omemo_iv_element()]}.

omemo_key_element(Rid) ->
    #xmlel{name = <<"key">>,
           attrs = [{<<"rid">>, Rid}],
           children = [#xmlcdata{content = base64:encode(crypto:strong_rand_bytes(176))}]}.

omemo_iv_element() ->
    #xmlel{name = <<"iv">>,
           children = [#xmlcdata{content = base64:encode(crypto:strong_rand_bytes(16))}]}.

encrypted_payload_element() ->
    #xmlel{name = <<"payload">>,
           children = [#xmlcdata{content = base64:encode(crypto:strong_rand_bytes(56))}]}.

encryption_element() ->
    #xmlel{name = <<"encryption">>,
           attrs = [{<<"xmlns">>, <<"urn:xmpp:eme:0' namespace='eu.siacs.conversations.axolotl">>},
                    {<<"name">>, <<"OMEMO">>}]}.

prepare_handlers(HandlerSpec) ->
    [fun(Client, Stanza) ->
             case Pred(Stanza) of
                 true -> Handler(Client, Stanza),
                         true;
                 false -> false
             end
     end || {Pred, Handler} <- HandlerSpec].

sent_stanza_handlers() ->
    [{fun is_omemo_message/1, fun(_, _) -> amoc_metrics:update_counter(messages_sent, 1) end},
     {fun escalus_pred:is_iq/1, fun(_, _) -> amoc_metrics:update_counter(iqs_sent, 1) end},
     {fun escalus_pred:is_presence/1, fun(_, _) -> amoc_metrics:update_counter(presences_sent, 1) end}].

received_stanza_handlers() ->
    [{fun is_presence_subscribe/1, fun handle_presence_subscribe/2},
     {fun escalus_pred:is_presence/1, fun(_, _) -> amoc_metrics:update_counter(presences_received, 1) end},
     {fun is_omemo_message/1, fun handle_omemo_message/2},
     {fun escalus_pred:is_roster_set/1, fun(_, _) -> amoc_metrics:update_counter(iqs_received) end},
     {fun(_) -> true end, fun skip_stanza/2}].

is_omemo_message(Message = #xmlel{name = <<"message">>}) ->
    lists:member(exml_query:attr(Message, <<"type">>), [<<"chat">>, <<"groupchat">>])
        andalso exml_query:subelement(Message, <<"encrypted">>) =/= undefined;
is_omemo_message(_) -> false.

is_presence_subscribe(Stanza = #xmlel{name = <<"presence">>}) ->
    exml_query:attr(Stanza, <<"type">>) =:= <<"subscribe">>;
is_presence_subscribe(_) -> false.

handle_presence_subscribe(Client, Stanza) ->
    lager:info("~p: handle subscribe ~p", [Client, Stanza]),
    Jid = escalus_utils:jid_to_lower(exml_query:attr(Stanza, <<"from">>)),
    amoc_metrics:update_counter(presences_received, 1),
    escalus_client:send(Client, escalus_stanza:presence_direct(Jid, <<"subscribed">>)).

handle_omemo_message(_Client, Stanza) ->
    Now = os:system_time(microsecond),
    Sent = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
    TTD = (Now - binary_to_integer(Sent)),
    amoc_metrics:update_time(message_ttd, TTD),
    amoc_metrics:update_counter(messages_received, 1).

skip_stanza(_Client, Stanza) ->
    amoc_metrics:update_counter(stanzas_skipped, 1),
    lager:warning("Skipping received stanza ~p", [Stanza]).

get_omemo_bundle(Client, JID, DeviceId) ->
    Req = escalus_pubsub_stanza:get_all_items(Client, <<"getDevice">>,
                                              {JID, ns(bundle, DeviceId)}),
    send_iq_and_get_result(Client, Req).

get_omemo_device_id(Client, JID) ->
    Req = escalus_pubsub_stanza:get_items(Client, escalus_stanza:id(),
                                          {JID, ns(devicelist)}, 1),
    Resp = send_iq_and_get_result_or_error(Client, Req),
    case escalus_pred:is_iq_result(Resp) of
        true ->
            exml_query:path(Resp, [{element, <<"pubsub">>},
                                   {element, <<"items">>},
                                   {element, <<"item">>},
                                   {element, <<"list">>},
                                   {element, <<"device">>},
                                   {attr, <<"id">>}]);
        false ->
            undefined
    end.
