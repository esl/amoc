-module(gdpr_removal).

-behavior(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-required_variable({'IQ_TIMEOUT',             <<"IQ timeout (milliseconds, def: 10000ms)"/utf8>>}).
-required_variable({'ROOM_CREATION_RATE',     <<"Rate of room creations (per minute, def:600)">>}).
-required_variable({'NODE_CREATION_RATE',     <<"Rate of node creations (per minute, def:600)">>}).
-required_variable({'ROOM_PUBLICATION_RATE',  <<"Rate of publications to room (per minute, def:1500)">>}).
-required_variable({'NODE_PUBLICATION_RATE',  <<"Rate of publications to PEP node (per minute, def:1500)">>}).
-required_variable({'ROOM_SIZE',              <<"Number of users in a room.">>}).
-required_variable({'N_OF_SUBSCRIBERS',       <<"Number of subscriptions for each node (def: 50)"/utf8>>}).
-required_variable({'ROOM_ACTIVATION_POLICY', <<"Publish after setup of (def: all_rooms | n_sers)"/utf8>>}).
-required_variable({'NODE_ACTIVATION_POLICY', <<"Publish after setup of (def: all_nodes | n_nodes)"/utf8>>}).
-required_variable({'GDPR_REMOVAL_RATE',      <<"Rate of user removals (per minute, def:1)">>}).
-required_variable({'PUBLICATION_SIZE',       <<"Size of additional payload (bytes, def:300)">>}).
-required_variable({'MIM_HOST',               <<"The virtual host served by the server (def: <<\"localhost\">>)"/utf8>>}).
-required_variable({'MUC_HOST',               <<"The virtual MUC host served by the server (def: <<\"muclight.localhost\">>)"/utf8>>}).

-define(ALL_PARAMETERS, [
    {iq_timeout,                  10000, positive_integer},
    {room_creation_rate,            600, positive_integer},
    {node_creation_rate,            600, positive_integer},
    {room_publication_rate,        1500, positive_integer},
    {node_publication_rate,        1500, positive_integer},
    {room_size,                      10, positive_integer},
    {n_of_subscribers,               50, nonnegative_integer},
    {room_activation_policy,  all_rooms, [all_rooms, n_rooms]},
    {node_activation_policy,  all_nodes, [all_nodes, n_nodes]},
    {gdpr_removal_rate,               2, positive_integer},
    {publication_size,              300, nonnegative_integer},
    {mim_host,          <<"localhost">>, bitstring},
    {muc_host, <<"muclight.localhost">>, bitstring}
]).

-define(ROOM_CREATION_THROTTLING, room_creation).
-define(NODE_CREATION_THROTTLING, node_creation).
-define(ROOM_PUBLICATION_THROTTLING, room_publication).
-define(NODE_PUBLICATION_THROTTLING, node_publication).
-define(REMOVAL_THROTTLING, user_removal).

-define(ROOM_CREATION_ID, <<"room_creation_id">>).
-define(NODE_CREATION_ID, <<"node_creation_id">>).

-define(PEP_NODE_NS, <<"just_some_random_namespace">>).
-define(CAPS_HASH, <<"erNmVoMSwRBR4brUU/inYQ5NFr0=">>). %% mod_caps:make_disco_hash(feature_elems(), sha1).
-define(NODE, {pep, ?PEP_NODE_NS}).

-define(NS_MUC_LIGHT_AFFILIATIONS, <<"urn:xmpp:muclight:0#affiliations">>).
-define(NS_MUC_LIGHT_CREATION, <<"urn:xmpp:muclight:0#create">>).

-export([init/0, start/2]).

-spec init() -> {ok, amoc_scenario:state()} | {error, Reason :: term()}.
init() ->
    init_metrics(),
    case amoc_config:parse_scenario_settings(?ALL_PARAMETERS) of
        {ok, Settings} ->
            http_req:start(),

            dump_settings(Settings),

            [RoomPublicationRate, NodePublicationRate, RoomCreationRate, NodeCreationRate, GDPRRemovalRate] =
                [get_parameter(Key, Settings) ||
                    Key <- [room_publication_rate, node_publication_rate,
                            room_creation_rate, node_creation_rate,
                            gdpr_removal_rate]],
            amoc_throttle:start(?ROOM_CREATION_THROTTLING, RoomCreationRate),
            amoc_throttle:start(?ROOM_PUBLICATION_THROTTLING, RoomPublicationRate),
            amoc_throttle:start(?NODE_CREATION_THROTTLING, NodeCreationRate),
            amoc_throttle:start(?NODE_PUBLICATION_THROTTLING, NodePublicationRate),
            amoc_throttle:start(?REMOVAL_THROTTLING, GDPRRemovalRate),

            start_coordinator(Settings),
            {ok, Settings};
        Error -> Error
    end.

-spec start(amoc_scenario:user_id(), amoc_scenario:state()) -> any().
start(Id, Settings) ->
    store_scenario_settings(Settings),
    Client = connect_amoc_user(Id),
    start_user(Client).

init_metrics() ->
    Counters = [pubsub_message, muc_light_message,
        room_creation_success, room_creation_timeout, room_creation_failure,
        node_creation_success, node_creation_timeout, node_creation_failure,
        publication_query, publication_result, publication_error,
        publication_success, publication_timeout,
        muc_light_message_sent,

        muc_light_affiliation_change_messages,

        room_affiliation_change_success, room_affiliation_change_timeout, room_affiliation_change_failure,

        gdpr_removal],
    Times = [room_creation, node_creation,
        pubsub_publication,
        pubsub_message_ttd, muc_light_ttd,
        room_affiliation_change,
        gdpr_removal],
    [amoc_metrics:init(counters, Metric) || Metric <- Counters],
    [amoc_metrics:init(times, Metric) || Metric <- Times].

%%------------------------------------------------------------------------------------------------
%% Coordinator
%%------------------------------------------------------------------------------------------------
start_coordinator(Settings) ->
    Plan = get_plan(Settings),
    amoc_coordinator:start(?MODULE, Plan).

get_plan(Settings) ->
    [{get_parameter(room_size, Settings),
      fun(_, PidsAndClients) ->
          make_full_rooms(PidsAndClients),
          room_activate_users(Settings, pids(PidsAndClients), n_rooms)
      end},
     {get_parameter(n_of_subscribers, Settings),
      fun(_, PidsAndClients) ->
          make_all_clients_friends(clients(PidsAndClients)),
          node_activate_users(Settings, pids(PidsAndClients), n_nodes)
      end},
     {all,
      fun(_, PidsAndClients) ->
          room_activate_users(Settings, pids(PidsAndClients), all_rooms),
          node_activate_users(Settings, pids(PidsAndClients), all_nodes),
          activate_removal(pids(PidsAndClients))
      end}].

clients(PidsAndClients) ->
    {_Pids, Clients} = lists:unzip(PidsAndClients),
    Clients.

pids(PidsAndClients) ->
    {Pids, _Clients} = lists:unzip(PidsAndClients),
    Pids.

node_activate_users(Settings, Pids, ActivationPolicy) ->
    case get_parameter(node_activation_policy, Settings) of
        ActivationPolicy ->
            lager:debug("Node activate users running. Policy ~p. Pids: ~p", [ActivationPolicy, Pids]),
            [schedule_node_publishing(Pid) || Pid <- Pids];
        _ -> ok
    end.

room_activate_users(Settings, Pids, ActivationPolicy) ->
    case get_parameter(room_activation_policy, Settings) of
        ActivationPolicy ->
            lager:debug("Room activate users running. Policy ~p. Pids: ~p", [ActivationPolicy, Pids]),
            [schedule_room_publishing(Pid) || Pid <- Pids];
        _ -> ok
    end.

activate_removal(Pids) ->
    [schedule_removal(Pid) || Pid <- Pids].

make_all_clients_friends(Clients) ->
    lager:debug("Make all clients friends."),
    escalus_utils:distinct_pairs(
        fun(C1, C2) ->
            send_presence(C1, <<"subscribe">>, C2),
            send_presence(C2, <<"subscribe">>, C1)
        end, Clients).

make_full_rooms(PidsAndClients) ->
    PidsAndJids = [{Pid, Client#client.jid} || {Pid, Client} <- PidsAndClients],
    [begin
         MemberJids = [Jid || {_, Jid} <- PidsAndJids, Jid =/= OwnerJid],
         OwnerPid ! {add_users, MemberJids}
     end || {OwnerPid, OwnerJid} <- PidsAndJids].

schedule_removal(Pid) ->
    amoc_throttle:send(?REMOVAL_THROTTLING, Pid, remove_user).

%%------------------------------------------------------------------------------------------------
%% User
%%------------------------------------------------------------------------------------------------
start_user(Client) ->
    lager:debug("User process ~p", [self()]),
    erlang:monitor(process, Client#client.rcv_pid),
    create_new_node(Client),
    lager:debug("Node created User process ~p", [self()]),
    send_presence_with_caps(Client),
    escalus_tcp:set_active(Client#client.rcv_pid, true),
    {TS, Id} = request_muc_light_room(Client),
    user_loop(Client, #{Id=>{new, TS}}).

create_new_node(Client) ->
    amoc_throttle:send_and_wait(?NODE_CREATION_THROTTLING, create_node),
    create_pubsub_node(Client).

user_loop(Client, Requests) ->
    IqTimeout = get_parameter(iq_timeout),
    receive
        {publish_item_room, RoomJid} ->
            amoc_metrics:update_counter(muc_light_message_sent),
            send_message_to_room(Client, RoomJid),
            user_loop(Client, Requests);
        publish_item_node ->
            {TS, Id} = publish_pubsub_item(Client),
            amoc_metrics:update_counter(publication_query),
            user_loop(Client, Requests#{Id=>{new, TS}});
        {add_users, MemberJids} ->
            {TS, Id} = add_users_to_room(Client, MemberJids),
            user_loop(Client, Requests#{Id=>{new, TS}});
        remove_user ->
            lager:debug("GDPR: Removing myself ~p (~p)", [escalus_client:short_jid(Client), self()]),
            remove_self(Client);
        {stanza, _, #xmlel{name = <<"message">>} = Stanza, #{recv_timestamp := RecvTimeStamp}} ->
            process_message(Stanza, RecvTimeStamp),
            user_loop(Client, Requests);
        {stanza, _, #xmlel{name = <<"iq">>} = Stanza, #{recv_timestamp := RecvTimeStamp}} ->
            NewRequests = process_iq(Client, Stanza, RecvTimeStamp, Requests),
            user_loop(Client, NewRequests);
        {stanza, _, #xmlel{name = <<"presence">>} = Stanza, _} ->
            process_presence(Client, Stanza),
            user_loop(Client, Requests);
        {'DOWN', _, process, Pid, Info} when Pid =:= Client#client.rcv_pid ->
            lager:error("TCP connection process ~p down: ~p", [Pid, Info]);
        Msg ->
            lager:error("unexpected message ~p", [Msg])
    after IqTimeout ->
        user_loop(Client, verify_request(Requests))
    end.

verify_request(Requests) ->
    IqTimeout = get_parameter(iq_timeout),
    Now = os:system_time(microsecond),
    VerifyFN =
        fun(Key, Value) ->
            case Value of
                {new, TS} when Now > TS + IqTimeout * 1000 ->
                    update_timeout_metrics(Key),
                    {timeout, TS};
                _ -> Value
            end
        end,
    maps:map(VerifyFN, Requests).

update_timeout_metrics(<<"publish", _/binary>>) ->
    amoc_metrics:update_counter(publication_timeout);
update_timeout_metrics(<<"affiliation", _/binary>>) ->
    amoc_metrics:update_counter(room_affiliation_change_timeout);
update_timeout_metrics(?ROOM_CREATION_ID) ->
    amoc_metrics:update_counter(room_creation_timeout);
update_timeout_metrics(Id) ->
    lager:error("unknown iq id ~p", Id).

schedule_room_publishing(Pid) ->
    amoc_throttle:send(?ROOM_PUBLICATION_THROTTLING, Pid, {publish_item_room, undefined}).
schedule_room_publishing(Pid, RoomJid) ->
    amoc_throttle:send(?ROOM_PUBLICATION_THROTTLING, Pid, {publish_item_room, RoomJid}).

schedule_node_publishing(Pid) ->
    amoc_throttle:send(?NODE_PUBLICATION_THROTTLING, Pid, publish_item_node).

remove_self(Client) ->
    %TODO when running with clt-swart make sure to use correct cfg, change ports here etc.
    Path = list_to_binary(["/api/users/", get_parameter(mim_host), "/", escalus_client:username(Client)]),

    {RemovalTime, {ok, _}} = timer:tc(fun() -> http_req:request("http://localhost:8088", Path, <<"DELETE">>, []) end),

    amoc_metrics:update_counter(gdpr_removal),
    amoc_metrics:update_time(gdpr_removal, RemovalTime),
    % Suppresses errors from escalus, unlike just jumping out of loop
    throw(stop).

%%------------------------------------------------------------------------------------------------
%% User connection
%%------------------------------------------------------------------------------------------------
connect_amoc_user(Id) ->
    Cfg = make_user_cfg(Id),
    {ok, Client, _} = escalus_connection:start(Cfg),
    erlang:put(jid, Client#client.jid),
    Client.

make_user_cfg(Id) ->
    BinId = integer_to_binary(Id),
    Username = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    Resource = <<"res1">>,
    ConnectionDetails = amoc_xmpp:pick_server([[{host, "127.0.0.1"}]]),
    [{username, Username},
        {server, get_parameter(mim_host)},
        {resource, Resource},
        {password, Password},
        {carbons, false},
        {stream_management, false},
        {socket_opts, socket_opts()} |
        ConnectionDetails].

socket_opts() ->
    [binary,
        {reuseaddr, false},
        {nodelay, true}].

%%------------------------------------------------------------------------------------------------
%% Node creation
%%------------------------------------------------------------------------------------------------
create_pubsub_node(Client) ->
    ReqId = ?NODE_CREATION_ID,
    Request = publish_pubsub_stanza(Client, ReqId, #xmlel{name = <<"nothing">>}),
    escalus:send(Client, Request),

    {CreateNodeTime, CreateNodeResult} = timer:tc(
        fun() ->
            catch escalus:wait_for_stanza(Client, get_parameter(iq_timeout))
        end),

    case {escalus_pred:is_iq_result(Request, CreateNodeResult), CreateNodeResult} of
        {true, _} ->
%%            lager:debug("node creation ~p (~p)", [?NODE, self()]),
            amoc_metrics:update_counter(node_creation_success),
            amoc_metrics:update_time(node_creation, CreateNodeTime);
        {false, {'EXIT', {timeout_when_waiting_for_stanza, _}}} ->
            amoc_metrics:update_counter(node_creation_timeout),
            lager:error("Timeout creating node: ~p", [CreateNodeResult]),
            exit(node_creation_timeout);
        {false, _} ->
            amoc_metrics:update_counter(node_creation_failure),
            lager:error("Error creating node: ~p", [CreateNodeResult]),
            exit(node_creation_failed)
    end.

%%------------------------------------------------------------------------------------------------
%% User presence & caps
%%------------------------------------------------------------------------------------------------
send_presence(From, Type, To = #client{}) ->
    ToJid = escalus_client:short_jid(To),
    send_presence(From, Type, ToJid);
send_presence(From, Type, To) ->
    Presence = escalus_stanza:presence_direct(To, Type),
    escalus_client:send(From, Presence).

send_presence_with_caps(Client) ->
    lager:debug("Send presence with caps ~p, (~p).", [escalus_client:short_jid(Client), self()]),
    Presence = escalus_stanza:presence(<<"available">>, [caps()]),
    escalus:send(Client, Presence).

caps() ->
    #xmlel{name = <<"c">>,
        attrs = [{<<"xmlns">>, <<"http://jabber.org/protocol/caps">>},
            {<<"hash">>, <<"sha-1">>},
            {<<"node">>, <<"http://www.chatopus.com">>},
            {<<"ver">>, ?CAPS_HASH}]}.

%%------------------------------------------------------------------------------------------------
%% Room creation
%%------------------------------------------------------------------------------------------------
request_muc_light_room(Client) ->
    amoc_throttle:send_and_wait(?ROOM_CREATION_THROTTLING, create_room),
    Id = ?ROOM_CREATION_ID,
    MucHost = get_parameter(muc_host),
    CreateRoomStanza = escalus_stanza:iq_set(?NS_MUC_LIGHT_CREATION, []),
    CreateRoomStanzaWithTo = escalus_stanza:to(CreateRoomStanza, MucHost),
    CreateRoomStanzaWithId = escalus_stanza:set_id(CreateRoomStanzaWithTo, Id),

    escalus:send(Client, CreateRoomStanzaWithId),
    {os:system_time(microsecond), Id}.

%%------------------------------------------------------------------------------------------------
%% Room affiliation change
%%------------------------------------------------------------------------------------------------
add_users_to_room(Client, Jids) ->
    Id = iq_id(affiliation, Client),
    RoomJid = erlang:get(my_room),
    AffList = [#xmlel{name = <<"user">>,
        attrs = [{<<"affiliation">>, <<"member">>}],
        children = [#xmlcdata{content = Jid}]} || Jid <- Jids],
    AffChangeStanza = escalus_stanza:iq_set(?NS_MUC_LIGHT_AFFILIATIONS, AffList),
    AffChangeStanzaWithId = escalus_stanza:set_id(AffChangeStanza, Id),
    lager:debug("Adding users to room: ~p", [Jids]),
    escalus:send(Client, escalus_stanza:to(AffChangeStanzaWithId, RoomJid)),
    {os:system_time(microsecond), Id}.

%%------------------------------------------------------------------------------------------------
%% Sending muc_light messages
%%------------------------------------------------------------------------------------------------
send_message_to_room(Client, undefined) ->
    RoomJid = erlang:get(my_room),
    send_message_to_room(Client, RoomJid);
send_message_to_room(Client, RoomJid) ->
    PayloadSize = get_parameter(publication_size),
    MessageBody = item_content(PayloadSize),
    Message = #xmlel{name = <<"message">>,
        attrs = [{<<"to">>, RoomJid},
            {<<"type">>, <<"groupchat">>}],
        children = [MessageBody]},
    escalus:send(Client, Message).

%%------------------------------------------------------------------------------------------------
%% Item publishing
%%------------------------------------------------------------------------------------------------

publish_pubsub_item(Client) ->
    Id = iq_id(publish, Client),
    PayloadSize = get_parameter(publication_size),
    Content = item_content(PayloadSize),
    Request = publish_pubsub_stanza(Client, Id, Content),
    escalus:send(Client, Request),
    {os:system_time(microsecond), Id}.

publish_pubsub_stanza(Client, Id, Content) ->
    ItemId = <<"current">>,
    escalus_pubsub_stanza:publish(Client, ItemId, Content, Id, ?NODE).

item_content(PayloadSize) ->
    Payload = #xmlcdata{content = <<<<"A">> || _ <- lists:seq(1, PayloadSize)>>},
    #xmlel{
        name = <<"entry">>,
        attrs = [{<<"timestamp">>, integer_to_binary(os:system_time(microsecond))},
            {<<"jid">>, erlang:get(jid)}],
        children = [Payload]}.

%%------------------------------------------------------------------------------------------------
%% Item processing
%%------------------------------------------------------------------------------------------------

process_message(Stanza, RecvTimeStamp) ->
    Type = exml_query:attr(Stanza, <<"type">>),
    case Type of
        <<"groupchat">> -> process_muc_light_message(Stanza, RecvTimeStamp);
        _ -> process_pubsub_msg(Stanza, RecvTimeStamp)
    end.

process_pubsub_msg(#xmlel{name = <<"message">>} = Stanza, TS) ->
    Entry = exml_query:path(Stanza, [{element, <<"event">>}, {element, <<"items">>},
        {element, <<"item">>}, {element, <<"entry">>}]),
    case Entry of
        undefined -> ok;
        _ ->
            case {exml_query:attr(Entry, <<"jid">>), erlang:get(jid)} of
                {JID, JID} -> schedule_node_publishing(self());
                _ -> ok
            end,
            TimeStampBin = exml_query:attr(Entry, <<"timestamp">>),
            TimeStamp = binary_to_integer(TimeStampBin),
            TTD = TS - TimeStamp,
%%            lager:debug("pubsub time to delivery ~p", [TTD]),
            amoc_metrics:update_counter(pubsub_message),
            amoc_metrics:update_time(pubsub_message_ttd, TTD)
    end.

process_muc_light_message(Stanza, RecvTimeStamp) ->
    case exml_query:subelement(Stanza, <<"x">>) of
        undefined ->
            handle_normal_muc_light_message(Stanza, RecvTimeStamp);
        #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_MUC_LIGHT_AFFILIATIONS}], children = _} ->
            handle_muc_light_affiliation_message(Stanza),
            amoc_metrics:update_counter(muc_light_affiliation_change_messages);
        _ -> lager:error("Unknown message.")
    end.

handle_normal_muc_light_message(Stanza, RecvTimeStamp) ->
    ReqTimeStampBin = exml_query:path(Stanza, [{element, <<"entry">>}, {attr, <<"timestamp">>}]),
    ReqTimeStamp = binary_to_integer(ReqTimeStampBin),

    RoomBareJid = get_sender_bare_jid(Stanza),

    From = exml_query:path(Stanza, [{element, <<"entry">>}, {attr, <<"jid">>}]),
    case erlang:get(jid) of
        From -> schedule_room_publishing(self(), RoomBareJid);
        _ -> ok
    end,

    TTD = RecvTimeStamp - ReqTimeStamp,
%%    lager:debug("muc light time to delivery ~p", [TTD]),
    amoc_metrics:update_counter(muc_light_message),
    amoc_metrics:update_time(muc_light_ttd, TTD).

handle_muc_light_affiliation_message(Stanza) ->
    amoc_metrics:update_counter(muc_light_affiliation_change_messages),
    case exml_query:subelement(Stanza, <<"prev-version">>) of
        % actually XEP states only that prev-version SHOULD NOT be sent to new users - not sure if can rely on that
        undefined -> handle_first_affiliation_message(Stanza);
        _ -> ok % drop affiliation change stanzas
    end.

handle_first_affiliation_message(Stanza) ->
    RoomJid = exml_query:attr(Stanza, <<"from">>),
    case erlang:get(rooms) of
        undefined ->
            erlang:put(rooms, [RoomJid]),
            erlang:put(my_room, RoomJid);
        RoomList ->
            case lists:member(RoomJid, RoomList) of
                true -> ok;
                false -> erlang:put(rooms, [RoomJid | RoomList])
            end
    end.

process_presence(Client, Stanza) ->
    case exml_query:attr(Stanza, <<"type">>) of
        <<"subscribe">> ->
            From = exml_query:attr(Stanza, <<"from">>),
            send_presence(Client, <<"subscribed">>, From);
        _ ->
            ok %%it's ok to just ignore other presence notifications
    end.

process_iq(Client, #xmlel{name = <<"iq">>} = Stanza, TS, Requests) ->
    Id = exml_query:attr(Stanza, <<"id">>),
    Type = exml_query:attr(Stanza, <<"type">>),
    NS = exml_query:path(Stanza, [{element, <<"query">>}, {attr, <<"xmlns">>}]),
    case {Type, NS, Id, maps:get(Id, Requests, undefined)} of
        {<<"result">>, undefined, ?ROOM_CREATION_ID, {Tag, ReqTS}} ->
            handle_muc_light_room_iq_result(Stanza, {Tag, TS - ReqTS}),
            send_info_to_coordinator(Client);
        {<<"result">>, _, <<"affiliation", _/binary>>, {Tag, ReqTS}} ->
            handle_affiliation_change_iq(Stanza, {Tag, TS - ReqTS});
        {<<"get">>, ?NS_DISCO_INFO, _, undefined} ->
            handle_disco_query(Client, Stanza);
        {<<"set">>, ?NS_ROSTER, _, undefined} ->
            ok; %%it's ok to just ignore roster pushes
        {_, undefined, <<"publish", _/binary>>, undefined} ->
            lager:warning("unknown publish iq ~p", [Stanza]);
        {_, undefined, <<"publish", _/binary>>, {Tag, ReqTS}} ->
            handle_publish_resp(Stanza, {Tag, TS - ReqTS});
        _ ->
            lager:warning("unexpected iq ~p", [Stanza])
    end,
    maps:remove(Id, Requests).

handle_muc_light_room_iq_result(CreateRoomResult, {Tag, RoomCreationTime}) ->
    case {escalus_pred:is_iq_result(CreateRoomResult), CreateRoomResult} of
        {true, _} ->
            lager:debug("Room creation ~p took ~p", [self(), RoomCreationTime]),
            amoc_metrics:update_time(room_creation, RoomCreationTime),
            IqTimeout = get_parameter(iq_timeout),
            case Tag of
                new when IqTimeout * 1000 > RoomCreationTime ->
                    amoc_metrics:update_counter(room_creation_success);
                new ->
                    amoc_metrics:update_counter(room_creation_timeout);
                timeout -> ok %% do nothing, it's already reported as timeout
            end;
        {false, _} ->
            amoc_metrics:update_counter(room_creation_failure),
            lager:error("Error creating room: ~p", [CreateRoomResult]),
            exit(room_creation_failed)
    end.

send_info_to_coordinator(Client) ->
    lager:debug("Process ~p, sending info about myself to coordinator", [self()]),
    amoc_coordinator:add(?MODULE, Client).

handle_affiliation_change_iq(AffiliationChangeResult, {Tag, AffiliationChangeTime}) ->
    case {escalus_pred:is_iq_result(AffiliationChangeResult), AffiliationChangeResult} of
        {true, _} ->
            lager:debug("Room creation ~p took ~p", [self(), AffiliationChangeTime]),
            amoc_metrics:update_time(room_affiliation_change, AffiliationChangeTime),
            IqTimeout = get_parameter(iq_timeout),
            case Tag of
                new when IqTimeout * 1000 > AffiliationChangeTime ->
                    amoc_metrics:update_counter(room_affiliation_change_success);
                new ->
                    amoc_metrics:update_counter(room_affiliation_change_timeout);
                timeout -> ok %% do nothing, it's already reported as timeout
            end;
        {false, _} ->
            amoc_metrics:update_counter(room_affiliation_change_failure),
            lager:error("Error affiliation change: ~p", [AffiliationChangeTime]),
            exit(affiliation_change_timeout)
    end.

handle_publish_resp(PublishResult, {Tag, PublishTime}) ->
    IqTimeout = get_parameter(iq_timeout),
    case escalus_pred:is_iq_result(PublishResult) of
        true ->
            amoc_metrics:update_counter(publication_result),
            amoc_metrics:update_time(pubsub_publication, PublishTime),
            case Tag of
                new when IqTimeout * 1000 > PublishTime ->
                    amoc_metrics:update_counter(publication_success);
                new ->
                    amoc_metrics:update_counter(publication_timeout);
                timeout -> ok %% do nothing, it's already reported as timeout
            end;
        _ ->
            amoc_metrics:update_counter(publication_error),
            lager:error("Error publishing failed: ~p", [PublishResult]),
            exit(publication_failed)
    end.


handle_disco_query(Client, DiscoRequest) ->
    lager:debug("handle_disco_query ~p", [self()]),
    QueryEl = escalus_stanza:query_el(<<"http://jabber.org/protocol/disco#info">>,
        feature_elems()),
    DiscoResult = escalus_stanza:iq_result(DiscoRequest, [QueryEl]),
    escalus:send(Client, DiscoResult).

feature_elems() ->
    NodeNs = ?PEP_NODE_NS,
    [#xmlel{name = <<"identity">>,
        attrs = [{<<"category">>, <<"client">>},
            {<<"name">>, <<"Psi">>},
            {<<"type">>, <<"pc">>}]},
        #xmlel{name = <<"feature">>,
            attrs = [{<<"var">>, <<"http://jabber.org/protocol/disco#info">>}]},
        #xmlel{name = <<"feature">>,
            attrs = [{<<"var">>, NodeNs}]},
        #xmlel{name = <<"feature">>,
            attrs = [{<<"var">>, <<NodeNs/bitstring, "+notify">>}]}].

%%------------------------------------------------------------------------------------------------
%% Stanza helpers
%%------------------------------------------------------------------------------------------------

iq_id(Type, Client) ->
    UserName = escalus_utils:get_username(Client),
    Suffix = random_suffix(),
    list_to_binary(io_lib:format("~s-~s-~p",
        [Type, UserName, Suffix])).

random_suffix() ->
    Suffix = base64:encode(crypto:strong_rand_bytes(5)),
    re:replace(Suffix, "/", "_", [global, {return, binary}]).

get_sender_bare_jid(Stanza) ->
    From = exml_query:attr(Stanza, <<"from">>),
    [BareJid | _] = binary:split(From, <<"/">>),
    BareJid.

%%------------------------------------------------------------------------------------------------
%% Config helpers
%%------------------------------------------------------------------------------------------------
dump_settings(Settings) ->
    lager:info("scenario settings: ~p", [Settings]).

store_scenario_settings(Settings) ->
    erlang:put(scenario_settings, Settings).

get_parameter(Name) ->
    case erlang:get(scenario_settings) of
        undefined ->
            lager:error("get_parameter/1 failed, no scenario settings"),
            exit(no_settings);
        Settings ->
            case amoc_config:get_scenario_parameter(Name, Settings) of
                {error, no_parameter} ->
                    lager:error("get_parameter/1 failed, no such parameter"),
                    exit(no_parameter);
                {ok, Value} -> Value
            end
    end.

get_parameter(Name, Settings) ->
    case amoc_config:get_scenario_parameter(Name, Settings) of
        {error, no_parameter} ->
            lager:error("get_parameter/2 failed, no such parameter"),
            exit(no_parameter);
        {ok, Value} -> Value
    end.