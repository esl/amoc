-module(mongoose_gdpr_remove).

-behavior(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-required_variable({'IQ_TIMEOUT',         <<"IQ timeout (milliseconds, def: 10000ms)"/utf8>>}).
-required_variable({'ROOM_CREATION_RATE', <<"Rate of room creations (per minute, def:600)">>}).
-required_variable({'NODE_CREATION_RATE', <<"Rate of node creations (per minute, def:600)">>}).
-required_variable({'ROOM_PUBLICATION_RATE', <<"Rate of publications to room (per minute, def:1500)">>}).
-required_variable({'NODE_PUBLICATION_RATE', <<"Rate of publications to PEP node (per minute, def:1500)">>}).
-required_variable({'ROOM_SIZE', <<"Number of users in a room.">>}).
-required_variable({'N_OF_SUBSCRIBERS', <<"Number of subscriptions for each node (def: 50)"/utf8>>}).
-required_variable({'ROOM_ACTIVATION_POLICY', <<"Publish after setup of (def: all_rooms | n_sers)"/utf8>>}).
-required_variable({'NODE_ACTIVATION_POLICY', <<"Publish after setup of (def: all_nodes | n_nodes)"/utf8>>}).
-required_variable({'PUBLICATION_SIZE', <<"Size of additional payload (bytes, def:300)">>}).
-required_variable({'MIM_HOST', <<"The virtual host served by the server (def: <<\"localhost\">>)"/utf8>>}).
-required_variable({'MUC_HOST', <<"The virtual MUC host served by the server (def: <<\"muclight.localhost\">>)"/utf8>>}).

-define(ALL_PARAMETERS, [
    {iq_timeout,         'IQ_TIMEOUT',                    10000, positive_integer},
    {room_creation_rate, 'ROOM_CREATION_RATE', 600, positive_integer},
    {node_creation_rate, 'NODE_CREATION_RATE', 600, positive_integer},
    {room_publication_rate, 'ROOM_PUBLICATION_RATE', 1500, positive_integer},
    {node_publication_rate, 'NODE_PUBLICATION_RATE', 1500, positive_integer},
    {room_size, 'ROOM_SIZE', 10, positive_integer},
    %TODO n_of_subs was 50
    {n_of_subscribers, 'N_OF_SUBSCRIBERS', 5, nonnegative_integer},
    {room_activation_policy, 'ROOM_ACTIVATION_POLICY', all_rooms, [all_rooms, n_rooms]},
    {node_activation_policy, 'NODE_ACTIVATION_POLICY', all_nodes, [all_nodes, n_nodes]},
    {publication_size, 'PUBLICATION_SIZE', 300, nonnegative_integer},
    {mim_host, 'MIM_HOST', <<"localhost">>, bitstring},
    {muc_host, 'MUC_HOST', <<"muclight.localhost">>, bitstring}
]).

-define(ROOM_CREATION_THROTTLING, room_creation).
-define(PUBLICATION_THROTTLING, publication).
-define(NODE_CREATION_THROTTLING, node_creation).

-define(ROOM_CREATION_ID, <<"room_creation_id">>).
-define(NODE_CREATION_ID, <<"node_creation_id">>).

-define(PEP_NODE_NS, <<"just_some_random_namespace">>).
-define(CAPS_HASH, <<"erNmVoMSwRBR4brUU/inYQ5NFr0=">>). %% mod_caps:make_disco_hash(feature_elems(), sha1).
-define(NODE, {pep, ?PEP_NODE_NS}).

-type client() :: #client{}.
-type plan() :: [{EveryNClients :: pos_integer() | all,
    DoFun :: fun(([{Pid :: pid(), Client :: client()}]) -> any())}].

-export([init/0, start/2]).

-spec init() -> {ok, amoc_scenario:state()} | {error, Reason :: term()}.
init() ->
    init_metrics(),
    case config:get_scenario_settings(?ALL_PARAMETERS) of
        {ok, Settings} ->
            config:store_scenario_settings(Settings), config:dump_settings(),

            [PublicationRate, RoomCreationRate] = [proplists:get_value(Key, Settings) ||
                Key <- [room_publication_rate, room_creation_rate]],
            amoc_throttle:start(?ROOM_CREATION_THROTTLING, RoomCreationRate),
            amoc_throttle:start(?PUBLICATION_THROTTLING, PublicationRate),

            start_coordinator(Settings),
            {ok, Settings};
        Error -> Error
    end.

-spec start(amoc_scenario:user_id(), amoc_scenario:state()) -> any().
start(Id, Settings) ->
    config:store_scenario_settings(Settings),
    Client = connect_amoc_user(Id),
    start_user(Client).

init_metrics() ->
    Counters = [message_received, room, message_published],
    Times = [room_creation, publication_interval, message_ttd],
    [amoc_metrics:init(counters, Metric) || Metric <- Counters],
    [amoc_metrics:init(times, Metric) || Metric <- Times].

%%------------------------------------------------------------------------------------------------
%% Coordinator
%%------------------------------------------------------------------------------------------------
start_coordinator(Settings) ->
    Plan = get_plan(),
    coordinator:start_link(Plan, Settings).

node_activate_users(Pids, ActivationPolicy) ->
    case get_parameter(node_activation_policy) of
        ActivationPolicy ->
            [schedule_publishing(Pid)||Pid<-Pids];
        _ -> ok
    end.

room_activate_users(Pids, ActivationPolicy) ->
    case get_parameter(room_activation_policy) of
        ActivationPolicy ->
            [schedule_publishing(Pid)||Pid<-Pids];
        _ -> ok
    end.

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

%%------------------------------------------------------------------------------------------------
%% User
%%------------------------------------------------------------------------------------------------
start_user(Client) ->
    lager:debug("User process ~p", [self()]),
    erlang:monitor(process, Client#client.rcv_pid),
    %create_new_node(Client),
    set_presence(Client),
    escalus_tcp:set_active(Client#client.rcv_pid, true),
    request_muc_light_room(Client),
    user_loop(Client, #{}, 0).

set_presence(Client) ->
    send_presence_with_caps(Client),
    % waiting for presence sent to self
    escalus:wait_for_stanza(Client).

create_new_node(Client) ->
    amoc_throttle:send_and_wait(?NODE_CREATION_THROTTLING, create_node),
    create_pubsub_node(Client).

send_info_to_coordinator(Client) ->
    lager:debug("Process ~p, sending info about myself to coordinator", [self()]),
    coordinator:register_user(Client).

user_loop(Client, Requests, Time) ->
    IqTimeout = get_parameter(iq_timeout),
    receive
        publish_item ->
            % publish to own room
            % TODO publish pubsub item, it should also put stuff in "Requests"
            amoc_metrics:update_counter(message_published, 1),
            Now = os:system_time(microsecond),
            send_message_to_room(Client),
            update_publication_interval(Time, Now),
            user_loop(Client, Requests, Now);
        {publish_item, RoomJid} ->
            % publish to room that client is a member of
            amoc_metrics:update_counter(message_published, 1),
            Now = os:system_time(microsecond),
            send_message_to_room(Client, RoomJid),
            update_publication_interval(Time, Now),
            user_loop(Client, Requests, Now);
        {stanza, _, #xmlel{name = <<"message">>} = Stanza, #{recv_timestamp := TimeStamp}} ->
            process_message(Stanza, TimeStamp),
            user_loop(Client, Requests, Time);
        {stanza, _, #xmlel{name = <<"iq">>} = Stanza, #{recv_timestamp := TimeStamp}} ->
            NewRequests = process_iq(Client, Stanza, TimeStamp, Requests),
            user_loop(Client, NewRequests, Time);
        {stanza, _, #xmlel{name = <<"presence">>} = Stanza, _} ->
            process_presence(Client, Stanza),
            user_loop(Client, Requests, Time);
        {add_users, MemberJids} ->
            add_users_to_room(Client, MemberJids),
            user_loop(Client, Requests, Time)
    after IqTimeout ->
        user_loop(Client, verify_request(Requests), Time)
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
    amoc_metrics:update_counter(publication_timeout, 1);
update_timeout_metrics(Id) ->
    lager:error("unknown iq id ~p", Id).

schedule_publishing(Pid, RoomJid) ->
    amoc_throttle:send(?PUBLICATION_THROTTLING, Pid, {publish_item, RoomJid}).
schedule_publishing(Pid) ->
    amoc_throttle:send(?PUBLICATION_THROTTLING, Pid, publish_item).

update_publication_interval(0, _Now) ->
    ok;
update_publication_interval(Time, Now) ->
    Interval = Now - Time,
    amoc_metrics:update_time(publication_interval, Interval).

update_message_ttd(0, _Now) ->
    ok;
update_message_ttd(Time, Now) ->
    Interval = Now - Time,
    amoc_metrics:update_time(message_ttd, Interval).

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
            lager:debug("node creation ~p (~p)", [?NODE, self()]),
            amoc_metrics:update_counter(node_creation_success, 1),
            amoc_metrics:update_time(node_creation, CreateNodeTime);
        {false,{'EXIT',{timeout_when_waiting_for_stanza,_}}}->
            amoc_metrics:update_counter(node_creation_timeout, 1),
            lager:error("Timeout creating node: ~p", [CreateNodeResult]),
            exit(node_creation_timeout);
        {false, _} ->
            amoc_metrics:update_counter(node_creation_failure, 1),
            lager:error("Error creating node: ~p", [CreateNodeResult]),
            exit(node_creation_failed)
    end.

%%------------------------------------------------------------------------------------------------
%% User presence & caps
%%------------------------------------------------------------------------------------------------
send_presence(From, Type, To) ->
    ToJid = escalus_client:short_jid(To),
    Presence = escalus_stanza:presence_direct(ToJid, Type),
    escalus_client:send(From, Presence).

send_presence_with_caps(Client) ->
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

    MucHost = get_parameter(muc_host),
    CreateRoomStanza = escalus_stanza:iq_set(<<"urn:xmpp:muclight:0#create">>, []),
    CreateRoomStanzaWithTo = escalus_stanza:to(CreateRoomStanza, MucHost),
    CreateRoomStanzaWithId = escalus_stanza:set_id(CreateRoomStanzaWithTo, ?ROOM_CREATION_ID),
    lager:debug("Create stanza: ~p~n", [CreateRoomStanzaWithId]),

    escalus:send(Client, CreateRoomStanzaWithId).

%%------------------------------------------------------------------------------------------------
%% Room affiliation change
%%------------------------------------------------------------------------------------------------
add_users_to_room(Client, Jids) ->
    RoomJid = erlang:get(room_jid),
    AffList = [#xmlel{name = <<"user">>,
        attrs = [{<<"affiliation">>, <<"member">>}],
        children = [#xmlcdata{content = Jid}]} || Jid <- Jids],
    AffChangeStanza = escalus_stanza:iq_set(<<"urn:xmpp:muclight:0#affiliations">>, AffList),
    lager:debug("Adding users: ~p", [Jids]),
    escalus:send(Client, escalus_stanza:to(AffChangeStanza, RoomJid)).

%%------------------------------------------------------------------------------------------------
%% Sending messages
%%------------------------------------------------------------------------------------------------
send_message_to_room(Client) ->
    RoomJid = erlang:get(room_jid),
    send_message_to_room(Client, RoomJid).
send_message_to_room(Client, RoomJid) ->
    MessageBody = io_lib:format("Hello from ~p", [self()]),
    Message = #xmlel{name = <<"message">>,
        attrs = [{<<"to">>, RoomJid},
            {<<"type">>, <<"groupchat">>}],
        children = [#xmlcdata{content = MessageBody}]},
    escalus:send(Client, Message).


%%------------------------------------------------------------------------------------------------
%% Item publishing
%%------------------------------------------------------------------------------------------------

publish_pubsub_stanza(Client, Id, Content) ->
    ItemId = <<"current">>,
    escalus_pubsub_stanza:publish(Client, ItemId, Content, Id, ?NODE).

%%------------------------------------------------------------------------------------------------
%% Item processing
%%------------------------------------------------------------------------------------------------

process_message(Stanza, TimeStamp) ->
    %TODO process normal message
    case exml_query:attr(Stanza, <<"id">>) of
        ?ROOM_CREATION_ID -> process_affiliation_message(Stanza);
        _ -> process_normal_message_to_room(Stanza, TimeStamp)
    end.

process_affiliation_message(Stanza) ->
    RoomJid = exml_query:attr(Stanza, <<"from">>),
    erlang:put(room_jid, RoomJid).

process_normal_message_to_room(Stanza, TimeStamp) ->
    Now = os:system_time(microsecond),
    BareJid = get_sender_bare_jid(Stanza),
    schedule_publishing(self(), BareJid),
    amoc_metrics:update_counter(message_received, 1),
    update_message_ttd(TimeStamp, Now).

process_presence(Client, Stanza) ->
    case exml_query:attr(Stanza, <<"type">>) of
        subscribe ->
            lager:debug("presence subscribe stanza ~p", [Stanza]),
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
        %TODO process IQs from adding users to rooms
        {<<"result">>, undefined, ?ROOM_CREATION_ID, undefined} ->
            %TODO maybe metric with room creation timeouts like requests timeouts?
            true = (undefined =/= erlang:get(room_jid)),
            send_info_to_coordinator(Client);
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


handle_publish_resp(PublishResult, {Tag, PublishTime}) ->
    %TODO read and make this work
    ok.
%%    IqTimeout = get_parameter(iq_timeout),
%%    case escalus_pred:is_iq_result(PublishResult) of
%%        true ->
%%            lager:debug("publish time ~p", [PublishTime]),
%%            amoc_metrics:update_counter(publication_result, 1),
%%            amoc_metrics:update_time(publication, PublishTime),
%%            case Tag of
%%                new when IqTimeout * 1000 > PublishTime ->
%%                    amoc_metrics:update_counter(publication_success, 1);
%%                new ->
%%                    amoc_metrics:update_counter(publication_timeout, 1);
%%                timeout -> ok %% do nothing, it's already reported as timeout
%%            end;
%%        _ ->
%%            amoc_metrics:update_counter(publication_error, 1),
%%            lager:error("Error publishing failed: ~p", [PublishResult]),
%%            exit(publication_failed)
%%    end.

handle_disco_query(Client, DiscoRequest) ->
    lager:debug("handle_disco_query ~p", [self()]),
    QueryEl = escalus_stanza:query_el(<<"http://jabber.org/protocol/disco#info">>,
        feature_elems()),
    DiscoResult = escalus_stanza:iq_result(DiscoRequest, [QueryEl]),
    escalus:send(Client, DiscoResult).

feature_elems() ->
    NodeNs = ?PEP_NODE_NS,
    [#xmlel{name  = <<"identity">>,
        attrs = [{<<"category">>, <<"client">>},
            {<<"name">>, <<"Psi">>},
            {<<"type">>, <<"pc">>}]},
        #xmlel{name  = <<"feature">>,
            attrs = [{<<"var">>, <<"http://jabber.org/protocol/disco#info">>}]},
        #xmlel{name  = <<"feature">>,
            attrs = [{<<"var">>, NodeNs}]},
        #xmlel{name  = <<"feature">>,
            attrs = [{<<"var">>, <<NodeNs/bitstring, "+notify">>}]}].

%%------------------------------------------------------------------------------------------------
%% Stanza helpers
%%------------------------------------------------------------------------------------------------
get_sender_bare_jid(Stanza) ->
    From = exml_query:attr(Stanza, <<"from">>),
    [BareJid | _] = binary:split(From, <<"/">>),
    BareJid.

%%------------------------------------------------------------------------------------------------
%% Config helpers
%%------------------------------------------------------------------------------------------------
get_parameter(Name) ->
    case config:get_parameter(Name) of
        {error, Err} ->
            lager:error("config_get_parameter/1 failed ~p", [Err]),
            exit(Err);
        {ok, Value} -> Value
    end.

get_room_size() ->
    get_parameter(room_size).

-spec(get_plan() -> Plan :: plan()).
get_plan() ->
    [{get_room_size(), fun(PidsAndClients) ->
        make_full_rooms(PidsAndClients),
        room_activate_users(pids(PidsAndClients), n_rooms) end},
        {get_parameter(n_of_subscribers), fun(PidsAndClients) ->
            make_all_clients_friends(clients(PidsAndClients)),
            node_activate_users(pids(PidsAndClients), n_nodes) end},
        {all, fun(PidsAndClients) ->
            room_activate_users(pids(PidsAndClients), all_rooms),
            node_activate_users(pids(PidsAndClients), all_nodes) end}].

print_execute(PidsAndClients) ->
    lager:debug("Executing plan."),
    [lager:debug("Pid: ~p, client: ~p~n", [Pid, Client]) || {Pid, Client} <- PidsAndClients].

clients(PidsAndClients) ->
    {_Pids, Clients} = lists:unzip(PidsAndClients),
    Clients.

pids(PidsAndClients) ->
    {Pids, _Clients} = lists:unzip(PidsAndClients),
    Pids.