-module(gdpr_removal).

-behavior(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-required_variable({'ROOM_SIZE', <<"Number of users in a room.">>}).
-required_variable({'ROOM_CREATION_RATE', <<"Rate of room creations (per minute, def:600)">>}).
-required_variable({'PUBLICATION_RATE', <<"Rate of publications (per minute, def:1500)">>}).
-required_variable({'ACTIVATION_POLICY', <<"Publish after subscribtion of (def: all_rooms | n_rooms)"/utf8>>}).
-required_variable({'MIM_HOST', <<"The virtual host served by the server (def: <<\"localhost\">>)"/utf8>>}).
-required_variable({'MUC_HOST', <<"The virtual MUC host served by the server (def: <<\"muclight.localhost\">>)"/utf8>>}).

-define(ALL_PARAMETERS, [
    {room_size, 'ROOM_SIZE', 5, positive_integer},
    {room_creation_rate, 'ROOM_CREATION_RATE', 600, positive_integer},
    {publication_rate, 'PUBLICATION_RATE', 1500, positive_integer},
    {activation_policy, 'ACTIVATION_POLICY', n_rooms, [all_rooms, n_rooms]},
    {mim_host, 'MIM_HOST', <<"localhost">>, bitstring},
    {muc_host, 'MUC_HOST', <<"muclight.localhost">>, bitstring}
]).

-define(GROUP_NAME, <<"coordinator">>).
-define(ROOM_CREATION_THROTTLING, room_creation).
-define(PUBLICATION_THROTTLING, publication).

-define(COORDINATOR_TIMEOUT, 100000).

-export([init/0, start/2]).

-spec init() -> {ok, amoc_scenario:state()} | {error, Reason :: term()}.
init() ->
    init_metrics(),
    case config:get_scenario_settings(?ALL_PARAMETERS) of
        {ok, Settings} ->
            config:store_scenario_settings(Settings), config:dump_settings(),

            [PublicationRate, RoomCreationRate] = [proplists:get_value(Key, Settings) ||
                Key <- [publication_rate, room_creation_rate]],
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
    pg2:create(?GROUP_NAME),
    Coordinator = spawn(fun() -> coordinator_fn(Settings) end),
    pg2:join(?GROUP_NAME, Coordinator).

coordinator_fn(Settings) ->
    lager:debug("coordinator process ~p", [self()]),
    config:store_scenario_settings(Settings),
    config:dump_settings(),
    coordinator_loop([]).

coordinator_loop(AllPids) ->
    N = get_room_size(),
    case wait_for_n_clients([], N) of
        {timeout, []} when AllPids =:= [] -> coordinator_loop([]);
        {timeout, Pids} ->
            activate_users(Pids, n_rooms),
            activate_users(Pids ++ AllPids, all_rooms),
            lager:error("Waited too long for the new user!"),
            coordinator_loop([]);
        {ok, Pids} ->
            activate_users(Pids, n_rooms),
            coordinator_loop(Pids ++ AllPids)
    end.

wait_for_n_clients(PidsAndJids, 0) ->
    make_full_rooms(PidsAndJids),
    Pids = [Pid || {Pid, _} <- PidsAndJids],
    {ok, Pids};
wait_for_n_clients(PidsAndJids, N) ->
    receive
        {new_client, NewPid, NewJid} ->
            lager:debug("Coordinator received ~p~n", [NewJid]),
            wait_for_n_clients([{NewPid, NewJid} | PidsAndJids], N - 1)
    after ?COORDINATOR_TIMEOUT ->
        Pids = [Pid || {Pid, _} <- PidsAndJids],
        make_full_rooms(PidsAndJids),
        {timeout, Pids}
    end.

make_full_rooms(PidsAndJids) ->
    [begin
         MemberJids = [Jid || {_, Jid} <- PidsAndJids, Jid =/= OwnerJid],
         OwnerPid ! {add_users, MemberJids}
     end || {OwnerPid, OwnerJid} <- PidsAndJids].

activate_users(Pids, ActivationPolicy) ->
    case get_parameter(activation_policy) of
        ActivationPolicy ->
            [schedule_publishing(Pid) || Pid <- Pids];
        _ -> ok
    end.

get_coordinator_pid() ->
    case pg2:get_members(?GROUP_NAME) of
        [Coordinator] -> Coordinator;
        _ -> %% [] or {error, {no_such_group, ?GROUP_NAME}}
            timer:sleep(100),
            get_coordinator_pid()
    end.

%%------------------------------------------------------------------------------------------------
%% User
%%------------------------------------------------------------------------------------------------
start_user(Client) ->
    lager:debug("User process ~p", [self()]),
    erlang:monitor(process, Client#client.rcv_pid),
    create_muc_light_room(Client),
    escalus_tcp:set_active(Client#client.rcv_pid, true),
    send_jid_and_pid_to_coordinator(Client),
    user_loop(Client, 0).

send_jid_and_pid_to_coordinator(Client) ->
    Coordinator = get_coordinator_pid(),
    lager:debug("Process ~p sending pid and jid to coordinator ~p", [self(), Coordinator]),
    Jid = escalus_client:short_jid(Client),
    Coordinator ! {new_client, self(), Jid}.

user_loop(Client, Time) ->
    receive
        publish_item ->
            RoomJid = get_room(),
            amoc_metrics:update_counter(message_published, 1),
            Now = os:system_time(microsecond),
            send_message_to_room(Client, RoomJid),
            schedule_publishing(self()),
            update_publication_interval(Time, Now),
            user_loop(Client, Now);
        {stanza, _, #xmlel{name = <<"message">>} = Stanza, #{recv_timestamp := TimeStamp}} ->
            case exml_query:subelement_with_name_and_ns(Stanza, <<"x">>, <<"urn:xmpp:muclight:0#affiliations">>) of
                undefined ->
                    Sent = get_muc_timestamp(Stanza),
                    amoc_metrics:update_counter(message_received, 1),
                    update_message_ttd(Sent, TimeStamp);
                _ -> %% affiliation stanza
                    RoomJid = exml_query:attr(Stanza, <<"from">>),
                    add_room(RoomJid)
            end,
            user_loop(Client, Time);

        {add_users, MemberJids} ->
            add_users_to_room(Client, MemberJids),
            user_loop(Client, Time)
    end.


schedule_publishing(Pid) ->
    amoc_throttle:send(?PUBLICATION_THROTTLING, Pid, publish_item).

update_publication_interval(0, _Now) ->
    ok;
update_publication_interval(Time, Now) ->
    Interval = Now - Time,
    amoc_metrics:update_time(publication_interval, Interval).

update_message_ttd(Time, Now) ->
    Interval = Now - Time,
    amoc_metrics:update_time(message_ttd, Interval).

%%------------------------------------------------------------------------------------------------
%% User connection
%%------------------------------------------------------------------------------------------------
connect_amoc_user(Id) ->
    Cfg = make_user_cfg(Id),
    {ok, Client, _} = escalus_connection:start(Cfg),
    send_presence_available(Client),
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
%% User presence
%%------------------------------------------------------------------------------------------------
-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus:send_and_wait(Client, Pres).

%%------------------------------------------------------------------------------------------------
%% Room creation
%%------------------------------------------------------------------------------------------------
create_muc_light_room(Client) ->
    amoc_throttle:send_and_wait(?ROOM_CREATION_THROTTLING, create_room),

    MucHost = get_parameter(muc_host),
    CreateRoomStanza = escalus_stanza:iq_set(<<"urn:xmpp:muclight:0#create">>, []),
    CreateRoomStanzaWithTo = escalus_stanza:to(CreateRoomStanza, MucHost),
    lager:debug("Create stanza: ~p~n", [CreateRoomStanzaWithTo]),

    {CreateRoomTime, AffMessage} = timer:tc(fun() -> escalus:send_and_wait(Client, CreateRoomStanzaWithTo) end),
    amoc_metrics:update_time(room_creation, CreateRoomTime),

    true = has_same_id(AffMessage, CreateRoomStanzaWithTo),

    RoomJid = exml_query:attr(AffMessage, <<"from">>),
    erlang:put(room_jid,RoomJid),
    add_room(RoomJid),

    IqResponse = escalus:wait_for_stanza(Client),
    lager:debug("Create room iq response: ~p~n", [IqResponse]),
    escalus:assert(is_iq_result, [CreateRoomStanza], IqResponse),

    amoc_metrics:update_counter(room, 1).

%%------------------------------------------------------------------------------------------------
%% Room affiliation change
%%------------------------------------------------------------------------------------------------
add_users_to_room(Client, Jids) ->
    RoomJid = erlang:get(room_jid),
    erlang:put(room_jid, RoomJid),
    AffList = [#xmlel{name = <<"user">>,
        attrs = [{<<"affiliation">>, <<"member">>}],
        children = [#xmlcdata{content = Jid}]} || Jid <- Jids],
    AffChangeStanza = escalus_stanza:iq_set(<<"urn:xmpp:muclight:0#affiliations">>, AffList),
    lager:debug("Adding users: ~p ~p", [RoomJid,Jids]),
    escalus:send(Client, escalus_stanza:to(AffChangeStanza, RoomJid)).

%%------------------------------------------------------------------------------------------------
%% Sending messages
%%------------------------------------------------------------------------------------------------
send_message_to_room(Client, RoomJid) ->
    TimeStamp = io_lib:format("~p", [os:system_time(microsecond)]),
    MessageBody = #xmlel{name     = <<"body">>,
                         children = [#xmlcdata{content = TimeStamp}]},
    Message = #xmlel{name     = <<"message">>,
                     attrs    = [{<<"to">>, RoomJid},
                                 {<<"type">>, <<"groupchat">>}],
                     children = [MessageBody]},
    escalus:send(Client, Message).

%%------------------------------------------------------------------------------------------------
%% Stanza helpers
%%------------------------------------------------------------------------------------------------
-spec has_same_id(exml:element(), exml:element()) -> boolean().
has_same_id(OrigStanza, Stanza) ->
    OrigId = exml_query:attr(OrigStanza, <<"id">>),
    Id = exml_query:attr(Stanza, <<"id">>),
    OrigId =:= Id.

-spec get_muc_timestamp(exml:element()) -> integer().
get_muc_timestamp(Stanza) ->
    TimeStampBin = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
    binary_to_integer(TimeStampBin).


%%------------------------------------------------------------------------------------------------
%% rooms management
%%------------------------------------------------------------------------------------------------
add_room(RoomJid) ->
    case erlang:get(rooms) of
        undefined ->
            erlang:put(rooms, [RoomJid]);
        Rooms ->
            case lists:member(RoomJid, Rooms) of
                true -> ok;
                _ -> erlang:put(rooms, [RoomJid | Rooms])
            end
    end.

get_room() ->
    case erlang:get(rooms) of
        undefined -> error(no_rooms);
        Rooms ->
            Size = length(Rooms),
            N = random:uniform(Size),
            lists:nth(N, Rooms)
    end.
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
