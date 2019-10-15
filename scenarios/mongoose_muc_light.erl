-module(mongoose_muc_light).

-behaviour(amoc_scenario).

-export([init/0,
         start/1]).

-include_lib("exml/include/exml.hrl").

-spec init() -> ok.
init() ->
    lager:info("init the scenario"),
    amoc_metrics:init(counters, muc_rooms_created),
    amoc_metrics:init(counters, muc_occupants),
    amoc_metrics:init(counters, muc_messages_sent),
    amoc_metrics:init(counters, muc_messages_received),
    amoc_metrics:init(counters, muc_notifications_received),
    amoc_metrics:init(counters, timeouts),
    amoc_metrics:init(times, response),
    amoc_metrics:init(times, muc_message_ttd),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(Id) ->
    {ok, Client, _Spec} = amoc_xmpp:connect_or_exit(Id, extra_user_spec()),
    send_presence_available(Client),
    RoomsToCreate = amoc_xmpp_muc:rooms_to_create(Id, cfg(rooms_per_user), cfg(users_per_room)),
    create_rooms(Client, Id, RoomsToCreate),

    escalus_connection:wait(Client, cfg(delay_before_sending_messages)),

    RoomJids = [room_jid(RoomId) || RoomId <- RoomsToCreate],
    send_messages(Client, my_timetable(RoomJids), 0),

    escalus_connection:wait_forever(Client).

extra_user_spec() ->
    [{sent_stanza_handlers, sent_stanza_handlers()},
     {received_stanza_handlers, received_stanza_handlers()}].

send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    Pred = fun(Stanza) ->
                   escalus_pred:is_presence_with_type(<<"available">>, Stanza)
                       andalso escalus_pred:is_stanza_from(Client, Stanza)
           end,
    amoc_xmpp:send_request_and_get_response(Client, Pres, Pred, response, 10000).

create_rooms(_Client, _Id, []) -> ok;
create_rooms(Client, Id, [RoomId | Rest]) ->
    create_room(Client, Id, RoomId),
    escalus_connection:wait(Client, cfg(delay_after_creating_room)),
    create_rooms(Client, Id, Rest).

create_room(Client, CreatorId, RoomId) ->
    MemberIds = amoc_xmpp_muc:room_members(CreatorId, cfg(users_per_room)),
    MemberJids = [amoc_xmpp_users:make_jid(MemberId) || MemberId <- MemberIds],
    Req = stanza_create_room(RoomId, MemberJids),
    amoc_xmpp:send_request_and_get_response(
      Client, Req, fun(Stanza) -> escalus_pred:is_iq_result(Req, Stanza) end, response, 10000),
    amoc_metrics:update_counter(muc_rooms_created).

send_messages(Client, [{Time, MessageType} | TimeTable], TimePassed) ->
    TimeDiff = max(0, Time - TimePassed),
    escalus_connection:wait(Client, TimeDiff),
    send_message(Client, MessageType),
    send_messages(Client, TimeTable, TimePassed + TimeDiff);
send_messages(_Client, [], _TimePassed) -> ok.

send_message(Client, {muc_message, RoomJid}) ->
    escalus_connection:send(Client, message_to_room(RoomJid)).

message_to_room(RoomJid) ->
    Timestamp = integer_to_binary(os:system_time(microsecond)),
    escalus_stanza:groupchat_to(RoomJid, Timestamp).

my_timetable(RoomJidsToSend) ->
    lists:merge([room_message_timetable(RoomJid) || RoomJid <- RoomJidsToSend]).

room_message_timetable(RoomJid) ->
    Count = cfg(messages_to_send_per_room),
    Interval = cfg(message_interval_per_room),
    Offset = rand:uniform(Interval),
    timetable({muc_message, RoomJid}, Count, Interval, Offset).

timetable(Event, Count, Interval, Offset) ->
    [{Interval * I + Offset, Event} || I <- lists:seq(0, Count - 1)].

stanza_create_room(RoomId, MemberJids) ->
    ConfigFields = [kv_el(<<"roomname">>, room_name(RoomId))],
    ConfigElement = #xmlel{name = <<"configuration">>, children = ConfigFields},
    UserFields = [user_element(Jid, <<"member">>) || Jid <- MemberJids],
    OccupantsElement = #xmlel{name = <<"occupants">>, children = UserFields},
    escalus_stanza:to(escalus_stanza:iq_set(ns(muc_light_create), [ConfigElement, OccupantsElement]),
                      room_jid(RoomId)).

user_element(Jid, Aff) ->
    #xmlel{name = <<"user">>,
           attrs = [{<<"affiliation">>, Aff}],
           children = [#xmlcdata{content = Jid}]}.

kv_el(K, V) ->
    #xmlel{name = K,
           children = [#xmlcdata{content = V}]}.

%% Handlers

sent_stanza_handlers() ->
    amoc_xmpp_handlers:stanza_handlers(
      [{fun is_muc_message/1,
        fun() -> amoc_metrics:update_counter(muc_messages_sent) end}]).

received_stanza_handlers() ->
    amoc_xmpp_handlers:stanza_handlers(
      [{fun is_muc_notification/1,
        fun() -> amoc_metrics:update_counter(muc_notifications_received) end},
       {fun is_muc_message/1,
        fun(_, Stanza, Metadata) ->
                amoc_metrics:update_counter(muc_messages_received),
                amoc_metrics:update_time(muc_message_ttd, ttd(Stanza, Metadata))
        end},
       {fun(_) -> true end,
        fun(_, Stanza) -> lager:warning("Skipping received stanza ~p", [Stanza]) end}]).

%% Predicates

is_muc_notification(Message = #xmlel{name = <<"message">>}) ->
    exml_query:attr(Message, <<"type">>) =:= <<"groupchat">>
        andalso lists:member(exml_query:path(Message, [{element, <<"x">>}, {attr, <<"xmlns">>}]),
                             [ns(muc_light_affiliations),
                              ns(muc_light_configuration)]);
is_muc_notification(_) -> false.

is_muc_message(Stanza = #xmlel{name = <<"message">>}) ->
    exml_query:attr(Stanza, <<"type">>) =:= <<"groupchat">>;
is_muc_message(_) -> false.

%% Helpers

ttd(Stanza, #{recv_timestamp := Recv}) ->
    SentBin = exml_query:path(Stanza, [{element, <<"body">>}, cdata]),
    Recv - binary_to_integer(SentBin).

cfg(Name) ->
    amoc_config:get(Name, default_cfg(Name)).

room_jid(RoomId) ->
    <<(room_name(RoomId))/binary, $@, (muc_host())/binary>>.

room_name(RoomId) ->
    <<"room_", (integer_to_binary(RoomId))/binary>>.

muc_host() ->
    <<"muclight.localhost">>.

ns(muc_light_create) -> <<"urn:xmpp:muclight:0#create">>;
ns(muc_light_affiliations) -> <<"urn:xmpp:muclight:0#affiliations">>;
ns(muc_light_configuration) -> <<"urn:xmpp:muclight:0#configuration">>.

default_cfg(delay_after_creating_room) -> 10000;
default_cfg(delay_before_sending_messages) -> 0;
default_cfg(messages_to_send_per_room) -> 1000;
default_cfg(message_interval_per_room) -> 1000;
default_cfg(rooms_per_user) -> 10;
default_cfg(users_per_room) -> 20.
