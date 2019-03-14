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
    amoc_metrics:init(counters, timeouts),
    amoc_metrics:init(times, response),
    amoc_metrics:init(times, muc_message_ttd),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(Id) ->
    {ok, Client, _Spec} = amoc_xmpp:connect_or_exit(Id, extra_user_spec()),
    escalus_session:send_presence_available(Client),

    RoomsToCreate = amoc_xmpp_muc:rooms_to_create(Id, rooms_per_user(), users_per_room()),
    create_rooms(Client, Id, RoomsToCreate),

    escalus_connection:wait(Client, delay_before_sending_messages()),

    RoomJids = [room_jid(RoomId) || RoomId <- RoomsToCreate],
    send_messages(Client, my_timetable(RoomJids), 0),

    escalus_connection:wait(Client, delay_after_sending_messages()).

extra_user_spec() ->
    [{sent_stanza_handlers, sent_stanza_handlers()},
     {received_stanza_handlers, received_stanza_handlers()}].

create_rooms(_Client, _Id, []) -> ok;
create_rooms(Client, Id, [RoomName | Rest]) ->
    create_room(Client, Id, RoomName),
    escalus_connection:wait(Client, delay_after_creating_room()),
    create_rooms(Client, Id, Rest).

create_room(Client, CreatorId, RoomName) ->
    MemberIds = amoc_xmpp_muc:room_members(CreatorId, users_per_room()),
    MemberJids = [amoc_xmpp_users:make_jid(MemberId) || MemberId <- MemberIds],
    Req = stanza_create_room(RoomName, MemberJids),
    amoc_xmpp:send_request_and_get_response(
      Client, Req, fun escalus_pred:is_iq_result/2, response, 10000),
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
    TimeStamp = integer_to_binary(os:system_time(microsecond)),
    escalus_stanza:message(<<"Hi!">>, #{type => <<"groupchat">>,
                                        to => RoomJid,
                                        timestamp => TimeStamp}).

my_timetable(RoomJidsToSend) ->
    lists:merge([room_message_timetable(RoomJid) || RoomJid <- RoomJidsToSend]).

room_message_timetable(RoomJid) ->
    Count = messages_to_send_per_room(),
    Interval = message_interval_per_room(),
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
      [{fun is_muc_message/1,
        fun(_, Stanza, Metadata) ->
                amoc_metrics:update_counter(muc_messages_received),
                amoc_metrics:update_time(muc_message_ttd, amoc_xmpp_handlers:ttd(Stanza, Metadata))
        end},
       {fun(_) -> true end,
        fun(_, Stanza) -> lager:warning("Skipping received stanza ~p", [Stanza]) end}]).

%% Predicates

is_muc_message(Stanza = #xmlel{name = <<"message">>}) ->
    exml_query:attr(Stanza, <<"type">>) =:= <<"groupchat">>;
is_muc_message(_) -> false.

%% Helpers

delay_after_creating_room() ->
    10000.

delay_before_sending_messages() ->
    10000.

messages_to_send_per_room() ->
    100.

message_interval_per_room() ->
    1000.

delay_after_sending_messages() ->
    60000.

rooms_per_user() ->
    20.

users_per_room() ->
    10.

room_jid(RoomId) ->
    <<(room_name(RoomId))/binary, $@, (muc_host())/binary>>.

room_name(RoomId) ->
    <<"room_", (integer_to_binary(RoomId))/binary>>.

muc_host() ->
    <<"muclight.localhost">>.

ns(muc_light_create) ->
    <<"urn:xmpp:muclight:0#create">>.
