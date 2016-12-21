%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%
%% In this scenarion users are reading last 10 msgs from archive and
%% sending message to its neighbours
%% (users wiht lower and grater idea defined by NUMBER_OF_*_NEIGHBOURS values)
%% Messages will be send NUMBER_OF_MESSAGES_PER_USER to every selected neighbour
%% after every message given the script will wait SLEEP_TIME_AFTER_EVERY_MESSAGE ms
%% Every CHECKER_SESSIONS_INDICATOR is a checker session which just measures message TTD
%%
%%==============================================================================
-module(mongoose_mam_load_test).

-behaviour(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-type binjid() :: binary().

-define(HOST, <<"localhost">>). %% The virtual host served by the server
-define(SERVER_IPS, {<<"127.0.0.1">>}). %% Tuple of servers, for example {<<"10.100.0.21">>, <<"10.100.0.22">>}

%% scenario behavior
-export([start/1]).
-export([init/0]).


message_counter(Type) -> [amoc, counters, Type, message_counter].
flush_duration(Type) -> [amoc, times, Type, flush_duration].
rand_access_duration(Type) -> [amoc, times, Type, random_access_query_duration].
paging_duration(Type) -> [amoc, times, Type, paging_query_duration].
last_page_duration(Type) -> [amoc, times, Type, last_page_query_duration].
errors_per_second() -> [amoc, counters, errors_per_second].


intermessage_time() -> amoc_config:get(intermessage_time, 100).
messages_per_user() -> amoc_config:get(messages_per_user, 1000).
backoff_base() -> amoc_config:get(backoff_base, 10).
backoff_cap() -> amoc_config:get(backoff_cap, timer:minutes(1)).
request_simple() -> amoc_config:get(request_simple, true).


-spec init() -> ok.
init() ->
    register(test_master, spawn_link(fun() -> test_master([], 0, [], 0) end)),

    exometer:new(errors_per_second(), spiral, [{time_span, timer:seconds(1)}]),
    exometer_report:subscribe(exometer_report_graphite, errors_per_second(), [one], 10000),

    lists:foreach(
      fun(Type) ->
              exometer:new(message_counter(Type), counter),
              exometer_report:subscribe(exometer_report_graphite, message_counter(Type), [value], 10000),

              lists:foreach(
                fun(Metric) ->
                        exometer:new(Metric, histogram, [{time_span, timer:seconds(5)}]),
                        exometer_report:subscribe(exometer_report_graphite, Metric, [mean, max, min, median, 95, 99], 10000)
                end,
                [flush_duration(Type), rand_access_duration(Type), paging_duration(Type), last_page_duration(Type)])
      end,
      [pm, muc]).


-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    lager:info("Starting user ~p", [MyId]),

    Client = connect(MyId),
    Room = room_name(MyId),

    escalus_connection:set_filter_predicate(Client, fun(_) -> false end),

    send_presence_available(Client),
    join_muc(Client, MyId, Room),
    initialize_muc(Client, MyId, Room),

    case register() of
        ok ->
            ok;
        new ->
            timer:sleep(timer:seconds(10))
    end,

    try
        [test(Client, R) || R <- [undefined, Room]]
    catch
        Error:Reason ->
            unregister(),
            erlang:raise(Error, Reason, erlang:get_stacktrace())
    end,

    send_presence_unavailable(Client),
    leave_muc(Client, MyId, Room),
    ok.


test(Client, Room) ->
    populate_mam(Client, Room),

    lists:foreach(
      fun({Fun, Repeats}) ->
              wait_for_all_users(),
              [Fun(Client, Room) || _ <- lists:seq(1, Repeats)]
      end,
      [
       {fun probe/2, 1},
       {fun random_access/2, 50},
       {fun last_page/2, 50},
       {fun paging/2, 1}
      ]).


test_master(UserSet, UserCount, WaitingList, UserCount) when UserCount =/= 0 ->
    Interarrival = amoc_config:get(interarrival, 50),
    lists:foldl(
      fun(Pid, Delay) ->
              erlang:send_after(Delay, Pid, resume),
              Delay + Interarrival
      end,
      0,
      WaitingList),
    test_master(UserSet, UserCount, [], 0);

test_master(UserSet, UserCount, WaitingList, WaitingCount) ->
    receive
        {register, Pid} ->
            NewUserSet = ordsets:add_element(Pid, UserSet),
            case NewUserSet of
                UserSet -> Pid ! ok;
                _ -> Pid ! new
            end,
            test_master(NewUserSet, ordsets:size(NewUserSet), WaitingList, WaitingCount);

        {unregister, Pid} ->
            NewUserSet = ordsets:del_element(Pid, UserSet),
            test_master(NewUserSet, ordsets:size(NewUserSet), WaitingList, WaitingCount);

        {wait_for_all, Pid} ->
            test_master(UserSet, UserCount, [Pid | WaitingList], WaitingCount + 1);

        {usercount, Pid} ->
            Pid ! {usercount, UserCount},
            test_master(UserSet, UserCount, WaitingList, WaitingCount)
    end.


-spec user_spec(binary(), binary(), binary()) -> escalus_users:user_spec().
user_spec(ProfileId, Password, Res) ->
    [
     {username, ProfileId},
     {server, ?HOST},
     {host, pick_server(?SERVER_IPS)},
     {password, Password},
     {carbons, false},
     {stream_management, false},
     {resource, Res}
    ].


-spec make_user(amoc_scenario:user_id(), binary()) -> escalus_users:user_spec().
make_user(Id, R) ->
    BinId = integer_to_binary(Id),
    ProfileId = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    user_spec(ProfileId, Password, R).


usercount() ->
    test_master ! {usercount, self()},
    receive
        {usercount, Val} -> Val
    after
        timer:seconds(5) -> error(timeout)
    end.


wait_for_all_users() ->
    test_master ! {wait_for_all, self()},
    receive
        resume -> ok
    after
        timer:minutes(5) -> error(timeout)
    end.


register() ->
    test_master ! {register, self()},
    receive
        Result -> Result
    after
        timer:minutes(1) -> error(timeout)
    end.


unregister() ->
    test_master ! {unregister, self()}.


connect(MyId) ->
    Cfg = make_user(MyId, <<"res1">>),
    case escalus_connection:start(Cfg) of
        {ok, ConnectedClient, _, _} ->
            ConnectedClient;

        Error ->
            lager:error("Could not connect user=~p, reason=~p", [Cfg, Error]),
            exit(connection_failed)
    end.


room_name(MyId) ->
    <<"room_", (integer_to_binary((MyId - 1) div 10))/binary>>.


room_address(Room) ->
    <<Room/binary, "@muc.", ?HOST/binary>>.

room_address(Room, Nick) ->
    <<Room/binary, "@muc.", ?HOST/binary, "/", Nick/binary>>.

stanza_to_room(Stanza, Room) ->
    escalus_stanza:to(Stanza, room_address(Room)).

stanza_to_room(Stanza, Room, Nick) ->
    escalus_stanza:to(Stanza, room_address(Room, Nick)).

stanza_muc_enter_room(Room, Nick) ->
    Stanza = escalus_stanza:presence(
               <<"available">>,
               [#xmlel{ name = <<"x">>, attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]}]),
    stanza_to_room(Stanza, Room, Nick).

stanza_instant_room(Room) ->
    X = #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_DATA_FORMS}, {<<"type">>, <<"submit">>}]},
    stanza_to_room(escalus_stanza:iq_set(?NS_MUC_OWNER, [X]), Room).


initialize_muc(Client, MyId, Room) ->
    escalus:send(Client, escalus_stanza:setattr(stanza_instant_room(Room), <<"from">>, make_jid(MyId))).


join_muc(Client, MyId, Room) ->
    escalus:send(Client, stanza_muc_enter_room(Room, make_nick(MyId))).


leave_muc(Client, MyId, Room) ->
    escalus:send(Client, stanza_to_room(escalus_stanza:presence(<<"unavailable">>), Room, make_nick(MyId))).


create_mam_request(RSM, undefined) ->
    create_mam_request(RSM);
create_mam_request(RSM, Room) ->
    stanza_to_room(create_mam_request(RSM), Room).

create_mam_request(RSM) ->
    escalus_stanza:mam_lookup_messages_iq(
      <<"QueryID">>, undefined, undefined, undefined, RSM, request_simple()).


neighbour_jids(Count) ->
    UserCount = usercount(),
    [make_jid(rand:uniform(UserCount)) || _ <- lists:seq(1, Count)].


populate_mam(Client, undefined) ->
    NeighbourJids = neighbour_jids(messages_per_user()),
    [send_message(Client, {jid, Jid}) || Jid <- NeighbourJids];
populate_mam(Client, Room) ->
    [send_message(Client, {room, Room}) || _ <- lists:seq(1, messages_per_user())].


query(Client, Query) ->
    query(Client, Query, 0, 1).

query(Client, Query, TotalDuration, MaxBackoff) ->
    escalus_connection:set_filter_predicate(
      Client,
      fun(Stanza) ->
              escalus_pred:is_mam_fin_message(Stanza) orelse
                  escalus_pred:is_iq_error(Stanza) orelse
                  escalus_pred:is_message(Stanza)
      end),

    SendTime = erlang:monotonic_time(),
    escalus_connection:send(Client, Query),
    {Stanza, Count} = get_fin_message(Client, 0),
    ResultTime = erlang:monotonic_time(),

    First = exml_query:path(Stanza,
                            [
                             {element, <<"fin">>},
                             {element, <<"set">>},
                             {element, <<"first">>},
                             cdata
                            ]),

    escalus_connection:set_filter_predicate(Client, fun(_) -> false end),

    Duration = TotalDuration + ResultTime - SendTime,

    case escalus_pred:is_iq_error(Stanza) of
        true ->
            lager:error("Lookup error: ~s", [exml:to_list(Stanza)]),
            update_errors(),
            timer:sleep(rand:uniform(MaxBackoff)),
            NextBackoff = min(MaxBackoff * backoff_base(), backoff_cap()),
            query(Client, Query, Duration, NextBackoff);

        false ->
            {erlang:convert_time_unit(Duration, native, milli_seconds), Count, First}
    end.


get_fin_message(Client, Count) ->
    Stanza = escalus_connection:get_stanza(Client, mam_result, timer:minutes(5)),
    case escalus_pred:is_mam_fin_message(Stanza) orelse escalus_pred:is_iq_error(Stanza) of
        true -> {Stanza, Count};
        false ->
            case escalus_pred:is_message(Stanza) of
                true ->
                    case exml_query:path(Stanza, [{element, <<"result">>}, {element, <<"forwarded">>}]) of
                        undefined -> get_fin_message(Client, Count);
                        _ -> get_fin_message(Client, Count + 1)
                    end;

                false ->
                    get_fin_message(Client, Count)
            end
    end.


update_flush(undefined, Duration) ->
    exometer:update(flush_duration(pm), Duration);
update_flush(_, Duration) ->
    exometer:update(flush_duration(muc), Duration).

update_rand(undefined, Duration) ->
    exometer:update(rand_access_duration(pm), Duration);
update_rand(_, Duration) ->
    exometer:update(rand_access_duration(muc), Duration).

update_last_page(undefined, Duration) ->
    exometer:update(last_page_duration(pm), Duration);
update_last_page(_, Duration) ->
    exometer:update(last_page_duration(muc), Duration).

update_pagings(undefined, Duration) ->
    exometer:update(paging_duration(pm), Duration);
update_pagings(_, Duration) ->
    exometer:update(paging_duration(muc), Duration).

update_message_counter({jid, _}) ->
    exometer:update(message_counter(pm), 1);
update_message_counter({room, _}) ->
    exometer:update(message_counter(muc), 1).

update_errors() ->
    exometer:update(errors_per_second(), 1).

% term_to_metric_type(undefined) -> pm;
% term_to_metric_type({jid, _}) -> pm;
% term_to_metric_type({room, _}) -> muc;
% term_to_metric_type(_) -> muc;

read_message_counter(undefined) ->
    {ok, [{value, Messages}]} = exometer:get_value(message_counter(pm), value),
    Messages;
read_message_counter(_) ->
    {ok, [{value, Messages}]} = exometer:get_value(message_counter(muc), value),
    Messages.


probe(Client, Room) ->
    probe(Client, Room, 0).

probe(Client, Room, TotalDuration) ->
    Query = create_mam_request({before, <<>>, 10}, Room),
    case query(Client, Query) of
        {Duration, 10, _} -> update_flush(Room, TotalDuration + Duration);
        {Duration, _, _} -> probe(Client, Room, TotalDuration + Duration)
    end.


random_access(Client, Room) ->
    Messages = read_message_counter(Room) div usercount(),
    Index = rand:uniform(Messages),
    Query = create_mam_request({index, Index, 10}, Room),
    case query(Client, Query) of
        {_Duration, 0, _} -> ok;
        {Duration, _, _} -> update_rand(Room, Duration)
    end.


last_page(Client, Room) ->
    Query = create_mam_request({before, <<>>, 10}, Room),
    case query(Client, Query) of
        {_Duration, 0, _} -> ok;
        {Duration, _, _} -> update_last_page(Room, Duration)
    end.


paging(Client, Room) ->
    case read_pages(Client, Room, <<>>, 100, 0, 0) of
        {Duration, 100} -> update_pagings(Room, Duration);
        {_, _} -> ok
    end.


read_pages(_Client, _Room, _Before, 0, TotalDuration, Count) ->
    {TotalDuration, Count};
read_pages(Client, Room, Before, PagesLeft, TotalDuration, Count) ->
    Query = create_mam_request({before, Before, 1}, Room),
    case query(Client, Query) of
        {Duration, 0, _} ->
            {TotalDuration + Duration, Count};

        {Duration, 1, First} ->
            read_pages(Client, Room, First, PagesLeft - 1,
                       TotalDuration + Duration, Count + 1)
    end.


-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).


-spec send_presence_unavailable(escalus:client()) -> ok.
send_presence_unavailable(Client) ->
    Pres = escalus_stanza:presence(<<"unavailable">>),
    escalus_connection:send(Client, Pres).


-spec send_message(escalus:client(), binjid()) -> ok.
send_message(Client, ToId) ->
    escalus_connection:send(Client, make_message(ToId)),
    update_message_counter(ToId),
    timer:sleep(intermessage_time()).


-spec make_message(binjid()) -> exml:element().
make_message({jid, ToId}) ->
    Body = <<"hello sir, you are a gentelman and a scholar.">>,
    escalus_stanza:chat_to(ToId, Body);
make_message({room, Room}) ->
    Body = <<"hello sirs, you are all gentelmen and scholars.">>,
    escalus_stanza:groupchat_to(room_address(Room), Body).


make_nick(Id) ->
    <<"user_", (integer_to_binary(Id))/binary>>.


-spec make_jid(amoc_scenario:user_id()) -> binjid().
make_jid(Id) ->
    <<(make_nick(Id))/binary, "@", ?HOST/binary>>.


-spec pick_server({binary()}) -> binary().
pick_server(Servers) ->
    N = erlang:phash2(self(), size(Servers)) + 1,
    element(N, Servers).
