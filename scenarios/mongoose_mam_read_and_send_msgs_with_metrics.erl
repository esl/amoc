%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%
%% In this scenarion users are reading last 10 msgs from archive and
%% sending message to its neighbours
%% (users wiht lower and grater idea defined by NUMBER_OF_*_NEIGHBOURS values)
%% Messages will be send NUMBER_OF_SEND_MESSAGE_REPEATS to every selected neighbour
%% after every message given the script will wait SLEEP_TIME_AFTER_EVERY_MESSAGE ms
%% Every CHECKER_SESSIONS_INDICATOR is a checker session which just measures message TTD
%%
%%==============================================================================
-module(mongoose_mam_read_and_send_msgs_with_metrics).

-behaviour(amoc_scenario).

-include_lib("exml/include/exml.hrl").

-define(HOST, <<"localhost">>). %% The virtual host served by the server
-define(SERVER_IPS, {<<"127.0.0.1">>}). %% Tuple of servers, for example {<<"10.100.0.21">>, <<"10.100.0.22">>}
-define(CHECKER_SESSIONS_INDICATOR, 73). %% How often a checker session should be generated
-define(SLEEP_TIME_AFTER_SCENARIO, infinity). %% wait 10s after scenario before disconnecting
-define(NUMBER_OF_PREV_NEIGHBOURS, 4).
-define(NUMBER_OF_NEXT_NEIGHBOURS, 4).
-define(NUMBER_OF_SEND_MESSAGE_REPEATS, 10).
-define(SLEEP_TIME_AFTER_EVERY_MESSAGE, 20000).

%% scenario behavior
-export([start/1]).
-export([init/0]).

-define(MESSAGES_CT, [amoc, counters, messages_sent]).
-define(MESSAGE_TTD_CT, [amoc, times, message_ttd]).
-define(MAM_READ_CT, [amoc, times, mam_last_10_read]).

-type binjid() :: binary().


-spec init() -> ok.
init() ->
    lager:info("init some metrics"),
    exometer:new(?MESSAGES_CT, spiral),
    exometer_report:subscribe(exometer_report_graphite, ?MESSAGES_CT, [one, count], 10000),
    exometer:new(?MESSAGE_TTD_CT, histogram),
    exometer_report:subscribe(exometer_report_graphite, ?MESSAGE_TTD_CT, [mean, min, max, median, 95, 99, 999], 10000),
    exometer:new(?MAM_READ_CT, histogram),
    exometer_report:subscribe(exometer_report_graphite, ?MAM_READ_CT, [mean, min, max, median, 95, 99, 999], 10000),
    ok.

-spec user_spec(binary(), binary(), binary()) -> escalus_users:user_spec().
user_spec(ProfileId, Password, Res) ->
    [ {username, ProfileId},
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

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Cfg = make_user(MyId, <<"res1">>),

    IsChecker = MyId rem ?CHECKER_SESSIONS_INDICATOR == 0,

    {ConnectionTime, ConnectionResult} = timer:tc(escalus_connection, start, [Cfg]),
    Client = case ConnectionResult of
        {ok, ConnectedClient, _, _} ->
            exometer:update([amoc, counters, connections], 1),
            exometer:update([amoc, times, connection], ConnectionTime),
            ConnectedClient;
        Error ->
            exometer:update([amoc, counters, connection_failures], 1),
            lager:error("Could not connect user=~p, reason=~p", [Cfg, Error]),
            exit(connection_failed)
    end,

    do(IsChecker, MyId, Client),

    timer:sleep(?SLEEP_TIME_AFTER_SCENARIO),
    send_presence_unavailable(Client),
    escalus_connection:stop(Client).

-spec do(boolean(), amoc_scenario:user_id(), escalus:client()) -> any().
do(false, MyId, Client) ->
    escalus_connection:set_filter_predicate(Client, none),

    send_presence_available(Client),
    read_archive(Client),

    escalus_connection:set_filter_predicate(Client, none),
    timer:sleep(5000),

    NeighbourIds = lists:delete(MyId, lists:seq(max(1,MyId-?NUMBER_OF_PREV_NEIGHBOURS),
                                                MyId+?NUMBER_OF_NEXT_NEIGHBOURS)),
    send_messages_many_times(Client, ?SLEEP_TIME_AFTER_EVERY_MESSAGE, NeighbourIds);
do(_Other, _MyId, Client) ->
    %lager:info("checker"),
    escalus_connection:set_filter_predicate(Client, none),
    send_presence_available(Client),
    read_archive(Client),
    escalus_connection:set_filter_predicate(Client, fun escalus_pred:is_message/1),
    receive_forever(Client).

-spec read_archive(escalus:client()) -> any().
read_archive(Client) ->
    Payload = [#xmlel{name = <<"set">>,
                      attrs = [{<<"xmlns">>, <<"http//jabber.org/protocol/rsm">>}],
                      children = [#xmlel{name = <<"before">>},
                                  #xmlel{name = <<"simple">>},
                                  #xmlel{name = <<"max">>,
                                         children = [#xmlcdata{content = <<"10">>}]}]}],
    MamNS = <<"urn:xmpp:mam:tmp">>,
    Query = escalus_stanza:iq_get(MamNS, Payload),
    escalus_connection:set_filter_predicate(Client,
        fun(Stanza) -> escalus_pred:is_iq(<<"result">>, MamNS, Stanza) end),
    escalus_connection:send(Client, Query),
    Start = os:timestamp(),
    IQResult = escalus_connection:get_stanza(Client, mam_result, 30000),
    Diff = timer:now_diff(os:timestamp(), Start),
    exometer:update(?MAM_READ_CT, Diff).

-spec receive_forever(escalus:client()) -> no_return().
receive_forever(Client) ->
    Stanza = escalus_connection:get_stanza(Client, message, infinity),
    Now = usec:from_now(os:timestamp()),
    case Stanza of
        #xmlel{name = <<"message">>, attrs=Attrs} ->
            case lists:keyfind(<<"timestamp">>, 1, Attrs) of
                {_, Sent} ->
                    TTD = (Now - binary_to_integer(Sent)),
                    exometer:update(?MESSAGE_TTD_CT, TTD);
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    receive_forever(Client).

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
    lists:foreach(S, lists:seq(1, ?NUMBER_OF_SEND_MESSAGE_REPEATS)).

-spec send_messages_to_neighbors(escalus:client(), [binjid()], timeout()) -> list().
send_messages_to_neighbors(Client, TargetIds, SleepTime) ->
    [send_message(Client, make_jid(TargetId), SleepTime)
     || TargetId <- TargetIds].

-spec send_message(escalus:client(), binjid(), timeout()) -> ok.
send_message(Client, ToId, SleepTime) ->
    MsgIn = make_message(ToId),
    TimeStamp = integer_to_binary(usec:from_now(os:timestamp())),
    escalus_connection:send(Client, escalus_stanza:setattr(MsgIn, <<"timestamp">>, TimeStamp)),
    exometer:update([amoc, counters, messages_sent], 1),
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

-spec pick_server({binary()}) -> binary().
pick_server(Servers) ->
    S = size(Servers),
    N = erlang:phash2(self(), S) + 1,
    element(N, Servers).
