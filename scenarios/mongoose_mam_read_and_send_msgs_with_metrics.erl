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

-define(CHECKER_SESSIONS_INDICATOR, 73). %% How often a checker session should be generated
-define(SLEEP_TIME_AFTER_SCENARIO, infinity). %% wait 10s after scenario before disconnecting
-define(NUMBER_OF_PREV_NEIGHBOURS, 4).
-define(NUMBER_OF_NEXT_NEIGHBOURS, 4).
-define(NUMBER_OF_SEND_MESSAGE_REPEATS, 10).
-define(SLEEP_TIME_AFTER_EVERY_MESSAGE, 20000).

%% scenario behavior
-export([start/1]).
-export([init/0]).

-define(MAM_READ_CT, mam_last_10_read).

-spec init() -> ok.
init() ->
    lager:info("init metrics"),
    amoc_metrics:init(counters, amoc_metrics:messages_spiral_name()),
    amoc_metrics:init(times, amoc_metrics:message_ttd_histogram_name()),
    amoc_metrics:init(times, ?MAM_READ_CT),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    ExtraSpec = send_and_recv_escalus_handlers(),
    {ok, Client, _} = amoc_xmpp:connect_or_exit(MyId, ExtraSpec),

    IsChecker = MyId rem ?CHECKER_SESSIONS_INDICATOR == 0,
    do(IsChecker, MyId, Client),

    timer:sleep(?SLEEP_TIME_AFTER_SCENARIO),
    escalus_session:send_presence_unavailable(Client),
    escalus_connection:stop(Client).

-spec do(boolean(), amoc_scenario:user_id(), escalus:client()) -> any().
do(false, MyId, Client) ->
    escalus_connection:set_filter_predicate(Client, none),

    escalus_session:send_presence_available(Client),
    read_archive(Client),

    escalus_connection:set_filter_predicate(Client, none),
    timer:sleep(5000),

    NeighbourIds = lists:delete(MyId, lists:seq(max(1,MyId-?NUMBER_OF_PREV_NEIGHBOURS),
                                                MyId+?NUMBER_OF_NEXT_NEIGHBOURS)),
    send_messages_many_times(Client, ?SLEEP_TIME_AFTER_EVERY_MESSAGE, NeighbourIds);
do(_Other, _MyId, Client) ->
    escalus_connection:set_filter_predicate(Client, none),
    escalus_session:send_presence_available(Client),
    read_archive(Client),
    escalus_connection:set_filter_predicate(Client, fun escalus_pred:is_message/1),
    escalus_connection:wait_forever(Client).

-spec read_archive(escalus:client()) -> any().
read_archive(Client) ->
    Payload = [#xmlel{name = <<"set">>,
                      attrs = [{<<"xmlns">>, <<"http//jabber.org/protocol/rsm">>}],
                      children = [#xmlel{name = <<"before">>},
                                  #xmlel{name = <<"simple">>},
                                  #xmlel{name = <<"max">>,
                                         children = [#xmlcdata{content = <<"10">>}]}]}],
    MamNS = <<"urn:xmpp:mam:2">>,
    Query = escalus_stanza:iq_get(MamNS, Payload),
    escalus_connection:set_filter_predicate(Client,
        fun(Stanza) -> escalus_pred:is_iq(<<"result">>, MamNS, Stanza) end),
    escalus_connection:send(Client, Query),
    Start = os:timestamp(),
    _IQResult = escalus_connection:get_stanza(Client, mam_result, 30000),
    Diff = timer:now_diff(os:timestamp(), Start),
    amoc_metrics:update_time(?MAM_READ_CT, Diff).

-spec send_messages_many_times(escalus:client(), timeout(), [amoc_scenario:user_id()]) -> ok.
send_messages_many_times(Client, MessageInterval, NeighbourIds) ->
    S = fun(_) ->
                send_messages_to_neighbors(Client, NeighbourIds, MessageInterval)
        end,
    lists:foreach(S, lists:seq(1, ?NUMBER_OF_SEND_MESSAGE_REPEATS)).

-spec send_messages_to_neighbors(escalus:client(), [amoc_scenario:user_id()], timeout()) -> list().
send_messages_to_neighbors(Client, TargetIds, SleepTime) ->
    [send_message(Client, TargetId, SleepTime)
     || TargetId <- TargetIds].

-spec send_message(escalus:client(), amoc_scenario:user_id(), timeout()) -> ok.
send_message(Client, ToId, SleepTime) ->
    Body = <<"hello sir, you are a gentelman and a scholar.">>,
    Msg = escalus_stanza:chat_to_with_id_and_timestamp(amoc_xmpp_users:make_jid(ToId), Body),
    escalus_connection:send(Client, Msg),
    timer:sleep(SleepTime).

-spec send_and_recv_escalus_handlers() -> [{atom(), any()}].
send_and_recv_escalus_handlers() ->
    [
      {received_stanza_handlers, [fun amoc_xmpp_handlers:measure_ttd/3]},
      {sent_stanza_handlers, [fun amoc_xmpp_handlers:measure_sent_messages/2]}
    ].

