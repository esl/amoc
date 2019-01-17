%%==============================================================================
%% Copyright 2019 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%
%% In this scenarion users are sending message to its neighbours
%% (users wiht lower and grater id defined by NUMBER_OF_*_NEIGHBOURS values)
%% Messages will be send NUMBER_OF_SEND_MESSAGE_REPEATS to every selected neighbour
%% after every message given the script will wait SLEEP_TIME_AFTER_EVERY_MESSAGE ms
%% Every CHECKER_SESSIONS_INDICATOR is a checker session which just measures message TTD
%%
%%==============================================================================
-module(mongoose_simple_with_metrics).

-include_lib("exml/include/exml.hrl").

-define(CHECKER_SESSIONS_INDICATOR, 10). %% How often a checker session should be generated
-define(SLEEP_TIME_AFTER_SCENARIO, 10000). %% wait 10s after scenario before disconnecting
-define(NUMBER_OF_PREV_NEIGHBOURS, 4).
-define(NUMBER_OF_NEXT_NEIGHBOURS, 4).
-define(NUMBER_OF_SEND_MESSAGE_REPEATS, 73).
-define(SLEEP_TIME_AFTER_EVERY_MESSAGE, 20000).

-behaviour(amoc_scenario).

-export([start/1]).
-export([init/0]).

-spec init() -> ok.
init() ->
    lager:info("init metrics"),
    amoc_metrics:init(counters, amoc_metrics:messages_spiral_name()),
    amoc_metrics:init(times, amoc_metrics:message_ttd_histogram_name()),
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
    timer:sleep(5000),

    NeighbourIds = lists:delete(MyId, lists:seq(max(1,MyId-?NUMBER_OF_PREV_NEIGHBOURS),
                                                MyId+?NUMBER_OF_NEXT_NEIGHBOURS)),
    send_messages_many_times(Client, ?SLEEP_TIME_AFTER_EVERY_MESSAGE, NeighbourIds);
do(_Other, MyId, Client) ->
    lager:info("checker ~p", [MyId]),
    escalus_session:send_presence_available(Client),
    escalus_connection:wait_forever(Client).

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
    Msg = amoc_xmpp_stanza:make_message_with_timestamp(ToId),
    escalus_connection:send(Client, Msg),
    timer:sleep(SleepTime).

-spec send_and_recv_escalus_handlers() -> [{atom(), any()}].
send_and_recv_escalus_handlers() ->
    [
      {received_stanza_handlers, [fun amoc_xmpp_handlers:measure_ttd/3]},
      {sent_stanza_handlers, [fun amoc_xmpp_handlers:measure_sent_messages/2]}
    ].

