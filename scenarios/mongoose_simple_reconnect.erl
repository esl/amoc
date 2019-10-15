%%==============================================================================
%% Copyright 2015-2019 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%
%% In this scenarion users are sending message to its neighbours
%% (users wiht lower and grater idea defined by NUMBER_OF_*_NEIGHBOURS values)
%% Messages will be send NUMBER_OF_SEND_MESSAGE_REPEATS to every selected neighbour
%% after every message given the script will wait SLEEP_TIME_AFTER_EVERY_MESSAGE ms
%% Every CHECKER_SESSIONS_INDICATOR is a checker session which just measures message TTD
%%
%%==============================================================================
-module(mongoose_simple_reconnect).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").

-define(CHECKER_SESSIONS_INDICATOR, 10). %% How often a checker session should be generated
-define(SLEEP_TIME_AFTER_SCENARIO, 0). %% wait 10s after scenario before disconnecting
-define(NUMBER_OF_PREV_NEIGHBOURS, 4).
-define(NUMBER_OF_NEXT_NEIGHBOURS, 4).
-define(NUMBER_OF_SEND_MESSAGE_REPEATS, 73).
-define(SLEEP_TIME_AFTER_EVERY_MESSAGE, 20000).
-define(SLEEP_TIME_BEFORE_RECONNECT, 5000).

-behaviour(amoc_scenario).

-export([start/1]).
-export([init/0]).

-define(RECONNECTS_CT, reconnects).

-spec init() -> ok.
init() ->
    lager:info("init metrics"),
    amoc_metrics:init(counters, amoc_metrics:messages_spiral_name()),
    amoc_metrics:init(times, amoc_metrics:message_ttd_histogram_name()),
    amoc_metrics:init(counters, ?RECONNECTS_CT),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    try
        session(MyId)
    catch exit:shutdown ->
              exit(shutdown);
          Exit:Reason ->
              lager:error("reconnection due to ~p ~p", [Exit, Reason]),
              amoc_metrics:update_counter(?RECONNECTS_CT),
              timer:sleep(?SLEEP_TIME_BEFORE_RECONNECT),
              start(MyId)
    end.

-spec session(amoc_scenario:user_id()) -> any().
session(MyId) ->
    ok = flush_mailbox(),

    {ok, Client, _} = amoc_xmpp:connect_or_exit(MyId, send_and_recv_escalus_handlers()),
    IsChecker = MyId rem ?CHECKER_SESSIONS_INDICATOR == 0,


    do(IsChecker, MyId, Client),

    timer:sleep(?SLEEP_TIME_AFTER_SCENARIO),
    send_presence_unavailable(Client),
    escalus_connection:stop(Client).

-spec do(boolean(), amoc_scenario:user_id(), escalus:client()) -> any().
do(false, MyId, Client) ->
    escalus_connection:set_filter_predicate(Client, none),

    send_presence_available(Client),
    timer:sleep(5000),

    NeighbourIds = lists:delete(MyId, lists:seq(max(1,MyId-?NUMBER_OF_PREV_NEIGHBOURS),
                                                MyId+?NUMBER_OF_NEXT_NEIGHBOURS)),
    send_messages_many_times(Client, ?SLEEP_TIME_AFTER_EVERY_MESSAGE, NeighbourIds);
do(_Other, _MyId, Client) ->
    lager:info("checker"),
    send_presence_available(Client),
    escalus_connection:wait_forever(Client).

-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

-spec send_presence_unavailable(escalus:client()) -> ok.
send_presence_unavailable(Client) ->
    Pres = escalus_stanza:presence(<<"unavailable">>),
    escalus_connection:send(Client, Pres).

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
    MsgIn = escalus_stanza:chat_to_with_id_and_timestamp(amoc_xmpp_users:make_jid(ToId), Body),
    escalus_connection:send(Client, MsgIn),
    timer:sleep(SleepTime).

-spec flush_mailbox() -> ok.
flush_mailbox() ->
    receive
        _ ->
            flush_mailbox()
    after 0 ->
        ok
    end.

-spec send_and_recv_escalus_handlers() -> [{atom(), any()}].
send_and_recv_escalus_handlers() ->
    [{received_stanza_handlers,
      amoc_xmpp_handlers:stanza_handlers(
        [{fun escalus_pred:is_message/1, fun amoc_xmpp_handlers:measure_ttd/3}])},
     {sent_stanza_handlers,
      amoc_xmpp_handlers:stanza_handlers(
        [{fun escalus_pred:is_message/1, fun amoc_xmpp_handlers:measure_sent_messages/0}])}
    ].
