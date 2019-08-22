%%==============================================================================
%% Copyright 2019 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%
%% In this scenarion users are sending message to its neighbours
%% (users wiht lower and grater id defined by NUMBER_OF_*_NEIGHBOURS values)
%% Messages will be sent NUMBER_OF_SEND_MESSAGE_REPEATS to every selected neighbour
%% after every message given the script will wait SLEEP_TIME_AFTER_EVERY_MESSAGE ms
%% Message TTD is calculated by the `received_stanza_handler`.
%%
%%==============================================================================
-module(mongoose_docker_one_to_one).

-include_lib("exml/include/exml.hrl").

-define(HOST, <<"localhost">>). %% The virtual host served by the server
-define(SLEEP_TIME_AFTER_SCENARIO, 10000). %% wait 10s after scenario before disconnecting
-required_variable({'NUMBER_OF_PREV_NEIGHBOURS', <<"Number of users before current one to use."/utf8>>}).
-required_variable({'NUMBER_OF_NEXT_NEIGHBOURS',<<"Number of users after current one to use."/utf8>>}).
-required_variable({'NUMBER_OF_SEND_MESSAGE_REPEATS', <<"Number of send message (to all neighours) repeats"/utf8>>}).
-required_variable({'SLEEP_TIME_AFTER_EVERY_MESSAGE', <<"Wait time between sent messages (in seconds)"/utf8>>}).

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
    ExtraProps = amoc_xmpp:pick_server([[{host, "127.0.0.1"}]]) ++
    send_and_recv_escalus_handlers() ++
    [{socket_opts, socket_opts()}],
    {ok, Client, _} = amoc_xmpp:connect_or_exit(MyId, ExtraProps),

    do(MyId, Client),

    escalus_connection:wait(Client, ?SLEEP_TIME_AFTER_SCENARIO),
    escalus_session:send_presence_unavailable(Client),
    escalus_connection:stop(Client).

-spec do(amoc_scenario:user_id(), escalus:client()) -> any().
do(MyId, Client) ->
    escalus_connection:set_filter_predicate(Client, fun escalus_pred:is_message/1),

    escalus_session:send_presence_available(Client),
    escalus_connection:wait(Client, 5000),

    PrevNeighbours = amoc_config:get('NUMBER_OF_PREV_NEIGHBOURS', 4),
    NextNeighbours = amoc_config:get('NUMBER_OF_NEXT_NEIGHBOURS', 4),
    NeighbourIds = lists:delete(MyId, lists:seq(max(1, MyId - PrevNeighbours),
                                                MyId + NextNeighbours)),
    SleepTimeAfterMessage = amoc_config:get('SLEEP_TIME_AFTER_EVERY_MESSAGE', 20),
    send_messages_many_times(Client, timer:seconds(SleepTimeAfterMessage), NeighbourIds).

-spec send_messages_many_times(escalus:client(), timeout(), [amoc_scenario:user_id()]) -> ok.
send_messages_many_times(Client, MessageInterval, NeighbourIds) ->
    S = fun(_) ->
                send_messages_to_neighbors(Client, NeighbourIds, MessageInterval)
        end,
    SendMessageRepeats = amoc_config:get('NUMBER_OF_SEND_MESSAGE_REPEATS', 73),
    lists:foreach(S, lists:seq(1, SendMessageRepeats)).


-spec send_messages_to_neighbors(escalus:client(), [amoc_scenario:user_id()], timeout()) -> list().
send_messages_to_neighbors(Client, TargetIds, SleepTime) ->
    [send_message(Client, TargetId, SleepTime)
     || TargetId <- TargetIds].

-spec send_message(escalus:client(), amoc_scenario:user_id(), timeout()) -> ok.
send_message(Client, ToId, SleepTime) ->
    Body = <<"hello sir, you are a gentelman and a scholar.">>,
    MsgIn = escalus_stanza:chat_to_with_id_and_timestamp(amoc_xmpp_users:make_jid(ToId), Body),
    escalus_connection:send(Client, MsgIn),
    escalus_connection:wait(Client, SleepTime).

-spec send_and_recv_escalus_handlers() -> [{atom(), any()}].
send_and_recv_escalus_handlers() ->
    [{received_stanza_handlers,
      amoc_xmpp_handlers:stanza_handlers(
        [{fun escalus_pred:is_message/1, fun amoc_xmpp_handlers:measure_ttd/3}])},
     {sent_stanza_handlers,
      amoc_xmpp_handlers:stanza_handlers(
        [{fun escalus_pred:is_message/1, fun amoc_xmpp_handlers:measure_sent_messages/0}])}
    ].

-spec socket_opts() -> [gen_tcp:option()].
socket_opts() ->
    [binary,
     {reuseaddr, false},
     {nodelay, true}].
