%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(mongoose_pubsub_docker).

-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").

-define(HOST, <<"localhost">>). %% The virtual host served by the server

-behaviour(amoc_scenario).

-export([init/0, start/1]).

-define(SUBSCRIBER_DELAY, 5000).
-define(DELAY_BETWEEN_MESSAGES, 1000).
-define(MESSAGES_PER_PUBLISHER, 5).
-define(SLEEP_TIME_AFTER_SCENARIO, 10000). %% wait 10s after scenario before disconnecting
-define(WAIT_FOR_STANZA_TIMEOUT, 10000).
-define(PUBSUB_ADDR, <<"pubsub.localhost">>).

-define(PUBSUB_NODES_CT, pubsub_nodes).
-define(SUBSCRIPTIONS_CT, subscriptions).
-define(ITEMS_SENT_CT, items_sent).
-define(ITEMS_RECEIVED_CT, items_received).

-spec init() -> ok.
init() ->
    set_env_interarrival(),
    amoc_metrics:init(counters, ?PUBSUB_NODES_CT),
    amoc_metrics:init(counters, ?SUBSCRIPTIONS_CT),
    amoc_metrics:init(counters, ?ITEMS_SENT_CT),
    amoc_metrics:init(counters, ?ITEMS_RECEIVED_CT),
    ok.

-spec start(amoc_scenario:user_id()) -> no_return().
start(MyId) ->
    Client = connect_amoc_user(MyId),
    case MyId rem 2 of
        1 -> work_as_publisher(MyId, Client);
        0 -> work_as_subscriber(MyId, Client)
    end.

work_as_publisher(MyId, Client) ->
    create_pubsub_node(Client, pubsub_node(MyId)),
    publish_items_forever(Client, MyId, pubsub_node(MyId), 1).

publish_items_forever(Client, MyId, Node, ItemId) ->
    timer:sleep(?DELAY_BETWEEN_MESSAGES),
    lager:debug("Publisher ~p publishing item ~p.", [MyId, ItemId]),
    publish(Client, integer_to_binary(ItemId), Node),
    publish_items_forever(Client, MyId, Node, ItemId + 1).

work_as_subscriber(MyId, Client) ->
    Nodes = discover_nodes(Client),
    [subscribe(Client, Node) || Node <- Nodes],
    lager:debug("Subscriber ~p subscribed to ~p nodes.", [MyId, length(Nodes)]),
    receive_messages_forever(MyId, Client).

receive_messages_forever(MyId, Client) ->
    Stanza = escalus:wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT),
    case escalus_pred:is_message(Stanza) of
        true -> verify_item_notification(MyId, Stanza);
        false -> verify_subscribe_response(MyId, Stanza)
    end,
    receive_messages_forever(MyId, Client).

verify_item_notification(MyId, Stanza) ->
    lager:debug("Subscriber ~p got message.", [MyId]),
    true = escalus_pred:is_stanza_from(?PUBSUB_ADDR, Stanza),
    true = escalus_pred:has_type(<<"headline">>, Stanza),
    ItemContent = item_content(),
    ItemContent = exml_query:path(Stanza, [{element, <<"event">>},
                                           {element, <<"items">>},
                                           {element, <<"item">>},
                                           {element, <<"entry">>}]),
    amoc_metrics:update_counter(?ITEMS_RECEIVED_CT, 1).

verify_subscribe_response(MyId, Stanza) ->
    lager:debug("Subscriber ~p got subscription response.", [MyId]),
    <<"subscribed">> = exml_query:path(Stanza, [{element, <<"pubsub">>},
                                                {element, <<"subscription">>},
                                                {attr, <<"subscription">>}]),
    amoc_metrics:update_counter(?SUBSCRIPTIONS_CT, 1).

create_pubsub_node(Client, Node) ->
    Id = id(Client, Node, <<"create">>),
    Request = escalus_pubsub_stanza:create_node(Client, Id, Node, []),
    escalus:send(Client, Request),
    Response = escalus:wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT),
    true = escalus_pred:is_iq_result(Response),
    amoc_metrics:update_counter(?PUBSUB_NODES_CT, 1).

%% delete_pubsub_node(Client, Node) ->
%%     Id = id(Client, Node, <<"delete">>),
%%     Request = escalus_pubsub_stanza:delete_node(Client, Id, Node),
%%     escalus:send(Client, Request),
%%     Response = escalus:wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT),
%%     true = escalus_pred:is_iq_result(Response).

publish(Client, ItemId, Node) ->
    Id = id(Client, Node, <<"publish">>),
    Request = escalus_pubsub_stanza:publish(Client, ItemId, item_content(), Id, Node),
    escalus:send(Client, Request),
    Response = escalus:wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT),
    true = escalus_pred:is_iq_result(Response),
    amoc_metrics:update_counter(?ITEMS_SENT_CT, 1).

item_content() ->
    #xmlel{name = <<"entry">>,
           attrs = [{<<"xmlns">>, <<"http://www.w3.org/2005/Atom">>}]}.

%% item_ids() ->
%%     [integer_to_binary(I) || I <- lists:seq(1, ?MESSAGES_PER_PUBLISHER)].

discover_nodes(Client) ->
    Request = escalus_pubsub_stanza:discover_nodes(Client, <<"discover">>, ?PUBSUB_ADDR),
    escalus:send(Client, Request),
    Response = escalus:wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT),
    true = escalus_pred:is_iq_result(Response),
    Items = exml_query:paths(Response, [{element, <<"query">>},
                                        {element, <<"item">>}]),
    [{exml_query:attr(Item, <<"jid">>),
      exml_query:attr(Item, <<"node">>)} || Item <- Items].

subscribe(Client, Node) ->
    Id = id(Client, Node, <<"subscribe">>),
    Request = escalus_pubsub_stanza:subscribe(Client, Id, Node),
    escalus:send(Client, Request).

%% unsubscribe(Client, Node) ->
%%     Id = id(Client, Node, <<"unsubscribe">>),
%%     Request = escalus_pubsub_stanza:unsubscribe(Client, Id, Node),
%%     escalus:send(Client, Request).

pubsub_node(PublisherId) ->
    NodeNo = integer_to_binary(PublisherId),
    {?PUBSUB_ADDR, <<"node", NodeNo/binary>>}.

id(Client, {NodeAddr, NodeName}, Suffix) ->
    UserName = escalus_utils:get_username(Client),
    list_to_binary(io_lib:format("~s-~s-~s-~s", [UserName, NodeAddr, NodeName, Suffix])).

%% TODO extract these common helpers to another module

connect_amoc_user(MyId) ->
    User = make_user(MyId, <<"res1">>),
    {ok, Client0, _} = amoc_xmpp:connect_or_exit(User),
    Client = Client0#client{jid = make_jid(MyId)},
    send_presence_available(Client),
    receive_presence(Client, Client),
    Client.

-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

receive_presence(Client1, Client2) ->
    PresenceNotification = escalus:wait_for_stanza(Client1, ?WAIT_FOR_STANZA_TIMEOUT),
    escalus:assert(is_presence, PresenceNotification),
    escalus:assert(is_stanza_from, [Client2], PresenceNotification).

-spec make_user(amoc_scenario:user_id(), binary()) -> escalus_users:user_spec().
make_user(Id, R) ->
    BinId = integer_to_binary(Id),
    ProfileId = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    user_spec(ProfileId, Password, R).

-spec user_spec(binary(), binary(), binary()) -> escalus_users:user_spec().
user_spec(ProfileId, Password, Res) ->
    [ {username, ProfileId},
      {server, ?HOST},
      {host, pick_server()},
      {password, Password},
      {carbons, false},
      {stream_management, false},
      {resource, Res}
    ].

-spec make_jid(amoc_scenario:user_id()) -> any().
make_jid(Id) ->
    BinInt = integer_to_binary(Id),
    ProfileId = <<"user_", BinInt/binary>>,
    Host = ?HOST,
    << ProfileId/binary, "@", Host/binary >>.

-spec pick_server() -> binary().
pick_server() ->
    Servers = env_servers(),
    S = size(Servers),
    N = erlang:phash2(self(), S) + 1,
    element(N, Servers).

-spec env_servers() -> {binary()}.
env_servers() ->
    List = re:split(os:getenv("AMOC_XMPP_SERVERS"), "\s",
                    [{return, binary}, trim]),
    list_to_tuple(List).

-spec set_env_interarrival() -> ok.
set_env_interarrival() ->
    Interarrival = case os:getenv("AMOC_INTERARRIVAL") of
                       false -> 100;
                       List -> list_to_integer(List)
                   end,
    application:set_env(amoc, interarrival, Interarrival).
