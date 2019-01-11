-module(pubsub_simple).

-behavior(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").

-define(HOST, <<"localhost">>). %% The virtual host served by the server
-define(PUBSUB_ADDR, <<"pubsub.localhost">>).
-define(WAIT_FOR_STANZA_TIMEOUT, 10000).
-define(GROUPNAME, <<"pubsub_test_group_name">>).
-define(NUMBER_OF_PUBSUB_NODES, 5).
-define(DELAY_BETWEEN_MESSAGES, 100).
-define(WAIT_FOR_NODES, 10000).
-define(PUBLISHER_SUBSCRIBER_DENOMINATOR, 6).
-define(PUBLISHERS_NUMERATOR, 2).

-define(NODE_CREATE_TIME, [amoc, timers, node_create_time]).
-define(SUBSCRIBE_TIME, [amoc, timers, subscribe_time]).
-define(PUBLISH_TIME, [amoc, timers, publish_time]).

-define(NODE_CREATE_FAILS_COUTER, [amoc, counters, node_create_failed]).
-define(SUBSCRIBE_FAILS_COUTER, [amoc, counters, subscribe_failed]).
-define(PUBLISH_FAILS_COUTER, [amoc, counters, publish_failed]).

-define(NODE_CREATE_SUCCESS_COUTER, [amoc, counters, node_create_success]).
-define(SUBSCRIBE_SUCCESS_COUTER, [amoc, counters, subscribe_success]).
-define(PUBLISH_SUCCESS_COUTER, [amoc, counters, publish_success]).

-export([init/0]).
-export([start/1]).

-required_variable({'NUMBER_OF_NODE_CREATORS', <<"Specifies how many users will create node"/utf8>>}).

-spec init() -> ok.
init() ->
    Stats =
    [
        {?NODE_CREATE_FAILS_COUTER, spiral},
        {?SUBSCRIBE_FAILS_COUTER, spiral},
        {?PUBLISH_FAILS_COUTER, spiral},
        {?NODE_CREATE_SUCCESS_COUTER, spiral},
        {?SUBSCRIBE_SUCCESS_COUTER, spiral},
        {?PUBLISH_SUCCESS_COUTER, spiral},
        {?NODE_CREATE_TIME, histogram},
        {?SUBSCRIBE_TIME, histogram},
        {?PUBLISH_TIME, histogram}
    ],
    [create_stat(Path, GraphType) ||  {Path, GraphType} <- Stats],
    ok.

create_stat(Path, spiral) ->
    exometer:new(Path, spiral),
    exometer_report:subscribe(exometer_report_graphite, Path, [one, count], 1000);

create_stat(Path, histogram) ->
    exometer:new(Path, histogram),
    exometer_report:subscribe(exometer_report_graphite, Path, [mean, min, max, median, 95, 99, 999], 1000).

-spec start(integer()) -> ok.
start(Id) ->
    erlang:put(item_id, 1),
    Creators = amoc_config:get('NUMBER_OF_NODE_CREATORS', 10),
    start(Id, Creators).

start(Id, Creators) when Id =< Creators ->
    create_pubsub_nodes(Id);
start(Id, _Creators) ->
    connect_to_nodes(Id).

connect_to_nodes(Id) ->
    pg2:create(?GROUPNAME),
    Client = connect_amoc_user(Id),
    Nodes = get_nodes(3, Client),
    case Id rem ?PUBLISHER_SUBSCRIBER_DENOMINATOR of
        X when X =< ?PUBLISHERS_NUMERATOR ->
            lager:info("Publisher, node: ~p~n", [Nodes]),
            publish_items_forever(Client, Id, Nodes, 1);
        _ ->
            lager:debug("Client: Starting subscriber Id = ~p\n", [Id]),
            work_as_subscriber(Id, Client, Nodes)
    end,
    ok.

provide_nodes_names(SelfClient, Nodes) ->
    receive
        {what_nodes, PID} ->
            PID ! {nodes, Nodes}
    after
        ?DELAY_BETWEEN_MESSAGES ->
            ItemID = erlang:get(item_id),
            erlang:put(item_id, ItemID + 1),
            publish(ItemID, SelfClient, Nodes)
    end,
    provide_nodes_names(SelfClient, Nodes).

get_nodes(0, _) ->
    lager:error("Client: NO NODES PROVIDED"),
    [];
get_nodes(Retries, Client) ->
    case erlang:get(nodes_names) of
        {nodes_names, Nodes} ->
            Nodes;
        _ ->
            Member = try_get_member([]),
            Member ! {what_nodes, self()},
            receive
                {nodes, Nodes} ->
                    erlang:put(nodes_names, {nodes_names, Nodes}),
                    Nodes
            after
                ?WAIT_FOR_NODES ->
                    logger:debug("Client: PID \n~p\n TIMEOUT", []),
                    get_nodes(Retries - 1, Client)
            end
    end.

publish_items_forever(Client, Id, Nodes, ItemId) ->
    timer:sleep(?DELAY_BETWEEN_MESSAGES),
    publish(ItemId, Client, Nodes),
    publish_items_forever(Client, Id, Nodes, ItemId + 1).

publish(ItemId, Client, Nodes) -> 
    ItemIdBin = integer_to_binary(ItemId),
    Resp = [ publish_pubsub_item(Client, <<"item_", ItemIdBin/binary>>, Node) || Node <- Nodes ],
    lager:debug("Client: Response is ~p\n", [Resp]).

work_as_subscriber(Id, Client, Nodes) ->
    [subscribe(Client, Node) || Node <- Nodes],
    lager:info("Subscriber, node: ~p~n", [Nodes]),
    receive_messages_forever(Id, Client).

receive_messages_forever(Id, Client) ->
    Stanza = escalus:wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT),
    lager:debug("Client: Subscriber ~p got stanza ~p.", [Id, Stanza]),
    receive_messages_forever(Id, Client).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


connect_amoc_user(Id) ->
    connect_amoc_user(Id, <<"res1">>).

connect_amoc_user(Id, Resource) ->
    Cfg = make_user(Id, Resource),
    {ok, Client, _} = escalus_connection:start(Cfg),
    Client.

create_pubsub_nodes(Id) ->
    pg2:create(?GROUPNAME),
    pg2:join(?GROUPNAME, self()),
    Client = connect_amoc_user(Id),
    Nodes = [create_pubsub_node(Client)],
    provide_nodes_names(Client, Nodes).

create_pubsub_node(Client) ->
    Node = pubsub_node(),
    ReqId = id(Client, Node, <<"create">>),
    NodeConfig = [{<<"pubsub#subscribe">>, <<"1">>},
                  {<<"pubsub#access_model">>, <<"open">>},
                  {<<"pubsub#publish_model">>, <<"open">>}],
    Request = escalus_pubsub_stanza:create_node(Client, ReqId,
						Node, NodeConfig),
    escalus:send(Client, Request),
    {CreateNodeTime, CreateNodeResult} = timer:tc(
        fun() ->
            escalus:wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT)
        end),

    case escalus_pred:is_iq_result(CreateNodeResult) of
        true ->
            exometer:update(?NODE_CREATE_SUCCESS_COUTER, 1),
            exometer:update(?NODE_CREATE_TIME, CreateNodeTime);
        Error ->
            exometer:update(?NODE_CREATE_FAILS_COUTER, 1),
            lager:error("Error creating node: ~p", [Error]),
            exit(connection_failed)
    end,
    Node.

pubsub_node() ->
    {?PUBSUB_ADDR, pubsub_node_name()}.

rand_name(Prefix) ->
    Suffix = base64:encode(crypto:strong_rand_bytes(5)),
    <<Prefix/binary, "_", Suffix/binary>>.

pubsub_node_name() ->
    Name0 = rand_name(<<"princely_musings">>),
    re:replace(Name0, "/", "_", [global, {return, binary}]).

id(Client, {NodeAddr, NodeName}, Suffix) ->
    UserName = escalus_utils:get_username(Client),
    list_to_binary(io_lib:format("~s-~s-~s-~s",
				 [UserName, NodeAddr, NodeName, Suffix])).

user_spec(ProfileId, Password, Res) ->
    ConnectionDetails = pick_server(),
    [ {username, ProfileId},
      {server, ?HOST},
      {password, Password},
      {carbons, false},
      {stream_management, false},
      {resource, Res}
    ] ++ ConnectionDetails.

make_user(Id, R) ->
    BinId = integer_to_binary(Id),
    ProfileId = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    user_spec(ProfileId, Password, R).

-spec pick_server() -> [proplists:property()].
pick_server() ->
    Servers = amoc_config:get(xmpp_servers),
    verify(Servers),
    S = length(Servers),
    N = erlang:phash2(self(), S) + 1,
    lists:nth(N, Servers).

verify(Servers) ->
    lists:foreach(
      fun(Proplist) ->
              true = proplists:is_defined(host, Proplist)
      end,
      Servers
     ).

try_get_member([]) ->
    Members = pg2:get_members(?GROUPNAME),
    try_get_member(Members);
try_get_member(Members) ->
    L = length(Members),
    N = erlang:phash2(self(), L) + 1,
    lists:nth(N, Members).

subscribe(Client, Node) ->
    Id = id(Client, Node, <<"subscribe">>),
    Request = escalus_pubsub_stanza:subscribe(Client, Id, Node),
    escalus:send(Client, Request),
    {SubscribeTime, SubscribeResult} = timer:tc(
        fun() ->
            escalus:wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT)
        end),
    case escalus_pred:is_iq_result(SubscribeResult) of
        true ->
            exometer:update(?SUBSCRIBE_SUCCESS_COUTER, 1),
            exometer:update(?SUBSCRIBE_TIME, SubscribeTime);
        Error ->
            exometer:update(?SUBSCRIBE_FAILS_COUTER, 1),
            lager:error("Error subscribing node ~p filed: ~p", [Node, Error]),
            exit(connection_failed)
        end.

publish_pubsub_item(Client, ItemId, Node) ->
    Id = id(Client, Node, <<"publish">>),
    Request = escalus_pubsub_stanza:publish(Client, ItemId, item_content(), Id, Node),
    escalus:send(Client, Request),
    {PublishTime, PublishResult} = timer:tc(
        fun() ->
            escalus:wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT)
        end),
    case escalus_pred:is_iq_result(PublishResult) of
        true ->
            exometer:update(?PUBLISH_SUCCESS_COUTER, 1),
            exometer:update(?PUBLISH_TIME, PublishTime);
        Error ->
            exometer:update(?PUBLISH_FAILS_COUTER, 1),
            lager:error("Error subscribing node ~p filed: ~p", [Node, Error]),
            exit(connection_failed)
        end.

item_content() ->
    #xmlel{name = <<"entry">>,
           attrs = [{<<"xmlns">>, <<"http://www.w3.org/2005/Atom">>}]}.
