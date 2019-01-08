-module(pubsub_simple).

-behavior(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").

-define(HOST, <<"localhost">>). %% The virtual host served by the server
-define(SERVER_IPS, {<<"127.0.0.1">>}). %% Tuple of servers, for example {<<"10.100.0.21">>, <<"10.100.0.22">>}
-define(PUBSUB_ADDR, <<"pubsub.localhost">>).
-define(WAIT_FOR_STANZA_TIMEOUT, 10000).
-define(GROUPNAME, <<"pubsub_test_group_name">>).
-define(NUMBER_OF_PUBSUB_NODES, 5).
-define(DELAY_BETWEEN_MESSAGES, 100).
-define(WAIT_FOR_NODES, 5000).

-export([init/0]).
-export([start/1]).

-spec init() -> ok.
init() -> ok.

-spec start(integer()) -> ok.

start(1) ->
    pg2:create(?GROUPNAME),
    pg2:join(?GROUPNAME, self()),
    Client = connect_amoc_user(1),
    Nodes = [create_pubsub_node(Client) || _ <- lists:seq(1, ?NUMBER_OF_PUBSUB_NODES)],
    provide_nodes_names(Client, Nodes);

start(Id) ->
    pg2:create(?GROUPNAME),
    Client = connect_amoc_user(Id),
    Nodes = get_nodes(3, Client),
    case Id rem 2 of
        0 ->
            lager:info("Client: Starting publisher\n", []),
            publish_items_forever(Client, Id, Nodes, 1);
        1 ->
            lager:info("Client: Starting subscriber\n", []),
            work_as_subscriber(Id, Client, Nodes)
    end,
    ok.

provide_nodes_names(SelfClient, Nodes) ->
    receive
        {what_nodes, PID, Client} ->
            AffChange = [{Client, <<"publisher">>}],
            [ set_affiliations(SelfClient, Node, AffChange) || Node <- Nodes],
            PID ! {nodes, Nodes}
    end,
    provide_nodes_names(SelfClient, Nodes).

get_nodes(0, _) ->
    loger:error("Client: NO NODES PROVIDED"),
    [];
get_nodes(Retries, Client) ->

    case erlang:get(nodes_names) of
        {nodes_names, Nodes} ->
            Nodes;
        _ ->
            Members = pg2:get_members(?GROUPNAME),
            [ M ! {what_nodes, self(), Client} || M <- Members ],
            receive
                {nodes, Nodes} ->
                    erlang:put(nodes_names, {nodes_names, Nodes}),
                    Nodes
            after
                ?WAIT_FOR_NODES ->
                    logger:info("Client: PID \n~p\n TIMEOUT", []),
                    get_nodes(Retries - 1, Client)
            end
    end.

publish_items_forever(Client, Id, Nodes, ItemId) ->
    timer:sleep(?DELAY_BETWEEN_MESSAGES),
    ItemIdBin = integer_to_binary(ItemId),
    Resp = [ publish_pubsub_item(Client, <<"item_", ItemIdBin/binary>>, Node) || Node <- Nodes ],
    lager:info("Client: Response is ~p\n", [Resp]),
    publish_items_forever(Client, Id, Nodes, ItemId + 1).

work_as_subscriber(Id, Client, Nodes) ->
    [subscribe(Client, Node) || Node <- Nodes],
    lager:info("Client: Subscriber ~p subscribed to ~p nodes.", [Id, length(Nodes)]),
    receive_messages_forever(Id, Client).

receive_messages_forever(Id, Client) ->
    Stanza = escalus:wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT),
    lager:info("Client: Subscriber ~p got stanza ~p.", [Id, Stanza]),
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

create_pubsub_node(Client) ->
    Node = pubsub_node(),
    ReqId = id(Client, Node, <<"create">>),
    Request = escalus_pubsub_stanza:create_node(Client, ReqId,
						Node, []),
    escalus:send(Client, Request),
    Response = escalus:wait_for_stanza(Client,
				       ?WAIT_FOR_STANZA_TIMEOUT),
    true = escalus_pred:is_iq_result(Response),
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
    [ {username, ProfileId},
      {server, ?HOST},
      {host, pick_server(?SERVER_IPS)},
      {password, Password},
      {carbons, false},
      {stream_management, false},
      {resource, Res}
    ].

make_user(Id, R) ->
    BinId = integer_to_binary(Id),
    ProfileId = <<"user", BinId/binary>>,
    Password = <<"password", BinId/binary>>,
    user_spec(ProfileId, Password, R).

pick_server(Servers) ->
    S = size(Servers),
    N = erlang:phash2(self(), S) + 1,
    element(N, Servers).

subscribe(Client, Node) ->
    Id = id(Client, Node, <<"subscribe">>),
    Request = escalus_pubsub_stanza:subscribe(Client, Id, Node),
    escalus:send(Client, Request),
    Response = escalus:wait_for_stanza(Client,
				       ?WAIT_FOR_STANZA_TIMEOUT),
    true = escalus_pred:is_iq_result(Response).

publish_pubsub_item(Client, ItemId, Node) ->
    Id = id(Client, Node, <<"publish">>),
    Request = escalus_pubsub_stanza:publish(Client, ItemId, item_content(), Id, Node),
    escalus:send(Client, Request),
    Response = escalus:wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT),
    lager:info("Client: Publisher:  Response is: ~n~p~n", [Response]),
    true = escalus_pred:is_iq_result(Response).

item_content() ->
    #xmlel{name = <<"entry">>,
           attrs = [{<<"xmlns">>, <<"http://www.w3.org/2005/Atom">>}]}.

set_affiliations(Client, Node, AffChange) ->
    Id = id(Client, Node, <<"set_affs">>),
    Request = escalus_pubsub_stanza:set_affiliations(Client, Id, Node, AffChange),
    escalus:send(Client, Request),
    Response = escalus:wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT),
    lager:info("Client: Publisher: Change Afilations Response is: ~n~p~n", [Response]),
    true = escalus_pred:is_iq_result(Response).
