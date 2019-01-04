-module(pubsub_simple).

-behavior(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").

-define(HOST, <<"localhost">>). %% The virtual host served by the server
-define(SERVER_IPS, {<<"127.0.0.1">>}). %% Tuple of servers, for example {<<"10.100.0.21">>, <<"10.100.0.22">>}
-define(PUBSUB_ADDR, <<"pubsub.localhost">>).
-define(WAIT_FOR_STANZA_TIMEOUT, 10000).

%% temrarily
-compile(export_all).

-export([init/0]).
-export([start/1]).


-spec init() -> ok.
init() ->
    ok.

-spec start(integer()) -> ok.
start(Id) ->
    Client = connect_amoc_user(Id),
    lager:info("Amoc user connected ~n~p~n", [Client]),
    Node = create_pubsub_node(Client),
    lager:info("Pubsub node created~n~p~n", [Node]),
    Res = delete_pubsub_node(Client, Node),
    lager:info("Pubsub node deleted? ~n~p~n", [Res]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec connect_amoc_user(amoc_scenario:user_id()) -> escalus_users:user_spec().
connect_amoc_user(Id) ->
    connect_amoc_user(Id, <<"res1">>).

-spec connect_amoc_user(amoc_scenario:user_id(), binary()) -> escalus_users:user_spec().
connect_amoc_user(Id, Resource) ->
    Cfg = make_user(Id, Resource),
    {ok, Client, _} = escalus_connection:start(Cfg),
    Client.

-spec create_pubsub_node(escalus_users:user_spec()) -> {binary(), binary()}.
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

-spec pubsub_node() -> {binary(), binary()}.
pubsub_node() ->
    {?PUBSUB_ADDR, pubsub_node_name()}.

-spec rand_name(binary()) -> binary().
rand_name(Prefix) ->
    Suffix = base64:encode(crypto:strong_rand_bytes(5)),
    <<Prefix/binary, "_", Suffix/binary>>.

%% Generates nodetree_tree-safe names
-spec pubsub_node_name() -> binary().
pubsub_node_name() ->
    Name0 = rand_name(<<"princely_musings">>),
    re:replace(Name0, "/", "_", [global, {return, binary}]).

-spec delete_pubsub_node(escalus_users:user_spec(), {binary(), binary()}) -> true.
delete_pubsub_node(Client, Node) ->
    Id = id(Client, Node, <<"delete">>),
    Request = escalus_pubsub_stanza:delete_node(Client, Id, Node),
    escalus:send(Client, Request),
    Response = escalus:wait_for_stanza(Client, ?WAIT_FOR_STANZA_TIMEOUT),
    true = escalus_pred:is_iq_result(Response),
    Response.

-spec id(escalus_users:user_spec(), {binary(), binary()}, binary()) -> binary().
id(Client, {NodeAddr, NodeName}, Suffix) ->
    UserName = escalus_utils:get_username(Client),
    list_to_binary(io_lib:format("~s-~s-~s-~s",
				 [UserName, NodeAddr, NodeName, Suffix])).

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
    ProfileId = <<"user", BinId/binary>>,
    Password = <<"password", BinId/binary>>,
    user_spec(ProfileId, Password, R).

-spec pick_server({binary()}) -> binary().
pick_server(Servers) ->
    S = size(Servers),
    N = erlang:phash2(self(), S) + 1,
    element(N, Servers).

jid(User, full) -> escalus_utils:get_jid(User);
jid(User, bare) -> escalus_utils:get_short_jid(User).
