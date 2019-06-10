-module(pubsub_simple).

-behavior(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").

-required_variable({'IQ_TIMEOUT',         <<"IQ timeout (milliseconds, def: 10000ms)"/utf8>>}).
-reqeired_variable({'COORDINATOR_DELAY',  <<"Delay after N subscriptions (milliseconds, def: 0ms)"/utf8>>}).
-required_variable({'NODE_CREATION_RATE', <<"Rate of node creations (per minute, def:600)">>}).
-required_variable({'PUBLICATION_SIZE',   <<"Size of additional payload (bytes, def:300)">>}).
-required_variable({'PUBLICATION_RATE',   <<"Rate of publications (per minute, def:1500)">>}).
-required_variable({'N_OF_SUBSCRIBERS',   <<"Number of subscriptions for each node (def: 50)"/utf8>>}).
-required_variable({'ACTIVATION_POLICY',  <<"Publish after subscribtion of (def: all_nodes | n_nodes)"/utf8>>}).
-required_variable({'MIM_HOST',           <<"The virtual host served by the server (def: <<\"localhost\">>)"/utf8>>}).
-required_variable({'PUBSUB_ADDR',        <<"Pubsub service address (def: <<\"pubsub.localhost\">>"/utf8>>}).

-define(ALL_PARAMETERS,[
    {iq_timeout,         'IQ_TIMEOUT',                           10000, positive_integer},
    {coordinator_delay,  'COORDINATOR_DELAY',                        0, nonnegative_integer},
    {node_creation_rate, 'NODE_CREATION_RATE',                     600, positive_integer},
    {publication_size,   'PUBLICATION_SIZE',                       300, nonnegative_integer},
    {publication_rate,   'PUBLICATION_RATE',                      1500, positive_integer},
    {n_of_subscribers,   'N_OF_SUBSCRIBERS',                        50, nonnegative_integer},
    {activation_policy,  'ACTIVATION_POLICY',                all_nodes, [all_nodes, n_nodes]},
    {mim_host,           'MIM_HOST',                   <<"localhost">>, bitstring},
    {pubsub_addr,        'PUBSUB_ADDR',         <<"pubsub.localhost">>, bitstring}
]).

-define(GROUP_NAME, <<"pubsub_simple_coordinator">>).
-define(NODE_CREATION_THROTTLING, node_creation).
-define(PUBLICATION_THROTTLING, publication).

-define(COORDINATOR_ID, 1).
-define(COORDINATOR_TIMEOUT, 100000).

-export([init/0, start/2]).

-spec init() -> {ok, amoc_scenario:state()} | {error, Reason :: term()}.
init() ->
    init_metrics(),
    case config:get_scenario_settings(?ALL_PARAMETERS) of
        {ok, Settings} ->
            [PublicationRate, NodeCreationRate] = [proplists:get_value(Key, Settings) ||
                                                      Key <- [publication_rate, node_creation_rate]],
            amoc_throttle:start(?NODE_CREATION_THROTTLING, NodeCreationRate),
            amoc_throttle:start(?PUBLICATION_THROTTLING, PublicationRate),
            {ok, Settings};
        Error -> Error
    end.

-spec start(amoc_scenario:user_id(), amoc_scenario:state()) -> any().
start(Id, Settings) ->
    config:store_scenario_settings(Settings),
    Client = connect_amoc_user(Id),
    case get_role(Id) of
        coordinator -> start_coordinator(Client, Settings);
        user -> start_user(Client)
    end.

init_metrics() ->
    Counters = [message,
                %% node counters
                node_creation_failure, node_creation_success, node_creation_timeout,
                %% publication counters
                publication_query, publication_result, publication_error,
                publication_success, publication_timeout,
                %% subscription counters
                subscription_query, subscription_result, subscription_error,
                subscription_success, subscription_timeout],
    Times = [node_creation, subscription, publication, message_ttd],
    [amoc_metrics:init(counters, Metric) || Metric <- Counters],
    [amoc_metrics:init(times, Metric) || Metric <- Times].

get_role(1) -> coordinator;
get_role(_) -> user.

%%------------------------------------------------------------------------------------------------
%% Coordinator
%%------------------------------------------------------------------------------------------------
start_coordinator(Client, Settings) ->
    pg2:create(?GROUP_NAME),
    Coordinator = spawn(fun() -> coordinator_fn(Settings) end),
    pg2:join(?GROUP_NAME, Coordinator),
    start_user(Client).

coordinator_fn(Settings) ->
    lager:debug("coordinator process ~p", [self()]),
    config:store_scenario_settings(Settings),
    config:dump_settings(),
    coordinator_loop([]).

coordinator_loop(AllPids) ->
    N = get_no_of_node_subscribers(),
    coordinator_delay(),
    case wait_for_n_nodes([], [], N) of
        {timeout, []} when AllPids =:= [] -> coordinator_loop([]);
        {timeout, Pids} ->
            activate_users(Pids, n_nodes),
            activate_users(Pids ++ AllPids, all_nodes),
            lager:error("Waited too long for the new user!"),
            coordinator_loop([]);
        {ok, Pids} ->
            activate_users(Pids, n_nodes),
            coordinator_loop(Pids ++ AllPids)
    end.

coordinator_delay() ->
    timer:sleep(get_parameter(coordinator_delay)).

wait_for_n_nodes(Pids, _, 0) ->
    {ok, Pids};
wait_for_n_nodes(Pids, Nodes, N) ->
    receive
        {new_node, NewPid, NewNode} ->
            subscribe_users(Pids, Nodes, NewPid, NewNode),
            wait_for_n_nodes([NewPid | Pids],
                             [NewNode | Nodes], N - 1)
    after ?COORDINATOR_TIMEOUT ->
        {timeout, Pids}
    end.

subscribe_users(Pids, Nodes, NewPid, NewNode) ->
    activate_users([NewPid], one_node),
    [subscribe_msg(NewPid, N) || N <- Nodes],
    [subscribe_msg(P, NewNode) || P <- Pids],
    subscribe_msg(NewPid, NewNode).

subscribe_msg(Pid, Node) ->
    Pid ! {subscribe_to, Node}.

activate_users(Pids, ActivationPolicy) ->
    case get_parameter(activation_policy) of
        ActivationPolicy ->
            [schedule_publishing(Pid)||Pid<-Pids];
        _ -> ok
    end.

get_coordinator_pid() ->
    case pg2:get_members(?GROUP_NAME) of
        [Coordinator] -> Coordinator;
        _ -> %% [] or {error, {no_such_group, ?GROUP_NAME}}
            timer:sleep(100),
            get_coordinator_pid()
    end.

%%------------------------------------------------------------------------------------------------
%% User
%%------------------------------------------------------------------------------------------------
start_user(Client) ->
    lager:debug("user process ~p", [self()]),
    Node = create_new_node(Client),
    erlang:monitor(process, Client#client.rcv_pid),
    escalus_tcp:set_active(Client#client.rcv_pid, true),
    user_loop(Client, Node, #{}).

create_new_node(Client) ->
    Coordinator = get_coordinator_pid(),
    amoc_throttle:send_and_wait(?NODE_CREATION_THROTTLING, create_node),
    Node = create_pubsub_node(Client),
    Coordinator ! {new_node, self(), Node},
    Node.

user_loop(Client, Node, Requests) ->
    IqTimeout = get_parameter(iq_timeout),
    receive
        {subscribe_to, N} ->
            {TS, Id} = subscribe(Client, N),
            amoc_metrics:update_counter(subscription_query, 1),
            user_loop(Client, Node, Requests#{Id=>{new, TS}});
        {stanza, _, #xmlel{name = <<"message">>} = Stanza, #{recv_timestamp := TimeStamp}} ->
            process_msg(Stanza, TimeStamp),
            user_loop(Client, Node, Requests);
        {stanza, _, #xmlel{name = <<"iq">>} = Stanza, #{recv_timestamp := TimeStamp}} ->
            NewRequests = process_iq(Stanza, TimeStamp, Requests),
            user_loop(Client, Node, NewRequests);
        publish_item ->
            {TS, Id} = publish_pubsub_item(Client, Node),
            amoc_metrics:update_counter(publication_query, 1),
            user_loop(Client, Node, Requests#{Id=>{new, TS}});
        {'DOWN', _, process, Pid, Info} when Pid =:= Client#client.rcv_pid ->
            lager:error("TCP connection process ~p down: ~p", [Pid, Info]);
        Msg ->
            lager:error("unexpected message ~p", [Msg])
    after IqTimeout ->
        user_loop(Client, Node, verify_request(Requests))
    end.

verify_request(Requests) ->
    IqTimeout = get_parameter(iq_timeout),
    Now = os:system_time(microsecond),
    VerifyFN =
        fun(Key, Value) ->
            case Value of
                {new, TS} when Now > TS + IqTimeout * 1000 ->
                    update_timeout_metrics(Key),
                    {timeout, TS};
                _ -> Value
            end
        end,
    maps:map(VerifyFN, Requests).

update_timeout_metrics(<<"publish", _/binary>>) ->
    amoc_metrics:update_counter(publication_timeout, 1);
update_timeout_metrics(<<"subscribe", _/binary>>) ->
    amoc_metrics:update_counter(subscription_timeout, 1);
update_timeout_metrics(Id) ->
    lager:error("unknown iq id ~p", Id).

schedule_publishing(Pid) ->
    amoc_throttle:send(?PUBLICATION_THROTTLING, Pid, publish_item).

%%------------------------------------------------------------------------------------------------
%% User connection
%%------------------------------------------------------------------------------------------------
connect_amoc_user(Id) ->
    Cfg = make_user_cfg(Id),
    {ok, Client, _} = escalus_connection:start(Cfg),
    erlang:put(jid, Client#client.jid),
    Client.

make_user_cfg(Id) ->
    BinId = integer_to_binary(Id),
    Username = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    Resource = <<"res1">>,
    ConnectionDetails = amoc_xmpp:pick_server([[{host, "127.0.0.1"}]]),
    [{username, Username},
     {server, get_parameter(mim_host)},
     {resource, Resource},
     {password, Password},
     {carbons, false},
     {stream_management, false},
     {socket_opts, socket_opts()} |
     ConnectionDetails].

socket_opts() ->
    [binary,
     {reuseaddr, false},
     {nodelay, true}].
%%------------------------------------------------------------------------------------------------
%% Node creation
%%------------------------------------------------------------------------------------------------
create_pubsub_node(Client) ->
    Node = pubsub_node(),
    ReqId = iq_id(create, Client, Node),
    NodeConfig = [{<<"pubsub#subscribe">>, <<"1">>},
                  {<<"pubsub#access_model">>, <<"open">>},
                  {<<"pubsub#publish_model">>, <<"open">>}],
    Request = escalus_pubsub_stanza:create_node(Client, ReqId,
                                                Node, NodeConfig),
    escalus:send(Client, Request),
    {CreateNodeTime, CreateNodeResult} = timer:tc(
        fun() ->
            catch escalus:wait_for_stanza(Client, get_parameter(iq_timeout))
        end),

    case {escalus_pred:is_iq_result(Request, CreateNodeResult), CreateNodeResult} of
        {true, _} ->
            lager:debug("node creation ~p (~p)", [Node, self()]),
            amoc_metrics:update_counter(node_creation_success, 1),
            amoc_metrics:update_time(node_creation, CreateNodeTime);
        {false,{'EXIT',{timeout_when_waiting_for_stanza,_}}}->
            amoc_metrics:update_counter(node_creation_timeout, 1),
            lager:error("Timeout creating node: ~p", [CreateNodeResult]),
            exit(node_creation_timeout);
        {false, _} ->
            amoc_metrics:update_counter(node_creation_failure, 1),
            lager:error("Error creating node: ~p", [CreateNodeResult]),
            exit(node_creation_failed)
    end,
    Node.

pubsub_node() ->
    Prefix = <<"princely_musings">>,
    Suffix = random_suffix(),
    Name = <<Prefix/binary, "_", Suffix/binary>>,
    {get_parameter(pubsub_addr), Name}.

%%------------------------------------------------------------------------------------------------
%% Node subscription
%%------------------------------------------------------------------------------------------------
subscribe(Client, Node) ->
    Id = iq_id(subscribe, Client, Node),
    Request = escalus_pubsub_stanza:subscribe(Client, Id, Node),
    escalus:send(Client, Request),
    {os:system_time(microsecond), Id}.

%%------------------------------------------------------------------------------------------------
%% Item publishing
%%------------------------------------------------------------------------------------------------
publish_pubsub_item(Client, Node) ->
    Id = iq_id(publish, Client, Node),
    PayloadSize = get_parameter(publication_size),
    Content = item_content(PayloadSize),
    Request = escalus_pubsub_stanza:publish(Client, Content, Id, Node),
    escalus:send(Client, Request),
    {os:system_time(microsecond), Id}.


item_content(PayloadSize) ->
    Payload = #xmlcdata{content = <<<<"A">> || _ <- lists:seq(1, PayloadSize)>>},
    #xmlel{
        name     = <<"entry">>,
        attrs    = [{<<"timestamp">>, integer_to_binary(os:system_time(microsecond))},
                    {<<"jid">>, erlang:get(jid)}],
        children = [Payload]}.

%%------------------------------------------------------------------------------------------------
%% Item processing
%%------------------------------------------------------------------------------------------------
process_msg(#xmlel{name = <<"message">>} = Stanza, TS) ->
    escalus:assert(is_message, Stanza),
    Entry = exml_query:path(Stanza, [{element, <<"event">>}, {element, <<"items">>},
                                     {element, <<"item">>}, {element, <<"entry">>}]),
    case {exml_query:attr(Entry, <<"jid">>), erlang:get(jid)} of
        {JID, JID} -> schedule_publishing(self());
        _ -> ok
    end,
    TimeStampBin = exml_query:attr(Entry, <<"timestamp">>),
    TimeStamp = binary_to_integer(TimeStampBin),
    TTD = TS - TimeStamp,
    lager:debug("time to delivery ~p", [TTD]),
    amoc_metrics:update_counter(message),
    amoc_metrics:update_time(message_ttd, TTD).

process_iq(#xmlel{name = <<"iq">>} = Stanza, TS, Requests) ->
    RespId = exml_query:attr(Stanza, <<"id">>),
    case {RespId, maps:get(RespId, Requests, undefined)} of
        {_, undefined} ->
            lager:warning("unknown iq ~p ~p", [Stanza]);
        {<<"publish", _/binary>>, {Tag, ReqTS}} ->
            handle_publish_resp(Stanza, {Tag, TS - ReqTS});
        {<<"subscribe", _/binary>>, {Tag, ReqTS}} ->
            handle_subscribe_resp(Stanza, {Tag, TS - ReqTS})
    end,
    maps:remove(RespId, Requests).

handle_publish_resp(PublishResult, {Tag, PublishTime}) ->
    IqTimeout = get_parameter(iq_timeout),
    case escalus_pred:is_iq_result(PublishResult) of
        true ->
            lager:debug("publish time ~p", [PublishTime]),
            amoc_metrics:update_counter(publication_result, 1),
            amoc_metrics:update_time(publication, PublishTime),
            case Tag of
                new when IqTimeout * 1000 > PublishTime ->
                    amoc_metrics:update_counter(publication_success, 1);
                new ->
                    amoc_metrics:update_counter(publication_timeout, 1);
                timeout -> ok %% do nothing, it's already reported as timeout
            end;
        _ ->
            amoc_metrics:update_counter(publication_error, 1),
            lager:error("Error publishing failed: ~p", [PublishResult]),
            exit(publication_failed)
    end.

handle_subscribe_resp(SubscribeResult, {Tag, SubscribeTime}) ->
    IqTimeout = get_parameter(iq_timeout),
    case escalus_pred:is_iq_result(SubscribeResult) of
        true ->
            lager:debug("subscribe time ~p", [SubscribeTime]),
            amoc_metrics:update_counter(subscription_result, 1),
            amoc_metrics:update_time(subscription, SubscribeTime),
            case Tag of
                new when IqTimeout > SubscribeTime ->
                    amoc_metrics:update_counter(subscription_success, 1);
                new ->
                    amoc_metrics:update_counter(subscription_timeout, 1);
                timeout -> ok %% do nothing, it's already reported as timeout
            end;
        _ ->
            amoc_metrics:update_counter(subscription_error, 1),
            lager:error("Error subscribing failed: ~p", [SubscribeResult]),
            exit(subscription_failed)
    end.

%%------------------------------------------------------------------------------------------------
%% Stanza helpers
%%------------------------------------------------------------------------------------------------
iq_id(Type, Client, {NodeAddr, NodeName}) ->
    UserName = escalus_utils:get_username(Client),
    Suffix = random_suffix(),
    list_to_binary(io_lib:format("~s-~s-~s-~s-~p",
                                 [Type, UserName, NodeAddr, NodeName, Suffix])).

random_suffix() ->
    Suffix = base64:encode(crypto:strong_rand_bytes(5)),
    re:replace(Suffix, "/", "_", [global, {return, binary}]).

%%------------------------------------------------------------------------------------------------
%% Config helpers
%%------------------------------------------------------------------------------------------------

get_parameter(Name) ->
    case config:get_parameter(Name) of
        {error,Err} ->
            lager:error("config:get_parameter/1 failed ~p", [Err]),
            exit(Err);
        {ok,Value} -> Value
    end.

get_no_of_node_subscribers() ->
    %instead of constant No of subscriptions we can use min/max values.
    get_parameter(n_of_subscribers).




