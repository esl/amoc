-module(pubsub_pep).

-behavior(amoc_scenario).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-required_variable({'IQ_TIMEOUT',         <<"IQ timeout (milliseconds, def: 10000ms)"/utf8>>}).
-reqeired_variable({'COORDINATOR_DELAY',  <<"Delay after N subscriptions (milliseconds, def: 0ms)"/utf8>>}).
-reqeired_variable({'ACTIVATION_DELAY',   <<"Delay between users activation (milliseconds, def: 100ms)"/utf8>>}).
-required_variable({'PUBLICATION_DELAY',  <<"Delay between items publishing (milliseconds, def: 30000ms)"/utf8>>}).
-required_variable({'PUBLICATION_SIZE',   <<"Size of additional payload (bytes, def:300)">>}).
-required_variable({'N_OF_SUBSCRIBERS',   <<"Number of subscriptions for each node (def: 50)"/utf8>>}).
-required_variable({'HELPERS_POOL_SIZE',  <<"Number of helper processes in the pool (def: 10)"/utf8>>}).
-required_variable({'CREATION_POLICY',    <<"Nodes created by (def: helper | user)"/utf8>>}).
-required_variable({'ACTIVATION_POLICY',  <<"Publish after subscribtion of (def: all_nodes | n_nodes | one_node)"/utf8>>}).
-required_variable({'SCHEDULING_POLICY',  <<"Scheduling policy (def: sync_rcv | syn_req | async)"/utf8>>}).
-required_variable({'MIM_HOST',           <<"The virtual host served by the server (def: <<\"localhost\">>)"/utf8>>}).

-define(ALL_PARAMETERS,[
    {iq_timeout,         'IQ_TIMEOUT',                    10000, positive_integer},
    {coordinator_delay,  'COORDINATOR_DELAY',                 0, nonnegative_integer},
    {activation_delay,   'ACTIVATION_DELAY',                100, positive_integer},
    {publication_delay,  'PUBLICATION_DELAY',             30000, nonnegative_integer},
    {publication_size,   'PUBLICATION_SIZE',                300, nonnegative_integer},
    {n_of_subscribers,   'N_OF_SUBSCRIBERS',                 50, nonnegative_integer},
    {helpers_pool_size,  'HELPERS_POOL_SIZE',                10, nonnegative_integer},
    {creation_policy,    'CREATION_POLICY',              helper, [helper, user]},
    {activation_policy,  'ACTIVATION_POLICY',         all_nodes, [all_nodes, n_nodes, one_node]},
    {scheduling_policy,  'SCHEDULING_POLICY',          sync_rcv, [sync_rcv, sync_req, async]},
    {mim_host,           'MIM_HOST',            <<"localhost">>, bitstring}
]).

-define(PEP_NODE_NS, <<"just_some_random_namespace">>).
-define(CAPS_HASH, <<"erNmVoMSwRBR4brUU/inYQ5NFr0=">>). %% mod_caps:make_disco_hash(feature_elems(), sha1).
-define(NODE, {pep, ?PEP_NODE_NS}).

-define(GROUP_NAME, <<"pubsub_simple_coordinator">>).
-define(HELPERS_GROUP_NAME, <<"pubsub_simple_helpers">>).

-define(COORDINATOR_ID, 1).
-define(COORDINATOR_TIMEOUT, 100000).

-export([init/0, start/1]).

-spec init() -> ok.
init() ->
    init_metrics(),
    verify_scenario_settings().

-spec start(amoc_scenario:user_id()) -> any().
start(Id) ->
    set_scenario_settings(),
    Client = connect_amoc_user(Id),
    case get_role(Id) of
        coordinator -> start_coordinator(Client);
        user -> start_user(Client)
    end.

init_metrics() ->
    Counters = [message,
                %% node counters
                node_creation_failure, node_creation_success, node_creation_timeout,
                %% publication counters
                publication_query, publication_result, publication_error,
                publication_success, publication_timeout],
    Times = [node_creation, publication, message_ttd],
    [amoc_metrics:init(counters, Metric) || Metric <- Counters],
    [amoc_metrics:init(times, Metric) || Metric <- Times].

get_role(1) -> coordinator;
get_role(_) -> user.


%%------------------------------------------------------------------------------------------------
%% Coordinator
%%------------------------------------------------------------------------------------------------
start_coordinator(Client) ->
    pg2:create(?GROUP_NAME),
    pg2:create(?HELPERS_GROUP_NAME),
    Coordinator = spawn(fun coordinator_fn/0),
    NoOfHelpers = get_parameter(helpers_pool_size),
    [spawn(fun start_helper/0) || _ <- lists:seq(1, NoOfHelpers)],
    pg2:join(?GROUP_NAME, Coordinator),
    start_user(Client).

coordinator_fn() ->
    lager:debug("coordinator process ~p", [self()]),
    set_scenario_settings(),
    coordinator_loop([]).

coordinator_loop(AllPids) ->
    N = get_no_of_node_subscribers(),
    coordinator_delay(),
    case wait_for_n_clients([], [], N) of
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

wait_for_n_clients(Pids, Clients, 0) ->
    make_all_clients_friends(Clients),
    {ok, Pids};
wait_for_n_clients(Pids, Clients, N) ->
    receive
        {new_client, NewPid, NewClient} ->
            activate_users([NewPid], one_node),
            wait_for_n_clients([NewPid | Pids],
                               [NewClient | Clients], N - 1)
    after ?COORDINATOR_TIMEOUT ->
        make_all_clients_friends(Clients),
        {timeout, Pids}
    end.

make_all_clients_friends(Clients) ->
    escalus_utils:distinct_pairs(
        fun(C1, C2) ->
            send_presence(C1, <<"subscribe">>, C2),
            send_presence(C2, <<"subscribe">>, C1)
        end, Clients).

activate_users(Pids, ActivationPolicy) ->
    case get_parameter(activation_policy) of
        ActivationPolicy ->
            ActivationDelay = get_parameter(activation_delay),
            spawn(fun() -> activate_users_with_delay(Pids, ActivationDelay) end);
        _ -> ok
    end.

activate_users_with_delay([], _) -> ok;
activate_users_with_delay([Pid | T], ActivationDelay) ->
    lager:debug("activate user ~p", [Pid]),
    Pid ! publish_item,
    timer:sleep(ActivationDelay),
    activate_users_with_delay(T, ActivationDelay).

get_coordinator_pid() ->
    case pg2:get_members(?GROUP_NAME) of
        [Coordinator] -> Coordinator;
        _ -> %% [] or {error, {no_such_group, ?GROUP_NAME}}
            timer:sleep(100),
            get_coordinator_pid()
    end.

%%------------------------------------------------------------------------------------------------
%% Helper
%%------------------------------------------------------------------------------------------------

start_helper() ->
    Self = self(),
    lager:debug("helper process ~p", [Self]),
    get_coordinator_pid(), %% ensure than coordinator is created
    pg2:join(?HELPERS_GROUP_NAME, Self),
    helper_loop().

helper_loop() ->
    UserPid = receive
                  {allocate_helper, Pid} ->
                      Pid
              end,
    UserPid ! allocated,
    receive
        release_helper ->
            helper_loop()
    end.


get_helper() ->
    case pg2:get_members(?HELPERS_GROUP_NAME) of
        [_ | _] = List -> %% nonempty list
            N = rand:uniform(length(List)),
            lists:nth(N, List);
        _ -> %% [] or {error, {no_such_group, ?GROUP_NAME}}
            timer:sleep(100),
            get_helper()
    end.

create_pubsub_node_for_user(Client) ->
    %% here helper processes are used as just pool of mutexes.
    %% we can send message from the helper process using Client.
    %% however we cannot receive any response back. triggering
    %% escalus:wait_for_stanza/2 from the helper process always
    %% results in timeout. as incoming message with stanza arrives
    %% to the Client's creator/owner (user) process, not the
    %% function caller process.
    Helper = get_helper(),
    Helper ! {allocate_helper, self()},
    receive
        allocated ->
            create_pubsub_node(Client),
            Helper ! release_helper
    end.

%%------------------------------------------------------------------------------------------------
%% User
%%------------------------------------------------------------------------------------------------
start_user(Client) ->
    lager:debug("user process ~p", [self()]),
    create_new_node(Client),
    erlang:monitor(process, Client#client.rcv_pid),
    escalus_tcp:set_active(Client#client.rcv_pid, true),
    send_presence_with_caps(Client),
    user_loop(Client, #{}).

create_new_node(Client) ->
    Coordinator = get_coordinator_pid(),
    case get_parameter(creation_policy) of
        user ->
            create_pubsub_node(Client);
        helper ->

            create_pubsub_node_for_user(Client)
    end,
    Coordinator ! {new_client, self(), Client}.

user_loop(Client, Requests) ->
    IqTimeout = get_parameter(iq_timeout),
    receive
        {stanza, _, #xmlel{name = <<"message">>} = Stanza, #{recv_timestamp := TimeStamp}} ->
            process_msg(Stanza, TimeStamp),
            user_loop(Client, Requests);
        {stanza, _, #xmlel{name = <<"iq">>} = Stanza, #{recv_timestamp := TimeStamp}} ->
            NewRequests = process_iq(Client, Stanza, TimeStamp, Requests),
            user_loop(Client, NewRequests);
        {stanza, _, #xmlel{name = <<"presence">>} = Stanza, _} ->
            process_presence(Client, Stanza),
            user_loop(Client, Requests);
        publish_item ->
            {TS, Id} = publish_pubsub_item(Client),
            amoc_metrics:update_counter(publication_query, 1),
            schedule_publishing(async),
            user_loop(Client, Requests#{Id=>{new, TS}});
        {'DOWN', _, process, Pid, Info} when Pid =:= Client#client.rcv_pid ->
            lager:error("TCP connection process ~p down: ~p", [Pid, Info]);
        Msg ->
            lager:error("unexpected message ~p", [Msg])
    after IqTimeout ->
        user_loop(Client, verify_request(Requests))
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
update_timeout_metrics(Id) ->
    lager:error("unknown iq id ~p", Id).

schedule_publishing(SchedulingPolicy) ->
    case get_parameter(scheduling_policy) of
        SchedulingPolicy ->
            DelayBetweenMessages = get_parameter(publication_delay),
            erlang:send_after(DelayBetweenMessages, self(), publish_item);
        _ -> ok
    end.


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
    ReqId = iq_id(create, Client),
    Request = publish_pubsub_stanza(Client, ReqId, #xmlel{name = <<"nothing">>}),
    %Request = escalus_pubsub_stanza:create_node(Client, ReqId, ?NODE),
    escalus:send(Client, Request),

    {CreateNodeTime, CreateNodeResult} = timer:tc(
        fun() ->
            catch escalus:wait_for_stanza(Client, get_parameter(iq_timeout))
        end),

    case {escalus_pred:is_iq_result(Request, CreateNodeResult), CreateNodeResult} of
        {true, _} ->
            lager:debug("node creation ~p (~p)", [?NODE, self()]),
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
    end.

%%------------------------------------------------------------------------------------------------
%% User presence & caps
%%------------------------------------------------------------------------------------------------
send_presence(From, Type, To) ->
    ToJid = escalus_client:short_jid(To),
    Presence = escalus_stanza:presence_direct(ToJid, Type),
    escalus_client:send(From, Presence).

send_presence_with_caps(Client) ->
    Presence = escalus_stanza:presence(<<"available">>, [caps()]),
    escalus:send(Client, Presence).

caps() ->
    #xmlel{name  = <<"c">>,
           attrs = [{<<"xmlns">>, <<"http://jabber.org/protocol/caps">>},
                    {<<"hash">>, <<"sha-1">>},
                    {<<"node">>, <<"http://www.chatopus.com">>},
                    {<<"ver">>, ?CAPS_HASH}]}.

%%------------------------------------------------------------------------------------------------
%% Item publishing
%%------------------------------------------------------------------------------------------------
publish_pubsub_item(Client) ->
    Id = iq_id(publish, Client),
    PayloadSize = get_parameter(publication_size),
    Content = item_content(PayloadSize),
    Request = publish_pubsub_stanza(Client, Id, Content),
    escalus:send(Client, Request),
    {os:system_time(microsecond), Id}.

publish_pubsub_stanza(Client, Id, Content) ->
    ItemId = <<"current">>,
    escalus_pubsub_stanza:publish(Client, ItemId, Content, Id, ?NODE).

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
    case Entry of
        undefined -> ok;
        _ ->
            case {exml_query:attr(Entry, <<"jid">>), erlang:get(jid)} of
                {JID, JID} -> schedule_publishing(sync_rcv);
                _ -> ok
            end,
            TimeStampBin = exml_query:attr(Entry, <<"timestamp">>),
            TimeStamp = binary_to_integer(TimeStampBin),
            TTD = TS - TimeStamp,
            lager:debug("time to delivery ~p", [TTD]),
            amoc_metrics:update_counter(message),
            amoc_metrics:update_time(message_ttd, TTD)
    end.

process_presence(Client, Stanza) ->
    case exml_query:attr(Stanza, <<"type">>) of
        subscribe ->
            lager:debug("presence subscribe stanza ~p", [Stanza]),
            From = exml_query:attr(Stanza, <<"from">>),
            send_presence(Client, <<"subscribed">>, From);
        _ ->
            ok %%it's ok to just ignore other presence notifications
    end.

process_iq(Client, #xmlel{name = <<"iq">>} = Stanza, TS, Requests) ->
    Id = exml_query:attr(Stanza, <<"id">>),
    Type = exml_query:attr(Stanza, <<"type">>),
    NS = exml_query:path(Stanza, [{element, <<"query">>}, {attr, <<"xmlns">>}]),
    case {Type, NS, Id, maps:get(Id, Requests, undefined)} of
        {<<"get">>, ?NS_DISCO_INFO, _, undefined} ->
            handle_disco_query(Client, Stanza);
        {<<"set">>, ?NS_ROSTER, _, undefined} ->
            ok; %%it's ok to just ignore roster pushes
        {_, undefined, <<"publish", _/binary>>, undefined} ->
            lager:warning("unknown publish iq ~p", [Stanza]);
        {_, undefined, <<"publish", _/binary>>, {Tag, ReqTS}} ->
            handle_publish_resp(Stanza, {Tag, TS - ReqTS});
        _ ->
            lager:warning("unexpected iq ~p", [Stanza])
    end,
    maps:remove(Id, Requests).

handle_publish_resp(PublishResult, {Tag, PublishTime}) ->
    IqTimeout = get_parameter(iq_timeout),
    case escalus_pred:is_iq_result(PublishResult) of
        true ->
            schedule_publishing(sync_req),
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

handle_disco_query(Client, DiscoRequest) ->
    lager:debug("handle_disco_query ~p", [self()]),
    QueryEl = escalus_stanza:query_el(<<"http://jabber.org/protocol/disco#info">>,
                                      feature_elems()),
    DiscoResult = escalus_stanza:iq_result(DiscoRequest, [QueryEl]),
    escalus:send(Client, DiscoResult).

feature_elems() ->
    NodeNs = ?PEP_NODE_NS,
    [#xmlel{name  = <<"identity">>,
            attrs = [{<<"category">>, <<"client">>},
                     {<<"name">>, <<"Psi">>},
                     {<<"type">>, <<"pc">>}]},
     #xmlel{name  = <<"feature">>,
            attrs = [{<<"var">>, <<"http://jabber.org/protocol/disco#info">>}]},
     #xmlel{name  = <<"feature">>,
            attrs = [{<<"var">>, NodeNs}]},
     #xmlel{name  = <<"feature">>,
            attrs = [{<<"var">>, <<NodeNs/bitstring, "+notify">>}]}].
%%------------------------------------------------------------------------------------------------
%% Stanza helpers
%%------------------------------------------------------------------------------------------------
iq_id(Type, Client) ->
    UserName = escalus_utils:get_username(Client),
    Suffix = random_suffix(),
    list_to_binary(io_lib:format("~s-~s-~p",
                                 [Type, UserName, Suffix])).

random_suffix() ->
    Suffix = base64:encode(crypto:strong_rand_bytes(5)),
    re:replace(Suffix, "/", "_", [global, {return, binary}]).

%%------------------------------------------------------------------------------------------------
%% Config helpers
%%------------------------------------------------------------------------------------------------

set_scenario_settings() ->
    [set_parameter(P) || P <- ?ALL_PARAMETERS].

verify_scenario_settings() ->
    ParametersVerification = [begin
                                  set_parameter(P),
                                  verify_parameter(P)
                              end || P <- ?ALL_PARAMETERS],
    dump_settings(),
    case lists:all(fun(El) -> El end, ParametersVerification) of
        false -> exit(invalid_settings);
        _ -> ok
    end.

set_parameter({Name, Env, DefaultValue, _VerificationMethod}) ->
    erlang:put(Name, amoc_config:get(Env, DefaultValue)).

verify_parameter({Name, _Env, DefaultValue, VerificationMethod}) ->
    DefaultValueVerification = verify_parameter(DefaultValue, VerificationMethod),
    ValueVerification = verify_parameter(erlang:get(Name), VerificationMethod),
    case {DefaultValueVerification, ValueVerification} of
        {true, true} -> true;
        {true, false} ->
            lager:error("Invalid default value for ~p", [Name]),
            false;
        {false, true} ->
            lager:error("Invalid value for ~p", [Name]),
            false;
        {false, false} ->
            lager:error("Invalid default value & value for ~p", [Name]),
            false
    end.

verify_parameter(Value, positive_integer) ->
    is_positive_integer(Value);
verify_parameter(Value, nonnegative_integer) ->
    is_nonnegative_integer(Value);
verify_parameter(Value, bitstring) ->
    is_bitstring(Value);
verify_parameter(Value, [_ | _] = NonemptyList) ->
    is_one_of(Value, NonemptyList);
verify_parameter(_Value, VerificationMethod) ->
    lager:error("invalid verification method ~p", [VerificationMethod]),
    false.

is_positive_integer(I) -> is_integer(I) andalso I > 0.

is_nonnegative_integer(I) -> is_integer(I) andalso I >= 0.

is_one_of(Element, List) -> lists:any(fun(El) -> El =:= Element end, List).

dump_settings() ->
    Settings = [{Name, erlang:get(Name)} || {Name, _, _, _} <- ?ALL_PARAMETERS],
    lager:info("scenario settings: ~p", [Settings]).

get_parameter(Name) ->
    case is_one_of(Name, [N || {N, _, _, _} <- ?ALL_PARAMETERS]) of
        true -> erlang:get(Name);
        false ->
            lager:error("invalid parameter name ~p", [Name]),
            exit(invalid_parameter)
    end.

get_no_of_node_subscribers() ->
    %instead of constant No of subscriptions we can use min/max values.
    get_parameter(n_of_subscribers).




