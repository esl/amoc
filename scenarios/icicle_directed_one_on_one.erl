%% The scenario herein allocates a TURN relay to each client, then
%% pairs them together: one sends data while the other receives data
%% through both of their respective relays. See notes under start/1
%% for details on the kind of network traffic to expect, i.e. when
%% monitoring this test.

-module(icicle_directed_one_on_one).

-export([init/0]).
-export([start/1]).

-define(SOCK_TABLE, sockets).
-define(PUT_TIME, 48 * 1000).

-define(USERNAME, <<"alice">>).
-define(PASSWORD, <<"ali">>).
-define(TEST_CREDENTIALS, {?USERNAME, ?PASSWORD}).

-define(TURN_PORT, 34780).
-define(REMOTE_INSTANCE, {54,172,65,222}).
-define(TEST_SERVER_SOCK, {?REMOTE_INSTANCE, ?TURN_PORT}).

-define(METRIC_NAME, [amoc, icicle, lapsetime]).

init() ->
    exometer:new(?METRIC_NAME, histogram),
    exometer_report:subscribe(exometer_report_graphite,
        ?METRIC_NAME, [mean, min, max, median, 95, 97], 10000),
    ets:new(?SOCK_TABLE, [public, named_table, set]),
    ok.

start(Id) when Id >= 0 ->
    %% NETWORK TRAFFIC: All procedures but the send/3 and
    %% get_message_buffer/1 make requests to a TURN server, expecting
    %% a response, so that's 2 datagrams for each. The send/3 will
    %% simply send 1 STUN formatted "indication", with NO response to
    %% be received. As for get_message_buffer/1, this is a call, but
    %% only to the generic server process (that is, the
    %% client). Clients are not stopped cleanly as they send and
    %% receive payloads indefinitely.
    {ok, Client} = icicle_client_server:start_link(?TEST_CREDENTIALS,
        ?TEST_SERVER_SOCK),
    failure_response = icicle_client_server:request_relay(Client),
    success_response = icicle_client_server:request_relay(Client),
    Sock = icicle_client_server:get_relay_address(Client),

    Peer = mock_out_of_band_rendezvous(Id, Sock),

    permission_success = icicle_client_server:set_permission(Client, Peer),
    {ok, 900} = icicle_client_server:set_allocation_lifetime(Client, 900),
    case parity(Id) of
	odd ->
	    send_forever(Client, Peer);
	even ->
	    receive_forever(Client)
    end.

sleep() ->
    timer:sleep(8*1000).

send_forever(Client, Peer) ->
    sleep(),
    Data = payload_with_timestamp(),
    icicle_client_server:send(Client, Data, Peer),
    send_forever(Client, Peer).

receive_forever(Client) ->
    case icicle_client_server:get_message_buffer(Client) of
	[] ->
	    ok;
	[{_Peer, Raw}] ->
	    sample_time_elapsed(Raw);
	Buff when is_list(Buff) ->
	    process_buffer(Buff)
    end,
    receive_forever(Client).

process_buffer([{_Peer, Raw}|Rest]) ->
    sample_time_elapsed(Raw),
    process_buffer(Rest);
process_buffer([]) ->
    ok.

mock_out_of_band_rendezvous(Id, Sock) ->
    mock_out_of_band_rendezvous(Id, Sock, ?PUT_TIME).

mock_out_of_band_rendezvous(Id, Sock, Wait) ->
    ok = put_client_in_table(Id, Sock),
    timer:sleep(Wait),
    {ok, Peer} = get_peer_from_table(Id),
    Peer.

put_client_in_table(Id, Sock) ->
    %% Make sure the mapping between client and their socket is only
    %% done once (to safeguard against anything strange happening).
    true  = ets:insert_new(?SOCK_TABLE, {Id, Sock}),
    lager:info("Id ~p put in table", [Id]),
    ok.

get_peer_from_table(Id) ->
    %% Why do we delete rows from the table in the second "case"?
    %% Because when `amoc_local:do/3' is done it's called again after
    %% some defualt time, enumerating the Id's as before, but the ETS
    %% table and continues to exist and holds these values. That would
    %% mess up the safeguard we put in place when inserting into the
    %% table.
    case parity(Id) of
	even ->
	    Peer = partner_one_down(Id);
	odd ->
	    Peer = partner_one_up(Id)
    end,
    case ets:lookup(?SOCK_TABLE, Peer) of
	[{Peer, Sock}] ->
	    true = ets:delete(?SOCK_TABLE, Peer),
	    {ok, Sock};
	[] ->
	    lager:info("Id ~p not in table", [Peer]),
	    {error, no_peer_in_table}
    end.

parity(Num) when is_integer(Num) ->
    case Num rem 2 =:= 0 of
	true ->
	    even;
	false ->
	    odd
    end.
	    
partner_one_down(Id) when is_integer(Id), Id > 0 ->
    Id - 1.

partner_one_up(Id) when is_integer(Id), Id > 0 ->
    Id + 1.

payload_with_timestamp() ->
    T = usec:from_now(os:timestamp()),
    integer_to_binary(T).

sample_time_elapsed(Raw) ->
    Time = calculate_time_elapsed(Raw),
    exometer:update(?METRIC_NAME, Time).

calculate_time_elapsed(Raw) when is_binary(Raw) ->
    Received = usec:from_now(os:timestamp()),
    Sent = binary_to_integer(Raw),
    _Lapsetime = Received - Sent.
