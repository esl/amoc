%% The scenario herein allocates a TURN relay to each client, then
%% pairs them together: one sends data while the other receives data
%% through both of their respective relays. See notes under start/1
%% for details on the kind of network traffic to expect, i.e. when
%% monitoring this test.

-module(icicle_directed_one_on_one).

-export([init/0]).
-export([start/1]).

-export([add_load_in_thousands/1]).

-define(SOCK_TABLE, sockets).
-define(PUT_TIME, 20 * 1000).
-define(INIT_REFRESH_COUNT, 512 * 1000).
-define(METRIC_NAME, [amoc, icicle, lapsetime]).
-define(SEND_SLEEP_TIME, 1). %% This is in seconds.

-define(USERNAME, <<"alice">>).
-define(PASSWORD, <<"ali">>).
-define(TEST_CREDENTIALS, {?USERNAME, ?PASSWORD}).

-define(TURN_PORT, 34780).
-define(REMOTE_INSTANCE, {54,172,65,222}).
-define(LOCAL_INSTANCE, {127,0,0,1}).
-define(MIM_ZERO, {10,10,10,222}).
-define(TEST_SERVER_SOCK, {<<"TCP">>, ?MIM_ZERO, ?TURN_PORT}).

add_load_in_thousands(N) when is_integer(N), N >= 0 ->
    amoc_local:do(?MODULE, 1, 1000),
    repeat_load_in_thousands(N-1).

repeat_load_in_thousands(0) ->
    ok;
repeat_load_in_thousands(N) ->
    sleep(60), %% To assess just payload traffic.
    amoc_local:add(1000),
    repeat_load_in_thousands(N-1).

init() ->
    ok = exometer:new(?METRIC_NAME, histogram),
    ok = exometer_report:subscribe(exometer_report_graphite,
        ?METRIC_NAME, [mean, min, max, median, 95, 99], 10000),
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
    log_every_thousandth(Id),
    case parity(Id) of
	odd ->
	    send_forever(Client, Peer);
	even ->
	    receive_forever(Client, Peer)
    end.

send_forever(Client, Peer) ->
    service_forever(Client, Peer, fun send/2).

receive_forever(Client, Peer) ->
    service_forever(Client, Peer, fun recv/2).

send(Client, Peer) ->
    sleep(?SEND_SLEEP_TIME),
    Data = padded_payload_with_timestamp(),
    icicle_client_server:send(Client, Data, Peer).

recv(Client, Peer) ->
    case icicle_client_server:get_message_buffer(Client) of
	[] ->
	    ok;
	[{Peer, Raw}] ->
	    Bin = extract_just_timestamp(Raw),
	    sample_time_elapsed(Bin);
	Buff when is_list(Buff) ->
	    process_buffer(Buff)
    end.

service_forever(C, P, Action) ->
    Action(C, P),
    service_forever(C, P, Action).

process_buffer([{_Peer, Raw}|Rest]) ->
    Bin = extract_just_timestamp(Raw),
    sample_time_elapsed(Bin),
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
%    lager:info("Id ~p put in table", [Id]),
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

padded_payload_with_timestamp() ->
    Timestamp = payload_with_timestamp(),
    <<Timestamp/binary,0:(484*8)>>.

payload_with_timestamp() ->
    T = usec:from_now(os:timestamp()),
    integer_to_binary(T).

sample_time_elapsed(Raw) ->
    Time = calculate_time_elapsed(Raw),
    exometer:update(?METRIC_NAME, Time).

extract_just_timestamp(<<Timestamp:16/bytes,_/binary>>) ->
    Timestamp.

calculate_time_elapsed(Raw) when is_binary(Raw) ->
    Received = usec:from_now(os:timestamp()),
    Sent = binary_to_integer(Raw),
    _Lapsetime = Received - Sent.

sleep(Seconds) when is_integer(Seconds) ->
    timer:sleep(timer:seconds(Seconds)).

log_every_thousandth(Id) ->
    case Id rem 1000 =:= 0 of
        true ->
            lager:info("Up to ~p thousandth client.", [Id div 1000]);
	false ->
            ok
    end.
