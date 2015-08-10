-module(icicle_one_on_one).

-export([init/0]).
-export([start/1]).

-define(USERNAME, <<"alice">>).
-define(PASSWORD, <<"ali">>).
-define(TEST_CREDENTIALS, {?USERNAME, ?PASSWORD}).

-define(TURN_PORT, 34780).
-define(REMOTE_INSTANCE, {54,172,65,222}).
-define(TEST_SERVER_SOCK, {?REMOTE_INSTANCE, ?TURN_PORT}).


init() ->

    ets:new(sockets, [public, named_table, set]),

    ok.

start(0) ->
    {error, ""};
start(Id) ->
    {ok, PID} = icicle_client_server:start_link(?TEST_CREDENTIALS,
        ?TEST_SERVER_SOCK),
    failure_response = icicle_client_server:request_relay(PID),
    success_response = icicle_client_server:request_relay(PID),

    Sock = icicle_client_server:get_relay_address(PID),
    %% === Below is meant to mock out of band comm =====
    ok = put_sock(Id, Sock),
    %% SLEEP FOR ALL PUT
    timer:sleep(32*1000),
    {ok, Partner} = get_partner_sock(Id),
    %% === Above is meant to mock out of band comm =====    

    ok = icicle_client_server:stop(PID).

parity(Num) when is_integer(Num) ->
    case Num rem 2 =:= 0 of
	true ->
	    even;
	false ->
	    odd
    end.

put_sock(Id, Sock) ->
    true  = ets:insert_new(sockets, {Id, Sock}),
    ok.

get_partner_sock(Id) ->
    Partner = case parity(Id) of
	even ->
	    partner_down(Id);
	odd ->
	    partner_up(Id)
    end,
    case ets:lookup(sockets, Partner) of
	[{Partner, Sock}] ->
	    {ok, Sock};
	_Otherwise ->
	    {error, ""}
    end.    
	    
partner_down(Id) when is_integer(Id), Id >= 1 ->
    Id - 1.

partner_up(Id) when is_integer(Id), Id >= 1 ->
    Id + 1.
