-module(amoc_xmpp).

-export([connect_or_exit/1]).
-export([connect_or_exit/2]).
-export([pick_server/0]).

%% @doc connects and authenticates a user with given id and additional properties
%% If the passed proplist is empty, a default user spec a created by
%% make_user function is passed.
%% If the proplist is not empty it will be merged with the default props.
%% When a property is defined both in default and passed props,
%% the one from passed props is used.
-spec connect_or_exit(amoc_scenario:user_id(), [{atom(), any()}]) ->
    {ok, escalus_connection:client(), escalus_users:user_spec()}.
connect_or_exit(Id, ExtraSpec) ->
    Spec = make_user(Id, ExtraSpec),
    connect_or_exit(Spec).

%% @doc connects and authenticates a users based on passed user spec.
%% This function exits if the connection or authentication was not successful.
-spec connect_or_exit(escalus_users:user_spec()) ->
    {ok, escalus_connection:client(), escalus_users:user_spec()}.
connect_or_exit(Spec) ->
    {ConnectionTime, ConnectionResult} = timer:tc(escalus_connection, start, [Spec]),
    case ConnectionResult of
        {ok, _, _} = Result ->
            amoc_metrics:update_counter(connections),
            amoc_metrics:update_time(connection, ConnectionTime),
            Result;
        Error ->
            amoc_metrics:update_counter(connection_failures),
            lager:error("Could not connect user=~p, reason=~p", [Spec, Error]),
            exit(connection_failed)
    end.

%% @doc Picks a random server based on config var `xmpp_servers`
%% This function expect a list of proplists defining the endpoint
%% to which an XMPP client can connect, for instance:
%% [[{host, "127.0.0.1"}, {port, 5222}], [{host, "127.0.0.1"}, {port, 5223}]]
%% One of the above proplists is picked and can be added to user's spec.
%% It's required that the proplists contains at least the `host` property.
%% Since the `xmpp_servers` config option is read via `amoc_config` API,
%% it's possible to pass it as an ENV var when starting amoc:
%% > AMOC_xmpp_servers="[[{host,\"127.0.0.2\"}, {port, 5222}],[{host, \"127.0.0.1\"}, {port, 5223}]]" make console
%% Default value, if xmpp_servers option is not set, is [{host, "127.0.0.1"}]
-spec pick_server() -> [proplists:property()].
pick_server() ->
    Servers = amoc_config:get(xmpp_servers, [[{host, "127.0.0.1"}]]),
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

make_user(Id, Props) ->
    BinId = integer_to_binary(Id),
    ProfileId = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    DefaultSpec = maps:from_list(default_user_spec(ProfileId, Password)),
    ExtraSpec = maps:from_list(Props),
    Merged = maps:merge(DefaultSpec, ExtraSpec),
    maps:to_list(Merged).

-spec default_user_spec(binary(), binary()) -> escalus_users:user_spec().
default_user_spec(ProfileId, Password) ->
    [ {username, ProfileId},
      {server, <<"localhost">>},
      {host, <<"127.0.0.1">>},
      {password, Password},
      {carbons, false},
      {stream_management, false},
      {resource, base64:encode(crypto:strong_rand_bytes(5))}].

