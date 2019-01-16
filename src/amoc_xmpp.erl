-module(amoc_xmpp).

-export([connect_or_exit/1]).

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

