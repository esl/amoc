-module(amoc_xmpp).

-export([connect_or_exit/1]).

-spec connect_or_exit(escalus_users:user_spec()) ->
    {ok, escalus_connection:client(), escalus_users:user_spec()}.
connect_or_exit(Spec) ->
    {ConnectionTime, ConnectionResult} = timer:tc(escalus_connection, start, [Spec]),
    case ConnectionResult of
        {ok, _, _} = Result ->
            amoc_metrics:update_counter(connections, 1),
            amoc_metrics:update_hist([times, connection], ConnectionTime),
            Result;
        Error ->
            exometer:update([amoc, counters, connection_failures], 1),
            lager:error("Could not connect user=~p, reason=~p", [Spec, Error]),
            exit(connection_failed)
    end.

