-module(amoc_xmpp_handlers).

-include_lib("exml/include/exml.hrl").

-export([measure_ttd/3]).
-export([measure_sent_messages/2]).

-spec measure_ttd(escalus_connection:client(), exml_stream:element(), map()) -> boolean().
measure_ttd(_Client, Stanza, Metadata) ->
    case Stanza of
        #xmlel{name = <<"message">>, attrs=Attrs} ->
            case lists:keyfind(<<"timestamp">>, 1, Attrs) of
                {_, Sent} ->
                    Now = maps:get(recv_timestamp, Metadata),
                    TTD = (Now - binary_to_integer(Sent)),
                    amoc_metrics:update_hist(amoc_metrics:message_ttd_histogram_name(), TTD);
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    true.

-spec measure_sent_messages(escalus_connection:client(), exml_stream:element()) -> boolean().
measure_sent_messages(_Client, #xmlel{name = <<"message">>}) ->
    amoc_metrics:update_spiral(amoc_metrics:messages_spiral_name(), 1),
    true;
measure_sent_messages(_, _) ->
    true.
