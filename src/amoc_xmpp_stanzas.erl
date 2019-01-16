-module(amoc_xmpp_stanzas).

-export([make_jid/1]).
-export([make_jid/2]).
-export([make_message/1]).
-export([make_message_with_timestamp/1]).

-spec make_jid(amoc_scenario:user_id()) -> binary().
make_jid(Id) ->
    make_jid(Id, <<"localhost">>).

-spec make_jid(amoc_scenario:user_id(), binary()) -> binary().
make_jid(Id, Host) ->
    BinInt = integer_to_binary(Id),
    ProfileId = <<"user_", BinInt/binary>>,
    << ProfileId/binary, "@", Host/binary >>.

-spec make_message(amoc_scenario:user_id() | binary()) -> exml:element().
make_message(ToId) when is_integer(ToId) ->
    ToJID = make_jid(ToId),
    make_message(ToJID);
make_message(ToJID) when is_binary(ToJID) ->
    Body = <<"hello sir, you are a gentelman and a scholar.">>,
    Id = escalus_stanza:id(),
    escalus_stanza:set_id(escalus_stanza:chat_to(ToJID, Body), Id).

-spec make_message_with_timestamp(amoc_scenario:user_id() | binary()) -> exml:element().
make_message_with_timestamp(ToIdOrJID) ->
    MsgIn = make_message(ToIdOrJID),
    TimeStamp = integer_to_binary(usec:from_now(os:timestamp())),
    escalus_stanza:setattr(MsgIn, <<"timestamp">>, TimeStamp).
