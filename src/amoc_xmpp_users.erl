-module(amoc_xmpp_users).

-export([make_jid/1]).
-export([make_jid/2]).

-spec make_jid(amoc_scenario:user_id()) -> binary().
make_jid(Id) ->
    make_jid(Id, <<"localhost">>).

-spec make_jid(amoc_scenario:user_id(), binary()) -> binary().
make_jid(Id, Host) ->
    ProfileId = username(Id),
    << ProfileId/binary, "@", Host/binary >>.

username(Id) ->
    BinInt = integer_to_binary(Id),
    <<"user_", BinInt/binary>>.

