-module(simple_rest_api).

-behavior(amoc_scenario).

-export([start/1]).
-export([init/0]).

-spec init() -> ok.
init() ->
    http_req:start(),
    ok.

-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    do_start(xmpp_or_rest(MyId), MyId).

do_start(rest, MyId) ->
    AuthHeader = auth_header(MyId),
    send_message(AuthHeader, MyId - 1),
    ok;
do_start(xmpp, MyId) ->
    ExtraProps = [{received_stanza_handlers, [fun log_message/2]}],

    {ok, Client, _} = amoc_xmpp:connect_or_exit(MyId, ExtraProps),

    escalus_session:send_presence_available(Client),

    escalus_connection:wait_forever(Client),

    escalus_connection:stop(Client).

log_message(_Client, Stanza) ->
    lager:warning("~p", [Stanza]),
    true.

send_message(AuthHeader, Id) ->
    Msg = #{to => ampc_xmpp_stanzas:make_jid(Id),
            body => <<"Hello, It's me">>
           },
    Headers = [AuthHeader,
               {<<"content-type">>, <<"application/json">>}],
    R = http_req:post_request("https://localhost:8089", <<"/api/messages">>, Headers, jiffy:encode(Msg)),
    lager:warning("~p", [R]).

auth_header(Id) ->
    Password = <<"password_", (integer_to_binary(Id))/binary>>,
    User = amoc_xmpp_users:make_jid(Id),
    UserAndPass = <<User/binary, ":", Password/binary>>,
    Base64  = base64:encode(UserAndPass),
    Basic = <<"basic ",Base64/binary>>,
    {<<"authorization">>, Basic}.

xmpp_or_rest(MyId) ->
    Id = MyId rem 2 + 1,
    element(Id, {rest, xmpp}).

