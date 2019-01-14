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
    Cfg = make_user(MyId, <<"xmpp-res">>),
    do_start(xmpp_or_rest(MyId), MyId, Cfg).

do_start(rest, MyId, Cfg) ->
    AuthHeader = auth_header(Cfg),
    send_message(AuthHeader, MyId - 1),
    ok;
do_start(xmpp, _MyId, Cfg) ->
    {ok, Client, _} = escalus_connection:start(Cfg),

    send_presence_available(Client),

    escalus_connection:wait_forever(Client),

    escalus_connection:stop(Client).


user_spec(ProfileId, Password, Res) ->
    [ {username, ProfileId},
      {server, host()},
      {host, pick_server()},
      {password, Password},
      {carbons, false},
      {stream_management, false},
      {resource, Res},
      {received_stanza_handlers, [fun log_message/2]}
    ].

log_message(_Client, Stanza) ->
    lager:warning("~p", [Stanza]),
    true.

send_message(AuthHeader, Id) ->
    Msg = #{to => full_jid(Id),
            body => <<"Hello, It's me">>
           },
    Headers = [AuthHeader,
               {<<"content-type">>, <<"application/json">>}],
    R = http_req:post_request("https://localhost:8089", <<"/api/messages">>, Headers, jiffy:encode(Msg)),
    lager:warning("~p", [R]).

auth_header(Cfg) ->
    Username = proplists:get_value(username, Cfg),
    Server = proplists:get_value(server, Cfg),
    Password = proplists:get_value(password, Cfg),
    User = <<Username/binary, "@", Server/binary>>,
    UserAndPass = <<User/binary, ":", Password/binary>>,
    Base64  = base64:encode(UserAndPass),
    Basic = <<"basic ",Base64/binary>>,
    {<<"authorization">>, Basic}.

-spec make_user(amoc_scenario:user_id(), binary()) -> escalus_users:user_spec().
make_user(Id, R) ->
    BinId = integer_to_binary(Id),
    ProfileId = username(Id),
    Password = <<"password_", BinId/binary>>,
    user_spec(ProfileId, Password, R).

username(BinId) when is_binary(BinId) ->
    <<"user_", BinId/binary>>;
username(Id) when is_integer(Id) ->
    username(integer_to_binary(Id)).

full_jid(Id) ->
    U = username(Id),
    H = host(),
    <<U/binary, "@", H/binary>>.

host() ->
    <<"localhost">>.

pick_server() ->
    "127.0.0.1".

xmpp_or_rest(MyId) ->
    Id = MyId rem 2 + 1,
    element(Id, {rest, xmpp}).

send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

