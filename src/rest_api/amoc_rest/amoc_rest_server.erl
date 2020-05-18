-module(amoc_rest_server).


-define(DEFAULT_LOGIC_HANDLER, amoc_rest_default_logic_handler).

-export([start/2]).

-spec start( ID :: any(), #{
    ip                => inet:ip_address(),
    port              => inet:port_number(),
    logic_handler     => module(),
    net_opts          => [],
    cowboy_extra_opts => cowboy:opts()
}) -> {ok, pid()} | {error, any()}.

start(ID, #{
    ip            := IP ,
    port          := Port,
    net_opts      := NetOpts
} = Params) ->
    {Transport, TransportOpts} = get_socket_transport(IP, Port, NetOpts),
    LogicHandler = maps:get(logic_handler, Params, ?DEFAULT_LOGIC_HANDLER),
    ExtraOpts = maps:get(cowboy_extra_opts, Params, #{}),
    CowboyOpts = get_cowboy_config(LogicHandler, ExtraOpts),
    case Transport of
        ssl ->
            cowboy:start_tls(ID, TransportOpts, CowboyOpts);
        tcp ->
            cowboy:start_clear(ID, TransportOpts, CowboyOpts)
    end.

get_socket_transport(IP, Port, Options) ->
    Opts = [
        {ip,   IP},
        {port, Port}
    ],
    case amoc_rest_utils:get_opt(ssl, Options) of
        SslOpts = [_|_] ->
            {ssl, Opts ++ SslOpts};
        undefined ->
            {tcp, Opts}
    end.

get_cowboy_config(LogicHandler, ExtraOpts) ->
    DefaultOpts = get_default_opts(LogicHandler),
    DefaultEnv = maps:get(env, DefaultOpts, #{}),
    ExtraEnv = maps:get(env, ExtraOpts, #{}),
    Env = maps:merge(DefaultEnv, ExtraEnv),
    Opts = maps:merge(DefaultOpts, ExtraOpts),
    maps:put(env, Env, Opts).

get_default_dispatch(LogicHandler) ->
    Paths = amoc_rest_router:get_paths(LogicHandler),
    #{dispatch => cowboy_router:compile(Paths)}.

get_default_opts(LogicHandler) ->
    #{env => get_default_dispatch(LogicHandler)}.

