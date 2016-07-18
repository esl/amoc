-module(amoc_api).

-export([start_listener/0]).

-spec start_listener() -> {ok, pid()}.
start_listener() ->
    Port = application:get_env(amoc, api_port, 4000),
    Dispatch = cowboy_router:compile(routes()),
    {ok, _Pid} = cowboy:start_http(amoc_api, 10, [{port, Port}],
                                   [{env, [{dispatch, Dispatch}]}]
                                  ).

-spec routes() -> cowboy_router:routes().
routes() ->
    [{'_',
      [{"/start", amoc_api_handler, [start]},
       {"/stop", amoc_api_handler, [stop]},
       {"/status", amoc_api_handler, [status]}]
     }].
