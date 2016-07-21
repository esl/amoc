-module(amoc_api).

-export([start_listener/0, stop/0]).

-spec start_listener() -> {ok, pid()}.
start_listener() ->
    Port = application:get_env(amoc, api_port, 4000),
    Dispatch = cowboy_router:compile(routes()),
    {ok, _Pid} = cowboy:start_http(amoc_api, 10, [{port, Port}],
                                   [{env, [{dispatch, Dispatch}]}]
                                  ).

-spec stop() -> ok | {error, not_found}.
stop() ->
    cowboy:stop_listener(amoc_api).

-spec routes() -> cowboy_router:routes().
routes() ->
    [{'_',
      [{"/start", amoc_api_scenario_handler, [start]},
       {"/stop", amoc_api_scenario_handler, [stop]},
       {"/list", amoc_api_scenario_handler, [list]},
       {"/load", amoc_api_scenario_handler, [load]},
       {"/ping_nodes", amoc_api_scenario_handler, [ping_nodes]},
       {"/status", amoc_api_handler, [status]},
         {"/test_status", amoc_api_test_handler, [test]}]
     }].
