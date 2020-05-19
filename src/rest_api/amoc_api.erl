-module(amoc_api).

-export([start/0]).

-spec start() -> {ok, pid()} | {error, any()}.
start() ->
    LogicHandler = amoc_api_logic_handler,
    Routes = get_routes(LogicHandler),
    Dispatch = cowboy_router:compile(Routes),
    ServerParams = #{ip => {0, 0, 0, 0}, port => 4000, net_opts => [],
                     cowboy_extra_opts => #{env => #{dispatch => Dispatch}},
                     logic_handler => LogicHandler},
    amoc_rest_server:start(http_server, ServerParams).

get_routes(LogicHandler) ->
    %% this function adds static routing for swagger-ui
    [{'_', DefaultPaths}] = amoc_rest_router:get_paths(LogicHandler),
    [{'_', [
        {"/api-docs", cowboy_static, {priv_file, amoc, "swagger_ui/index.html"}},
        {"/api-docs/[...]", cowboy_static, {priv_dir, amoc, "swagger_ui"}},
        {"/openapi.json", cowboy_static, {priv_file, amoc, "openapi.json"}} |
        DefaultPaths]}].