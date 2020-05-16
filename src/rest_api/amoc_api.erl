-module(amoc_api).

-export([start/0]).

-spec start() -> {ok, pid()} | {error, any()}.
start() ->
    amoc_rest_server:start(http_server, #{ip=>{0,0,0,0}, port=>4000, net_opts=>[]}).
