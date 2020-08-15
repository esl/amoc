-module(amoc_api_helper).

-export([get/1, put/2, patch/1, patch/2,
         remove_module/1, module_src/1, module_beam/1,
         start_amoc/0, stop_amoc/0]).

-type json() :: jsx:json_term().


-spec start_amoc() -> any().
start_amoc() ->
    application:ensure_all_started(amoc).

-spec stop_amoc() -> any().
stop_amoc() ->
    application:stop(inets),
    application:stop(amoc),
    amoc_api:stop().

-spec remove_module(module()) -> any().
remove_module(M) ->
    erlang:delete_module(M),
    erlang:purge_module(M),
    ok = file:delete(module_src(M)),
    ok = file:delete(module_beam(M)).

-spec module_src(module()) -> string().
module_src(M) ->
    CodeDir = filename:join(code:priv_dir(amoc), "scenarios"),
    filename:join([CodeDir, atom_to_list(M) ++ ".erl"]).

-spec module_beam(module()) -> string().
module_beam(M) ->
    BeamDir = filename:join(code:priv_dir(amoc), "scenarios_ebin"),
    filename:join([BeamDir, atom_to_list(M) ++ ".beam"]).


-spec get(string()) -> {integer(), json()}.
get(Path) -> get(get_url(), Path).

-spec get(string(), string()) -> {integer(), json()}.
get(BaseUrl, Path) ->
    request(BaseUrl, erlang:list_to_bitstring(Path), <<"GET">>).

-spec put(string(), binary()) ->
    {integer(), json()}.
put(Path, Body) -> put(get_url(), Path, Body).

-spec put(string(), string(), binary()) ->
    {integer(), json()}.
put(BaseUrl, Path, Body) ->
    request(BaseUrl, erlang:list_to_bitstring(Path), <<"PUT">>, Body, <<"text/plain">>).

-spec patch(string()) ->
    {integer(), json()}.
patch(Path) -> patch(get_url(), Path, <<"">>).

-spec patch(string(), json()) ->
    {integer(), json()}.
patch(Path, JSON) -> patch(get_url(), Path, jsx:encode(JSON)).

-spec patch(string(), string(), binary()) ->
    {integer(), json()}.
patch(BaseUrl, Path, Body) ->
    request(BaseUrl, erlang:list_to_bitstring(Path), <<"PATCH">>, Body).

-spec request(string(), binary(), binary()) ->
    {integer(), json()}.
request(BaseUrl, Path, Method) -> request(BaseUrl, Path, Method, <<"">>).

-spec request(string(), binary(), binary(), binary()) ->
    {integer(), json()}.
request(BaseUrl, Path, Method, RequestBody) ->
    request(BaseUrl, Path, Method, RequestBody, <<"application/json">>).

request(BaseUrl, Path, Method, RequestBody, ContentType) ->
    {ok, Client} = fusco:start(BaseUrl, []),
    {ok, Result} = fusco:request(
                    Client, Path, Method,
                    [{<<"content-type">>, ContentType}],
                    RequestBody, 5000),
    {{CodeHttpBin, _}, _Headers, Body, _, _} = Result,
    BodyErl = case Body of
                  <<"">> -> [];
                  _ -> jsx:decode(Body, [return_maps])
              end,
    fusco:disconnect(Client),
    {erlang:binary_to_integer(CodeHttpBin), BodyErl}.

-spec get_url() -> string().
get_url() ->
    Port = amoc_config_env:get(api_port, 4000),
    "http://localhost:" ++ erlang:integer_to_list(Port).
