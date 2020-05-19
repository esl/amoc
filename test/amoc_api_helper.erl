-module(amoc_api_helper).

-export([get/1, get/2, put/2, put/3, patch/2, patch/3]).

-spec get(string()) -> {integer(), jiffy:jiffy_decode_result()}. 
get(Path) -> get(get_url(), Path).

-spec get(string(), string()) -> {integer(), jiffy:jiffy_decode_result()}. 
get(BaseUrl, Path) -> 
    request(BaseUrl, erlang:list_to_bitstring(Path), <<"GET">>).

-spec put(string(), binary()) ->
    {integer(), jiffy:jiffy_decode_result()}.
put(Path, Body) -> put(get_url(), Path, Body).

-spec put(string(), string(), binary()) ->
    {integer(), jiffy:jiffy_decode_result()}.
put(BaseUrl, Path, Body) ->
    request(BaseUrl, erlang:list_to_bitstring(Path), <<"PUT">>, Body, <<"text/plain">>).

-spec patch(string(), binary()) ->
    {integer(), jiffy:jiffy_decode_result()}.
patch(Path, Body) -> patch(get_url(), Path, Body). 

-spec patch(string(), string(), binary()) ->
    {integer(), jiffy:jiffy_decode_result()}.
patch(BaseUrl, Path, Body) ->
    request(BaseUrl, erlang:list_to_bitstring(Path), <<"PATCH">>, Body).

-spec request(string(), binary(), binary()) -> 
    {integer(), jiffy:jiffy_decode_result()}.
request(BaseUrl, Path, Method) -> request(BaseUrl, Path, Method, <<"">>).

-spec request(string(), binary(), binary(), binary()) ->
    {integer(), jiffy:jiffy_decode_result()}.
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
                _ -> jiffy:decode(Body)
              end,
    fusco:disconnect(Client),
    {erlang:binary_to_integer(CodeHttpBin), BodyErl}.

-spec get_url() -> string().
get_url() ->
    Port = amoc_config_env:get(api_port, 4000),
    "http://localhost:" ++ erlang:integer_to_list(Port).
