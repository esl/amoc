-module(amoc_api_status_handler).

-behavior(trails_handler).

-export([trails/0]).

-export([init/3]).

-export([rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2,
         get_status/0]).

-type state() :: [].

-spec trails() -> trails:trails().
trails() ->
    Metadata =
        #{get =>
          #{tags => ["status"],
            description => "Gets AMOC status, whether it is running or not. Returns {\"status\": true | false}",
            produces => ["application/json"]
          }
    },
    [trails:trail("/status", amoc_api_status_handler, [], Metadata)].

-spec init(tuple(), cowboy:req(), state()) -> {upgrade, protocol, cowboy_rest}.
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

-spec rest_init(cowboy:req(), any()) -> {ok, cowboy:req(), state()}.
rest_init(Req, _) ->
    {ok, Req, []}.

-spec allowed_methods(cowboy:req(), state()) -> 
        {[binary()], cowboy:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

-spec content_types_provided(cowboy:req(), state()) -> 
        {[tuple()], cowboy:req(), state()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

-spec to_json(cowboy:req(), state()) ->
    {binary(), cowboy:req(), state()}.
to_json(Req0, State) ->
    Status = get_status(),
    StatusJson = jsx:encode([{result, Status}]),
    {StatusJson, Req0, State}.
    
-spec get_status() -> boolean().
get_status() ->
    Results = application:which_applications(),
    Res = lists:keyfind(amoc, 1, Results),
    case Res of
        {amoc, _Desc, _Vsn} -> true;
        false -> false
    end.
