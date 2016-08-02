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
            description => "Gets AMOC status, whether it is running or not. 
                            Returns {\"status\": true | false}",
            produces => ["application/json"]
          }
    },
    [trails:trail("/status", amoc_api_status_handler, [], Metadata)].

-spec init(tuple(), cowboy_req:req(), state()) -> 
          {upgrade, protocol, cowboy_rest}.
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

-spec rest_init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), state()}.
rest_init(Req, _) ->
    {ok, Req, []}.

-spec allowed_methods(cowboy_req:req(), state()) -> 
        {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) -> 
        {[tuple()], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

-spec to_json(cowboy_req:req(), state()) ->
    {binary(), cowboy_req:req(), state()}.
to_json(Req0, State) ->
    Status = get_status(),
    StatusJson = jsx:encode([{node_status, Status}]),
    {StatusJson, Req0, State}.
    
-spec get_status() -> boolean().
get_status() ->
    Results = application:which_applications(),
    Res = lists:keyfind(amoc, 1, Results),
    case Res of
        {amoc, _Desc, _Vsn} -> up;
        false -> down
    end.
