-module(amoc_api_status_handler).

-behavior(trails_handler).

-export([trails/0]).

-export([init/2]).

-export([allowed_methods/2,
         content_types_provided/2,
         to_json/2,
         get_status/0]).

-type state() :: [].

-spec trails() -> trails:trails().
trails() ->
    ResponseBody =
    #{<<"200">> =>
      #{description => <<"response object">>,
        schema =>
        #{type => <<"object">>,
          required => [<<"node_status">>],
          properties =>
          #{node_status => #{<<"type">> => <<"string">>,
                             <<"description">> => <<"up | down">>
                           }
          }
        }
      }
    },

    Metadata =
    #{get =>
      #{tags => ["status"],
        description => "Gets AMOC status, whether it is running or not.",
        produces => ["application/json"],
        responses => ResponseBody
      }
    },
    [trails:trail("/status", ?MODULE, [], Metadata)].

-spec init(cowboy_req:req(), state()) ->
          {cowboy_rest, cowboy_req:req(), state()}.
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

-spec allowed_methods(cowboy_req:req(), state()) ->
        {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
        {[tuple()], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

-spec to_json(cowboy_req:req(), state()) ->
    {iolist(), cowboy_req:req(), state()}.
to_json(Req0, State) ->
    Status = get_status(),
    StatusJson = jiffy:encode({[{node_status, Status}]}),
    {StatusJson, Req0, State}.

-spec get_status() -> up | down.
get_status() ->
    Results = application:which_applications(),
    Res = lists:keyfind(amoc, 1, Results),
    case Res of
        {amoc, _Desc, _Vsn} -> up;
        false -> down
    end.
