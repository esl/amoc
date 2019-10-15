-module(amoc_api_node_handler).

-behavior(trails_handler).

-export([trails/0]).

-export([init/2]).

-export([allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-type state() :: [].

-spec trails() -> [trails:trail()].
trails() ->
    ResponseBody =
    #{<<"200">> =>
      #{description => <<"response object">>,
        schema =>
          #{type => <<"object">>,
            required => [<<"nodes">>],
            properties =>
            #{nodes => #{<<"type">> => <<"object">>,
                         <<"description">> => <<"nodeName: up | down">>
                       }
            }
          }
      }
    },

    Metadata =
    #{get =>
      #{tags => ["node"],
        description => "Pings AMOC nodes from master node.",
        produces => ["application/json"],
        responses => ResponseBody
      }
    },
    [trails:trail("/nodes", ?MODULE, [], Metadata)].

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
to_json(Req, State) ->
    Nodes = amoc_dist:ping_nodes(),
    ResponseList = lists:map(
                       fun({X, pong}) -> {X, up};
                          ({X, pang}) -> {X, down}
                        end, Nodes),
    {jiffy:encode({[{<<"nodes">>, {ResponseList}}]}), Req, State}.
