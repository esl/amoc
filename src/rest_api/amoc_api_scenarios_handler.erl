-module(amoc_api_scenarios_handler).

-behavior(trails_handler).

-export([trails/0]).

-export([init/2]).

-export([allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-type state() :: [].

-spec trails() -> trails:trails().
trails() ->
    ResponseBodyGet =
    #{<<"200">> =>
      #{description => <<"response object">>,
        schema =>
        #{type => <<"object">>,
          required => [<<"scenarios">>],
          properties =>
          #{scenarios => #{<<"type">> => <<"array">>,
                           <<"items">> => #{<<"type">> => <<"string">>},
                           <<"description">> => <<"Scenario names">>
                         }
          }
        }
      }
    },

    Metadata =
    #{get =>
      #{tags => ["scenarios"],
        description => "Gets list of available scenarios",
        produces => ["application/json"],
        responses => ResponseBodyGet
      }
    },

    [trails:trail("/scenarios", ?MODULE, [], Metadata)].

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

%% Request processing functions
-spec to_json(cowboy_req:req(), state()) ->
    {iolist(), cowboy_req:req(), state()}.
to_json(Req0, State) ->
    Scenarios = amoc_scenario:list_scenario_modules(),
    Reply = jiffy:encode({[{scenarios, Scenarios}]}),
    {Reply, Req0, State}.

