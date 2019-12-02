-module(amoc_api_scenario_handler).

-behavior(trails_handler).

-export([trails/0]).

-export([init/2]).

-export([allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         to_json/2,
         from_json/2]).

-export([test_status/1]).

-include_lib("kernel/include/logger.hrl").

-record(state, {resource, users}).

-type state() :: #state{}.

-type scenario_status() :: error | running | finished | loaded.

-spec trails() -> trails:trails().
trails() ->
    Id =
    #{name => <<"id">>,
      in => path,
      description => <<"Scenario id">>,
      type => string,
      required => true},

    RequestBody =
    #{name => <<"request body">>,
      in => body,
      description => <<"request body (as json)">>,
      required => true,
      schema =>
      #{type => <<"object">>,
        required => [<<"users">>],
        properties =>
        #{users => #{<<"type">> => <<"integer">>,
                     <<"description">> => <<"Number of users to start">>
                   }
        }
      }
    },

    ResponseBodyGet =
    #{<<"200">> =>
      #{description => <<"response object">>,
        schema =>
        #{type => <<"object">>,
          required => [<<"scenario_status">>],
          properties =>
          #{scenario_status => #{<<"type">> => <<"string">>,
                                 <<"description">> =>
                                   <<"loaded | running | finished">>
                               }
          }
        }
      }
    },

    ResponseBodyPatch =
    #{<<"200">> =>
      #{description => <<"response object">>,
        schema =>
        #{type => <<"object">>,
          required => [<<"scenario">>],
          properties =>
          #{scenario => #{<<"type">> => <<"string">>,
                          <<"description">> => <<"started | wrong_json">>
                        }
          }
        }
      }
    },

    Metadata =
    #{get =>
      #{tags => ["scenario"],
        description => "Gets scenario status",
        produces => ["application/json"],
        parameters => [Id],
        responses => ResponseBodyGet
      },
      patch =>
      #{tags => ["scenario"],
        description => "Starts scenario",
        produces => ["application/json"],
        parameters => [Id, RequestBody],
        responses => ResponseBodyPatch
      }
    },
    [trails:trail("/scenarios/:id", ?MODULE, [], Metadata)].

-spec init(cowboy_req:req(), state()) ->
    {cowboy_rest, cowboy_req:req(), state()}.
init(Req, _Opts) ->
    ResourceB = cowboy_req:binding(id, Req),
    Resource = erlang:binary_to_list(ResourceB),
    {cowboy_rest, Req, #state{resource = Resource}}.

-spec allowed_methods(cowboy_req:req(), state()) ->
    {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"PATCH">>, <<"GET">>], Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[tuple()], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[tuple()], cowboy_req:req(), state()}.
content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

-spec resource_exists(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State = #state{resource = Resource}) ->
    RetValue = try
                   Mod = erlang:list_to_existing_atom(Resource),
                   amoc_scenario:does_scenario_exist(Mod)
               catch
                   _:_ -> false
               end,
    {RetValue, Req, State}.


%% Request processing functions

-spec to_json(cowboy_req:req(), state()) ->
    {iolist(), cowboy_req:req(), state()}.
to_json(Req0, State = #state{resource = Resource}) ->
    Status = ?MODULE:test_status(erlang:list_to_atom(Resource)),
    Reply = jiffy:encode({[{scenario_status, Status}]}),
    {Reply, Req0, State}.


-spec from_json(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
from_json(Req, State = #state{resource = Resource}) ->
    case get_users_from_body(Req) of
        {ok, Users, Settings, Req2} ->
            Scenario = erlang:list_to_atom(Resource),
            ScenarioResult = amoc_dist:do(Scenario, Users, Settings),
            Reply = jiffy:encode({[{scenario, get_result(ScenarioResult)}]}),
            Req3 = cowboy_req:set_resp_body(Reply, Req2),
            {true, Req3, State};
        {error, bad_request, Req2} ->
            Reply = jiffy:encode({[{scenario, bad_request}]}),
            Req3 = cowboy_req:set_resp_body(Reply, Req2),
            {false, Req3, State}
    end.


%% internal function
-spec get_users_from_body(cowboy_req:req()) ->
    {ok, term(), amoc_config_scenario:settings(), cowboy_req:req()} |
    {error, bad_request, cowboy_req:req()}.
get_users_from_body(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    try
        {JSON} = jiffy:decode(Body),
        Users = proplists:get_value(<<"users">>, JSON),
        RawSettings = proplists:get_value(<<"settings">>, JSON, {[]}),
        true = is_integer(Users),
        {ok, Settings} = process_settings(RawSettings),
        {ok, Users, Settings, Req2}
    catch _:_ ->
              {error, bad_request, Req2}
    end.


process_settings({RawSettings}) ->
    Ret = [begin
               {ok, Key} = amoc_config_env:parse_value(K),
               {ok, Value} = amoc_config_env:parse_value(V),
               {Key, Value}
           end || {K, V} <- RawSettings],
    {ok, Ret}.

-spec get_result({ok, term()} | {error, term()}) -> started | error.
get_result({error,Error}) ->
    ?LOG_ERROR("Run scenario error: ~p", [Error]),
    error;
get_result({ok,_}) ->
    started.

-spec test_status(amoc:scenario()) -> scenario_status().
test_status(ScenarioName) ->
    Nodes = amoc_cluster:all_nodes(),
    Status = [get_node_test_status(ScenarioName, Node) || Node <- Nodes],
    pick_status(Status, [error, loaded, running, finished]).


-spec pick_status([scenario_status()], [scenario_status()]) -> scenario_status().
pick_status(StatusList, [H | T]) ->
    case lists:member(H, StatusList) of
        true -> H;
        false -> pick_status(StatusList, T)
    end.

-spec get_node_test_status(amoc:scenario(), atom()) -> disabled | scenario_status().
get_node_test_status(ScenarioName, Node) ->
    try
        case rpc:call(Node, amoc_controller, get_status, []) of
            {idle, Scenarios} ->
                case lists:member(ScenarioName, Scenarios) of
                    true -> loaded;
                    false -> error
                end;
            {running, ScenarioName, _, _} -> running;
            {finished, ScenarioName} -> finished;
            {error, _} -> error;
            disabled -> disabled;
            {badrpc, _} -> error
        end
    catch _:_ ->
        error
    end.