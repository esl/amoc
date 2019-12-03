-module(amoc_api_scenarios_handler).

-behavior(trails_handler).

-export([trails/0]).

-export([init/2]).

-export([allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         to_json/2,
         from_json/2]).

-type state() :: [].

-spec trails() -> trails:trails().
trails() ->
    RequestBody =
    #{name => <<"request body">>,
      in => body,
      description => <<"request body (as json)">>,
      required => true,
      schema =>
      #{type => <<"object">>,
        required => [<<"scenario">>, <<"module_source">>],
        properties =>
        #{scenario => #{<<"type">> => <<"string">>,
                        <<"description">> =>
                            <<"Name of scenario module without .erl suffix">>
                      },
          module_source => #{<<"type">> => <<"string">>,
                             <<"description">> => <<"Source code of scenario">>
                           }
        }
      }
    },

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

    ResponseBodyPost =
    #{<<"200">> =>
      #{description => <<"response object">>,
        schema =>
        #{type => <<"object">>,
          required => [<<"compile">>],
          properties =>
          #{compile => #{<<"type">> => <<"string">>,
                          <<"description">> => <<"ok | Errors">>
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
      },
      post =>
      #{tags => ["scenarios"],
        description => "Uploads new scenario",
        produces => ["application/json"],
        parameters => [RequestBody],
        responses => ResponseBodyPost
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
    {[<<"POST">>, <<"GET">>], Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[tuple()], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[tuple()], cowboy_req:req(), state()}.
content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

%% Request processing functions

-spec to_json(cowboy_req:req(), state()) ->
    {iolist(), cowboy_req:req(), state()}.
to_json(Req0, State) ->
    Scenarios = amoc_scenario:list_scenario_modules(),
    Reply = jiffy:encode({[{scenarios, Scenarios}]}),
    {Reply, Req0, State}.


-spec from_json(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
from_json(Req, State) ->
    case get_vars_from_body(Req) of
        {ok, {ModuleName, ModuleSource}, Req2} ->
            Nodes = amoc_cluster:all_nodes(),
            ResultBody =  erlang:list_to_bitstring(
                            process_multicall_results(Nodes, install_scenario_on_nodes(Nodes, ModuleName, ModuleSource))),
            Reply = jiffy:encode({[{compile, ResultBody}]}),
            Req3 = cowboy_req:set_resp_body(Reply, Req2),
            {true, Req3, State};
        {error, Reason, Req2} ->
            Reply = jiffy:encode({[{error, Reason}]}),
            Req3 = cowboy_req:set_resp_body(Reply, Req2),
            {false, Req3, State}
    end.

%% internal function
-spec get_vars_from_body(cowboy_req:req()) ->
    {ok, {binary(), binary()}, cowboy_req:req()} |
    {error, wrong_json, cowboy_req:req()}.
get_vars_from_body(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    try
        {JSON} = jiffy:decode(Body),
        ModuleName = proplists:get_value(<<"scenario">>, JSON),
        ModuleSource = proplists:get_value(
                         <<"module_source">>,
                         JSON),
        true = is_binary(ModuleName),
        true = is_binary(ModuleSource),
        {ok, {ModuleName, ModuleSource}, Req2}
    catch _:_ ->
        {error, wrong_json, Req2}
    end.

install_scenario_on_nodes(Nodes, ModuleName, ModuleSource) ->
    Module=binary_to_atom(ModuleName,latin1),
    rpc:multicall(Nodes, amoc_scenario, install_scenario, [Module, ModuleSource]).

process_multicall_results(Nodes, {Results, BadNodes}) ->
    Errors = [{Node, Error} || {Node, {badrpc, Error}} <- lists:zip(Nodes -- BadNodes, Results)],
    Msg = case {BadNodes, Errors} of
        {[], []} ->
                  case lists:all(fun(X) -> X == ok end, Results) of
                      true ->
                          [<<"ok">>];
                      _ ->
                          [result_to_string(Res) || Res <- Results]
                  end;
        {[], _} ->
            process_reachable_nodes(Nodes, Errors);
        {_, _} ->
            [process_reachable_nodes(Nodes -- BadNodes, Errors)
             | io_lib:format("Error, unreachable nodes: ~p~n", [BadNodes])]
    end,
    lists:flatten(Msg).

result_to_string(Result) ->
    case Result of
        ok -> <<"ok">>;
        {error, Errors, _Warnings} ->
            R = io_lib:format("~p", [Errors]),
            erlang:list_to_bitstring(lists:flatten(R))
    end.

process_reachable_nodes([], _) ->
    [];
process_reachable_nodes(Nodes, Errors) ->
    ErrorNodes = [Node || {Node, _} <- Errors],
    SuccessNodes = Nodes -- ErrorNodes,
    SuccessMsg = case SuccessNodes of
                     [] ->
                         [];
                     _ ->
                         io_lib:format("Success on nodes: ~p~n", [SuccessNodes])
                 end,
    ErrorMsg = [node_error_message({Node, Error}) || {Node, Error} <- Errors],
    [SuccessMsg | ErrorMsg].

node_error_message({Node, Error}) ->
    io_lib:format("Error on node ~s: ~p~n", [Node, Error]).



