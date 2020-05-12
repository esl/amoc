-module(amoc_api_upload_handler).

-behavior(trails_handler).

-export([trails/0]).

-export([init/2]).

-export([allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_text/2]).

-type state() :: [].

-spec trails() -> trails:trails().
trails() ->
    ResponseBodyPut =
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
    Description = "Uploads new scenario (add '--upload-file"
                  " scenario.erl' flag to the curl command)",
    Metadata =
        #{
            put =>
                #{tags => ["upload"],
                  description => Description,
                  consumes => ["text/plain"],
                  produces => ["application/json"],
                  responses => ResponseBodyPut
                 }
         },
    [trails:trail("/upload", ?MODULE, [], Metadata)].

-spec init(cowboy_req:req(), state()) ->
    {cowboy_rest, cowboy_req:req(), state()}.
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

-spec allowed_methods(cowboy_req:req(), state()) ->
    {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"PUT">>], Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) ->
    {[tuple()], cowboy_req:req(), state()}.
content_types_accepted(Req, State) ->
    {[{<<"text/plain">>, from_text}], Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) ->
    {[tuple()], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, undefined}], Req, State}.

%% Request processing functions

-spec from_text(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
from_text(Req, State) ->
    case get_vars_from_body(Req) of
        {ok, {ModuleName, ModuleSource}, Req2} ->
            Nodes = amoc_cluster:all_nodes(),
            Result = install_scenario_on_nodes(Nodes, ModuleName, ModuleSource),
            ResultBody =  erlang:list_to_bitstring(
                            process_multicall_results(Nodes, Result)),
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
    {ok, {atom(), binary()}, cowboy_req:req()} |
    {error, wrong_json, cowboy_req:req()}.
get_vars_from_body(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    try
        ModuleName = get_module_name(Body),
        ModuleSource = Body,
        true = is_atom(ModuleName),
        true = is_binary(ModuleSource),
        {ok, {ModuleName, ModuleSource}, Req2}
    catch _:_ ->
        {error, invalid_module, Req2}
    end.

-spec get_module_name(binary()) -> atom().
get_module_name(SourceCode)->
    {match, [ModuleStr]} =
        re:run(SourceCode, "^\s*-\s*module.*$", [{newline, any},
                                                 multiline,
                                                 {capture, first, list}]),
    {ok, Tokens, _} = erl_scan:string(ModuleStr),
    {ok, {attribute, _, module, ModuleName}} = erl_parse:parse(Tokens),
    ModuleName.

install_scenario_on_nodes(Nodes, Module, ModuleSource) ->
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



