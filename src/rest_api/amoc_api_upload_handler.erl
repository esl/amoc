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
    [trails:trail("/scenarios/upload", ?MODULE, [], Metadata)].

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
            ResultBody =  process_multicall_results(Result),
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
    {error, atom(), cowboy_req:req()}.
get_vars_from_body(Req) ->
    {ok, ModuleSource, Req2} = cowboy_req:read_body(Req),
    case get_module_name(ModuleSource) of
        {ok, ModuleName} ->
            {ok, {ModuleName, ModuleSource}, Req2};
        {error, Reason} ->
            {error, Reason, Req2}
    end.

-spec get_module_name(binary()) -> {ok, atom()} | {error, atom()}.
get_module_name(SourceCode) ->
    try
        {match, [ModuleStr]} =
            re:run(SourceCode, "^\s*-\s*module.*$", [{newline, any},
                                                     multiline,
                                                     {capture, first, list}]),
        {ok, Tokens, _} = erl_scan:string(ModuleStr),
        {ok, {attribute, _, module, ModuleName}} = erl_parse:parse(Tokens),
        true = is_atom(ModuleName),
        {ok, ModuleName}
    catch _:_ ->
        {error, invalid_module}
    end.

install_scenario_on_nodes(Nodes, Module, ModuleSource) ->
    rpc:multicall(Nodes, amoc_scenario, install_scenario, [Module, ModuleSource]).

process_multicall_results({Results, []}) ->
    case lists:all(fun(X) -> X == ok end, Results) of
        true -> <<"ok">>;
        _ -> results_to_binary(Results)
    end;
process_multicall_results({Results, BadNodes}) ->
    BadNodesError = bad_nodes_to_binary(BadNodes),
    case process_multicall_results({Results, []}) of
        <<"ok">> -> BadNodesError;
        ErrorMsg -> <<ErrorMsg/binary, BadNodesError/binary>>
    end.

results_to_binary(Results) ->
    <<(result_to_binary(Res)) || Res <- Results>>.

result_to_binary(Result) ->
    ErrorsMsg = case Result of
                    ok -> [];
                    {badrpc, BadRPC} ->
                        io_lib:format("compilation errors: ~p~n", [BadRPC]);
                    {error, Errors, _Warnings} ->
                        io_lib:format("compilation errors: ~p~n", [Errors])
                end,
    erlang:list_to_binary(lists:flatten(ErrorsMsg)).

bad_nodes_to_binary(BadNodes) ->
    ErrorsMsg = io_lib:format("unreachable nodes: ~p~n", [BadNodes]),
    erlang:list_to_binary(lists:flatten(ErrorsMsg)).
