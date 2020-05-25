%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_api_upload_scenario).
%% API
-export([upload/1]).

-spec upload(binary()) -> ok | {error, binary()}.
upload(ModuleSrc) ->
    case get_module_name(ModuleSrc) of
        {ok, ModuleName} ->
            Nodes = amoc_cluster:all_nodes(),
            Results = install_scenario_on_nodes(Nodes, ModuleName, ModuleSrc),
            process_multicall_results(Results);
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_module_name(binary()) -> {ok, atom()} | {error, binary()}.
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
        {error, <<"invalid_module">>}
    end.

install_scenario_on_nodes(Nodes, Module, ModuleSource) ->
    rpc:multicall(Nodes, amoc_scenario, install_scenario, [Module, ModuleSource]).

process_multicall_results({Results, []}) ->
    case lists:all(fun(X) -> X == ok end, Results) of
        true -> ok;
        _ ->
            ErrorMsg = results_to_binary(Results),
            {error, ErrorMsg}
    end;
process_multicall_results({Results, BadNodes}) ->
    BadNodesError = bad_nodes_to_binary(BadNodes),
    case process_multicall_results({Results, []}) of
        ok ->
            {error, BadNodesError};
        {error, ErrorMsg} ->

            {error, <<ErrorMsg/binary, BadNodesError/binary>>}
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
