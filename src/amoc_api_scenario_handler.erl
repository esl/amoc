-module(amoc_api_scenario_handler).

-export([init/3]).

-export([rest_init/2,
         allowed_methods/2,
         content_types_accepted/2,
         from_json/2,
         process_json/2]).

-record(state, {action}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, [Action]) ->
    {ok, Req, #state{action = Action}}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, from_json}
    ],Req, State}.

from_json(Req0, State) ->
    {ok, Data, Req1} = cowboy_req:body(Req0),
    try
        Term = jsx:decode(Data),
        {Status, Reply} = process_json(Term, State),
        lager:info(Reply),
        ContentType = {<<"content-type">>, <<"application/json">>},
        {ok,Req2} = cowboy_req:reply(Status,[ContentType], Reply, Req1),
        {halt, Req2, State}
    catch _:_ ->
        {ok,ReqE} = cowboy_req:reply(500, Req1),
        {false, ReqE, State}
    end.

%%%%%%%%%%%%%%
% Request processing functions
%%%%%%%%%%%%%%

process_json(Term, #state{action = start}) ->
    {<<"scenario">>, ScenarioB} = lists:keyfind(<<"scenario">>, 1, Term),
    {<<"users">>, _Users} = lists:keyfind(<<"users">>, 1, Term),
    
    Scenario = erlang:binary_to_atom(ScenarioB, utf8),
    ScenarioPath = "scenarios/" ++ erlang:atom_to_list(Scenario) ++ ".erl",
    
    {ok, Scenario} = compile:file(ScenarioPath,[{outdir,"ebin"}]),
    code:purge(Scenario),
    {module, Scenario} = code:load_file(Scenario),
    
    % _ = amoc_dist:do(Scenario, 1, Users),
    {200, jsx:encode([{ScenarioB,<<"ok">>}])};

process_json(_Term, #state{action = stop}) ->
    {200, jsx:encode("ok")};

process_json(Term, #state{action = load}) ->
    {<<"scenario">>, ScenarioB} = lists:keyfind(<<"scenario">>, 1, Term),
    {<<"module_bin">>, ModuleB} = lists:keyfind(<<"module_bin">>, 1, Term),
    {<<"overwrite">>, OverwriteB} = lists:keyfind(<<"overwrite">>, 1, Term),
    Scenario = erlang:binary_to_atom(ScenarioB, utf8),
    Overwrite = erlang:binary_to_atom(OverwriteB, utf8),
    ScenarioPath = "scenarios/" ++ erlang:atom_to_list(Scenario) ++ ".erl",
    
    case {file:open(ScenarioPath, [read]), Overwrite} of
        {_, true} -> 
            ok = file:write_file(ScenarioPath, ModuleB, [write]),
            {200, jsx:encode([{ScenarioB, <<"loaded">>}])};
        {{ok, Pid}, _} ->
            file:close(Pid),
            {501, jsx:encode([{ScenarioB, <<"already_exists">>}])}
    end;

process_json(_Term, #state{action = list}) ->
    {ok, Filenames} = file:list_dir("scenarios"),
    FilenamesB = [ list_to_binary(X) || X <- Filenames ],
    {200, jsx:encode([{<<"scenarios">>, FilenamesB}])}.
