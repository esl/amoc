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
    ContentType = {<<"content-type">>, <<"application/json">>},
    try
        Term = jsx:decode(Data),
        {Status, Reply} = process_json(Term, State),
        lager:info(Reply),
        {ok,Req2} = cowboy_req:reply(Status,[ContentType], Reply, Req1),
        {halt, Req2, State}
    catch _:_ ->
        ReplyE = jsx:encode([{<<"error">>,<<"unknown">>}]),
        {ok,ReqE} = cowboy_req:reply(500, [ContentType], ReplyE, Req1),
        {halt, ReqE, State}
    end.

%%%%%%%%%%%%%%
% Request processing functions
%%%%%%%%%%%%%%

process_json(Term, #state{action = start}) ->
   ScenarioB = proplists:get_value(<<"scenario">>, Term),
    Users = proplists:get_value(<<"users">>, Term),
    
    Scenario = erlang:binary_to_atom(ScenarioB, utf8),
    
    case code:load_file(Scenario) of
        {error,nofile} ->
            {500, jsx:encode([{<<"error">>,<<"module_not_exists">>}])};
        {module, Scenario} -> 
            _ = amoc_dist:do(Scenario, 1, Users),
            {200, jsx:encode([{ScenarioB,<<"ok">>}])}
    end;

process_json(_Term, #state{action = stop}) ->
    {200, jsx:encode("ok")};

process_json(Term, #state{action = load}) ->
    ScenarioB = proplists:get_value(<<"scenario">>, Term),
    ModuleB = proplists:get_value(<<"module_source">>, Term),
    Scenario = erlang:binary_to_atom(ScenarioB, utf8),
    ScenarioPath = "scenarios/" ++ erlang:atom_to_list(Scenario) ++ ".erl",
    ok = file:write_file(ScenarioPath, ModuleB, [write]),
    case compile:file(ScenarioPath,[{outdir,"ebin"}]) of
        {ok, Scenario} ->
            code:purge(Scenario),
            {200, jsx:encode([{ScenarioB, <<"loaded">>}])};
        error ->
            {500, jsx:encode([{<<"error">>, <<"compilation_error">>}])}
    end;

process_json(_Term, #state{action = ping_nodes}) ->
    Nodes = amoc_dist:ping_nodes(),
    {200, jsx:encode([{<<"nodes">>, Nodes}])};

process_json(_Term, #state{action = list}) ->
    {ok, Filenames} = file:list_dir("scenarios"),
    FilenamesB = [ list_to_binary(X) || X <- Filenames ],
    {200, jsx:encode([{<<"scenarios">>, FilenamesB}])}.
