-module(amoc_api_scenario_handler).

-export([init/3]).

-export([rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         process_request/2,
         process_json/2]).

-record(state, {action}).

-type state() :: #state{}.

-spec init(tuple(), cowboy:req(), state()) -> {upgrade, protocol, cowboy_rest}.
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

-spec rest_init(cowboy:req(), [atom()]) -> {ok, cowboy:req(), state()}.
rest_init(Req, [Action]) ->
    {ok, Req, #state{action = Action}}.

-spec allowed_methods(cowboy:req(), state()) -> 
        {[binary()], cowboy:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"GET">>], Req, State}.

-spec content_types_provided(cowboy:req(), state()) -> 
        {[tuple()], cowboy:req(), state()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, process_request}], Req, State}.

-spec content_types_accepted(cowboy:req(), state()) -> 
        {[tuple()], cowboy:req(), state()}.
content_types_accepted(Req, State) ->
    {[{<<"application/json">>, process_request}], Req, State}.

-spec process_request(cowboy:req(), state()) -> 
    {halt, cowboy:req(), state()}.
process_request(Req0, State) ->
    {ok, Data, Req1} = cowboy_req:body(Req0),
    ContentType = {<<"content-type">>, <<"application/json">>},
    try
        Term = case jsx:is_json(Data) of
            true -> jsx:decode(Data);
            false -> undef
        end,
        {Status, Reply} = process_json(Term, State),
        lager:info(Reply),
        {ok, Req2} = cowboy_req:reply(Status, [ContentType], Reply, Req1),
        {halt, Req2, State}
    catch _:_ ->
        ReplyE = jsx:encode([{<<"error">>, <<"unknown">>}]),
        {ok, ReqE} = cowboy_req:reply(500, [ContentType], ReplyE, Req1),
        {halt, ReqE, State}
    end.

%%%%%%%%%%%%%%
% Request processing functions
%%%%%%%%%%%%%%
-spec process_json(jsx:json_term(), state()) -> {integer(), jsx:json_text()}.
process_json(Term, #state{action = start}) ->
   ScenarioB = proplists:get_value(<<"scenario">>, Term),
    Users = proplists:get_value(<<"users">>, Term),
    
    Scenario = erlang:binary_to_atom(ScenarioB, utf8),
    
    case code:load_file(Scenario) of
        {error,nofile} ->
            {500, jsx:encode([{<<"error">>, <<"module_not_exists">>}])};
        {module, Scenario} -> 
            _ = amoc_dist:do(Scenario, 1, Users),
            {200, jsx:encode([{ScenarioB, <<"ok">>}])}
    end;

process_json(_Term, #state{action = stop}) ->
    {200, jsx:encode("ok")};

process_json(Term, #state{action = load}) ->
    ScenarioB = proplists:get_value(<<"scenario">>, Term),
    ModuleB = proplists:get_value(<<"module_source">>, Term),
    Scenario = erlang:binary_to_atom(ScenarioB, utf8),
    ScenarioPath = "scenarios/" ++ erlang:atom_to_list(Scenario) ++ ".erl",
    ok = file:write_file(ScenarioPath, ModuleB, [write]),
    case compile:file(ScenarioPath, [{outdir, "ebin"}]) of
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
