-module(amoc_api_scenarios_handler).

-behavior(trails_handler).

-export([trails/0]).

-export([init/3]).

-export([rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         to_json/2,
         from_json/2]).


-record(state, {module_name, module_source}).

-type state() :: #state{}.

-spec trails() -> trails:trails().
trails() ->
    Metadata =
    #{get =>
      #{tags => ["scenarios"],
        description => "Gets list of available scenarios",
        produces => ["application/json"]
       },
      post =>
      #{tags => ["scenarios"],
        description => "Uploads new scenario",
        produces => ["application/json"]
       }
     },
    [trails:trail("/scenarios", ?MODULE, [], Metadata)].

-spec init(tuple(), cowboy_req:req(), state()) ->
    {upgrade, protocol, cowboy_rest}.
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

-spec rest_init(cowboy_req:req(), [atom()]) -> {ok, cowboy_req:req(), state()}.
rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

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
    {jsx:json_text(), cowboy_req:req(), state()}.
to_json(Req0, State = #state{}) ->
    {ok, Filenames} = file:list_dir("scenarios"),
    Filenames2 =
        lists:filter(
          fun(X) -> string:right(X, 3) == "erl" end,
          Filenames),

    Scenarios = 
    [ erlang:list_to_binary(Y) ||  X <- Filenames2,
                                   Y <- string:tokens(X, "."),
                                   Y =/= "erl" ],
    Reply = jsx:encode([{scenarios, Scenarios}]),
    {Reply, Req0, State}.


-spec from_json(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
from_json(Req, #state{}) ->
    case get_vars_from_body(Req) of
        {ok, State, Req2} ->
            ModuleName = State#state.module_name,
            ModuleSource = State#state.module_source,
            ScenarioPath = "scenarios/" ++ erlang:binary_to_list(ModuleName),
            file:write_file(
              ScenarioPath ++ ".erl",
              ModuleSource,
              [write]
             ),
            Result = compile_and_load_scenario(
                       ModuleName,
                       ScenarioPath),
            Reply = jsx:encode([{compile, Result}]),
            Req3 = cowboy_req:set_resp_body(Reply, Req2),
            {true, Req3, State};
        {error, Reason, Req2} ->
            Reply = jsx:encode([{error, Reason}]),
            Req3 = cowboy_req:set_resp_body(Reply, Req2),
            {false, Req3, #state{}}
    end.

%% internal function
-spec get_vars_from_body(cowboy_req:req()) ->
    {ok | error, state() | wrong_json, cowboy_req:req()}.
get_vars_from_body(Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    try
        JSON = jsx:decode(Body),
        ModuleName = proplists:get_value(<<"scenario">>, JSON),
        ModuleSource = proplists:get_value(
                         <<"module_source">>,
                         JSON),
        true = is_binary(ModuleName),
        true = is_binary(ModuleSource),
        State = #state{
                   module_name = ModuleName,
                   module_source = ModuleSource},
        {ok, State, Req2}
    catch _:_ ->
              {error, wrong_json, Req2}
    end.

-spec compile_and_load_scenario(binary(), string()) ->
    ok | error.
compile_and_load_scenario(BinModuleName, ScenarioPath) ->
    case compile:file(ScenarioPath, [{outdir, ebin}]) of
        {ok, _} ->
            Module = erlang:binary_to_atom(BinModuleName, utf8),
            code:load_file(Module),
            ok;
        error ->
            file:delete(ScenarioPath ++ ".erl"),
            error
    end.


