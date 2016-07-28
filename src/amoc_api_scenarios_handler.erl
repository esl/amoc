-module(amoc_api_scenarios_handler).

-behavior(trails_handler).

-export([trails/0]).

-export([init/3]).

-export([rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         malformed_request/2,
         handle_get/2,
         handle_post/2]).


-record(state, {module_name, module_source}).

-type state() :: #state{}.

-spec trails() -> [tuple()] .
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

-spec init(tuple(), cowboy:req(), state()) -> {upgrade, protocol, cowboy_rest}.
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

-spec rest_init(cowboy:req(), [atom()]) -> {ok, cowboy:req(), state()}.
rest_init(Req, _Opts) ->
    {ok, Req, #state{}}.

-spec allowed_methods(cowboy:req(), state()) -> 
    {[binary()], cowboy:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"POST">>, <<"GET">>], Req, State}.

-spec content_types_provided(cowboy:req(), state()) -> 
    {[tuple()], cowboy:req(), state()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get}], Req, State}.

-spec content_types_accepted(cowboy:req(), state()) -> 
    {[tuple()], cowboy:req(), state()}.
content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_post}], Req, State}.

-spec malformed_request(cowboy:req(), state()) ->
    {boolean(), cowboy:req(), state()}.
malformed_request(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    malformed_request(Method, Req2, State).

malformed_request(<<"POST">>, Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case jsx:is_json(Body) of
        false ->
            {true, Req2, State};
        true ->
            JSON = jsx:decode(Body),
            ContainModuleName = proplists:is_defined(
                                  <<"scenario">>,
                                  JSON),
            ContainModuleSource = proplists:is_defined(
                                    <<"module_source">>,
                                    JSON),

            case ContainModuleName and ContainModuleSource of
                true ->
                    ModuleName = proplists:get_value(
                                   <<"scenario">>,
                                   JSON),
                    ModuleSource = proplists:get_value(
                                     <<"module_source">>,
                                     JSON),
                    State2 = State#state{
                               module_name = ModuleName,
                               module_source = ModuleSource
                               },
                    {false, Req2, State2};
                false ->
                    {true, Req2, State}
            end
    end;

malformed_request(_, Req, State) ->
    {false, Req, State}.


%% Request processing functions

-spec handle_get(cowboy:req(), state()) -> 
    {string() | halt, cowboy:req(), state()}.
handle_get(Req0, State = #state{}) ->
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


-spec handle_post(cowboy:req(), state()) ->
    {string() | halt | ok, cowboy:req(), state()}.
handle_post(Req0, State = #state{
                             module_name = ModuleName,
                             module_source = ModuleSource}) ->
    ScenarioPath = "scenarios/" ++ erlang:binary_to_list(ModuleName),

    file:write_file(
      ScenarioPath ++ ".erl",
      ModuleSource,
      [write]
     ),
    
    Result =
    case compile:file(ScenarioPath, [{outdir, ebin}]) of
        {ok, _} ->
            ok;
        error ->
            file:delete(ScenarioPath ++ ".erl"),
            error
    end,

    Reply = jsx:encode([{compile, Result}]),
    Req1 = cowboy_req:set_resp_body(Reply, Req0),
    {true, Req1, State}.

