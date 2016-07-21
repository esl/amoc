-module(amoc_api_test_handler).

-export([init/3]).

-export([rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         to_json/2,
         from_json/2]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _) ->
    {ok, Req, []}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, from_json}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json}
    ], Req, State}.

from_json(Req0, State) ->
    {ok, Data, Req1} = cowboy_req:body(Req0),
    [{<<"name">>, ScenarioName}] = jsx:decode(Data),

    try 
        ScenarioNameAtom = erlang:binary_to_existing_atom(ScenarioName, utf8),  
        Status = gen_event:call(amoc_event, amoc_test_event, 
                                {get_test, ScenarioNameAtom}),
        Status == undefined andalso error(not_exists),
        {ok, Req2} = cowboy_req:reply(200, [], 
                        jsx:encode([{ScenarioNameAtom, Status}]), Req1),
        {halt, Req2, State}
    catch _:_ ->
        {ok, ReqE} = cowboy_req:reply(500, Req1),
        {false, ReqE, State}
    end.

to_json(Req0, State) ->
    ResultErl = gen_event:call(amoc_event, amoc_test_event, get_all_tests),
    Response = jsx:encode(ResultErl),
    {ok, Req1} = cowboy_req:reply(200, [], Response, Req0),
    {halt, Req1, State}.
