-module(amoc_api_handler).

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
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

from_json(Req0, State) ->
    {ok, Data, Req1} = cowboy_req:body(Req0),
    try
        Term = jsx:decode(Data),
        ok = process_json(Term, State),
        {true, Req1, State}
    catch _:_ ->
        {false, Req1, State}
    end.

process_json(Term, #state{action = start}) ->
    {<<"scenario">>, ScenarioB} = lists:keyfind(<<"scenario">>, 1, Term),
    {<<"users">>, Users} = lists:keyfind(<<"users">>, 1, Term),
    Scenario = erlang:binary_to_existing_atom(ScenarioB, utf8),
    _ = amoc_dist:do(Scenario, 1, Users),
    ok.
