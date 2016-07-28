-module(amoc_api_handler).

-export([init/3]).

-export([rest_init/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
         process_json/2,
         status_response/2]).

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
    {[{<<"application/json">>, status_response}, 
      {<<"text/plain">>, status_response},
      {<<"text/html">>, status_response}], Req, State}.

-spec content_types_accepted(cowboy:req(), state()) -> 
        {[tuple()], cowboy:req(), state()}.
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

-spec from_json(cowboy:req(), state()) -> {true | false, cowboy:req(), state()}.
from_json(Req0, State) ->
    {ok, Data, Req1} = cowboy_req:body(Req0),
    try
        Term = jsx:decode(Data),
        ok = process_json(Term, State),
        {true, Req1, State}
    catch _:_ ->
        {false, Req1, State}
    end.

-spec process_json(term(), state()) -> ok.
process_json(Term, #state{action = start}) ->
    {<<"scenario">>, ScenarioB} = lists:keyfind(<<"scenario">>, 1, Term),
    {<<"users">>, Users} = lists:keyfind(<<"users">>, 1, Term),
    Scenario = erlang:binary_to_existing_atom(ScenarioB, utf8),
    _ = amoc_dist:do(Scenario, 1, Users),
    ok.

-spec status_response(cowboy:req(), state()) -> {atom(), cowboy:req(), state()}.
status_response(Req, State) ->
    Status = get_status(),
    Code = case Status of
             true -> 200;
             false -> 503
    end,
    {ok, Reply} = cowboy_req:reply(Code, Req),
    {halt, Reply, State}.

-spec get_status() -> true | false.
get_status() ->
    Results = application:which_applications(),
    Res = lists:keyfind(amoc, 1, Results),
    case Res of
        {amoc, _Desc, _Vsn} -> 
            true;
        false ->
            false
    end.
