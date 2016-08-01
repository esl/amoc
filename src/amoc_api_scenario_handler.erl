-module(amoc_api_scenario_handler).

-behavior(trails_handler).

-export([trails/0]).

-export([init/3]).

-export([rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         resource_exists/2,
         to_json/2,
         from_json/2]).


-record(state, {resource, users}).

-type state() :: #state{}.

-spec trails() -> trails:trails().
trails() ->
    Metadata =
    #{get =>
      #{tags => ["scenario"],
        description => "Gets scenario status",
        produces => ["application/json"]
       },
      patch =>
      #{tags => ["scenario"],
        description => "Starts scenario",
        produces => ["application/json"]
       }
     },
    [trails:trail("/scenarios/:id", ?MODULE, [], Metadata)].

-spec init(tuple(), cowboy_req:req(), state()) ->
    {upgrade, protocol, cowboy_rest}.
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

-spec rest_init(cowboy_req:req(), [atom()]) ->
    {ok, cowboy_req:req(), state()}.
rest_init(Req, _Opts) ->
    {ResourceB, Req2} = cowboy_req:binding(id, Req),
    Resource = erlang:binary_to_list(ResourceB),
    {ok, Req2, #state{resource = Resource}}.

-spec allowed_methods(cowboy_req:req(), state()) -> 
    {[binary()], cowboy_req:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"PATCH">>, <<"GET">>], Req, State}.

-spec content_types_provided(cowboy_req:req(), state()) -> 
    {[tuple()], cowboy_req:req(), state()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

-spec content_types_accepted(cowboy_req:req(), state()) -> 
    {[tuple()], cowboy_req:req(), state()}.
content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

-spec resource_exists(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State = #state{resource = Resource}) ->
    {ok, Files} = file:list_dir("scenarios/"),
    Pred = fun (File) -> File == Resource ++ ".erl" end,
    case lists:filter(Pred, Files) of
        [] ->
            {false, Req, State};
        [_File] ->
            {true, Req, State}
    end.



%% Request processing functions

-spec to_json(cowboy_req:req(), state()) -> 
    {jsx:json_text(), cowboy_req:req(), state()}.
to_json(Req0, State = #state{resource = Resource}) ->
    {ok, Status} = amoc_controller:test_status(
                     erlang:list_to_atom(Resource)),
    Reply = jsx:encode([{status, Status}]),
    {Reply, Req0, State}.


-spec from_json(cowboy_req:req(), state()) ->
    {boolean(), cowboy_req:req(), state()}.
from_json(Req, State = #state{resource = Resource}) ->
    case get_users_from_body(Req) of
        {ok, Users, Req2} ->
            Scenario = erlang:list_to_atom(Resource),
            _ = amoc_dist:do(Scenario, 1, Users),
            Reply = jsx:encode([{scenario, started}]),
            Req3 = cowboy_req:set_resp_body(Reply, Req2),
            {true, Req3, State};
        {error, bad_request, Req2} ->
            Reply = jsx:encode([{scenario, bad_request}]),
            Req3 = cowboy_req:set_resp_body(Reply, Req2),
            {false, Req3, State}
    end.


%% internal function
get_users_from_body(Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    try
        JSON = jsx:decode(Body),
        Users = proplists:get_value(<<"users">>, JSON),
        true = is_integer(Users),
        {ok, Users, Req2}
    catch _:_ ->
              {error, bad_request, Req2}
    end.

