-module(amoc_api_scenario_handler).

-behavior(trails_handler).

-export([trails/0]).

-export([init/3]).

-export([rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         malformed_request/2,
         resource_exists/2,
         handle_get/2,
         handle_patch/2]).


-record(state, {resource, users}).

-type state() :: #state{}.

-spec trails() -> [tuple()] .
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

-spec init(tuple(), cowboy:req(), state()) -> {upgrade, protocol, cowboy_rest}.
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

-spec rest_init(cowboy:req(), [atom()]) -> {ok, cowboy:req(), state()}.
rest_init(Req, _Opts) ->
    {PathB, Req2} = cowboy_req:path(Req),
    Resource = get_resource_string(PathB),
    {ok, Req2, #state{resource = Resource}}.

-spec allowed_methods(cowboy:req(), state()) -> 
    {[binary()], cowboy:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"PATCH">>, <<"GET">>], Req, State}.

-spec content_types_provided(cowboy:req(), state()) -> 
    {[tuple()], cowboy:req(), state()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get}], Req, State}.

-spec content_types_accepted(cowboy:req(), state()) -> 
    {[tuple()], cowboy:req(), state()}.
content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_patch}], Req, State}.

-spec malformed_request(cowboy:req(), state()) ->
    {boolean(), cowboy:req(), state()}.
malformed_request(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    malformed_request(Method, Req2, State).

malformed_request(<<"PATCH">>, Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    case jsx:is_json(Body) of
        false ->
            {true, Req2, State};
        true ->
            JSON = jsx:decode(Body),
            ContainUsers = proplists:is_defined(<<"users">>, JSON),
            case ContainUsers of
                true ->
                    Users = proplists:get_value(<<"users">>, JSON),
                    State2 = State#state{users = Users},                    
                    {false, Req2, State2};
                false ->
                    {true, Req2, State}
            end
    end;

malformed_request(_, Req, State) ->
    {false, Req, State}.

-spec resource_exists(cowboy:req(), state()) ->
    {boolean(), cowboy:req(), state()}.
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

-spec handle_get(cowboy:req(), state()) -> 
    {string() | halt, cowboy:req(), state()}.
handle_get(Req0, State = #state{resource = Resource}) ->
    %% Need to retrive state of module here
    Reply = jsx:encode([{module, Resource}]),
    {Reply, Req0, State}.


-spec handle_patch(cowboy:req(), state()) ->
    {string() | halt | ok, cowboy:req(), state()}.
handle_patch(Req0, State = #state{resource = Resource, users = Users}) ->
    Scenario = erlang:list_to_atom(Resource),
    code:purge(Scenario),
    Result = code:load_file(Scenario),

    _ = amoc_dist:do(Scenario, 1, Users),

    Reply = jsx:encode([Result]),
    Req1 = cowboy_req:set_resp_body(Reply, Req0),
    {true, Req1, State}.

%% Internal
-spec get_resource_string(binary()) -> string().
get_resource_string(PathBinary) ->
    PathList = binary_to_list(PathBinary),
    PathSplit = string:tokens(PathList, "/"),
    lists:last(PathSplit).

