-module(amoc_api_node_handler).

-behavior(trails_handler).

-export([trails/0]).

-export([init/3]).

-export([rest_init/2,
         allowed_methods/2,
         content_types_provided/2,
         status_response/2]).

-record(state, {action}).

-type state() :: #state{}.

-spec trails() -> [tuple()] .
trails() ->
    Metadata =
        #{get =>
          #{tags => ["node"],
            description => "Pings AMOC nodes from master node",
            produces => ["application/json"]
          }
    },
    [trails:trail("/nodes", amoc_api_node_handler, [], Metadata)].

-spec init(tuple(), cowboy:req(), state()) -> {upgrade, protocol, cowboy_rest}.
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

-spec rest_init(cowboy:req(), [atom()]) -> {ok, cowboy:req(), state()}.
rest_init(Req, [Action]) ->
    {ok, Req, #state{action = Action}}.

-spec allowed_methods(cowboy:req(), state()) -> 
        {[binary()], cowboy:req(), state()}.
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

-spec content_types_provided(cowboy:req(), state()) -> 
        {[tuple()], cowboy:req(), state()}.
content_types_provided(Req, State) ->
    {[{<<"application/json">>, status_response}, 
      {<<"text/plain">>, status_response},
      {<<"text/html">>, status_response}], Req, State}.


-spec content_types_accepted(cowboy:req(), state()) ->
    {[tuple()], cowboy:req(), state()}.
content_types_accepted(Req, State) ->
    {[{<<"application/json">>, process_request}], Req, State}.


%% TODO: do NOT use cowboy_req, let cowboy handle this
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
