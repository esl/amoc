%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_annotations).
-behaviour(gen_event).

%% ------------------------------------------------------------------
%% gen_event Function Exports
%% ------------------------------------------------------------------
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%% ------------------------------------------------------------------
%% gen_event Function Definitions
%% ------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

handle_event(Event, State) ->
    case annotation(Event) of
        {Tags, Format, Args} ->
            annotate(Tags, Format, Args);
        _ ->
            ok
    end,
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
annotate(Tags, Format, Args) ->
    case application:get_env(amoc, graphite_endpoint) of
        undefined ->
            {error, no_graphite};
        {ok, Host} ->
            What = iolist_to_binary(io_lib:format(Format, Args)),
            Struct = {struct, [{what, What}, {tags, Tags}]},
            Json = mochijson2:encode(Struct),
            Path = "http://" ++ Host ++ "/events/",
            Response = lhttpc:request(Path, "POST", [], Json, 5000),
            {ok, {{200, _}, _, _}} = Response,
            ok
    end.

annotation({dist_do, Scenario, Start, End, Nodes}) ->
    {<<"amoc start">>, "Scenario: ~p. Start: ~p. End: ~p. Nodes: ~p.",
     [Scenario, Start, End, length(Nodes)]};
annotation({dist_add, Count}) ->
    {<<"amoc add">>, "Added ~p users", [Count]};
annotation({dist_remove, Count, Opts}) ->
    {<<"amoc remove">>, "Removed ~p users. Opts: ~p", [Count, Opts]};
annotation(_) ->
    ignore.
