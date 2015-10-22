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

-type tag() :: binary().
-type state() :: #state{}.

-type command() :: {dist_do, amoc:scenario(), amoc_scenario:user_id(),
                    amoc_scenario:user_id(), amoc:do_opts()} |
                   {dist_add, non_neg_integer()} |
                   {dist_remove, non_neg_integer(), amoc:remove_opts()}.


%% ------------------------------------------------------------------
%% gen_event Function Definitions
%% ------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
    {ok, #state{}}.

-spec handle_event(command(), state()) ->{ok, state()}.
handle_event(Event, State) ->
    case annotation(Event) of
        {Tags, Format, Args} ->
            annotate(Tags, Format, Args);
        _ ->
            ok
    end,
    {ok, State}.

-spec handle_call(any(), state()) -> {ok, ok, state()}.
handle_call(_Request, State) ->
    {ok, ok, State}.

-spec handle_info(any(), state()) -> {ok, state()}.
handle_info(_Info, State) ->
    {ok, State}.

-spec terminate(any(), any()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(any(), any(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec annotate(tag(), list(), list(any())) -> {error, no_graphite} | ok.
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

-spec annotation(command()) -> {tag(), io:format(), list()}  | ignore.
annotation({dist_do, Scenario, Start, End, Opts}) ->
    Nodes = proplists:get_value(nodes, Opts, nodes()),
    Comment = proplists:get_value(comment, Opts, "none"),
    Format = "Scenario: ~p. comment: ~p Start: ~p. End: ~p. Nodes: ~p.",
    {<<"amoc start">>, Format, [Scenario, Comment, Start, End, length(Nodes)]};
annotation({dist_add, Count}) ->
    {<<"amoc add">>, "Added ~p users", [Count]};
annotation({dist_remove, Count, Opts}) ->
    {<<"amoc remove">>, "Removed ~p users. Opts: ~p", [Count, Opts]};
annotation(_) ->
    ignore.
