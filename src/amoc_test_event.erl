-module(amoc_test_event).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-spec init(term()) -> {ok, term()}.
init(_) ->
	{ok, []}.

-spec handle_event({atom(), atom()}, [tuple()]) -> {ok, [tuple()]}.
handle_event({test_begin, ScenarioName}, State) ->
	lager:info("Test started: ~p", [ScenarioName]),
	{ok, insert(ScenarioName, test_begin, State)};

handle_event({test_end, ScenarioName}, State) ->
	lager:info("Test ended: ~p", [ScenarioName]),
	{ok, insert(ScenarioName, test_end, State)};

handle_event(_, State) ->
	{ok, State}.	

-spec handle_call(atom(), [tuple()]) -> {ok, [tuple()], [tuple()]}.
handle_call(get_all_tests, State) ->
	{ok, State, State};

handle_call({get_test, ScenarioName}, State) ->
	{ok, find(ScenarioName, State), State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

-spec insert(atom(), atom(), [tuple()]) -> [tuple()].
insert(ScenarioName, Type, State) ->
	case lists:keymember(ScenarioName, 1, State) of
		true -> 
			lists:keyreplace(ScenarioName, 1, State, {ScenarioName, Type});
		false ->
			[{ScenarioName, Type}] ++ State
	end.

-spec find(atom(), [tuple()]) -> atom().
find(ScenarioName, State) ->
	Res = lists:keyfind(ScenarioName, 1, State),
	case Res of
		{ScenarioName, Status} ->
			Status;
		false ->
			undefined
	end.
