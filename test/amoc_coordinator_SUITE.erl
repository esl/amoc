-module(amoc_coordinator_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(MOCK_MOD, mock_mod).
-define(TELEMETRY_HANDLER, telemetry_handler).
-define(TELEMETRY_HANDLER_CONFIG, #{dummy_config => true}).


all() ->
    [execute_plan_without_timeout,
     reset_plan_without_timeout,
     execute_plan_with_timeout].

init_per_suite(Config) ->
    meck:new(?MOCK_MOD, [non_strict, no_link]),
    meck:expect(?MOCK_MOD, f_1, ['_', '_'], ok),
    meck:expect(?MOCK_MOD, f_2, ['_', '_', '_'], ok),
    meck:expect(?MOCK_MOD, f_3, ['_', '_', '_', '_'], ok),
    meck:new(?TELEMETRY_HANDLER, [non_strict, no_link]),
    meck:expect(?TELEMETRY_HANDLER, handler, ['_', '_', '_', '_'], ok),
    application:start(telemetry),
    TelemetryEvents = [[amoc, coordinator, Event] ||
                           Event <- [start, stop, timeout, reset, add]],
    TelemetryHandler = fun ?TELEMETRY_HANDLER:handler/4,
    telemetry:attach_many(?TELEMETRY_HANDLER, TelemetryEvents,
                           TelemetryHandler, ?TELEMETRY_HANDLER_CONFIG),
    Config.

end_per_suite(_Config) ->
    application:stop(telemetry),
    meck:unload().

init_per_testcase(_, Config) ->
    meck:reset(?MOCK_MOD),
    meck:reset(?TELEMETRY_HANDLER),
    Config.

end_per_testcase(_Config) ->
    ok.

execute_plan_without_timeout(_Config) ->
    N = 4, Name = ?FUNCTION_NAME,

    Plan = [Item1 = {2, [mocked_action(item1, 2), mocked_action(item1, 1)]},
            Item2 = {4, [mocked_action(item2, 3), mocked_action(item2, 1)]},
            All1 = {all, [mocked_action(all1, 2), mocked_action(all1, 1)]},
            Item3 = {3, mocked_action(item3, 1)},
            All2 = {all, mocked_action(all2, 1)}],

    ?assertEqual(ok, amoc_coordinator:start(Name, Plan, infinity)),
    [amoc_coordinator:add(Name, User) || User <- lists:seq(1, N)],

    amoc_coordinator:stop(Name),
    meck:wait(length(Plan), ?MOCK_MOD, f_1, ['_', {stop, '_'}], 1000),

    History = meck:history(?MOCK_MOD),
    [?assertEqual(stop, check_item_calls(History, Item, Tag, N)) ||
        {Item, Tag} <- [{Item1, item1}, {Item2, item2}, {Item3, item3},
                        {All1, all1}, {All2, all2}]],

    nothing_after_tags(History, [all1, all2]),
    assert_telemetry_events(Name, [start, {N, add}, stop]).

reset_plan_without_timeout(_Config) ->
    N1 = 5, N2 = 6, Name = ?FUNCTION_NAME,

    Plan = [All1 = {all, [mocked_action(all1, 2), mocked_action(all1, 1)]},
            Item1 = {2, [mocked_action(item1, 2), mocked_action(item1, 1)]},
            Item2 = {4, [mocked_action(item2, 3), mocked_action(item2, 1)]},
            All2 = {all, mocked_action(all2, 1)},
            Item3 = {3, mocked_action(item3, 1)}],

    ?assertEqual(ok, amoc_coordinator:start(Name, Plan, 1)), %% timeout is 1 second
    [amoc_coordinator:add(Name, User) || User <- lists:seq(1, N1)],

    amoc_coordinator:reset(Name),
    meck:wait(length(Plan), ?MOCK_MOD, f_1, ['_', {reset, '_'}], 1000),

    %% ensure that timeout doesn't occur after reset
    ?assertError(timeout, meck:wait(?MOCK_MOD, f_1, ['_', {timeout, '_'}], 2000)),

    History1 = meck:history(?MOCK_MOD),
    [?assertEqual(reset, check_item_calls(History1, Item, Tag, N1)) ||
        {Item, Tag} <- [{Item1, item1}, {Item2, item2}, {Item3, item3},
                        {All1, all1}, {All2, all2}]],

    nothing_after_tags(History1, [all1, all2]),

    meck:reset(?MOCK_MOD),
    [amoc_coordinator:add(Name, User) || User <- lists:seq(1, N2)],

    amoc_coordinator:reset(Name),
    meck:wait(length(Plan), ?MOCK_MOD, f_1, ['_', {reset, '_'}], 1000),

    %% ensure that timeout doesn't occur after reset
    ?assertError(timeout, meck:wait(?MOCK_MOD, f_1, ['_', {timeout, '_'}], 2000)),

    History2 = meck:history(?MOCK_MOD),
    [?assertEqual(reset, check_item_calls(History2, Item, Tag, N2)) ||
        {Item, Tag} <- [{Item1, item1}, {Item2, item2}, {Item3, item3},
                        {All1, all1}, {All2, all2}]],

    nothing_after_tags(History2, [all1, all2]),

    meck:reset(?MOCK_MOD),

    %% reset can be triggered twice in a row, and all the handlers
    %% are triggered twice in this case
    amoc_coordinator:reset(Name),
    meck:wait(length(Plan), ?MOCK_MOD, f_1, ['_', {reset, '_'}], 1000),

    %% ensure that timeout doesn't occur after reset
    ?assertError(timeout, meck:wait(?MOCK_MOD, f_1, ['_', {timeout, '_'}], 2000)),

    History3 = meck:history(?MOCK_MOD),
    [?assertEqual(reset, check_item_calls(History3, Item, Tag, 0)) ||
        {Item, Tag} <- [{Item1, item1}, {Item2, item2}, {Item3, item3},
                        {All1, all1}, {All2, all2}]],

    nothing_after_tags(History3, [all1, all2]),

    meck:reset(?MOCK_MOD),

    amoc_coordinator:stop(Name),
    meck:wait(length(Plan), ?MOCK_MOD, f_1, ['_', {stop, '_'}], 1000),

    History4 = meck:history(?MOCK_MOD),
    [?assertEqual(stop, check_item_calls(History4, Item, Tag, 0)) ||
        {Item, Tag} <- [{Item1, item1}, {Item2, item2}, {Item3, item3},
                        {All1, all1}, {All2, all2}]],

    nothing_after_tags(History4, [all1, all2]),

    assert_telemetry_events(Name, [start, {N1, add}, reset,
                                   {N2, add}, {2, reset}, stop]).


execute_plan_with_timeout(_Config) ->
    N1 = 7, N2 = 8, Name = ?FUNCTION_NAME,

    Plan = [Item1 = {2, [mocked_action(item1, 2), mocked_action(item1, 1)]},
            All1 = {all, [mocked_action(all1, 2), mocked_action(all1, 1)]},
            Item2 = {4, [mocked_action(item2, 3), mocked_action(item2, 1)]},
            All2 = {all, mocked_action(all2, 1)},
            Item3 = {3, mocked_action(item3, 1)}],

    ?assertEqual(ok, amoc_coordinator:start(Name, Plan, 1)), %% timeout is 1 second

    %% ensure there's no timeout happens if no users are added yet.
    ?assertError(timeout, meck:wait(?MOCK_MOD, f_1, ['_', {timeout, '_'}], 2000)),

    [amoc_coordinator:add(Name, User) || User <- lists:seq(1, N1)],

    meck:wait(length(Plan), ?MOCK_MOD, f_1, ['_', {timeout, '_'}], 2000),

    %% ensure that timeout occurs just once if no new users added.
    ?assertError(timeout, meck:wait(length(Plan) + 1, ?MOCK_MOD, f_1, ['_', {timeout, '_'}], 2000)),

    History1 = meck:history(?MOCK_MOD),
    [?assertEqual(timeout, check_item_calls(History1, Item, Tag, N1)) ||
        {Item, Tag} <- [{Item1, item1}, {Item2, item2}, {Item3, item3},
                        {All1, all1}, {All2, all2}]],

    nothing_after_tags(History1, [all1, all2]),

    meck:reset(?MOCK_MOD),
    [amoc_coordinator:add(Name, User) || User <- lists:seq(1, N2)],

    meck:wait(length(Plan), ?MOCK_MOD, f_1, ['_', {timeout, '_'}], 2000),

    %% ensure that timeout occurs just once if no new users added.
    ?assertError(timeout, meck:wait(length(Plan) + 1, ?MOCK_MOD, f_1, ['_', {timeout, '_'}], 2000)),

    History2 = meck:history(?MOCK_MOD),
    [?assertEqual(timeout, check_item_calls(History2, Item, Tag, N2)) ||
        {Item, Tag} <- [{Item1, item1}, {Item2, item2}, {Item3, item3},
                        {All1, all1}, {All2, all2}]],

    nothing_after_tags(History2, [all1, all2]),

    meck:reset(?MOCK_MOD),

    amoc_coordinator:stop(Name),
    meck:wait(length(Plan), ?MOCK_MOD, f_1, ['_', {stop, '_'}], 1000),

    History3 = meck:history(?MOCK_MOD),
    [?assertEqual(stop, check_item_calls(History3, Item, Tag, 0)) ||
        {Item, Tag} <- [{Item1, item1}, {Item2, item2}, {Item3, item3},
                        {All1, all1}, {All2, all2}]],

    nothing_after_tags(History2, [all1, all2]),

    assert_telemetry_events(Name, [start, {N1, add}, timeout,
                                   {N2, add}, timeout, stop]).


%% Helpers

mocked_action(Tag, 1) ->
    fun(Event) -> mock_mod:f_1(Tag, Event) end;
mocked_action(Tag, 2) ->
    Self = self(),
    fun(Event, ListOfUsers) ->
        UsersData = [Data || {Pid, Data} <- ListOfUsers, Pid =:= Self],
        mock_mod:f_2(Tag, Event, UsersData)
    end;
mocked_action(Tag, 3) ->
    Self = self(),
    fun(Event, {Pid, Data1}, {Pid, Data2}) when Pid =:= Self ->
           mock_mod:f_3(Tag, Event, Data1, Data2);
       (Event, {Pid, Data1}, undefined) when Pid =:= Self ->
           mock_mod:f_3(Tag, Event, Data1, undefined);
       (Event, undefined, undefined) ->
           mock_mod:f_3(Tag, Event, undefined, undefined)
    end.

nothing_after_tags(History, Tags) ->
    nothing_after_tags_(History, lists:reverse(Tags)).

nothing_after_tags_(_, []) -> ok;
nothing_after_tags_(History, [Tag | T]) ->
    CallsWithTag = nothing_after_tag(History, Tag),
    nothing_after_tags_(History -- CallsWithTag, T).

nothing_after_tag(History, Tag) ->
    CallsWithTag = lists:foldl(
        fun({_, {_, _, [Tag1 | _]}, _} = Call, Acc) when Tag =:= Tag1 -> [Call | Acc];
           (_, [_]) -> throw({calls_after_tag, Tag, History}); %% nonempty Acc, another tag
           (_, []) -> []
        end, [], History),
    ?assertNotMatch([], CallsWithTag, {tag, Tag}),
    CallsWithTag.

check_item_calls(CallsHistory, {all, Actions}, Tag, NoOfUsers) ->
    check_item_calls(CallsHistory, {NoOfUsers + 1, Actions}, Tag, NoOfUsers);
check_item_calls(CallsHistory, {N, Action}, Tag, NoOfUsers) when is_function(Action) ->
    check_item_calls(CallsHistory, {N, [Action]}, Tag, NoOfUsers);
check_item_calls(CallsHistory, {N, Actions}, Tag, NoOfUsers) ->
    check_filtered_calls(filter_history(CallsHistory, Tag), {N, Actions}, NoOfUsers, 1).

check_filtered_calls(CallsHistory, {N, Actions}, NoOfUsers, StartFrom) when N =< NoOfUsers ->
    {{coordinate, N}, NewCallsHistory} = check_actions(CallsHistory, Actions, N, StartFrom),
    check_filtered_calls(NewCallsHistory, {N, Actions}, NoOfUsers - N, StartFrom + N);
check_filtered_calls(CallsHistory, {_, Actions}, NoOfUsers, StartFrom) ->
    {{E, NoOfUsers}, []} = check_actions(CallsHistory, Actions, NoOfUsers, StartFrom),
    E.

filter_history(CallsHistory, Tag) ->
    [{M, F, A} || {_, {M, F, [Tag1 | A]}, _} <- CallsHistory, Tag1 == Tag].

check_actions([{_, _, [Event | _]} | _] = CallsHistory, Actions, N, StartFrom) ->
    check_actions(Event, CallsHistory, Actions, N, StartFrom).

check_actions(Event, CallsHistory, [], _N, _StartFrom) ->
    {Event, CallsHistory};
check_actions(Event, [{_, f_1, [Event]} | CT], [F | FT], N, StartFrom) when is_function(F, 1) ->
    check_actions(Event, CT, FT, N, StartFrom);
check_actions(Event, [{_, f_2, [Event, Args]} | CT], [F | FT],
              N, StartFrom) when is_function(F, 2) ->
    ?assertEqual(lists:sort(Args), seq(StartFrom, N)),
    check_actions(Event, CT, FT, N, StartFrom);
check_actions(Event, CallsHistory, [F | FT], N, StartFrom) when is_function(F, 3) ->
    Data = distinct_pairs(seq(StartFrom, N)),
    {CallsToVerify, RemainingCalls} = lists:split(length(Data), CallsHistory),
    DataToVerify = [{D1, D2} || {_, f_3, [E, D1, D2]} <- CallsToVerify, E =:= Event],
    ?assertEqual(sort_list_of_pairs(Data), sort_list_of_pairs(DataToVerify)),
    check_actions(Event, RemainingCalls, FT, N, StartFrom).

seq(StartFrom, N) ->
    seq_loop([], StartFrom, N).

seq_loop(Acc, _StartFrom, 0) ->
    Acc;
seq_loop(Acc, StartFrom, N) ->
    seq_loop([StartFrom + N - 1 | Acc], StartFrom, N - 1).

sort_list_of_pairs(List) ->
    lists:sort([{erlang:min(A, B), erlang:max(A, B)} || {A, B} <- List]).

extract_arguments(Plan) ->
    [A || {_Pid, {_M, _F, A}, ok} <- Plan].

distinct_pairs(List) ->
    distinct_pairs([], List).

distinct_pairs(_, []) ->
    [{undefined, undefined}];
distinct_pairs(_, [OneElement]) ->
    [{OneElement, undefined}];
distinct_pairs(Acc, [Element1, Element2]) ->
    [{Element1, Element2} | Acc];
distinct_pairs(Acc, [Element1 | Tail]) ->
    %% Tail has at least 2 elements
    NewAcc = [{Element1, Element2} || Element2 <- Tail] ++ Acc,
    distinct_pairs(NewAcc, Tail).

assert_telemetry_events(Name, EventList) ->
    History = meck:history(?TELEMETRY_HANDLER),
    ct:pal("meck history = ~p", [History]),
    UnfoldedEventList = unfold_event_list(EventList),
    assert_telemetry_events(Name, History, UnfoldedEventList),
    ok.

unfold_event_list(EventList) ->
    lists:flatten(
        [case E of
            {N, Event} when is_integer(N) andalso N > 0 ->
                [Event || _ <- lists:seq(1, N)];
            Event -> Event
        end || E <- EventList]).

assert_telemetry_events(_Name, [], []) -> ok;
assert_telemetry_events(_Name, History, EventList)
  when length(History) > length(EventList) ->
    ct:fail("unexpected telemetry events:~n ~p~n ~p", [History, EventList]);
assert_telemetry_events(_Name, History, EventList)
  when length(History) < length(EventList) ->
    ct:fail("missing telemetry events:~n ~p~n ~p", [History, EventList]);
assert_telemetry_events(Name, [{_Pid, Call, _Ret} | History], [Event | EventList]) ->
    assert_telemetry_handler_call(Name, Call, Event),
    assert_telemetry_events(Name, History, EventList).

assert_telemetry_handler_call(Name, Call, Event) ->
    EventName = [amoc, coordinator, Event],
    Measurements = #{count => 1},
    EventMetadata = #{name => Name},
    HandlerConfig = ?TELEMETRY_HANDLER_CONFIG,
    ExpectedHandlerCall = {?TELEMETRY_HANDLER, handler,
                           [EventName, Measurements,
                            EventMetadata, HandlerConfig]},
    ?assertEqual(ExpectedHandlerCall, Call).
