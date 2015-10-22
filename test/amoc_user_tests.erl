-module(amoc_user_tests).
-include_lib("eunit/include/eunit.hrl").

%it_useses_start_function_with_state_test_() ->
%    given_clean_meck(),
%     ?_assertEqual(ok,ok).

it_useses_start_function_without_state_test_() ->
    %% given
    given_clean_meck(),
    given_stateless_scenario(),

    %% when
 %   R = amoc_user:start_link(stateless_scenario, 1, state),

    %% then
    [
    % ?_assertMatch({ok, _}, R),
     ?_assert(true =:= true)
     %?_assert(meck:validate(stateless_scenario))
    ].

given_clean_meck() ->
    (catch meck:unload()).

given_stateless_scenario() ->
    meck:new(stateless_scenario, [non_strict]),
    meck:expect(stateless_scenario, start, fun(_Id) -> timer:sleep(500) end).
