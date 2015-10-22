-module(amoc_user_registry_tests).
-include_lib("eunit/include/eunit.hrl").

initial_count_is_zero_test_() ->
    given_user_registry(),
    ?_assertEqual(0, amoc_user_registry:count()).

count_chages_when_adding_or_deleting_users_test_() ->
    given_user_registry(),
    amoc_user_registry:add(1, self()),
    R1 = amoc_user_registry:count(),

    amoc_user_registry:remove(1),
    R2 = amoc_user_registry:count(),
    [
     ?_assertEqual(1, R1),
     ?_assertEqual(0, R2)
    ].

verify_last_id_test_(N) ->
    given_user_registry(),
    amoc_user_registry:add(6, self()),
    ?_assertEqual(6, amoc_user_registry:last_id()).

given_user_registry() ->
    catch amoc_user_registry:create_ets().

%given_clean_meck() ->
%    (catch meck:unload()).
