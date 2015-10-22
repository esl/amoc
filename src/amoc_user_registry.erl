-module(amoc_user_registry).

-define(TABLE, amoc_users).

-export([create_ets/0,
         add/2,
         remove/1,
         count/0,
         last_n_users/1,
         last_id/0
        ]).


-spec create_ets() -> atom().
create_ets() ->
    ets:new(?TABLE, [
                     named_table,
                     ordered_set,
                     public,
                     {write_concurrency, true},
                     {read_concurrency, true}]).

-spec add(amoc_scenario:user_id(), pid()) -> ok | {error, term()}.
add(UserId, Pid) ->
    ets:insert(?TABLE, {UserId, Pid}).

-spec remove(amoc_scenario:user_id()) -> true.
remove(UserId) ->
    true = ets:delete(amoc_users, UserId).

-spec count() -> non_neg_integer().
count() ->
    ets:info(?TABLE, size).

-spec last_n_users(non_neg_integer()) -> [{amoc_scenario:user_id(), pid()}].
last_n_users(N) ->
    [User || User <- last_users(N, ets:last(?TABLE), [])].

-spec last_id() -> amoc_scenario:user_id().
last_id() ->
    case ets:last(?TABLE) of
        '$end_of_table' -> 0;
        Other -> Other
    end.

last_users(0, _, Acc) ->
    Acc;
last_users(_, '$end_of_table', Acc) ->
    Acc;
last_users(Count, Current, Acc) ->
    Prev = ets:prev(?TABLE, Current),
    [User] = ets:lookup(?TABLE, Current),
    last_users(Count-1, Prev, [User | Acc]).
