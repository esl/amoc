-module(amoc_always_null_ssl_session_cache).

-behaviour(ssl_session_cache_api).

-export([delete/2,
         foldl/3,
         init/1,
         lookup/2,
         select_session/2,
         terminate/1,
         update/3,
         size/1]).

%% not exported by ssl
%% only defined in "ssl_internal.hrl", we redefine it
-type db_handle() :: any().
-type key() :: tuple().
-type session() :: tuple().

-spec init(list()) -> invalid.
init(_) -> invalid.

-spec terminate(db_handle()) -> ok.
terminate(_) -> ok.

-spec lookup(db_handle(), key()) -> undefined.
lookup(_, _) -> undefined.

-spec update(db_handle(), key(), session()) -> ok.
update(_, _, _) -> ok.

-spec delete(db_handle(), key()) -> ok.
delete(_, _) -> ok.

-spec foldl(fun(), any(), db_handle()) -> any().
foldl(_, Acc, _) -> Acc.

-spec select_session(db_handle(), tuple() | inet:port_number()) -> [].
select_session(_, _) -> [].

-spec size(db_handle()) -> integer().
size(_) ->
    0.
