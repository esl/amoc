-module(amoc_always_null_ssl_session_cache).

-behaviour(ssl_session_cache_api).

-export([delete/2,
         foldl/3,
         init/1,
         lookup/2,
         select_session/2,
         terminate/1,
         update/3]).

init(_) -> invalid.

terminate(_) -> ok.

lookup(_,_) -> undefined.

update(_,_,_) -> ok.

delete(_,_) -> ok.

foldl(_,Acc,_) -> Acc.

select_session(_,_) -> [].
