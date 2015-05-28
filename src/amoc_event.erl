-module(amoc_event).

-export([start_link/0,
         add_handler/2,
         notify/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

notify(Event) ->
    gen_event:notify(?MODULE, Event).
