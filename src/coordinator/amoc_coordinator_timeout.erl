-module(amoc_coordinator_timeout).

-behaviour(gen_server).

-export([start_link/2, notify/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-record(timeouts, {
          name :: amoc_coordinator:name(),
          timeout :: timeout()
         }).
-type state() :: #timeouts{}.

-spec notify(pid(), amoc_coordinator:event()) -> ok.
notify(Pid, Event) ->
    gen_server:cast(Pid, Event).

-spec start_link(amoc_coordinator:name(), timeout()) ->
    {ok, pid()}.
start_link(Name, Timeout) ->
    gen_server:start_link(?MODULE, {Name, Timeout}, []).

-spec init({amoc_coordinator:name(), timeout()}) ->
    {ok, state()}.
init({Name, Timeout}) ->
    case Timeout of
        infinity ->
            State = #timeouts{name = Name, timeout = infinity},
            {ok, State};
        Int when is_integer(Int), Int > 0 ->
            State = #timeouts{name = Name, timeout = timer:seconds(Timeout)},
            {ok, State}
    end.

-spec handle_call(any(), term(), state()) ->
    {reply, not_implemented, state()}.
handle_call(_, _, State) ->
    {reply, not_implemented, State}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {stop, normal, state()}.
handle_cast({coordinate, _}, #timeouts{timeout = Timeout} = State) ->
    {noreply, State, Timeout};
handle_cast(reset_coordinator, State) ->
    {noreply, State, infinity};
handle_cast(coordinator_timeout, State) ->
    {noreply, State, infinity};
handle_cast(_, State) ->
    {stop, normal, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(timeout, #timeouts{name = Name} = State) ->
    amoc_coordinator:notify(Name, coordinator_timeout),
    {noreply, State, infinity};
handle_info(_, State) ->
    {noreply, State}.
