%% @private
%% @see amoc_throttle
%% @copyright 2023 Erlang Solutions Ltd.
-module(amoc_throttle_pool).

-behaviour(supervisor).

-export([start_process_pool/4]).
-export([start_link/4, init/1]).

-spec start_process_pool(
        amoc_throttle:name(),
        amoc_throttle:interval(),
        amoc_throttle:rate(),
        pos_integer()
       ) -> ok | error.
start_process_pool(Name, Interval, Rate, NoOfProcesses) ->
    {ok, _} = supervisor:start_child(amoc_throttle_pooler, [Name, Interval, Rate, NoOfProcesses]),
    ok.

-spec start_link(
        amoc_throttle:name(),
        amoc_throttle:interval(),
        amoc_throttle:rate(),
        pos_integer()
       ) -> {ok, Pid :: pid()}.
start_link(Name, Interval, Rate, NoOfProcesses) when NoOfProcesses > 0 ->
    supervisor:start_link(?MODULE, {Name, Interval, Rate, NoOfProcesses}).

-spec init({amoc_throttle:name(), amoc_throttle:rate(), amoc_throttle:interval(), pos_integer()}) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({Name, Interval, Rate, NoOfProcesses}) ->
    RatesPerProcess = calculate_rate_per_process(Rate, NoOfProcesses),
    Tags = lists:seq(1, NoOfProcesses),
    Children = [
                #{id => {amoc_throttle_process, Name, N},
                  start => {amoc_throttle_process, start_link, [Name, Interval, RatePerProcess]},
                  type => worker,
                  shutdown => timer:seconds(5),
                  restart => transient,
                  modules => [amoc_throttle_process]
                 }
                || {RatePerProcess, N} <- lists:zip(RatesPerProcess, Tags)
               ],
    SupFlags = #{strategy => one_for_one, intensity => 0},
    {ok, {SupFlags, Children}}.

%% Helpers
calculate_rate_per_process(Rate, NoOfProcesses) ->
    calculate_rate_per_process([], Rate, NoOfProcesses).

calculate_rate_per_process(Acc, Rate, 1) ->
    [Rate | Acc];
calculate_rate_per_process(Acc, Rate, N) when is_integer(N), N > 1 ->
    ProcessRate = Rate div N,
    calculate_rate_per_process([ProcessRate | Acc], Rate - ProcessRate, N - 1).
