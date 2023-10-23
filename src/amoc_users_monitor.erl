-module(amoc_users_monitor).

-behaviour(gen_server).

-export([is_users_monitor_enabled/0]).
-export([start_link/0]).
-export([monitor_user/1, get_all_users_pids/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

is_users_monitor_enabled() ->
    application:get_env(amoc, amoc_users_monitor, disabled).

get_all_users_pids() ->
    case whereis(?MODULE) of
        undefined -> "Amoc users monitor is disabled.";
        _ -> gen_server:call(?MODULE, all_users_pids)
        end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

monitor_user(UserPid) ->
    gen_server:call(?MODULE, {monitor_user, UserPid}).


init(_Args) ->
    {ok, #{
        monitored_users => sets:new([{version, 2}]),
        children_which_died_normally => [],
        children_which_died_unnormaly => []
    }}.

handle_call(all_users_pids, _From, State) ->
    {reply, State, State};
handle_call({monitor_user, UserPid}, _From, State = #{monitored_users := MonitoredUsers}) ->
    MonitorRef = monitor(process, UserPid),
    NewState = State#{monitored_users => sets:add_element(UserPid, MonitoredUsers)},
    {reply, {ok, MonitorRef}, NewState}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, normal}, State = #{
        monitored_users := MonitoredUsers,
        children_which_died_normally := ProcessesDiendNormally}) ->
    NewState = State#{
        monitored_users => sets:del_element(Pid, MonitoredUsers),
        children_which_died_normally => [Pid | ProcessesDiendNormally]
    },
    logger:info("Process ~p died normally", [Pid]),
    {noreply, NewState};
handle_info({'DOWN', _Ref, process, Pid, Reason}, State = #{
    monitored_users := MonitoredUsers,
    children_which_died_normally := ProcessesDiendNormally}) ->
    NewState = State#{
        monitored_users => sets:del_element(Pid, MonitoredUsers),
        children_which_died_unnormaly => [{Pid, Reason} | ProcessesDiendNormally]
    },
    logger:warning("Process ~p died with reason ~p", [Pid, Reason]),
    {noreply, NewState}.

