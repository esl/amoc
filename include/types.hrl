-type error() :: embedded | badfile | native_code | nofile | on_load.
-type option() :: force | term().
-type maybe_user_id() :: user_id() | '$end_of_table'.


%% Amoc_annotations
-record(state_annotations, {}).
-type tag() :: binary().
-type state_annotations() :: #state_annotations{}.
-type event() :: {dist_do, scenario(), integer(), integer(), nodes(), any()} | {dist_add, integer()} | {dist_remove, integer(), list(option())} | any().

%% Amoc_controller
-type node_id() :: non_neg_integer() | undefined.
-type nodes() :: non_neg_integer() | undefined.
-type scenario_state() :: any().
-type user_ids() :: list(user_id()).
-type user_id() :: integer().
-type scenario() :: module() | undefined.
-record(state, {scenario :: module(),
                scenario_state :: scenario_state(),
                nodes :: nodes(),
                node_id :: node_id()}).
-type controller_state() :: #state{}.
-type users_info() :: [{count, integer()} |  {last, maybe_user_id()}, ...].
-type from() :: {pid(),any()}.
-type server_reply_sync(Reply) ::   {reply, Reply, controller_state()} |
{stop, any(), controller_state()} |
{stop, any(), Reply,  controller_state()}.
-type server_reply_as() ::      {noreply, controller_state()} |
{noreply, controller_state(), integer()} |
{noreply, controller_state(), hibernate} |
{stop, any(), controller_state()}.
