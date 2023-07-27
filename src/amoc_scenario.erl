%%==============================================================================
%% Copyright 2023 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_scenario).
%% API
-export([start_link/0,
         add_module/1,
         upload_module/3,
         does_scenario_exist/1,
         list_scenario_modules/0,
         list_uploaded_modules/0,
         list_configurable_modules/0]).

-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

%%-------------------------------------------------------------------------
%% behaviour definition
%%-------------------------------------------------------------------------
-export_type([user_id/0, state/0]).

-type user_id() :: pos_integer().
-type state() :: any().

-callback init() -> {ok, state()} | ok | {error, Reason :: term()}.
-callback start(user_id(), state()) -> any().
-callback start(user_id()) -> any().
-callback terminate(state()) -> any().

%% either start/1 or start/2 must be exported from the behaviour module
-optional_callbacks([start/1, start/2]).
-optional_callbacks([terminate/1]).

%%-------------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add_module(module()) -> ok | {error, term()}.
add_module(Module) ->
    gen_server:call(?MODULE, {add_module, Module}).

-spec upload_module(module(), binary(), file:filename()) -> ok | {error, term()}.
upload_module(Module, Binary, Filename) ->
    gen_server:call(?MODULE, {upload_module, Module, Binary, Filename}).

-spec does_scenario_exist(module()) -> boolean().
does_scenario_exist(Scenario) ->
    [{Scenario, scenario}] =:= ets:lookup(configurable_modules, Scenario).

-spec list_scenario_modules() -> [module()].
list_scenario_modules() ->
    [S || [S] <- ets:match(configurable_modules, {'$1', scenario})].

-spec list_uploaded_modules() -> [{module(), binary(), file:filename()}].
list_uploaded_modules() ->
    ets:tab2list(uploaded_modules).

-spec list_configurable_modules() -> [module()].
list_configurable_modules() ->
    [S || [S] <- ets:match(configurable_modules, {'$1', configurable})].

%%-------------------------------------------------------------------------
%% gen_server callbacks
%%-------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
    start_scenarios_ets(),
    ok = add_code_paths(),
    find_scenario_modules(),
    {ok, ok}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({add_module, Module}, _, State) ->
    Reply = add_module_internal(Module),
    {reply, Reply, State};
handle_call({upload_module, Module, Binary, Filename}, _, State) ->
    Reply = upload_module_internal(Module, Binary, Filename),
    {reply, Reply, State};
handle_call(_, _, State) ->
    {reply, {error, not_implemented}, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------------
%%  local functions
%%-------------------------------------------------------------------------
-spec start_scenarios_ets() -> term().
start_scenarios_ets() ->
    EtsOptions = [named_table, protected, {read_concurrency, true}],
    ets:new(configurable_modules, EtsOptions),
    ets:new(uploaded_modules, EtsOptions).

-spec add_code_paths() -> ok | {error, {bad_directories, [file:filename()]}}.
add_code_paths() ->
    AdditionalCodePaths = amoc_config_env:get(extra_code_paths, []),
    Res = [{code:add_pathz(Path), Path} || Path <- AdditionalCodePaths],
    case [Dir || {{error, bad_directory}, Dir} <- Res] of
        [] -> ok;
        BadDirectories -> {error, {bad_directories, BadDirectories}}
    end.

-spec find_scenario_modules() -> [module()].
find_scenario_modules() ->
    ErtsPath = code:lib_dir(),
    AllPaths = [Path || Path <- code:get_path(), not lists:prefix(ErtsPath, Path)],
    AllBeamFiles = [File || Path <- AllPaths, File <- filelib:wildcard("*.beam", Path)],
    AllModules = [list_to_atom(filename:rootname(BeamFile)) || BeamFile <- AllBeamFiles],
    ok = code:ensure_modules_loaded(AllModules),
    [maybe_store_module(Module) || {Module, _} <- code:all_loaded()].

-spec maybe_store_module(module()) -> any().
maybe_store_module(Module) ->
    case get_module_type(Module) of
        scenario ->
            ets:insert(configurable_modules, {Module, scenario});
        configurable ->
            ets:insert(configurable_modules, {Module, configurable});
        ordinary ->
            ok
    end.

-spec get_module_type(module()) -> scenario | configurable | ordinary.
get_module_type(Module) ->
    case erlang:function_exported(Module, module_info, 1) of
        false ->
            %% This can happen with the mocked (meck:new/2) and
            %% later unloaded (meck:unload/1) module. So this
            %% clause is required to pass the tests.
            ordinary;
        true ->
            ModuleAttributes = apply(Module, module_info, [attributes]),
            lists:foldl(fun({behaviour, [?MODULE]}, _) -> scenario;
                           ({behavior, [?MODULE]}, _) -> scenario;
                           ({required_variable, _}, ordinary) -> configurable;
                           (_, Ret) -> Ret
                        end, ordinary, ModuleAttributes)
    end.

-spec add_module_internal(module()) -> ok | {error, module_version_has_changed |
                                                    no_beam_file_for_module |
                                                    module_is_not_loaded}.
add_module_internal(Module) ->
    case {code:is_loaded(Module), code:get_object_code(Module),
          ets:lookup(uploaded_modules, Module)} of
        {false, _, _} ->
            {error, module_is_not_loaded};
        {_, error, [{Module, Binary, Filename}]} ->
            %% this is uploaded module, there's no beam file for it.
            ok;
        {_, error, _} ->
            %% might happen if directory with beam file is not added to the code path
            {error, no_beam_file_for_module};
        {_, {Module, Binary, Filename}, []} ->
            store_uploaded_module(Module, Binary, Filename);
        {_, {Module, Binary, Filename}, [{Module, Binary, Filename}]} ->
            %% this module is already added, no need to do anything.
            ok;
        {_, {Module, Binary1, Filename1}, [{Module, Binary2, Filename2}]}  ->
            %% Another version of module is added.
            %% Normally this should not happen, the most
            %% possible case is module recompilation.
            {error, module_version_has_changed}
    end.

store_uploaded_module(Module, Binary, Filename) ->
    true = ets:insert_new(uploaded_modules, {Module, Binary, Filename}),
    maybe_store_module(Module),
    ok.

upload_module_internal(Module, Binary, Filename) ->
    case code:is_loaded(Module) of
        false ->
            case code:load_binary(Module, Filename, Binary) of
                {error, _Error} -> {error, module_loading_error};
                {module, Module} -> store_uploaded_module(Module, Binary, Filename)
            end;
        {file, _} ->
            case {code:get_object_code(Module), ets:lookup(uploaded_modules, Module)} of
                {{Module, Binary, Filename}, []} ->
                    %% the same module is loaded, just add in uploaded modules table to
                    %% keep nodes consistent.
                    store_uploaded_module(Module, Binary, Filename);
                {error, [{Module, Binary, Filename}]} ->
                    %% this module is already uploaded, no need to do anything.
                    ok;
                {{Module, Binary, Filename}, [{Module, Binary, Filename}]} ->
                    %% this module is already uploaded, no need to do anything.
                    ok;
                _ ->
                    {error, another_version_is_loaded}
           end
    end.
