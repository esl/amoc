%%==============================================================================
%% Copyright 2023 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_code_server).
%% API
-export([start_link/0,
         add_module/1,
         distribute_modules/1,
         does_scenario_exist/1,
         list_scenario_modules/0,
         list_configurable_modules/0]).

-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(uploaded_module, {name :: module(),
                          filename :: file:filename(),
                          binary :: binary()}).

-type state() :: map().

%%-------------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add_module(module()) -> ok | {error, term()}.
add_module(Module) ->
    gen_server:call(?MODULE, {add_module, Module}).

-spec distribute_modules(node()) ->[{module(), ok | {error, term()}}].
distribute_modules(Node) ->
    UploadedModules = ets:tab2list(uploaded_modules),
    [{Module, gen_server:call({?MODULE, Node}, {upload_module, UM})}
        || #uploaded_module{name = Module} = UM <- UploadedModules].

-spec does_scenario_exist(module()) -> boolean().
does_scenario_exist(Scenario) ->
    [{Scenario, scenario}] =:= ets:lookup(configurable_modules, Scenario).

-spec list_scenario_modules() -> [module()].
list_scenario_modules() ->
    [S || [S] <- ets:match(configurable_modules, {'$1', scenario})].

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
    {ok, #{}}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({add_module, Module}, _, State) ->
    Reply = add_module_internal(Module),
    {reply, Reply, State};
handle_call({upload_module, #uploaded_module{} = UM}, _, State) ->
    Reply = upload_module_internal(UM),
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
    ets:new(uploaded_modules, [{keypos, #uploaded_module.name} | EtsOptions]).

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
    [maybe_store_configurable_module(Module) || {Module, _} <- code:all_loaded()].

-spec maybe_store_configurable_module(module()) -> any().
maybe_store_configurable_module(Module) ->
    case get_module_type(Module) of
        scenario ->
            ets:insert_new(configurable_modules, {Module, scenario});
        configurable ->
            ets:insert_new(configurable_modules, {Module, configurable});
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
            lists:foldl(fun({behaviour, [amoc_scenario]}, _) -> scenario;
                           ({behavior, [amoc_scenario]}, _) -> scenario;
                           ({required_variable, _}, ordinary) -> configurable;
                           (_, Ret) -> Ret
                        end, ordinary, ModuleAttributes)
    end.

-spec add_module_internal(module()) -> ok | {error, module_version_has_changed |
                                                    no_beam_file_for_module |
                                                    module_is_not_loaded}.
add_module_internal(Module) ->
    case {code:is_loaded(Module), code:get_object_code(Module)} of
        {false, _} ->
            {error, module_is_not_loaded};
        {{file, BeamFile}, error} ->
            %% might happen if directory with beam file is not added to the code path
            case maybe_add_code_path(BeamFile) of
                true -> add_module_internal(Module);
                false -> {error, no_beam_file_for_module}
            end;
        {_, {Module, Binary, Filename}} ->
            maybe_store_uploaded_module(Module, Binary, Filename)
    end.

maybe_store_uploaded_module(Module, Binary, Filename) ->
    UploadedModule = #uploaded_module{name = Module, binary = Binary,
                                      filename = Filename},
    case ets:insert_new(uploaded_modules, UploadedModule) of
        true ->
            maybe_store_configurable_module(Module),
            ok;
        false ->
            check_uploaded_module_version(UploadedModule)
    end.

-spec check_uploaded_module_version(#uploaded_module{}) ->
    ok | {error, module_version_has_changed}.
check_uploaded_module_version(#uploaded_module{name = Module, binary = Binary}) ->
    case {ets:lookup(uploaded_modules, Module), code:get_object_code(Module)} of
        {[#uploaded_module{binary = Binary}], _} ->
            %% Binary is the same, we have the same version of module already stored in ETS
            ok;
        {[], {Module, Binary, _}} ->
            %% the same version of module is loaded, but not yet stored in ETS
            %% so let's store it for consistency.
            add_module_internal(Module),
            ok;
        _ ->
            {error, module_version_has_changed}
    end.

maybe_add_code_path(BeamFile) ->
    try
        true = is_list(BeamFile),
        true = filelib:is_regular(BeamFile),
        BeamDir = filename:dirname(filename:absname(BeamFile)),
        true = filelib:is_dir(BeamDir),
        CodePath = code:get_path(),
        code:add_pathz(BeamDir),
        NewCodePath = code:get_path(),
        NewCodePath =/= CodePath
    catch
        _C:_E -> false
    end.

upload_module_internal(#uploaded_module{name = Module, binary = Binary,
                                        filename = Filename} = UM) ->
    case code:is_loaded(Module) of
        false ->
            case code:load_binary(Module, Filename, Binary) of
                {error, _Error} -> {error, module_loading_error};
                {module, Module} -> maybe_store_uploaded_module(Module, Binary, Filename)
            end;
        {file, _} ->
            check_uploaded_module_version(UM)
    end.
