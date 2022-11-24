%%==============================================================================
%% Copyright 2020 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_scenario).
%% API
-export([start_link/0,
         install_module/2,
         does_scenario_exist/1,
         list_scenario_modules/0,
         list_uploaded_modules/0]).

-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3,
         handle_cast/2, code_change/3,
         handle_info/2, terminate/2]).

-define(PRIV_DIR, code:priv_dir(amoc)).
-define(EBIN_DIR, filename:join(code:priv_dir(amoc), "scenarios_ebin")).

%%-------------------------------------------------------------------------
%% behaviour definition
%%-------------------------------------------------------------------------
-export_type([user_id/0, state/0]).

-type user_id() :: pos_integer().
-type state() :: any().
-type sourcecode() :: binary().

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

-spec install_module(module(), sourcecode()) ->
    ok | {error, [Errors :: string()], [Warnings :: string()]}.
install_module(Module, ModuleSource) ->
    case gen_server:call(?MODULE, {add_module, Module, ModuleSource}) of
        ok -> 
            [gen_server:call({?MODULE, N}, {add_module, Module, ModuleSource})
                || N <- amoc_cluster:connected_nodes()],
            ok;
        Error -> Error
    end.

-spec does_scenario_exist(module()) -> boolean().
does_scenario_exist(Scenario) ->
    AllScenarios = list_scenario_modules(),
    lists:member(Scenario, AllScenarios).

-spec list_scenario_modules() -> [module()].
list_scenario_modules() ->
    [S || [S] <- ets:match(amoc_scenarios, {'$1', scenario})].

-spec list_uploaded_modules() -> [{module(), sourcecode()}].
list_uploaded_modules() ->
    ets:tab2list(uploaded_modules).

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
handle_call({add_module, Module, ModuleSource}, _, State) ->
    Reply = add_module(Module, ModuleSource),
    {reply, Reply, State};
handle_call(_, _, State) ->
    {reply, {error, not_implemented}, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
    lager:warning("Unexpected message: ~p", [Info]),
    {noreply, State}.

-spec code_change(OldVsn :: any(), State :: state(), Extra :: any()) ->
        {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(Reason :: any(), State :: state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%%-------------------------------------------------------------------------
%%  local functions
%%-------------------------------------------------------------------------
-spec start_scenarios_ets() -> amoc_scenarios.
start_scenarios_ets() ->
    EtsOptions = [named_table, protected, {read_concurrency, true}],
    ets:new(amoc_scenarios, EtsOptions),
    ets:new(uploaded_modules, EtsOptions).

-spec add_code_paths() -> ok | {error, {bad_directories, [file:filename()]}}.
add_code_paths() ->
    true = code:add_pathz(?EBIN_DIR),
    AdditionalCodePaths = amoc_config_env:get(extra_code_paths, []),
    Res = [{code:add_pathz(Path), Path} || Path <- [?EBIN_DIR | AdditionalCodePaths]],
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
    [code:load_file(M)|| M <- AllModules, not is_loaded(M)],
    [maybe_store_module(Module) || Module <- erlang:loaded()].


is_loaded(Module) ->
    case code:is_loaded(Module) of
        false -> false;
        {file, _} -> true
    end.

-spec maybe_store_module(module()) -> any().
maybe_store_module(Module) ->
    case get_module_type(Module) of
        scenario ->
            ets:insert(amoc_scenarios, {Module, scenario});
        ordinary ->
            ok
    end.

-spec get_module_type(module()) -> scenario | ordinary.
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
                           (_, Ret) -> Ret
                        end, ordinary, ModuleAttributes)
    end.

-spec add_module(module(), sourcecode()) ->
    ok | {error, [Errors :: string()], [Warnings :: string()]}.
add_module(Module, ModuleSource) ->
    case erlang:module_loaded(Module) of
        true ->
            case ets:lookup(uploaded_modules, Module) of
                [{Module, ModuleSource}] -> ok;
                _ -> {error, ["module with such name already exists"], []}
            end;
        false ->
            ScenarioPath = scenario_path_name(Module),
            write_scenario_to_file(ModuleSource, ScenarioPath),
            case compile_and_load_scenario(ScenarioPath) of
                {ok, Module} ->
                    maybe_store_module(Module),
                    ets:insert(uploaded_modules, {Module, ModuleSource}),
                    ok;
                Error -> Error
            end
    end.

-spec scenario_path_name(module()) -> file:filename().
scenario_path_name(Module) -> %% w/o ".erl" extension
    filename:join([?PRIV_DIR, "scenarios", atom_to_list(Module)]).

-spec write_scenario_to_file(sourcecode(), file:filename()) -> ok.
write_scenario_to_file(ModuleSource, ScenarioPath) ->
    ok = file:write_file(ScenarioPath ++ ".erl", ModuleSource, [write]).

-spec compile_and_load_scenario(string()) -> {ok, module()} | {error, [string()], [string()]}.
compile_and_load_scenario(ScenarioPath) ->
    CompilationFlags = [{outdir, ?EBIN_DIR}, return_errors, report_errors, verbose],
    case compile:file(ScenarioPath, CompilationFlags) of
        {ok, Module} ->
            {module, Module} = code:load_file(Module),
            {ok, Module};
        {error, Errors, Warnings} ->
            file:delete(ScenarioPath ++ ".erl"),
            {error, Errors, Warnings}
    end.
