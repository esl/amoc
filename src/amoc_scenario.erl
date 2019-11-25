%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%==============================================================================
-module(amoc_scenario).
%% API
-export([start_link/0,
         install_scenario/2,
         does_scenario_exist/1,
         list_scenario_modules/0]).

-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-define(PRIV_DIR, code:priv_dir(amoc)).
-define(EBIN_DIR, filename:join(code:priv_dir(amoc), "scenarios_ebin")).

%%-------------------------------------------------------------------------
%% behaviour definition
%%-------------------------------------------------------------------------
-export_type([user_id/0, state/0]).

-type user_id() :: non_neg_integer().
-type state() :: any().

-callback init() -> {ok, state()} | ok | {error, Reason :: term()}.
-callback start(user_id(), state()) -> any().
-callback start(user_id()) -> any().

%% either start/1 or start/2 must be exported from the behaviour module
-optional_callbacks([start/1, start/2]).

%%-------------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec install_scenario(module(), binary()) ->
    ok | {error, [Errors :: string()], [Warnings :: string()]}.
install_scenario(Module, ModuleSource) ->
    gen_server:call(?MODULE, {add_scenario, Module, ModuleSource}).

-spec does_scenario_exist(module()) -> boolean().
does_scenario_exist(Scenario) ->
    AllScenarios = list_scenario_modules(),
    lists:member(Scenario, AllScenarios).

-spec list_scenario_modules() -> [module()].
list_scenario_modules() ->
    [element(1, T) || T <- ets:tab2list(amoc_scenarios)].

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
handle_call({add_scenario, Module, ModuleSource}, _, State) ->
    Reply = add_scenario(Module, ModuleSource),
    {reply, Reply, State};
handle_call(_, _, State) ->
    {reply, {error, not_implemented}, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------------
%%  local functions
%%-------------------------------------------------------------------------
-spec start_scenarios_ets() -> amoc_scenarios.
start_scenarios_ets() ->
    ets:new(amoc_scenarios, [named_table,
                             protected,
                             {read_concurrency, true}]).

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
    AllBeamFiles = [File || Path <- code:get_path(), File <- filelib:wildcard("*.beam", Path)],
    AllModules = [list_to_atom(filename:rootname(BeamFile)) || BeamFile <- AllBeamFiles],
    ok = code:ensure_modules_loaded(AllModules),
    AllScenarios = [Module || Module <- erlang:loaded(), is_scenario(Module)],
    [ets:insert(amoc_scenarios, {Scenario, scenario, preloaded}) || Scenario <- AllScenarios].

-spec is_scenario(module()) -> boolean().
is_scenario(Module) ->
    case erlang:function_exported(Module, module_info, 1) of
        false -> false;
        true ->
            ModuleAttributes = apply(Module, module_info, [attributes]),
            lists:any(fun({behaviour, [?MODULE]}) -> true;
                         ({behavior, [?MODULE]}) -> true;
                         (_) -> false
                      end, ModuleAttributes)
    end.

-spec add_scenario(module(), binary()) ->
    ok | {error, [Errors :: string()], [Warnings :: string()]}.
add_scenario(Module, ModuleSource) ->
    case erlang:module_loaded(Module) of
        true ->
            case ets:lookup(amoc_scenarios, Module) of
                [{Module, _, {uploaded, ModuleSource}}] -> ok;
                _ -> {error, ["module with such name already exists"], []}
            end;
        false ->
            ScenarioPath = scenario_path_name(Module),
            write_scenario_to_file(ModuleSource, ScenarioPath),
            case compile_and_load_scenario(ScenarioPath) of
                {ok, Module} ->
                    Type = case is_scenario(Module) of
                               true -> scenario;
                               false -> helper
                           end,
                    ets:insert(amoc_scenarios, {Module, Type, {uploaded, ModuleSource}}),
                    ok;
                Error -> Error
            end
    end.

-spec scenario_path_name(module()) -> file:filename().
scenario_path_name(Module) -> %% w/o ".erl" extension
    filename:join([?PRIV_DIR, "scenarios", atom_to_list(Module)]).

-spec write_scenario_to_file(binary(), file:filename()) -> ok.
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
