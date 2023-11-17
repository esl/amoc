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

-ifdef(TEST).

-export([upload_module/2,
         uploaded_module_to_map/1,
         map_to_uploaded_module/1]).

-define(RECORD2MAP(RecordName),
        (fun(#RecordName{} = Record) ->
             RecordFields = lists:zip(lists:seq(2, record_info(size, RecordName)),
                                      record_info(fields, RecordName)),
             BasicMap = #{'$RECORD_NAME' => RecordName},
             lists:foldl(fun({FieldPos, FieldName}, Map) ->
                             Map#{FieldName => erlang:element(FieldPos, Record)}
                         end,
                         BasicMap, RecordFields)
         end)).

-define(MAP2RECORD(RecordName),
        (fun(#{'$RECORD_NAME' := RecordName} = Map) ->
             BasicRecord = #RecordName{},
             RecordFields =
                 lists:zip3(lists:seq(2, tuple_size(BasicRecord)),
                            record_info(fields, RecordName),
                            tl(tuple_to_list(BasicRecord))),
             lists:foldl(fun({FieldPos, FieldName, DefaultValue}, Record) ->
                             Value = maps:get(FieldName, Map, DefaultValue),
                             setelement(FieldPos, Record, Value)
                         end,
                         BasicRecord, RecordFields)
         end)).

-endif.

-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

%% renaming the fields of the uploaded_module record will
%% break most test cases in amoc_code_server_SUITE because
%% the module info map used in the suite must have key names
%% matched to the field names of this record.
-record(uploaded_module, {module :: module(),
                          beam_filename :: file:filename(),
                          binary :: binary(),
                          md5 :: binary()}).

-type uploaded_module() :: #uploaded_module{}.

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

-spec distribute_modules(node()) -> [{module(), ok | {error, term()}}].
distribute_modules(Node) ->
    UploadedModules = ets:tab2list(uploaded_modules),
    [{Module, upload_module(Node, UM)} ||
        #uploaded_module{module = Module} = UM <- UploadedModules].

-spec upload_module(node(), uploaded_module()) -> ok | {error, term()}.
upload_module(Node, UploadedModule) ->
    %% format of the call request is critical for the tests, it must be
    %%    {upload_module, #uploaded_module{}}
    gen_server:call({?MODULE, Node}, {upload_module, UploadedModule}).

-ifdef(TEST).
uploaded_module_to_map(Record) ->
    ?RECORD2MAP(uploaded_module)(Record).

map_to_uploaded_module(Map) ->
    ?MAP2RECORD(uploaded_module)(Map).
-endif.

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
    find_all_configurable_and_scenario_modules(),
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
    ets:new(uploaded_modules, [{keypos, #uploaded_module.module} | EtsOptions]).

-spec add_code_paths() -> ok | {error, {bad_directories, [file:filename()]}}.
add_code_paths() ->
    AdditionalCodePaths = amoc_config_env:get(extra_code_paths, []),
    Res = [{code:add_pathz(Path), Path} || Path <- AdditionalCodePaths],
    case [Dir || {{error, bad_directory}, Dir} <- Res] of
        [] -> ok;
        BadDirectories -> {error, {bad_directories, BadDirectories}}
    end.

-spec find_all_configurable_and_scenario_modules() -> [module()].
find_all_configurable_and_scenario_modules() ->
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

-spec add_module_internal(module()) ->
    ok | {error, module_version_has_changed | no_beam_file_for_module |
                 module_is_not_loaded | code_path_collision}.
add_module_internal(Module) ->
    case maybe_add_module(Module) of
        ok -> ok;
        {error, no_beam_file_for_module} ->
            %% might happen if directory with beam file is not added to the code path
            case maybe_add_code_path(Module) of
                true -> maybe_add_module(Module);
                false -> {error, no_beam_file_for_module}
            end;
        Error -> Error
    end.

maybe_add_module(Module) ->
    case {code:is_loaded(Module), code:get_object_code(Module)} of
        {false, _} ->
            {error, module_is_not_loaded};
        {{file, _BeamFile}, error} ->
            {error, no_beam_file_for_module};
        {{file, BeamFile}, {Module, _Binary, Filename}} when BeamFile =/= Filename ->
            {error, code_path_collision};
        {{file, BeamFile}, {Module, Binary, BeamFile}} ->
            maybe_store_uploaded_module(Module, Binary, BeamFile)
    end.

maybe_store_uploaded_module(Module, Binary, Filename) ->
    MD5 = get_md5(Module),
    UploadedModule = #uploaded_module{module = Module, binary = Binary,
                                      beam_filename = Filename, md5 = MD5},
    case ets:insert_new(uploaded_modules, UploadedModule) of
        true ->
            maybe_store_configurable_module(Module),
            ok;
        false ->
            check_uploaded_module_version(UploadedModule)
    end.

-spec check_uploaded_module_version(uploaded_module()) ->
    ok | {error, module_version_has_changed | module_is_not_uploaded}.
check_uploaded_module_version(#uploaded_module{module = Module, md5 = MD5}) ->
    case {ets:lookup(uploaded_modules, Module), get_md5(Module)} of
        {[#uploaded_module{md5 = MD5}], MD5} ->
            %% md5 is the same, we have the same version of module loaded & stored in ETS
            ok;
        {[], MD5} ->
            %% this can happen for upload_module_internal/1 calls
            %% and should never happen for add_module_internal/1
            {error, module_is_not_uploaded};
        _ ->
            {error, module_version_has_changed}
    end.

-spec get_md5(module()) -> binary().
get_md5(Module) ->
    Module:module_info(md5).

maybe_add_code_path(Module) ->
    try
        {file, BeamFile} = code:is_loaded(Module),
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

upload_module_internal(#uploaded_module{module = Module, binary = Binary,
                                        beam_filename = Filename} = UM) ->
    case code:is_loaded(Module) of
        false ->
            case code:load_binary(Module, Filename, Binary) of
                {error, _Error} -> {error, module_loading_error};
                {module, Module} -> maybe_store_uploaded_module(Module, Binary, Filename)
            end;
        {file, _} ->
            case check_uploaded_module_version(UM) of
                ok -> ok;
                {error, module_is_not_uploaded} ->
                    %% the same version of module is loaded, but not yet stored in ETS
                    %% so let's try to store it for consistency. if module adding fails
                    %% (e.g. if there's no beam file), it's not a big problem and can
                    %% be ignored.
                    add_module_internal(Module),
                    ok;
                Error -> Error
            end
    end.
