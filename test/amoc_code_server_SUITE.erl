-module(amoc_code_server_SUITE).

-include_lib("eunit/include/eunit.hrl").
-export([all/0, groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

-export([verify_module_info_compatibility/1,
         module_md5_and_binary_test/1,
         add_regular_module_test/1,
         add_configurable_modules_test/1,
         modules_distribution_test/1,
         basic_modules_uploading_test/1,
         uploading_module_with_different_md5_test/1,
         reuploading_modules_after_amoc_code_server_restart_test/1,
         uploading_release_modules_test/1]).

-import(amoc_code_server, [map_to_uploaded_module/1,
                           uploaded_module_to_map/1]).

-define(assertEqualLists(L1, L2),
        ?assertEqual(lists:sort(L1), lists:sort(L2))).

-define(DUMMY_CODE_PATH, "/invalid/code/path").

all() ->
    [{group, all_tests}].

groups() ->
    [{all_tests, [sequence], all_tests()}].

all_tests() ->
    %% if the first test fails, then it makes no
    %% sense to run any of the following tests
    [verify_module_info_compatibility,
     % failing_testcase,
     module_md5_and_binary_test,
     add_regular_module_test,
     add_configurable_modules_test,
     modules_distribution_test,
     basic_modules_uploading_test,
     uploading_module_with_different_md5_test,
     reuploading_modules_after_amoc_code_server_restart_test,
     uploading_release_modules_test].

init_per_suite(Config) ->
    % ct:pal("~nConfig = ~p~n", [Config]),

    OriginalCodePath = code:get_path(),
    WorkingDir = recreate_working_dir(Config),
    NewConfig = [{original_code_path, OriginalCodePath},
                 {working_dir, WorkingDir} | Config],
    %% ModuleA and ModuleB are essentially the same, however there are few differences.
    %% while module_info(md5) call for both modules returns one and the same checksum,
    %% code:get_object_code/1 returns different binaries. These modules can be used for
    %% amoc_code_server testing. ModuleC has different MD5 and different binary.
    %% see module_md5_and_binary_test test case for more details.
    ModuleInfoA = generate_module(NewConfig, #{}), %% "tag" => [$\n, $\n, $\n]
    ModuleInfoB = generate_module(NewConfig, #{"tag" => ""}),
    ModuleInfoC = generate_module(NewConfig, #{"tag" => "-compile(export_all)."}),

    ScenarioModuleInfo = generate_module(NewConfig,
                                         #{"tag" => "-behavior(amoc_scenario).",
                                           "module_name" => "some_scenario"}),
    HelperModuleInfo = generate_module(NewConfig,
                                       #{"tag" => "-required_variable(some_var).",
                                         "module_name" => "some_helper"}),

    [{module_a, ModuleInfoA}, {module_b, ModuleInfoB}, {module_c, ModuleInfoC},
     {scenario_module, ScenarioModuleInfo}, {helper_module, HelperModuleInfo}
     | NewConfig].

end_per_suite(Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    unload_modules_and_restore_code_path(Config),
    Config.

end_per_testcase(_TestCase, Config) ->
    unload_modules_and_restore_code_path(Config),
    Config.

%%---------------------------------------------------------
%% test cases
%%---------------------------------------------------------

verify_module_info_compatibility(Config) ->
    %% this is not a test case, but rather a verification of
    %% compatibility between internal uploaded_module record
    %% defined in amoc_code_server and module info map used
    %% in this suite. if this test fails, then mostly like all
    %% other test cases would fail as well.

    DummyUploadedModuleRec =
         map_to_uploaded_module(#{'$RECORD_NAME' => uploaded_module}),
    DummyUploadedModuleMap = uploaded_module_to_map(DummyUploadedModuleRec),
    %% while a record tuple has an extra additional element with a record name,
    %% its map counterpart contains an additional '$RECORD_NAME' key, so the sizes
    %% of map and tuple must be the same.
    ?assertEqual(map_size(DummyUploadedModuleMap), tuple_size(DummyUploadedModuleRec)),
    UploadedModuleKeys = maps:keys(DummyUploadedModuleMap),
    ModuleInfoA = proplists:get_value(module_a, Config),
    [?assert(maps:is_key(Key, ModuleInfoA)) || Key <- UploadedModuleKeys],
    UploadedModuleRec = map_to_uploaded_module(ModuleInfoA),
    UploadedModuleMap = uploaded_module_to_map(UploadedModuleRec),
    ?assertEqual(UploadedModuleMap, maps:with(UploadedModuleKeys, UploadedModuleMap)),
    ?assertEqual(UploadedModuleRec, map_to_uploaded_module(UploadedModuleMap)).

module_md5_and_binary_test(Config) ->
    %% this is not a test case, but rather a rational why MD5
    %% should be used instead of binary for modules comparison.

    [ModuleInfoA, ModuleInfoB, ModuleInfoC] =
        [proplists:get_value(Key, Config) || Key <- [module_a, module_b, module_c]],

    %% note that compare_module_info/3 not only verifies that the
    %% values for keys supplied in the EqualKeys list are the same
    %% in both ModuleInfo maps, but in addition to that it also
    %% verifies that all other keys have different values associated
    %% with them in ModuleInfo maps.
    EqualKeys = [module, working_dir, erl_file],

    %% reason #1, empty lines at the beginning or in the middle
    %% of the document affect binary module representation, however
    %% module's MD5 remains the same. this situation is possible
    %% if we manually copy-past and upload code on 2 nodes of the
    %% cluster and accidentally copy code with an extra empty line
    %% while uploading on one node and without that extra line when
    %% uploading on another node.
    compare_module_info(ModuleInfoA, ModuleInfoC, EqualKeys),
    compare_module_info(ModuleInfoA, ModuleInfoB, [md5 | EqualKeys]),
    SouceCodeA = maps:get(source_code, ModuleInfoA),
    SouceCodeB = maps:get(source_code, ModuleInfoB),
    ct:pal("Module w/o extra empty lines:~n~n----~n~s~n----~n",
           [re:replace(SouceCodeA, "\n(\s*\n)+", "\n", [global])]),
    ?assertEqual(re:replace(SouceCodeA, "\n(\s*\n)+", "\n", [global]),
                 re:replace(SouceCodeB, "\n(\s*\n)+", "\n", [global])),

    %% reason #2, when compiling one and the same module but
    %% with different path to source code, it changes the binary
    %% representation of the module, while MD5 remains unchanged.
    WorkingDir = maps:get(working_dir, ModuleInfoA),
    NewWorkingDir = filename:join(WorkingDir, "tmp"),
    file:make_dir(NewWorkingDir),
    ?assertEqual(true, filelib:is_dir(NewWorkingDir)),
    ModuleInfoA2 = compile_and_load_module(ModuleInfoA#{working_dir := NewWorkingDir}),
    compare_module_info(ModuleInfoA, ModuleInfoA2, [module, md5, source_code]),

    %% reason #3, if module is loaded using code:load_binary/3
    %% interface, then there might be no beam file. so there's
    %% no way to fetch an actual binary for the module. more
    %% over, code:get_object_code/3 doesn't use the beam file
    %% of the currently loaded module, but searches for the
    %% first suitable beam file in the code path. if it's not
    %% the same beam file as the loaded one, then the fetched
    %% binary is incorrect.
    #{beam_filename := BeamFileA, out_dir := OutDirA,
      binary := BinaryA, module := Module} = ModuleInfoA,
    #{binary := BinaryC, out_dir := OutDirC,
      beam_filename := BeamFileC} = ModuleInfoC,
    compare_module_info(ModuleInfoA, ModuleInfoC, EqualKeys),
    load_module(ModuleInfoA),
    ?assertEqual(error, code:get_object_code(Module)),
    assert_module_loaded(ModuleInfoA),
    ?assertEqual(true, code:add_patha(OutDirC)),
    ?assertEqual({Module, BinaryC, BeamFileC}, code:get_object_code(Module)),
    assert_module_loaded(ModuleInfoA),
    ?assertEqual(true, code:add_patha(OutDirA)),
    ?assertEqual({Module, BinaryA, BeamFileA}, code:get_object_code(Module)),
    assert_module_loaded(ModuleInfoA),
    ok.

add_regular_module_test(Config) ->
    [ModuleInfoA, ModuleInfoB, ModuleInfoC, OriginalCodePath] =
        [proplists:get_value(Key, Config) ||
            Key <- [module_a, module_b, module_c, original_code_path]],
    #{module := Module, out_dir := OutDirA} = ModuleInfoA,
    {ok, Pid} = amoc_code_server:start_link(),

    %% note that test modules are loaded only after the start of the amoc_code_server,
    %% so they are not added into configurable_modules ETS during the initial modules
    %% analysis
    ConfigurableModules = amoc_code_server:list_configurable_modules(),
    ScenarioModules = amoc_code_server:list_scenario_modules(),

    %% initially uploaded modules ETS is empty
    assert_uploaded_modules_ets([]),

    %% if module is not loaded, amoc_code_server:add_module/1 returns error and
    %% nothing is added to the uploaded modules ETS
    ?assertEqual({error, module_is_not_loaded}, amoc_code_server:add_module(Module)),
    assert_uploaded_modules_ets([]),

    %% if beam file for the module is missing, amoc_code_server:add_module/1 returns
    %% error and nothing is added to the uploaded modules ETS
    load_module(invalidate_filename(ModuleInfoA)),
    assert_invalid_filename(Module),
    ?assertEqual({error, no_beam_file_for_module}, amoc_code_server:add_module(Module)),
    assert_uploaded_modules_ets([]),

    %% if beam file is located properly, amoc_code_server:add_module/1 adds module
    %% to the uploaded modules ETS
    load_module(ModuleInfoA),
    ?assertEqual(ok, amoc_code_server:add_module(Module)),
    %% if required, amoc_code_server:add_module/1 call adds
    %% missing code path automatically
    ?assertEqualLists([OutDirA | OriginalCodePath], code:get_path()),
    assert_uploaded_modules_ets([ModuleInfoA]),

    %% after loading ModuleInfoB we run into collision, because code path
    %% for ModuleInfoA is already added, but it's not added for ModuleInfoB.
    %% so code:get_object_code/1 returns a binary for ModuleInfoA. we must
    %% ensure that amoc_code_server detects it and returns an error.
    %% uploaded modules ETS must remain unchanged.
    load_module(ModuleInfoB),
    ?assertEqual({error, code_path_collision}, amoc_code_server:add_module(Module)),
    assert_uploaded_modules_ets([ModuleInfoA]),

    %% if there is no code path collision, MD5 of the loaded module is compared
    %% with the one already stored in ETS. if it's the same, amoc_code_server
    %% returns ok. uploaded modules ETS must remain unchanged.
    restore_code_path(OriginalCodePath),
    ?assertEqual(ok, amoc_code_server:add_module(Module)),
    assert_uploaded_modules_ets([ModuleInfoA]),

    %% if MD5 of the loaded module different than the one already stored in ETS,
    %% amoc_code_server must return an error. uploaded modules ETS must remain
    %% unchanged.
    load_module(ModuleInfoC),
    restore_code_path(OriginalCodePath),
    ?assertEqual({error, module_version_has_changed},
                 amoc_code_server:add_module(Module)),
    assert_uploaded_modules_ets([ModuleInfoA]),

    %% check that added before module is not erronously identified as a scenario
    %% or a configurable module
    ?assertEqualLists(ConfigurableModules,
                      amoc_code_server:list_configurable_modules()),
    ?assertEqualLists(ScenarioModules,
                      amoc_code_server:list_scenario_modules()),

    gen_server:stop(Pid),
    ok.

add_configurable_modules_test(Config) ->
    [ScenarioModuleInfo, HelperModuleInfo] =
        [proplists:get_value(Key, Config) || Key <- [scenario_module, helper_module]],
    {ok, Pid1} = amoc_code_server:start_link(),
    %% note that modules are loaded only after the start of the amoc_code_server,
    %% so they are not added into configurable_modules ETS table during the initial
    %% modules' analysis on amoc_code_server startup.
    ConfigurableModules1 = amoc_code_server:list_configurable_modules(),
    ScenarioModules1 = amoc_code_server:list_scenario_modules(),

    %% initially uploaded modules ETS is empty
    assert_uploaded_modules_ets([]),

    %% load scenario & configurable helper modules
    load_module(ScenarioModuleInfo),
    load_module(HelperModuleInfo),

    %% when module is added, it must be analyzed and if it's a scenario
    %% or configurable module, amoc_code_server must be added into the
    %% configurable_modules ETS table
    ScenarioModule = maps:get(module, ScenarioModuleInfo),
    ?assertEqualLists(ScenarioModules1, amoc_code_server:list_scenario_modules()),
    ?assertNot(amoc_code_server:does_scenario_exist(ScenarioModule)),
    ?assertEqual(ok, amoc_code_server:add_module(ScenarioModule)),
    assert_uploaded_modules_ets([ScenarioModuleInfo]),
    ?assert(amoc_code_server:does_scenario_exist(ScenarioModule)),
    ScenarioModules2 = amoc_code_server:list_scenario_modules(),
    ?assertEqualLists([ScenarioModule | ScenarioModules1], ScenarioModules2),
    ?assertEqualLists(ConfigurableModules1, amoc_code_server:list_configurable_modules()),

    HelperModule = maps:get(module, HelperModuleInfo),
    ?assertEqual(ok, amoc_code_server:add_module(HelperModule)),
    assert_uploaded_modules_ets([ScenarioModuleInfo, HelperModuleInfo]),
    ?assertEqualLists(ScenarioModules2, amoc_code_server:list_scenario_modules()),
    ConfigurableModules2 = amoc_code_server:list_configurable_modules(),
    ?assertEqualLists([HelperModule | ConfigurableModules1], ConfigurableModules2),

    %% if amoc_code_server gets restarted, then all of the previously added modules
    %% are analyzed properly during the startup phase, however ETS with uploaded
    %% module cannot be repopulated properly.
    ?assertNotEqual(undefined, ets:whereis(configurable_modules)),
    ?assertNotEqual(undefined, ets:whereis(uploaded_modules)),

    gen_server:stop(Pid1),

    ?assertEqual(undefined, ets:whereis(configurable_modules)),
    ?assertEqual(undefined, ets:whereis(uploaded_modules)),

    {ok, Pid2} = amoc_code_server:start_link(),

    ?assertNotEqual(undefined, ets:whereis(configurable_modules)),
    ?assertNotEqual(undefined, ets:whereis(uploaded_modules)),
    ?assertEqualLists(ScenarioModules2, amoc_code_server:list_scenario_modules()),
    ?assertEqualLists(ConfigurableModules2, amoc_code_server:list_configurable_modules()),
    assert_uploaded_modules_ets([]),

    gen_server:stop(Pid2),
    ok.

modules_distribution_test(Config) ->
    [OriginalCodePath | TestModules ] =
        [proplists:get_value(Key, Config) ||
            Key <- [original_code_path, module_a, scenario_module, helper_module]],

    {ok, Pid1} = amoc_code_server:start_link(),

    [begin
        load_module(ModuleInfo),
        ?assertEqual(ok, amoc_code_server:add_module(M))
     end || #{module := M} = ModuleInfo <- TestModules],

    start_system_events_logging(Pid1),
    DistributionRet = amoc_code_server:distribute_modules(node()),
    LoggedEvents1 = stop_system_events_logging(Pid1),

    ExpectedDistributionRet = [{M, ok} || #{module := M} <- TestModules],
    ?assertEqualLists(DistributionRet, ExpectedDistributionRet),

    %% note that extract_gen_server_call_request function crashes if supplied
    %% message is not a gen_server call, this works as an extra verification
    %% that only gen_server calls were made during distribution.
    DistributionGenServerCalls1 =
        [extract_gen_server_call_request(Msg) || {in, Msg} <- LoggedEvents1],
    ct:pal("distribution gen_server calls = ~p", [DistributionGenServerCalls1]),
    %% and the number of gen_server calls corresponds to the number of
    %% the added modules.
    ?assertEqual(length(TestModules), length(DistributionGenServerCalls1)),
    verify_upload_module_requests(TestModules, DistributionGenServerCalls1),

    %% now let's ensure that manual modules uploading does exactly the same.
    %% modules' distribution is idempotent action, so it must be fine to call
    %% it multiple times.
    start_system_events_logging(Pid1),
    ?assertEqualLists(ExpectedDistributionRet, upload_modules(TestModules)),
    LoggedEvents2 = stop_system_events_logging(Pid1),

    DistributionGenServerCalls2 =
        [extract_gen_server_call_request(Msg) || {in, Msg} <- LoggedEvents2],
    ?assertEqualLists(DistributionGenServerCalls1, DistributionGenServerCalls2),

    assert_uploaded_modules_ets(TestModules),
    gen_server:stop(Pid1),

    %% restore the initial state of the system and check that modules uploading
    %% is working correctly.
    restore_code_path(OriginalCodePath),
    [unload_module(M) || M <- TestModules],

    {ok, Pid2} = amoc_code_server:start_link(),

    start_system_events_logging(Pid2),
    ?assertEqualLists(ExpectedDistributionRet, upload_modules(TestModules)),
    LoggedEvents3 = stop_system_events_logging(Pid2),

    DistributionGenServerCalls3 =
        [extract_gen_server_call_request(Msg) || {in, Msg} <- LoggedEvents3],
    ?assertEqualLists(DistributionGenServerCalls1, DistributionGenServerCalls3),
    assert_uploaded_modules_ets(TestModules),
    [assert_module_loaded(MI) || MI <- TestModules],

    gen_server:stop(Pid2),
    ok.

basic_modules_uploading_test(Config) ->
    [ScenarioModuleInfo, HelperModuleInfo, _] = OriginalTestModules =
        [proplists:get_value(Key, Config) ||
            Key <- [scenario_module, helper_module, module_a]],

    %% normally all the uploaded modules have invalid beam path
    %% associated with them.
    TestModules = [invalidate_filename(M) || M <- OriginalTestModules],

    ScenarioModule = maps:get(module, ScenarioModuleInfo),
    HelperModule = maps:get(module, HelperModuleInfo),

    {ok, Pid} = amoc_code_server:start_link(),

    [?assertNot(code:is_loaded(M)) || #{module := M} <- TestModules],
    assert_uploaded_modules_ets([]),
    ?assertNot(amoc_code_server:does_scenario_exist(ScenarioModule)),
    ConfigurableModules = amoc_code_server:list_configurable_modules(),
    ScenarioModules = amoc_code_server:list_scenario_modules(),

    ExpectedUploadingRet = [{M, ok} || #{module := M} <- TestModules],

    ?assertEqualLists(ExpectedUploadingRet, upload_modules(TestModules)),
    assert_uploaded_modules_ets(TestModules),
    [assert_invalid_filename(MI) || MI <- TestModules],
    [assert_module_loaded(MI) || MI <- TestModules],

    %% scenario and helper modules are added properly to the 'configurable_modules' ETS
    ?assert(amoc_code_server:does_scenario_exist(ScenarioModule)),
    ?assertEqualLists([HelperModule | ConfigurableModules],
                      amoc_code_server:list_configurable_modules()),
    ?assertEqualLists([ScenarioModule | ScenarioModules],
                      amoc_code_server:list_scenario_modules()),

    %% uploading modules with the same MD5 just returns 'ok'
    %% without actual modules reloading
    ?assertEqualLists(ExpectedUploadingRet, upload_modules(OriginalTestModules)),
    assert_uploaded_modules_ets(TestModules),
    [?assertError(_, assert_module_loaded(MI)) || MI <- OriginalTestModules],
    [assert_module_loaded(MI) || MI <- TestModules],

    %% scenario and helper modules are added properly to the 'configurable_modules' ETS
    ?assert(amoc_code_server:does_scenario_exist(ScenarioModule)),
    ?assertEqualLists([HelperModule | ConfigurableModules],
                      amoc_code_server:list_configurable_modules()),
    ?assertEqualLists([ScenarioModule | ScenarioModules],
                      amoc_code_server:list_scenario_modules()),

    gen_server:stop(Pid).

reuploading_modules_after_amoc_code_server_restart_test(Config) ->
    [ScenarioModuleInfo, HelperModuleInfo, _] = OriginalTestModules =
        [proplists:get_value(Key, Config) ||
            Key <- [scenario_module, helper_module, module_a]],

    %% normally all the uploaded modules have invalid beam path
    %% associated with them. (that is the case when *.erl file is
    %% compiled on another node and module is uploaded as a binary)
    TestModules = [invalidate_filename(M) || M <- OriginalTestModules],

    ScenarioModule = maps:get(module, ScenarioModuleInfo),
    HelperModule = maps:get(module, HelperModuleInfo),

    %% note that modules are loaded with invalid beam path before we start
    %% amoc_code_sever. this precisely simulates amoc_code_sever restart
    %% after initial modules uploading.
    [load_module(MI) || MI <- TestModules],

    {ok, Pid} = amoc_code_server:start_link(),

    %% after restarting amoc_code_server 'uploaded_modules' ETS is empty.
    %% however, both ScenarioModule and HelperModule must be added into
    %% 'configurable_modules' ETS during the initial modules' analysis
    assert_uploaded_modules_ets([]),
    ConfigurableModules = amoc_code_server:list_configurable_modules(),
    ?assert(lists:member(HelperModule, ConfigurableModules)),
    ScenarioModules = amoc_code_server:list_scenario_modules(),
    ?assert(lists:member(ScenarioModule, ScenarioModules)),
    ?assert(amoc_code_server:does_scenario_exist(ScenarioModule)),

    %% reuploading must succeed, since MD5 of the loaded and reuploaded modules
    %% are the same. however, none of the reuploaded modules can be added into
    %% 'uploaded_modules' ETS. it happens because beam file names associated with
    %% such modules are typically invalid and we cannot get modules' binary using
    %% code:get_object_code/3 interface.
    ExpectedUploadingRet = [{M, ok} || #{module := M} <- TestModules],
    ?assertEqualLists(ExpectedUploadingRet, upload_modules(TestModules)),
    assert_uploaded_modules_ets([]),

    %% also, 'configurable_modules' ETS must remain unchanged
    ?assert(amoc_code_server:does_scenario_exist(ScenarioModule)),
    ?assertEqualLists(ScenarioModules, amoc_code_server:list_scenario_modules()),
    ?assertEqualLists(ConfigurableModules, amoc_code_server:list_configurable_modules()),

    gen_server:stop(Pid),
    ok.

uploading_release_modules_test(Config) ->
    [ScenarioModuleInfo, HelperModuleInfo | _] = OriginalTestModules =
        [proplists:get_value(Key, Config) ||
            Key <- [scenario_module, helper_module, module_a]],

    %% module_b has the same MD5 as module_a, but different binary representation.
    %% also, for the sake of testing, let's invalidate beam file name for uploaded
    %% scenario and helper modules.
    TestModules = [proplists:get_value(module_b, Config) |
                      [invalidate_filename(M) ||
                         M <- [ScenarioModuleInfo, HelperModuleInfo]]],

    ScenarioModule = maps:get(module, ScenarioModuleInfo),
    HelperModule = maps:get(module, HelperModuleInfo),

    %% loading original test modules with correct beam file names before
    %% amoc_code_server initialization. therefore, these modules are
    %% no different from the regular modules included in the release.
    [load_module(MI) || MI <- OriginalTestModules],

    {ok, Pid} = amoc_code_server:start_link(),

    %% after starting amoc_code_server 'uploaded_modules' ETS is empty.
    %% however, both ScenarioModule and HelperModule must be added into
    %% 'configurable_modules' ETS during the initial modules' analysis
    assert_uploaded_modules_ets([]),
    ConfigurableModules = amoc_code_server:list_configurable_modules(),
    ?assert(lists:member(HelperModule, ConfigurableModules)),
    ScenarioModules = amoc_code_server:list_scenario_modules(),
    ?assert(lists:member(ScenarioModule, ScenarioModules)),
    ?assert(amoc_code_server:does_scenario_exist(ScenarioModule)),

    %% uploading of the modules must succeed, since MD5 of the loaded and
    %% uploaded modules are the same. however, no modules reloading should
    %% take place and original versions of the modules must be added into
    %% 'uploaded_modules' ETS.
    ExpectedUploadingRet = [{M, ok} || #{module := M} <- TestModules],
    ?assertEqualLists(ExpectedUploadingRet, upload_modules(TestModules)),
    ?assertError(_, assert_uploaded_modules_ets(TestModules)),
    assert_uploaded_modules_ets(OriginalTestModules),
    [?assertError(_, assert_module_loaded(MI)) || MI <- TestModules],
    [assert_module_loaded(MI) || MI <- OriginalTestModules],

    %% also, 'configurable_modules' ETS must remain unchanged
    ?assert(amoc_code_server:does_scenario_exist(ScenarioModule)),
    ?assertEqualLists(ScenarioModules, amoc_code_server:list_scenario_modules()),
    ?assertEqualLists(ConfigurableModules, amoc_code_server:list_configurable_modules()),

    gen_server:stop(Pid),
    ok.

uploading_module_with_different_md5_test(Config) ->
    [#{module := ModuleName} = ModuleInfoA, ModuleInfoB, ModuleInfoC] =
        [proplists:get_value(Key, Config) ||
            Key <- [module_a, module_b, module_c]],

    {ok, Pid} = amoc_code_server:start_link(),

    %% note that test module is loaded only after the start of the amoc_code_server,
    %% so they are not added into 'configurable_modules' ETS table during the initial
    %% modules' analysis on amoc_code_server startup.
    ConfigurableModules = amoc_code_server:list_configurable_modules(),
    ScenarioModules = amoc_code_server:list_scenario_modules(),

    ?assertNot(code:is_loaded(ModuleName)),
    assert_uploaded_modules_ets([]),

    ?assertEqual([{ModuleName, ok}], upload_modules([ModuleInfoA])),
    assert_uploaded_modules_ets([ModuleInfoA]),
    assert_module_loaded(ModuleInfoA),

    %% uploading module with the same MD5 just returns 'ok'
    %% without actual modules reloading
    ?assertEqual([{ModuleName, ok}], upload_modules([ModuleInfoB])),
    ?assertError(_, assert_module_loaded(ModuleInfoB)),
    assert_module_loaded(ModuleInfoA),

    %% uploading module with another MD5 returns error
    %% without actual modules reloading
    ?assertEqual([{ModuleName, {error, module_version_has_changed}}],
                 upload_modules([ModuleInfoC])),
    ?assertError(_, assert_module_loaded(ModuleInfoC)),
    assert_module_loaded(ModuleInfoA),

    %% none of the actions above lead to the changes
    %% in the 'configurable_modules' ETS
    ?assertEqualLists(ConfigurableModules,
                      amoc_code_server:list_configurable_modules()),
    ?assertEqualLists(ScenarioModules,
                      amoc_code_server:list_scenario_modules()),

    gen_server:stop(Pid),
    ok.

%%---------------------------------------------------------
%% local functions
%%---------------------------------------------------------
extract_gen_server_call_request({'$gen_call', _From, CallData}) ->
    %% this function crashes if supplied message is not gen_server call
    CallData.

start_system_events_logging(Pid) ->
    ?assertEqual(ok, sys:log(Pid, true)),
    ?assertEqual({ok, []}, sys:log(Pid, get)).

stop_system_events_logging(Pid) ->
    {ok, LoggedEvents} = sys:log(Pid, get),
    ?assertEqual(ok, sys:log(Pid, false)),
    LoggedEvents.

recreate_working_dir(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    WorkingDir = filename:join(DataDir, "working_dir"),
    case filelib:is_dir(WorkingDir) of
        true -> ?assertEqual(ok, file:del_dir_r(WorkingDir));
        false -> ok
    end,
    ?assertEqual(ok, filelib:ensure_dir(WorkingDir)),
    ?assertEqual(ok, file:make_dir(WorkingDir)),
    WorkingDir.

unload_modules_and_restore_code_path(Config) ->
    [unload_module(ModuleInfo) || {_, #{module := _} = ModuleInfo} <- Config],
    restore_code_path(proplists:get_value(original_code_path, Config)).

generate_module(Config, TemplateVars) ->
    IncompleteModuleInfo = generate_source_code(Config, TemplateVars),
    compile_and_load_module(IncompleteModuleInfo).

generate_source_code(Config, TemplateVars0) ->
    DefaultTemplateVars = #{"module_name"=>"some_module",
                            "tag" => [$\n, $\n, $\n]},
    TemplateVars = maps:merge(DefaultTemplateVars, TemplateVars0),
    DataDir = proplists:get_value(data_dir, Config),
    TemplateFile = filename:join(DataDir, "module.mustache"),
    Template = bbmustache:parse_file(TemplateFile),
    SourceCode = bbmustache:compile(Template, TemplateVars),
    % ct:pal("Module:~n~n----~n~s~n----~n", [SourceCode]),
    ModuleName = maps:get("module_name", TemplateVars),
    Module = list_to_atom(ModuleName),
    WorkingDir = proplists:get_value(working_dir, Config),
    ?assertEqual(true, filelib:is_dir(WorkingDir)),
    #{module => Module, source_code => SourceCode, working_dir => WorkingDir}.

restore_code_path(OriginalCodePath) ->
    CurrentCodePath = code:get_path(),
    PathsToRemove = CurrentCodePath -- OriginalCodePath,
    ct:pal("PathsToRemove = ~p", [PathsToRemove]),
    [?assertEqual(true, code:del_path(P)) || P<- PathsToRemove].

%% module info helper functions
compile_and_load_module(#{working_dir := WorkingDir, module := Module,
                          source_code := SourceCode} = ModuleInfo) ->
    ModuleName = atom_to_list(Module),
    OutDirTemplate = filename:join(WorkingDir, ModuleName ++ ".XXXXX"),
    OutDir = string:trim(os:cmd("mktemp -d '" ++ OutDirTemplate ++ "'")),
    % ct:pal("OutDirTemplate = '~s'", [OutDirTemplate]),
    % ct:pal("OutDir = '~s'", [OutDir]),
    ErlFile = filename:join(WorkingDir, ModuleName ++ ".erl"),
    BeamFile = filename:join(OutDir, ModuleName ++ ".beam"),
    file:write_file(ErlFile, SourceCode),
    {ok, Module} = compile:file(filename:rootname(ErlFile),
                                [{outdir, OutDir}]),
    code:purge(Module), %% purge old version of module, if any
    {module, Module} = code:load_abs(filename:rootname(BeamFile)),
    code:purge(Module), %% purge old version of module, if any
    code:add_pathz(OutDir), %% required for code:get_object_code/1
    {Module, Binary, BeamFile} = code:get_object_code(Module),
    true = code:del_path(OutDir),
    MD5 = Module:module_info(md5),
    % ct:pal("~p module MD5: ~p", [Module, MD5]),
    % ct:pal("~p module bin: ~p", [Module, erlang:md5(Binary)]),
    ModuleInfo#{beam_filename => BeamFile, erl_file => ErlFile,
                out_dir => OutDir, binary => Binary, md5 => MD5,
                %% '$RECORD_NAME' key is required for consistency with internal
                %% uploaded_module record defined at amoc_code_sever
                '$RECORD_NAME' => uploaded_module}.

compare_module_info(ModuleInfoA, ModuleInfoB, EqualKeys0)
  when map_size(ModuleInfoA) =:= map_size(ModuleInfoB) ->
    ModuleInfoKeys = maps:keys(ModuleInfoA),
    ?assertEqual(ModuleInfoKeys, maps:keys(ModuleInfoA)),
    EqualKeys = ['$RECORD_NAME' | EqualKeys0],
    DifferentKeys = ModuleInfoKeys -- EqualKeys,
    compare_maps(ModuleInfoA, ModuleInfoB, EqualKeys, DifferentKeys).

compare_maps(MapA, MapB, EqualKeys, DifferentKeys) ->
    [?assertEqual(maps:get(Key, MapA), maps:get(Key, MapB)) ||
        Key <- EqualKeys],
    [?assertNotEqual(maps:get(Key, MapA), maps:get(Key, MapB)) ||
        Key <- DifferentKeys].

verify_upload_module_requests(ModuleInfoList, GenServerCalls) ->
    List1 = [map_to_uploaded_module(MI) || MI <- ModuleInfoList],
    List2 = [ M || {upload_module, M} <- GenServerCalls],
    ?assertEqualLists(List1, List2).

unload_module(#{module := Module}) ->
    unload_module(Module);
unload_module(Module) when is_atom(Module) ->
    case erlang:module_loaded(Module) of
        true ->
            code:purge(Module),
            ?assertEqual(true, code:delete(Module)),
            code:purge(Module),
            ?assertEqual(false, erlang:module_loaded(Module)),
            ?assertEqual(false, erlang:check_old_code(Module));
        false -> ok
    end.

load_module(#{module := Module, binary := Binary,
              beam_filename := BeamFile} = ModuleInfo) ->
    unload_module(ModuleInfo),
    code:load_binary(Module, BeamFile, Binary).

invalidate_filename(#{module := Module} = ModuleInfo) ->
    ModuleName = atom_to_list(Module),
    %% this DummyBeamFilename path is intentionally invalid, so
    %% amoc_code_server couldn't add a directory into code path
    DummyBeamFilename = ?DUMMY_CODE_PATH ++ ModuleName ++ ".beam",
    ModuleInfo#{beam_filename := DummyBeamFilename}.

assert_invalid_filename(#{beam_filename := ?DUMMY_CODE_PATH ++ _ = Path,
                          module := Module}) ->
    ?assertMatch({file, Path}, code:is_loaded(Module));
assert_invalid_filename(Module) when is_atom(Module) ->
    ?assertMatch({file, ?DUMMY_CODE_PATH ++ _}, code:is_loaded(Module)).

assert_module_loaded(#{beam_filename := BeamFileA, module := Module, md5 := MD5}) ->
    ?assertEqual(MD5, Module:module_info(md5)),
    ?assertEqual({file, BeamFileA}, code:is_loaded(Module)),
    ?assertEqual(BeamFileA, code:which(Module)).

assert_uploaded_modules_ets(Modules) ->
    ExpectedModules = [map_to_uploaded_module(M) || M <- Modules],
    UploadedModules = ets:tab2list(uploaded_modules),
    ?assertEqualLists(ExpectedModules, UploadedModules).

upload_modules(Modules) ->
    UploadedModuleRecords =
        [{ModName, map_to_uploaded_module(M)} || #{module := ModName} = M <- Modules],
    [{ModName, amoc_code_server:upload_module(node(), UploadedModule)} ||
        {ModName, UploadedModule} <- UploadedModuleRecords].
