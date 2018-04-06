-module(dialyzer_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [run_dialyzer].

init_per_suite(Config) ->
    ok = filelib:ensure_dir(filename:join(dialyzer_dir(), ".file")),
    Config.

end_per_suite(Config) ->
    Config.

run_dialyzer(Config) ->
    build_or_check_plts(Config),
    dialyze(Config).

dialyze(Config) ->
    ct:pal("Running analysis"),
    Plts = ["erlang.plt", "amoc.plt"],
    %% `gun` has issues with dialyzer which cannot be
    %% prevented by setting flags for dialyzer.
    %Plts = ["erlang.plt", "deps.plt", "amoc.plt"],
    run([{analysis_type, succ_typings},
         {plts, [ plt_file(Plt) || Plt <- Plts ]},
         {files_rec, [ebin_dir()]},
         {check_plt, false},
         {get_warnings, true},
         %% TODO: Remove those warnings
         {warnings, [no_undefined_callbacks, no_return, no_match]},
         {output_file, log_file(Config, "error.log")}]).

build_or_check_plts(Config) ->
    build_or_check_plt(plt_file("erlang.plt"),
                       [{apps, [kernel, stdlib, erts, crypto, compiler, ssl]},
                        {output_file, log_file(Config, "erlang.log")}]),
    %build_or_check_plt(plt_file("deps.plt"),
    %                   [{files_rec, deps_dirs()},
    %                    {output_file, log_file(Config, "deps.log")}]),
    build_or_check_plt(plt_file("amoc.plt"),
                       [{files_rec, [ebin_dir()]},
                        {output_file, log_file(Config, "amoc.log")}]),
    ok.

build_or_check_plt(File, Opts) ->
    case filelib:is_file(File) of
        false ->
            ct:pal("Building plt ~p", [File]),
            run([{output_plt, File}, {analysis_type, plt_build} | Opts]);
        true ->
            ct:pal("Checking plt ~p", [File]),
            run([{plts, [File]}, {analysis_type, plt_check} | Opts])
    end.

run(Opts) ->
    case dialyzer:run(Opts) of
        [] ->
            ok;
        Result ->
            ct:pal("Dialyzer step finished with errors:~n~p", [Result]),
            ct:fail({dialyzer_returned_errors, Result})
    end.

log_file(Config, Filename) ->
    filename:join(?config(priv_dir, Config), Filename).

plt_file(Filename) ->
    filename:join(dialyzer_dir(), Filename).

deps_dirs() ->
    filelib:wildcard(filename:join([amoc_dir(), "..", "*", "ebin"])) -- ebin_dir().

dialyzer_dir() ->
    filename:join([amoc_dir(), "dialyzer"]).

ebin_dir() ->
    filename:join([amoc_dir(), "ebin"]).

amoc_dir() ->
    code:lib_dir(amoc).
