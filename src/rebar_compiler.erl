-module(rebar_compiler).

-export([compile_all/2,
         clean/2,

         needs_compile/3,
         ok_tuple/2,
         error_tuple/4,
         maybe_report/1,
         format_error_source/2,
         report/1]).

-include("rebar.hrl").

-type extension() :: string().
-type out_mappings() :: [{extension(), file:filename()}].

-callback context(rebar_app_info:t()) -> #{src_dirs     => [file:dirname()],
                                           include_dirs => [file:dirname()],
                                           src_ext      => extension(),
                                           out_mappings => out_mappings()}.
-callback needed_files(digraph:graph(), [file:filename()], out_mappings(),
                       rebar_app_info:t()) ->
    {{[file:filename()], term()}, % ErlFirstFiles (erl_opts global priority)
     {[file:filename()] | % [Sequential]
      {[file:filename()], [file:filename()]}, % {Sequential, Parallel}
      term()}}.
-callback dependencies(file:filename(), file:dirname(), [file:dirname()]) -> [file:filename()].
-callback compile(file:filename(), out_mappings(), rebar_dict(), list()) ->
    ok | {ok, [string()]} | {ok, [string()], [string()]}.
-callback clean([file:filename()], rebar_app_info:t()) -> _.


-define(RE_PREFIX, "^(?!\\._)").

-spec compile_all([{module(), digraph:graph()}, ...], rebar_app_info:t()) -> ok
      ;          ([module(), ...], rebar_app_info:t()) -> ok.
compile_all(DAGs, AppInfo) when is_tuple(hd(DAGs)) -> % > 3.13.0
    prepare_compiler_env(AppInfo),
    lists:foreach(fun({Compiler, G}) ->
        run(G, Compiler, AppInfo),
        %% TODO: disable default recursivity in extra_src_dirs compiling to
        %% prevent compiling sample modules in _SUITE_data/ directories
        %% in CT.
        ExtraApps = annotate_extras(AppInfo),
        [run(G, Compiler, ExtraAppInfo) || ExtraAppInfo <- ExtraApps],
        ok
    end,
    DAGs);
compile_all(Compilers, AppInfo) -> % =< 3.13.0 interface; plugins use this!
    %% Support the old-style API by re-declaring a local DAG for the
    %% compile steps needed.
    lists:foreach(fun(Compiler) ->
        OutDir = rebar_app_info:out_dir(AppInfo),
        G = rebar_compiler_dag:init(OutDir, Compiler, undefined, []),
        compile_all([{Compiler, G}], AppInfo),
        rebar_compiler_dag:maybe_store(G, OutDir, Compiler, undefined, []),
        rebar_compiler_dag:terminate(G)
     end, Compilers).

prepare_compiler_env(AppInfo) ->
    EbinDir = rebar_utils:to_list(rebar_app_info:ebin_dir(AppInfo)),
    %% Make sure that outdir is on the path
    ok = rebar_file_utils:ensure_dir(EbinDir),
    true = code:add_patha(filename:absname(EbinDir)),

    %% necessary for erlang:function_exported/3 to work as expected
    %% called here for clarity as it's required by both opts_changed/2
    %% and erl_compiler_opts_set/0 in needed_files
    _ = code:ensure_loaded(compile),
    ok.

run(G, CompilerMod, AppInfo) ->
    #{src_dirs := SrcDirs,
      include_dirs := InclDirs,
      src_ext := SrcExt,
      out_mappings := Mappings} = CompilerMod:context(AppInfo),

    BaseDir = rebar_utils:to_list(rebar_app_info:dir(AppInfo)),
    EbinDir = rebar_utils:to_list(rebar_app_info:ebin_dir(AppInfo)),

    BaseOpts = rebar_app_info:opts(AppInfo),
    AbsInclDirs = [filename:join(BaseDir, InclDir) || InclDir <- InclDirs],
    FoundFiles = find_source_files(BaseDir, SrcExt, SrcDirs, BaseOpts),

    AbsSrcDirs = [filename:join(BaseDir, SrcDir) || SrcDir <- SrcDirs],

    InDirs = lists:usort(AbsInclDirs ++ AbsSrcDirs),

    rebar_compiler_dag:prune(G, AbsSrcDirs, EbinDir, FoundFiles),
    rebar_compiler_dag:update(G, CompilerMod, InDirs, FoundFiles),
    {{FirstFiles, FirstFileOpts},
     {RestFiles, Opts}} = CompilerMod:needed_files(G, FoundFiles, Mappings, AppInfo),

    compile_each(FirstFiles, FirstFileOpts, BaseOpts, Mappings, CompilerMod),
    case RestFiles of
        {Sequential, Parallel} -> % new parallelizable form
            compile_each(Sequential, Opts, BaseOpts, Mappings, CompilerMod),
            compile_parallel(Parallel, Opts, BaseOpts, Mappings, CompilerMod);
        _ when is_list(RestFiles) -> % traditional sequential build
            compile_each(RestFiles, Opts, BaseOpts, Mappings, CompilerMod)
    end.

compile_each([], _Opts, _Config, _Outs, _CompilerMod) ->
    ok;
compile_each([Source | Rest], Opts, Config, Outs, CompilerMod) ->
    case CompilerMod:compile(Source, Outs, Config, Opts) of
        ok ->
            ?DEBUG("~tsCompiled ~ts", [rebar_utils:indent(1), filename:basename(Source)]);
        {ok, Warnings} ->
            report(Warnings),
            ?DEBUG("~tsCompiled ~ts", [rebar_utils:indent(1), filename:basename(Source)]);
        skipped ->
            ?DEBUG("~tsSkipped ~ts", [rebar_utils:indent(1), filename:basename(Source)]);
        Error ->
            NewSource = format_error_source(Source, Config),
            ?ERROR("Compiling ~ts failed", [NewSource]),
            maybe_report(Error),
            ?DEBUG("Compilation failed: ~p", [Error]),
            ?FAIL
    end,
    compile_each(Rest, Opts, Config, Outs, CompilerMod).

compile_worker(QueuePid, Opts, Config, Outs, CompilerMod) ->
    QueuePid ! self(),
    receive
        {compile, Source} ->
            Result = CompilerMod:compile(Source, Outs, Config, Opts),
            QueuePid ! {Result, Source},
            compile_worker(QueuePid, Opts, Config, Outs, CompilerMod);
        empty ->
            ok
    end.

compile_parallel([], _Opts, _BaseOpts, _Mappings, _CompilerMod) ->
    ok;
compile_parallel(Targets, Opts, BaseOpts, Mappings, CompilerMod) ->
    Self = self(),
    F = fun() -> compile_worker(Self, Opts, BaseOpts, Mappings, CompilerMod) end,
    Jobs = min(length(Targets), erlang:system_info(schedulers)),
    ?DEBUG("Starting ~B compile worker(s)", [Jobs]),
    Pids = [spawn_monitor(F) || _I <- lists:seq(1, Jobs)],
    compile_queue(Targets, Pids, Opts, BaseOpts, Mappings, CompilerMod).

compile_queue([], [], _Opts, _Config, _Outs, _CompilerMod) ->
    ok;
compile_queue(Targets, Pids, Opts, Config, Outs, CompilerMod) ->
    receive
        Worker when is_pid(Worker), Targets =:= [] ->
            Worker ! empty,
            compile_queue(Targets, Pids, Opts, Config, Outs, CompilerMod);
        Worker when is_pid(Worker) ->
            Worker ! {compile, hd(Targets)},
            compile_queue(tl(Targets), Pids, Opts, Config, Outs, CompilerMod);
        {ok, Source} ->
            ?DEBUG("~sCompiled ~s", [rebar_utils:indent(1), Source]),
            compile_queue(Targets, Pids, Opts, Config, Outs, CompilerMod);
        {{ok, Warnings}, Source} ->
            report(Warnings),
            ?DEBUG("~sCompiled ~s", [rebar_utils:indent(1), Source]),
            compile_queue(Targets, Pids, Opts, Config, Outs, CompilerMod);
        {skipped, Source} ->
            ?DEBUG("~sSkipped ~s", [rebar_utils:indent(1), Source]),
            compile_queue(Targets, Pids, Opts, Config, Outs, CompilerMod);
        {Error, Source} ->
            NewSource = format_error_source(Source, Config),
            ?ERROR("Compiling ~ts failed", [NewSource]),
            maybe_report(Error),
            ?FAIL;
        {'DOWN', Mref, _, Pid, normal} ->
            Pids2 = lists:delete({Pid, Mref}, Pids),
            compile_queue(Targets, Pids2, Opts, Config, Outs, CompilerMod);
        {'DOWN', _Mref, _, _Pid, Info} ->
            ?ERROR("Compilation failed: ~p", [Info]),
            ?FAIL
    end.

%% @doc remove compiled artifacts from an AppDir.
-spec clean([module()], rebar_app_info:t()) -> 'ok'.
clean(Compilers, AppInfo) ->
    lists:foreach(fun(CompilerMod) ->
        clean_(CompilerMod, AppInfo, undefined),
        Extras = annotate_extras(AppInfo),
        [clean_(CompilerMod, ExtraApp, "extra") || ExtraApp <- Extras]
    end, Compilers).

clean_(CompilerMod, AppInfo, _Label) ->
    #{src_dirs := SrcDirs,
      src_ext := SrcExt} = CompilerMod:context(AppInfo),
    BaseDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),

    FoundFiles = find_source_files(BaseDir, SrcExt, SrcDirs, Opts),
    CompilerMod:clean(FoundFiles, AppInfo),
    ok.

-spec needs_compile(filename:all(), extension(), [{extension(), file:dirname()}]) -> boolean().
needs_compile(Source, OutExt, Mappings) ->
    Ext = filename:extension(Source),
    BaseName = filename:basename(Source, Ext),
    {_, OutDir} = lists:keyfind(OutExt, 1, Mappings),
    Target = filename:join(OutDir, BaseName++OutExt),
    filelib:last_modified(Source) > filelib:last_modified(Target).

annotate_extras(AppInfo) ->
    ExtraDirs = rebar_dir:extra_src_dirs(rebar_app_info:opts(AppInfo), []),
    OldSrcDirs = rebar_app_info:get(AppInfo, src_dirs, ["src"]),
    AppDir = rebar_app_info:dir(AppInfo),
    lists:map(fun(Dir) ->
        EbinDir = filename:join(rebar_app_info:out_dir(AppInfo), Dir),
        AppInfo1 = rebar_app_info:ebin_dir(AppInfo, EbinDir),
        AppInfo2 = rebar_app_info:set(AppInfo1, src_dirs, [Dir]),
        AppInfo3 = rebar_app_info:set(AppInfo2, extra_src_dirs, OldSrcDirs),
        add_to_includes( % give access to .hrl in app's src/
            AppInfo3,
            [filename:join([AppDir, D]) || D <- OldSrcDirs]
        )
    end,
    [ExtraDir || ExtraDir <- ExtraDirs,
                 filelib:is_dir(filename:join(AppDir, ExtraDir))]
    ).

%% These functions are here for the ultimate goal of getting rid of
%% rebar_base_compiler. This can't be done because of existing plugins.

ok_tuple(Source, Ws) ->
    rebar_base_compiler:ok_tuple(Source, Ws).

error_tuple(Source, Es, Ws, Opts) ->
    rebar_base_compiler:error_tuple(Source, Es, Ws, Opts).

maybe_report(Reportable) ->
    rebar_base_compiler:maybe_report(Reportable).

format_error_source(Path, Opts) ->
    rebar_base_compiler:format_error_source(Path, Opts).

report(Messages) ->
    rebar_base_compiler:report(Messages).

%%% private functions

find_source_files(BaseDir, SrcExt, SrcDirs, Opts) ->
    SourceExtRe = "^(?!\\._).*\\" ++ SrcExt ++ [$$],
    lists:flatmap(fun(SrcDir) ->
                      Recursive = rebar_dir:recursive(Opts, SrcDir),
                      rebar_utils:find_files_in_dirs([filename:join(BaseDir, SrcDir)], SourceExtRe, Recursive)
                  end, SrcDirs).

add_to_includes(AppInfo, Dirs) ->
    Opts = rebar_app_info:opts(AppInfo),
    List = rebar_opts:get(Opts, erl_opts, []),
    NewErlOpts = [{i, Dir} || Dir <- Dirs] ++ List,
    NewOpts = rebar_opts:set(Opts, erl_opts, NewErlOpts),
    rebar_app_info:opts(AppInfo, NewOpts).
