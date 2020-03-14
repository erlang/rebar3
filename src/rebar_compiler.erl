-module(rebar_compiler).

-export([analyze_all/2,
         compile_analyzed/3,
         compile_all/2,
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

-callback context(rebar_app_info:t()) -> #{src_dirs     => [file:dirname()], % mandatory
                                           include_dirs => [file:dirname()], % mandatory
                                           src_ext      => extension(),      % mandatory
                                           out_mappings => out_mappings(),   % mandatory
                                           dependencies_opts => term()}.     % optional
-callback needed_files(digraph:graph(), [file:filename()], out_mappings(),
                       rebar_app_info:t()) ->
    {{[file:filename()], term()}, % ErlFirstFiles (erl_opts global priority)
     {[file:filename()] | % [Sequential]
      {[file:filename()], [file:filename()]}, % {Sequential, Parallel}
      term()}}.
-callback dependencies(file:filename(), file:dirname(), [file:dirname()]) -> [file:filename()].
-callback dependencies(file:filename(), file:dirname(), [file:dirname()], term()) -> [file:filename()].
-callback compile(file:filename(), out_mappings(), rebar_dict(), list()) ->
    ok | {ok, [string()]} | error | {error, [string()], [string()]} | skipped.
-callback compile_and_track(file:filename(), out_mappings(), rebar_dict(), list()) ->
    {ok, [{file:filename(), file:filename(), term()}]} |
    {ok, [{file:filename(), file:filename(), term()}], [string()]} |
    {error, [string()], [string()]} | error.
-callback clean([file:filename()], rebar_app_info:t()) -> _.

-optional_callbacks([dependencies/4, compile_and_track/4]).

-define(RE_PREFIX, "^(?!\\._)").


%% @doc analysis by the caller, in order to let an OTP app
%% find and resolve all its dependencies as part of compile_all's new
%% API, which presumes a partial analysis is done ahead of time
-spec analyze_all(DAG, [App, ...]) -> ok when
      DAG :: {module(), digraph:graph()},
      App :: rebar_app_info:t().
analyze_all({Compiler, G}, Apps) ->
    prepare_compiler_env(Compiler, Apps),
    %% Analyze apps one by one
    %% then cover the include files in the digraph to update them
    %% then propagate?
    Contexts = gather_contexts(Compiler, Apps),
    AppRes = [analyze_app({Compiler, G}, Contexts, AppInfo) || AppInfo <- Apps],
    {AppOutPaths, AbsSources} = lists:unzip(AppRes),
    SrcExt = maps:get(src_ext, Contexts),
    OutExt = maps:get(artifact_exts, Contexts),

    rebar_compiler_dag:prune(
        G, SrcExt, OutExt, lists:append(AbsSources), AppOutPaths
    ),
    rebar_compiler_dag:populate_deps(G, SrcExt, OutExt),
    rebar_compiler_dag:propagate_stamps(G),

    AppPaths = [{rebar_app_info:name(AppInfo),
                 rebar_utils:to_list(rebar_app_info:dir(AppInfo))}
                || AppInfo <- Apps],
    AppNames = rebar_compiler_dag:compile_order(G, AppPaths),
    {Contexts, sort_apps(AppNames, Apps)}.

gather_contexts(Compiler, Apps) ->
    Default = default_ctx(),
    Contexts = [{rebar_app_info:name(AppInfo),
                 maps:merge(Default, Compiler:context(AppInfo))}
                || AppInfo <- Apps],
    ContextMap = maps:from_list(Contexts),
    %% only support one extension type at once for now
    [{_, #{src_ext := SrcExt}} | _] = Contexts,
    %% gather multi-app stuff once to avoid recomputing it
    ArtifactExts = lists:usort(
        [Ext || {_, #{out_mappings := Mappings}} <- Contexts,
                {Ext, _Dir} <- Mappings]
    ),
    InDirs = gather_in_dirs(lists:zip(Apps, [Context || {_, Context} <- Contexts])),
    ContextMap#{src_ext => SrcExt,
                artifact_exts => ArtifactExts,
                in_dirs => InDirs}.

gather_in_dirs(AppCtx) ->
    gather_in_dirs(AppCtx, []).

gather_in_dirs([], Paths) ->
    lists:usort(Paths);
gather_in_dirs([{AppInfo, Ctx} | Rest], Acc) ->
    #{include_dirs := InclDirs,
      src_dirs := SrcDirs} = Ctx,
    BaseDir = rebar_utils:to_list(rebar_app_info:dir(AppInfo)),
    AbsIncl = [filename:join(BaseDir, InclDir) || InclDir <- InclDirs],
    AbsSrc = [filename:join(BaseDir, SrcDir) || SrcDir <- SrcDirs],
    gather_in_dirs(Rest, AbsSrc ++ AbsIncl ++ Acc).

analyze_app({Compiler, G}, Contexts, AppInfo) ->
    AppName = rebar_app_info:name(AppInfo),
    BaseDir = rebar_utils:to_list(rebar_app_info:dir(AppInfo)),
    OutDir = rebar_utils:to_list(rebar_app_info:out_dir(AppInfo)),
    BaseOpts = rebar_app_info:opts(AppInfo),
    #{src_dirs := SrcDirs,
      src_ext := SrcExt,
      out_mappings := [{_OutExt, OutPath}|_], % prune one dir for now (compat mode!)
      dependencies_opts := DepOpts} = maps:get(AppName, Contexts),
    %% Local resources
    ArtifactDir = filename:join([OutDir, OutPath]),
    AbsSources = find_source_files(BaseDir, SrcExt, SrcDirs, BaseOpts),
    %% Multi-app resources
    InDirs = maps:get(in_dirs, Contexts),
    %% Run the analysis
    rebar_compiler_dag:populate_sources(
        G, Compiler, InDirs, AbsSources, DepOpts
    ),
    {{BaseDir, ArtifactDir}, AbsSources}.

sort_apps(Names, Apps) ->
    NamedApps = [{rebar_app_info:name(App), App} || App <- Apps],
    [App || Name <- Names,
            {_, App} <- [lists:keyfind(Name, 1, NamedApps)]].

-spec compile_analyzed({module(), digraph:graph()}, rebar_app_info:t(), map()) -> ok.
compile_analyzed({Compiler, G}, AppInfo, Contexts) -> % > 3.13.0
    run(G, Compiler, AppInfo, Contexts),
    %% Extras are tricky and get their own mini-analysis
    ExtraApps = annotate_extras(AppInfo),
    [begin
         {ExtraCtx, [SortedExtra]} = analyze_all({Compiler, G}, [ExtraAppInfo]),
         run(G, Compiler, SortedExtra, ExtraCtx)
     end || ExtraAppInfo <- ExtraApps],
    ok.

-spec compile_all([module(), ...], rebar_app_info:t()) -> ok.
compile_all(Compilers, AppInfo) -> % =< 3.13.0 interface; plugins use this!
    %% Support the old-style API by re-declaring a local DAG for the
    %% compile steps needed.
    lists:foreach(fun(Compiler) ->
        OutDir = rebar_app_info:out_dir(AppInfo),
        G = rebar_compiler_dag:init(OutDir, Compiler, undefined, []),
        Ctx = analyze_all({Compiler, G}, [AppInfo]),
        compile_analyzed({Compiler, G}, AppInfo, Ctx),
        rebar_compiler_dag:maybe_store(G, OutDir, Compiler, undefined, []),
        rebar_compiler_dag:terminate(G)
     end, Compilers).

prepare_compiler_env(Compiler, Apps) ->
    lists:foreach(
        fun(AppInfo) ->
            EbinDir = rebar_utils:to_list(rebar_app_info:ebin_dir(AppInfo)),
            %% Make sure that outdir is on the path
            ok = rebar_file_utils:ensure_dir(EbinDir),
            true = code:add_patha(filename:absname(EbinDir))
        end,
        Apps
    ),
    %% necessary for erlang:function_exported/3 to work as expected
    %% called here for clarity as it's required by both opts_changed/2
    %% and erl_compiler_opts_set/0 in needed_files
    _ = code:ensure_loaded(compile),
    _ = code:ensure_loaded(Compiler),
    ok.

run(G, CompilerMod, AppInfo, Contexts) ->
    Name = rebar_app_info:name(AppInfo),
    #{src_dirs := SrcDirs,
      src_ext := SrcExt,
      out_mappings := Mappings} = maps:get(Name, Contexts),

    BaseDir = rebar_utils:to_list(rebar_app_info:dir(AppInfo)),

    BaseOpts = rebar_app_info:opts(AppInfo),
    FoundFiles = find_source_files(BaseDir, SrcExt, SrcDirs, BaseOpts),

    {{FirstFiles, FirstFileOpts},
     {RestFiles, Opts}} = CompilerMod:needed_files(G, FoundFiles, Mappings, AppInfo),

    compile_each(FirstFiles, FirstFileOpts, BaseOpts, Mappings, CompilerMod),
    Tracked = case RestFiles of
        {Sequential, Parallel} -> % parallelizable form
            compile_each(Sequential, Opts, BaseOpts, Mappings, CompilerMod) ++
            compile_parallel(Parallel, Opts, BaseOpts, Mappings, CompilerMod);
        _ when is_list(RestFiles) -> % traditional sequential build
            compile_each(RestFiles, Opts, BaseOpts, Mappings, CompilerMod)
    end,
    store_artifacts(G, Tracked).

compile_each([], _Opts, _Config, _Outs, _CompilerMod) ->
    [];
compile_each([Source | Rest], Opts, Config, Outs, CompilerMod) ->
    case erlang:function_exported(CompilerMod, compile_and_track, 4) of
        false ->
            do_compile(CompilerMod, Source, Outs, Config, Opts),
            compile_each(Rest, Opts, Config, Outs, CompilerMod);
        true ->
            do_compile_and_track(CompilerMod, Source, Outs, Config, Opts)
            ++ compile_each(Rest, Opts, Config, Outs, CompilerMod)
    end.

do_compile(CompilerMod, Source, Outs, Config, Opts) ->
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
    end.

do_compile_and_track(CompilerMod, Source, Outs, Config, Opts) ->
    case CompilerMod:compile_and_track(Source, Outs, Config, Opts) of
        {ok, Tracked} ->
            ?DEBUG("~tsCompiled ~ts", [rebar_utils:indent(1), filename:basename(Source)]),
            Tracked;
        {ok, Tracked, Warnings} ->
            report(Warnings),
            ?DEBUG("~tsCompiled ~ts", [rebar_utils:indent(1), filename:basename(Source)]),
            Tracked;
        skipped ->
            ?DEBUG("~tsSkipped ~ts", [rebar_utils:indent(1), filename:basename(Source)]),
            [];
        Error ->
            NewSource = format_error_source(Source, Config),
            ?ERROR("Compiling ~ts failed", [NewSource]),
            maybe_report(Error),
            ?DEBUG("Compilation failed: ~p", [Error]),
            ?FAIL
    end.

store_artifacts(_G, []) ->
    ok;
store_artifacts(G, [{Source, Target, Meta}|Rest]) ->
    %% Assume the source exists since it was tracked to be compiled
    digraph:add_vertex(G, Target, {artifact, Meta}),
    digraph:add_edge(G, Target, Source, artifact),
    store_artifacts(G, Rest).

compile_worker(QueuePid, Opts, Config, Outs, CompilerMod) ->
    QueuePid ! self(),
    receive
        {compile, Source} ->
            Result =
            case erlang:function_exported(CompilerMod, compile_and_track, 4) of
                false ->
                    CompilerMod:compile(Source, Outs, Config, Opts);
                true ->
                    CompilerMod:compile_and_track(Source, Outs, Config, Opts)
            end,
            QueuePid ! {Result, Source},
            compile_worker(QueuePid, Opts, Config, Outs, CompilerMod);
        empty ->
            ok
    end.

compile_parallel([], _Opts, _BaseOpts, _Mappings, _CompilerMod) ->
    [];
compile_parallel(Targets, Opts, BaseOpts, Mappings, CompilerMod) ->
    Self = self(),
    F = fun() -> compile_worker(Self, Opts, BaseOpts, Mappings, CompilerMod) end,
    Jobs = min(length(Targets), erlang:system_info(schedulers)),
    ?DEBUG("Starting ~B compile worker(s)", [Jobs]),
    Pids = [spawn_monitor(F) || _I <- lists:seq(1, Jobs)],
    compile_queue(Targets, Pids, Opts, BaseOpts, Mappings, CompilerMod).

compile_queue([], [], _Opts, _Config, _Outs, _CompilerMod) ->
    [];
compile_queue(Targets, Pids, Opts, Config, Outs, CompilerMod) ->
    Tracking = erlang:function_exported(CompilerMod, compile_and_track, 4),
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
        {{ok, Tracked}, Source} when Tracking ->
            ?DEBUG("~sCompiled ~s", [rebar_utils:indent(1), Source]),
            Tracked ++
              compile_queue(Targets, Pids, Opts, Config, Outs, CompilerMod);
        {{ok, Warnings}, Source} when not Tracking ->
            report(Warnings),
            ?DEBUG("~sCompiled ~s", [rebar_utils:indent(1), Source]),
            compile_queue(Targets, Pids, Opts, Config, Outs, CompilerMod);
        {{ok, Tracked, Warnings}, Source} when Tracking ->
            report(Warnings),
            ?DEBUG("~sCompiled ~s", [rebar_utils:indent(1), Source]),
            Tracked ++
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

default_ctx() ->
    #{dependencies_opts => []}.
