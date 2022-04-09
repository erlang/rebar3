-module(rebar_prv_compile).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-export([compile/2, compile/3, compile/4]).
-export([copy_app_dirs/3]).

-include_lib("providers/include/providers.hrl").
-include("rebar.hrl").

-define(PROVIDER, compile).
-define(ERLC_HOOK, erlc_compile).
-define(APP_HOOK, app_compile).
-define(DEPS, [lock]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 compile"},
                                                               {short_desc, "Compile apps .app.src and .erl files."},
                                                               {desc, "Compile apps .app.src and .erl files."},
                                                               {opts, [{deps_only, $d, "deps_only", undefined,
                                                                        "Only compile dependencies, no project apps will be built."}]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    IsDepsOnly = is_deps_only(State),
    rebar_paths:set_paths([deps], State),

    Providers = rebar_state:providers(State),
    MustBuildDeps = rebar_state:deps_to_build(State),
    Deps = rebar_state:all_deps(State),
    CompiledDeps = copy_and_build_deps(State, Providers, MustBuildDeps, Deps),
    State0 = rebar_state:merge_all_deps(State, CompiledDeps),

    State1 = case IsDepsOnly of
                 true ->
                     State0;
                 false ->
                     handle_project_apps(Providers, State0)
             end,

    rebar_paths:set_paths([plugins], State1),

    {ok, State1}.

is_deps_only(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    proplists:get_value(deps_only, Args, false).

handle_project_apps(Providers, State) ->
    Cwd = rebar_state:dir(State),
    ProjectApps = rebar_state:project_apps(State),
    {ok, ProjectApps1} = rebar_digraph:compile_order(ProjectApps),

    %% Run top level hooks *before* project apps compiled but *after* deps are
    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State),

    ProjectApps2 = copy_and_build_project_apps(State, Providers, ProjectApps1),
    State2 = rebar_state:project_apps(State, ProjectApps2),

    %% build extra_src_dirs in the root of multi-app projects
    build_root_extras(State, ProjectApps2),

    State3 = update_code_paths(State2, ProjectApps2),

    rebar_hooks:run_all_hooks(Cwd, post, ?PROVIDER, Providers, State3),
    case rebar_state:has_all_artifacts(State3) of
        {false, File} ->
            throw(?PRV_ERROR({missing_artifact, File}));
        true ->
            true
    end,

    State3.


-spec format_error(any()) -> iolist().
format_error({missing_artifact, File}) ->
    io_lib:format("Missing artifact ~ts", [File]);
format_error({bad_project_builder, Name, Type, Module}) ->
    io_lib:format("Error building application ~s:~n     Required project builder ~s function "
                  "~s:build/1 not found", [Name, Type, Module]);
format_error({unknown_project_type, Name, Type}) ->
    io_lib:format("Error building application ~s:~n     "
                  "No project builder is configured for type ~s", [Name, Type]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

pick_deps_to_build(State, MustBuild, All, Tag) ->
    InvalidDags = lists:any(
        fun({_Mod, Status}) ->
            %% A DAG that is not found can be valid, it just has no matching
            %% source file. However, bad_vsn, bad_format, and bad_meta should
            %% all trigger rebuilds.
            %% Good:
            %%  lists:member(Status, [valid, not_found])
            %% Bad:
            %%  lists:member(Status, [bad_vsn, bad_format, bad_meta])
            %%
            %% Since the fastest check is done on smaller lists handling
            %% the common case first:
            not lists:member(Status, [valid, not_found])
        end,
        check_dags(State, Tag)
    ),
    case InvalidDags of
        true -> All;
        false -> MustBuild
    end.

copy_and_build_deps(State, Providers, MustBuildApps, AllApps) ->
    Apps = pick_deps_to_build(State, MustBuildApps, AllApps, apps),
    Apps0 = [prepare_app(State, Providers, App) || App <- Apps],
    compile(State, Providers, Apps0, apps).

copy_and_build_project_apps(State, Providers, Apps) ->
    Apps0 = [prepare_project_app(State, Providers, App) || App <- Apps],
    compile(State, Providers, Apps0, project_apps).

-spec compile(rebar_state:t(), [rebar_app_info:t()]) -> [rebar_app_info:t()]
      ;      (rebar_state:t(), rebar_app_info:t()) -> rebar_app_info:t().
compile(State, AppInfo) ->
    compile(State, rebar_state:providers(State), AppInfo).

-spec compile(rebar_state:t(), [providers:t()],
              [rebar_app_info:t()]) -> [rebar_app_info:t()]
      ;      (rebar_state:t(), [providers:t()],
              rebar_app_info:t()) -> rebar_app_info:t().
compile(State, Providers, AppInfo) when not is_list(AppInfo) ->
    [Res] = compile(State, Providers, [AppInfo], undefined),
    Res;
compile(State, Providers, Apps) ->
    compile(State, Providers, Apps, undefined).

-spec compile(rebar_state:t(), [providers:t()],
              [rebar_app_info:t()], atom() | undefined) -> [rebar_app_info:t()].
compile(State, Providers, Apps, Tag) ->
    ?DEBUG("Compile (~p)", [if Tag =:= undefined -> untagged; true -> Tag end]),
    Apps1 = [prepare_compile(State, Providers, App) || App <- Apps],
    Apps2 = [prepare_compilers(State, Providers, App) || App <- Apps1],
    Apps3 = run_compilers(State, Providers, Apps2, Tag),
    Apps4 = [finalize_compilers(State, Providers, App) || App <- Apps3],
    Apps5 = [prepare_app_file(State, Providers, App) || App <- Apps4],
    Apps6 = compile_app_files(State, Providers, Apps5),
    Apps7 = [finalize_app_file(State, Providers, App) || App <- Apps6],
    [finalize_compile(State, Providers, App) || App <- Apps7].

prepare_app(_State, _Providers, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    OutDir = rebar_app_info:out_dir(AppInfo),
    copy_app_dirs(AppInfo, AppDir, OutDir),
    AppInfo.

prepare_project_app(_State, _Providers, AppInfo) ->
    copy_app_dirs(AppInfo,
                  rebar_app_info:dir(AppInfo),
                  rebar_app_info:out_dir(AppInfo)),
    %% application code path must be added to the source
    %%  otherwise code_server will remember 'lib_dir' for
    %%  this application, and all `-include_lib` directives
    %%  will actually go into _build/profile/lib/...
    code:add_pathsz([rebar_app_info:dir(AppInfo)]),
    AppInfo.

prepare_compile(State, Providers, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    rebar_hooks:run_all_hooks(AppDir, pre, ?PROVIDER,  Providers, AppInfo, State).

prepare_compilers(State, Providers, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    rebar_hooks:run_all_hooks(AppDir, pre, ?ERLC_HOOK, Providers, AppInfo, State).

run_compilers(State, _Providers, Apps, Tag) ->
    %% Get the compiler graphs for all modules and artifacts, for each
    %% compiler we run
    DAGsInfo = load_dags(State, Tag),
    DAGs = [{Mod, DAG} || {Mod, {DAG, _Meta}} <- DAGsInfo],
    %% Compile all the apps
    build_apps(DAGs, Apps, State),
    %% Potentially store shared compiler DAGs so next runs can easily
    %% share the base information for easy re-scans.
    lists:foreach(fun({Mod, {G, {Dir, DAGLabel, CritMeta}}}) ->
        rebar_compiler_dag:maybe_store(G, Dir, Mod, DAGLabel, CritMeta),
        rebar_compiler_dag:terminate(G)
    end, DAGsInfo),
    Apps.

load_dags(State, Tag) ->
    F = fun(Dir, Mod, DAGLabel, CritMeta) ->
            {rebar_compiler_dag:init(Dir, Mod, DAGLabel, CritMeta),
             {Dir, DAGLabel, CritMeta}}
        end,
    map_dags(F, State, Tag).

check_dags(State, Tag) ->
    map_dags(fun rebar_compiler_dag:status/4, State, Tag).

map_dags(F, State, Tag) ->
    DAGLabel = format_label(Tag),
    %% The Dir for the DAG is set to deps_dir so builds taking place
    %% in different contexts (i.e. plugins) don't risk clobbering regular deps.
    Dir = rebar_dir:deps_dir(State),
    SharedCritMeta = [],
    [{Mod, F(Dir, Mod, DAGLabel, mod_meta(Mod, SharedCritMeta))}
     || Mod <- rebar_state:compilers(State)].

mod_meta(rebar_compiler_erl, CritMeta) ->
    application:load(compiler),
    {ok, CompileVsn} = application:get_key(compiler, vsn),
    [{compiler, CompileVsn} | CritMeta];
mod_meta(_, CritMeta) ->
    CritMeta.

format_label(Tag) ->
    %% The Tag allows to create a Label when someone cares about a specific
    %% run for compilation;
    case Tag of
        undefined -> undefined;
        _ -> atom_to_list(Tag)
    end.

finalize_compilers(State, Providers, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    rebar_hooks:run_all_hooks(AppDir, post, ?ERLC_HOOK, Providers, AppInfo, State).

prepare_app_file(State, Providers, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    rebar_hooks:run_all_hooks(AppDir, pre, ?APP_HOOK, Providers, AppInfo, State).

compile_app_files(State, Providers, Apps) ->
    %% Load plugins back for make_vsn calls in custom resources.
    %% The rebar_otp_app compilation step is safe regarding the
    %% overall path management, so we can just load all plugins back
    %% in memory.
    rebar_paths:set_paths([plugins], State),
    NewApps = [compile_app_file(State, Providers, App) || App <- Apps],
    %% Clean up after ourselves, leave things as they were with deps first
    rebar_paths:set_paths([deps], State),
    NewApps.

compile_app_file(State, _Providers, AppInfo) ->
    case rebar_otp_app:compile(State, AppInfo) of
        {ok, AppInfo2} -> AppInfo2;
        Error -> throw(Error)
    end.

finalize_app_file(State, Providers, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    rebar_hooks:run_all_hooks(AppDir, post, ?APP_HOOK, Providers, AppInfo, State).

finalize_compile(State, Providers, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    AppInfo2 = rebar_hooks:run_all_hooks(AppDir, post, ?PROVIDER, Providers, AppInfo, State),
    has_all_artifacts(AppInfo),
    AppInfo2.

build_root_extras(State, Apps) ->
    %% The root extra src dirs belong to no specific applications;
    %% because the compiler works on OTP apps, we instead build
    %% a fake AppInfo record that only contains the root extra_src
    %% directories, has access to all the top-level apps' public
    %% include files, and builds to a specific extra outdir.
    %% TODO: figure out digraph strategy to properly ensure no
    %%       cross-contamination but proper change detection.
    BaseDir = rebar_state:dir(State),
    F = fun(App) -> rebar_app_info:dir(App) == BaseDir end,
    case lists:any(F, Apps) of
        true ->
            [];
        false ->
            ProjOpts = rebar_state:opts(State),
            Extras = rebar_dir:extra_src_dirs(ProjOpts, []),
            {ok, VirtApp} = rebar_app_info:new("extra", "0.0.0", BaseDir, []),

            case extra_virtual_apps(State, VirtApp, Extras) of
                %% If there are no virtual apps do nothing.
                [] ->
                    [];
                VirtApps ->
                    %% re-use the project-apps digraph?
                    run_compilers(State, [], VirtApps, project_apps)
            end
    end.

extra_virtual_apps(_, _, []) ->
    [];
extra_virtual_apps(State, VApp0, [Dir|Dirs]) ->
    SrcDir = filename:join([rebar_state:dir(State), Dir]),
    case ec_file:is_dir(SrcDir) of
        false ->
            extra_virtual_apps(State, VApp0, Dirs);
        true ->
            BaseDir = filename:join([rebar_dir:base_dir(State), "extras"]),
            OutDir = filename:join([BaseDir, Dir]),
            copy(rebar_state:dir(State), BaseDir, Dir),
            VApp1 = rebar_app_info:out_dir(VApp0, BaseDir),
            VApp2 = rebar_app_info:ebin_dir(VApp1, OutDir),
            Opts = rebar_state:opts(State),
            VApp3 = rebar_app_info:opts(VApp2, Opts),
            %% ensure we don't end up running a similar extra fake app while
            %% compiling root-level extras by marking it explicitly null
            VApp4 = rebar_app_info:set(VApp3, extra_src_dirs, []),
            %% need a unique name to prevent lookup issues that clobber entries
            AppName = unicode:characters_to_binary(["extra_", Dir]),
            VApp5 = rebar_app_info:name(VApp4, AppName),
            [rebar_app_info:set(VApp5, src_dirs, [OutDir])
             | extra_virtual_apps(State, VApp0, Dirs)]
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

build_apps(DAGs, Apps, State) ->
    {Rebar3, Custom} = lists:partition(
        fun(AppInfo) ->
            Type = rebar_app_info:project_type(AppInfo),
            Type =:= rebar3 orelse Type =:= undefined
        end,
        Apps
    ),
    [build_custom_builder_app(AppInfo, State) || AppInfo <- Custom],
    build_rebar3_apps(DAGs, Rebar3, State).

build_custom_builder_app(AppInfo, State) ->
    ?INFO("Compiling ~ts", [rebar_app_info:name(AppInfo)]),
    Type = rebar_app_info:project_type(AppInfo),
    ProjectBuilders = rebar_state:project_builders(State),
    case lists:keyfind(Type, 1, ProjectBuilders) of
        {_, Module} ->
            %% load plugins since that's where project builders would be,
            %% prevents parallelism at this level.
            rebar_paths:set_paths([deps, plugins], State),
            Res = Module:build(AppInfo),
            rebar_paths:set_paths([deps], State),
            case Res of
                ok -> ok;
                {error, Reason} -> throw({error, {Module, Reason}})
            end;
        _ ->
            throw(?PRV_ERROR({unknown_project_type, rebar_app_info:name(AppInfo), Type}))
    end.

build_rebar3_apps(DAGs, Apps, _State) when DAGs =:= []; Apps =:= [] ->
    %% No apps to actually build, probably just other compile phases
    %% to run for non-rebar3 apps, someone wanting .app files built,
    %% or just needing the hooks to run maybe.
    ok;
build_rebar3_apps(DAGs, Apps, State) ->
    rebar_paths:set_paths([deps], State),
    %% To maintain output order, we need to mention each app being compiled
    %% in order, even if the order isn't really there anymore due to each
    %% compiler being run in broken sequence. The last compiler tends to be
    %% the big ERLC one so we use the last compiler for the output.
    LastDAG = lists:last(DAGs),
    %% we actually need to compile each DAG one after the other to prevent
    %% issues where a .yrl file that generates a .erl file gets to be seen.
    ?INFO("Analyzing applications...", []),
    [begin
         {Ctx, ReorderedApps} = rebar_compiler:analyze_all(DAG, Apps),
         lists:foreach(
             fun(AppInfo) ->
                DAG =:= LastDAG andalso
                  ?INFO("Compiling ~ts", [rebar_app_info:name(AppInfo)]),
                rebar_compiler:compile_analyzed(DAG, AppInfo, Ctx)
             end,
             ReorderedApps
         ),
         {ExtraCtx, ReorderedExtraApps} = rebar_compiler:analyze_all_extras(DAG, Apps),
         lists:foreach(
             fun(AppInfo) ->
                rebar_compiler:compile_analyzed(DAG, AppInfo, ExtraCtx)
             end,
             ReorderedExtraApps
         )
     end || DAG <- DAGs],
    ok.

update_code_paths(State, ProjectApps) ->
    ProjAppsPaths = paths_for_apps(ProjectApps),
    ExtrasPaths = paths_for_extras(State, ProjectApps),
    DepsPaths = rebar_state:code_paths(State, all_deps),
    rebar_state:code_paths(State, all_deps, DepsPaths ++ ProjAppsPaths ++ ExtrasPaths).

paths_for_apps(Apps) -> paths_for_apps(Apps, []).

paths_for_apps([], Acc) -> Acc;
paths_for_apps([App|Rest], Acc) ->
    {_SrcDirs, ExtraDirs} = resolve_src_dirs(rebar_app_info:opts(App)),
    Paths = [filename:join([rebar_app_info:out_dir(App), Dir]) || Dir <- ["ebin"|ExtraDirs]],
    FilteredPaths = lists:filter(fun ec_file:is_dir/1, Paths),
    paths_for_apps(Rest, Acc ++ FilteredPaths).

paths_for_extras(State, Apps) ->
    F = fun(App) -> rebar_app_info:dir(App) == rebar_state:dir(State) end,
    %% check that this app hasn't already been dealt with
    case lists:any(F, Apps) of
        false -> paths_for_extras(State);
        true  -> []
    end.

paths_for_extras(State) ->
    {_SrcDirs, ExtraDirs} = resolve_src_dirs(rebar_state:opts(State)),
    Paths = [filename:join([rebar_dir:base_dir(State), "extras", Dir]) || Dir <- ExtraDirs],
    lists:filter(fun ec_file:is_dir/1, Paths).

has_all_artifacts(AppInfo1) ->
    case rebar_app_info:has_all_artifacts(AppInfo1) of
        {false, File} ->
            throw(?PRV_ERROR({missing_artifact, File}));
        true ->
            rebar_app_utils:lint_app_info(AppInfo1),
            true
    end.

copy_app_dirs(AppInfo, OldAppDir, AppDir) ->
    case rebar_utils:to_binary(filename:absname(OldAppDir)) =/=
        rebar_utils:to_binary(filename:absname(AppDir)) of
        true ->
            EbinDir = filename:join([OldAppDir, "ebin"]),
            %% copy all files from ebin if it exists
            case filelib:is_dir(EbinDir) of
                true ->
                    OutEbin = filename:join([AppDir, "ebin"]),
                    filelib:ensure_dir(filename:join([OutEbin, "dummy.beam"])),
                    ?DEBUG("Copying existing files from ~ts to ~ts", [EbinDir, OutEbin]),
                    rebar_file_utils:cp_r(filelib:wildcard(filename:join([EbinDir, "*"])), OutEbin);
                false ->
                    ok
            end,

            filelib:ensure_dir(filename:join([AppDir, "dummy"])),

            %% link or copy mibs if it exists
            case filelib:is_dir(filename:join([OldAppDir, "mibs"])) of
                true ->
                    %% If mibs exist it means we must ensure priv exists.
                    %% mibs files are compiled to priv/mibs/
                    filelib:ensure_dir(filename:join([OldAppDir, "priv", "dummy"])),
                    symlink_or_copy(OldAppDir, AppDir, "mibs");
                false ->
                    ok
            end,
            {SrcDirs, ExtraDirs} = resolve_src_dirs(rebar_app_info:opts(AppInfo)),
            %% link to src_dirs to be adjacent to ebin is needed for R15 use of cover/xref
            %% priv/ and include/ are symlinked unconditionally to allow hooks
            %% to write to them _after_ compilation has taken place when the
            %% initial directory did not, and still work
            [symlink_or_copy(OldAppDir, AppDir, Dir) || Dir <- ["priv", "include"]],
            [symlink_or_copy_existing(OldAppDir, AppDir, Dir) || Dir <- SrcDirs],
            %% copy all extra_src_dirs as they build into themselves and linking means they
            %% are shared across profiles
            [copy(OldAppDir, AppDir, Dir) || Dir <- ExtraDirs];
        false ->
            ok
    end.

symlink_or_copy(OldAppDir, AppDir, Dir) ->
    Source = filename:join([OldAppDir, Dir]),
    Target = filename:join([AppDir, Dir]),
    rebar_file_utils:symlink_or_copy(Source, Target).

symlink_or_copy_existing(OldAppDir, AppDir, Dir) ->
    Source = filename:join([OldAppDir, Dir]),
    Target = filename:join([AppDir, Dir]),
    case ec_file:is_dir(Source) of
        true -> rebar_file_utils:symlink_or_copy(Source, Target);
        false -> ok
    end.

copy(OldAppDir, AppDir, Dir) ->
    Source = filename:join([OldAppDir, Dir]),
    Target = filename:join([AppDir, Dir]),
    case ec_file:is_dir(Source) of
        true  -> copy(Source, Target);
        false -> ok
    end.

%% TODO: use ec_file:copy/2 to do this, it preserves timestamps and
%% may prevent recompilation of files in extra dirs
copy(Source, Source) ->
    %% allow users to specify a directory in _build as a directory
    %% containing additional source/tests
    ok;
copy(Source, Target) ->
    %% important to do this so no files are copied onto themselves
    %% which truncates them to zero length on some platforms
    ok = delete_if_symlink(Target),
    ok = filelib:ensure_dir(filename:join([Target, "dummy.beam"])),
    {ok, Files} = rebar_utils:list_dir(Source),
    case [filename:join([Source, F]) || F <- Files] of
        []    -> ok;
        Paths -> rebar_file_utils:cp_r(Paths, Target)
    end.

delete_if_symlink(Path) ->
    case ec_file:is_symlink(Path) of
        true  -> file:delete(Path);
        false -> ok
    end.

resolve_src_dirs(Opts) ->
    SrcDirs = rebar_dir:src_dirs(Opts, ["src"]),
    ExtraDirs = rebar_dir:extra_src_dirs(Opts, []),
    normalize_src_dirs(SrcDirs, ExtraDirs).

%% remove duplicates and make sure no directories that exist
%% in src_dirs also exist in extra_src_dirs
normalize_src_dirs(SrcDirs, ExtraDirs) ->
    S = lists:usort(SrcDirs),
    E = lists:subtract(lists:usort(ExtraDirs), S),
    ok = warn_on_problematic_directories(S ++ E),
    {S, E}.

%% warn when directories called `eunit' and `ct' are added to compile dirs
warn_on_problematic_directories(AllDirs) ->
    F = fun(Dir) ->
        case is_a_problem(Dir) of
            true  -> ?WARN("Possible name clash with directory ~p.", [Dir]);
            false -> ok
        end
    end,
    lists:foreach(F, AllDirs).

is_a_problem("eunit") -> true;
is_a_problem("common_test") -> true;
is_a_problem(_) -> false.
