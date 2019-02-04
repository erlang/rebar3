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
    {{[file:filename()], term()}, {[file:filename()], term()}}.
-callback dependencies(file:filename(), file:dirname(), [file:dirname()]) -> [file:filename()].
-callback compile(file:filename(), out_mappings(), rebar_dict(), list()) ->
    ok | {ok, [string()]} | {ok, [string()], [string()]}.
-callback clean([file:filename()], rebar_app_info:t()) -> _.

-define(DAG_VSN, 2).
-define(DAG_ROOT, "source").
-define(DAG_EXT, ".dag").
-type dag_v() :: {digraph:vertex(), term()} | 'false'.
-type dag_e() :: {digraph:vertex(), digraph:vertex()}.
-type dag() :: {list(dag_v()), list(dag_e()), list(string())}.
-record(dag, {vsn = ?DAG_VSN :: pos_integer(),
              info = {[], [], []} :: dag()}).

-define(RE_PREFIX, "^(?!\\._)").

compile_all(Compilers, AppInfo) ->
    EbinDir = rebar_utils:to_list(rebar_app_info:ebin_dir(AppInfo)),
    %% Make sure that outdir is on the path
    ok = rebar_file_utils:ensure_dir(EbinDir),
    true = code:add_patha(filename:absname(EbinDir)),

    %% necessary for erlang:function_exported/3 to work as expected
    %% called here for clarity as it's required by both opts_changed/2
    %% and erl_compiler_opts_set/0 in needed_files
    _ = code:ensure_loaded(compile),

    lists:foreach(fun(CompilerMod) ->
        run(CompilerMod, AppInfo, undefined),
        run_on_extra_src_dirs(CompilerMod, AppInfo,
                              fun(Mod, App) -> run(Mod, App, "extra") end)
    end, Compilers),
    ok.

run(CompilerMod, AppInfo, Label) ->
    #{src_dirs := SrcDirs,
      include_dirs := InclDirs,
      src_ext := SrcExt,
      out_mappings := Mappings} = CompilerMod:context(AppInfo),

    BaseDir = rebar_utils:to_list(rebar_app_info:dir(AppInfo)),
    EbinDir = rebar_utils:to_list(rebar_app_info:ebin_dir(AppInfo)),

    BaseOpts = rebar_app_info:opts(AppInfo),
    AbsInclDirs = [filename:join(BaseDir, InclDir) || InclDir <- InclDirs],
    FoundFiles = find_source_files(BaseDir, SrcExt, SrcDirs, BaseOpts),

    OutDir = rebar_app_info:out_dir(AppInfo),
    AbsSrcDirs = [filename:join(BaseDir, SrcDir) || SrcDir <- SrcDirs],
    G = init_dag(CompilerMod, AbsInclDirs, AbsSrcDirs, FoundFiles, OutDir, EbinDir, Label),
    {{FirstFiles, FirstFileOpts}, {RestFiles, Opts}} = CompilerMod:needed_files(G, FoundFiles,
                                                                                Mappings, AppInfo),
    true = digraph:delete(G),

    compile_each(FirstFiles, FirstFileOpts, BaseOpts, Mappings, CompilerMod),
    compile_each(RestFiles, Opts, BaseOpts, Mappings, CompilerMod).

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

%% @doc remove compiled artifacts from an AppDir.
-spec clean([module()], rebar_app_info:t()) -> 'ok'.
clean(Compilers, AppInfo) ->
    lists:foreach(fun(CompilerMod) ->
        clean_(CompilerMod, AppInfo, undefined),
        run_on_extra_src_dirs(CompilerMod, AppInfo,
                              fun(Mod, App) -> clean_(Mod, App, "extra") end)
    end, Compilers).

clean_(CompilerMod, AppInfo, Label) ->
    #{src_dirs := SrcDirs,
      src_ext := SrcExt} = CompilerMod:context(AppInfo),
    BaseDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    EbinDir = rebar_app_info:ebin_dir(AppInfo),

    FoundFiles = find_source_files(BaseDir, SrcExt, SrcDirs, Opts),
    CompilerMod:clean(FoundFiles, AppInfo),
    rebar_file_utils:rm_rf(dag_file(CompilerMod, EbinDir, Label)).

-spec needs_compile(filename:all(), extension(), [{extension(), file:dirname()}]) -> boolean().
needs_compile(Source, OutExt, Mappings) ->
    Ext = filename:extension(Source),
    BaseName = filename:basename(Source, Ext),
    {_, OutDir} = lists:keyfind(OutExt, 1, Mappings),
    Target = filename:join(OutDir, BaseName++OutExt),
    filelib:last_modified(Source) > filelib:last_modified(Target).

run_on_extra_src_dirs(CompilerMod, AppInfo, Fun) ->
    ExtraDirs = rebar_dir:extra_src_dirs(rebar_app_info:opts(AppInfo), []),
    run_on_extra_src_dirs(ExtraDirs, CompilerMod, AppInfo, Fun).

run_on_extra_src_dirs([], _CompilerMod, _AppInfo, _Fun) ->
    ok;
run_on_extra_src_dirs([Dir | Rest], CompilerMod, AppInfo, Fun) ->
    case filelib:is_dir(filename:join(rebar_app_info:dir(AppInfo), Dir)) of
        true ->
            OldSrcDirs = rebar_app_info:get(AppInfo, src_dirs, ["src"]),
            AppDir = rebar_app_info:dir(AppInfo),
            EbinDir = filename:join(rebar_app_info:out_dir(AppInfo), Dir),
            AppInfo1 = rebar_app_info:ebin_dir(AppInfo, EbinDir),
            AppInfo2 = rebar_app_info:set(AppInfo1, src_dirs, [Dir]),
            AppInfo3 = rebar_app_info:set(AppInfo2, extra_src_dirs, OldSrcDirs),
            AppInfo4 = add_to_includes( % give access to .hrl in app's src/
                AppInfo3,
                [filename:join([AppDir, D]) || D <- OldSrcDirs]
            ),
            Fun(CompilerMod, AppInfo4);
        _ ->
            ok
    end,
    run_on_extra_src_dirs(Rest, CompilerMod, AppInfo, Fun).

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

%% private functions

find_source_files(BaseDir, SrcExt, SrcDirs, Opts) ->
    SourceExtRe = "^(?!\\._).*\\" ++ SrcExt ++ [$$],
    lists:flatmap(fun(SrcDir) ->
                      Recursive = rebar_dir:recursive(Opts, SrcDir),
                      rebar_utils:find_files_in_dirs([filename:join(BaseDir, SrcDir)], SourceExtRe, Recursive)
                  end, SrcDirs).

%% @private generate the name for the DAG based on the compiler module and
%% a custom label, both of which are used to prevent various compiler runs
%% from clobbering each other. The label `undefined' is kept for a default
%% run of the compiler, to keep in line with previous versions of the file.
dag_file(CompilerMod, Dir, undefined) ->
    filename:join([rebar_dir:local_cache_dir(Dir), CompilerMod,
                   ?DAG_ROOT ++ ?DAG_EXT]);
dag_file(CompilerMod, Dir, Label) ->
    filename:join([rebar_dir:local_cache_dir(Dir), CompilerMod,
                   ?DAG_ROOT ++ "_" ++ Label ++ ?DAG_EXT]).

%% private graph functions

%% Get dependency graph of given Erls files and their dependencies (header files,
%% parse transforms, behaviours etc.) located in their directories or given
%% InclDirs. Note that last modification times stored in vertices already respect
%% dependencies induced by given graph G.
init_dag(Compiler, InclDirs, SrcDirs, Erls, Dir, EbinDir, Label) ->
    G = digraph:new([acyclic]),
    try restore_dag(Compiler, G, InclDirs, Dir, Label)
    catch
        _:_ ->
            ?WARN("Failed to restore ~ts file. Discarding it.~n", [dag_file(Compiler, Dir, Label)]),
            file:delete(dag_file(Compiler, Dir, Label))
    end,
    Dirs = lists:usort(InclDirs ++ SrcDirs),
    %% A source file may have been renamed or deleted. Remove it from the graph
    %% and remove any beam file for that source if it exists.
    Modified = maybe_rm_beams_and_edges(G, EbinDir, Erls),
    Modified1 = lists:foldl(update_dag_fun(G, Compiler, Dirs), Modified, Erls),
    if Modified1 -> store_dag(Compiler, G, InclDirs, Dir, Label);
       not Modified1 -> ok
    end,
    G.

maybe_rm_beams_and_edges(G, Dir, Files) ->
    Vertices = digraph:vertices(G),
    case lists:filter(fun(File) ->
                              case filename:extension(File) =:= ".erl" of
                                  true ->
                                      maybe_rm_beam_and_edge(G, Dir, File);
                                  false ->
                                      false
                              end
                      end, lists:sort(Vertices) -- lists:sort(Files)) of
        [] ->
            false;
        _ ->
            true
    end.

maybe_rm_beam_and_edge(G, OutDir, Source) ->
    %% This is NOT a double check it is the only check that the source file is actually gone
    case filelib:is_regular(Source) of
        true ->
            %% Actually exists, don't delete
            false;
        false ->
            Target = target_base(OutDir, Source) ++ ".beam",
            ?DEBUG("Source ~ts is gone, deleting previous beam file if it exists ~ts", [Source, Target]),
            file:delete(Target),
            digraph:del_vertex(G, Source),
            true
    end.


target_base(OutDir, Source) ->
    filename:join(OutDir, filename:basename(Source, ".erl")).

restore_dag(Compiler, G, InclDirs, Dir, Label) ->
    case file:read_file(dag_file(Compiler, Dir, Label)) of
        {ok, Data} ->
            % Since externally passed InclDirs can influence dependency graph (see
            % modify_dag), we have to check here that they didn't change.
            #dag{vsn=?DAG_VSN, info={Vs, Es, InclDirs}} =
                binary_to_term(Data),
            lists:foreach(
              fun({V, LastUpdated}) ->
                      digraph:add_vertex(G, V, LastUpdated)
              end, Vs),
            lists:foreach(
              fun({_, V1, V2, _}) ->
                      digraph:add_edge(G, V1, V2)
              end, Es);
        {error, _} ->
            ok
    end.

store_dag(Compiler, G, InclDirs, Dir, Label) ->
    Vs = lists:map(fun(V) -> digraph:vertex(G, V) end, digraph:vertices(G)),
    Es = lists:map(fun(E) -> digraph:edge(G, E) end, digraph:edges(G)),
    File = dag_file(Compiler, Dir, Label),
    ok = filelib:ensure_dir(File),
    Data = term_to_binary(#dag{info={Vs, Es, InclDirs}}, [{compressed, 2}]),
    file:write_file(File, Data).

update_dag(G, Compiler, Dirs, Source) ->
    case digraph:vertex(G, Source) of
        {_, LastUpdated} ->
            case filelib:last_modified(Source) of
                0 ->
                    %% The file doesn't exist anymore,
                    %% erase it from the graph.
                    %% All the edges will be erased automatically.
                    digraph:del_vertex(G, Source),
                    modified;
                LastModified when LastUpdated < LastModified ->
                    modify_dag(G, Compiler, Source, LastModified, filename:dirname(Source), Dirs);
                _ ->
                    Modified = lists:foldl(
                        update_dag_fun(G, Compiler, Dirs),
                        false, digraph:out_neighbours(G, Source)),
                    MaxModified = update_max_modified_deps(G, Source),
                    case Modified orelse MaxModified > LastUpdated of
                        true -> modified;
                        false -> unmodified
                    end
            end;
        false ->
            modify_dag(G, Compiler, Source, filelib:last_modified(Source), filename:dirname(Source), Dirs)
    end.

modify_dag(G, Compiler, Source, LastModified, SourceDir, Dirs) ->
    AbsIncls = Compiler:dependencies(Source, SourceDir, Dirs),
    digraph:add_vertex(G, Source, LastModified),
    digraph:del_edges(G, digraph:out_edges(G, Source)),
    lists:foreach(
      fun(Incl) ->
              update_dag(G, Compiler, Dirs, Incl),
              digraph:add_edge(G, Source, Incl)
      end, AbsIncls),
    modified.

update_dag_fun(G, Compiler, Dirs) ->
    fun(Erl, Modified) ->
        case update_dag(G, Compiler, Dirs, Erl) of
            modified -> true;
            unmodified -> Modified
        end
    end.

update_max_modified_deps(G, Source) ->
    MaxModified =
        lists:foldl(fun(File, Acc) ->
                            case digraph:vertex(G, File) of
                                {_, MaxModified} when MaxModified > Acc ->
                                    MaxModified;
                                _ ->
                                    Acc
                            end
                    end, 0, [Source | digraph:out_neighbours(G, Source)]),
    digraph:add_vertex(G, Source, MaxModified),
    MaxModified.

add_to_includes(AppInfo, Dirs) ->
    Opts = rebar_app_info:opts(AppInfo),
    List = rebar_opts:get(Opts, erl_opts, []),
    NewErlOpts = [{i, Dir} || Dir <- Dirs] ++ List,
    NewOpts = rebar_opts:set(Opts, erl_opts, NewErlOpts),
    rebar_app_info:opts(AppInfo, NewOpts).
