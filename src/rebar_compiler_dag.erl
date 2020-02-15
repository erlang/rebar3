%%% Module handling the directed graph required for the analysis
%%% of all top-level applications by the various compiler plugins.
-module(rebar_compiler_dag).
-export([init/4, prune/4, update/5, maybe_store/5, terminate/1]).
-export([populate_sources/5, populate_deps/3, propagate_stamps/1,
         compile_order/2]).

-include("rebar.hrl").

-define(DAG_VSN, 3).
-define(DAG_ROOT, "source").
-define(DAG_EXT, ".dag").

-type dag_v() :: {digraph:vertex(), term()} | 'false'.
-type dag_e() :: {digraph:vertex(), digraph:vertex()}.
-type critical_meta() :: term(). % if this changes, the DAG is invalid
-type dag_rec() :: {list(dag_v()), list(dag_e()), critical_meta()}.
-type dag() :: digraph:graph().
-record(dag, {vsn = ?DAG_VSN :: pos_integer(),
              info = {[], [], []} :: dag_rec()}).

%% You should initialize one DAG per compiler module.
%% `CritMeta' is any contextual information that, if it is found to change,
%% must invalidate the DAG loaded from disk.
-spec init(file:filename_all(), atom(), string() | undefined, critical_meta()) -> dag().
init(Dir, Compiler, Label, CritMeta) ->
    G = digraph:new([acyclic]),
    File = dag_file(Dir, Compiler, Label),
    try
        restore_dag(G, File, CritMeta)
    catch
        _:_ ->
            %% Don't mark as dirty yet to avoid creating compiler DAG files for
            %% compilers that are actually never used.
            ?WARN("Failed to restore ~ts file. Discarding it.~n", [File]),
            file:delete(File)
    end,
    G.

-spec prune(dag(), file:filename_all(), file:filename_all(), [file:filename_all()]) -> ok.
prune(G, SrcDirs, EbinDir, Erls) ->
    %% A source file may have been renamed or deleted. Remove it from the graph
    %% and remove any beam file for that source if it exists.
    Vertices = digraph:vertices(G),
    SrcParts = [filename:split(SrcDir) || SrcDir <- SrcDirs],
    [maybe_rm_beam_and_edge(G, EbinDir, File)
     || File <- lists:sort(Vertices) -- lists:sort(Erls),
        filename:extension(File) =:= ".erl",
        lists:any(fun(Src) -> lists:prefix(Src, filename:split(File)) end,
                  SrcParts)],
    ok.

%% @doc this function scans all the source files found and looks into
%% all the `InDirs' for deps (other erl or .hrl files) that are related
%% to them (by calling `CompileMod:dependencies()' on them).
%%
%% The trick here is that change detection, done with last_modified stamps,
%% takes place at the same time as the graph propagation (finding deps)
%% themselves. As such, this is a confusing mutually recursive depth-first
%% search function that relies on side-effects and precise order-of-traversal
%% to propagate file changes.
%%
%% To be replaced by a more declarative EPP-based flow.
-spec update(dag(), module(), [file:filename_all()], [file:filename_all()],
             term()) -> ok.
update(_, _, _, [], _) ->
    ok;
update(G, Compiler, InDirs, [Source|Erls], DepOpts) ->
    case digraph:vertex(G, Source) of
        {_, LastUpdated} ->
            case filelib:last_modified(Source) of
                0 ->
                    %% The file doesn't exist anymore,
                    %% erase it from the graph.
                    %% All the edges will be erased automatically.
                    digraph:del_vertex(G, Source),
                    mark_dirty(G),
                    update(G, Compiler, InDirs, Erls, DepOpts);
                LastModified when LastUpdated < LastModified ->
                    add_to_dag(G, Compiler, InDirs, Source, LastModified,
                               filename:dirname(Source), DepOpts),
                    update(G, Compiler, InDirs, Erls, DepOpts);
                _ ->
                    AltErls = digraph:out_neighbours(G, Source),
                    %% Deps must be explored before the module itself
                    update(G, Compiler, InDirs, AltErls, DepOpts),
                    Modified = is_dirty(G),
                    MaxModified = update_max_modified_deps(G, Source),
                    case Modified orelse MaxModified > LastUpdated of
                        true -> mark_dirty(G);
                        false -> ok
                    end,
                    update(G, Compiler, InDirs, Erls, DepOpts)
            end;
        false ->
            add_to_dag(G, Compiler, InDirs, Source, filelib:last_modified(Source),
                       filename:dirname(Source), DepOpts),
            update(G, Compiler, InDirs, Erls, DepOpts)
    end.

populate_sources(_G, _Compiler, _InDirs, [], _DepOpts) ->
    ok;
populate_sources(G, Compiler, InDirs, [Source|Erls], DepOpts) ->
    case digraph:vertex(G, Source) of
        {_, LastUpdated} ->
            case filelib:last_modified(Source) of
                0 ->
                    %% The File doesn't exist anymore, delete
                    %% from the graph.
                    digraph:del_vertex(G, Source),
                    mark_dirty(G),
                    populate_sources(G, Compiler, InDirs, Erls, DepOpts);
                LastModified when LastUpdated < LastModified ->
                    digraph:add_vertex(G, Source, LastModified),
                    prepopulate_deps(G, Compiler, InDirs, Source, DepOpts),
                    mark_dirty(G);
                _ -> % unchanged
                    ok
            end;
        false ->
            LastModified = filelib:last_modified(Source),
            digraph:add_vertex(G, Source, LastModified),
            prepopulate_deps(G, Compiler, InDirs, Source, DepOpts),
            mark_dirty(G)
    end,
    populate_sources(G, Compiler, InDirs, Erls, DepOpts).

prepopulate_deps(G, Compiler, InDirs, Source, DepOpts) ->
    SourceDir = filename:dirname(Source),
    AbsIncls = case erlang:function_exported(Compiler, dependencies, 4) of
        false ->
            Compiler:dependencies(Source, SourceDir, InDirs);
        true ->
            Compiler:dependencies(Source, SourceDir, InDirs, DepOpts)
    end,
    %% the file hasn't been visited yet; set it to existing, but with
    %% a last modified value that's null so it gets updated to something new.
    [digraph:add_vertex(G, Src, 0) || Src <- AbsIncls,
                                      digraph:vertex(G, Src) =:= false],
    [digraph:add_edge(G, Source, Incl) || Incl <- AbsIncls],
    ok.

populate_deps(G, SourceExt, ArtifactExts) ->
    %% deps are files that are part of the digraph, but couldn't be scanned
    %% because they are neither source files (`SourceExt') nor mappings
    %% towards build artifacts (`ArtifactExts'); they will therefore never
    %% be handled otherwise and need to be re-scanned for accuracy, even
    %% if they are not being analyzed (we assume `Compiler:deps' did that
    %% in depth already, and improvements should be driven at that level)
    IgnoredExts = [SourceExt | ArtifactExts],
    Vertices = digraph:vertices(G),
    [refresh_dep(G, File)
     || File <- Vertices,
        Ext <- [filename:extension(File)],
        not lists:member(Ext, IgnoredExts)],
    ok.

%% Take the timestamps/diff changes and propagate them from a dep to the parent;
%% given:
%%   A 0 -> B 1 -> C 3 -> D 2
%% then we expect to get back:
%%   A 3 -> B 3 -> C 3 -> D 2
%% This is going to be safe for the current run of regeneration, but also for the
%% next one; unless any file in the chain has changed, the stamp won't move up
%% and there won't be a reason to recompile.
%% The obvious caveat to this one is that a file changing by restoring an old version
%% won't be picked up, but this weakness already existed in terms of timestamps.
propagate_stamps(G) ->
    case is_dirty(G) of
        false ->
            %% no change, no propagation to make
            ok;
        true ->
            %% we can use a topsort, start at the end of it (files with no deps)
            %% and update them all in order. By doing this, each file only needs to check
            %% for one level of out-neighbours to set itself to the right appropriate time.
            DepSort = lists:reverse(digraph_utils:topsort(G)),
            propagate_stamps(G, DepSort)
    end.


propagate_stamps(_G, []) ->
    ok;
propagate_stamps(G, [File|Files]) ->
    Stamps = [element(2, digraph:vertex(G, F))
              || F <- digraph:out_neighbours(G, File)],
    case Stamps of
        [] ->
            ok;
        _ ->
            Max = lists:max(Stamps),
            case digraph:vertex(G, File) of
                {_, Smaller} when Smaller < Max ->
                    digraph:add_vertex(G, File, Max);
                _ ->
                    ok
            end
    end,
    propagate_stamps(G, Files).


compile_order(G, AppDefs) ->
    %% Return the reverse sorting order to get dep-free apps first.
    %% -- we would usually not need to consider the non-source files for the order to
    %% be complete, but using them doesn't hurt.
    Edges = [{V1,V2} || E <- digraph:edges(G),
                        {_,V1,V2,_} <- [digraph:edge(G, E)]],
    AppPaths = prepare_app_paths(AppDefs),
    compile_order(Edges, AppPaths, #{}).

compile_order([], _AppPaths, AppDeps) ->
    %% use a digraph so we don't reimplement topsort by hand.
    G = digraph:new([acyclic]), % ignore cycles and hope it works
    Tups = maps:keys(AppDeps),
    {Va,Vb} = lists:unzip(Tups),
    [digraph:add_vertex(G, V) || V <- Va],
    [digraph:add_vertex(G, V) || V <- Vb],
    [digraph:add_edge(G, V1, V2) || {V1, V2} <- Tups],
    Res = lists:reverse(digraph_utils:topsort(G)),
    digraph:delete(G),
    Res;
compile_order([{P1,P2}|T], AppPaths, AppDeps) ->
    %% Assume most dependencies are between files of the same app
    %% so ask to see if it's the same before doing a deeper check:
    {P1App, P1Path} = find_app(P1, AppPaths),
    {P2App, _} = find_cached_app(P2, {P1App, P1Path}, AppPaths),
    case {P1App, P2App} of
        {A, A} ->
            compile_order(T, AppPaths, AppDeps);
        {V1, V2} ->
            compile_order(T, AppPaths, AppDeps#{{V1, V2} => true})
    end.

prepare_app_paths(AppPaths) ->
    lists:sort([{filename:split(Path), Name} || {Name, Path} <- AppPaths]).

find_app(Path, AppPaths) ->
    find_app_(filename:split(Path), AppPaths).

find_cached_app(Path, {Name, AppPath}, AppPaths) ->
    Split = filename:split(Path),
    case find_app_(Split, [{AppPath, Name}]) of
        not_found -> find_app_(Split, AppPaths);
        LastEntry -> LastEntry
    end.

find_app_(_Path, []) ->
    not_found;
find_app_(Path, [{AppPath, AppName}|Rest]) ->
    case lists:prefix(AppPath, Path) of
        true ->
            {AppName, AppPath};
        false when AppPath > Path ->
            not_found;
        false ->
            find_app_(Path, Rest)
    end.


refresh_dep(G, File) ->
    {_, LastUpdated} = digraph:vertex(G, File),
    case filelib:last_modified(File) of
        0 ->
            %% Gone! Erase from the graph
            digraph:del_vertex(G, File),
            mark_dirty(G);
        LastModified when LastUpdated < LastModified ->
            digraph:add_vertex(G, File, LastModified),
            mark_dirty(G);
        _ ->
            % unchanged
            ok
    end.

maybe_store(G, Dir, Compiler, Label, CritMeta) ->
    case is_dirty(G) of
        true ->
            clear_dirty(G),
            File = dag_file(Dir, Compiler, Label),
            store_dag(G, File, CritMeta);
        false ->
            ok
    end.

terminate(G) ->
    true = digraph:delete(G).

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%
%% @private generate the name for the DAG based on the compiler module and
%% a custom label, both of which are used to prevent various compiler runs
%% from clobbering each other. The label `undefined' is kept for a default
%% run of the compiler, to keep in line with previous versions of the file.
dag_file(Dir, CompilerMod, undefined) ->
    filename:join([rebar_dir:local_cache_dir(Dir), CompilerMod,
                   ?DAG_ROOT ++ ?DAG_EXT]);
dag_file(Dir, CompilerMod, Label) ->
    filename:join([rebar_dir:local_cache_dir(Dir), CompilerMod,
                   ?DAG_ROOT ++ "_" ++ Label ++ ?DAG_EXT]).

restore_dag(G, File, CritMeta) ->
    case file:read_file(File) of
        {ok, Data} ->
            %% The CritMeta value is checked and if it doesn't match, we fail
            %% the whole restore operation.
            #dag{vsn=?DAG_VSN, info={Vs, Es, CritMeta}} = binary_to_term(Data),
            [digraph:add_vertex(G, V, LastUpdated) || {V, LastUpdated} <- Vs],
            [digraph:add_edge(G, V1, V2) || {_, V1, V2, _} <- Es],
            ok;
        {error, _Err} ->
            ok
    end.

store_dag(G, File, CritMeta) ->
    ok = filelib:ensure_dir(File),
    Vs = lists:map(fun(V) -> digraph:vertex(G, V) end, digraph:vertices(G)),
    Es = lists:map(fun(E) -> digraph:edge(G, E) end, digraph:edges(G)),
    Data = term_to_binary(#dag{info={Vs, Es, CritMeta}}, [{compressed, 2}]),
    file:write_file(File, Data).

%% Drop a file from the digraph if it doesn't exist, and if so,
%% delete its related build artifact
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
            mark_dirty(G),
            true
    end.

%% @private Return what should be the base name of an erl file, relocated to the
%% target directory. For example:
%% target_base("ebin/", "src/my_module.erl") -> "ebin/my_module"
target_base(OutDir, Source) ->
    filename:join(OutDir, filename:basename(Source, ".erl")).

%% @private a file has been found to change or wasn't part of the DAG before,
%% and must be added, along with all its dependencies.
add_to_dag(G, Compiler, InDirs, Source, LastModified, SourceDir, DepOpts) ->
    AbsIncls = case erlang:function_exported(Compiler, dependencies, 4) of
        false ->
            Compiler:dependencies(Source, SourceDir, InDirs);
        true ->
            Compiler:dependencies(Source, SourceDir, InDirs, DepOpts)
    end,
    digraph:add_vertex(G, Source, LastModified),
    digraph:del_edges(G, digraph:out_edges(G, Source)),
    %% Deps must be explored before the module itself
    [begin
         update(G, Compiler, InDirs, [Incl], DepOpts),
         digraph:add_edge(G, Source, Incl)
     end || Incl <- AbsIncls],
    mark_dirty(G),
    AbsIncls.

%% @private change status propagation: if the dependencies of a file have
%% been updated, mark the last_modified time for that file to be equivalent
%% to its most-recently-changed dependency; that way, nested header file
%% change stamps are propagated to the final module.
%% This is required because at some point the module is compared to its
%% associated .beam file's last-generation stamp to know if it requires
%% rebuilding.
%% The responsibility for this is however diffuse across various modules.
update_max_modified_deps(G, Source) ->
    MaxModified = lists:foldl(
        fun(File, Acc) ->
            case digraph:vertex(G, File) of
                {_, MaxModified} when MaxModified > Acc -> MaxModified;
                _ -> Acc
            end
        end,
        0,
        [Source | digraph:out_neighbours(G, Source)]
    ),
    digraph:add_vertex(G, Source, MaxModified),
    MaxModified.

%% Mark the digraph as having been modified, which is required to
%% save its updated form on disk after the compiling run.
%% This uses a magic vertex to carry the dirty state. This is less
%% than ideal because listing vertices may expect filenames and
%% instead there's going to be one trick atom through it.
mark_dirty(G) ->
    digraph:add_vertex(G, '$r3_dirty_bit', true),
    ok.

%% Check whether the digraph has been modified and is considered dirty.
is_dirty(G) ->
    case digraph:vertex(G, '$r3_dirty_bit') of
        {_, Bool} -> Bool;
        false -> false
    end.

%% Remove the dirty status. Because the saving of a digraph on disk saves all
%% vertices, clear the flag before serializing it.
clear_dirty(G) ->
    digraph:del_vertex(G, '$r3_dirty_bit').
