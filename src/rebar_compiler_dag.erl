%%% Module handling the directed graph required for the analysis
%%% of all top-level applications by the various compiler plugins.
-module(rebar_compiler_dag).
-export([init/4, prune/4, update/4, maybe_store/5, terminate/1]).

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
-spec update(dag(), module(), [file:filename_all()], [file:filename_all()]) -> ok.
update(_, _, _, []) ->
    ok;
update(G, Compiler, InDirs, [Source|Erls]) ->
    case digraph:vertex(G, Source) of
        {_, LastUpdated} ->
            case filelib:last_modified(Source) of
                0 ->
                    %% The file doesn't exist anymore,
                    %% erase it from the graph.
                    %% All the edges will be erased automatically.
                    digraph:del_vertex(G, Source),
                    mark_dirty(G),
                    update(G, Compiler, InDirs, Erls);
                LastModified when LastUpdated < LastModified ->
                    add_to_dag(G, Compiler, InDirs, Source, LastModified, filename:dirname(Source)),
                    update(G, Compiler, InDirs, Erls);
                _ ->
                    AltErls = digraph:out_neighbours(G, Source),
                    %% Deps must be explored before the module itself
                    update(G, Compiler, InDirs, AltErls),
                    Modified = is_dirty(G),
                    MaxModified = update_max_modified_deps(G, Source),
                    case Modified orelse MaxModified > LastUpdated of
                        true -> mark_dirty(G);
                        false -> ok
                    end,
                    update(G, Compiler, InDirs, Erls)
            end;
        false ->
            add_to_dag(G, Compiler, InDirs, Source, filelib:last_modified(Source), filename:dirname(Source)),
            update(G, Compiler, InDirs, Erls)
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
add_to_dag(G, Compiler, InDirs, Source, LastModified, SourceDir) ->
    AbsIncls = Compiler:dependencies(Source, SourceDir, InDirs),
    digraph:add_vertex(G, Source, LastModified),
    digraph:del_edges(G, digraph:out_edges(G, Source)),
    %% Deps must be explored before the module itself
    [begin
         update(G, Compiler, InDirs, [Incl]),
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
