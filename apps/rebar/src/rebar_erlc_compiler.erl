%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_erlc_compiler).

-export([compile/1, compile/2, compile/3,
         compile_dir/3, compile_dir/4,
         compile_dirs/5,
         clean/1]).

-include("rebar.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

-define(ERLCINFO_VSN, 2).
-define(ERLCINFO_FILE, "erlcinfo").
-type erlc_info_v() :: {digraph:vertex(), term()} | 'false'.
-type erlc_info_e() :: {digraph:vertex(), digraph:vertex()}.
-type erlc_info() :: {list(erlc_info_v()), list(erlc_info_e()), list(string())}.
-record(erlcinfo, {
    vsn = ?ERLCINFO_VSN :: pos_integer(),
    info = {[], [], []} :: erlc_info()
}).

-type compile_opts() :: [compile_opt()].
-type compile_opt() :: {recursive, boolean()}.

-define(DEFAULT_OUTDIR, "ebin").
-define(RE_PREFIX, "^(?!\\._)").

%% ===================================================================
%% Public API
%% ===================================================================

%% Supported configuration variables:
%%
%% * erl_opts - Erlang list of options passed to compile:file/2
%%              It is also possible to specify platform specific
%%              options by specifying a pair or a triplet where the
%%              first string is a regex that is checked against the
%%              string
%%
%%                OtpRelease ++ "-" ++ SysArch ++ "-" ++ Words.
%%
%%              where
%%
%%                OtpRelease = erlang:system_info(otp_release).
%%                SysArch = erlang:system_info(system_architecture).
%%                Words = integer_to_list(8 *
%%                            erlang:system_info({wordsize, external})).
%%
%%              E.g. to define HAVE_SENDFILE only on systems with
%%              sendfile(), to define BACKLOG on Linux/FreeBSD as 128,
%%              and to define 'old_inets' for R13 OTP release do:
%%
%%              {erl_opts, [{platform_define,
%%                           "(linux|solaris|freebsd|darwin)",
%%                           'HAVE_SENDFILE'},
%%                          {platform_define, "(linux|freebsd)",
%%                           'BACKLOG', 128},
%%                          {platform_define, "R13",
%%                           'old_inets'}]}.
%%

%% @equiv compile(AppInfo, [])
-spec compile(rebar_app_info:t()) -> ok.
compile(AppInfo) when element(1, AppInfo) == app_info_t ->
    compile(AppInfo, []).

%% @doc compile an individual application.
-spec compile(rebar_app_info:t(), compile_opts()) -> ok.
compile(AppInfo, CompileOpts) when element(1, AppInfo) == app_info_t ->
    warn_deprecated(),
    Dir = rebar_utils:to_list(rebar_app_info:out_dir(AppInfo)),
    RebarOpts = rebar_app_info:opts(AppInfo),

    SrcOpts = [check_last_mod,
               {recursive, dir_recursive(RebarOpts, "src", CompileOpts)}],
    MibsOpts = [check_last_mod,
                {recursive, dir_recursive(RebarOpts, "mibs", CompileOpts)}],

    rebar_base_compiler:run(RebarOpts,
                            check_files([filename:join(Dir, File)
                                         || File <- rebar_opts:get(RebarOpts, xrl_first_files, [])]),
                            filename:join(Dir, "src"), ".xrl", filename:join(Dir, "src"), ".erl",
                            fun compile_xrl/3, SrcOpts),
    rebar_base_compiler:run(RebarOpts,
                            check_files([filename:join(Dir, File)
                                         || File <- rebar_opts:get(RebarOpts, yrl_first_files, [])]),
                            filename:join(Dir, "src"), ".yrl", filename:join(Dir, "src"), ".erl",
                            fun compile_yrl/3, SrcOpts),
    rebar_base_compiler:run(RebarOpts,
                            check_files([filename:join(Dir, File)
                                         || File <- rebar_opts:get(RebarOpts, mib_first_files, [])]),
                            filename:join(Dir, "mibs"), ".mib", filename:join([Dir, "priv", "mibs"]), ".bin",
                            compile_mib(AppInfo), MibsOpts),

    SrcDirs = lists:map(fun(SrcDir) -> filename:join(Dir, SrcDir) end,
                        rebar_dir:src_dirs(RebarOpts, ["src"])),
    OutDir = filename:join(Dir, outdir(RebarOpts)),
    compile_dirs(RebarOpts, Dir, SrcDirs, OutDir, CompileOpts),

    ExtraDirs = rebar_dir:extra_src_dirs(RebarOpts),
    F = fun(D) ->
        case ec_file:is_dir(filename:join([Dir, D])) of
            true  -> compile_dirs(RebarOpts, Dir, [D], D, CompileOpts);
            false -> ok
        end
    end,
    lists:foreach(F, lists:map(fun(SrcDir) -> filename:join(Dir, SrcDir) end, ExtraDirs)).

%% @hidden
%% these are kept for backwards compatibility but they're bad functions with
%% bad interfaces you probably shouldn't use
%% State/RebarOpts have to have src_dirs set and BaseDir must be the parent
%% directory of those src_dirs

-spec compile(rebar_dict() | rebar_state:t(), file:name(), file:name()) -> ok.
compile(State, BaseDir, OutDir) when element(1, State) == state_t ->
    compile(rebar_state:opts(State), BaseDir, OutDir, [{recursive, false}]);
compile(RebarOpts, BaseDir, OutDir) ->
    compile(RebarOpts, BaseDir, OutDir, [{recursive, false}]).

%% @hidden

-spec compile(rebar_dict() | rebar_state:t(), file:name(), file:name(), compile_opts()) -> ok.
compile(State, BaseDir, OutDir, CompileOpts) when element(1, State) == state_t ->
    compile(rebar_state:opts(State), BaseDir, OutDir, CompileOpts);
compile(RebarOpts, BaseDir, OutDir, CompileOpts) ->
    warn_deprecated(),
    SrcDirs = lists:map(fun(SrcDir) -> filename:join(BaseDir, SrcDir) end,
                        rebar_dir:src_dirs(RebarOpts, ["src"])),
    compile_dirs(RebarOpts, BaseDir, SrcDirs, OutDir, CompileOpts),

    ExtraDirs = rebar_dir:extra_src_dirs(RebarOpts),
    F = fun(D) ->
        case ec_file:is_dir(filename:join([BaseDir, D])) of
            true  -> compile_dirs(RebarOpts, BaseDir, [D], D, CompileOpts);
            false -> ok
        end
    end,
    lists:foreach(F, lists:map(fun(SrcDir) -> filename:join(BaseDir, SrcDir) end, ExtraDirs)).

%% @equiv compile_dirs(Context, BaseDir, [Dir], Dir, [{recursive, false}])
-spec compile_dir(rebar_dict() | rebar_state:t(), file:name(), file:name()) -> ok.
compile_dir(State, BaseDir, Dir) when element(1, State) == state_t ->
    compile_dir(rebar_state:opts(State), BaseDir, Dir, [{recursive, false}]);
compile_dir(RebarOpts, BaseDir, Dir) ->
    compile_dir(RebarOpts, BaseDir, Dir, [{recursive, false}]).

%% @equiv compile_dirs(Context, BaseDir, [Dir], Dir, Opts)
-spec compile_dir(rebar_dict() | rebar_state:t(), file:name(), file:name(), compile_opts()) -> ok.
compile_dir(State, BaseDir, Dir, Opts) when element(1, State) == state_t ->
    compile_dirs(rebar_state:opts(State), BaseDir, [Dir], Dir, Opts);
compile_dir(RebarOpts, BaseDir, Dir, Opts) ->
    compile_dirs(RebarOpts, BaseDir, [Dir], Dir, Opts).

%% @doc compile a list of directories with the given opts.
-spec compile_dirs(rebar_dict() | rebar_state:t(),
                   file:filename(),
                   [file:filename()],
                   file:filename(),
                   compile_opts()) -> ok.
compile_dirs(State, BaseDir, Dirs, OutDir, CompileOpts) when element(1, State) == state_t ->
    compile_dirs(rebar_state:opts(State), BaseDir, Dirs, OutDir, CompileOpts);
compile_dirs(RebarOpts, BaseDir, SrcDirs, OutDir, CompileOpts) ->
    ErlOpts = rebar_opts:erl_opts(RebarOpts),
    ?DEBUG("erlopts ~p", [ErlOpts]),
    AllErlFiles = gather_src(RebarOpts, BaseDir, SrcDirs, CompileOpts),
    ?DEBUG("files to compile ~p", [AllErlFiles]),

    %% Make sure that outdir is on the path
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),
    true = code:add_patha(filename:absname(OutDir)),

    G = init_erlcinfo(include_abs_dirs(ErlOpts, BaseDir), AllErlFiles, BaseDir, OutDir),

    {ParseTransforms, Rest} = split_source_files(AllErlFiles, ErlOpts),
    NeededErlFiles = case needed_files(G, ErlOpts, RebarOpts, BaseDir, OutDir, ParseTransforms) of
        [] -> needed_files(G, ErlOpts, RebarOpts, BaseDir, OutDir, Rest);
        %% at least one parse transform in the opts needs updating, so recompile all
        _  -> AllErlFiles
    end,

    {ErlFirstFiles, ErlOptsFirst} = erl_first_files(RebarOpts, ErlOpts, BaseDir, NeededErlFiles),
    {DepErls, OtherErls} = lists:partition(
                             fun(Source) -> lists:any(
                                 fun(Edge) ->
                                     {_E, _V1, _V2, Kind} = digraph:edge(G, Edge),
                                     Kind =/= artifact
                                 end,
                                 digraph:in_edges(G, Source))
                             end,
                             [File || File <- NeededErlFiles, not lists:member(File, ErlFirstFiles)]),
    SubGraph = digraph_utils:subgraph(G, DepErls),
    DepErlsOrdered = digraph_utils:topsort(SubGraph),
    FirstErls = ErlFirstFiles ++ lists:reverse(DepErlsOrdered),
    try
        rebar_base_compiler:run(
            RebarOpts, FirstErls, OtherErls,
            fun(S, C) ->
                    ErlOpts1 = case lists:member(S, ErlFirstFiles) of
                                   true -> ErlOptsFirst;
                                   false -> ErlOpts
                               end,
                    internal_erl_compile(C, BaseDir, S, OutDir, ErlOpts1, RebarOpts)
            end)
    after
        true = digraph:delete(SubGraph),
        true = digraph:delete(G)
    end,
    ok.

%% @doc remove compiled artifacts from an AppDir.
-spec clean(rebar_app_info:t()) -> 'ok'.
clean(AppInfo) ->
    AppDir = rebar_app_info:out_dir(AppInfo),

    MibFiles = rebar_utils:find_files(filename:join([AppDir, "mibs"]), ?RE_PREFIX".*\\.mib\$"),
    MIBs = [filename:rootname(filename:basename(MIB)) || MIB <- MibFiles],
    rebar_file_utils:delete_each(
      [filename:join([AppDir, "include",MIB++".hrl"]) || MIB <- MIBs]),
    ok = rebar_file_utils:rm_rf(filename:join([AppDir, "priv/mibs/*.bin"])),

    YrlFiles = rebar_utils:find_files(filename:join([AppDir, "src"]), ?RE_PREFIX".*\\.[x|y]rl\$"),
    rebar_file_utils:delete_each(
      [rebar_utils:to_list(re:replace(F, "\\.[x|y]rl$", ".erl", [unicode]))
       || F <- YrlFiles]),

    BinDirs = ["ebin"|rebar_dir:extra_src_dirs(rebar_app_info:opts(AppInfo))],
    ok = clean_dirs(AppDir, BinDirs),

    %% Delete the build graph, if any
    rebar_file_utils:rm_rf(erlcinfo_file(AppDir)).

clean_dirs(_AppDir, []) -> ok;
clean_dirs(AppDir, [Dir|Rest]) ->
    ok = rebar_file_utils:rm_rf(filename:join([AppDir, Dir, "*.beam"])),
    %% Erlang compilation is recursive, so it's possible that we have a nested
    %% directory structure in ebin with .beam files within. As such, we want
    %% to scan whatever is left in the app's out_dir directory for sub-dirs which
    %% satisfy our criteria.
    BeamFiles = rebar_utils:find_files(filename:join([AppDir, Dir]), ?RE_PREFIX".*\\.beam\$"),
    rebar_file_utils:delete_each(BeamFiles),
    lists:foreach(fun(D) -> delete_dir(D, dirs(D)) end, dirs(filename:join([AppDir, Dir]))),
    clean_dirs(AppDir, Rest).


%% ===================================================================
%% Internal functions
%% ===================================================================

gather_src(Opts, BaseDir, Dirs, CompileOpts) ->
    gather_src(Opts, filename:split(BaseDir), Dirs, [], CompileOpts).

gather_src(_Opts, _BaseDirParts, [], Srcs, _CompileOpts) -> Srcs;
gather_src(Opts, BaseDirParts, [Dir|Rest], Srcs, CompileOpts) ->
    DirParts = filename:split(Dir),
    RelDir = case lists:prefix(BaseDirParts,DirParts) of
                 true ->
                     case lists:nthtail(length(BaseDirParts),DirParts) of
                         [] -> ".";
                         RestParts -> filename:join(RestParts)
                     end;
                 false -> Dir
             end,
    DirRecursive = dir_recursive(Opts, RelDir, CompileOpts),
    gather_src(Opts, BaseDirParts, Rest, Srcs ++ rebar_utils:find_files(Dir, ?RE_PREFIX".*\\.erl\$", DirRecursive), CompileOpts).

%% Get files which need to be compiled first, i.e. those specified in erl_first_files
%% and parse_transform options.  Also produce specific erl_opts for these first
%% files, so that yet to be compiled parse transformations are excluded from it.
erl_first_files(Opts, ErlOpts, Dir, NeededErlFiles) ->
    ErlFirstFilesConf = rebar_opts:get(Opts, erl_first_files, []),
    valid_erl_first_conf(ErlFirstFilesConf),
    NeededSrcDirs = lists:usort(lists:map(fun filename:dirname/1, NeededErlFiles)),
    %% NOTE: order of files here is important!
    ErlFirstFiles =
        [filename:join(Dir, File) || File <- ErlFirstFilesConf,
                                     lists:member(filename:join(Dir, File), NeededErlFiles)],
    {ParseTransforms, ParseTransformsErls} =
        lists:unzip(lists:flatmap(
                      fun(PT) ->
                              PTerls = [filename:join(D, module_to_erl(PT)) || D <- NeededSrcDirs],
                              [{PT, PTerl} || PTerl <- PTerls, lists:member(PTerl, NeededErlFiles)]
                      end, proplists:get_all_values(parse_transform, ErlOpts))),
    ErlOptsFirst = lists:filter(fun({parse_transform, PT}) ->
                                        not lists:member(PT, ParseTransforms);
                                   (_) ->
                                        true
                               end, ErlOpts),
    {ErlFirstFiles ++ ParseTransformsErls, ErlOptsFirst}.

split_source_files(SourceFiles, ErlOpts) ->
    ParseTransforms = proplists:get_all_values(parse_transform, ErlOpts),
    lists:partition(fun(Source) ->
                            lists:member(filename_to_atom(Source), ParseTransforms)
                    end, SourceFiles).

filename_to_atom(F) -> list_to_atom(filename:rootname(filename:basename(F))).

%% Get subset of SourceFiles which need to be recompiled, respecting
%% dependencies induced by given graph G.
needed_files(G, ErlOpts, RebarOpts, Dir, OutDir, SourceFiles) ->
    lists:filter(fun(Source) ->
                         TargetBase = target_base(OutDir, Source),
                         Target = TargetBase ++ ".beam",
                         PrivIncludes = [{i, filename:join(Dir, Src)}
                                         || Src <- rebar_dir:all_src_dirs(RebarOpts, ["src"], [])],
                         AllOpts = [{outdir, filename:dirname(Target)}
                                   ,{i, filename:join(Dir, "include")}
                                   ,{i, Dir}] ++ PrivIncludes ++ ErlOpts,
                         %% necessary for erlang:function_exported/3 to work as expected
                         %% called here for clarity as it's required by both opts_changed/2
                         %% and erl_compiler_opts_set/0
                         _ = code:ensure_loaded(compile),
                         digraph:vertex(G, Source) > {Source, filelib:last_modified(Target)}
                              orelse opts_changed(AllOpts, TargetBase)
                              orelse erl_compiler_opts_set()
                 end, SourceFiles).

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

opts_changed(NewOpts, Target) ->
    TotalOpts = case erlang:function_exported(compile, env_compiler_options, 0) of
        true  -> NewOpts ++ compile:env_compiler_options();
        false -> NewOpts
    end,
    case compile_info(Target) of
        {ok, Opts} -> lists:any(fun effects_code_generation/1, lists:usort(TotalOpts) -- lists:usort(Opts));
        _          -> true
    end.

effects_code_generation(Option) ->
    case Option of
        beam -> false;
        report_warnings -> false;
        report_errors -> false;
        return_errors-> false;
        return_warnings-> false;
        report -> false;
        warnings_as_errors -> false;
        binary -> false;
        verbose -> false;
        {cwd,_} -> false;
        {outdir, _} -> false;
        _ -> true
    end.

compile_info(Target) ->
    case beam_lib:chunks(Target, [compile_info]) of
        {ok, {_mod, Chunks}} ->
            CompileInfo = proplists:get_value(compile_info, Chunks, []),
            {ok, proplists:get_value(options, CompileInfo, [])};
        {error, beam_lib, Reason} ->
            ?WARN("Couldn't read debug info from ~p for reason: ~p", [Target, Reason]),
            {error, Reason}
    end.

erl_compiler_opts_set() ->
    EnvSet = case os:getenv("ERL_COMPILER_OPTIONS") of
        false -> false;
        _     -> true
    end,
    %% return false if changed env opts would have been caught in opts_changed/2
    EnvSet andalso not erlang:function_exported(compile, env_compiler_options, 0).

erlcinfo_file(Dir) ->
    filename:join(rebar_dir:local_cache_dir(Dir), ?ERLCINFO_FILE).

%% Get dependency graph of given Erls files and their dependencies (header files,
%% parse transforms, behaviours etc.) located in their directories or given
%% InclDirs. Note that last modification times stored in vertices already respect
%% dependencies induced by given graph G.
init_erlcinfo(InclDirs, Erls, Dir, OutDir) ->
    G = digraph:new([acyclic]),
    try restore_erlcinfo(G, InclDirs, Dir)
    catch
        _:_ ->
            ?WARN("Failed to restore ~ts file. Discarding it.~n", [erlcinfo_file(Dir)]),
            file:delete(erlcinfo_file(Dir))
    end,
    Dirs = source_and_include_dirs(InclDirs, Erls),
    %% A source file may have been renamed or deleted. Remove it from the graph
    %% and remove any beam file for that source if it exists.
    Modified = maybe_rm_beams_and_edges(G, OutDir, Erls),
    Modified1 = lists:foldl(update_erlcinfo_fun(G, Dirs), Modified, Erls),
    if Modified1 -> store_erlcinfo(G, InclDirs, Dir); not Modified1 -> ok end,
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

source_and_include_dirs(InclDirs, Erls) ->
    SourceDirs = lists:map(fun filename:dirname/1, Erls),
    lists:usort(InclDirs ++ SourceDirs).

update_erlcinfo(G, Dirs, Source) ->
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
                    modify_erlcinfo(G, Source, LastModified, filename:dirname(Source), Dirs);
                _ ->
                    Modified = lists:foldl(
                        update_erlcinfo_fun(G, Dirs),
                        false, digraph:out_neighbours(G, Source)),
                    MaxModified = update_max_modified_deps(G, Source),
                    case Modified orelse MaxModified > LastUpdated of
                        true -> modified;
                        false -> unmodified
                    end
            end;
        false ->
            modify_erlcinfo(G, Source, filelib:last_modified(Source), filename:dirname(Source), Dirs)
    end.

update_erlcinfo_fun(G, Dirs) ->
    fun(Erl, Modified) ->
        case update_erlcinfo(G, Dirs, Erl) of
            modified -> true;
            unmodified -> Modified
        end
    end.

update_max_modified_deps(G, Source) ->
    MaxModified = lists:max(lists:map(
        fun(File) -> {_, MaxModified} = digraph:vertex(G, File), MaxModified end,
        [Source|digraph:out_neighbours(G, Source)])),
    digraph:add_vertex(G, Source, MaxModified),
    MaxModified.

modify_erlcinfo(G, Source, LastModified, Dir, Dirs) ->
    {ok, Fd} = file:open(Source, [read]),
    Incls = parse_attrs(Fd, [], Dir),
    AbsIncls = expand_file_names(Incls, Dirs),
    ok = file:close(Fd),
    digraph:add_vertex(G, Source, LastModified),
    digraph:del_edges(G, digraph:out_edges(G, Source)),
    lists:foreach(
      fun(Incl) ->
              update_erlcinfo(G, Dirs, Incl),
              digraph:add_edge(G, Source, Incl)
      end, AbsIncls),
    modified.

restore_erlcinfo(G, InclDirs, Dir) ->
    case file:read_file(erlcinfo_file(Dir)) of
        {ok, Data} ->
            % Since externally passed InclDirs can influence erlcinfo graph (see
            % modify_erlcinfo), we have to check here that they didn't change.
            #erlcinfo{vsn=?ERLCINFO_VSN, info={Vs, Es, InclDirs}} =
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

store_erlcinfo(G, InclDirs, Dir) ->
    Vs = lists:map(fun(V) -> digraph:vertex(G, V) end, digraph:vertices(G)),
    Es = lists:map(fun(E) -> digraph:edge(G, E) end, digraph:edges(G)),
    File = erlcinfo_file(Dir),
    ok = filelib:ensure_dir(File),
    Data = term_to_binary(#erlcinfo{info={Vs, Es, InclDirs}}, [{compressed, 2}]),
    file:write_file(File, Data).

%% NOTE: If, for example, one of the entries in Files, refers to
%% gen_server.erl, that entry will be dropped. It is dropped because
%% such an entry usually refers to the beam file, and we don't pass a
%% list of OTP src dirs for finding gen_server.erl's full path. Also,
%% if gen_server.erl was modified, it's not rebar's task to compile a
%% new version of the beam file. Therefore, it's reasonable to drop
%% such entries. Also see process_attr(behaviour, Form, Includes).
-spec expand_file_names([file:filename()],
                        [file:filename()]) -> [file:filename()].
expand_file_names(Files, Dirs) ->
    %% We check if Files exist by itself or within the directories
    %% listed in Dirs.
    %% Return the list of files matched.
    lists:flatmap(
      fun(Incl) ->
              case filelib:is_regular(Incl) of
                  true ->
                      [Incl];
                  false ->
                      lists:flatmap(
                        fun(Dir) ->
                                FullPath = filename:join(Dir, Incl),
                                case filelib:is_regular(FullPath) of
                                    true ->
                                        [FullPath];
                                    false ->
                                        []
                                end
                        end, Dirs)
              end
      end, Files).

-spec internal_erl_compile(rebar_dict(), file:filename(), file:filename(),
                           file:filename(), list(), rebar_dict()) ->
      ok | {ok, any()} | {error, any(), any()}.
internal_erl_compile(Opts, Dir, Module, OutDir, ErlOpts, RebarOpts) ->
    Target = target_base(OutDir, Module) ++ ".beam",
    ok = filelib:ensure_dir(Target),
    PrivIncludes = [{i, filename:join(Dir, Src)}
                    || Src <- rebar_dir:all_src_dirs(RebarOpts, ["src"], [])],
    AllOpts = [{outdir, filename:dirname(Target)}, no_spawn_compiler_process]
              ++ ErlOpts ++ PrivIncludes ++
              [{i, filename:join(Dir, "include")}, {i, Dir}, return],
    case compile:file(Module, AllOpts) of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            FormattedWs = format_error_sources(Ws, Opts),
            rebar_base_compiler:ok_tuple(Module, FormattedWs);
        {error, Es, Ws} ->
            error_tuple(Module, Es, Ws, AllOpts, Opts)
    end.

error_tuple(Module, Es, Ws, AllOpts, Opts) ->
    FormattedEs = format_error_sources(Es, Opts),
    FormattedWs = format_error_sources(Ws, Opts),
    rebar_base_compiler:error_tuple(Module, FormattedEs, FormattedWs, AllOpts).

format_error_sources(Es, Opts) ->
    [{rebar_base_compiler:format_error_source(Src, Opts), Desc}
     || {Src, Desc} <- Es].

target_base(OutDir, Source) ->
    filename:join(OutDir, filename:basename(Source, ".erl")).

-spec compile_mib(rebar_app_info:t()) ->
    fun((file:filename(), file:filename(), rebar_dict()) -> 'ok').
compile_mib(AppInfo) ->
    fun(Source, Target, Opts) ->
        Dir = filename:dirname(Target),
        Mib = filename:rootname(Target),
        HrlFilename = Mib ++ ".hrl",

        AppInclude = filename:join([rebar_app_info:dir(AppInfo), "include"]),

        ok = filelib:ensure_dir(Target),
        ok = filelib:ensure_dir(filename:join([AppInclude, "dummy.hrl"])),

        AllOpts = [{outdir, Dir}
                  ,{i, [Dir]}] ++
            rebar_opts:get(Opts, mib_opts, []),

        case snmpc:compile(Source, AllOpts) of
            {ok, _} ->
                MibToHrlOpts =
                    case proplists:get_value(verbosity, AllOpts, undefined) of
                        undefined ->
                            #options{specific = [],
                                     cwd = rebar_dir:get_cwd()};
                        Verbosity ->
                            #options{specific = [{verbosity, Verbosity}],
                                     cwd = rebar_dir:get_cwd()}
                    end,
                ok = snmpc:mib_to_hrl(Mib, Mib, MibToHrlOpts),
                rebar_file_utils:mv(HrlFilename, AppInclude),
                ok;
            {error, compilation_failed} ->
                ?ABORT
        end
    end.

-spec compile_xrl(file:filename(), file:filename(),
                  rebar_dict()) -> 'ok'.
compile_xrl(Source, Target, Opts) ->
    AllOpts = [{scannerfile, Target} | rebar_opts:get(Opts, xrl_opts, [])],
    compile_xrl_yrl(Opts, Source, Target, AllOpts, leex).

-spec compile_yrl(file:filename(), file:filename(),
                  rebar_dict()) -> 'ok'.
compile_yrl(Source, Target, Opts) ->
    AllOpts = [{parserfile, Target} | rebar_opts:get(Opts, yrl_opts, [])],
    compile_xrl_yrl(Opts, Source, Target, AllOpts, yecc).

-spec compile_xrl_yrl(rebar_dict(), file:filename(),
                      file:filename(), list(), module()) -> 'ok'.
compile_xrl_yrl(_Opts, Source, Target, AllOpts, Mod) ->
    %% FIX ME: should be the outdir or something
    Dir = filename:dirname(filename:dirname(Target)),
    AllOpts1 = [{includefile, filename:join(Dir, I)} || {includefile, I} <- AllOpts,
                                                        filename:pathtype(I) =:= relative],
    case needs_compile(Source, Target) of
        true ->
            case Mod:file(Source, AllOpts1 ++ [{return, true}]) of
                {ok, _} ->
                    ok;
                {ok, _Mod, Ws} ->
                    rebar_base_compiler:ok_tuple(Source, Ws);
                {error, Es, Ws} ->
                    rebar_base_compiler:error_tuple(Source,
                                                    Es, Ws, AllOpts1)
            end;
        false ->
            skipped
    end.

needs_compile(Source, Target) ->
    filelib:last_modified(Source) > filelib:last_modified(Target).



-spec dirs(file:filename()) -> [file:filename()].
dirs(Dir) ->
    [F || F <- filelib:wildcard(filename:join([Dir, "*"])), filelib:is_dir(F)].

-spec delete_dir(file:filename(), [string()]) -> 'ok' | {'error', atom()}.
delete_dir(Dir, []) ->
    file:del_dir(Dir);
delete_dir(Dir, Subdirs) ->
    lists:foreach(fun(D) -> delete_dir(D, dirs(D)) end, Subdirs),
    file:del_dir(Dir).

parse_attrs(Fd, Includes, Dir) ->
    case io:parse_erl_form(Fd, "") of
        {ok, Form, _Line} ->
            case erl_syntax:type(Form) of
                attribute ->
                    NewIncludes = process_attr(Form, Includes, Dir),
                    parse_attrs(Fd, NewIncludes, Dir);
                _ ->
                    parse_attrs(Fd, Includes, Dir)
            end;
        {eof, _} ->
            Includes;
        _Err ->
            parse_attrs(Fd, Includes, Dir)
    end.

process_attr(Form, Includes, Dir) ->
    AttrName = erl_syntax:atom_value(erl_syntax:attribute_name(Form)),
    process_attr(AttrName, Form, Includes, Dir).

process_attr(import, Form, Includes, _Dir) ->
    case erl_syntax_lib:analyze_import_attribute(Form) of
        {Mod, _Funs} ->
            [module_to_erl(Mod)|Includes];
        Mod ->
            [module_to_erl(Mod)|Includes]
    end;
process_attr(file, Form, Includes, _Dir) ->
    {File, _} = erl_syntax_lib:analyze_file_attribute(Form),
    [File|Includes];
process_attr(include, Form, Includes, _Dir) ->
    [FileNode] = erl_syntax:attribute_arguments(Form),
    File = erl_syntax:string_value(FileNode),
    [File|Includes];
process_attr(include_lib, Form, Includes, Dir) ->
    [FileNode] = erl_syntax:attribute_arguments(Form),
    RawFile = erl_syntax:string_value(FileNode),
    maybe_expand_include_lib_path(RawFile, Dir) ++ Includes;
process_attr(behavior, Form, Includes, _Dir) ->
    process_attr(behaviour, Form, Includes, _Dir);
process_attr(behaviour, Form, Includes, _Dir) ->
    [FileNode] = erl_syntax:attribute_arguments(Form),
    File = module_to_erl(erl_syntax:atom_value(FileNode)),
    [File|Includes];
process_attr(compile, Form, Includes, _Dir) ->
    [Arg] = erl_syntax:attribute_arguments(Form),
    case erl_syntax:concrete(Arg) of
        {parse_transform, Mod} ->
            [module_to_erl(Mod)|Includes];
        {core_transform, Mod} ->
            [module_to_erl(Mod)|Includes];
        L when is_list(L) ->
            lists:foldl(
              fun({parse_transform, Mod}, Acc) ->
                      [module_to_erl(Mod)|Acc];
                 ({core_transform, Mod}, Acc) ->
                      [module_to_erl(Mod)|Acc];
                 (_, Acc) ->
                      Acc
              end, Includes, L);
        _ ->
            Includes
    end;
process_attr(_, _Form, Includes, _Dir) ->
    Includes.

module_to_erl(Mod) ->
    atom_to_list(Mod) ++ ".erl".

%% Given a path like "stdlib/include/erl_compile.hrl", return
%% "OTP_INSTALL_DIR/lib/erlang/lib/stdlib-x.y.z/include/erl_compile.hrl".
%% Usually a simple [Lib, SubDir, File1] = filename:split(File) should
%% work, but to not crash when an unusual include_lib path is used,
%% utilize more elaborate logic.
maybe_expand_include_lib_path(File, Dir) ->
    File1 = filename:basename(File),
    case filename:split(filename:dirname(File)) of
        [_] ->
            warn_and_find_path(File, Dir);
        [Lib | SubDir] ->
            case code:lib_dir(list_to_atom(Lib), list_to_atom(filename:join(SubDir))) of
                {error, bad_name} ->
                    warn_and_find_path(File, Dir);
                AppDir ->
                    [filename:join(AppDir, File1)]
            end
    end.

%% The use of -include_lib was probably incorrect by the user but lets try to make it work.
%% We search in the outdir and outdir/../include to see if the header exists.
warn_and_find_path(File, Dir) ->
    SrcHeader = filename:join(Dir, File),
    case filelib:is_regular(SrcHeader) of
        true ->
            [SrcHeader];
        false ->
            IncludeDir = filename:join(rebar_utils:droplast(filename:split(Dir))++["include"]),
            IncludeHeader = filename:join(IncludeDir, File),
            case filelib:is_regular(IncludeHeader) of
                true ->
                    [filename:join(IncludeDir, File)];
                false ->
                    []
            end
    end.

%%
%% Ensure all files in a list are present and abort if one is missing
%%
-spec check_files([file:filename()]) -> [file:filename()].
check_files(FileList) ->
    [check_file(F) || F <- FileList].

check_file(File) ->
    case filelib:is_regular(File) of
        false -> ?ABORT("File ~p is missing, aborting\n", [File]);
        true -> File
    end.

outdir(RebarOpts) ->
    ErlOpts = rebar_opts:erl_opts(RebarOpts),
    proplists:get_value(outdir, ErlOpts, ?DEFAULT_OUTDIR).

include_abs_dirs(ErlOpts, BaseDir) ->
    ErlOptIncludes = proplists:get_all_values(i, ErlOpts),
    InclDirs = lists:map(fun(Incl) -> filename:absname(Incl) end, ErlOptIncludes),
    [filename:join([BaseDir, "include"])|InclDirs].

dir_recursive(Opts, Dir, CompileOpts) when is_list(CompileOpts) ->
    case proplists:get_value(recursive,CompileOpts) of
        undefined -> rebar_dir:recursive(Opts, Dir);
        Recursive -> Recursive
    end.

valid_erl_first_conf(FileList) ->
    Strs = filter_file_list(FileList),
    case rebar_utils:is_list_of_strings(Strs) of
        true -> true;
        false -> ?ABORT("An invalid file list (~p) was provided as part of your erl_first_files directive",
                        [FileList])
    end.

filter_file_list(FileList) ->
    Atoms = lists:filter( fun(X) -> is_atom(X) end, FileList),
    case Atoms of
        [] ->
            FileList;
        _ ->
          atoms_in_erl_first_files_warning(Atoms),
          lists:filter( fun(X) -> not(is_atom(X)) end, FileList)
     end.

atoms_in_erl_first_files_warning(Atoms) ->
  W = "You have provided atoms as file entries in erl_first_files; "
      "erl_first_files only expects lists of filenames as strings. "
      "The following modules (~p) may not work as expected and it is advised "
      "that you change these entries to string format "
      "(e.g., \"src/module.erl\") ",
  ?WARN(W, [Atoms]).

warn_deprecated() ->
    case get({deprecate_warn, ?MODULE}) of
        undefined ->
            ?WARN("Calling deprecated ~p compiler module. This module has been "
                  "replaced by rebar_compiler and rebar_compiler_erl, but will "
                  "remain available.", [?MODULE]),
            put({deprecate_warn, ?MODULE}, true),
            ok;
        _ ->
            ok
    end.
