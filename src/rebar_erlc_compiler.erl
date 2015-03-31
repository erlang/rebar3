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

-export([compile/2,
         compile/3,
         clean/2]).

-include("rebar.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

-define(ERLCINFO_VSN, 2).
-define(ERLCINFO_FILE, "erlcinfo").
-type erlc_info_v() :: {digraph:vertex(), term()} | 'false'.
-type erlc_info_e() :: {digraph:vertex(), digraph:vertex()}.
-type erlc_info() :: {list(erlc_info_v()), list(erlc_info_e()), list(string())}.
-record(erlcinfo,
        {
          vsn = ?ERLCINFO_VSN :: pos_integer(),
          info = {[], [], []} :: erlc_info()
        }).

-define(RE_PREFIX, "^[^._]").

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

-spec compile(rebar_state:t(), file:name()) -> 'ok'.
compile(Config, Dir) ->
    compile(Config, Dir, filename:join([Dir, "ebin"])).

-spec compile(rebar_state:t(), file:name(), file:name()) -> 'ok'.
compile(Config, Dir, OutDir) ->
    rebar_base_compiler:run(Config,
                            check_files(rebar_state:get(
                                          Config, xrl_first_files, [])),
                            filename:join(Dir, "src"), ".xrl", filename:join(Dir, "src"), ".erl",
                            fun compile_xrl/3),
    rebar_base_compiler:run(Config,
                            check_files(rebar_state:get(
                                          Config, yrl_first_files, [])),
                            filename:join(Dir, "src"), ".yrl", filename:join(Dir, "src"), ".erl",
                            fun compile_yrl/3),
    rebar_base_compiler:run(Config,
                            check_files(rebar_state:get(
                                          Config, mib_first_files, [])),
                            filename:join(Dir, "mibs"), ".mib", filename:join([Dir, "priv", "mibs"]), ".bin",
                            fun compile_mib/3),
    doterl_compile(Config, Dir, OutDir).

-spec clean(rebar_state:t(), file:filename()) -> 'ok'.
clean(_Config, AppDir) ->
    MibFiles = rebar_utils:find_files(filename:join(AppDir, "mibs"), ?RE_PREFIX".*\\.mib\$"),
    MIBs = [filename:rootname(filename:basename(MIB)) || MIB <- MibFiles],
    rebar_file_utils:delete_each(
      [filename:join([AppDir, "include",MIB++".hrl"]) || MIB <- MIBs]),
    lists:foreach(fun(F) -> ok = rebar_file_utils:rm_rf(F) end,
                  [filename:join(AppDir, "ebin/*.beam"), filename:join(AppDir, "priv/mibs/*.bin")]),

    YrlFiles = rebar_utils:find_files(filename:join(AppDir, "src"), ?RE_PREFIX".*\\.[x|y]rl\$"),
    rebar_file_utils:delete_each(
      [ binary_to_list(iolist_to_binary(re:replace(F, "\\.[x|y]rl$", ".erl")))
        || F <- YrlFiles ]),

    %% Delete the build graph, if any
    rebar_file_utils:rm_rf(erlcinfo_file()),

    %% Erlang compilation is recursive, so it's possible that we have a nested
    %% directory structure in ebin with .beam files within. As such, we want
    %% to scan whatever is left in the ebin/ directory for sub-dirs which
    %% satisfy our criteria.
    BeamFiles = rebar_utils:find_files(filename:join(AppDir, "ebin"), ?RE_PREFIX".*\\.beam\$"),
    rebar_file_utils:delete_each(BeamFiles),
    lists:foreach(fun(Dir) -> delete_dir(Dir, dirs(Dir)) end, dirs(filename:join(AppDir, "ebin"))),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec doterl_compile(rebar_state:t(), file:filename(), file:filename()) -> ok.
doterl_compile(State, Dir, ODir) ->
    ErlOpts = rebar_utils:erl_opts(State),
    doterl_compile(State, Dir, ODir, [], ErlOpts).

doterl_compile(Config, Dir, OutDir, MoreSources, ErlOpts) ->
    ?DEBUG("erl_opts ~p", [ErlOpts]),
    %% Support the src_dirs option allowing multiple directories to
    %% contain erlang source. This might be used, for example, should
    %% eunit tests be separated from the core application source.
    SrcDirs = [filename:join(Dir, X) || X <- proplists:get_value(src_dirs, ErlOpts, ["src"])],
    AllErlFiles = gather_src(SrcDirs, []) ++ MoreSources,

    %% Make sure that ebin/ exists and is on the path
    ok = filelib:ensure_dir(filename:join(OutDir, "dummy.beam")),
    CurrPath = code:get_path(),
    true = code:add_path(filename:absname(OutDir)),
    OutDir1 = proplists:get_value(outdir, ErlOpts, OutDir),

    G = init_erlcinfo(proplists:get_all_values(i, ErlOpts), AllErlFiles),
    NeededErlFiles = needed_files(G, ErlOpts, Dir, OutDir1, AllErlFiles),
    ErlFirstFiles = erl_first_files(Config, NeededErlFiles),
    {DepErls, OtherErls} = lists:partition(
                             fun(Source) -> digraph:in_degree(G, Source) > 0 end,
                             [File || File <- NeededErlFiles, not lists:member(File, ErlFirstFiles)]),
    DepErlsOrdered = digraph_utils:topsort(digraph_utils:subgraph(G, DepErls)),
    FirstErls = ErlFirstFiles ++ lists:reverse(DepErlsOrdered),
    ?DEBUG("Files to compile first: ~p", [FirstErls]),
    rebar_base_compiler:run(
      Config, FirstErls, OtherErls,
      fun(S, C) ->
              internal_erl_compile(C, Dir, S, OutDir1, ErlOpts)
      end),
    true = code:set_path(CurrPath),
    ok.

erl_first_files(Config, NeededErlFiles) ->
    %% NOTE: rebar_config:get_local perhaps?
    ErlFirstFilesConf = rebar_state:get(Config, erl_first_files, []),
    %% NOTE: order of files in ErlFirstFiles is important!
    [File || File <- ErlFirstFilesConf, lists:member(File, NeededErlFiles)].

%% Get subset of SourceFiles which need to be recompiled, respecting
%% dependencies induced by given graph G.
needed_files(G, ErlOpts, Dir, OutDir, SourceFiles) ->
    lists:filter(fun(Source) ->
                         Target = target_base(OutDir, Source) ++ ".beam",
                         Opts = [{outdir, filename:dirname(Target)}
                                ,{i, filename:join(Dir, "include")}] ++ ErlOpts,
                         digraph:vertex(G, Source) > {Source, filelib:last_modified(Target)}
                              orelse opts_changed(Opts, Target)
                 end, SourceFiles).

opts_changed(Opts, Target) ->
    Basename = filename:basename(Target, ".beam"),
    Dirname = filename:dirname(Target),
    ObjectFile = filename:join([Dirname, Basename]),
    case code:load_abs(ObjectFile) of
        {module, Mod} ->
            Compile = Mod:module_info(compile),
            lists:sort(Opts) =/= lists:sort(proplists:get_value(options,
                                                                Compile,
                                                                undefined));
        {error, _} -> true
    end.

erlcinfo_file() ->
    filename:join(rebar_dir:local_cache_dir(), ?ERLCINFO_FILE).

%% Get dependency graph of given Erls files and their dependencies (header files,
%% parse transforms, behaviours etc.) located in their directories or given
%% InclDirs. Note that last modification times stored in vertices already respect
%% dependencies induced by given graph G.
init_erlcinfo(InclDirs, Erls) ->
    G = digraph:new([acyclic]),
    try restore_erlcinfo(G, InclDirs)
    catch
        _:_ ->
            ?WARN("Failed to restore ~s file. Discarding it.~n", [erlcinfo_file()]),
            file:delete(erlcinfo_file())
    end,
    Dirs = source_and_include_dirs(InclDirs, Erls),
    Modified = lists:foldl(update_erlcinfo_fun(G, Dirs), false, Erls),
    if Modified -> store_erlcinfo(G, InclDirs); not Modified -> ok end,
    G.

source_and_include_dirs(InclDirs, Erls) ->
    SourceDirs = lists:map(fun filename:dirname/1, Erls),
    lists:usort(["include" | InclDirs ++ SourceDirs]).

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
                    modify_erlcinfo(G, Source, LastModified, Dirs);
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
            modify_erlcinfo(G, Source, filelib:last_modified(Source), Dirs)
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

modify_erlcinfo(G, Source, LastModified, Dirs) ->
    {ok, Fd} = file:open(Source, [read]),
    Incls = parse_attrs(Fd, []),
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

restore_erlcinfo(G, InclDirs) ->
    case file:read_file(erlcinfo_file()) of
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

store_erlcinfo(G, InclDirs) ->
    Vs = lists:map(fun(V) -> digraph:vertex(G, V) end, digraph:vertices(G)),
    Es = lists:map(fun(E) -> digraph:edge(G, E) end, digraph:edges(G)),
    File = erlcinfo_file(),
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


-spec internal_erl_compile(rebar_config:config(), file:filename(), file:filename(),
    file:filename(), list()) -> ok | {ok, any()} | {error, any(), any()}.
internal_erl_compile(Config, Dir, Module, OutDir, ErlOpts) ->
    Target = target_base(OutDir, Module) ++ ".beam",
    ok = filelib:ensure_dir(Target),
    Opts = [{outdir, filename:dirname(Target)}] ++ ErlOpts ++
        [{i, filename:join(Dir, "include")}, return],
    case compile:file(Module, Opts) of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_base_compiler:ok_tuple(Config, Module, Ws);
        {error, Es, Ws} ->
            rebar_base_compiler:error_tuple(Config, Module, Es, Ws, Opts)
    end.

target_base(OutDir, Source) ->
    Module = filename:basename(Source, ".erl"),
    filename:join([OutDir|string:tokens(Module, ".")]).

-spec compile_mib(file:filename(), file:filename(),
                  rebar_state:t()) -> 'ok'.
compile_mib(Source, Target, Config) ->
    ok = filelib:ensure_dir(Target),
    ok = filelib:ensure_dir(filename:join("include", "dummy.hrl")),
    Opts = [{outdir, "priv/mibs"}, {i, ["priv/mibs"]}] ++
        rebar_state:get(Config, mib_opts, []),
    case snmpc:compile(Source, Opts) of
        {ok, _} ->
            Mib = filename:rootname(Target),
            MibToHrlOpts =
                case proplists:get_value(verbosity, Opts, undefined) of
                    undefined ->
                        #options{specific = []};
                    Verbosity ->
                        #options{specific = [{verbosity, Verbosity}]}
                end,
            ok = snmpc:mib_to_hrl(Mib, Mib, MibToHrlOpts),
            Hrl_filename = Mib ++ ".hrl",
            rebar_file_utils:mv(Hrl_filename, "include"),
            ok;
        {error, compilation_failed} ->
            ?FAIL
    end.

-spec compile_xrl(file:filename(), file:filename(),
                  rebar_state:t()) -> 'ok'.
compile_xrl(Source, Target, Config) ->
    Opts = [{scannerfile, Target} | rebar_state:get(Config, xrl_opts, [])],
    compile_xrl_yrl(Config, Source, Target, Opts, leex).

-spec compile_yrl(file:filename(), file:filename(),
                  rebar_state:t()) -> 'ok'.
compile_yrl(Source, Target, Config) ->
    Opts = [{parserfile, Target} | rebar_state:get(Config, yrl_opts, [])],
    compile_xrl_yrl(Config, Source, Target, Opts, yecc).

-spec compile_xrl_yrl(rebar_state:t(), file:filename(),
                      file:filename(), list(), module()) -> 'ok'.
compile_xrl_yrl(Config, Source, Target, Opts, Mod) ->
    Dir = rebar_state:dir(Config),
    Opts1 = [{includefile, filename:join(Dir, I)} || {includefile, I} <- Opts,
                                                     filename:pathtype(I) =:= relative],
    case filelib:last_modified(Source) > filelib:last_modified(Target) of
        true ->
            case Mod:file(Source, Opts1 ++ [{return, true}]) of
                {ok, _} ->
                    ok;
                {ok, _Mod, Ws} ->
                    rebar_base_compiler:ok_tuple(Config, Source, Ws);
                {error, Es, Ws} ->
                    rebar_base_compiler:error_tuple(Config, Source,
                                                    Es, Ws, Opts1)
            end;
        false ->
            skipped
    end.

gather_src([], Srcs) ->
    Srcs;
gather_src([Dir|Rest], Srcs) ->
    gather_src(
      Rest, Srcs ++ rebar_utils:find_files(Dir, ?RE_PREFIX".*\\.erl\$")).

-spec dirs(file:filename()) -> [file:filename()].
dirs(Dir) ->
    [F || F <- filelib:wildcard(filename:join([Dir, "*"])), filelib:is_dir(F)].

-spec delete_dir(file:filename(), [string()]) -> 'ok' | {'error', atom()}.
delete_dir(Dir, []) ->
    file:del_dir(Dir);
delete_dir(Dir, Subdirs) ->
    lists:foreach(fun(D) -> delete_dir(D, dirs(D)) end, Subdirs),
    file:del_dir(Dir).

parse_attrs(Fd, Includes) ->
    case io:parse_erl_form(Fd, "") of
        {ok, Form, _Line} ->
            case erl_syntax:type(Form) of
                attribute ->
                    NewIncludes = process_attr(Form, Includes),
                    parse_attrs(Fd, NewIncludes);
                _ ->
                    parse_attrs(Fd, Includes)
            end;
        {eof, _} ->
            Includes;
        _Err ->
            parse_attrs(Fd, Includes)
    end.

process_attr(Form, Includes) ->
    AttrName = erl_syntax:atom_value(erl_syntax:attribute_name(Form)),
    process_attr(AttrName, Form, Includes).

process_attr(import, Form, Includes) ->
    case erl_syntax_lib:analyze_import_attribute(Form) of
        {Mod, _Funs} ->
            [module_to_erl(Mod)|Includes];
        Mod ->
            [module_to_erl(Mod)|Includes]
    end;
process_attr(file, Form, Includes) ->
    {File, _} = erl_syntax_lib:analyze_file_attribute(Form),
    [File|Includes];
process_attr(include, Form, Includes) ->
    [FileNode] = erl_syntax:attribute_arguments(Form),
    File = erl_syntax:string_value(FileNode),
    [File|Includes];
process_attr(include_lib, Form, Includes) ->
    [FileNode] = erl_syntax:attribute_arguments(Form),
    RawFile = erl_syntax:string_value(FileNode),
    maybe_expand_include_lib_path(RawFile) ++ Includes;
process_attr(behaviour, Form, Includes) ->
    [FileNode] = erl_syntax:attribute_arguments(Form),
    File = module_to_erl(erl_syntax:atom_value(FileNode)),
    [File|Includes];
process_attr(compile, Form, Includes) ->
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
process_attr(_, _Form, Includes) ->
    Includes.

module_to_erl(Mod) ->
    atom_to_list(Mod) ++ ".erl".


%% Given the filename from an include_lib attribute, if the path
%% exists, return unmodified, or else get the absolute ERL_LIBS
%% path.
maybe_expand_include_lib_path(File) ->
    case filelib:is_regular(File) of
        true ->
            [File];
        false ->
            expand_include_lib_path(File)
    end.

%% Given a path like "stdlib/include/erl_compile.hrl", return
%% "OTP_INSTALL_DIR/lib/erlang/lib/stdlib-x.y.z/include/erl_compile.hrl".
%% Usually a simple [Lib, SubDir, File1] = filename:split(File) should
%% work, but to not crash when an unusual include_lib path is used,
%% utilize more elaborate logic.
expand_include_lib_path(File) ->
    File1 = filename:basename(File),
    Split = filename:split(filename:dirname(File)),
    Lib = hd(Split),
    SubDir = filename:join(tl(Split)),
    case code:lib_dir(list_to_atom(Lib), list_to_atom(SubDir)) of
        {error, bad_name} -> [];
        Dir -> [filename:join(Dir, File1)]
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
