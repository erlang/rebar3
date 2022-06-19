-module(rebar_compiler_erl).

-behaviour(rebar_compiler).

-export([context/1,
         needed_files/4,
         dependencies/3, dependencies/4,
         compile/4, compile_and_track/4,
         clean/2,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

context(AppInfo) ->
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    Mappings = [{".beam", EbinDir}],

    OutDir = rebar_app_info:dir(AppInfo),
    SrcDirs = rebar_dir:src_dirs(rebar_app_info:opts(AppInfo), ["src"]),
    ExistingSrcDirs = lists:filter(fun(D) ->
                                           ec_file:is_dir(filename:join(OutDir, D))
                                   end, SrcDirs),

    RebarOpts = rebar_app_info:opts(AppInfo),
    ErlOpts = rebar_opts:erl_opts(RebarOpts),
    ErlOptIncludes = proplists:get_all_values(i, ErlOpts),
    AbsIncl = [filename:join(OutDir, "include") | % standard include path
               %% includes specified by erl_opts
               lists:map(fun(Incl) -> filename:absname(Incl) end, ErlOptIncludes)] ++
              %% all source directories are valid, and might also be recursive
              lists:append([
                  find_recursive_incl(OutDir, Src, RebarOpts) ||
                  Src <- rebar_dir:all_src_dirs(RebarOpts, ["src"], [])
              ]) ++
              %% top-level dir for legacy stuff
              [OutDir],
    PTrans = proplists:get_all_values(parse_transform, ErlOpts),
    Macros = [case Tup of
                  {d,Name} -> Name;
                  {d,Name,Val} -> {Name,Val}
              end || Tup <- ErlOpts,
                     is_tuple(Tup) andalso element(1,Tup) == d],

    #{src_dirs => ExistingSrcDirs,
      include_dirs => AbsIncl,
      src_ext => ".erl",
      out_mappings => Mappings,
      dependencies_opts => [{includes, AbsIncl}, {macros, Macros},
                            {parse_transforms, PTrans}]}.


needed_files(Graph, FoundFiles, _, AppInfo) ->
    OutDir = rebar_app_info:out_dir(AppInfo),
    Dir = rebar_app_info:dir(AppInfo),
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    RebarOpts = rebar_app_info:opts(AppInfo),
    ErlOpts = rebar_opts:erl_opts(RebarOpts),
    ?DEBUG("compile options: {erl_opts, ~p}.", [ErlOpts]),
    ?DEBUG("files to analyze ~p", [FoundFiles]),

    {ParseTransforms, Rest} = split_source_files(FoundFiles, ErlOpts),
    NeededErlFiles = case needed_files(Graph, ErlOpts, RebarOpts, OutDir, EbinDir, ParseTransforms) of
                         [] ->
                             needed_files(Graph, ErlOpts, RebarOpts, OutDir, EbinDir, Rest);
                         _ ->
                             %% at least one parse transform in the opts needs updating, so recompile all
                             FoundFiles
                     end,
    {ErlFirstFiles, ErlOptsFirst} = erl_first_files(RebarOpts, ErlOpts, Dir, NeededErlFiles),
    SubGraph = digraph_utils:subgraph(Graph, NeededErlFiles),
    DepErlsOrdered = digraph_utils:topsort(SubGraph),
    %% Break out the files required by other modules from those
    %% that none other depend of; the former must be sequentially
    %% built, the rest is parallelizable.
    OtherErls = lists:partition(
        fun(Erl) -> lists:any(
            fun(Edge) ->
                {_E, _V1, _V2, Kind} = digraph:edge(Graph, Edge),
                Kind =/= artifact
            end, digraph:in_edges(Graph, Erl)) end,
        lists:reverse([Dep || Dep <- DepErlsOrdered,
                              not lists:member(Dep, ErlFirstFiles)])
    ),

    PrivIncludes = [{i, filename:join(OutDir, Src)}
                    || Src <- rebar_dir:all_src_dirs(RebarOpts, ["src"], [])],
    AdditionalOpts = PrivIncludes ++ [{i, filename:join(OutDir, "include")}, {i, OutDir}, return],

    true = digraph:delete(SubGraph),

    {{ErlFirstFiles, ErlOptsFirst ++ AdditionalOpts},
     {OtherErls, ErlOpts ++ AdditionalOpts}}.

dependencies(Source, SourceDir, Dirs) ->
    case file:open(Source, [read]) of
        {ok, Fd} ->
            Incls = parse_attrs(Fd, [], SourceDir),
            AbsIncls = expand_file_names(Incls, Dirs),
            ok = file:close(Fd),
            AbsIncls;
        {error, Reason} ->
            throw(?PRV_ERROR({cannot_read_file, Source, file:format_error(Reason)}))
    end.

dependencies(Source, _SourceDir, Dirs, DepOpts) ->
    rebar_compiler_epp:ensure_started(),
    OptPTrans = proplists:get_value(parse_transforms, DepOpts, []),
    try rebar_compiler_epp:deps(Source, DepOpts) of
        #{include := AbsIncls,
          missing_include_file := _MissIncl,
          missing_include_lib := _MissInclLib,
          parse_transform := PTrans,
          behaviour := Behaviours} ->
            %% TODO: check for core transforms?
            {_MissIncl, _MissInclLib} =/= {[],[]} andalso
            ?DIAGNOSTIC("Missing: ~p", [{_MissIncl, _MissInclLib}]),
            lists:filtermap(
                fun (Mod) -> rebar_compiler_epp:resolve_source(Mod, Dirs) end,
                OptPTrans ++ PTrans ++ Behaviours) ++ AbsIncls
    catch
        error:{badmatch, {error, Reason}} ->
            case file:format_error(Reason) of
                "unknown POSIX error" ->
                    throw(?PRV_ERROR({cannot_read_file, Source, Reason}));
                ReadableReason ->
                    throw(?PRV_ERROR({cannot_read_file, Source, ReadableReason}))
            end;
        error:Reason ->
            throw(?PRV_ERROR({cannot_read_file, Source, Reason}))
    end.

compile(Source, [{_, OutDir}], Config, ErlOpts) ->
    case compile:file(Source, [{outdir, OutDir}, no_spawn_compiler_process | ErlOpts]) of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, []} ->
            ok;
        {ok, _Mod, Ws} ->
            FormattedWs = format_error_sources(Ws, Config),
            rebar_compiler:ok_tuple(Source, FormattedWs);
        {error, Es, Ws} ->
            error_tuple(Source, Es, Ws, Config, ErlOpts);
        error ->
            error
    end.

compile_and_track(Source, [{Ext, OutDir}], Config, ErlOpts) ->
    rebar_compiler_epp:flush(),
    BuildOpts = [{outdir, OutDir}, no_spawn_compiler_process | ErlOpts],
    Target = target_base(OutDir, Source) ++ Ext,
    {ok, CompileVsn} = application:get_key(compiler, vsn),
    AllOpts = case erlang:function_exported(compile, env_compiler_options, 0) of
        true  -> [{compiler_version, CompileVsn}] ++ BuildOpts ++ compile:env_compiler_options();
        false -> [{compiler_version, CompileVsn}] ++ BuildOpts
    end,
    case compile:file(Source, BuildOpts) of
        {ok, _Mod} ->
            {ok, [{Source, Target, AllOpts}]};
        {ok, _Mod, []} ->
            {ok, [{Source, Target, AllOpts}]};
        {ok, _Mod, Ws} ->
            FormattedWs = format_error_sources(Ws, Config),
            {ok, Warns} = rebar_compiler:ok_tuple(Source, FormattedWs),
            {ok, [{Source, Target, AllOpts}], Warns};
        {error, Es, Ws} ->
            error_tuple(Source, Es, Ws, Config, ErlOpts);
        error ->
            error
    end.


clean(Files, AppInfo) ->
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    [begin
         Source = filename:basename(File, ".erl"),
         Target = target_base(EbinDir, Source) ++ ".beam",
         file:delete(Target)
     end || File <- Files].

%%

error_tuple(Module, Es, Ws, AllOpts, Opts) ->
    FormattedEs = format_error_sources(Es, AllOpts),
    FormattedWs = format_error_sources(Ws, AllOpts),
    rebar_compiler:error_tuple(Module, FormattedEs, FormattedWs, Opts).

format_error_sources(Es, Opts) ->
    [{rebar_compiler:format_error_source(Src, Opts), Desc}
     || {Src, Desc} <- Es].

find_recursive_incl(Base, Src, Opts) ->
    find_recursive_incl(Base, Src, Opts, rebar_dir:recursive(Opts, Src)).

find_recursive_incl(Base, Src, _Opts, false) ->
    [filename:join(Base, Src)];
find_recursive_incl(Base, Src, Opts, true) ->
    Dir = filename:join(Base, Src),
    case file:list_dir(Dir) of
        {error, _} ->
            [Dir];
        {ok, Files} ->
            [Dir] ++
            lists:append([find_recursive_incl(Dir, File, Opts)
                          || File <- Files, filelib:is_dir(filename:join(Dir, File))])
    end.

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
needed_files(Graph, ErlOpts, RebarOpts, Dir, OutDir, SourceFiles) ->
    PrivIncludes = [{i, filename:join(Dir, Src)}
                    || Src <- rebar_dir:all_src_dirs(RebarOpts, ["src"], [])],
    SharedOpts = [{i, filename:join(Dir, "include")},
                  {i, Dir}] ++ PrivIncludes ++ ErlOpts,
    CompilerOptsSet = erl_compiler_opts_set(),
    lists:filter(fun(Source) ->
                         TargetBase = target_base(OutDir, Source),
                         Target = TargetBase ++ ".beam",
                         AllOpts = [{outdir, filename:dirname(Target)} | SharedOpts],
                         digraph:vertex(Graph, Source) > {Source, filelib:last_modified(Target)}
                              orelse opts_changed(Graph, AllOpts, Target, TargetBase)
                              orelse CompilerOptsSet
                 end, SourceFiles).

target_base(OutDir, Source) ->
    filename:join(OutDir, filename:basename(Source, ".erl")).

opts_changed(Graph, NewOpts, Target, TargetBase) ->
    ModuleName = list_to_atom(filename:basename(TargetBase)),
    {ok, CompileVsn} = application:get_key(compiler, vsn),
    TotalOpts = case erlang:function_exported(compile, env_compiler_options, 0) of
        true  -> [{compiler_version, CompileVsn}] ++ NewOpts ++ compile:env_compiler_options();
        false -> [{compiler_version, CompileVsn}] ++ NewOpts
    end,
    TargetOpts = case digraph:vertex(Graph, Target) of
        {_Target, {artifact, Opts}} -> % tracked dep is found
            Opts;
        false -> % not found; might be a non-tracked DAG
            case compile_info(TargetBase) of
                {ok, Opts} -> Opts;
                _ -> []
            end
    end,
    lists:any(fun(Option) -> effects_code_generation(ModuleName, Option) end,
              lists:usort(TotalOpts) -- lists:usort(TargetOpts)).

effects_code_generation(ModuleName, Option) ->
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
        {parse_transform, ModuleName} -> false;
        no_spawn_compiler_process -> false;
        _ -> true
    end.

compile_info(Target) ->
    case beam_lib:chunks(Target, [compile_info]) of
        {ok, {_mod, Chunks}} ->
            CompileInfo = proplists:get_value(compile_info, Chunks, []),
            CompileVsn = proplists:get_value(version, CompileInfo, "unknown"),
            {ok, [{compiler_version, CompileVsn}
                  | proplists:get_value(options, CompileInfo, [])]};
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

module_to_erl(Mod) ->
    atom_to_list(Mod) ++ ".erl".

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
            lists:usort(Includes);
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
                      Res = rebar_utils:find_files_in_dirs(Dirs, [$^, Incl, $$], true),
                      case Res of
                          [] ->
                              ?DIAGNOSTIC("FILE ~p NOT FOUND", [Incl]),
                              [];
                          _ ->
                              Res
                      end
              end
      end, Files).

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

format_error({cannot_read_file, Source, Reason}) ->
    lists:flatten(io_lib:format("Cannot read file '~s': ~s", [Source, Reason]));
format_error(Other) ->
    io_lib:format("~p", [Other]).
