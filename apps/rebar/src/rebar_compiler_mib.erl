-module(rebar_compiler_mib).

-behaviour(rebar_compiler).

-export([context/1,
         needed_files/4,
         dependencies/3,
         compile/4, compile_and_track/4,
         clean/2]).

-include("rebar.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

context(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    Mappings = [{".bin", filename:join([Dir, "priv", "mibs"])},
               {".hrl", filename:join(Dir, "include")}],

    #{src_dirs => ["mibs"],
      include_dirs => [],
      src_ext => ".mib",
      out_mappings => Mappings}.

needed_files(Graph, FoundFiles, Mappings, AppInfo) ->
    RebarOpts = rebar_app_info:opts(AppInfo),
    MibFirstConf = rebar_opts:get(RebarOpts, mib_first_files, []),
    valid_mib_first_conf(MibFirstConf),
    Dir = rebar_app_info:dir(AppInfo),
    MibFirstFiles = [filename:join(Dir, File) || File <- MibFirstConf],

    %% Remove first files from found files
    RestFiles = [Source || Source <- FoundFiles, not lists:member(Source, MibFirstFiles)],

    Opts = rebar_opts:get(rebar_app_info:opts(AppInfo), mib_opts, []),
    {{[F || F <- MibFirstFiles,
            needs_rebuild(Graph, F, Mappings, Opts)], Opts},
     {[F || F <- RestFiles,
            needs_rebuild(Graph, F, Mappings, Opts)], Opts}}.

needs_rebuild(Graph, Source, Mappings, Opts) ->
    {_, BinOut} = lists:keyfind(".bin", 1, Mappings),
    BaseName = filename:basename(Source, ".mib"),
    AllOpts = [{outdir, BinOut}, {i, [BinOut]}] ++ Opts,
    OutFiles = [filename:join(OutDir, BaseName) ++ Ext || {Ext, OutDir} <- Mappings],
    Stamp = digraph:vertex(Graph, Source),
    lists:any(fun(OutFile) ->
        Stamp > {Source, filelib:last_modified(OutFile)}
        orelse opts_changed(Graph, AllOpts, OutFile)
    end, OutFiles).

opts_changed(Graph, NewOpts, Target) ->
    case digraph:vertex(Graph, filename:basename(Target)) of
        {_Target, {artifact, Opts}} ->
            lists:usort(NewOpts) =/= lists:usort(Opts);
        _Other ->
            true
end.

valid_mib_first_conf(FileList) ->
    Strs = filter_file_list(FileList),
    case rebar_utils:is_list_of_strings(Strs) of
        true -> true;
        false -> ?ABORT("An invalid file list (~p) was provided as part of your mib_first_files directive",
                        [FileList])
    end.

filter_file_list(FileList) ->
    Atoms = lists:filter( fun(X) -> is_atom(X) end, FileList),
    case Atoms of
        [] ->
            FileList;
        _ ->
          atoms_in_mib_first_files_warning(Atoms),
          lists:filter( fun(X) -> not(is_atom(X)) end, FileList)
     end.

atoms_in_mib_first_files_warning(Atoms) ->
  W = "You have provided atoms as file entries in mib_first_files; "
      "mib_first_files only expects lists of filenames as strings. "
      "The following MIBs (~p) may not work as expected and it is advised "
      "that you change these entries to string format "
      "(e.g., \"mibs/SOME-MIB.mib\") ",
  ?WARN(W, [Atoms]).


dependencies(File, _Dir, SrcDirs) ->
    SrcFiles = lists:append([src_files(SrcDir) || SrcDir <- SrcDirs]),
    file_deps(File, SrcFiles).

compile(Source, OutDirs, _, Opts) ->
    {_, BinOut} = lists:keyfind(".bin", 1, OutDirs),
    {_, HrlOut} = lists:keyfind(".hrl", 1, OutDirs),

    ok = rebar_file_utils:ensure_dir(BinOut),
    ok = rebar_file_utils:ensure_dir(HrlOut),
    Mib = filename:join(BinOut, filename:basename(Source, ".mib")),
    HrlFilename = Mib ++ ".hrl",

    AllOpts = [{outdir, BinOut}, {i, [BinOut]}] ++ Opts,

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
            rebar_file_utils:mv(HrlFilename, HrlOut),
            ok;
        {error, compilation_failed} ->
            ?ABORT
    end.

compile_and_track(Source, OutDirs, _Config, Opts) ->
    {_, BinOut} = lists:keyfind(".bin", 1, OutDirs),
    {_, HrlOut} = lists:keyfind(".hrl", 1, OutDirs),

    ok = rebar_file_utils:ensure_dir(BinOut),
    ok = rebar_file_utils:ensure_dir(HrlOut),
    Mib = filename:join(BinOut, filename:basename(Source, ".mib")),
    HrlFilename = Mib ++ ".hrl",

    AllOpts = [{outdir, BinOut}, {i, [BinOut]}] ++ Opts,

    case snmpc:compile(Source, AllOpts) of
        {ok, BinName} ->
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
            rebar_file_utils:mv(HrlFilename, HrlOut),
            {ok, [{Source, filename:basename(BinName), AllOpts},
                  {Source, filename:basename(HrlFilename), AllOpts}]};
        {error, compilation_failed} ->
            ?ABORT
    end.

clean(MibFiles, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    MIBs = [filename:rootname(filename:basename(MIB)) || MIB <- MibFiles],
    rebar_file_utils:delete_each(
      [filename:join([AppDir, "include", MIB++".hrl"]) || MIB <- MIBs]),
    ok = rebar_file_utils:rm_rf(filename:join([AppDir, "priv/mibs/*.bin"])).

src_files(Dir) ->
    %% .mib extension is assumed to be valid here
    case file:list_dir(Dir) of
        {ok, Files} ->
            [filename:join(Dir, File)
             || File <- Files,
                filename:extension(File) =:= ".mib"];
        _ ->
            []
    end.

file_deps(File, Files) ->
    DepMods = imports_in_file(File),
    lists:filter(
        fun(F) ->
            Mods = modules_in_file(F),
            lists:any(fun(M) -> lists:member(M, Mods) end, DepMods)
        end,
        Files
    ).

modules_in_file(File) ->
    {ok, Bin} = file:read_file(File),
    Res = re:run(
        Bin,
        "^([a-zA-Z0-9_-]+)\\s+DEFINITIONS\\s+::=\\s+BEGIN",
        [multiline, {capture, all_but_first, list}, global, unicode]
    ),
    case Res of
        nomatch ->
            [];
        {match, List} ->
            lists:usort(lists:append(List))
    end.

imports_in_file(File) ->
    {ok, Bin} = file:read_file(File),
    ImportMatch = re:run(
        Bin,
        "IMPORTS\\s+(.*);",
        [multiline, dotall, ungreedy, {capture, all_but_first, list}, global]
    ),
    case ImportMatch of
        nomatch ->
            [];
        {match, ImportSections} ->
            Modules = re:run(
                ImportSections,
                "FROM\\s+([a-zA-Z0-9_-]+)\\s+",
                [multiline, {capture, all_but_first, list}, global]
            ),
            case Modules of
                nomatch ->
                    [];
                {match, List} ->
                    lists:usort(lists:append(List))
            end
    end.


