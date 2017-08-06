%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_dialyzer).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, dialyzer).
-define(DEPS, [compile]).
-define(PLT_PREFIX, "rebar3").

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Opts = [{update_plt, $u, "update-plt", boolean, "Enable updating the PLT. Default: true"},
            {succ_typings, $s, "succ-typings", boolean, "Enable success typing analysis. Default: true"}],
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 dialyzer"},
                                                               {short_desc, short_desc()},
                                                               {desc, desc()},
                                                               {opts, Opts}])),
    {ok, State1}.

desc() ->
    short_desc() ++ "\n"
    "\n"
    "This command will build, and keep up-to-date, a suitable PLT and will use "
    "it to carry out success typing analysis on the current project.\n"
    "\n"
    "The following (optional) configurations can be added to a `proplist` of "
    "options `dialyzer` in rebar.config:\n"
    "`warnings` - a list of dialyzer warnings\n"
    "`get_warnings` - display warnings when altering a PLT file (boolean)\n"
    "`native` - use natively compiled key modules (boolean)\n"
    "`native_location` - the location of the native compiled module cache,"
    "`global` to store in $HOME/.cache/rebar3 (default) or a custom "
    "directory.\n"
    "`plt_apps` - the strategy for determining the applications which included "
    "in the PLT file, `top_level_deps` to include just the direct dependencies "
    "or `all_deps` to include all nested dependencies*\n"
    "`plt_extra_apps` - a list of extra applications to include in the PLT "
    "file\n"
    "`plt_extra_mods` - a list of extra modules to includes in the PLT file\n"
    "`plt_location` - the location of the PLT file, `local` to store in the "
    "profile's base directory (default) or a custom directory.\n"
    "`plt_prefix` - the prefix to the PLT file, defaults to \"rebar3\"**\n"
    "`base_plt_apps` - a list of applications to include in the base "
    "PLT file***\n"
    "`base_plt_mods` - a list of modules to include in the base "
    "PLT file***\n"
    "`base_plt_location` - the location of base PLT file, `global` to store in "
    "$HOME/.cache/rebar3 (default) or a custom directory***\n"
    "`base_plt_prefix` - the prefix to the base PLT file, defaults to "
    "\"rebar3\"** ***\n"
    "`exclude_apps` - a list of applications to exclude from PLT files and "
    "success typing analysis, `plt_extra_mods` and `base_plt_mods` can add "
    "modules from excluded applications\n"
    "`exclude_mods` - a list of modules to exclude from PLT files and "
    "success typing analysis\n"
    "\n"
    "For example, to warn on unmatched returns: \n"
    "{dialyzer, [{warnings, [unmatched_returns]}]}.\n"
    "\n"
    "*The direct dependent applications are listed in `applications` and "
    "`included_applications` of their .app files.\n"
    "**PLT files are named \"<prefix>_<otp_release>_plt\".\n"
    "***The base PLT is a PLT containing the core applications often required "
    "for a project's PLT. One base PLT is created per OTP version and "
    "stored in `base_plt_location`. A base PLT is used to build project PLTs."
    "\n".

short_desc() ->
    "Run the Dialyzer analyzer on the project.".

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    maybe_fix_env(),
    ?INFO("Dialyzer starting, this may take a while...", []),
    code:add_pathsa(rebar_state:code_paths(State, all_deps)),
    Plt = get_plt(State),

    try
        do(State, Plt)
    catch
        throw:{dialyzer_error, Error} ->
            ?PRV_ERROR({error_processing_apps, Error});
        throw:{dialyzer_warnings, Warnings} ->
            ?PRV_ERROR({dialyzer_warnings, Warnings});
        throw:{unknown_application, _} = Error ->
            ?PRV_ERROR(Error);
        throw:{unknown_module, _} = Error ->
            ?PRV_ERROR(Error);
        throw:{duplicate_module, _, _, _} = Error ->
            ?PRV_ERROR(Error);
        throw:{output_file_error, _, _} = Error ->
            ?PRV_ERROR(Error);
        throw:{native_cache_error, _, _, _} = Error ->
            ?PRV_ERROR(Error);
        throw:{native_module_error, _, _, _, _} = Error ->
            ?PRV_ERROR(Error)
    after
        rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)),
        cleanup_native(State)
    end.

%% This is used to workaround dialyzer quirk discussed here
%% https://github.com/erlang/rebar3/pull/489#issuecomment-107953541
%% Dialyzer gets default plt location wrong way by peeking HOME environment
%% variable which usually is not defined on Windows.
maybe_fix_env() ->
    os:putenv("DIALYZER_PLT", filename:join(rebar_dir:home_dir(), ".dialyzer_plt")).

-spec format_error(any()) -> iolist().
format_error({error_processing_apps, Error}) ->
    io_lib:format("Error in dialyzing apps: ~s", [Error]);
format_error({dialyzer_warnings, Warnings}) ->
    io_lib:format("Warnings occurred running dialyzer: ~b", [Warnings]);
format_error({unknown_application, App}) ->
    io_lib:format("Could not find application: ~s", [App]);
format_error({unknown_module, Mod}) ->
    io_lib:format("Could not find module: ~s", [Mod]);
format_error({duplicate_module, Mod, File1, File2}) ->
    io_lib:format("Duplicates of module ~s: ~s ~s", [Mod, File1, File2]);
format_error({output_file_error, File, Error}) ->
    Error1 = file:format_error(Error),
    io_lib:format("Failed to write to ~s: ~s", [File, Error1]);
format_error({native_cache_error, Cache, Action, Error}) ->
    io_lib:format("Failed to ~s ~s: ~s", [Action, Cache, Error]);
format_error({native_module_error, Cache, Action, Mod, Error}) ->
    io_lib:format("Failed to ~s ~s with ~s: ~s", [Action, Mod, Cache, Error]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Internal functions

get_plt(State) ->
    Prefix = get_config(State, plt_prefix, ?PLT_PREFIX),
    Name = plt_name(Prefix),
    case get_config(State, plt_location, local) of
        local ->
            BaseDir = rebar_dir:base_dir(State),
            filename:join(BaseDir, Name);
        Dir ->
            filename:join(Dir, Name)
    end.

plt_name(Prefix) ->
    Prefix ++ "_" ++ rebar_utils:otp_release() ++ "_plt".

do(State, Plt) ->
    native(State),
    Output = get_output_file(State),
    {PltWarnings, State1} = update_proj_plt(State, Plt, Output),
    {Warnings, State2} = succ_typings(State1, Plt, Output),
    case PltWarnings + Warnings of
        0 ->
            {ok, State2};
        TotalWarnings ->
            ?INFO("Warnings written to ~s", [Output]),
            throw({dialyzer_warnings, TotalWarnings})
    end.

native(State) ->
    case get_config(State, native, true) andalso is_native_available() of
        true ->
            load_native(State);
        false ->
            ok
    end.

is_native_available() ->
    case erlang:system_info(hipe_architecture) of
        undefined ->
            false;
        _ ->
            true
    end.

load_native(State) ->
    Natives = lookup_native_mods(),
    Cache = native_location(State),
    case zip:unzip(Cache, [memory]) of
        {ok, Files} ->
            load_native(Files, Natives, Cache);
        {error, enoent} ->
            compile_native(Natives, Cache);
        {error, Reason} ->
            ?DEBUG("Deleting native cache: ~p", [Cache]),
            _ = file:delete(Cache),
            throw({native_cache_error, Cache, unzip, Reason})
    end.

lookup_native_mods() ->
    ?INFO("Resolving native modules...", []),
    rebar_utils:filtermap(fun lookup_native_mod/1, native_mods()).

lookup_native_mod(Mod) ->
    _ = code:ensure_loaded(Mod),
    case code:is_module_native(Mod) of
        true ->
            false;
        false ->
            {true, {Mod, code:which(Mod)}};
        undefined ->
            false
    end.

%% As used by dialyzer in OTP-19.1
native_mods() ->
    [lists, dict, digraph, digraph_utils, ets,
     cerl, erl_types, cerl_trees, erl_bif_types,
     dialyzer_analysis_callgraph, dialyzer, dialyzer_behaviours,
     dialyzer_codeserver, dialyzer_contracts,
     dialyzer_coordinator, dialyzer_dataflow, dialyzer_dep,
     dialyzer_plt, dialyzer_succ_typings, dialyzer_typesig,
     dialyzer_worker].

native_location(State) ->
    Name = rebar_utils:otp_release() ++ ".dialyzer_modules",
    case get_config(State, native_location, global) of
        global ->
            GlobalCacheDir = rebar_dir:global_cache_dir(rebar_state:opts(State)),
            filename:join(GlobalCacheDir, Name);
        Dir ->
            filename:join(Dir, Name)
    end.

load_native(Files, Natives, Cache) ->
    Ext = code:objfile_extension(),
    NFiles = [{list_to_atom(filename:basename(File, Ext)), File, Binary} ||
              {File, Binary} <- Files],
    ?DEBUG("Native cache: ~p", [[{Mod, File} || {Mod, File, _} <- NFiles]]),
    NFiles2 = [FileInfo ||
               {Mod, _, _} = FileInfo <- NFiles,
               lists:keymember(Mod, 1, Natives)],
    ?INFO("Loading ~b native modules from ~p...", [length(NFiles2), Cache]),
    ?DEBUG("Loading native modules: ~p", [[{Mod, File} || {Mod, File, _} <- NFiles2]]),
    load_native_mods(Files, Cache).

load_native_mods(Files, Cache) ->
    _ = [load_native_mod(Mod, File, Binary, Cache) ||
         {Mod, File, Binary} <- Files],
    ok.

load_native_mod(Mod, File, Binary, Cache) ->
    case code:is_sticky(Mod) of
        true ->
            sticky_native_load(Mod, File, Binary, Cache);
        false ->
            load_native_binary(Mod, File, Binary, Cache)
    end.

sticky_native_load(Mod, File, Binary, Cache) ->
    Dir = filename:dirname(code:which(Mod)),
    try
        _ = code:unstick_dir(Dir),
        load_native_binary(Mod, File, Binary, Cache)
    after
        _ = code:stick_dir(Dir)
    end.

load_native_binary(Mod, File, Binary, Cache) ->
    case code:load_binary(Mod, File, Binary) of
        {module, _} ->
            code:purge(Mod);
        {error, Reason} ->
            ?DEBUG("Deleting native cache: ~p", [Cache]),
            _ = file:delete(Cache),
            throw({native_module_error, Cache, load, Mod, Reason})
    end.

compile_native(Files, Cache) ->
    ?INFO("Compiling ~b native modules to ~p...", [length(Files), Cache]),
    ?DEBUG("Native cache: ~p", [Files]),
    NFiles = compile_native_mods(Files, Cache),
    case zip:zip(Cache, NFiles) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            throw({native_cache_error, Cache, zip, Reason})
    end.

compile_native_mods(Files, Cache) ->
    [compile_native_mod(Mod, File, Cache) || {Mod, File} <- Files].

compile_native_mod(Mod, File, Cache) ->
    case compile:file(File, [from_beam, native, binary]) of
        {ok, Mod, Binary} ->
            Dir = filename:dirname(Cache),
            NFile = filename:join(Dir, filename:basename(File)),
            load_native_mod(Mod, NFile, Binary, Cache),
            {NFile, Binary};
        Reason ->
            throw({native_module_error, Cache, compile, Mod, Reason})
    end.

cleanup_native(State) ->
    case get_config(State, native, true) andalso is_native_available() of
        true ->
            _ = [cleanup_native_mod(Mod) || Mod <- native_mods()],
            ok;
        false ->
            ok
    end.

cleanup_native_mod(Mod) ->
    case code:where_is_file(atom_to_list(Mod) ++ code:objfile_extension()) of
        File when is_list(File) ->
            ensure_mod(Mod, File);
        non_existing ->
            false
    end.

ensure_mod(Mod, File) ->
    case code:which(Mod) of
        File ->
            false;
        _ ->
            load_mod(Mod, File)
    end.

load_mod(Mod, File) ->
    case code:is_sticky(Mod) of
        true ->
            sticky_load_mod(Mod, File);
        false ->
            do_load_mod(Mod, File)
    end.

sticky_load_mod(Mod, File) ->
    Dir = filename:dirname(File),
    try
        _ = code:unstick_dir(Dir),
        do_load_mod(Mod, File)
    after
        _ = code:stick_dir(Dir)
    end.

do_load_mod(Mod, File) ->
    {module, Mod} = code:load_abs(filename:rootname(File)),
    code:purge(Mod).

get_output_file(State) ->
    BaseDir = rebar_dir:base_dir(State),
    Output = filename:join(BaseDir, default_output_file()),
    case file:open(Output, [write]) of
        {ok, File} ->
            ok = file:close(File),
            Output;
        {error, Reason} ->
            throw({output_file_error, Output, Reason})
    end.

default_output_file() ->
    rebar_utils:otp_release() ++ ".dialyzer_warnings".

update_proj_plt(State, Plt, Output) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(update_plt, Args) of
        false ->
            {0, State};
        _ ->
            do_update_proj_plt(State, Plt, Output)
    end.

do_update_proj_plt(State, Plt, Output) ->
    ?INFO("Updating plt...", []),
    Files = proj_plt_files(State),
    case read_plt(State, Plt) of
        {ok, OldFiles} ->
            check_plt(State, Plt, Output, OldFiles, Files);
        error ->
            build_proj_plt(State, Plt, Output, Files)
    end.

proj_plt_files(State) ->
    BasePltApps = base_plt_apps(State),
    PltApps = get_config(State, plt_extra_apps, []) ++ BasePltApps,
    BasePltMods = get_config(State, base_plt_mods, []),
    PltMods = get_config(State, plt_extra_mods, []) ++ BasePltMods,
    Apps = proj_apps(State),
    DepApps = proj_deps(State),
    get_files(State, DepApps ++ PltApps, Apps -- PltApps, PltMods, []).

proj_apps(State) ->
    [ec_cnv:to_atom(rebar_app_info:name(App)) ||
     App <- rebar_state:project_apps(State)].

proj_deps(State) ->
    Apps = rebar_state:project_apps(State),
    DepApps = lists:flatmap(fun rebar_app_info:applications/1, Apps),
    case get_config(State, plt_apps, top_level_deps) of
        top_level_deps -> DepApps;
        all_deps       -> collect_nested_dependent_apps(DepApps)
    end.

get_files(State, Apps, SkipApps, Mods, SkipMods) ->
    ?INFO("Resolving files...", []),
    ExcludeApps = get_config(State, exclude_apps, []),
    Files = apps_files(Apps, ExcludeApps ++ SkipApps, dict:new()),
    ExcludeMods = get_config(State, exclude_mods, []),
    Files2 = mods_files(Mods, ExcludeMods ++ SkipMods, Files),
    dict:fold(fun(_, File, Acc) -> [File | Acc] end, [], Files2).

apps_files([], _, Files) ->
    Files;
apps_files([AppName | DepApps], SkipApps, Files) ->
    case lists:member(AppName, SkipApps) of
        true ->
            apps_files(DepApps, SkipApps, Files);
        false ->
            AppFiles = app_files(AppName),
            ?DEBUG("~s modules: ~p", [AppName, dict:fetch_keys(AppFiles)]),
            Files2 = merge_files(Files, AppFiles),
            apps_files(DepApps, [AppName | SkipApps], Files2)
    end.

app_files(AppName) ->
    case app_ebin(AppName) of
        {ok, EbinDir} ->
            ebin_files(EbinDir);
        {error, bad_name} ->
            throw({unknown_application, AppName})
    end.

app_ebin(AppName) ->
    case code:lib_dir(AppName, ebin) of
        {error, bad_name} = Error ->
            Error;
        EbinDir ->
            check_ebin(EbinDir)
    end.

check_ebin(EbinDir) ->
    case filelib:is_dir(EbinDir) of
        true ->
            {ok, EbinDir};
        false ->
            {error, bad_name}
    end.

ebin_files(EbinDir) ->
    Ext = code:objfile_extension(),
    Wildcard = "*" ++ Ext,
    Files = filelib:wildcard(Wildcard, EbinDir),
    Store = fun(File, Mods) ->
                    Mod = list_to_atom(filename:basename(File, Ext)),
                    Absname = filename:join(EbinDir, File),
                    dict:store(Mod, Absname, Mods)
            end,
    lists:foldl(Store, dict:new(), Files).

merge_files(Files1, Files2) ->
    Duplicate = fun(Mod, File1, File2) ->
                        throw({duplicate_module, Mod, File1, File2})
               end,
    dict:merge(Duplicate, Files1, Files2).

mods_files(Mods, SkipMods, Files) ->
    Keep = fun(File) -> File end,
    Ensure = fun(Mod, Acc) ->
                     case lists:member(Mod, SkipMods) of
                         true ->
                             Acc;
                         false ->
                             dict:update(Mod, Keep, mod_file(Mod), Acc)
                     end
          end,
    Files2 = lists:foldl(Ensure, Files, Mods),
    lists:foldl(fun dict:erase/2, Files2, SkipMods).

mod_file(Mod) ->
    File = atom_to_list(Mod) ++ code:objfile_extension(),
    case code:where_is_file(File) of
        non_existing -> throw({unknown_module, Mod});
        Absname      -> Absname
    end.

read_plt(_State, Plt) ->
    Vsn = dialyzer_version(),
    case plt_files(Plt) of
        {ok, Files} when Vsn < {2, 9, 0} ->
            % Before dialyzer-2.9 (OTP 18.3) removing a beam file from the PLT
            % that no longer exists would crash. Therefore force a rebuild of
            % PLT if any files no longer exist.
            read_plt_files(Plt, Files);
        {ok, _} = Result when Vsn >= {2, 9, 0} ->
            Result;
        {error, no_such_file} ->
            error;
        {error, not_valid} ->
            error;
        {error, read_error} ->
            Error = io_lib:format("Could not read the PLT file ~p", [Plt]),
            throw({dialyzer_error, Error})
    end.

plt_files(Plt) ->
    case dialyzer:plt_info(Plt) of
        {ok, Info} ->
            {ok, proplists:get_value(files, Info, [])};
        {error, _} = Error ->
            Error
    end.

%% If any file no longer exists dialyzer will fail when updating the PLT.
read_plt_files(Plt, Files) ->
    case [File || File <- Files, not filelib:is_file(File)] of
        [] ->
            {ok, Files};
        Missing ->
            ?INFO("Could not find ~p files in ~p...", [length(Missing), Plt]),
            ?DEBUG("Could not find files: ~p", [Missing]),
            error
    end.

check_plt(State, Plt, Output, OldList, FilesList) ->
    Old = sets:from_list(OldList),
    Files = sets:from_list(FilesList),
    Remove = sets:to_list(sets:subtract(Old, Files)),
    {RemWarnings, State1} = remove_plt(State, Plt, Output, Remove),
    Check = sets:to_list(sets:intersection(Files, Old)),
    {CheckWarnings, State2} = check_plt(State1, Plt, Output, Check),
    Add = sets:to_list(sets:subtract(Files, Old)),
    {AddWarnings, State3} = add_plt(State2, Plt, Output, Add),
    {RemWarnings + CheckWarnings + AddWarnings, State3}.

remove_plt(State, _Plt, _Output, []) ->
    {0, State};
remove_plt(State, Plt, Output, Files) ->
    ?INFO("Removing ~b files from ~p...", [length(Files), Plt]),
    run_plt(State, Plt, Output, plt_remove, Files).

check_plt(State, _Plt, _Output, []) ->
    {0, State};
check_plt(State, Plt, Output, Files) ->
    ?INFO("Checking ~b files in ~p...", [length(Files), Plt]),
    run_plt(State, Plt, Output, plt_check, Files).

add_plt(State, _Plt, _Output, []) ->
    {0, State};
add_plt(State, Plt, Output, Files) ->
    ?INFO("Adding ~b files to ~p...", [length(Files), Plt]),
    run_plt(State, Plt, Output, plt_add, Files).

run_plt(State, Plt, Output, Analysis, Files) ->
    GetWarnings = get_config(State, get_warnings, false),
    Opts = [{analysis_type, Analysis},
            {get_warnings, GetWarnings},
            {init_plt, Plt},
            {output_plt, Plt},
            {from, byte_code},
            {files, Files}],
    run_dialyzer(State, Opts, Output).

build_proj_plt(State, Plt, Output, Files) ->
    BasePlt = get_base_plt(State),
    ?INFO("Updating base plt...", []),
    BaseFiles = base_plt_files(State),
    {BaseWarnings, State1} = update_base_plt(State, BasePlt, Output, BaseFiles),
    ?INFO("Copying ~p to ~p...", [BasePlt, Plt]),
    _ = filelib:ensure_dir(Plt),
    case file:copy(BasePlt, Plt) of
        {ok, _} ->
            {CheckWarnings, State2} = check_plt(State1, Plt, Output, BaseFiles,
                                                Files),
            {BaseWarnings + CheckWarnings, State2};
        {error, Reason} ->
            Error = io_lib:format("Could not copy PLT from ~p to ~p: ~p",
                                  [BasePlt, Plt, file:format_error(Reason)]),
            throw({dialyzer_error, Error})
    end.

get_base_plt(State) ->
    Prefix = get_config(State, base_plt_prefix, ?PLT_PREFIX),
    Name = plt_name(Prefix),
    case get_config(State, base_plt_location, global) of
        global ->
            GlobalCacheDir = rebar_dir:global_cache_dir(rebar_state:opts(State)),
            filename:join(GlobalCacheDir, Name);
        Dir ->
            filename:join(Dir, Name)
    end.

base_plt_files(State) ->
    BasePltApps = base_plt_apps(State),
    BasePltMods = get_config(State, base_plt_mods, []),
    get_files(State, BasePltApps, [], BasePltMods, []).

base_plt_apps(State) ->
    get_config(State, base_plt_apps, [erts, crypto, kernel, stdlib]).

update_base_plt(State, BasePlt, Output, BaseFiles) ->
    case read_plt(State, BasePlt) of
        {ok, OldBaseFiles} ->
            check_plt(State, BasePlt, Output, OldBaseFiles, BaseFiles);
        error ->
            _ = filelib:ensure_dir(BasePlt),
            build_plt(State, BasePlt, Output, BaseFiles)
    end.

build_plt(State, Plt, _, []) ->
    ?INFO("Building with no files in ~p...", [Plt]),
    Opts = [{get_warnings, false},
            {output_plt, Plt},
            {apps, [erts]}],
    % Create a PLT with erts files and then remove erts files to be left with an
    % empty PLT. Dialyzer will crash when trying to build a PLT with an empty
    % file list.
    _ = dialyzer:run([{analysis_type, plt_build} | Opts]),
    _ = dialyzer:run([{analysis_type, plt_remove}, {init_plt, Plt} | Opts]),
    {0, State};
build_plt(State, Plt, Output, Files) ->
    ?INFO("Building with ~b files in ~p...", [length(Files), Plt]),
    GetWarnings = get_config(State, get_warnings, false),
    Opts = [{analysis_type, plt_build},
            {get_warnings, GetWarnings},
            {output_plt, Plt},
            {files, Files}],
    run_dialyzer(State, Opts, Output).

succ_typings(State, Plt, Output) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(succ_typings, Args) of
        false ->
            {0, State};
        _ ->
            ?INFO("Doing success typing analysis...", []),
            Files = proj_files(State),
            succ_typings(State, Plt, Output, Files)
    end.

succ_typings(State, Plt, _, []) ->
    ?INFO("Analyzing no files with ~p...", [Plt]),
    {0, State};
succ_typings(State, Plt, Output, Files) ->
    ?INFO("Analyzing ~b files with ~p...", [length(Files), Plt]),
    Opts = [{analysis_type, succ_typings},
            {get_warnings, true},
            {from, byte_code},
            {files, Files},
            {init_plt, Plt}],
    run_dialyzer(State, Opts, Output).

proj_files(State) ->
    Apps = proj_apps(State),
    BasePltApps = get_config(State, base_plt_apps, []),
    PltApps = get_config(State, plt_extra_apps, []) ++ BasePltApps,
    BasePltMods = get_config(State, base_plt_mods, []),
    PltMods = get_config(State, plt_extra_mods, []) ++ BasePltMods,
    get_files(State, Apps, PltApps, [], PltMods).

run_dialyzer(State, Opts, Output) ->
    %% dialyzer may return callgraph warnings when get_warnings is false
    case proplists:get_bool(get_warnings, Opts) of
        true ->
            WarningsList = get_config(State, warnings, []),
            Opts2 = [{warnings, legacy_warnings(WarningsList)},
                     {check_plt, false} |
                     Opts],
            ?DEBUG("Running dialyzer with options: ~p~n", [Opts2]),
            Warnings = format_warnings(rebar_state:opts(State),
                                       Output, dialyzer:run(Opts2)),
            {Warnings, State};
        false ->
            Opts2 = [{warnings, no_warnings()},
                     {check_plt, false} |
                     Opts],
            ?DEBUG("Running dialyzer with options: ~p~n", [Opts2]),
            dialyzer:run(Opts2),
            {0, State}
    end.

legacy_warnings(Warnings) ->
    case dialyzer_version() of
       TupleVsn when TupleVsn < {2, 8, 0} ->
            [Warning || Warning <- Warnings, Warning =/= unknown];
        _ ->
            Warnings
    end.

format_warnings(Opts, Output, Warnings) ->
    Warnings1 = rebar_dialyzer_format:format_warnings(Opts, Warnings),
    console_warnings(Warnings1),
    file_warnings(Output, Warnings),
    length(Warnings).

console_warnings(Warnings) ->
    _ = [?CONSOLE("~s", [Warning]) || Warning <- Warnings],
    ok.

file_warnings(_, []) ->
    ok;
file_warnings(Output, Warnings) ->
    Warnings1 = [[dialyzer:format_warning(Warning, fullpath), $\n] || Warning <- Warnings],
    case file:write_file(Output, Warnings1, [append]) of
        ok ->
            ok;
        {error, Reason} ->
            throw({output_file_error, Output, Reason})
    end.

no_warnings() ->
    [no_return,
     no_unused,
     no_improper_lists,
     no_fun_app,
     no_match,
     no_opaque,
     no_fail_call,
     no_contracts,
     no_behaviours,
     no_undefined_callbacks].

get_config(State, Key, Default) ->
    Config = rebar_state:get(State, dialyzer, []),
    proplists:get_value(Key, Config, Default).

-spec collect_nested_dependent_apps([atom()]) -> [atom()].
collect_nested_dependent_apps(RootApps) ->
    Deps = lists:foldl(fun collect_nested_dependent_apps/2, sets:new(), RootApps),
    sets:to_list(Deps).

-spec collect_nested_dependent_apps(atom(), rebar_set()) -> rebar_set().
collect_nested_dependent_apps(App, Seen) ->
    case sets:is_element(App, Seen) of
        true ->
            Seen;
        false ->
            Seen1 = sets:add_element(App, Seen),
            case code:lib_dir(App) of
                {error, _} ->
                    throw({unknown_application, App});
                AppDir ->
                    case rebar_app_discover:find_app(AppDir, all) of
                        false ->
                            throw({unknown_application, App});
                        {true, AppInfo}  ->
                            lists:foldl(fun collect_nested_dependent_apps/2,
                                        Seen1,
                                        rebar_app_info:applications(AppInfo))
                    end
            end
    end.

dialyzer_version() ->
    _ = application:load(dialyzer),
    {ok, Vsn} = application:get_key(dialyzer, vsn),
    case string:tokens(Vsn, ".") of
        [Major, Minor] ->
            version_tuple(Major, Minor, "0");
        [Major, Minor, Patch | _] ->
            version_tuple(Major, Minor, Patch)
    end.

version_tuple(Major, Minor, Patch) ->
    {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch)}.
