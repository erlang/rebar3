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
            {succ_typings, $s, "succ-typings", boolean, "Enable success typing analysis. Default: true"},
            {base_plt_location, undefined, "base-plt-location", string, "The location of base PLT file, defaults to $HOME/.cache/rebar3"},
            {plt_location, undefined, "plt-location", string, "The location of the PLT file, defaults to the profile's base directory"},
            {plt_prefix, undefined, "plt-prefix", string, "The prefix to the PLT file, defaults to \"rebar3\"" },
            {app, $a, "app", string, "Perform success typing analysis of a single application"},
            {base_plt_prefix, undefined, "base-plt-prefix", string, "The prefix to the base PLT file, defaults to \"rebar3\"" },
            {statistics, undefined, "statistics", boolean, "Print information about the progress of execution. Default: false" }],
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
    "`plt_apps` - the strategy for determining the applications which included "
    "in the PLT file, `top_level_deps` to include just the direct dependencies "
    "or `all_deps` to include all nested dependencies "
    "or `all_apps` to include all project apps and nested dependencies*\n"
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
    "$HOME/.cache/rebar3 (default) or  a custom directory***\n"
    "`base_plt_prefix` - the prefix to the base PLT file, defaults to "
    "\"rebar3\"** ***\n"
    "`exclude_apps` - a list of applications to exclude from PLT files and "
    "success typing analysis, `plt_extra_mods` and `base_plt_mods` can add "
    "modules from excluded applications\n"
    "`exclude_mods` - a list of modules to exclude from PLT files and "
    "success typing analysis\n"
    "`output_format` - configure whether the dialyzer_warnings file will have "
    "the `raw` or `formatted` output\n"
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
    rebar_paths:unset_paths([plugins], State), % no plugins in analysis
    rebar_paths:set_paths([deps], State),
    {Args, _} = rebar_state:command_parsed_args(State),
    Plt = get_plt(Args, State),

    try
        do(Args, State, Plt)
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
            ?PRV_ERROR(Error)
    after
        rebar_paths:set_paths([plugins,deps], State)
    end.

%% This is used to workaround dialyzer quirk discussed here
%% https://github.com/erlang/rebar3/pull/489#issuecomment-107953541
%% Dialyzer gets default plt location wrong way by peeking HOME environment
%% variable which usually is not defined on Windows.
maybe_fix_env() ->
    os:putenv("DIALYZER_PLT", filename:join(rebar_dir:home_dir(), ".dialyzer_plt")).

-spec format_error(any()) -> iolist().
format_error({error_processing_apps, Error}) ->
    io_lib:format("Error in dialyzing apps: ~ts", [Error]);
format_error({dialyzer_warnings, Warnings}) ->
    io_lib:format("Warnings occurred running dialyzer: ~b", [Warnings]);
format_error({unknown_application, App}) ->
    io_lib:format("Could not find application: ~ts", [App]);
format_error({unknown_module, Mod}) ->
    io_lib:format("Could not find module: ~ts", [Mod]);
format_error({duplicate_module, Mod, File1, File2}) ->
    io_lib:format("Duplicates of module ~ts: ~ts ~ts", [Mod, File1, File2]);
format_error({output_file_error, File, Error}) ->
    Error1 = file:format_error(Error),
    io_lib:format("Failed to write to ~ts: ~ts", [File, Error1]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Internal functions

get_plt(Args, State) ->
    Prefix = proplists:get_value(plt_prefix, Args, get_config(State, plt_prefix, ?PLT_PREFIX)),
    Name = plt_name(Prefix),
    case proplists:get_value(plt_location, Args, get_config(State, plt_location, local)) of
        local ->
            BaseDir = rebar_dir:base_dir(State),
            filename:join(BaseDir, Name);
        Dir ->
            filename:join(Dir, Name)
    end.

plt_name(Prefix) ->
    Prefix ++ "_" ++ rebar_utils:otp_release() ++ "_plt".

do(Args, State, Plt) ->
    Output = get_output_file(State),
    case debug_info(State) of
        true ->
            ok;
        false ->
            ?WARN("Add debug_info to compiler options (erl_opts) "
                  "if Dialyzer fails to load Core Erlang.", [])
    end,
    {PltWarnings, State1} = update_proj_plt(Args, State, Plt, Output),
    {Warnings, State2} = succ_typings(Args, State1, Plt, Output),
    case PltWarnings + Warnings of
        0 ->
            {ok, State2};
        TotalWarnings ->
            ?INFO("Warnings written to ~ts", [rebar_dir:format_source_file_name(Output)]),
            throw({dialyzer_warnings, TotalWarnings})
    end.

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

update_proj_plt(Args, State, Plt, Output) ->
    case proplists:get_value(update_plt, Args) of
        false ->
            {0, State};
        _ ->
            do_update_proj_plt(Args, State, Plt, Output)
    end.

do_update_proj_plt(Args, State, Plt, Output) ->
    ?INFO("Updating plt...", []),
    Files = proj_plt_files(State),
    case read_plt(State, Plt) of
        {ok, OldFiles} ->
            check_plt(State, Plt, Output, OldFiles, Files);
        error ->
            build_proj_plt(Args, State, Plt, Output, Files)
    end.

proj_plt_files(State) ->
    BasePltApps = base_plt_apps(State),
    PltApps = get_config(State, plt_extra_apps, []) ++ BasePltApps,
    BasePltMods = get_config(State, base_plt_mods, []),
    PltMods = get_config(State, plt_extra_mods, []) ++ BasePltMods,
    DepApps = lists:usort(proj_plt_apps(State) ++ PltApps),
    get_files(State, DepApps, [], PltMods, [], []).

proj_apps(State) ->
    [ec_cnv:to_atom(rebar_app_info:name(App)) ||
     App <- rebar_state:project_apps(State)].

proj_plt_apps(State) ->
    Apps = rebar_state:project_apps(State),
    DepApps = lists:flatmap(
               fun(App) ->
                       rebar_app_info:applications(App) ++
                           rebar_app_info:included_applications(App)
               end, Apps),
    ProjApps = proj_apps(State),
    case get_config(State, plt_apps, top_level_deps) of
        top_level_deps ->
            DepApps -- ProjApps;
        all_deps       ->
            collect_nested_dependent_apps(DepApps, State) -- ProjApps;
        all_apps       ->
            proj_apps(State) ++ collect_nested_dependent_apps(DepApps, State)
    end.

get_files(State, Apps, SkipApps, Mods, SkipMods, ExtraDirs) ->
    ?INFO("Resolving files...", []),
    ExcludeApps = get_config(State, exclude_apps, []),
    Files0 = apps_files(Apps, ExcludeApps ++ SkipApps, ExtraDirs, dict:new()),
    BaseDir = filename:join(rebar_dir:base_dir(State), "extras"),
    Files1 = extras_files(BaseDir, ExtraDirs, Files0),
    ExcludeMods = get_config(State, exclude_mods, []),
    Files2 = mods_files(Mods, ExcludeMods ++ SkipMods, Files1),
    dict:fold(fun(_, File, Acc) -> [File | Acc] end, [], Files2).

apps_files([], _, _ExtraDirs, Files) ->
    Files;
apps_files([AppName | DepApps], SkipApps, ExtraDirs, Files) ->
    case lists:member(AppName, SkipApps) of
        true ->
            apps_files(DepApps, SkipApps, ExtraDirs, Files);
        false ->
            AppFiles = app_files(AppName, ExtraDirs),
            ?DEBUG("~ts modules: ~p", [AppName, dict:fetch_keys(AppFiles)]),
            Files2 = merge_files(Files, AppFiles),
            apps_files(DepApps, [AppName | SkipApps], ExtraDirs, Files2)
    end.

app_files(AppName, ExtraDirs) ->
    case app_ebin(AppName) of
        {ok, EbinDir} ->
            merge_files(ebin_files(EbinDir), extra_files(AppName, ExtraDirs));
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

extras_files(_BaseDir, [], Acc) ->
    Acc;
extras_files(BaseDir, [ExtraDir | Rest], Acc) ->
    Files = ebin_files(filename:join(BaseDir, ExtraDir)),
    extras_files(BaseDir, Rest, merge_files(Acc, Files)).

extra_files(AppName, ExtraDirs) ->
    lists:foldl(
        fun(ExtraDir, Files) ->
            merge_files(Files, ebin_files(filename:join(code:lib_dir(AppName), ExtraDir)))
        end,
        dict:new(),
        ExtraDirs
    ).

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
            Error = io_lib:format("Could not read the PLT file ~ts", [format_path(Plt)]),
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
            ?INFO("Could not find ~p files in ~ts...", [length(Missing), format_path(Plt)]),
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
    ?INFO("Removing ~b files from ~ts...", [length(Files), format_path(Plt)]),
    run_plt(State, Plt, Output, plt_remove, Files).

check_plt(State, _Plt, _Output, []) ->
    {0, State};
check_plt(State, Plt, Output, Files) ->
    ?INFO("Checking ~b files in ~ts...", [length(Files), format_path(Plt)]),
    run_plt(State, Plt, Output, plt_check, Files).

add_plt(State, _Plt, _Output, []) ->
    {0, State};
add_plt(State, Plt, Output, Files) ->
    ?INFO("Adding ~b files to ~ts...", [length(Files), format_path(Plt)]),
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

build_proj_plt(Args, State, Plt, Output, Files) ->
    BasePlt = get_base_plt(Args, State),
    ?INFO("Updating base plt...", []),
    BaseFiles = base_plt_files(State),
    {BaseWarnings, State1} = update_base_plt(State, BasePlt, Output, BaseFiles),
    ?INFO("Copying ~ts to ~ts...", [format_path(BasePlt), format_path(Plt)]),
    _ = filelib:ensure_dir(Plt),
    case file:copy(BasePlt, Plt) of
        {ok, _} ->
            {CheckWarnings, State2} = check_plt(State1, Plt, Output, BaseFiles,
                                                Files),
            {BaseWarnings + CheckWarnings, State2};
        {error, Reason} ->
            Error = io_lib:format("Could not copy PLT from ~ts to ~ts: ~ts",
                                  [BasePlt, Plt, file:format_error(Reason)]),
            throw({dialyzer_error, Error})
    end.

get_base_plt(Args, State) ->
    Prefix = proplists:get_value(base_plt_prefix, Args, get_config(State, base_plt_prefix, ?PLT_PREFIX)),
    Name = plt_name(Prefix),
    case proplists:get_value(base_plt_location, Args, get_config(State, base_plt_location, global)) of
        global ->
            GlobalCacheDir = rebar_dir:global_cache_dir(rebar_state:opts(State)),
            filename:join(GlobalCacheDir, Name);
        Dir ->
            filename:join(Dir, Name)
    end.

base_plt_files(State) ->
    BasePltApps = base_plt_apps(State),
    BasePltMods = get_config(State, base_plt_mods, []),
    get_files(State, BasePltApps, [], BasePltMods, [], []).

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
    ?INFO("Building with no files in ~ts...", [format_path(Plt)]),
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
    ?INFO("Building with ~b files in ~ts...", [length(Files), format_path(Plt)]),
    GetWarnings = get_config(State, get_warnings, false),
    Opts = [{analysis_type, plt_build},
            {get_warnings, GetWarnings},
            {output_plt, Plt},
            {files, Files}],
    run_dialyzer(State, Opts, Output).

succ_typings(Args, State, Plt, Output) ->
    case proplists:get_value(succ_typings, Args) of
        false ->
            {0, State};
        _ ->
            ?INFO("Doing success typing analysis...", []),
            Files = proj_files(proplists:get_value(app, Args), State),
            succ_typings_(State, Plt, Output, Files)
    end.

succ_typings_(State, Plt, _, []) ->
    ?INFO("Analyzing no files with ~ts...", [format_path(Plt)]),
    {0, State};
succ_typings_(State, Plt, Output, Files) ->
    ?INFO("Analyzing ~b files with ~ts...", [length(Files), format_path(Plt)]),
    Opts = [{analysis_type, succ_typings},
            {get_warnings, true},
            {from, byte_code},
            {files, Files},
            {init_plt, Plt}],
    run_dialyzer(State, Opts, Output).

succ_typing_apps(undefined, ProjApps) ->
    ProjApps;
succ_typing_apps(App, ProjApps) ->
    try
        true = lists:member(ec_cnv:to_atom(App), ProjApps),
        [ec_cnv:to_atom(App)]
    catch
        error:_ ->
            throw({unknown_application, App})
    end.

proj_files(SingleApp, State) ->
    Apps = succ_typing_apps(SingleApp, proj_apps(State)),
    BasePltApps = get_config(State, base_plt_apps, []),
    PltApps = get_config(State, plt_extra_apps, []) ++ BasePltApps,
    BasePltMods = get_config(State, base_plt_mods, []),
    PltMods = get_config(State, plt_extra_mods, []) ++ BasePltMods,
    ExtraDirs = rebar_dir:extra_src_dirs(rebar_state:opts(State)),
    get_files(State, Apps, PltApps, [], PltMods, ExtraDirs).

run_dialyzer(State, Opts, Output) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    %% dialyzer uses command-line option `--statistics` for enabling
    %% additional info about progress of execution, but internally
    %% this option has name `timing`.
    %% NOTE: Option `timing` accept boolean() or 'debug', but here we support
    %% only boolean().
    Timing = proplists:get_bool(statistics, Args),
    %% dialyzer may return callgraph warnings when get_warnings is false
    case proplists:get_bool(get_warnings, Opts) of
        true ->
            WarningsList = get_config(State, warnings, []),
            Opts2 = [{warnings, legacy_warnings(WarningsList)},
                     {check_plt, false},
                     {timing, Timing} |
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
    Config = rebar_opts:get(Opts, dialyzer, []),
    OutputFormat = proplists:get_value(output_format, Config, formatted),
    file_warnings(Output, Warnings, OutputFormat),
    length(Warnings).

console_warnings(Warnings) ->
    _ = [?CONSOLE("~ts", [Warning]) || Warning <- Warnings],
    ok.

file_warnings(_, [], _) ->
    ok;
file_warnings(Output, Warnings, raw) ->
    Warnings1 = [[io_lib:format("~tp.\n", [W]) || W <- Warnings]],
    write_file_warnings(Output, Warnings1);
file_warnings(Output, Warnings, formatted) ->
    Warnings1 = [[dialyzer:format_warning(Warning, fullpath), $\n]
                 || Warning <- Warnings],
    write_file_warnings(Output, Warnings1).

write_file_warnings(Output, Warnings) ->
    case file:write_file(Output, Warnings, [append]) of
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

debug_info(State) ->
    Config = rebar_state:get(State, erl_opts, []),
    proplists:get_value(debug_info, Config, false) =/= false orelse
    proplists:get_value(debug_info_key, Config, false) =/= false orelse
    proplists:get_value(encrypt_debug_info, Config, false) =/= false.

-spec collect_nested_dependent_apps([atom()], rebar_state:t()) -> [atom()].
collect_nested_dependent_apps(RootApps, State) ->
    Deps = collect_nested_dependent_apps(RootApps, sets:new(), State),
    sets:to_list(Deps).

-spec collect_nested_dependent_apps([atom()], rebar_set(), rebar_state:t()) -> rebar_set().
collect_nested_dependent_apps(RootApps, Init, State) ->
    lists:foldl(
        fun (App, Seen) ->
                collect_nested_dependent_app(App, Seen, State)
        end,
        Init,
        RootApps
    ).


-spec collect_nested_dependent_app(atom(), rebar_set(), rebar_state:t()) -> rebar_set().
collect_nested_dependent_app(App, Seen, State) ->
    case sets:is_element(App, Seen) of
        true ->
            Seen;
        false ->
            Seen1 = sets:add_element(App, Seen),
            case code:lib_dir(App) of
                {error, _} ->
                    throw({unknown_application, App});
                AppDir ->
                    case rebar_app_discover:find_app(AppDir, all, State) of
                        false ->
                            throw({unknown_application, App});
                        {true, AppInfo}  ->
                            collect_nested_dependent_apps(rebar_app_info:applications(AppInfo), Seen1, State)
                    end
            end
    end.

dialyzer_version() ->
    _ = application:load(dialyzer),
    {ok, Vsn} = application:get_key(dialyzer, vsn),
    case rebar_string:lexemes(Vsn, ".") of
        [Major, Minor] ->
            version_tuple(Major, Minor, "0");
        [Major, Minor, Patch | _] ->
            version_tuple(Major, Minor, Patch)
    end.

version_tuple(Major, Minor, Patch) ->
    {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch)}.

format_path(Path) ->
    Normalized = rebar_dir:format_source_file_name(Path),
    case filelib:is_file(Normalized) of
        true -> Normalized;
        false -> rebar_dir:format_source_file_name(Path, abs_path_opts())
    end.

abs_path_opts() ->
    dict:from_list([{compiler_source_format, absolute}]).
