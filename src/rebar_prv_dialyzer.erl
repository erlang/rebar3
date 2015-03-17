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

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Opts = [{update_plt, $u, "update-plt", boolean, "Enable updating the PLT. Default: true"},
            {succ_typings, $s, "succ-typings", boolean, "Enable success typing analysis. Default: true"}],
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
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
    "The following (optional) configurations can be added to a rebar.config:\n"
    "`dialyzer_warnings` - a list of dialyzer warnings\n"
    "`dialyzer_plt_location` - the directory of the PLT file\n"
    "`dialyzer_plt_extra_apps` - a list of applications to include in the PLT file*\n"
    "`dialyzer_plt_warnings` - display warnings when updating a PLT file "
    "(boolean)\n"
    "`dialyzer_shared_plt_location` - the directory of the shared PLT**\n"
    "`dialyzer_shared_plt_apps` - a list of applications to include in the "
    "shared PLT file**\n"
    "\n"
    "*The applications in `dialyzer_shared_plt_apps` and any `applications` "
    "and included_applications` listed in their .app files will be added to "
    "list.\n"
    "**The shared PLT is a PLT containing the core OTP applications often "
    "required for a project's PLT. One shared PLT is created per OTP version "
    " and stored in `dialyzer_shared_plt_location` (defaults to "
    "$HOME/.cache/rebar3/). A shared PLT is used to create a project's initial "
    "PLT.".

short_desc() ->
    "Run the Dialyzer analyzer on the project.".

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?INFO("Dialyzer starting, this may take a while...", []),
    Plt = get_plt_location(State),

    try
        do(State, Plt)
    catch
        throw:{dialyzer_error, Error} ->
            ?PRV_ERROR({error_processing_apps, Error});
        throw:{dialyzer_warnings, Warnings} ->
            ?PRV_ERROR({dialyzer_warnings, Warnings})
    end.

-spec format_error(any()) -> iolist().
format_error({error_processing_apps, Error}) ->
    io_lib:format("Error in dialyzing apps: ~s", [Error]);
format_error({dialyzer_warnings, Warnings}) ->
    io_lib:format("Warnings occured running dialyzer: ~b", [Warnings]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Internal functions

get_plt_location(State) ->
    case rebar_state:get(State, dialyzer_plt_location, local) of
        local ->
            BaseDir = rebar_dir:base_dir(State),
            filename:join(BaseDir, plt_name());
        shared ->
            get_shared_plt_location(State);
        PltDir ->
            filename:join(PltDir, plt_name())
    end.

plt_name() ->
    rebar_utils:otp_release() ++ ".plt".

do(State, Plt) ->
    {PltWarnings, State1} = update_proj_plt(State, Plt),
    {Warnings, State2} = succ_typings(State1, Plt),
    case PltWarnings + Warnings of
        0 ->
            {ok, State2};
        TotalWarnings ->
            throw({dialyzer_warnings, TotalWarnings})
    end.

update_proj_plt(State, Plt) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(update_plt, Args) of
        false ->
            {0, State};
        _ ->
            do_update_proj_plt(State, Plt)
    end.

do_update_proj_plt(State, Plt) ->
    ?INFO("Updating plt...", []),
    {Files, Warnings} = proj_plt_files(State),
    Warnings2 = format_warnings(Warnings),
    {Warnings3, State2} = case read_plt(State, Plt) of
                              {ok, OldFiles} ->
                                  check_plt(State, Plt, OldFiles, Files);
                              {error, no_such_file} ->
                                  build_proj_plt(State, Plt, Files)
                          end,
    {Warnings2 + Warnings3, State2}.

proj_plt_files(State) ->
    SharedPltApps = rebar_state:get(State, dialyzer_shared_plt_apps,
                                    default_plt_apps()),
    ExtraPltApps = rebar_state:get(State, dialyzer_plt_extra_apps, []),
    Apps = rebar_state:project_apps(State),
    DepApps = lists:flatmap(fun rebar_app_info:applications/1, Apps),
    get_plt_files(SharedPltApps ++ ExtraPltApps ++ DepApps, Apps).

default_plt_apps() ->
    [erts,
     crypto,
     kernel,
     stdlib].

get_plt_files(DepApps, Apps) ->
    ?INFO("Resolving files...", []),
    get_plt_files(DepApps, Apps, [], [], []).

get_plt_files([], _, _, Files, Warnings) ->
    {Files, Warnings};
get_plt_files([AppName | DepApps], Apps, PltApps, Files, Warnings) ->
    case lists:member(AppName, PltApps) orelse app_member(AppName, Apps) of
        true ->
            get_plt_files(DepApps, Apps, PltApps, Files, Warnings);
        false ->
            {DepApps2, Files2, Warnings2} = app_name_to_info(AppName),
            ?DEBUG("~s dependencies: ~p", [AppName, DepApps2]),
            ?DEBUG("~s files: ~p", [AppName, Files2]),
            DepApps3 = DepApps2 ++ DepApps,
            PltApps2 = [AppName | PltApps],
            Files3 = Files2 ++ Files,
            Warnings3 = Warnings2 ++ Warnings,
            get_plt_files(DepApps3, Apps, PltApps2, Files3, Warnings3)
    end.

app_member(AppName, Apps) ->
    case rebar_app_utils:find(ec_cnv:to_binary(AppName), Apps) of
        {ok, _App} ->
            true;
        error ->
            false
    end.

app_name_to_info(AppName) ->
    case app_name_to_ebin(AppName) of
        {error, _} ->
            {[], [], [{unknown_application, {"", 0}, [AppName]}]};
        EbinDir ->
            ebin_to_info(EbinDir, AppName)
    end.

app_name_to_ebin(AppName) ->
    case code:lib_dir(AppName, ebin) of
        {error, bad_name} ->
            search_ebin(AppName);
        EbinDir ->
            check_ebin(EbinDir, AppName)
    end.

check_ebin(EbinDir, AppName) ->
    case filelib:is_dir(EbinDir) of
        true ->
            EbinDir;
        false ->
            search_ebin(AppName)
    end.

search_ebin(AppName) ->
    case code:where_is_file(atom_to_list(AppName) ++ ".app") of
        non_existing ->
            {error, bad_name};
        AppFile ->
            filename:dirname(AppFile)
    end.

ebin_to_info(EbinDir, AppName) ->
    AppFile = filename:join(EbinDir, atom_to_list(AppName) ++ ".app"),
    ?DEBUG("Consulting app file ~p", [AppFile]),
    case file:consult(AppFile) of
        {ok, [{application, AppName, AppDetails}]} ->
            DepApps = proplists:get_value(applications, AppDetails, []),
            IncApps = proplists:get_value(included_applications, AppDetails,
                                          []),
            Modules = proplists:get_value(modules, AppDetails, []),
            {Files, Warnings} = modules_to_files(Modules, EbinDir),
            {IncApps ++ DepApps, Files, Warnings};
        {error, enoent} when AppName =:= erts ->
            {[], ebin_files(EbinDir), []};
        _ ->
            Error = io_lib:format("Could not parse ~p", [AppFile]),
            throw({dialyzer_error, Error})
    end.

modules_to_files(Modules, EbinDir) ->
    Ext = code:objfile_extension(),
    Result = [module_to_file(Module, EbinDir, Ext) || Module <- Modules],
    Files = [File || {_, File} <- Result, File =/= unknown],
    Warnings = [{unknown_module, {"", 0}, [Module]} ||
                {Module, unknown} <- Result],
    {Files, Warnings}.

module_to_file(Module, EbinDir, Ext) ->
    File = filename:join(EbinDir, atom_to_list(Module) ++ Ext),
    case filelib:is_file(File) of
        true ->
            {Module, File};
        false ->
            {Module, unknown}
    end.

ebin_files(EbinDir) ->
    Wildcard = "*" ++ code:objfile_extension(),
    [filename:join(EbinDir, File) ||
     File <- filelib:wildcard(Wildcard, EbinDir)].

read_plt(_State, Plt) ->
    case dialyzer:plt_info(Plt) of
        {ok, Info} ->
            Files = proplists:get_value(files, Info, []),
            {ok, Files};
        {error, no_such_file} = Error ->
            Error;
        {error, read_error} ->
            Error = io_lib:format("Could not read the PLT file ~p", [Plt]),
            throw({dialyzer_error, Error})
    end.

check_plt(State, Plt, OldList, FilesList) ->
    Old = sets:from_list(OldList),
    Files = sets:from_list(FilesList),
    Remove = sets:subtract(Old, Files),
    {RemWarnings, State1} = remove_plt(State, Plt, sets:to_list(Remove)),
    Check = sets:intersection(Files, Old),
    {CheckWarnings, State2} = check_plt(State1, Plt, sets:to_list(Check)),
    Add = sets:subtract(Files, Old),
    {AddWarnings, State3} = add_plt(State2, Plt, sets:to_list(Add)),
    {RemWarnings + CheckWarnings + AddWarnings, State3}.

remove_plt(State, _Plt, []) ->
    {0, State};
remove_plt(State, Plt, Files) ->
    ?INFO("Removing ~b files from ~p...", [length(Files), Plt]),
    run_plt(State, Plt, plt_remove, Files).

check_plt(State, _Plt, []) ->
    {0, State};
check_plt(State, Plt, Files) ->
    ?INFO("Checking ~b files in ~p...", [length(Files), Plt]),
    run_plt(State, Plt, plt_check, Files).

add_plt(State, _Plt, []) ->
    {0, State};
add_plt(State, Plt, Files) ->
    ?INFO("Adding ~b files to ~p...", [length(Files), Plt]),
    run_plt(State, Plt, plt_add, Files).

run_plt(State, Plt, Analysis, Files) ->
    GetWarnings = rebar_state:get(State, dialyzer_plt_warnings, false),
    Opts = [{analysis_type, Analysis},
            {get_warnings, GetWarnings},
            {init_plt, Plt},
            {from, byte_code},
            {files, Files}],
    run_dialyzer(State, Opts).

build_proj_plt(State, Plt, Files) ->
    SharedPlt = get_shared_plt_location(State),
    {SharedFiles, SharedWarnings} = shared_plt_files(State),
    SharedWarnings2 = format_warnings(SharedWarnings),
    {SharedWarnings3, State1} = update_shared_plt(State, SharedPlt,
                                                  SharedFiles),
    _ = filelib:ensure_dir(Plt),
    copy_plt(SharedPlt, Plt),
    {CheckWarnings, State2} = check_plt(State1, Plt, SharedFiles,
                                        Files),
    {SharedWarnings2 + SharedWarnings3 + CheckWarnings, State2}.

copy_plt(Source, Source) ->
    ok;
copy_plt(Source, Target) ->
    ?INFO("Copying ~p to ~p...", [Source, Target]),
    case file:copy(Source, Target) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            Error = io_lib:format("Could not copy PLT from ~p to ~p: ~p",
                                  [Source, Target, file:format_error(Reason)]),
            throw({dialyzer_error, Error})
    end.

get_shared_plt_location(State) ->
    case rebar_state:get(State, dialyzer_shared_plt_location, shared) of
        shared ->
            GlobalCacheDir = rebar_dir:global_cache_dir(State),
            filename:join(GlobalCacheDir, plt_name());
        SharedDir ->
            filename:join(SharedDir, plt_name())
    end.

shared_plt_files(State) ->
    SharedPltApps = rebar_state:get(State, dialyzer_shared_plt_apps,
                                    default_plt_apps()),
    Apps = rebar_state:project_apps(State),
    get_plt_files(SharedPltApps, Apps).

update_shared_plt(State, SharedPlt, SharedFiles) ->
    ?INFO("Updating shared plt...", []),
    case read_plt(State, SharedPlt) of
        {ok, OldSharedFiles} ->
            check_plt(State, SharedPlt, OldSharedFiles, SharedFiles);
        {error, no_such_file} ->
            _ = filelib:ensure_dir(SharedPlt),
            build_plt(State, SharedPlt, SharedFiles)
    end.

build_plt(State, Plt, Files) ->
    ?INFO("Adding ~b files to ~p...", [length(Files), Plt]),
    GetWarnings = rebar_state:get(State, dialyzer_plt_warnings, false),
    Opts = [{analysis_type, plt_build},
            {get_warnings, GetWarnings},
            {output_plt, Plt},
            {files, Files}],
    run_dialyzer(State, Opts).

succ_typings(State, Plt) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(succ_typings, Args) of
        false ->
            {0, State};
        _ ->
            Apps = rebar_state:project_apps(State),
            succ_typings(State, Plt, Apps)
    end.

succ_typings(State, Plt, Apps) ->
    ?INFO("Doing success typing analysis...", []),
    {Files, Warnings} = apps_to_files(Apps),
    Warnings2 = format_warnings(Warnings),
    ?INFO("Analyzing ~b files with ~p...", [length(Files), Plt]),
    Opts = [{analysis_type, succ_typings},
            {get_warnings, true},
            {from, byte_code},
            {files, Files},
            {init_plt, Plt}],
    {Warnings3, State2} = run_dialyzer(State, Opts),
    {Warnings2 + Warnings3, State2}.

apps_to_files(Apps) ->
    ?INFO("Resolving files...", []),
    Result = [{Files, Warnings} ||
              App <- Apps,
              {Files, Warnings} <- [app_to_files(App)]],
    Files = [File || {Files, _} <- Result, File <- Files],
    Warnings = [Warning || {_, Warnings} <- Result, Warning <- Warnings],
    {Files, Warnings}.

app_to_files(App) ->
    AppName = ec_cnv:to_atom(rebar_app_info:name(App)),
    {_, Files, Warnings} = app_name_to_info(AppName),
    {Files, Warnings}.

run_dialyzer(State, Opts) ->
    %% dialyzer may return callgraph warnings when get_warnings is false
    case proplists:get_bool(get_warnings, Opts) of
        true ->
            WarningsList = rebar_state:get(State, dialyzer_warnings, []),
            Opts2 = [{warnings, WarningsList},
                     {check_plt, false} |
                     Opts],
            ?DEBUG("Running dialyzer with options: ~p~n", [Opts2]),
            Warnings = format_warnings(dialyzer:run(Opts2)),
            {Warnings, State};
        false ->
            Opts2 = [{warnings, no_warnings()},
                     {check_plt, false} |
                     Opts],
            ?DEBUG("Running dialyzer with options: ~p~n", [Opts2]),
            _ = dialyzer:run(Opts2),
            {0, State}
    end.

format_warnings(Warnings) ->
    format_warnings(Warnings, 0).

format_warnings([Warning | Warnings], N) ->
    format_warning(Warning),
    format_warnings(Warnings, N + 1);
format_warnings([], N) ->
    N.

format_warning({unknown_application, _, [AppName]}) ->
    ?CONSOLE("Unknown application: ~s", [AppName]);
format_warning({unknown_module, _, [Module]}) ->
    ?CONSOLE("Unknown module: ~s", [Module]);
format_warning(Warning) ->
    case strip(dialyzer:format_warning(Warning, fullpath)) of
        ":0: " ++ Unknown ->
            ?CONSOLE("~s", [Unknown]);
        Warning2 ->
            ?CONSOLE("~s", [Warning2])
    end.

strip(Warning) ->
    string:strip(Warning, right, $\n).

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
