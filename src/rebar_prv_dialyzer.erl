%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_dialyzer).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

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
                                                               {example, "rebar dialyzer"},
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
    "`dialyzer_plt` - the PLT file to use\n"
    "`dialyzer_plt_apps` - a list of applications to include in the PLT file*\n"
    "\n"
    "*If this configuration is not present a selection of applications will be "
    "used based on the `applications` and `included_applications` fields in "
    "the relevant .app files.\n"
    "\n"
    "Note that it may take a long time to build the initial PLT file. Once a "
    "PLT file (defaults to `.rebar.plt`) has been created for a project it can "
    "safely be copied and reused in another project. If a PLT file has been "
    "copied from another project this command will do the minimial alterations "
    "to the PLT file for use in the new project. This will likely be faster "
    "than building a new PLT file from scratch.".

short_desc() ->
    "Run the Dialyzer analyzer on the project.".

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?INFO("Dialyzer starting, this may take a while...", []),
    State1 = set_plt_location(State),
    Apps = rebar_state:project_apps(State1),
    Deps = rebar_state:get(State, all_deps, []),

    try
        {ok, State2} = update_plt(State1, Apps, Deps),
        succ_typings(State2, Apps)
    catch
        throw:{dialyzer_error, Error} ->
            {error, {?MODULE, {error_processing_apps, Error, Apps}}}
    end.

-spec format_error(any()) -> iolist().
format_error({error_processing_apps, Error, _Apps}) ->
    io_lib:format("Error in dialyzing apps: ~s", [Error]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Internal functions

set_plt_location(State) ->
    BuildDir = rebar_state:get(State, base_dir, ?DEFAULT_BASE_DIR),
    DefaultPlt = filename:join([BuildDir, ".rebar.plt"]),
    Plt = rebar_state:get(State, dialyzer_plt, DefaultPlt),
    rebar_state:set(State, dialyzer_plt, Plt).

update_plt(State, Apps, Deps) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(update_plt, Args) of
        false ->
            {ok, State};
        _ ->
            do_update_plt(State, Apps, Deps)
    end.

do_update_plt(State, Apps, Deps) ->
    ?INFO("Updating plt...", []),
    Files = get_plt_files(State, Apps, Deps),
    case read_plt(State) of
        {ok, OldFiles} ->
            check_plt(State, OldFiles, Files);
        {error, no_such_file} ->
            build_plt(State, Files)
    end.

get_plt_files(State, Apps, Deps) ->
    case rebar_state:get(State, dialyzer_plt_apps) of
        undefined ->
            default_plt_files(Apps, Deps);
        PltApps ->
            app_names_to_files(PltApps, Apps ++ Deps)
    end.

default_plt_files(Apps, Deps) ->
    DepApps = lists:flatmap(fun rebar_app_info:applications/1, Apps),
    default_plt_files(default_plt_apps() ++ DepApps, Apps, Deps, [], []).

default_plt_apps() ->
    [erts,
     kernel,
     stdlib].

default_plt_files([], _, _, _, Files) ->
    Files;
default_plt_files([AppName | DepApps], Apps, Deps, PltApps, Files) ->
    case lists:member(AppName, PltApps) of
        true ->
            default_plt_files(DepApps, Apps, Deps, PltApps, Files);
        false ->
            {DepApps2, Files2} = app_name_to_info(AppName, Apps, Deps),
            DepApps3 = DepApps2 ++ DepApps,
            Files3 = Files2 ++ Files,
            default_plt_files(DepApps3, Apps, Deps, [AppName | PltApps], Files3)
    end.

app_name_to_info(AppName, Apps, Deps) ->
    case rebar_app_utils:find(ec_cnv:to_binary(AppName), Apps) of
        {ok, App} ->
            % Don't include project app files in plt
            {rebar_app_info:applications(App), []};
        error ->
            app_name_to_info(AppName, Deps)
    end.

apps_to_files(Apps) ->
    lists:flatmap(fun app_to_files/1, Apps).

app_to_files(App) ->
    AppDetails = rebar_app_info:app_details(App),
    Modules = proplists:get_value(modules, AppDetails, []),
    EbinDir = rebar_app_info:ebin_dir(App),
    modules_to_files(Modules, EbinDir).

modules_to_files(Modules, EbinDir) ->
    Ext = code:objfile_extension(),
    Mod2File = fun(Module) -> module_to_file(Module, EbinDir, Ext) end,
    rebar_utils:filtermap(Mod2File, Modules).

module_to_file(Module, EbinDir, Ext) ->
    File = filename:join(EbinDir, atom_to_list(Module) ++ Ext),
    case filelib:is_file(File) of
        true ->
            {true, File};
        false ->
            ?CONSOLE("Unknown module ~s", [Module]),
            false
    end.

app_names_to_files(AppNames, Apps) ->
    ToFiles = fun(AppName) ->
                      {_, Files} = app_name_to_info(AppName, Apps),
                      Files
              end,
    lists:flatmap(ToFiles, AppNames).

app_name_to_info(AppName, Apps) ->
    case rebar_app_utils:find(ec_cnv:to_binary(AppName), Apps) of
        {ok, App} ->
            DepApps = rebar_app_info:applications(App),
            Files = app_to_files(App),
            {DepApps, Files};
        error ->
            app_name_to_info(AppName)
    end.

app_name_to_info(AppName) ->
    case code:lib_dir(AppName) of
        {error, _} ->
            ?CONSOLE("Unknown application ~s", [AppName]),
            {[], []};
        AppDir ->
            app_dir_to_info(AppDir, AppName)
    end.

app_dir_to_info(AppDir, AppName) ->
    EbinDir = filename:join(AppDir, "ebin"),
    AppFile = filename:join(EbinDir, atom_to_list(AppName) ++ ".app"),
    case file:consult(AppFile) of
        {ok, [{application, AppName, AppDetails}]} ->
            DepApps = proplists:get_value(applications, AppDetails, []),
            IncApps = proplists:get_value(included_applications, AppDetails,
                                          []),
            Modules = proplists:get_value(modules, AppDetails, []),
            Files = modules_to_files(Modules, EbinDir),
            {IncApps ++ DepApps, Files};
        _ ->
            Error = io_lib:format("Could not parse ~p", [AppFile]),
            throw({dialyzer_error, Error})
    end.

read_plt(State) ->
    Plt = rebar_state:get(State, dialyzer_plt),
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

check_plt(State, OldList, FilesList) ->
    Old = sets:from_list(OldList),
    Files = sets:from_list(FilesList),
    Remove = sets:subtract(Old, Files),
    {ok, State1} = remove_plt(State, sets:to_list(Remove)),
    Check = sets:intersection(Files, Old),
    {ok, State2} = check_plt(State1, sets:to_list(Check)),
    Add = sets:subtract(Files, Old),
    add_plt(State2, sets:to_list(Add)).

remove_plt(State, []) ->
    {ok, State};
remove_plt(State, Files) ->
    ?INFO("Removing ~b files from plt...", [length(Files)]),
    run_plt(State, plt_remove, Files).

check_plt(State, []) ->
    {ok, State};
check_plt(State, Files) ->
    ?INFO("Checking ~b files in plt...", [length(Files)]),
    run_plt(State, plt_check, Files).

add_plt(State, []) ->
    {ok, State};
add_plt(State, Files) ->
    ?INFO("Adding ~b files to plt...", [length(Files)]),
    run_plt(State, plt_add, Files).

run_plt(State, Analysis, Files) ->
    Plt = rebar_state:get(State, dialyzer_plt),
    Opts = [{analysis_type, Analysis},
            {init_plt, Plt},
            {from, byte_code},
            {files, Files}],
    run_dialyzer(State, Opts).

build_plt(State, Files) ->
    Plt = rebar_state:get(State, dialyzer_plt),
    ?INFO("Adding ~b files to plt...", [length(Files)]),
    Opts = [{analysis_type, plt_build},
            {output_plt, Plt},
            {files, Files}],
    run_dialyzer(State, Opts).

succ_typings(State, Apps) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(succ_typings, Args) of
        false ->
            {ok, State};
        _ ->
            do_succ_typings(State, Apps)
    end.

do_succ_typings(State, Apps) ->
    ?INFO("Doing success typing analysis...", []),
    Files = apps_to_files(Apps),
    Plt = rebar_state:get(State, dialyzer_plt),
    Opts = [{analysis_type, succ_typings},
            {from, byte_code},
            {files, Files},
            {plts, [Plt]}],
    run_dialyzer(State, Opts).

run_dialyzer(State, Opts) ->
    Warnings = rebar_state:get(State, dialyzer_warnings, default_warnings()),
    Opts2 = [{get_warnings, true},
             {warnings, Warnings} |
             Opts],
    _ = [?CONSOLE(format_warning(Warning), [])
         || Warning <- dialyzer:run(Opts2)],
    {ok, State}.

format_warning(Warning) ->
    string:strip(dialyzer_format_warning(Warning), right, $\n).

dialyzer_format_warning(Warning) ->
    case dialyzer:format_warning(Warning) of
        ":0: " ++ Warning2 ->
            Warning2;
        Warning2 ->
            Warning2
    end.
default_warnings() ->
    [error_handling,
     unmatched_returns,
     underspecs].
