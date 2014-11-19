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
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, "rebar dialyzer"},
                                                               {short_desc, "Run the Dialyzer analyzer on the project."},
                                                               {desc, ""},
                                                               {opts, []}])),
    {ok, State1}.

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
    DefaultPlt = filename:join([BuildDir, ".deps.plt"]),
    Plt = rebar_state:get(State, plt, DefaultPlt),
    rebar_state:set(State, plt, Plt).

update_plt(State, Apps, Deps) ->
    ?INFO("Updating plt...", []),
    Files = get_plt_files(State, Apps, Deps),
    case read_plt(State) of
        {ok, OldFiles} ->
            check_plt(State, OldFiles, Files);
        {error, no_such_file} ->
            build_plt(State, Files)
    end.

get_plt_files(State, Apps, Deps) ->
    case rebar_state:get(State, plt_apps) of
        undefined ->
            apps_to_files(Deps);
        PltApps ->
            app_names_to_files(PltApps, Apps ++ Deps)
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
    lists:flatmap(fun(AppName) -> app_name_to_files(AppName, Apps) end,
                  AppNames).

app_name_to_files(AppName, Apps) ->
    case rebar_app_utils:find(ec_cnv:to_binary(AppName), Apps) of
        {ok, App} ->
            app_to_files(App);
        error ->
            app_name_to_files(AppName)
    end.

app_name_to_files(AppName) ->
    case code:lib_dir(AppName) of
        {error, _} ->
            ?CONSOLE("Unknown application ~s", [AppName]),
            [];
        AppDir ->
            app_dir_to_files(AppDir, AppName)
    end.

app_dir_to_files(AppDir, AppName) ->
    EbinDir = filename:join(AppDir, "ebin"),
    AppFile = filename:join(EbinDir, atom_to_list(AppName) ++ ".app"),
    case file:consult(AppFile) of
        {ok, [{application, AppName, AppDetails}]} ->
            Modules = proplists:get_value(modules, AppDetails, []),
            modules_to_files(Modules, EbinDir);
        _ ->
            Error = io_lib:format("Could not parse ~p", [AppFile]),
            throw({dialyzer_error, Error})
    end.

read_plt(State) ->
    Plt = rebar_state:get(State, plt),
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
    Plt = rebar_state:get(State, plt),
    Opts = [{analysis_type, Analysis},
            {init_plt, Plt},
            {from, byte_code},
            {files, Files}],
    run_dialyzer(State, Opts).

build_plt(State, Files) ->
    Plt = rebar_state:get(State, plt),
    ?INFO("Building PLT with ~b files...", [length(Files)]),
    Opts = [{analysis_type, plt_build},
            {output_plt, Plt},
            {files, Files}],
    run_dialyzer(State, Opts).

succ_typings(State, Apps) ->
    ?INFO("Doing success typing analysis...", []),
    Files = apps_to_files(Apps),
    Plt = rebar_state:get(State, plt),
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
    string:strip(dialyzer:format_warning(Warning), right, $\n).

default_warnings() ->
    [error_handling,
     unmatched_returns,
     underspecs].
