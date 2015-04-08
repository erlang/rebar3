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
    "`dialyzer_ignored_warnings` - a list of patterns for ignoring warnings\n"
    "`dialyzer_plt` - the PLT file to use\n"
    "`dialyzer_plt_apps` - a list of applications to include in the PLT file*\n"
    "`dialyzer_plt_warnings` - display warnings when updating a PLT file "
    "(boolean)\n"
    "`dialyzer_base_plt` - the base PLT file to use**\n"
    "`dialyzer_base_plt_dir` - the base PLT directory**\n"
    "`dialyzer_base_plt_apps` - a list of applications to include in the base "
    "PLT file**\n"
    "\n"
    "*The applications in `dialyzer_base_plt_apps` and any `applications` and "
    "`included_applications` listed in their .app files will be added to the "
    "list.\n"
    "**The base PLT is a PLT containing the core OTP applications often "
    "required for a project's PLT. One base PLT is created per OTP version and "
    "stored in `dialyzer_base_plt_dir` (defaults to $HOME/.rebar3/). A base "
    "PLT is used to create a project's initial PLT.".

short_desc() ->
    "Run the Dialyzer analyzer on the project.".

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?INFO("Dialyzer starting, this may take a while...", []),
    Plt = get_plt_location(State),
    Apps = rebar_state:project_apps(State),

    try
        do(State, Plt, Apps)
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
    BaseDir = rebar_dir:base_dir(State),
    DefaultPlt = filename:join(BaseDir, default_plt()),
    rebar_state:get(State, dialyzer_plt, DefaultPlt).

default_plt() ->
    rebar_utils:otp_release() ++ ".plt".

do(State, Plt, Apps) ->
    {PltWarnings, State1} = update_proj_plt(State, Plt, Apps),
    {Warnings, State2} = succ_typings(State1, Plt, Apps),
    case PltWarnings + Warnings of
        0 ->
            {ok, State2};
        TotalWarnings ->
            throw({dialyzer_warnings, TotalWarnings})
    end.

update_proj_plt(State, Plt, Apps) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(update_plt, Args) of
        false ->
            {0, State};
        _ ->
            do_update_proj_plt(State, Plt, Apps)
    end.

do_update_proj_plt(State, Plt, Apps) ->
    ?INFO("Updating plt...", []),
    Files = get_plt_files(State, Apps),
    case read_plt(State, Plt) of
        {ok, OldFiles} ->
            check_plt(State, Plt, OldFiles, Files);
        {error, no_such_file} ->
            build_proj_plt(State, Plt, Files)
    end.

get_plt_files(State, Apps) ->
    BasePltApps = rebar_state:get(State, dialyzer_base_plt_apps,
                                  default_plt_apps()),
    PltApps = rebar_state:get(State, dialyzer_plt_apps, []),
    DepApps = lists:flatmap(fun rebar_app_info:applications/1, Apps),
    get_plt_files(BasePltApps ++ PltApps ++ DepApps, Apps, [], []).

default_plt_apps() ->
    [erts,
     crypto,
     kernel,
     stdlib].

get_plt_files([], _, _, Files) ->
    Files;
get_plt_files([AppName | DepApps], Apps, PltApps, Files) ->
    case lists:member(AppName, PltApps) orelse app_member(AppName, Apps) of
        true ->
            get_plt_files(DepApps, Apps, PltApps, Files);
        false ->
            {DepApps2, Files2} = app_name_to_info(AppName),
            ?DEBUG("~s dependencies: ~p", [AppName, DepApps2]),
            ?DEBUG("~s files: ~p", [AppName, Files2]),
            DepApps3 = DepApps2 ++ DepApps,
            Files3 = Files2 ++ Files,
            get_plt_files(DepApps3, Apps, [AppName | PltApps], Files3)
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
            ?CONSOLE("Unknown application ~s", [AppName]),
            {[], []};
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
            Files = modules_to_files(Modules, EbinDir),
            {IncApps ++ DepApps, Files};
        {error, enoent} when AppName =:= erts ->
            {[], ebin_files(EbinDir)};
        _ ->
            Error = io_lib:format("Could not parse ~p", [AppFile]),
            throw({dialyzer_error, Error})
    end.

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
    BasePlt = get_base_plt_location(State),
    BaseFiles = get_base_plt_files(State),
    {BaseWarnings, State1} = update_base_plt(State, BasePlt, BaseFiles),
    ?INFO("Copying ~p to ~p...", [BasePlt, Plt]),
    _ = filelib:ensure_dir(Plt),
    case file:copy(BasePlt, Plt) of
        {ok, _} ->
            {CheckWarnings, State2} = check_plt(State1, Plt, BaseFiles, Files),
            {BaseWarnings + CheckWarnings, State2};
        {error, Reason} ->
            Error = io_lib:format("Could not copy PLT from ~p to ~p: ~p",
                                  [BasePlt, Plt, file:format_error(Reason)]),
            throw({dialyzer_error, Error})
    end.

get_base_plt_location(State) ->
    GlobalCacheDir = rebar_dir:global_cache_dir(State),
    BaseDir = rebar_state:get(State, dialyzer_base_plt_dir, GlobalCacheDir),
    BasePlt = rebar_state:get(State, dialyzer_base_plt, default_plt()),
    filename:join(BaseDir, BasePlt).

get_base_plt_files(State) ->
    BasePltApps = rebar_state:get(State, dialyzer_base_plt_apps,
                                  default_plt_apps()),
    app_names_to_files(BasePltApps).

app_names_to_files(AppNames) ->
    ToFiles = fun(AppName) ->
                      {_, Files} = app_name_to_info(AppName),
                      Files
              end,
    lists:flatmap(ToFiles, AppNames).

update_base_plt(State, BasePlt, BaseFiles) ->
    ?INFO("Updating base plt...", []),
    case read_plt(State, BasePlt) of
        {ok, OldBaseFiles} ->
            check_plt(State, BasePlt, OldBaseFiles, BaseFiles);
        {error, no_such_file} ->
            _ = filelib:ensure_dir(BasePlt),
            build_plt(State, BasePlt, BaseFiles)
    end.

build_plt(State, Plt, Files) ->
    ?INFO("Adding ~b files to ~p...", [length(Files), Plt]),
    GetWarnings = rebar_state:get(State, dialyzer_plt_warnings, false),
    Opts = [{analysis_type, plt_build},
            {get_warnings, GetWarnings},
            {output_plt, Plt},
            {files, Files}],
    run_dialyzer(State, Opts).

succ_typings(State, Plt, Apps) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(succ_typings, Args) of
        false ->
            {0, State};
        _ ->
            do_succ_typings(State, Plt, Apps)
    end.

do_succ_typings(State, Plt, Apps) ->
    ?INFO("Doing success typing analysis...", []),
    Files = apps_to_files(Apps),
    ?INFO("Analyzing ~b files with ~p...", [length(Files), Plt]),
    Opts = [{analysis_type, succ_typings},
            {get_warnings, true},
            {from, byte_code},
            {files, Files},
            {init_plt, Plt}],
    run_dialyzer(State, Opts).

apps_to_files(Apps) ->
    lists:flatmap(fun app_to_files/1, Apps).

app_to_files(App) ->
    AppName = ec_cnv:to_atom(rebar_app_info:name(App)),
    {_, Files} = app_name_to_info(AppName),
    Files.

run_dialyzer(State, Opts) ->
    %% dialyzer may return callgraph warnings when get_warnings is false
    case proplists:get_bool(get_warnings, Opts) of
        true ->
            WarningsList = rebar_state:get(State, dialyzer_warnings, []),
            IgnoredWarningsList = rebar_state:get(State, dialyzer_ignored_warnings, []),
            Opts2 = [{warnings, WarningsList},
                     {check_plt, false} |
                     Opts],
            ?DEBUG("Running dialyzer with options: ~p~n", [Opts2]),
            {Unknowns, Warnings} = format_warnings(filter_warnings(IgnoredWarningsList, dialyzer:run(Opts2))),
            _ = [?CONSOLE("~s", [Unknown]) || Unknown <- Unknowns],
            _ = [?CONSOLE("~s", [Warning]) || Warning <- Warnings],
            {length(Warnings), State};
        false ->
            Opts2 = [{warnings, no_warnings()},
                     {check_plt, false} |
                     Opts],
            ?DEBUG("Running dialyzer with options: ~p~n", [Opts2]),
            _ = dialyzer:run(Opts2),
            {0, State}
    end.

filter_warnings([], Warnings) ->
    Warnings;
filter_warnings(Ignores, Warnings) ->
    lists:filter(fun(Warning) ->
        not lists:any(fun (Ignore) -> match_warning(Ignore, Warning) end, Ignores)
    end, Warnings).

match_warning({MatchTag, {MatchPath, MatchLine}, MatchMsg}, {Tag, {Path, Line}, Msg}) when MatchPath =/= '_' ->
    lists:suffix(MatchPath, Path) andalso match_term([MatchTag, MatchLine, MatchMsg], [Tag, Line, Msg]);
match_warning({_, _, _} = Match, {_, {_, _}, _} = Warning) ->
    match_term(Match, Warning).

match_term(Term, Term) ->
    true;
match_term('_', _Term) ->
    true;
match_term(Term1, Term2) when is_list(Term1), is_list(Term2), length(Term1) =:= length(Term2) ->
    lists:all(fun({T1, T2}) -> match_term(T1, T2) end, lists:zip(Term1, Term2));
match_term(Term1, Term2) when is_tuple(Term1), is_tuple(Term2), tuple_size(Term1) =:= tuple_size(Term2) ->
    lists:all(fun({T1, T2}) -> match_term(T1, T2) end, lists:zip(tuple_to_list(Term1), tuple_to_list(Term2)));
match_term(_, _) ->
    false.

format_warnings(Warnings) ->
    format_warnings(Warnings, [], []).

format_warnings([Warning | Rest], Unknowns, Warnings) ->
    case dialyzer:format_warning(Warning, fullpath) of
        ":0: " ++ Unknown ->
            format_warnings(Rest, [strip(Unknown) | Unknowns], Warnings);
        Warning2 ->
            format_warnings(Rest, Unknowns, [strip(Warning2) | Warnings])
    end;
format_warnings([], Unknowns, Warnings) ->
    {Unknowns, Warnings}.

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
