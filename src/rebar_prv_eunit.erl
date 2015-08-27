%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_eunit).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, eunit).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {deps, ?DEPS},
                                 {bare, true},
                                 {example, "rebar3 eunit"},
                                 {short_desc, "Run EUnit Tests."},
                                 {desc, "Run EUnit Tests."},
                                 {opts, eunit_opts(State)},
                                 {profiles, [test]}]),
    State1 = rebar_state:add_provider(State, Provider),
    State2 = rebar_state:add_to_profile(State1, test, test_state(State1)),
    {ok, State2}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?INFO("Performing EUnit tests...", []),
    rebar_utils:update_code(rebar_state:code_paths(State, all_deps)),

    %% Run eunit provider prehooks
    Providers = rebar_state:providers(State),
    Cwd = rebar_dir:get_cwd(),
    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State),

    case prepare_tests(State) of
        {ok, Tests} ->
            case do_tests(State, Tests) of
                {ok, State1} ->
                    %% Run eunit provider posthooks
                    rebar_hooks:run_all_hooks(Cwd, post, ?PROVIDER, Providers, State1),
                    rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)),
                    {ok, State1};
                Error ->
                    rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)),
                    Error
            end;
        Error ->
            rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)),
            Error
    end.

do_tests(State, Tests) ->
    EUnitOpts = resolve_eunit_opts(State),
    Result = eunit:test(Tests, EUnitOpts),
    ok = rebar_prv_cover:maybe_write_coverdata(State, ?PROVIDER),
    case handle_results(Result) of
        {error, Reason} ->
            ?PRV_ERROR(Reason);
        ok ->
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error(unknown_error) ->
    io_lib:format("Error running tests", []);
format_error({error_running_tests, Reason}) ->
    io_lib:format("Error running tests: ~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

test_state(State) ->
    ErlOpts = rebar_state:get(State, eunit_compile_opts, []),
    TestOpts = safe_define_test_macro(ErlOpts),
    TestDir = [{extra_src_dirs, ["test"]}],
    first_files(State) ++ [{erl_opts, TestOpts ++ TestDir}].

safe_define_test_macro(Opts) ->
    %% defining a compile macro twice results in an exception so
    %% make sure 'TEST' is only defined once
    case test_defined(Opts) of
       true -> Opts;
       false -> [{d, 'TEST'}] ++ Opts
    end.

test_defined([{d, 'TEST'}|_]) -> true;
test_defined([{d, 'TEST', true}|_]) -> true;
test_defined([_|Rest]) -> test_defined(Rest);
test_defined([]) -> false.

first_files(State) ->
    EUnitFirst = rebar_state:get(State, eunit_first_files, []),
    [{erl_first_files, EUnitFirst}].

prepare_tests(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    ok = maybe_cover_compile(State, RawOpts),
    ProjectApps = project_apps(State),
    resolve_tests(ProjectApps, RawOpts).

maybe_cover_compile(State, Opts) ->
    State1 = case proplists:get_value(cover, Opts, false) of
        true  -> rebar_state:set(State, cover_enabled, true);
        false -> State
    end,
    rebar_prv_cover:maybe_cover_compile(State1).

resolve_tests(ProjectApps, RawOpts) ->
    case proplists:get_value(file, RawOpts) of
        undefined -> resolve_apps(ProjectApps, RawOpts);
        Files     -> resolve_files(ProjectApps, Files, RawOpts)
    end.

resolve_files(ProjectApps, Files, RawOpts) ->
    case {proplists:get_value(app, RawOpts), proplists:get_value(suite, RawOpts)} of
        {undefined, undefined} -> resolve_files(Files, []);
        _                      ->
        case resolve_apps(ProjectApps, RawOpts) of
            {ok, TestSet} -> resolve_files(Files, TestSet);
            Error         -> Error
        end
    end.

resolve_files(Files, TestSet) ->
    FileNames = string:tokens(Files, [$,]),
    {ok, TestSet ++ set_files(FileNames, [])}.

resolve_apps(ProjectApps, RawOpts) ->
    case proplists:get_value(app, RawOpts) of
        undefined -> resolve_suites(ProjectApps, RawOpts);
        %% convert app name strings to `rebar_app_info` objects
        Apps      -> AppNames = string:tokens(Apps, [$,]),
                     case filter_apps_by_name(AppNames, ProjectApps) of
                         {ok, TestApps} -> resolve_suites(TestApps, RawOpts);
                         Error          -> Error
                     end
    end.

resolve_suites(Apps, RawOpts) ->
    case proplists:get_value(suite, RawOpts) of
        undefined  -> test_set(Apps, all);
        Suites     -> SuiteNames = string:tokens(Suites, [$,]),
                      case filter_suites_by_apps(SuiteNames, Apps) of
                          {ok, S} -> test_set(Apps, S);
                          Error   -> Error
                      end
    end.

project_apps(State) ->
    filter_checkouts(rebar_state:project_apps(State)).

filter_checkouts(Apps) -> filter_checkouts(Apps, []).

filter_checkouts([], Acc) -> lists:reverse(Acc);
filter_checkouts([App|Rest], Acc) ->
    case rebar_app_info:is_checkout(App) of
        true  -> filter_checkouts(Rest, Acc);
        false -> filter_checkouts(Rest, [App|Acc])
    end.

%% make sure applications specified actually exist
filter_apps_by_name(AppNames, ProjectApps) ->
    filter_apps_by_name(AppNames, ProjectApps, []).

filter_apps_by_name([], _ProjectApps, Acc) -> {ok, lists:reverse(Acc)};
filter_apps_by_name([Name|Rest], ProjectApps, Acc) ->
    case find_app_by_name(Name, ProjectApps) of
        {error, app_not_found} ->
            ?PRV_ERROR({error_running_tests,
                        "Application `" ++ Name ++ "' not found in project."});
        App ->
            filter_apps_by_name(Rest, ProjectApps, [App|Acc])
    end.

find_app_by_name(_, []) -> {error, app_not_found};
find_app_by_name(Name, [App|Rest]) ->
    case Name == binary_to_list(rebar_app_info:name(App)) of
        true  -> App;
        false -> find_app_by_name(Name, Rest)
    end.

%% ensure specified suites are in the applications included
filter_suites_by_apps(Suites, ProjectApps) ->
    filter_suites_by_apps(Suites, ProjectApps, []).

filter_suites_by_apps([], _ProjectApps, Acc) -> {ok, lists:reverse(Acc)};
filter_suites_by_apps([Suite|Rest], Apps, Acc) ->
    Modules = app_modules([binary_to_atom(rebar_app_info:name(A), unicode) || A <- Apps], []),
    case lists:member(list_to_atom(Suite), Modules) of
        false ->
            ?PRV_ERROR({error_running_tests,
                        "Module `" ++ Suite ++ "' not found in applications."});
        true  ->
            filter_suites_by_apps(Rest, Apps, [Suite|Acc])
    end.

app_modules([], Acc) -> Acc;
app_modules([App|Rest], Acc) ->
    Unload = case application:load(App) of
        ok                           -> true;
        {error, {already_loaded, _}} -> false
    end,
    NewAcc = case application:get_key(App, modules) of
        {ok, Modules} -> Modules ++ Acc;
        undefined     -> Acc
    end,
    case Unload of
        true  ->
            application:unload(App),
            app_modules(Rest, NewAcc);
        false ->
            app_modules(Rest, NewAcc)
    end.

test_set(Apps, all) -> {ok, set_apps(Apps, [])};
test_set(_Apps, Suites) -> {ok, set_suites(Suites, [])}.

set_apps([], Acc) -> lists:reverse(Acc);
set_apps([App|Rest], Acc) ->
    AppName = list_to_atom(binary_to_list(rebar_app_info:name(App))),
    set_apps(Rest, [{application, AppName}|Acc]).

set_suites([], Acc) -> lists:reverse(Acc);
set_suites([Suite|Rest], Acc) ->
    set_suites(Rest, [{module, list_to_atom(Suite)}|Acc]).

set_files([], Acc) -> lists:reverse(Acc);
set_files([File|Rest], Acc) ->
    set_files(Rest, [{file, File}|Acc]).

resolve_eunit_opts(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    EUnitOpts = rebar_state:get(State, eunit_opts, []),
    case proplists:get_value(verbose, Opts, false) of
        true  -> set_verbose(EUnitOpts);
        false -> EUnitOpts
    end.

set_verbose(Opts) ->
    %% if `verbose` is already set don't set it again
    case lists:member(verbose, Opts) of
        true  -> Opts;
        false -> [verbose] ++ Opts
    end.

handle_results(ok) -> ok;
handle_results(error) ->
    {error, unknown_error};
handle_results({error, Reason}) ->
    {error, {error_running_tests, Reason}}.

eunit_opts(_State) ->
    [{app, undefined, "app", string, help(app)},
     {cover, $c, "cover", boolean, help(cover)},
     {file, $f, "file", string, help(file)},
     {suite, undefined, "suite", string, help(suite)},
     {verbose, $v, "verbose", boolean, help(verbose)}].

help(app)     -> "Comma seperated list of application test suites to run. Equivalent to `[{application, App}]`.";
help(cover)   -> "Generate cover data. Defaults to false.";
help(file)    -> "Comma seperated list of files to run. Equivalent to `[{file, File}]`.";
help(suite)   -> "Comma seperated list of test suites to run. Equivalent to `[{module, Suite}]`.";
help(verbose) -> "Verbose output. Defaults to false.".
