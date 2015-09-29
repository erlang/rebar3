%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_eunit).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).
%% exported solely for tests
-export([compile/1, prepare_tests/1, eunit_opts/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, eunit).
%% we need to modify app_info state before compile
-define(DEPS, [lock]).

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
    case compile(State) of
        %% successfully compiled apps
        {ok, S} -> do_tests(S);
        %% this should look like a compiler error, not an eunit error
        Error   -> Error
    end.

do_tests(State) ->
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
    ?DEBUG("eunit_tests ~p", [Tests]),
    ?DEBUG("eunit_opts  ~p", [EUnitOpts]),
    Result = eunit:test(Tests, EUnitOpts),
    ok = maybe_write_coverdata(State),
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
    io_lib:format("Error running tests: ~p", [Reason]);
format_error({eunit_test_errors, Errors}) ->
    io_lib:format(lists:concat(["Error Running EUnit Tests:"] ++
                               lists:map(fun(Error) -> "~n  " ++ Error end, Errors)), []);
format_error({error, Error}) ->
    format_error({error_running_tests, Error}).

%% ===================================================================
%% Internal functions
%% ===================================================================

test_state(State) ->
    ErlOpts = rebar_state:get(State, erl_opts, []),
    TestOpts = safe_define_test_macro(ErlOpts),
    [{extra_src_dirs, ["test"]}, {erl_opts, TestOpts}].

safe_define_test_macro(Opts) ->
    %% defining a compile macro twice results in an exception so
    %% make sure 'TEST' is only defined once
    case test_defined(Opts) of
       true -> Opts;
       false -> [{d, 'TEST'}] ++ Opts
    end.

compile(State) ->
    %% inject `eunit_first_files` and `eunit_compile_opts` into the applications to be compiled
    NewState = inject_eunit_state(State),

    case rebar_prv_compile:do(NewState) of
        %% successfully compiled apps
        {ok, S} ->
            ok = maybe_cover_compile(S),
            ok = maybe_compile_bare_testdir(S),
            {ok, S};
        %% this should look like a compiler error, not an eunit error
        Error   -> Error
    end.

inject_eunit_state(State) ->
    Apps = project_apps(State),
    ModdedApps = lists:map(fun(App) -> inject(State, App) end, Apps),
    rebar_state:project_apps(State, ModdedApps).

inject(State, App) ->
    %% append `eunit_compile_opts` to app defined `erl_opts`
    ErlOpts = rebar_app_info:get(App, erl_opts, []),
    EUnitOpts = rebar_state:get(State, eunit_compile_opts, []),
    NewOpts = EUnitOpts ++ ErlOpts,
    %% append `eunit_first_files` to app defined `erl_first_files`
    FirstFiles = rebar_app_info:get(App, erl_first_files, []),
    EUnitFirstFiles = rebar_state:get(State, eunit_first_files, []),
    NewFirstFiles = EUnitFirstFiles ++ FirstFiles,
    %% insert the new keys into the app
    lists:foldl(fun({K, V}, NewApp) -> rebar_app_info:set(NewApp, K, V) end,
                App,
                [{erl_opts, NewOpts}, {erl_first_files, NewFirstFiles}]).

test_defined([{d, 'TEST'}|_]) -> true;
test_defined([{d, 'TEST', true}|_]) -> true;
test_defined([_|Rest]) -> test_defined(Rest);
test_defined([]) -> false.

prepare_tests(State) ->
    %% parse and translate command line tests
    CmdTests = resolve_tests(State),
    CfgTests = rebar_state:get(State, eunit_tests, []),
    ProjectApps = project_apps(State),
    %% prioritize tests to run first trying any command line specified
    %% tests falling back to tests specified in the config file finally
    %% running a default set if no other tests are present
    Tests = select_tests(State, ProjectApps, CmdTests, CfgTests),
    %% check applications for existence in project, modules for existence
    %% in applications, files and dirs for existence on disk and allow
    %% any unrecognized tests through for eunit to handle
    validate_tests(State, ProjectApps, Tests).

resolve_tests(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    Apps         = resolve(app, application, RawOpts),
    Applications = resolve(application, RawOpts),
    Dirs         = resolve(dir, RawOpts),
    Files        = resolve(file, RawOpts),
    Modules      = resolve(module, RawOpts),
    Suites       = resolve(suite, module, RawOpts),
    Apps ++ Applications ++ Dirs ++ Files ++ Modules ++ Suites.

resolve(Flag, RawOpts) -> resolve(Flag, Flag, RawOpts).

resolve(Flag, EUnitKey, RawOpts) ->
    case proplists:get_value(Flag, RawOpts) of
        undefined -> [];
        Args      -> lists:map(fun(Arg) -> normalize(EUnitKey, Arg) end, string:tokens(Args, [$,]))
    end.

normalize(Key, Value) when Key == dir; Key == file -> {Key, Value};
normalize(Key, Value) -> {Key, list_to_atom(Value)}.

project_apps(State) ->
    filter_checkouts(rebar_state:project_apps(State)).

filter_checkouts(Apps) -> filter_checkouts(Apps, []).

filter_checkouts([], Acc) -> lists:reverse(Acc);
filter_checkouts([App|Rest], Acc) ->
    case rebar_app_info:is_checkout(App) of
        true  -> filter_checkouts(Rest, Acc);
        false -> filter_checkouts(Rest, [App|Acc])
    end.

select_tests(State, ProjectApps, [], []) -> default_tests(State, ProjectApps);
select_tests(_State, _ProjectApps, [], Tests)  -> Tests;
select_tests(_State, _ProjectApps, Tests, _) -> Tests.

default_tests(State, Apps) ->
    Tests = set_apps(Apps, []),
    BareTest = filename:join([rebar_state:dir(State), "test"]),
    F = fun(App) -> rebar_app_info:dir(App) == rebar_state:dir(State) end,
    case filelib:is_dir(BareTest) andalso not lists:any(F, Apps) of
        %% `test` dir at root of project is already scheduled to be
        %%  included or `test` does not exist
        false -> lists:reverse(Tests);
        %% need to add `test` dir at root to dirs to be included
        true  -> lists:reverse([{dir, filename:join([rebar_dir:base_dir(State), "test"])}|Tests])
    end.

set_apps([], Acc) -> Acc;
set_apps([App|Rest], Acc) ->
    AppName = list_to_atom(binary_to_list(rebar_app_info:name(App))),
    set_apps(Rest, [{application, AppName}|Acc]).

validate_tests(State, ProjectApps, Tests) ->
    gather_tests(fun(Elem) -> validate(State, ProjectApps, Elem) end, Tests, []).

gather_tests(_F, [], Acc) -> {ok, lists:reverse(Acc)};
gather_tests(F, [Test|Rest], Acc) ->
    case F(Test) of
        ok             -> gather_tests(F, Rest, [Test|Acc]);
        %% failure mode, switch to gathering errors
        {error, Error} -> gather_errors(F, Rest, [Error])
    end.

gather_errors(_F, [], Acc) -> ?PRV_ERROR({eunit_test_errors, lists:reverse(Acc)});
gather_errors(F, [Test|Rest], Acc) ->
    case F(Test) of
        ok             -> gather_errors(F, Rest, Acc);
        {error, Error} -> gather_errors(F, Rest, [Error|Acc])
    end.

validate(State, ProjectApps, {application, App}) ->
    validate_app(State, ProjectApps, App);
validate(State, _ProjectApps, {dir, Dir}) ->
    validate_dir(State, Dir);
validate(State, _ProjectApps, {file, File}) ->
    validate_file(State, File);
validate(State, _ProjectApps, {module, Module}) ->
    validate_module(State, Module);
validate(State, _ProjectApps, {suite, Module}) ->
    validate_module(State, Module);
validate(State, _ProjectApps, Module) when is_atom(Module) ->
    validate_module(State, Module);
validate(State, ProjectApps, Path) when is_list(Path) ->
    case ec_file:is_dir(Path) of
        true  -> validate(State, ProjectApps, {dir, Path});
        false -> validate(State, ProjectApps, {file, Path})
    end;
%% unrecognized tests should be included. if they're invalid eunit will error
%% and rebar.config may contain arbitrarily complex tests that are effectively
%% unvalidatable
validate(_State, _ProjectApps, _Test) -> ok.

validate_app(_State, [], AppName) ->
    {error, lists:concat(["Application `", AppName, "' not found in project."])};
validate_app(State, [App|Rest], AppName) ->
    case AppName == binary_to_atom(rebar_app_info:name(App), unicode) of
        true  -> ok;
        false -> validate_app(State, Rest, AppName)
    end.

validate_dir(_State, Dir) ->
    case ec_file:is_dir(Dir) of
        true  -> ok;
        false -> {error, lists:concat(["Directory `", Dir, "' not found."])}
    end.

validate_file(_State, File) ->
    case ec_file:exists(File) of
        true  -> ok;
        false -> {error, lists:concat(["File `", File, "' not found."])}
    end.

validate_module(_State, Module) ->
    Path = code:which(Module),
    case beam_lib:chunks(Path, [exports]) of
        {ok, _}              -> ok;
        {error, beam_lib, _} -> {error, lists:concat(["Module `", Module, "' not found in project."])}
    end.

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

maybe_cover_compile(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    State1 = case proplists:get_value(cover, RawOpts, false) of
        true  -> rebar_state:set(State, cover_enabled, true);
        false -> State
    end,
    rebar_prv_cover:maybe_cover_compile(State1).

maybe_cover_compile(State, Dir) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    State1 = case proplists:get_value(cover, RawOpts, false) of
        true  -> rebar_state:set(State, cover_enabled, true);
        false -> State
    end,
    rebar_prv_cover:maybe_cover_compile(State1, [Dir]).

maybe_write_coverdata(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    State1 = case proplists:get_value(cover, RawOpts, false) of
        true  -> rebar_state:set(State, cover_enabled, true);
        false -> State
    end,
    rebar_prv_cover:maybe_write_coverdata(State1, ?PROVIDER).

maybe_compile_bare_testdir(State) ->
    Apps = project_apps(State),
    BareTest = filename:join([rebar_state:dir(State), "test"]),
    F = fun(App) -> rebar_app_info:dir(App) == rebar_state:dir(State) end,
    case ec_file:is_dir(BareTest) andalso not lists:any(F, Apps) of
        true ->
            ErlOpts = rebar_state:get(State, erl_opts, []),
            EUnitOpts = rebar_state:get(State, eunit_compile_opts, []),
            NewErlOpts = safe_define_test_macro(EUnitOpts ++ ErlOpts),
            %% append `eunit_first_files` to app defined `erl_first_files`
            FirstFiles = rebar_state:get(State, erl_first_files, []),
            EUnitFirstFiles = rebar_state:get(State, eunit_first_files, []),
            NewFirstFiles = EUnitFirstFiles ++ FirstFiles,
            Opts = rebar_state:opts(State),
            NewOpts = lists:foldl(fun({K, V}, Dict) -> rebar_opts:set(Dict, K, V) end,
                                  Opts,
                                  [{erl_opts, NewErlOpts}, {erl_first_files, NewFirstFiles}, {src_dirs, ["test"]}]),
            OutDir = filename:join([rebar_dir:base_dir(State), "test"]),
            filelib:ensure_dir(filename:join([OutDir, "dummy.beam"])),
            rebar_erlc_compiler:compile(NewOpts, rebar_state:dir(State), OutDir),
            maybe_cover_compile(State, [OutDir]);
        false -> ok
    end.

handle_results(ok) -> ok;
handle_results(error) ->
    {error, unknown_error};
handle_results({error, Reason}) ->
    {error, {error_running_tests, Reason}}.

eunit_opts(_State) ->
    [{app, undefined, "app", string, help(app)},
     {application, undefined, "application", string, help(app)},
     {cover, $c, "cover", boolean, help(cover)},
     {dir, undefined, "dir", string, help(dir)},
     {file, undefined, "file", string, help(file)},
     {module, undefined, "module", string, help(module)},
     {suite, undefined, "suite", string, help(module)},
     {verbose, $v, "verbose", boolean, help(verbose)}].

help(app)     -> "Comma separated list of application test suites to run. Equivalent to `[{application, App}]`.";
help(cover)   -> "Generate cover data. Defaults to false.";
help(dir)     -> "Comma separated list of dirs to load tests from. Equivalent to `[{dir, Dir}]`.";
help(file)    -> "Comma separated list of files to load tests from. Equivalent to `[{file, File}]`.";
help(module)  -> "Comma separated list of modules to load tests from. Equivalent to `[{module, Module}]`.";
help(verbose) -> "Verbose output. Defaults to false.".
