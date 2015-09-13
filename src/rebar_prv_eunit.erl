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
    ?DEBUG("eunit_tests ~p", [Tests]),
    ?DEBUG("eunit_opts  ~p", [EUnitOpts]),
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
    io_lib:format("Error running tests: ~p", [Reason]);
format_error({error, Error}) ->
    format_error({error_running_tests, Error}).

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
    CmdTests = resolve_tests(RawOpts),
    CfgTests = rebar_state:get(State, eunit_tests, []),
    ProjectApps = project_apps(State),
    Tests = select_tests(ProjectApps, CmdTests, CfgTests),
    validate_tests(State, ProjectApps, Tests).

resolve_tests(RawOpts) ->
    Apps    = resolve(app, application, RawOpts),
    Dirs    = resolve(dir, RawOpts),
    Files   = resolve(file, RawOpts),
    Modules = resolve(module, RawOpts),
    Suites  = resolve(suite, module, RawOpts),
    Apps ++ Dirs ++ Files ++ Modules ++ Suites.

resolve(Flag, RawOpts) -> resolve(Flag, Flag, RawOpts).

resolve(Flag, EUnitKey, RawOpts) ->
    case proplists:get_value(Flag, RawOpts) of
        undefined -> [];
        Args      -> lists:map(fun(Arg) -> normalize(EUnitKey, Arg) end, string:tokens(Args, [$,]))
    end.

normalize(Key, Value) when Key == dir; Key == file -> {Key, Value};
normalize(Key, Value) -> {Key, list_to_atom(Value)}.

select_tests(ProjectApps, [], []) -> default_tests(ProjectApps);
select_tests(_ProjectApps, A, B)  -> A ++ B.

validate_tests(State, ProjectApps, Tests) ->
    {ok, lists:filter(fun(Elem) -> validate(State, ProjectApps, Elem) end, Tests)}.

validate(_State, ProjectApps, {application, App}) ->
    validate_app(App, ProjectApps);
validate(State, _ProjectApps, {dir, Dir}) ->
    ok = maybe_compile_dir(State, Dir),
    validate_dir(Dir);
validate(State, _ProjectApps, {file, File}) ->
    ok = maybe_compile_file(State, File),
    validate_file(File);
validate(_State, ProjectApps, {module, Module}) ->
    validate_module(Module, ProjectApps);
validate(_State, ProjectApps, {suite, Module}) ->
    validate_module(Module, ProjectApps);
validate(_State, ProjectApps, Module) when is_atom(Module) ->
    validate_module(Module, ProjectApps);
validate(State, ProjectApps, Path) when is_list(Path) ->
    case ec_file:is_dir(Path) of
        true  -> validate(State, ProjectApps, {dir, Path});
        false -> validate(State, ProjectApps, {file, Path})
    end;
%% unrecognized tests should be included. if they're invalid eunit will error
%% and rebar.config may contain arbitrarily complex tests that are effectively
%% unvalidatable
validate(_State, _ProjectApps, _Test) -> true.

validate_app(AppName, []) ->
    ?WARN(lists:concat(["Application `", AppName, "' not found in project."]), []),
    false;
validate_app(AppName, [App|Rest]) ->
    case AppName == binary_to_atom(rebar_app_info:name(App), unicode) of
        true  -> true;
        false -> validate_app(AppName, Rest)
    end.

validate_dir(Dir) ->
    case ec_file:is_dir(Dir) of
        true  -> true;
        false -> ?WARN(lists:concat(["Directory `", Dir, "' not found."]), []), false
    end.

validate_file(File) ->
    case ec_file:exists(File) of
        true  -> true;
        false -> ?WARN(lists:concat(["File `", File, "' not found."]), []), false
    end.

validate_module(Module, Apps) ->
    AppModules = app_modules([binary_to_atom(rebar_app_info:name(A), unicode) || A <- Apps], []),
    case lists:member(Module, AppModules) of
        true  -> true;
        false -> ?WARN(lists:concat(["Module `", Module, "' not found in applications."]), []), false
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

default_tests(Apps) -> set_apps(Apps, []).

set_apps([], Acc) -> lists:reverse(Acc);
set_apps([App|Rest], Acc) ->
    AppName = list_to_atom(binary_to_list(rebar_app_info:name(App))),
    set_apps(Rest, [{application, AppName}|Acc]).

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

maybe_compile_dir(_, _) -> ok.
maybe_compile_file(_, _) -> ok.

maybe_cover_compile(State, Opts) ->
    State1 = case proplists:get_value(cover, Opts, false) of
        true  -> rebar_state:set(State, cover_enabled, true);
        false -> State
    end,
    rebar_prv_cover:maybe_cover_compile(State1).

handle_results(ok) -> ok;
handle_results(error) ->
    {error, unknown_error};
handle_results({error, Reason}) ->
    {error, {error_running_tests, Reason}}.

eunit_opts(_State) ->
    [{app, undefined, "app", string, help(app)},
     {cover, $c, "cover", boolean, help(cover)},
     {dir, undefined, "dir", string, help(dir)},
     {file, undefined, "file", string, help(file)},
     {module, undefined, "module", string, help(module)},
     {suite, undefined, "suite", string, help(suite)},
     {verbose, $v, "verbose", boolean, help(verbose)}].

help(app)     -> "Comma separated list of application test suites to run. Equivalent to `[{application, App}]`.";
help(cover)   -> "Generate cover data. Defaults to false.";
help(dir)     -> "Comma separated list of dirs to load tests from. Equivalent to `[{dir, Dir}]`.";
help(file)    -> "Comma separated list of files to load tests from. Equivalent to `[{file, File}]`.";
help(module)  -> "Comma separated list of modules to load tests from. Equivalent to `[{module, Module}]`.";
help(suite)   -> "Comma separated list of test suites to run. Equivalent to `[{module, Suite}]`.";
help(verbose) -> "Verbose output. Defaults to false.".
