%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_eunit).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).
%% exported solely for tests
-export([prepare_tests/1, eunit_opts/1, validate_tests/2]).

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
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Tests = prepare_tests(State),
    %% inject `eunit_first_files`, `eunit_compile_opts` and any
    %% directories required by tests into the applications
    NewState = inject_eunit_state(State, Tests),
    case compile(NewState) of
        %% successfully compiled apps
        {ok, S} -> do(S, Tests);
        Error   -> Error
    end.

do(State, Tests) ->
    ?INFO("Performing EUnit tests...", []),

    rebar_utils:update_code(rebar_state:code_paths(State, all_deps), [soft_purge]),

    %% Run eunit provider prehooks
    Providers = rebar_state:providers(State),
    Cwd = rebar_dir:get_cwd(),
    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State),

    case validate_tests(State, Tests) of
        {ok, T} ->
            case run_tests(State, T) of
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

run_tests(State, Tests) ->
    T = translate_paths(State, Tests),
    EUnitOpts = resolve_eunit_opts(State),
    ?DEBUG("eunit_tests ~p", [T]),
    ?DEBUG("eunit_opts  ~p", [EUnitOpts]),
    Result = eunit:test(T, EUnitOpts),
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
format_error({badconfig, {Msg, {Value, Key}}}) ->
    io_lib:format(Msg, [Value, Key]);
format_error({error, Error}) ->
    format_error({error_running_tests, Error}).

%% ===================================================================
%% Internal functions
%% ===================================================================

prepare_tests(State) ->
    %% parse and translate command line tests
    CmdTests = resolve_tests(State),
    CfgTests = cfg_tests(State),
    ProjectApps = rebar_state:project_apps(State),
    %% prioritize tests to run first trying any command line specified
    %% tests falling back to tests specified in the config file finally
    %% running a default set if no other tests are present
    select_tests(State, ProjectApps, CmdTests, CfgTests).

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

cfg_tests(State) ->
    case rebar_state:get(State, eunit_tests, []) of
        Tests when is_list(Tests) -> Tests;
        Wrong ->
            %% probably a single non list term
            ?PRV_ERROR({badconfig, {"Value `~p' of option `~p' must be a list", {Wrong, eunit_tests}}})
    end.

select_tests(_State, _ProjectApps, {error, _} = Error, _) -> Error;
select_tests(_State, _ProjectApps, _, {error, _} = Error) -> Error;
select_tests(State, ProjectApps, [], []) -> {ok, default_tests(State, ProjectApps)};
select_tests(_State, _ProjectApps, [], Tests)  -> {ok, Tests};
select_tests(_State, _ProjectApps, Tests, _) -> {ok, Tests}.

default_tests(State, Apps) ->
    Tests = set_apps(Apps, []),
    BareTest = filename:join([rebar_state:dir(State), "test"]),
    F = fun(App) -> rebar_app_info:dir(App) == rebar_state:dir(State) end,
    case filelib:is_dir(BareTest) andalso not lists:any(F, Apps) of
        %% `test` dir at root of project is already scheduled to be
        %%  included or `test` does not exist
        false -> lists:reverse(Tests);
        %% need to add `test` dir at root to dirs to be included
        true  -> lists:reverse([{dir, BareTest}|Tests])
    end.

set_apps([], Acc) -> Acc;
set_apps([App|Rest], Acc) ->
    AppName = list_to_atom(binary_to_list(rebar_app_info:name(App))),
    set_apps(Rest, [{application, AppName}|Acc]).

inject_eunit_state(State, {ok, Tests}) ->
    Apps = rebar_state:project_apps(State),
    case inject_eunit_state(State, Apps, []) of
        {ok, {NewState, ModdedApps}} ->
            test_dirs(NewState, ModdedApps, Tests);
        {error, _} = Error           -> Error
    end;
inject_eunit_state(_State, Error) -> Error.

inject_eunit_state(State, [App|Rest], Acc) ->
    case inject(rebar_app_info:opts(App)) of
        {error, _} = Error -> Error;
        NewOpts            ->
            NewApp = rebar_app_info:opts(App, NewOpts),
            inject_eunit_state(State, Rest, [NewApp|Acc])
    end;
inject_eunit_state(State, [], Acc) ->
    case inject(rebar_state:opts(State)) of
        {error, _} = Error -> Error;
        NewOpts            -> {ok, {rebar_state:opts(State, NewOpts), lists:reverse(Acc)}}
    end.

opts(Opts, Key, Default) ->
    case rebar_opts:get(Opts, Key, Default) of
        Vs when is_list(Vs) -> Vs;
        Wrong ->
            ?PRV_ERROR({badconfig, {"Value `~p' of option `~p' must be a list", {Wrong, Key}}})
    end.

inject(Opts) -> erl_opts(Opts).

erl_opts(Opts) ->
    %% append `eunit_compile_opts` to app defined `erl_opts`
    ErlOpts = opts(Opts, erl_opts, []),
    EUnitOpts = opts(Opts, eunit_compile_opts, []),
    case append(EUnitOpts, ErlOpts) of
        {error, _} = Error -> Error;
        NewErlOpts         -> first_files(rebar_opts:set(Opts, erl_opts, NewErlOpts))
    end.

first_files(Opts) ->
    %% append `eunit_first_files` to app defined `erl_first_files`
    FirstFiles = opts(Opts, erl_first_files, []),
    EUnitFirstFiles = opts(Opts, eunit_first_files, []),
    case append(EUnitFirstFiles, FirstFiles) of
        {error, _} = Error -> Error;
        NewFirstFiles  -> rebar_opts:set(Opts, erl_first_files, NewFirstFiles)
    end.

append({error, _} = Error, _) -> Error;
append(_, {error, _} = Error) -> Error;
append(A, B) -> A ++ B.

test_dirs(State, Apps, []) -> rebar_state:project_apps(State, Apps);
test_dirs(State, Apps, [{dir, Dir}|Rest]) ->
    %% insert `Dir` into an app if relative, or the base state if not
    %% app relative but relative to the root or not at all if outside
    %% project scope
    {NewState, NewApps} = maybe_inject_test_dir(State, [], Apps, Dir),
    test_dirs(NewState, NewApps, Rest);
test_dirs(State, Apps, [{file, File}|Rest]) ->
    Dir = filename:dirname(File),
    {NewState, NewApps} = maybe_inject_test_dir(State, [], Apps, Dir),
    test_dirs(NewState, NewApps, Rest);
test_dirs(State, Apps, [_|Rest]) -> test_dirs(State, Apps, Rest).

maybe_inject_test_dir(State, AppAcc, [App|Rest], Dir) ->
    case rebar_file_utils:path_from_ancestor(Dir, rebar_app_info:dir(App)) of
        {ok, Path} ->
            Opts = inject_test_dir(rebar_app_info:opts(App), Path),
            {State, AppAcc ++ [rebar_app_info:opts(App, Opts)] ++ Rest};
        {error, badparent} ->
            maybe_inject_test_dir(State, AppAcc ++ [App], Rest, Dir)
    end;
maybe_inject_test_dir(State, AppAcc, [], Dir) ->
    case rebar_file_utils:path_from_ancestor(Dir, rebar_state:dir(State)) of
        {ok, Path} ->
            Opts = inject_test_dir(rebar_state:opts(State), Path),
            {rebar_state:opts(State, Opts), AppAcc};
        {error, badparent} ->
            {State, AppAcc}
    end.

inject_test_dir(Opts, Dir) ->
    %% append specified test targets to app defined `extra_src_dirs`
    ExtraSrcDirs = rebar_dir:extra_src_dirs(Opts),
    rebar_opts:set(Opts, extra_src_dirs, ExtraSrcDirs ++ [Dir]).

compile({error, _} = Error) -> Error;
compile(State) ->
    case rebar_prv_compile:do(State) of
        %% successfully compiled apps
        {ok, S} ->
            ok = maybe_cover_compile(S),
            {ok, S};
        %% this should look like a compiler error, not an eunit error
        Error   -> Error
    end.

validate_tests(State, {ok, Tests}) ->
    gather_tests(fun(Elem) -> validate(State, Elem) end, Tests, []);
validate_tests(_State, Error) -> Error.

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

validate(State, {application, App}) ->
    validate_app(State, App);
validate(State, {dir, Dir}) ->
    validate_dir(State, Dir);
validate(State, {file, File}) ->
    validate_file(State, File);
validate(State, {module, Module}) ->
    validate_module(State, Module);
validate(State, {suite, Module}) ->
    validate_module(State, Module);
validate(State, Module) when is_atom(Module) ->
    validate_module(State, Module);
validate(State, Path) when is_list(Path) ->
    case ec_file:is_dir(Path) of
        true  -> validate(State, {dir, Path});
        false -> validate(State, {file, Path})
    end;
%% unrecognized tests should be included. if they're invalid eunit will error
%% and rebar.config may contain arbitrarily complex tests that are effectively
%% unvalidatable
validate(_State, _Test) -> ok.

validate_app(State, AppName) ->
    ProjectApps = rebar_state:project_apps(State),
    validate_app(State, ProjectApps, AppName).

validate_app(_State, [], AppName) ->
    {error, lists:concat(["Application `", AppName, "' not found in project."])};
validate_app(State, [App|Rest], AppName) ->
    case AppName == binary_to_atom(rebar_app_info:name(App), unicode) of
        true  -> ok;
        false -> validate_app(State, Rest, AppName)
    end.

validate_dir(State, Dir) ->
    case ec_file:is_dir(filename:join([rebar_state:dir(State), Dir])) of
        true  -> ok;
        false -> {error, lists:concat(["Directory `", Dir, "' not found."])}
    end.

validate_file(State, File) ->
    case ec_file:exists(filename:join([rebar_state:dir(State), File])) of
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
    EUnitOpts1 = case proplists:get_value(verbose, Opts, false) of
                    true  -> set_verbose(EUnitOpts);
                    false -> EUnitOpts
                 end,
    IsVerbose = lists:member(verbose, EUnitOpts1),
    case proplists:get_value(eunit_formatters, EUnitOpts1, not IsVerbose) of
        true  -> custom_eunit_formatters(EUnitOpts1);
        false -> EUnitOpts1
    end.

custom_eunit_formatters(Opts) ->
    %% If `report` is already set then treat that like `eunit_formatters` is false
    case lists:keymember(report, 1, Opts) of
        true -> Opts;
        false -> [no_tty, {report, {eunit_progress, [colored, profile]}} | Opts]
    end.

set_verbose(Opts) ->
    %% if `verbose` is already set don't set it again
    case lists:member(verbose, Opts) of
        true  -> Opts;
        false -> [verbose] ++ Opts
    end.

translate_paths(State, Tests) -> translate_paths(State, Tests, []).

translate_paths(_State, [], Acc) -> lists:reverse(Acc);
translate_paths(State, [{dir, Dir}|Rest], Acc) ->
    Apps = rebar_state:project_apps(State),
    translate_paths(State, Rest, [translate(State, Apps, Dir)|Acc]);
translate_paths(State, [{file, File}|Rest], Acc) ->
    Dir = filename:dirname(File),
    Apps = rebar_state:project_apps(State),
    translate_paths(State, Rest, [translate(State, Apps, Dir)|Acc]);
translate_paths(State, [Test|Rest], Acc) ->
    translate_paths(State, Rest, [Test|Acc]).

translate(State, [App|Rest], Dir) ->
    case rebar_file_utils:path_from_ancestor(Dir, rebar_app_info:dir(App)) of
        {ok, Path}         -> {dir, filename:join([rebar_app_info:out_dir(App), Path])};
        {error, badparent} -> translate(State, Rest, Dir)
    end;
translate(State, [], Dir) ->
    case rebar_file_utils:path_from_ancestor(Dir, rebar_state:dir(State)) of
        {ok, Path}         -> {dir, filename:join([rebar_dir:base_dir(State), "extras", Path])};
        %% not relative, leave as is
        {error, badparent} -> {dir, Dir}
    end.

maybe_cover_compile(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    State1 = case proplists:get_value(cover, RawOpts, false) of
        true  -> rebar_state:set(State, cover_enabled, true);
        false -> State
    end,
    rebar_prv_cover:maybe_cover_compile(State1).

maybe_write_coverdata(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    State1 = case proplists:get_value(cover, RawOpts, false) of
        true  -> rebar_state:set(State, cover_enabled, true);
        false -> State
    end,
    rebar_prv_cover:maybe_write_coverdata(State1, ?PROVIDER).

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
