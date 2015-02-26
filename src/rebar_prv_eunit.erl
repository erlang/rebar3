%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_eunit).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

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
                                 {bare, false},
                                 {example, "rebar eunit"},
                                 {short_desc, "Run EUnit Tests."},
                                 {desc, ""},
                                 {opts, eunit_opts(State)},
                                 {profiles, [test, eunit]}]),
    State1 = rebar_state:add_provider(State, Provider),
    State2 = rebar_state:add_to_profile(State1, eunit, test_state(State1)),
    {ok, State2}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?INFO("Performing EUnit tests...", []),
    {Opts, _} = rebar_state:command_parsed_args(State),
    EUnitOpts = resolve_eunit_opts(State, Opts),
    TestApps = filter_checkouts(rebar_state:project_apps(State)),
    ok = maybe_compile_extra_tests(TestApps, State),
    AppsToTest = test_dirs(TestApps),
    Result = eunit:test(AppsToTest, EUnitOpts),
    case handle_results(Result) of
        {error, Reason} ->
            {error, {?MODULE, Reason}};
        ok ->
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error(unknown_error) ->
    io_lib:format("Error running tests", []);
format_error({error_running_tests, Reason}) ->
    io_lib:format("Error running tests: ~p", [Reason]).

eunit_opts(_State) ->
    [{verbose, $v, "verbose", boolean, help(verbose)}].

help(verbose) -> "Verbose output".

filter_checkouts(Apps) -> filter_checkouts(Apps, []).

filter_checkouts([], Acc) -> lists:reverse(Acc);
filter_checkouts([App|Rest], Acc) ->
    AppDir = filename:absname(rebar_app_info:dir(App)),
    CheckoutsDir = filename:absname("_checkouts"),
    case lists:prefix(CheckoutsDir, AppDir) of
        true -> filter_checkouts(Rest, Acc);
        false -> filter_checkouts(Rest, [App|Acc])
    end.

resolve_eunit_opts(State, Opts) ->
    EUnitOpts = rebar_state:get(State, eunit_opts, []),
    case proplists:get_value(verbose, Opts, false) of
        true -> set_verbose(EUnitOpts);
        false -> EUnitOpts
    end.

test_dirs(TestApps) ->
    %% we need to add "./ebin" but only if it's not already due to be added
    F = fun(App) -> rebar_app_info:dir(App) == rebar_dir:get_cwd() end,
    case lists:any(F, TestApps) of
        true  -> test_dirs(TestApps, []);
        false -> [{dir, "ebin"}|test_dirs(TestApps, [])]
    end.

test_dirs([], Acc) -> lists:reverse(Acc);
test_dirs([App|Rest], Acc) ->
    test_dirs(Rest, [{dir, rebar_app_info:ebin_dir(App)}|Acc]).

test_state(State) ->
    ErlOpts = rebar_state:get(State, eunit_compile_opts, []),
    ErlOpts1 = add_test_dir(ErlOpts),
    TestOpts = safe_define_test_macro(ErlOpts1),
    first_files(State) ++ [{erl_opts, TestOpts}].

add_test_dir(Opts) ->
    %% if no src_dirs are set we have to specify `src` or it won't
    %% be built
    case proplists:append_values(src_dirs, Opts) of
        [] -> [{src_dirs, ["src", "test"]} | Opts];
        _ -> [{src_dirs, ["test"]} | Opts]
    end.

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

set_verbose(Opts) ->
    %% if `verbose` is already set don't set it again
    case lists:member(verbose, Opts) of
        true -> Opts;
        false -> [verbose] ++ Opts
    end.

maybe_compile_extra_tests(TestApps, State) ->
    F = fun(App) -> rebar_app_info:dir(App) == rebar_dir:get_cwd() end,
    case lists:filter(F, TestApps) of
        %% compile just the `test` and extra test directories of the base dir
        [] -> rebar_erlc_compiler:compile(State, rebar_dir:get_cwd());
        %% already compiled `./test` so do nothing
        _  -> ok
    end.

handle_results(ok) -> ok;
handle_results(error) ->
    {error, unknown_error};
handle_results({error, Reason}) ->
    {error, {error_running_tests, Reason}}.
