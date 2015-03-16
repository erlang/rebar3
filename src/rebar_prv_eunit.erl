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
                                 {bare, false},
                                 {example, "rebar3 eunit"},
                                 {short_desc, "Run EUnit Tests."},
                                 {desc, ""},
                                 {opts, eunit_opts(State)},
                                 {profiles, [test]}]),
    State1 = rebar_state:add_provider(State, Provider),
    State2 = rebar_state:add_to_profile(State1, test, test_state(State1)),
    {ok, State2}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?INFO("Performing EUnit tests...", []),
    {Opts, _} = rebar_state:command_parsed_args(State),
    EUnitOpts = resolve_eunit_opts(State, Opts),
    TestApps = filter_checkouts(rebar_state:project_apps(State)),
    ok = compile_tests(State, TestApps),
    ok = maybe_cover_compile(State, Opts),
    AppsToTest = test_dirs(State, TestApps),
    Result = eunit:test(AppsToTest, EUnitOpts),
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

eunit_opts(_State) ->
    [{cover, $c, "cover", boolean, help(cover)},
     {verbose, $v, "verbose", boolean, help(verbose)}].

help(cover) -> "Generate cover data";
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

test_dirs(State, TestApps) ->
    %% we need to add "./ebin" if it exists but only if it's not already
    %%  due to be added
    F = fun(App) -> rebar_app_info:dir(App) =/= rebar_dir:get_cwd() end,
    BareEbin = filename:join([rebar_dir:base_dir(State), "ebin"]),
    case lists:any(F, TestApps) andalso filelib:is_dir(BareEbin) of
        false -> application_dirs(TestApps, []);
        true  -> [{dir, BareEbin}|application_dirs(TestApps, [])]
    end.

application_dirs([], Acc) -> lists:reverse(Acc);
application_dirs([App|Rest], Acc) ->
    AppName = list_to_atom(binary_to_list(rebar_app_info:name(App))),
    application_dirs(Rest, [{application, AppName}|Acc]).

test_state(State) ->
    ErlOpts = rebar_state:get(State, eunit_compile_opts, []),
    TestOpts = safe_define_test_macro(ErlOpts),
    first_files(State) ++ [{erl_opts, TestOpts}].

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

compile_tests(State, TestApps) ->
    F = fun(AppInfo) ->
        AppDir = rebar_app_info:dir(AppInfo),
        S = case rebar_app_info:state(AppInfo) of
            undefined ->
                C = rebar_config:consult(AppDir),
                rebar_state:new(State, C, AppDir);
            AppState ->
                AppState
        end,
        ok = rebar_erlc_compiler:compile(replace_src_dirs(S),
                                         ec_cnv:to_list(rebar_app_info:out_dir(AppInfo)))
    end,
    lists:foreach(F, TestApps),
    case filelib:is_dir(filename:join([rebar_dir:get_cwd(), "test"])) of
        true  -> compile_bare_tests(State, TestApps);
        false -> ok
    end.

compile_bare_tests(State, TestApps) ->
    F = fun(App) -> rebar_app_info:dir(App) == rebar_dir:get_cwd() end,
    case lists:filter(F, TestApps) of
        %% compile and link just the `test` directory of the base dir
        [] ->
            Source = filename:join([rebar_dir:get_cwd(), "test"]),
            Target = filename:join([rebar_dir:base_dir(State), "test"]),
            ok = rebar_file_utils:symlink_or_copy(Source, Target),
            rebar_erlc_compiler:compile(replace_src_dirs(State),
                                        rebar_dir:base_dir(State),
                                        filename:join([rebar_dir:base_dir(State), "ebin"]));
        %% already compiled `./test` so do nothing
        _  -> ok
    end.

replace_src_dirs(State) ->
    %% replace any `src_dirs` with just the `test` dir
    ErlOpts = rebar_state:get(State, erl_opts, []),
    StrippedOpts = lists:keydelete(src_dirs, 1, ErlOpts),
    rebar_state:set(State, erl_opts, [{src_dirs, ["test"]}|StrippedOpts]).

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
