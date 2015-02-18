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
                                 {profiles, [test]}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?INFO("Performing EUnit tests...", []),
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    Opts = transform_opts(RawOpts, State),
    TestApps = filter_checkouts(rebar_state:project_apps(State)),
    OutDir = proplists:get_value(outdir, Opts, default_test_dir(State)),
    ?DEBUG("Compiling EUnit instrumented modules in: ~p", [OutDir]),
    lists:foreach(fun(App) ->
                      AppDir = rebar_app_info:dir(App),
                      C = rebar_config:consult(AppDir),
                      S = rebar_state:new(State, C, AppDir),
                      %% combine `erl_first_files` and `eunit_first_files` and adjust
                      %% compile opts to include `eunit_compile_opts`, `{d, 'TEST'}`
                      %% and `{src_dirs, "test"}`
                      TestState = first_files(test_state(S, OutDir)),
                      ok = rebar_erlc_compiler:compile(TestState, AppDir)
                  end, TestApps),
    ok = maybe_compile_extra_tests(TestApps, State, OutDir),
    Path = code:get_path(),
    true = code:add_patha(OutDir),
    EUnitOpts = resolve_eunit_opts(State, Opts),
    AppsToTest = [{application, erlang:binary_to_atom(rebar_app_info:name(App), unicode)}
                  || App <- TestApps],
    handle_results(eunit:test(AppsToTest, EUnitOpts)),
    true = code:set_path(Path),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(error) ->
    io_lib:format("Error running tests", []);
format_error({error, Reason}) ->
    io_lib:format("Error running tests: ~p", [Reason]).

eunit_opts(_State) ->
    [{outdir, $o, "outdir", string, help(outdir)},
     {verbose, $v, "verbose", boolean, help(verbose)}].

help(outdir) -> "Output directory for EUnit compiled modules";
help(verbose) -> "Verbose output".

transform_opts(Opts, State) -> transform_opts(Opts, State, []).

transform_opts([], _State, Acc) -> Acc;
transform_opts([{outdir, Path}|Rest], State, Acc) ->
    NewAcc = case filename:pathtype(Path) of
        absolute -> [{outdir, Path}] ++ Acc;
        _ -> [{outdir, filename:join([rebar_state:dir(State), Path])}] ++ Acc
    end,
    transform_opts(Rest, State, NewAcc);
transform_opts([{Key, Val}|Rest], State, Acc) ->
    transform_opts(Rest, State, [{Key, Val}|Acc]).

filter_checkouts(Apps) -> filter_checkouts(Apps, []).

filter_checkouts([], Acc) -> lists:reverse(Acc);
filter_checkouts([App|Rest], Acc) ->
    AppDir = filename:absname(rebar_app_info:dir(App)),
    CheckoutsDir = filename:absname("_checkouts"),
    case lists:prefix(CheckoutsDir, AppDir) of
        true -> filter_checkouts(Rest, Acc);
        false -> filter_checkouts(Rest, [App|Acc])
    end.

default_test_dir(State) ->
    Tmp = rebar_file_utils:system_tmpdir(),
    Root = filename:join([rebar_state:dir(State), Tmp]),
    Project = filename:basename(rebar_state:dir(State)),
    OutDir = filename:join([Root, Project ++ "_rebar3_eunit"]),
    ok = rebar_file_utils:reset_dir(OutDir),
    OutDir.

test_state(State, TmpDir) ->
    ErlOpts = rebar_state:get(State, eunit_compile_opts, []) ++
        rebar_utils:erl_opts(State),
    ErlOpts1 = [{outdir, TmpDir}] ++
        add_test_dir(ErlOpts),
    TestOpts = safe_define_test_macro(ErlOpts1),
    rebar_state:set(State, erl_opts, TestOpts).

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
    BaseFirst = rebar_state:get(State, erl_first_files, []),
    EUnitFirst = rebar_state:get(State, eunit_first_files, []),
    rebar_state:set(State, erl_first_files, BaseFirst ++ EUnitFirst).

resolve_eunit_opts(State, Opts) ->
    EUnitOpts = rebar_state:get(State, eunit_opts, []),
    case lists:member({verbose, true}, Opts) of
        true -> set_verbose(EUnitOpts);
        false -> EUnitOpts
    end.

set_verbose(Opts) ->
    case lists:member(verbose, Opts) of
        true -> Opts;
        false -> [verbose] ++ Opts
    end.

maybe_compile_extra_tests(TestApps, State, OutDir) ->
    F = fun(App) -> rebar_app_info:dir(App) == rebar_dir:get_cwd() end,
    case lists:filter(F, TestApps) of
        %% compile just the `test` and extra test directories of the base dir
        [] ->
            ErlOpts = rebar_state:get(State, common_test_compile_opts, []) ++
                      rebar_utils:erl_opts(State),
            TestOpts = [{outdir, OutDir}] ++
                       [{src_dirs, ["test"]}] ++
                       safe_define_test_macro(lists:keydelete(src_dirs, 1, ErlOpts)),
            TestState = first_files(rebar_state:set(State, erl_opts, TestOpts)),
            rebar_erlc_compiler:compile(TestState, rebar_dir:get_cwd());
        %% already compiled `./test` so do nothing
        _ -> ok
    end.

handle_results(error) ->
    ?WARN("Error running tests", []);
handle_results({error, Reason}) ->
    ?WARN("Error running tests: ~p", [Reason]);
handle_results(_) -> ok.
