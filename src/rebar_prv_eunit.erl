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
                                 {profile, test}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?INFO("Performing EUnit tests...", []),
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    Opts = transform_opts(RawOpts, State),
    ProjectApps = rebar_state:project_apps(State),
    OutDir = case proplists:get_value(outdir, Opts, undefined) of
        undefined -> filename:join([rebar_state:dir(State),
                     ec_file:insecure_mkdtemp()]);
        Path -> Path
    end,
    ?DEBUG("Compiling EUnit instrumented modules in: ~p", [OutDir]),
    lists:foreach(fun(App) ->
                      AppDir = rebar_app_info:dir(App),
                      C = rebar_config:consult(AppDir),
                      S = rebar_state:new(State, C, AppDir),
                      %% combine `erl_first_files` and `eunit_first_files` and adjust
                      %% compile opts to include `eunit_compile_opts`, `{d, 'TEST'}`
                      %% and `{src_dirs, "test"}`
                      TestState = first_files(test_opts(S, OutDir)),
                      ok = rebar_erlc_compiler:compile(TestState, AppDir),
                      true = code:add_patha(OutDir)
                  end, ProjectApps),
    EUnitOpts = resolve_eunit_opts(State, Opts),
    case handle_results(eunit:test({dir, OutDir}, EUnitOpts)) of
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
    [{outdir, $o, "outdir", string, help(outdir)},
     {verbose, $v, "verbose", boolean, help(verbose)}].

help(outdir) -> "Output directory for EUnit compiled modules";
help(verbose) -> "Verbose output".

transform_opts(Opts, State) -> transform_opts(Opts, State, []).

transform_opts([], _State, Acc) -> Acc;
transform_opts([{dir, Path}|Rest], State, Acc) ->
    NewAcc = case filename:pathtype(Path) of
        absolute -> [{dir, Path}] ++ Acc;
        _ -> [{dir, filename:join([rebar_state:dir(State), Path])}] ++ Acc
    end,
    transform_opts(Rest, State, NewAcc);
transform_opts([{Key, Val}|Rest], State, Acc) ->
    transform_opts(Rest, State, [{Key, Val}|Acc]).

test_opts(State, TmpDir) ->
    ErlOpts = rebar_state:get(State, eunit_compile_opts, []) ++
              rebar_utils:erl_opts(State),
    TestOpts = [{outdir, TmpDir}] ++
               add_test_dir(ErlOpts) ++
               safe_define_test_macro(ErlOpts),
    rebar_state:set(State, erl_opts, TestOpts).

add_test_dir(Opts) ->
    %% if no src_dirs are set we have to specify `src` or it won't
    %% be built
    case proplists:append_values(src_dirs, Opts) of
        [] -> [{src_dirs, ["src", "test"]}];
        _ -> [{src_dirs, ["test"]}]
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
    rebar_state:set(State, erl_first_modules, BaseFirst ++ EUnitFirst).

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

handle_results(ok) -> ok;
handle_results(error) ->
    {error, unknown_error};
handle_results({error, Reason}) ->
    {error, {error_running_tests, Reason}}.

