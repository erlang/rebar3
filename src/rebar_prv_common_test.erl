%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_common_test).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, ct).
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
                                 {example, "rebar ct"},
                                 {short_desc, "Run Common Tests."},
                                 {desc, ""},
                                 {opts, ct_opts(State)},
                                 {profiles, [test]}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?INFO("Running Common Test suites...", []),
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    {InDirs, OutDir} = split_ct_dirs(State, RawOpts),
    Opts = transform_opts(RawOpts),
    TestApps = filter_checkouts(rebar_state:project_apps(State)),
    ok = create_dirs(Opts),
    ?DEBUG("Compiling Common Test suites in: ~p", [OutDir]),
    lists:foreach(fun(App) ->
                      AppDir = rebar_app_info:dir(App),
                      C = rebar_config:consult(AppDir),
                      S = rebar_state:new(State, C, AppDir),
                      %% combine `erl_first_files` and `common_test_first_files` and
                      %% adjust compile opts to include `common_test_compile_opts`
                      %% and `{src_dirs, "test"}`
                      TestState = test_state(S, InDirs, OutDir),
                      ok = rebar_erlc_compiler:compile(TestState, AppDir)
                  end, TestApps),
    ok = maybe_compile_extra_tests(TestApps, State, InDirs, OutDir),
    Path = code:get_path(),
    true = code:add_patha(OutDir),
    CTOpts = resolve_ct_opts(State, Opts, OutDir),
    Result = handle_results(ct:run_test(CTOpts)),
    true = code:set_path(Path),
    case Result of
        {error, Reason} ->
            {error, {?MODULE, Reason}};
        ok ->
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error({failures_running_tests, FailedCount}) ->
    io_lib:format("Failures occured running tests: ~p", [FailedCount]);
format_error({error_running_tests, Reason}) ->
    io_lib:format("Error running tests: ~p", [Reason]).

ct_opts(State) ->
    DefaultLogsDir = filename:join([rebar_state:dir(State), "logs"]),
    [{dir, undefined, "dir", string, help(dir)}, %% comma-seperated list
     {outdir, undefined, "outdir", string, help(outdir)}, %% string
     {suite, undefined, "suite", string, help(suite)}, %% comma-seperated list
     {group, undefined, "group", string, help(group)}, %% comma-seperated list
     {testcase, undefined, "case", string, help(testcase)}, %% comma-seperated list
     {spec, undefined, "spec", string, help(spec)}, %% comma-seperated list
     {join_specs, undefined, "join_specs", boolean, help(join_specs)}, %% Boolean
     {label, undefined, "label", string, help(label)}, %% String
     {config, undefined, "config", string, help(config)}, %% comma-seperated list
     {userconfig, undefined, "userconfig", string, help(userconfig)}, %% [{CallbackMod, CfgStrings}] | {CallbackMod, CfgStrings}
     {allow_user_terms, undefined, "allow_user_terms", boolean, help(allow_user_terms)}, %% Bool
     {logdir, undefined, "logdir", {string, DefaultLogsDir}, help(logdir)}, %% dir
     {logopts, undefined, "logopts", string, help(logopts)}, %% enum, no_nl | no_src
     {verbosity, undefined, "verbosity", string, help(verbosity)}, %% Integer OR [{Category, VLevel}]
     {silent_connections, undefined, "silent_connections", string,
      help(silent_connections)}, % all OR %% comma-seperated list
     {stylesheet, undefined, "stylesheet", string, help(stylesheet)}, %% file
     {cover, undefined, "cover", string, help(cover)}, %% file
     {cover_stop, undefined, "cover_stop", boolean, help(cover_stop)}, %% Boolean
     {event_handler, undefined, "event_handler", string, help(event_handler)}, %% EH | [EH] WHERE EH atom() | {atom(), InitArgs} | {[atom()], InitArgs}
     {include, undefined, "include", string, help(include)}, % comma-seperated list
     {abort_if_missing_suites, undefined, "abort_if_missing_suites", {boolean, true},
      help(abort_if_missing_suites)}, %% boolean
     {multiply_timetraps, undefined, "multiply_timetraps", integer,
      help(multiply_timetraps)}, %% integer
     {scale_timetraps, undefined, "scale_timetraps", boolean, help(scale_timetraps)}, %% Boolean
     {create_priv_dir, undefined, "create_priv_dir", string, help(create_priv_dir)}, %% enum: auto_per_run | auto_per_tc | manual_per_tc
     {repeat, undefined, "repeat", integer, help(repeat)}, %% integer
     {duration, undefined, "duration", string, help(duration)}, % format: HHMMSS
     {until, undefined, "until", string, help(until)}, %% format: YYMoMoDD[HHMMSS]
     {force_stop, undefined, "force_stop", string, help(force_stop)}, % enum: skip_rest, bool
     {basic_html, undefined, "basic_html", boolean, help(basic_html)}, %% Booloean
     {ct_hooks, undefined, "ct_hooks", string, help(ct_hooks)} %% List: [CTHModule | {CTHModule, CTHInitArgs}] where CTHModule is atom CthInitArgs is term
    ].

help(outdir) ->
    "Output directory for compiled modules";
help(dir) ->
    "List of additional directories containing test suites";
help(suite) ->
    "List of test suites to run";
help(group) ->
    "List of test groups to run";
help(testcase) ->
    "List of test cases to run";
help(spec) ->
    "List of test specs to run";
help(join_specs) ->
    ""; %% ??
help(label) ->
    "Test label";
help(config) ->
    "List of config files";
help(allow_user_terms) ->
    ""; %% ??
help(logdir) ->
    "Log folder";
help(logopts) ->
    ""; %% ??
help(verbosity) ->
    "Verbosity";
help(silent_connections) ->
    ""; %% ??
help(stylesheet) ->
    "Stylesheet to use for test results";
help(cover) ->
    "Cover file to use";
help(cover_stop) ->
    ""; %% ??
help(event_handler) ->
    "Event handlers to attach to the runner";
help(include) ->
    "Include folder";
help(abort_if_missing_suites) ->
    "Abort if suites are missing";
help(multiply_timetraps) ->
    ""; %% ??
help(scale_timetraps) ->
    ""; %% ??
help(create_priv_dir) ->
    ""; %% ??
help(repeat) ->
    "How often to repeat tests";
help(duration) ->
    "Max runtime (format: HHMMSS)";
help(until) ->
    "Run until (format: HHMMSS)";
help(force_stop) ->
    "Force stop after time";
help(basic_html) ->
    "Show basic HTML";
help(ct_hooks) ->
    "";
help(userconfig) ->
    "".

split_ct_dirs(State, RawOpts) ->
    %% preserve the override nature of command line opts by only checking
    %% `rebar.config` defined additional test dirs if none are defined via
    %% command line flag
    InDirs = case proplists:get_value(dir, RawOpts) of
        undefined ->
            CTOpts = rebar_state:get(State, common_test_opts, []),
            proplists:get_value(dir, CTOpts, []);
        Dirs -> split_string(Dirs)
    end,
    OutDir = proplists:get_value(outdir, RawOpts, default_test_dir(State)),
    {InDirs, OutDir}.

default_test_dir(State) ->
    Tmp = case erlang:system_info(system_architecture) of
        "win32" ->
            "./tmp";
        _SysArch ->
            "/tmp"
    end,
    Root = filename:join([rebar_state:dir(State), Tmp]),
    Project = filename:basename(rebar_state:dir(State)),
    OutDir = filename:join([Root, Project ++ "_rebar3_ct"]),
    %% delete the directory if it exists so tests run with clean state
    _ = ec_file:remove(OutDir, [recursive]),
    %% recreate the directory
    ok = filelib:ensure_dir(filename:join([OutDir, "dummy.beam"])),
    OutDir.

transform_opts(Opts) ->
    transform_opts(Opts, []).

transform_opts([], Acc) -> Acc;
%% drop `outdir` so it's not passed to common_test
transform_opts([{outdir, _}|Rest], Acc) ->
    transform_opts(Rest, Acc);
transform_opts([{ct_hooks, CtHooks}|Rest], Acc) ->
    transform_opts(Rest, [{ct_hooks, parse_term(CtHooks)}|Acc]);
transform_opts([{force_stop, "skip_rest"}|Rest], Acc) ->
    transform_opts(Rest, [{force_stop, skip_rest}|Acc]);
transform_opts([{force_stop, _}|Rest], Acc) ->
    transform_opts(Rest, [{force_stop, true}|Acc]);
transform_opts([{repeat, Repeat}|Rest], Acc) ->
    transform_opts(Rest, [{repeat,
                           ec_cnv:to_integer(Repeat)}|Acc]);
transform_opts([{create_priv_dir, CreatePrivDir}|Rest], Acc) ->
    transform_opts(Rest, [{create_priv_dir,
                           to_atoms(CreatePrivDir)}|Acc]);
transform_opts([{multiply_timetraps, MultiplyTimetraps}|Rest], Acc) ->
    transform_opts(Rest, [{multiply_timetraps,
                           ec_cnv:to_integer(MultiplyTimetraps)}|Acc]);
transform_opts([{event_handler, EventHandler}|Rest], Acc) ->
    transform_opts(Rest, [{event_handler, parse_term(EventHandler)}|Acc]);
transform_opts([{silent_connections, "all"}|Rest], Acc) ->
    transform_opts(Rest, [{silent_connections, all}|Acc]);
transform_opts([{silent_connections, SilentConnections}|Rest], Acc) ->
    transform_opts(Rest, [{silent_connections,
                           to_atoms(split_string(SilentConnections))}|Acc]);
transform_opts([{verbosity, Verbosity}|Rest], Acc) ->
    transform_opts(Rest, [{verbosity, parse_term(Verbosity)}|Acc]);
transform_opts([{logopts, LogOpts}|Rest], Acc) ->
    transform_opts(Rest, [{logopts, to_atoms(split_string(LogOpts))}|Acc]);
transform_opts([{userconfig, UserConfig}|Rest], Acc) ->
    transform_opts(Rest, [{userconfig, parse_term(UserConfig)}|Acc]);
transform_opts([{testcase, Testcase}|Rest], Acc) ->
    transform_opts(Rest, [{testcase, to_atoms(split_string(Testcase))}|Acc]);
transform_opts([{group, Group}|Rest], Acc) -> % @TODO handle ""
    % Input is a list or an atom. It can also be a nested list.
    transform_opts(Rest, [{group, parse_term(Group)}|Acc]);
transform_opts([{Key, Val}|Rest], Acc) when is_list(Val) ->
    % Default to splitting a string on comma, that works fine for both flat
    % lists of which there are many and single-items.
    Val1 = case split_string(Val) of
               [Val2] ->
                   Val2;
               Val2 ->
                   Val2
           end,
    transform_opts(Rest, [{Key, Val1}|Acc]);
transform_opts([{Key, Val}|Rest], Acc) ->
    transform_opts(Rest, [{Key, Val}|Acc]).

to_atoms(List) ->
    lists:map(fun(X) -> list_to_atom(X) end, List).

split_string(String) ->
    string:tokens(String, ",").

parse_term(String) ->
    String1 = "[" ++ String ++ "].",
    {ok, Tokens, _} = erl_scan:string(String1),
    case erl_parse:parse_term(Tokens) of
        {ok, [Terms]} ->
            Terms;
        Term ->
            Term
    end.

filter_checkouts(Apps) -> filter_checkouts(Apps, []).

filter_checkouts([], Acc) -> lists:reverse(Acc);
filter_checkouts([App|Rest], Acc) ->
    AppDir = filename:absname(rebar_app_info:dir(App)),
    CheckoutsDir = filename:absname("_checkouts"),
    case lists:prefix(CheckoutsDir, AppDir) of
        true -> filter_checkouts(Rest, Acc);
        false -> filter_checkouts(Rest, [App|Acc])
    end.

create_dirs(Opts) ->
    LogDir = proplists:get_value(logdir, Opts),
    ensure_dir([LogDir]),
    ok.

ensure_dir([]) -> ok;
ensure_dir([Dir|Rest]) ->
    case ec_file:is_dir(Dir) of
        true ->
            ok;
        false ->
            ec_file:mkdir_path(Dir)
    end,
    ensure_dir(Rest).

test_state(State, InDirs, OutDir) ->
    ErlOpts = rebar_state:get(State, common_test_compile_opts, []) ++
              rebar_utils:erl_opts(State),
    TestOpts = [{outdir, OutDir}] ++
               add_test_dir(ErlOpts, InDirs),
    first_files(rebar_state:set(State, erl_opts, TestOpts)).

add_test_dir(Opts, InDirs) ->
    %% if no src_dirs are set we have to specify `src` or it won't
    %% be built
    case proplists:append_values(src_dirs, Opts) of
        [] -> [{src_dirs, ["src", "test"|InDirs]}];
        _ -> [{src_dirs, ["test"|InDirs]}]
    end.

first_files(State) ->
    BaseFirst = rebar_state:get(State, erl_first_files, []),
    CTFirst = rebar_state:get(State, common_test_first_files, []),
    rebar_state:set(State, erl_first_modules, BaseFirst ++ CTFirst).

resolve_ct_opts(State, CmdLineOpts, OutDir) ->
    CTOpts = rebar_state:get(State, common_test_opts, []),
    Opts = lists:ukeymerge(1,
                    lists:ukeysort(1, CmdLineOpts),
                    lists:ukeysort(1, CTOpts)),
    %% rebar has seperate input and output directories whereas `common_test`
    %% uses only a single directory so set `dir` to our precompiled `OutDir`
    %% and disable `auto_compile`
    [{auto_compile, false}, {dir, OutDir}] ++ lists:keydelete(dir, 1, Opts).

maybe_compile_extra_tests(TestApps, State, InDirs, OutDir) ->
    F = fun(App) -> rebar_app_info:dir(App) == rebar_dir:get_cwd() end,
    case lists:filter(F, TestApps) of
        %% compile just the `test` and extra test directories of the base dir
        [] ->
            ErlOpts = rebar_state:get(State, common_test_compile_opts, []) ++
                      rebar_utils:erl_opts(State),
            TestOpts = [{outdir, OutDir}] ++
                       [{src_dirs, ["test"|InDirs]}] ++
                       lists:keydelete(src_dirs, 1, ErlOpts),
            TestState = first_files(rebar_state:set(State, erl_opts, TestOpts)),
            rebar_erlc_compiler:compile(TestState, rebar_dir:get_cwd());
        %% already compiled `./test` so do nothing
        _ -> ok
    end.

handle_results([Result]) ->
    handle_results(Result);
handle_results([Result|Results]) when is_list(Results) ->
    case handle_results(Result) of
        ok ->
            handle_results(Results);
        Error ->
            Error
    end;
handle_results({_, 0, _}) ->
    ok;
handle_results({_, FailedCount, _}) ->
    {error, {failures_running_tests, FailedCount}};
handle_results({error, Reason}) ->
    {error, {error_running_tests, Reason}}.
