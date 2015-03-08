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
                                 {example, "rebar3 ct"},
                                 {short_desc, "Run Common Tests."},
                                 {desc, ""},
                                 {opts, ct_opts(State)},
                                 {profiles, [test]}]),
    State1 = rebar_state:add_provider(State, Provider),
    State2 = rebar_state:add_to_profile(State1, test, test_state(State1)),
    {ok, State2}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?INFO("Running Common Test suites...", []),
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    Opts = transform_opts(RawOpts),
    TestApps = filter_checkouts(rebar_state:project_apps(State)),
    ok = create_dirs(Opts),
    InDirs = in_dirs(State, RawOpts),
    ok = compile_tests(State, TestApps, InDirs),
    case resolve_ct_opts(State, TestApps, Opts) of
        {ok, CTOpts} ->
            run_test(State, RawOpts, CTOpts);
        {error, Reason} ->
            {error, {?MODULE, Reason}}
    end.

run_test(State, RawOpts, CTOpts) ->
    ok = maybe_cover_compile(State, RawOpts),
    Verbose = proplists:get_value(verbose, RawOpts, false),
    Result = run_test(CTOpts, Verbose),
    ok = rebar_prv_cover:maybe_write_coverdata(State, ?PROVIDER),
    case Result of
        {error, Reason} ->
            {error, {?MODULE, Reason}};
        ok ->
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error({failures_running_tests, {Failed, AutoSkipped}}) ->
    io_lib:format("Failures occured running tests: ~b", [Failed+AutoSkipped]);
format_error({error_running_tests, Reason}) ->
    io_lib:format("Error running tests: ~p", [Reason]);
format_error({error_processing_options, Reason}) ->
    io_lib:format("Error processing options: ~p", [Reason]).

run_test(CTOpts, true) ->
    handle_results(ct:run_test(CTOpts));
run_test(CTOpts, false) ->
    Pid = self(),
    LogDir = proplists:get_value(logdir, CTOpts),
    erlang:spawn_monitor(fun() ->
        {ok, F} = file:open(filename:join([LogDir, "ct.latest.log"]),
                            [write]),
        true = group_leader(F, self()),
        Pid ! ct:run_test(CTOpts)
    end),
    receive Result -> handle_quiet_results(CTOpts, Result) end.

ct_opts(_State) ->
    DefaultLogsDir = filename:join([rebar_dir:get_cwd(), "_build", "logs"]),
    [{dir, undefined, "dir", string, help(dir)}, %% comma-seperated list
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
     {cover, $c, "cover", boolean, help(cover)},
     {cover_spec, undefined, "cover_spec", string, help(cover_spec)}, %% file
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
     {ct_hooks, undefined, "ct_hooks", string, help(ct_hooks)}, %% List: [CTHModule | {CTHModule, CTHInitArgs}] where CTHModule is atom CthInitArgs is term
     {verbose, $v, "verbose", boolean, help(verbose)}
    ].

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
    "Generate cover data";
help(cover_spec) ->
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
    "";
help(verbose) ->
    "Verbose output".

transform_opts(Opts) ->
    transform_opts(Opts, []).

transform_opts([], Acc) -> Acc;
%% drop `cover` and `verbose` so they're not passed as an option to common_test
transform_opts([{cover, _}|Rest], Acc) ->
    transform_opts(Rest, Acc);
transform_opts([{verbose, _}|Rest], Acc) ->
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
transform_opts([{suite, Suite}|Rest], Acc) ->
    transform_opts(Rest, [{suite, split_string(Suite)}|Acc]);
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

in_dirs(State, Opts) ->
    %% preserve the override nature of command line opts by only checking
    %% `rebar.config` defined additional test dirs if none are defined via
    %% command line flag
    case proplists:get_value(dir, Opts) of
        undefined ->
            CTOpts = rebar_state:get(State, ct_opts, []),
            proplists:get_value(dir, CTOpts, []);
        Dirs -> split_string(Dirs)
    end.

test_dirs(State, TestApps) ->
    %% we need to add "./ebin" if it exists but only if it's not already
    %%  due to be added
    F = fun(App) -> rebar_app_info:dir(App) =/= rebar_dir:get_cwd() end,
    BareEbin = filename:join([rebar_dir:base_dir(State), "ebin"]),
    case lists:any(F, TestApps) andalso filelib:is_dir(BareEbin) of
        false -> application_dirs(TestApps, []);
        true  -> [BareEbin|application_dirs(TestApps, [])]
    end.

application_dirs([], Acc) -> lists:reverse(Acc);
application_dirs([App|Rest], Acc) ->
    application_dirs(Rest, [rebar_app_info:ebin_dir(App)|Acc]).

test_state(State) ->
    TestOpts = case rebar_state:get(State, ct_compile_opts, []) of
        []    -> [];
        Opts  -> [{erl_opts, Opts}]
    end,
    [first_files(State)|TestOpts].

first_files(State) ->
    CTFirst = rebar_state:get(State, ct_first_files, []),
    {erl_first_files, CTFirst}.

resolve_ct_opts(State, TestApps, CmdLineOpts) ->
    CTOpts = rebar_state:get(State, ct_opts, []),
    Opts = lists:ukeymerge(1,
                    lists:ukeysort(1, CmdLineOpts),
                    lists:ukeysort(1, CTOpts)),
    TestDirs = test_dirs(State, TestApps),
    try resolve_ct_opts(TestDirs, Opts) of
        Opts2 ->
            %% disable `auto_compile`
            {ok, [{auto_compile, false} | Opts2]}
    catch
        throw:{error, Reason}->
            {error, {error_processing_options, Reason}}
    end.

resolve_ct_opts(Dirs, Opts) ->
    Opts2 = lists:keydelete(dir, 1, Opts),
    case lists:keytake(suite, 1, Opts2) of
        {value, {suite, Suites}, Opts3} ->
            %% Find full path to suites so that test names are consistent with
            %% names when testing all dirs.
            Suites2 = [resolve_suite(Dirs, Suite) || Suite <- Suites],
            [{suite, Suites2} | Opts3];
        false ->
            %% No suites, test all dirs.
            [{dir, Dirs} | Opts2]
    end.

resolve_suite(Dirs, Suite) ->
    File = Suite ++ code:objfile_extension(),
    case [Path || Dir <- Dirs,
                  Path <- [filename:join(Dir, File)],
                  filelib:is_file(Path)] of
        [Suite2] ->
            Suite2;
        [] ->
            throw({error, {unknown_suite, File}});
        Suites ->
            throw({error, {duplicate_suites, Suites}})
    end.

compile_tests(State, TestApps, InDirs) ->
    F = fun(AppInfo) ->
        AppDir = rebar_app_info:dir(AppInfo),
        S = case rebar_app_info:state(AppInfo) of
            undefined ->
                C = rebar_config:consult(AppDir),
                rebar_state:new(State, C, AppDir);
            AppState ->
                AppState
        end,
        ok = rebar_erlc_compiler:compile(replace_src_dirs(S, ["test"]),
                                         ec_cnv:to_list(rebar_app_info:out_dir(AppInfo)))
    end,
    lists:foreach(F, TestApps),
    compile_extra_tests(State, TestApps, InDirs).

%% extra directories containing tests can be passed to ct via the `dir` option
compile_extra_tests(State, TestApps, InDirs) ->
    F = fun(App) -> rebar_app_info:dir(App) == rebar_dir:get_cwd() end,
    TestDirs = case lists:filter(F, TestApps) of
        %% add `test` to indirs if it exists at the root of the project and
        %%  it hasn't already been compiled
        [] -> ["test"|InDirs];
        %% already compiled `./test` so do nothing
        _  -> InDirs
    end,
    %% symlink each of the extra dirs
    lists:foreach(fun(Dir) ->
        Source = filename:join([rebar_dir:get_cwd(), Dir]),
        Target = filename:join([rebar_dir:base_dir(State), Dir]),
        ok = rebar_file_utils:symlink_or_copy(Source, Target)
    end, TestDirs),
    rebar_erlc_compiler:compile(replace_src_dirs(State, TestDirs),
                                rebar_dir:base_dir(State),
                                filename:join([rebar_dir:base_dir(State), "ebin"])).

replace_src_dirs(State, Dirs) ->
    %% replace any `src_dirs` with the test dirs
    ErlOpts = rebar_state:get(State, erl_opts, []),
    StrippedOpts = lists:keydelete(src_dirs, 1, ErlOpts),
    rebar_state:set(State, erl_opts, [{src_dirs, Dirs}|StrippedOpts]).

maybe_cover_compile(State, Opts) ->
    State1 = case proplists:get_value(cover, Opts, false) of
        true  -> rebar_state:set(State, cover_enabled, true);
        false -> State
    end,
    rebar_prv_cover:maybe_cover_compile(State1).

handle_results(Results) when is_list(Results) ->
    Result = lists:foldl(fun sum_results/2, {0, 0, {0,0}}, Results),
    handle_results(Result);
handle_results({_, Failed, {_, AutoSkipped}})
  when Failed > 0 orelse AutoSkipped > 0 ->
    {error, {failures_running_tests, {Failed, AutoSkipped}}};
handle_results({error, Reason}) ->
    {error, {error_running_tests, Reason}};
handle_results(_) ->
    ok.

sum_results({Passed, Failed, {UserSkipped, AutoSkipped}},
            {Passed2, Failed2, {UserSkipped2, AutoSkipped2}}) ->
    {Passed+Passed2, Failed+Failed2,
     {UserSkipped+UserSkipped2, AutoSkipped+AutoSkipped2}}.

handle_quiet_results(_, {error, _} = Result) ->
    handle_results(Result);
handle_quiet_results(_, {'DOWN', _, _, _, Reason}) ->
    handle_results({error, Reason});
handle_quiet_results(CTOpts, Results) when is_list(Results) ->
    _ = [format_result(Result) || Result <- Results],
    case handle_results(Results) of
        {error, {failures_running_tests, _}} = Error ->
            LogDir = proplists:get_value(logdir, CTOpts),
            Index = filename:join([LogDir, "index.html"]),
            ?CONSOLE("Results written to ~p.", [Index]),
            Error;
        Other ->
            Other
    end;
handle_quiet_results(CTOpts, Result) ->
    handle_quiet_results(CTOpts, [Result]).

format_result({Passed, 0, {0, 0}}) ->
    ?CONSOLE("All ~p tests passed.", [Passed]);
format_result({Passed, Failed, Skipped}) ->
    Format = [format_failed(Failed), format_skipped(Skipped),
              format_passed(Passed)],
    ?CONSOLE("~s", [Format]).

format_failed(0) ->
    [];
format_failed(Failed) ->
    io_lib:format("Failed ~p tests. ", [Failed]).

format_passed(Passed) ->
    io_lib:format("Passed ~p tests. ", [Passed]).

format_skipped({0, 0}) ->
    [];
format_skipped({User, Auto}) ->
    io_lib:format("Skipped ~p (~p, ~p) tests. ", [User+Auto, User, Auto]).
