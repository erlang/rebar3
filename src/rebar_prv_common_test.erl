%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_common_test).
-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).
%% exported for test purposes, consider private
-export([setup_ct/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

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
    try
        case setup_ct(State) of
            {error, {no_tests_specified, Opts}} ->
                ?WARN("No tests specified in opts: ~p", [Opts]),
                {ok, State};
            Opts ->
                Opts1 = setup_logdir(State, Opts),
                ?DEBUG("common test opts: ~p", [Opts1]),
                run_test(State, Opts1)
        end
    catch error:Reason -> ?PRV_ERROR(Reason)
    end.

-spec format_error(any()) -> iolist().
format_error({multiple_dirs_and_suites, Opts}) ->
    io_lib:format("Multiple dirs declared alongside suite in opts: ~p", [Opts]);
format_error({bad_dir_or_suite, Opts}) ->
    io_lib:format("Bad value for dir or suite in opts: ~p", [Opts]);
format_error({failures_running_tests, {Failed, AutoSkipped}}) ->
    io_lib:format("Failures occured running tests: ~b", [Failed+AutoSkipped]);
format_error({error_running_tests, Reason}) ->
    io_lib:format("Error running tests: ~p", [Reason]);
format_error({error, Reason}) ->
    io_lib:format("Unknown error: ~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

run_test(State, Opts) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    Result = case proplists:get_value(verbose, RawOpts, false) of
        true  -> run_test(Opts);
        false -> run_test_quiet(Opts)
    end,
    ok = rebar_prv_cover:maybe_write_coverdata(State, ?PROVIDER),
    case Result of
        ok    -> {ok, State};
        Error -> Error
    end.

run_test(Opts) -> handle_results(ct:run_test(Opts)).

run_test_quiet(Opts) ->
    Pid = self(),
    LogDir = proplists:get_value(logdir, Opts),
    erlang:spawn_monitor(fun() ->
        {ok, F} = file:open(filename:join([LogDir, "ct.latest.log"]),
                            [write]),
        true = group_leader(F, self()),
        Pid ! ct:run_test(Opts)
    end),
    receive Result -> handle_quiet_results(Opts, Result) end.

handle_results(Results) when is_list(Results) ->
    Result = lists:foldl(fun sum_results/2, {0, 0, {0,0}}, Results),
    handle_results(Result);
handle_results({_, Failed, {_, AutoSkipped}})
  when Failed > 0 orelse AutoSkipped > 0 ->
    ?PRV_ERROR({failures_running_tests, {Failed, AutoSkipped}});
handle_results({error, Reason}) ->
    ?PRV_ERROR({error_running_tests, Reason});
handle_results(_) ->
    ok.

sum_results({Passed, Failed, {UserSkipped, AutoSkipped}},
            {Passed2, Failed2, {UserSkipped2, AutoSkipped2}}) ->
    {Passed+Passed2, Failed+Failed2,
     {UserSkipped+UserSkipped2, AutoSkipped+AutoSkipped2}}.

handle_quiet_results(_, {error, _} = Result) ->
    handle_results(Result);
handle_quiet_results(_, {'DOWN', _, _, _, Reason}) ->
    handle_results(?PRV_ERROR(Reason));
handle_quiet_results(CTOpts, Results) when is_list(Results) ->
    _ = [format_result(Result) || Result <- Results],
    case handle_results(Results) of
        ?PRV_ERROR({failures_running_tests, _}) = Error ->
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

test_state(State) ->
    TestOpts = case rebar_state:get(State, ct_compile_opts, []) of
        []    -> [];
        Opts  -> [{erl_opts, Opts}]
    end,
    [first_files(State)|TestOpts].

first_files(State) ->
    CTFirst = rebar_state:get(State, ct_first_files, []),
    {erl_first_files, CTFirst}.

setup_ct(State) ->
    Opts = resolve_ct_opts(State),
    Opts1 = discover_tests(State, Opts),
    copy_and_compile_tests(State, Opts1).

resolve_ct_opts(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    CmdOpts = transform_opts(RawOpts),
    CfgOpts = rebar_state:get(State, ct_opts, []),
    Merged = lists:ukeymerge(1,
                             lists:ukeysort(1, CmdOpts),
                             lists:ukeysort(1, CfgOpts)),
    %% make sure `dir` and/or `suite` from command line go in as
    %%  a pair overriding both `dir` and `suite` from config if
    %%  they exist
    case {proplists:get_value(suite, CmdOpts), proplists:get_value(dir, CmdOpts)} of
        {undefined, undefined} -> Merged;
        {_Suite, undefined}    -> lists:keydelete(dir, 1, Merged);
        {undefined, _Dir}      -> lists:keydelete(suite, 1, Merged);
        {_Suite, _Dir}         -> Merged
    end.

discover_tests(State, Opts) ->
    case proplists:get_value(spec, Opts) of
        undefined -> discover_dirs_and_suites(State, Opts);
        TestSpec  -> discover_testspec(TestSpec, Opts)
    end.

discover_dirs_and_suites(State, Opts) ->
    case {proplists:get_value(dir, Opts), proplists:get_value(suite, Opts)} of
        %% no dirs or suites defined, try using `$APP/test` and `$ROOT/test`
        %%  as suites
        {undefined, undefined} -> test_dirs(State, Opts);
        %% no dirs defined
        {undefined, _} -> Opts;
        %% no suites defined
        {_, undefined} -> Opts;
        %% a single dir defined, this is ok
        {Dirs, Suites} when is_integer(hd(Dirs)), is_list(Suites) -> Opts;
        %% still a single dir defined, adjust to make acceptable to ct
        {[Dir], Suites} when is_integer(hd(Dir)), is_list(Suites) ->
            [{dir, Dir}|lists:keydelete(dir, 1, Opts)];
        %% multiple dirs and suites, error now to simplify later steps
        {_, _} -> erlang:error({multiple_dirs_and_suites, Opts})
    end.

discover_testspec(_TestSpec, Opts) ->
    lists:keydelete(auto_compile, 1, Opts).

copy_and_compile_tests(State, Opts) ->
    %% possibly enable cover
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    State1 = case proplists:get_value(cover, RawOpts, false) of
        true  -> rebar_state:set(State, cover_enabled, true);
        false -> State
    end,
    copy_and_compile_test_suites(State1, Opts).

copy_and_compile_test_suites(State, Opts) ->
    case proplists:get_value(suite, Opts) of
        %% no suites, try dirs
        undefined -> copy_and_compile_test_dirs(State, Opts);
        Suites    ->
            Dir = proplists:get_value(dir, Opts, undefined),
            AllSuites = join(Dir, Suites),
            Dirs = find_suite_dirs(AllSuites),
            lists:foreach(fun(S) ->
                NewPath = copy(State, S),
                compile_dir(State, NewPath)
            end, Dirs),
            NewSuites = lists:map(fun(S) -> retarget_path(State, S) end, AllSuites),
            [{suite, NewSuites}|lists:keydelete(suite, 1, Opts)]
    end.

copy_and_compile_test_dirs(State, Opts) ->
    case proplists:get_value(dir, Opts) of
        undefined -> {error, {no_tests_specified, Opts}};
        %% dir is a single directory
        Dir when is_list(Dir), is_integer(hd(Dir)) ->
            NewPath = copy(State, Dir),
            [{dir, compile_dir(State, NewPath)}|lists:keydelete(dir, 1, Opts)];
        %% dir is a list of directories
        Dirs when is_list(Dirs) ->
            NewDirs = lists:map(fun(Dir) ->
                NewPath = copy(State, Dir),
                compile_dir(State, NewPath)
            end, Dirs),
            [{dir, NewDirs}|lists:keydelete(dir, 1, Opts)]
    end.

join(undefined, Suites) -> Suites;
join(Dir, Suites) when is_list(Dir), is_integer(hd(Dir)) ->
    lists:map(fun(S) -> filename:join([Dir, S]) end, Suites);
%% multiple dirs or a bad dir argument, try to continue anyways
join(_, Suites) -> Suites. 

find_suite_dirs(Suites) ->
    AllDirs = lists:map(fun(S) -> filename:dirname(filename:absname(S)) end, Suites),
    %% eliminate duplicates
    lists:usort(AllDirs).

copy(State, Target) ->
    case retarget_path(State, Target) of
        %% directory lies outside of our project's file structure so
        %%  don't copy it
        Target    -> Target;
        NewTarget ->
            %% unlink the directory if it's a symlink
            case ec_file:is_symlink(NewTarget) of
                true  -> ok = ec_file:remove(NewTarget);
                false -> ok
            end,
            ok = ec_file:copy(Target, NewTarget, [recursive]),
            NewTarget
    end.

compile_dir(State, Dir) ->
    NewState = replace_src_dirs(State, [Dir]),
    ok = rebar_erlc_compiler:compile(NewState, rebar_dir:base_dir(State), Dir),
    ok = maybe_cover_compile(State, Dir),
    Dir.

retarget_path(State, Path) ->
    ProjectApps = rebar_state:project_apps(State),
    retarget_path(State, Path, ProjectApps).

%% not relative to any apps in project, check to see it's relative to
%%  project root
retarget_path(State, Path, []) ->
    case relative_path(reduce_path(Path), rebar_state:dir(State)) of
        {ok, NewPath}         -> filename:join([rebar_dir:base_dir(State), NewPath]);
        %% not relative to project root, don't modify
        {error, not_relative} -> Path
    end;
%% relative to current app, retarget to the same dir relative to
%%  the app's out_dir
retarget_path(State, Path, [App|Rest]) ->
    case relative_path(reduce_path(Path), rebar_app_info:dir(App)) of
        {ok, NewPath}         -> filename:join([rebar_app_info:out_dir(App), NewPath]);
        {error, not_relative} -> retarget_path(State, Path, Rest)
    end.

relative_path(Target, To) ->
    relative_path1(filename:split(filename:absname(Target)),
                   filename:split(filename:absname(To))).

relative_path1([Part|Target], [Part|To]) -> relative_path1(Target, To);
relative_path1([], [])                   -> {ok, ""};
relative_path1(Target, [])               -> {ok, filename:join(Target)};
relative_path1(_, _)                     -> {error, not_relative}.

reduce_path(Dir) -> reduce_path([], filename:split(filename:absname(Dir))).

reduce_path([], [])                -> filename:nativename("/");
reduce_path(Acc, [])               -> filename:join(lists:reverse(Acc));
reduce_path(Acc, ["."|Rest])       -> reduce_path(Acc, Rest);
reduce_path([_|Acc], [".."|Rest])  -> reduce_path(Acc, Rest);
reduce_path([], [".."|Rest])       -> reduce_path([], Rest);
reduce_path(Acc, [Component|Rest]) -> reduce_path([Component|Acc], Rest).

replace_src_dirs(State, Dirs) ->
    %% replace any `src_dirs` with the test dirs
    ErlOpts = rebar_state:get(State, erl_opts, []),
    StrippedOpts = lists:keydelete(src_dirs, 1, ErlOpts),
    rebar_state:set(State, erl_opts, [{src_dirs, Dirs}|StrippedOpts]).

test_dirs(State, Opts) ->
    BareTest = filename:join([rebar_state:dir(State), "test"]),
    F = fun(App) -> rebar_app_info:dir(App) == rebar_state:dir(State) end,
    TestApps = project_apps(State),
    case filelib:is_dir(BareTest) andalso not lists:any(F, TestApps) of
        %% `test` dir at root of project is already scheduled to be
        %%  included or `test` does not exist
        false -> application_dirs(TestApps, Opts, []);
        %% need to add `test` dir at root to dirs to be included
        true  -> application_dirs(TestApps, Opts, [BareTest])
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

application_dirs([], Opts, []) -> Opts;
application_dirs([], Opts, [Acc]) -> [{dir, Acc}|Opts];
application_dirs([], Opts, Acc) -> [{dir, lists:reverse(Acc)}|Opts];
application_dirs([App|Rest], Opts, Acc) ->
    TestDir = filename:join([rebar_app_info:dir(App), "test"]),
    case filelib:is_dir(TestDir) of
        true  -> application_dirs(Rest, Opts, [TestDir|Acc]);
        false -> application_dirs(Rest, Opts, Acc)
    end.

setup_logdir(State, Opts) ->
    Logdir = case proplists:get_value(logdir, Opts) of
        undefined -> filename:join([rebar_dir:base_dir(State), "logs"]);
        Dir       -> Dir
    end,
    ensure_dir([Logdir]),
    [{logdir, Logdir}|lists:keydelete(logdir, 1, Opts)].

ensure_dir([]) -> ok;
ensure_dir([Dir|Rest]) ->
    case ec_file:is_dir(Dir) of
        true ->
            ok;
        false ->
            ec_file:mkdir_path(Dir)
    end,
    ensure_dir(Rest).

maybe_cover_compile(State, Dir) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    State1 = case proplists:get_value(cover, Opts, false) of
        true  -> rebar_state:set(State, cover_enabled, true);
        false -> State
    end,
    rebar_prv_cover:maybe_cover_compile(State1, [Dir]).

ct_opts(_State) ->
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
     {logdir, undefined, "logdir", string, help(logdir)}, %% dir
     {logopts, undefined, "logopts", string, help(logopts)}, %% enum, no_nl | no_src
     {verbosity, undefined, "verbosity", string, help(verbosity)}, %% Integer OR [{Category, VLevel}]
     {silent_connections, undefined, "silent_connections", string,
      help(silent_connections)}, % all OR %% comma-seperated list
     {stylesheet, undefined, "stylesheet", string, help(stylesheet)}, %% file
     {cover, $c, "cover", {boolean, false}, help(cover)},
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
     {auto_compile, undefined, "auto_compile", {boolean, false}, help(auto_compile)},
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
help(label) ->
    "Test label";
help(config) ->
    "List of config files";
help(logdir) ->
    "Log folder";
help(verbosity) ->
    "Verbosity";
help(stylesheet) ->
    "Stylesheet to use for test results";
help(cover) ->
    "Generate cover data";
help(cover_spec) ->
    "Cover file to use";
help(event_handler) ->
    "Event handlers to attach to the runner";
help(include) ->
    "Include folder";
help(abort_if_missing_suites) ->
    "Abort if suites are missing";
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
help(verbose) ->
    "Verbose output";
help(_) ->
    "".

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