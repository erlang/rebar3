%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_common_test).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).
%% exported for test purposes, consider private
-export([compile/2, prepare_tests/1, translate_paths/2]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, ct).
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
                                 {example, "rebar3 ct"},
                                 {short_desc, "Run Common Tests."},
                                 {desc, "Run Common Tests."},
                                 {opts, ct_opts(State)},
                                 {profiles, [test]}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Tests = prepare_tests(State),
    case compile(State, Tests) of
        %% successfully compiled apps
        {ok, S} -> do(S, Tests);
        %% this should look like a compiler error, not a ct error
        Error   -> Error
    end.

do(State, Tests) ->
    ?INFO("Running Common Test suites...", []),
    rebar_utils:update_code(rebar_state:code_paths(State, all_deps), [soft_purge]),

    %% Run ct provider prehooks
    Providers = rebar_state:providers(State),
    Cwd = rebar_dir:get_cwd(),
    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State),

    case Tests of
        {ok, T} ->
            case run_tests(State, T) of
                ok    ->
                    %% Run ct provider posthooks
                    rebar_hooks:run_all_hooks(Cwd, post, ?PROVIDER, Providers, State),
                    rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)),
                    {ok, State};
                Error ->
                    rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)),
                    Error
            end;
        Error ->
            rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)),
            Error
    end.

run_tests(State, Opts) ->
    T = translate_paths(State, Opts),
    Opts1 = setup_logdir(State, T),
    Opts2 = turn_off_auto_compile(Opts1),
    ?DEBUG("ct_opts ~p", [Opts2]),
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    Result = case proplists:get_value(verbose, RawOpts, false) of
        true  -> run_test_verbose(Opts2);
        false -> run_test_quiet(Opts2)
    end,
    ok = maybe_write_coverdata(State),
    Result.

-spec format_error(any()) -> iolist().
format_error({error, Reason}) ->
    io_lib:format("Error running tests:~n  ~p", [Reason]);
format_error({error_running_tests, Reason}) ->
    format_error({error, Reason});
format_error({failures_running_tests, {Failed, AutoSkipped}}) ->
    io_lib:format("Failures occured running tests: ~b", [Failed+AutoSkipped]);
format_error({badconfig, {Msg, {Value, Key}}}) ->
    io_lib:format(Msg, [Value, Key]);
format_error({badconfig, Msg}) ->
    io_lib:format(Msg, []);
format_error({multiple_errors, Errors}) ->
    io_lib:format(lists:concat(["Error running tests:"] ++
                               lists:map(fun(Error) -> "~n  " ++ Error end, Errors)), []).

%% ===================================================================
%% Internal functions
%% ===================================================================

prepare_tests(State) ->
    %% command line test options
    CmdOpts = cmdopts(State),
    %% rebar.config test options
    CfgOpts = cfgopts(State),
    ProjectApps = rebar_state:project_apps(State),
    %% prioritize tests to run first trying any command line specified
    %% tests falling back to tests specified in the config file finally
    %% running a default set if no other tests are present
    select_tests(State, ProjectApps, CmdOpts, CfgOpts).

cmdopts(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    %% filter out opts common_test doesn't know about and convert
    %% to ct acceptable forms
    transform_opts(RawOpts, []).

transform_opts([], Acc) -> lists:reverse(Acc);
transform_opts([{dir, Dirs}|Rest], Acc) ->
    transform_opts(Rest, [{dir, split_string(Dirs)}|Acc]);
transform_opts([{suite, Suites}|Rest], Acc) ->
    transform_opts(Rest, [{suite, split_string(Suites)}|Acc]);
transform_opts([{group, Groups}|Rest], Acc) ->
    transform_opts(Rest, [{group, split_string(Groups)}|Acc]);
transform_opts([{testcase, Cases}|Rest], Acc) ->
    transform_opts(Rest, [{testcase, split_string(Cases)}|Acc]);
transform_opts([{config, Configs}|Rest], Acc) ->
    transform_opts(Rest, [{config, split_string(Configs)}|Acc]);
transform_opts([{logopts, LogOpts}|Rest], Acc) ->
    transform_opts(Rest, [{logopts, lists:map(fun(P) -> list_to_atom(P) end, split_string(LogOpts))}|Acc]);
transform_opts([{force_stop, "true"}|Rest], Acc) ->
    transform_opts(Rest, [{force_stop, true}|Acc]);
transform_opts([{force_stop, "false"}|Rest], Acc) ->
    transform_opts(Rest, [{force_stop, false}|Acc]);
transform_opts([{force_stop, "skip_rest"}|Rest], Acc) ->
    transform_opts(Rest, [{force_stop, skip_rest}|Acc]);
transform_opts([{create_priv_dir, CreatePrivDir}|Rest], Acc) ->
    transform_opts(Rest, [{create_priv_dir, list_to_atom(CreatePrivDir)}|Acc]);
%% drop cover from opts, ct doesn't care about it
transform_opts([{cover, _}|Rest], Acc) ->
    transform_opts(Rest, Acc);
%% drop verbose from opts, ct doesn't care about it
transform_opts([{verbose, _}|Rest], Acc) ->
    transform_opts(Rest, Acc);
%% getopt should handle anything else
transform_opts([Opt|Rest], Acc) ->
    transform_opts(Rest, [Opt|Acc]).

split_string(String) ->
    string:tokens(String, [$,]).

cfgopts(State) ->
    case rebar_state:get(State, ct_opts, []) of
        Opts when is_list(Opts) ->
            ensure_opts(add_hooks(Opts, State), []);
        Wrong ->
            %% probably a single non list term
            ?PRV_ERROR({badconfig, {"Value `~p' of option `~p' must be a list", {Wrong, ct_opts}}})
    end.

ensure_opts([], Acc) -> lists:reverse(Acc);
ensure_opts([{test_spec, _}|_Rest], _Acc) ->
    ?PRV_ERROR({badconfig, "Test specs not supported"});
ensure_opts([{auto_compile, _}|_Rest], _Acc) ->
    ?PRV_ERROR({badconfig, "Auto compile not supported"});
ensure_opts([{suite, Suite}|Rest], Acc) when is_integer(hd(Suite)) ->
    ensure_opts(Rest, [{suite, Suite}|Acc]);
ensure_opts([{suite, Suite}|Rest], Acc) when is_atom(Suite) ->
    ensure_opts(Rest, [{suite, atom_to_list(Suite)}|Acc]);
ensure_opts([{suite, Suites}|Rest], Acc) ->
    NewSuites = {suite, lists:map(fun(S) when is_atom(S) -> atom_to_list(S);
                                     (S) when is_list(S) -> S
                                  end,
                                  Suites)},
    ensure_opts(Rest, [NewSuites|Acc]);
ensure_opts([{K, V}|Rest], Acc) ->
    ensure_opts(Rest, [{K, V}|Acc]);
ensure_opts([V|_Rest], _Acc) ->
    ?PRV_ERROR({badconfig, {"Member `~p' of option `~p' must be a 2-tuple", {V, ct_opts}}}).

add_hooks(Opts, State) ->
    case {readable(State), lists:keyfind(ct_hooks, 1, Opts)} of
        {false, _} ->
            Opts;
        {true, false} ->
            [{ct_hooks, [cth_readable_failonly, cth_readable_shell]} | Opts];
        {true, {ct_hooks, Hooks}} ->
            %% Make sure hooks are there once only.
            ReadableHooks = [cth_readable_failonly, cth_readable_shell],
            NewHooks =  (Hooks -- ReadableHooks) ++ ReadableHooks,
            lists:keyreplace(ct_hooks, 1, Opts, {ct_hooks, NewHooks})
    end.

select_tests(_, _, {error, _} = Error, _) -> Error;
select_tests(_, _, _, {error, _} = Error) -> Error;
select_tests(State, ProjectApps, CmdOpts, CfgOpts) ->
    Merged = lists:ukeymerge(1,
                             lists:ukeysort(1, CmdOpts),
                             lists:ukeysort(1, CfgOpts)),
    %% make sure `dir` and/or `suite` from command line go in as
    %% a pair overriding both `dir` and `suite` from config if
    %% they exist
    Opts = case {proplists:get_value(suite, CmdOpts), proplists:get_value(dir, CmdOpts)} of
        {undefined, undefined} -> Merged;
        {_Suite, undefined}    -> lists:keydelete(dir, 1, Merged);
        {undefined, _Dir}      -> lists:keydelete(suite, 1, Merged);
        {_Suite, _Dir}         -> Merged
    end,
    discover_tests(State, ProjectApps, Opts).

discover_tests(State, ProjectApps, Opts) ->
    case {proplists:get_value(suite, Opts), proplists:get_value(dir, Opts)} of
        %% no dirs or suites defined, try using `$APP/test` and `$ROOT/test`
        %%  as suites
        {undefined, undefined} -> {ok, [default_tests(State, ProjectApps)|Opts]};
        {_, _}                 -> {ok, Opts}
    end.

default_tests(State, ProjectApps) ->
    BareTest = filename:join([rebar_state:dir(State), "test"]),
    F = fun(App) -> rebar_app_info:dir(App) == rebar_state:dir(State) end,
    AppTests = application_dirs(ProjectApps, []),
    case filelib:is_dir(BareTest) andalso not lists:any(F, ProjectApps) of
        %% `test` dir at root of project is already scheduled to be
        %%  included or `test` does not exist
        false -> {dir, AppTests};
        %% need to add `test` dir at root to dirs to be included
        true  -> {dir, AppTests ++ [BareTest]}
    end.

application_dirs([], []) -> [];
application_dirs([], Acc) -> lists:reverse(Acc);
application_dirs([App|Rest], Acc) ->
    TestDir = filename:join([rebar_app_info:dir(App), "test"]),
    case filelib:is_dir(TestDir) of
        true  -> application_dirs(Rest, [TestDir|Acc]);
        false -> application_dirs(Rest, Acc)
    end.

compile(State, {ok, _} = Tests) ->
    %% inject `ct_first_files` and `ct_compile_opts` into the applications
    %% to be compiled
    case inject_ct_state(State, Tests) of
        {ok, NewState} -> do_compile(NewState);
        Error          -> Error
    end;
%% maybe compile even in the face of errors?
compile(_State, Error) -> Error.

do_compile(State) ->
    case rebar_prv_compile:do(State) of
        %% successfully compiled apps
        {ok, S} ->
            ok = maybe_cover_compile(S),
            {ok, S};
        %% this should look like a compiler error, not an eunit error
        Error   -> Error
    end.

inject_ct_state(State, {ok, Tests}) ->
    Apps = rebar_state:project_apps(State),
    case inject_ct_state(State, Apps, []) of
        {ok, {NewState, ModdedApps}} ->
            test_dirs(NewState, ModdedApps, Tests);
        {error, _} = Error           -> Error
    end;
inject_ct_state(_State, Error) -> Error.

inject_ct_state(State, [App|Rest], Acc) ->
    case inject(rebar_app_info:opts(App), State) of
        {error, _} = Error -> Error;
        NewOpts            ->
            NewApp = rebar_app_info:opts(App, NewOpts),
            inject_ct_state(State, Rest, [NewApp|Acc])
    end;
inject_ct_state(State, [], Acc) ->
    case inject(rebar_state:opts(State), State) of
        {error, _} = Error -> Error;
        NewOpts            -> {ok, {rebar_state:opts(State, NewOpts), lists:reverse(Acc)}}
    end.

opts(Opts, Key, Default) ->
    case rebar_opts:get(Opts, Key, Default) of
        Vs when is_list(Vs) -> Vs;
        Wrong ->
            ?PRV_ERROR({badconfig, {"Value `~p' of option `~p' must be a list", {Wrong, Key}}})
    end.

inject(Opts, State) -> erl_opts(Opts, State).

erl_opts(Opts, State) ->
    %% append `ct_compile_opts` to app defined `erl_opts`
    ErlOpts = opts(Opts, erl_opts, []),
    CTOpts = opts(Opts, ct_compile_opts, []),
    case add_transforms(append(CTOpts, ErlOpts), State) of
        {error, Error} -> {error, Error};
        NewErlOpts     -> first_files(rebar_opts:set(Opts, erl_opts, NewErlOpts))
    end.

first_files(Opts) ->
    %% append `ct_first_files` to app defined `erl_first_files`
    FirstFiles = opts(Opts, erl_first_files, []),
    CTFirstFiles = opts(Opts, ct_first_files, []),
    case append(CTFirstFiles, FirstFiles) of
        {error, _} = Error -> Error;
        NewFirstFiles  -> rebar_opts:set(Opts, erl_first_files, NewFirstFiles)
    end.

append({error, _} = Error, _) -> Error;
append(_, {error, _} = Error) -> Error;
append(A, B) -> A ++ B.

add_transforms(CTOpts, State) when is_list(CTOpts) ->
    case readable(State) of
        true ->
            ReadableTransform = [{parse_transform, cth_readable_transform}],
            (CTOpts -- ReadableTransform) ++ ReadableTransform;
        false ->
            CTOpts
    end;
add_transforms({error, _} = Error, _State) -> Error.

readable(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(readable, RawOpts) of
        true  -> true;
        false -> false;
        undefined -> rebar_state:get(State, ct_readable, true)
    end.

test_dirs(State, Apps, Opts) ->
    case {proplists:get_value(suite, Opts), proplists:get_value(dir, Opts)} of
        {Suites, undefined} -> set_compile_dirs(State, Apps, {suite, Suites});
        {undefined, Dirs}   -> set_compile_dirs(State, Apps, {dir, Dirs});
        {Suites, Dir} when is_integer(hd(Dir)) ->
            set_compile_dirs(State, Apps, join(Suites, Dir));
        {Suites, [Dir]} when is_integer(hd(Dir)) ->
            set_compile_dirs(State, Apps, join(Suites, Dir));          
        {_Suites, _Dirs}    -> {error, "Only a single directory may be specified when specifying suites"}
    end.

join(Suite, Dir) when is_integer(hd(Suite)) ->
    {suite, [filename:join([Dir, Suite])]};
join(Suites, Dir) ->
    {suite, lists:map(fun(S) -> filename:join([Dir, S]) end, Suites)}.

set_compile_dirs(State, Apps, {dir, Dir}) when is_integer(hd(Dir)) ->
    %% single directory
    %% insert `Dir` into an app if relative, or the base state if not
    %% app relative but relative to the root or not at all if outside
    %% project scope
    {NewState, NewApps} = maybe_inject_test_dir(State, [], Apps, Dir),
    {ok, rebar_state:project_apps(NewState, NewApps)};
set_compile_dirs(State, Apps, {dir, Dirs}) ->
    %% multiple directories
    F = fun(Dir, {S, A}) -> maybe_inject_test_dir(S, [], A, Dir) end,
    {NewState, NewApps} = lists:foldl(F, {State, Apps}, Dirs),
    {ok, rebar_state:project_apps(NewState, NewApps)};
set_compile_dirs(State, Apps, {suite, Suites}) ->
    %% suites with dir component
    Dirs = find_suite_dirs(Suites),
    F = fun(Dir, {S, A}) -> maybe_inject_test_dir(S, [], A, Dir) end,
    {NewState, NewApps} = lists:foldl(F, {State, Apps}, Dirs),
    {ok, rebar_state:project_apps(NewState, NewApps)}.

find_suite_dirs(Suites) ->
    AllDirs = lists:map(fun(S) -> filename:dirname(filename:absname(S)) end, Suites),
    %% eliminate duplicates
    lists:usort(AllDirs).

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
        {ok, []}   ->
            ?WARN("Can't have suites in root of project dir, dropping from tests", []),
            {State, AppAcc};
        {ok, Path} ->
            Opts = inject_test_dir(rebar_state:opts(State), Path),
            {rebar_state:opts(State, Opts), AppAcc};
        {error, badparent} ->
            {State, AppAcc}
    end.

inject_test_dir(Opts, Dir) ->
    %% append specified test targets to app defined `extra_src_dirs`
    ExtraSrcDirs = rebar_opts:get(Opts, extra_src_dirs, []),
    rebar_opts:set(Opts, extra_src_dirs, ExtraSrcDirs ++ [Dir]).

translate_paths(State, Opts) ->
    case {proplists:get_value(suite, Opts), proplists:get_value(dir, Opts)} of
        {_Suites, undefined} -> translate_suites(State, Opts, []);
        {undefined, _Dirs}   -> translate_dirs(State, Opts, []);
        %% both dirs and suites are defined, only translate dir paths
        _                    -> translate_dirs(State, Opts, [])
    end.

translate_dirs(_State, [], Acc) -> lists:reverse(Acc);
translate_dirs(State, [{dir, Dir}|Rest], Acc) when is_integer(hd(Dir)) ->
    %% single dir
    Apps = rebar_state:project_apps(State),
    translate_dirs(State, Rest, [{dir, translate(State, Apps, Dir)}|Acc]);
translate_dirs(State, [{dir, Dirs}|Rest], Acc) ->
    %% multiple dirs
    Apps = rebar_state:project_apps(State),
    NewDirs = {dir, lists:map(fun(Dir) -> translate(State, Apps, Dir) end, Dirs)},
    translate_dirs(State, Rest, [NewDirs|Acc]);
translate_dirs(State, [Test|Rest], Acc) ->
    translate_dirs(State, Rest, [Test|Acc]).

translate_suites(_State, [], Acc) -> lists:reverse(Acc);
translate_suites(State, [{suite, Suite}|Rest], Acc) when is_integer(hd(Suite)) ->
    %% single suite
    Apps = rebar_state:project_apps(State),
    translate_suites(State, Rest, [{suite, translate(State, Apps, Suite)}|Acc]);
translate_suites(State, [{suite, Suites}|Rest], Acc) ->
    %% multiple suites
    Apps = rebar_state:project_apps(State),
    NewSuites = {suite, lists:map(fun(Suite) -> translate(State, Apps, Suite) end, Suites)},
    translate_suites(State, Rest, [NewSuites|Acc]);
translate_suites(State, [Test|Rest], Acc) ->
    translate_suites(State, Rest, [Test|Acc]).

translate(State, [App|Rest], Path) ->
    case rebar_file_utils:path_from_ancestor(Path, rebar_app_info:dir(App)) of
        {ok, P}            -> filename:join([rebar_app_info:out_dir(App), P]);
        {error, badparent} -> translate(State, Rest, Path)
    end;
translate(State, [], Path) ->
    case rebar_file_utils:path_from_ancestor(Path, rebar_state:dir(State)) of
        {ok, P}            -> filename:join([rebar_dir:base_dir(State), "extras", P]);
        %% not relative, leave as is
        {error, badparent} -> Path
    end.

setup_logdir(State, Opts) ->
    Logdir = case proplists:get_value(logdir, Opts) of
        undefined -> filename:join([rebar_dir:base_dir(State), "logs"]);
        Dir       -> Dir
    end,
    filelib:ensure_dir(filename:join([Logdir, "dummy.beam"])),
    [{logdir, Logdir}|lists:keydelete(logdir, 1, Opts)].

turn_off_auto_compile(Opts) ->
    [{auto_compile, false}|lists:keydelete(auto_compile, 1, Opts)].

run_test_verbose(Opts) -> handle_results(ct:run_test(Opts)).

run_test_quiet(Opts) ->
    Pid = self(),
    Ref = erlang:make_ref(),
    LogDir = proplists:get_value(logdir, Opts),
    {_, Monitor} = erlang:spawn_monitor(fun() ->
        {ok, F} = file:open(filename:join([LogDir, "ct.latest.log"]),
                            [write]),
        true = group_leader(F, self()),
        Pid ! {Ref, ct:run_test(Opts)}
    end),
    receive
        {Ref, Result} -> handle_quiet_results(Opts, Result);
        {'DOWN', Monitor, _, _, Reason} -> handle_results(?PRV_ERROR(Reason))
    end.

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

ct_opts(_State) ->
    [{dir, undefined, "dir", string, help(dir)}, %% comma-seperated list
     {suite, undefined, "suite", string, help(suite)}, %% comma-seperated list
     {group, undefined, "group", string, help(group)}, %% comma-seperated list
     {testcase, undefined, "case", string, help(testcase)}, %% comma-seperated list
     {label, undefined, "label", string, help(label)}, %% String
     {config, undefined, "config", string, help(config)}, %% comma-seperated list
     {allow_user_terms, undefined, "allow_user_terms", boolean, help(allow_user_terms)}, %% Bool
     {logdir, undefined, "logdir", string, help(logdir)}, %% dir
     {logopts, undefined, "logopts", string, help(logopts)}, %% comma seperated list
     {verbosity, undefined, "verbosity", integer, help(verbosity)}, %% Integer
     {cover, $c, "cover", {boolean, false}, help(cover)},
     {repeat, undefined, "repeat", integer, help(repeat)}, %% integer
     {duration, undefined, "duration", string, help(duration)}, % format: HHMMSS
     {until, undefined, "until", string, help(until)}, %% format: YYMoMoDD[HHMMSS]
     {force_stop, undefined, "force_stop", string, help(force_stop)}, %% String
     {basic_html, undefined, "basic_html", boolean, help(basic_html)}, %% Boolean
     {stylesheet, undefined, "stylesheet", string, help(stylesheet)}, %% String
     {decrypt_key, undefined, "decrypt_key", string, help(decrypt_key)}, %% String
     {decrypt_file, undefined, "decrypt_file", string, help(decrypt_file)}, %% String
     {abort_if_missing_suites, undefined, "abort_if_missing_suites", {boolean, true}, help(abort_if_missing_suites)}, %% Boolean
     {multiply_timetraps, undefined, "multiply_timetraps", integer, help(multiple_timetraps)}, %% Integer
     {scale_timetraps, undefined, "scale_timetraps", boolean, help(scale_timetraps)},
     {create_priv_dir, undefined, "create_priv_dir", string, help(create_priv_dir)},
     {readable, undefined, "readable", boolean, help(readable)},
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
help(label) ->
    "Test label";
help(config) ->
    "List of config files";
help(allow_user_terms) ->
    "Allow user defined config values in config files";
help(logdir) ->
    "Log folder";
help(logopts) ->
    "Options for common test logging";
help(verbosity) ->
    "Verbosity";
help(cover) ->
    "Generate cover data";
help(repeat) ->
    "How often to repeat tests";
help(duration) ->
    "Max runtime (format: HHMMSS)";
help(until) ->
    "Run until (format: HHMMSS)";
help(force_stop) ->
    "Force stop on test timeout (true | false | skip_rest)";
help(basic_html) ->
    "Show basic HTML";
help(stylesheet) ->
    "CSS stylesheet to apply to html output";
help(decrypt_key) ->
    "Path to key for decrypting config";
help(decrypt_file) ->
    "Path to file containing key for decrypting config";
help(abort_if_missing_suites) ->
    "Abort if suites are missing";
help(multiply_timetraps) ->
    "Multiply timetraps";
help(scale_timetraps) ->
    "Scale timetraps";
help(create_priv_dir) ->
    "Create priv dir (auto_per_run | auto_per_tc | manual_per_tc)";
help(readable) ->
    "Shows test case names and only displays logs to shell on failures";
help(verbose) ->
    "Verbose output";
help(_) ->
    "".

