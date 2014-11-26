%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_common_test).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, ct).
-define(DEPS, [test_deps, compile]).

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
                                 {short_desc, "Run Common Tests"},
                                 {desc, ""},
                                 {opts, ct_opts(State)}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    Opts1 = transform_opts(Opts),
    ok = create_dirs(Opts1),
    expand_test_deps(filename:absname(rebar_state:get(State, test_deps_dir, ?DEFAULT_TEST_DEPS_DIR))),
    ct:run_test(Opts1),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

expand_test_deps(Dir) ->
    Apps = filelib:wildcard(filename:join([Dir, "*", "ebin"])),
    ok = code:add_pathsa(Apps).

ct_opts(State) ->
    DefaultTestDir = filename:join([rebar_state:dir(State), "test"]),
    DefaultLogsDir = filename:join([rebar_state:dir(State), "logs"]),
    [{dir, undefined, "dir", {string, DefaultTestDir}, help(dir)}, %% dir
     {suite, undefined, "suite", string, help(suite)}, %% comma-seperated list
     {group, undefined, "group", string, help(group)}, %% comma-seperated list
     {testcase, undefined, "case", string, help(testcase)}, %% comma-seperated list
     {spec, undefined, "spec", string, help(spec)}, %% comma-seperated list
     {join_specs, undefined, "join_specs", boolean, help(join_specs)}, %% Boolean
     {label, undefined, "label", string, help(label)}, %% String
     {config, undefined, "config", string, help(config)}, %% comma-seperated list
     {userconfig, undefined, "userconfig", string, help(userconfig)}, %% [{CallbackMod, CfgStrings}] | {CallbackMod, CfgStrings}
     {allow_user_terms, undefined, "allow_user_terms", boolean, help(allow_user_terms)}, %% Bool
     {logdir, undefined, "logdir", {string, DefaultLogsDir}, help(logdir)}, %% string
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

transform_opts(Opts) ->
    transform_opts(Opts, []).

transform_opts([], Acc) -> Acc;
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
                           to_atoms(split_string(CreatePrivDir))}|Acc]);
transform_opts([{multiply_timetraps, MultiplyTimetraps}|Rest], Acc) ->
    transform_opts(Rest, [{multiply_timetraps,
                           ec_cnv:to_integer(MultiplyTimetraps)}|Acc]);
transform_opts([{event_handler, EventHandler}|Rest], Acc) ->
    transform_opts(Rest, [{event_handler, parse_term(EventHandler)}|Acc]);
transform_opts([{silent_connections, SilentConnections}|Rest], Acc) ->
    transform_opts(Rest, [{silent_connections,
                           to_atoms(split_string(SilentConnections))}|Acc]);
transform_opts([{verbosity, "all"}|Rest], Acc) ->
    transform_opts(Rest, [{verbosity, all}|Acc]);
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

create_dirs(Opts) ->
    LogDir = proplists:get_value(logdir, Opts),
    TestDir = proplists:get_value(dir, Opts),
    ensure_logdir(LogDir),
    ensure_testdir(TestDir),
    ok.

ensure_logdir(Logdir) ->
    case ec_file:is_dir(Logdir) of
        true ->
            ok;
        false ->
            ec_file:mkdir_path(Logdir)
    end.

ensure_testdir(Testdir) ->
    case ec_file:is_dir(Testdir) of
        false ->
            ?INFO("Test directory ~s does not exist:\n",
                  [Testdir]),
            ?FAIL;
        _ -> ok
    end.

help(dir) ->
    "Test folder (default: test/)";
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
