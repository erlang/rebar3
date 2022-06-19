-module(rebar_compiler_dag_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

all() ->
    [exists, {group, with_project}, prune_preserve_artifacts].

groups() ->
    %% The tests in this group are dirty, the order is specific
    %% and required across runs for tests to work.
    [{with_project, [sequence], [
        find_structure, app_sort,
        propagate_include_app1a, propagate_include_app1b,
        propagate_include_app2, propagate_behaviour,
        propagate_app1_ptrans, propagate_app2_ptrans,
        propagate_app2_ptrans_hrl
      ]}
    ].

init_per_suite(Config) ->
    rebar_compiler_erl:module_info(), % ensure it is loaded
    Config.

end_per_suite(Config) ->
    Config.

init_per_group(with_project, Config) ->
    NewConfig = rebar_test_utils:init_rebar_state(Config, "apps"),
    AppDir = ?config(apps, NewConfig),

    Name1 = rebar_test_utils:create_random_name("app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name1]), Name1, Vsn1, [kernel, stdlib]),

    Name2 = rebar_test_utils:create_random_name("app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name2]), Name2, Vsn2, [kernel, stdlib]),

    Name3 = rebar_test_utils:create_random_name("app3_"),
    Vsn3 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name3]), Name3, Vsn3, [kernel, stdlib]),

    apply_project(AppDir, [{app1, Name1}, {app2, Name2}, {app3, Name3}],
                  project()),

    [{app_names, [Name1, Name2, Name3]},
     {vsns, [Vsn1, Vsn2, Vsn3]}
     | NewConfig];
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

exists(Config) ->
    %% Create a DAG
    Priv = ?config(priv_dir, Config),
    G = rebar_compiler_dag:init(Priv, compilermod, "label", [crit_meta]),
    rebar_compiler_dag:store_artifact(G, "somefile", "someartifact", [written]),
    rebar_compiler_dag:maybe_store(G, Priv, compilermod, "label", [crit_meta]),
    rebar_compiler_dag:terminate(G),

    ?assertEqual(valid,     rebar_compiler_dag:status(Priv, compilermod, "label", [crit_meta])),
    ?assertEqual(not_found, rebar_compiler_dag:status(Priv, compilermad, "label", [crit_meta])),
    ?assertEqual(not_found, rebar_compiler_dag:status(Priv, compilermod, "lobel", [crit_meta])),
    ?assertEqual(bad_meta,  rebar_compiler_dag:status(Priv, compilermod, "label", [crit_zeta])),
    ok.

project() ->
    [{app1, [
        {"src/app1.erl",
         "-module(app1).\n"
         "-include(\"app1_a.hrl\").\n"
         "-include(\"app1_b.hrl\").\n"
         "-include_lib(\"{{app2}}/include/app2.hrl\").\n"
         "-compile({parse_transform, app1_trans}).\n"
         "-compile({parse_transform, {app3, []}}).\n"
         "-behaviour(app2).\n"
         "-export([cb/0]).\n"
         "cb() -> {?APP1A, ?APP1B, ?APP2}.\n"},
        {"src/app1_trans.erl",
         "-module(app1_trans).n"
         "-export([parse_transform/2]).\n"
         "parse_transform(Forms, _Opts) -> Forms.\n"},
        {"src/app1_a.hrl",
         "-define(APP1A, 1).\n"},
        {"include/app1_b.hrl",
         "-define(APP1B, 1).\n"}
     ]},
     {app2, [
        {"src/app2.erl",
         "-module(app2).\n"
         "-callback cb() -> term().\n"},
        {"include/app2.hrl",
         "-include(\"app2_resolve.hrl\").\n"
         "-define(APP2, 1).\n"},
        {"src/app2_resolve.hrl",
         "this file should be found but never is"},
        {"include/never_found.hrl",
         "%% just comments"}
     ]},
     {app3, [
        {"src/app3.erl",
         "-module(app3).\n"
         "-include_lib(\"{{app2}}/include/app2.hrl\").\n"
         "-include(\"app3_resolve.hrl\").\n"
         "-export([parse_transform/2]).\n"
         "parse_transform(Forms, _Opts) -> Forms.\n"},
        {"src/app3_resolve.hrl",
         "-behaviour(app2).\n"
         "-export([cb/0]).\n"
         "cb() -> {}.\n"
         "%% this file should be found"}
     ]}
    ].

find_structure() ->
    [{doc, "ensure a proper digraph is built with all files"}].
find_structure(Config) ->
    AppDir = ?config(apps, Config),
    AppNames = ?config(app_names, Config),
    %% assume an empty graph
    G = digraph:new([acyclic]),
    analyze_apps(G, AppNames, AppDir),
    FileStamps = [digraph:vertex(G, V) || V <- digraph:vertices(G)],
    Edges = [{V1,V2} || E <- digraph:edges(G),
                        {_,V1,V2,_} <- [digraph:edge(G, E)]],
    %% All timestamps are the same since we just created the thing
    {_, Stamp} = hd(FileStamps),
    Matches = [
        {"/src/app1.erl", Stamp},
        {"/src/app1_trans.erl", Stamp},
        {"/src/app1_a.hrl", Stamp},
        {"/include/app1_b.hrl", Stamp},
        {"/src/app2.erl", Stamp},
        {"/include/app2.hrl", Stamp},
        {"/include/app2.hrl", Stamp},
        {"/src/app3.erl", Stamp},
        {"/src/app3_resolve.hrl", Stamp}
    ],
    matches(Matches, FileStamps),
    ?assertEqual(undefined, find_match(".*/never_found.hrl", FileStamps)),
    ?assertEqual(undefined, find_match(".*/app2_resolve.hrl", FileStamps)),
    ct:pal("Edges: ~p", [Edges]),
    edges([
        {"/src/app1.erl", "/src/app1_a.hrl"},
        {"/src/app1.erl", "/include/app1_b.hrl"},
        {"/src/app1.erl", "/src/app2.erl"},
        {"/src/app1.erl", "/include/app2.hrl"},
        {"/src/app1.erl", "/src/app1_trans.erl"},
        {"/src/app1.erl", "/src/app3.erl"},
        {"/src/app3.erl", "/include/app2.hrl"},
        {"/src/app3.erl", "/src/app3_resolve.hrl"}
    ], Edges, FileStamps),
    ok.

app_sort() ->
    [{doc, "once the digraph is complete, we can sort apps by dependency order"}].
app_sort(Config) ->
    AppDir = ?config(apps, Config),
    AppNames = ?config(app_names, Config),
    %% assume an empty graph
    G = digraph:new([acyclic]),
    analyze_apps(G, AppNames, AppDir),
    AppPaths = [
        {AppName, filename:join([AppDir, "apps", AppName])} || AppName <- AppNames
    ],
    ?assertEqual([lists:nth(2, AppNames),
                  lists:nth(3, AppNames),
                  lists:nth(1, AppNames)],
                 rebar_compiler_dag:compile_order(G, AppPaths, ".erl", ".beam")),
    ok.

propagate_include_app1a() ->
    [{doc, "changing the app1a header file propagates to its dependents"}].
propagate_include_app1a(Config) ->
    AppDir = ?config(apps, Config),
    AppNames = ?config(app_names, Config),
    %% assume an empty graph
    G = digraph:new([acyclic]),
    next_second(),
    F = filename:join([AppDir, "apps", lists:nth(1, AppNames), "src/app1_a.hrl"]),
    bump_file(F),
    analyze_apps(G, AppNames, AppDir),
    FileStamps = [digraph:vertex(G, V) || V <- digraph:vertices(G)],
    %% All timestamps are the same since we just created the thing
    [Stamp1, Stamp2] = lists:usort([S || {_, S} <- FileStamps]),
    Matches = [
        {"/src/app1.erl", Stamp2},
        {"/src/app1_trans.erl", Stamp1},
        {"/src/app1_a.hrl", Stamp2},
        {"/include/app1_b.hrl", Stamp1},
        {"/src/app2.erl", Stamp1},
        {"/include/app2.hrl", Stamp1},
        {"/src/app3.erl", Stamp1},
        {"/src/app3_resolve.hrl", Stamp1}
    ],
    matches(Matches, FileStamps),
    ok.

propagate_include_app1b() ->
    [{doc, "changing the app1b header file propagates to its dependents"}].
propagate_include_app1b(Config) ->
    AppDir = ?config(apps, Config),
    AppNames = ?config(app_names, Config),
    %% assume an empty graph
    G = digraph:new([acyclic]),
    next_second(),
    F = filename:join([AppDir, "apps", lists:nth(1, AppNames), "include/app1_b.hrl"]),
    bump_file(F),
    analyze_apps(G, AppNames, AppDir),
    FileStamps = [digraph:vertex(G, V) || V <- digraph:vertices(G)],
    %% All timestamps are the same since we just created the thing
    [Stamp1, Stamp2, Stamp3] = lists:usort([S || {_, S} <- FileStamps]),
    Matches = [
        {"/src/app1.erl", Stamp3},
        {"/src/app1_trans.erl", Stamp1},
        {"/src/app1_a.hrl", Stamp2},
        {"/include/app1_b.hrl", Stamp3},
        {"/src/app2.erl", Stamp1},
        {"/include/app2.hrl", Stamp1},
        {"/src/app3.erl", Stamp1},
        {"/src/app3_resolve.hrl", Stamp1}
    ],
    matches(Matches, FileStamps),
    ok.

propagate_include_app2() ->
    [{doc, "changing the app2 header file propagates to its dependents"}].
propagate_include_app2(Config) ->
    AppDir = ?config(apps, Config),
    AppNames = ?config(app_names, Config),
    %% assume an empty graph
    G = digraph:new([acyclic]),
    next_second(),
    F = filename:join([AppDir, "apps", lists:nth(2, AppNames), "include/app2.hrl"]),
    bump_file(F),
    analyze_apps(G, AppNames, AppDir),
    FileStamps = [digraph:vertex(G, V) || V <- digraph:vertices(G)],
    %% All timestamps are the same since we just created the thing
    [S1, S2, S3, S4] = lists:usort([S || {_, S} <- FileStamps]),
    Matches = [
        {"/src/app1.erl", S4},
        {"/src/app1_trans.erl", S1},
        {"/src/app1_a.hrl", S2},
        {"/include/app1_b.hrl", S3},
        {"/src/app2.erl", S1},
        {"/include/app2.hrl", S4},
        {"/src/app3.erl", S4},
        {"/src/app3_resolve.hrl", S1}
    ],
    matches(Matches, FileStamps),
    ok.

propagate_behaviour() ->
    [{doc, "changing the behaviour file propagates to its dependents"}].
propagate_behaviour(Config) ->
    AppDir = ?config(apps, Config),
    AppNames = ?config(app_names, Config),
    %% assume an empty graph
    G = digraph:new([acyclic]),
    next_second(),
    F = filename:join([AppDir, "apps", lists:nth(2, AppNames), "src/app2.erl"]),
    bump_file(F),
    analyze_apps(G, AppNames, AppDir),
    FileStamps = [digraph:vertex(G, V) || V <- digraph:vertices(G)],
    %% All timestamps are the same since we just created the thing
    [S1, S2, S3, S4, S5] = lists:usort([S || {_, S} <- FileStamps]),
    Matches = [
        {"/src/app1.erl", S5},
        {"/src/app1_trans.erl", S1},
        {"/src/app1_a.hrl", S2},
        {"/include/app1_b.hrl", S3},
        {"/src/app2.erl", S5},
        {"/include/app2.hrl", S4},
        {"/src/app3.erl", S5},
        {"/src/app3_resolve.hrl", S1}
    ],
    matches(Matches, FileStamps),
    ok.

propagate_app1_ptrans() ->
    [{doc, "changing an app-local parse transform propagates to its dependents"}].
propagate_app1_ptrans(Config) ->
    AppDir = ?config(apps, Config),
    AppNames = ?config(app_names, Config),
    %% assume an empty graph
    G = digraph:new([acyclic]),
    next_second(),
    F = filename:join([AppDir, "apps", lists:nth(1, AppNames), "src/app1_trans.erl"]),
    bump_file(F),
    analyze_apps(G, AppNames, AppDir),
    FileStamps = [digraph:vertex(G, V) || V <- digraph:vertices(G)],
    %% All timestamps are the same since we just created the thing
    [S1, S2, S3, S4, S5, S6] = lists:usort([S || {_, S} <- FileStamps]),
    Matches = [
        {"/src/app1.erl", S6},
        {"/src/app1_trans.erl", S6},
        {"/src/app1_a.hrl", S2},
        {"/include/app1_b.hrl", S3},
        {"/src/app2.erl", S5},
        {"/include/app2.hrl", S4},
        {"/src/app3.erl", S5},
        {"/src/app3_resolve.hrl", S1}
    ],
    matches(Matches, FileStamps),
    ok.

propagate_app2_ptrans() ->
    [{doc, "changing an app-foreign parse transform propagates to its dependents"}].
propagate_app2_ptrans(Config) ->
    AppDir = ?config(apps, Config),
    AppNames = ?config(app_names, Config),
    %% assume an empty graph
    G = digraph:new([acyclic]),
    next_second(),
    F = filename:join([AppDir, "apps", lists:nth(3, AppNames), "src/app3.erl"]),
    bump_file(F),
    analyze_apps(G, AppNames, AppDir),
    FileStamps = [digraph:vertex(G, V) || V <- digraph:vertices(G)],
    %% All timestamps are the same since we just created the thing
    [S1, S2, S3, S4, S5, S6, S7] = lists:usort([S || {_, S} <- FileStamps]),
    Matches = [
        {"/src/app1.erl", S7},
        {"/src/app1_trans.erl", S6},
        {"/src/app1_a.hrl", S2},
        {"/include/app1_b.hrl", S3},
        {"/src/app2.erl", S5},
        {"/include/app2.hrl", S4},
        {"/src/app3.erl", S7},
        {"/src/app3_resolve.hrl", S1}
    ],
    matches(Matches, FileStamps),
    ok.

propagate_app2_ptrans_hrl() ->
    %% the app-foreign ptrans' foreign hrl dep is tested by propagate_include_app2 as well
    [{doc, "changing an app-foreign parse transform's local hrl propagates to its dependents"}].
propagate_app2_ptrans_hrl(Config) ->
    AppDir = ?config(apps, Config),
    AppNames = ?config(app_names, Config),
    %% assume an empty graph
    G = digraph:new([acyclic]),
    next_second(),
    F = filename:join([AppDir, "apps", lists:nth(3, AppNames), "src/app3_resolve.hrl"]),
    bump_file(F),
    analyze_apps(G, AppNames, AppDir),
    FileStamps = [digraph:vertex(G, V) || V <- digraph:vertices(G)],
    %% All timestamps are the same since we just created the thing
    %% S1 and S7 are gone from the propagation now
    [S2, S3, S4, S5, S6, S8] = lists:usort([S || {_, S} <- FileStamps]),
    Matches = [
        {"/src/app1.erl", S8},
        {"/src/app1_trans.erl", S6},
        {"/src/app1_a.hrl", S2},
        {"/include/app1_b.hrl", S3},
        {"/src/app2.erl", S5},
        {"/include/app2.hrl", S4},
        {"/src/app3.erl", S8},
        {"/src/app3_resolve.hrl", S8}
    ],
    matches(Matches, FileStamps),
    ok.

prune_preserve_artifacts() ->
    [{doc, "Build artifacts are kept through the pruning process, even with multiple "
           "artifact types."}].
prune_preserve_artifacts(Config) ->
    Priv = ?config(priv_dir, Config),
    Source = filename:join(Priv, "file.src"),
    %% The source file must exist not to get the artifacts pruned
    ok = file:write_file(Source, <<"hello, world!">>),
    Opts = [some_option, {with, terms}],
    G = digraph:new([acyclic]),
    digraph:add_vertex(G, Source, 123),
    %% The artifact names are prefixed with z- to sort nicer with windows tests that
    %% prefix a drive letter to paths, so we sort the same on all filesystems...
    rebar_compiler_dag:store_artifact(G, Source, "z-artifact.type1", Opts),
    rebar_compiler_dag:store_artifact(G, Source, "z-artifact.type2", Opts),
    rebar_compiler_dag:store_artifact(G, Source, "z-derived.type3", Opts),
    AppPaths = [{Priv, filename:join(Priv, "out")}],
    ct:pal("all vertices: ~p~n", [digraph:vertices(G)]),
    %% Prune with all types being valid aside from the source file;
    %% expect all of the artifacts to be kept
    rebar_compiler_dag:prune(G, ".src", [".type1",".type2",".type3"], [Source], AppPaths),
    ?assertEqual([Source, "z-artifact.type1", "z-artifact.type2", "z-derived.type3"],
                 lists:sort(digraph:vertices(G) -- ['$r3_dirty_bit'])),
    %% Prune artifacts that no longer belong to the compiler definition
    rebar_compiler_dag:prune(G, ".src", [".type1",".type2"], [Source], AppPaths),
    ?assertEqual([Source, "z-artifact.type1", "z-artifact.type2"],
                 lists:sort(digraph:vertices(G) -- ['$r3_dirty_bit'])),
    %% if the source file is gone, prune everything
    ok = file:delete(Source),
    rebar_compiler_dag:prune(G, ".src", [".type1",".type2"], [Source], AppPaths),
    ?assertEqual([Source, "z-artifact.type1", "z-artifact.type2"],
                 lists:sort(digraph:vertices(G) -- ['$r3_dirty_bit'])),
    rebar_compiler_dag:prune(G, ".src", [".type1",".type2"], [], AppPaths),
    ?assertEqual([], lists:sort(digraph:vertices(G) -- ['$r3_dirty_bit'])),
    ok.

%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

apply_project(_BaseDir, _Names, []) ->
    ok;
apply_project(BaseDir, Names, [{_AppName, []}|Rest]) ->
    apply_project(BaseDir, Names, Rest);
apply_project(BaseDir, Names, [{AppName, [File|Files]}|Rest]) ->
    apply_file(BaseDir, Names, AppName, File),
    apply_project(BaseDir, Names, [{AppName, Files}|Rest]).

apply_file(BaseDir, Names, App, {FileName, Contents}) ->
    AppName = proplists:get_value(App, Names),
    FilePath = filename:join([BaseDir, "apps", AppName, FileName]),
    ok = filelib:ensure_dir(FilePath),
    file:write_file(FilePath, apply_template(Contents, Names)).

apply_template("", _) -> "";
apply_template("{{" ++ Text, Names) ->
    {Var, Rest} = parse_to_var(Text),
    App = list_to_atom(Var),
    proplists:get_value(App, Names) ++ apply_template(Rest, Names);
apply_template([H|T], Names) ->
    [H|apply_template(T, Names)].

parse_to_var(Str) -> parse_to_var(Str, []).

parse_to_var("}}"++Rest, Acc) ->
    {lists:reverse(Acc), Rest};
parse_to_var([H|T], Acc) ->
    parse_to_var(T, [H|Acc]).

analyze_apps(G, AppNames, AppDir) ->
    populate_app(G, lists:nth(1, AppNames), AppNames, AppDir, ["app1.erl", "app1_trans.erl"]),
    populate_app(G, lists:nth(2, AppNames), AppNames, AppDir, ["app2.erl"]),
    populate_app(G, lists:nth(3, AppNames), AppNames, AppDir, ["app3.erl"]),
    rebar_compiler_dag:populate_deps(G, ".erl", [{".beam", "ebin/"}]),
    rebar_compiler_dag:propagate_stamps(G),
    %% manually clear the dirty bit for ease of validation
    digraph:del_vertex(G, '$r3_dirty_bit').

populate_app(G, Name, AppNames, AppDir, Sources) ->
    InDirs = [filename:join([AppDir, "apps", AppName, "src"])
              || AppName <- AppNames]
          ++ [filename:join([AppDir, "apps", AppName, "include"])
              || AppName <- AppNames],
    AbsSources = [filename:join([AppDir, "apps", Name, "src", Src])
                  || Src <- Sources],
    DepOpts = [{includes,
                [filename:join([AppDir, "apps", Name, "src"]),
                 filename:join([AppDir, "apps", Name, "include"])
                ]},
               {include_libs, [filename:join([AppDir, "apps"])]}
              ],
    rebar_compiler_dag:populate_sources(
        G, rebar_compiler_erl,
        InDirs, AbsSources, DepOpts
    ).

find_match(Regex, FileStamps) ->
    try
        [throw(F) || {F, _} <- FileStamps, re:run(F, Regex) =/= nomatch],
        undefined
    catch
        throw:F -> {ok, F}
    end.

matches([], _) ->
    ok;
matches([{R, Stamp} | T], FileStamps) ->
    case find_match(R, FileStamps) of
        {ok, F} ->
            ?assertEqual(Stamp, proplists:get_value(F, FileStamps)),
            matches(T, FileStamps);
        undefined ->
            ?assertEqual({R, Stamp}, FileStamps)
    end.

edges([], _, _) ->
    ok;
edges([{A,B}|T], Edges, Files) ->
    {ok, AbsA} = find_match(A, Files),
    {ok, AbsB} = find_match(B, Files),
    ?assert(lists:member({AbsA, AbsB}, Edges)),
    edges(T, Edges, Files).

bump_file(F) ->
    {ok, Bin} = file:read_file(F),
    file:write_file(F, [Bin, "\n"]).

next_second() ->
    %% Sleep until the next second. Rather than just doing a
    %% sleep(1000) call, sleep for the amount of time required
    %% to reach the next second as seen by the OS; this can save us
    %% a few hundred milliseconds per test by triggering shorter delays.
    {Mega, Sec, Micro} = os:timestamp(),
    Now = (Mega*1000000 + Sec)*1000 + round(Micro/1000),
    Ms = (trunc(Now / 1000)*1000 + 1000) - Now,
    %% add a 50ms for jitter since the exact amount sometimes causes failures
    timer:sleep(max(Ms+50, 1000)).
