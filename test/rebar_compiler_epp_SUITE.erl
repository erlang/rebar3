%%% @doc
%%% Unit tests for epp-related compiler utils.
%%% Make it easier to validate internal behaviour of compiler data and
%%% handling of module parsing without having to actually set up
%%% entire projects.
%%% @end
-module(rebar_compiler_epp_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).

all() ->
    [{group, module}].

groups() ->
    [{module, [], [
        analyze, analyze_old_behaviour, analyze_old_behavior,
        analyze_empty, analyze_bad_mod,
        resolve_module
     ]}
    ].

init_per_group(module, Config) ->
    to_file(Config, {"direct.hrl", "-direct(val). "}),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% module analysis group %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

analyze() ->
    [{docs, "Analyzing a module returns all the "
            "parseable dependencies for it in a map."}].
analyze(Config) ->
    ?assert(check_analyze(
       #{include => [
           "eunit-[0-9.]+/include/eunit.hrl$",
           "stdlib-[0-9.]+/include/assert.hrl$",
           "/direct.hrl$"
         ],
         %% missing includes
         missing_include_file => [
            "^false.hrl$"
         ],
         missing_include_lib => [
            "^some_app/include/lib.hrl$"
         ],
         parse_transform => [
            erl_id_trans,
            eunit_autoexport, % added by include file!
            missing_parse_trans1,
            missing_parse_trans2
         ],
         behaviour => [gen_server, gen_statem],
         is_behaviour => true
       },
       rebar_compiler_epp:deps(
         to_file(Config, fake_mod()),
         [{includes, []}, {macros, []}]
       )
    )),
    ok.

analyze_old_behaviour() ->
    [{docs, "Analyzing old-style behaviour annotation"}].
analyze_old_behaviour(Config) ->
    ?assert(check_analyze(
       #{include => [],
         missing_include_file => [],
         missing_include_lib => [],
         parse_transform => [],
         behaviour => [],
         is_behaviour => true
       },
       rebar_compiler_epp:deps(
         to_file(Config, old_behaviour_mod()),
         [{includes, []}, {macros, []}]
       )
    )),
    ok.

analyze_old_behavior() ->
    [{docs, "Analyzing old-style behavior annotation"}].
analyze_old_behavior(Config) ->
    ?assert(check_analyze(
       #{include => [],
         missing_include_file => [],
         missing_include_lib => [],
         parse_transform => [],
         behaviour => [],
         is_behaviour => true
       },
       rebar_compiler_epp:deps(
         to_file(Config, old_behavior_mod()),
         [{includes, []}, {macros, []}]
       )
    )),
    ok.

analyze_empty() ->
    [{docs, "Making sure empty files are properly handled as valid but null "
            "and let some other compiler phase handle this. We follow "
            "what EPP handles."}].
analyze_empty(Config) ->
    ?assert(check_analyze(
       #{include => [],
         missing_include_file => [],
         missing_include_lib => [],
         parse_transform => [],
         behaviour => [],
         is_behaviour => false
       },
       rebar_compiler_epp:deps(
         to_file(Config, empty_mod()),
         [{includes, []}, {macros, []}]
       )
    )),
    ok.

analyze_bad_mod() ->
    [{docs, "Errors for bad modules that don't compile are skipped "
            "by EPP and so we defer that to a later phase of the "
            "compilation process"}].
analyze_bad_mod(Config) ->
    ?assert(check_analyze(
       #{include => [],
         missing_include_file => [],
         missing_include_lib => [],
         parse_transform => [],
         behaviour => [],
         is_behaviour => false
       },
       rebar_compiler_epp:deps(
         to_file(Config, bad_mod()),
         [{includes, []}, {macros, []}]
       )
    )),
    ok.

resolve_module() ->
    [{doc, "given a module name and a bunch of paths, find "
           "the first path that matches the module"}].
resolve_module(Config) ->
    Path1 = to_file(Config, fake_mod()),
    Path2 = to_file(Config, old_behaviour_mod()),
    Path3 = to_file(Config, empty_mod()),
    ?assertEqual(
       {ok, Path2},
       rebar_compiler_epp:resolve_module(
         old_behaviour,
         [Path1, Path2, Path3]
       )
    ),
    ok.

%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

%% check each field of `Map' and validate them against `CheckMap'.
%% This allows to check each value in the map has a matching assertion.
%% Then check each field of `CheckMap' against `Map' to find if
%% any missing value exists.
check_analyze(CheckMap, Map) ->
    ct:pal("check_analyze:~n~p~n~p", [CheckMap, Map]),
    maps:fold(fun(K,V,Acc) -> check(CheckMap, K, V) and Acc end,
              true, Map)
    andalso
    maps:fold(
      fun(K,_,Acc) ->
          check(CheckMap, K, maps:get(K, Map, make_ref())) and Acc
      end,
      true,
      Map
    ).

check(Map, K, V) ->
    case maps:is_key(K, Map) of
        false -> false;
        true ->
            #{K := Val} = Map,
            compare_val(Val, V)
    end.

%% two identical values always works
compare_val(V, V) ->
    true;
%% compare lists of strings; each string must be checked individually
%% because they are assumed to be regexes.
compare_val(V1, V2) when is_list(hd(V1)) ->
    match_regexes(V1, V2);
compare_val(V1, _V2) when not is_integer(hd(V1)) ->
    %% failing list of some sort, but not a string
    false;
%% strings as regexes
compare_val(V1, V2) when is_list(V1) ->
    match_regex(V1, [V2]) =/= nomatch;
%% anything else is not literally the same and is bad
compare_val(_, _) ->
    false.

match_regexes([], List) ->
    List == []; % no extra patterns, that would be weird
match_regexes([H|T], List) ->
    case match_regex(H, List) of
       nomatch ->
            false;
       {ok, Entry} ->
            match_regexes(T, List -- [Entry])
    end.

match_regex(_Pattern, []) ->
    nomatch;
match_regex(Pattern, [H|T]) ->
    case re:run(H, Pattern) of
        nomatch -> match_regex(Pattern, T);
        _ -> {ok, H}
    end.

%% custom zip function that causes value failures (by using make_ref()
%% that will never match in compare_val/2) rather than crashing because
%% of lists of different lengths.
zip([], []) -> [];
zip([], [H|T]) -> [{make_ref(),H} | zip([], T)];
zip([H|T], []) -> [{H,make_ref()} | zip(T, [])];
zip([X|Xs], [Y|Ys]) -> [{X,Y} | zip(Xs, Ys)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module specifications %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% turn a module string to a file that will live in CT's scratch dir
to_file(Config, {Name,Contents}) ->
    Path = filename:join([?config(priv_dir, Config), Name]),
    file:write_file(Path, Contents, [sync]),
    Path.

%% base module with all the interesting includes and attributes
%% we want to track
fake_mod() ->
    {"somemod.erl", "
-module(somemod).
-export([f/1]).
-include(\"direct.hrl\").
-include(\"direct.hrl\").
-include_lib(\"some_app/include/lib.hrl\").
-include_lib(\"eunit/include/eunit.hrl\").
-compile({parse_transform, {erl_id_trans, []}}).
-compile({parse_transform, missing_parse_trans1}).
-compile([{parse_transform, {missing_parse_trans2, []}}]).
-behaviour(gen_server).
-behavior(gen_statem).
-callback f() -> ok.
-ifdef(OPT).
-include(\"true.hrl\").
-else.
-include(\"false.hrl\").
-endif.
f(X) -> X.
    "}.

%% variations for attributes that can't be checked in the
%% same base module
old_behaviour_mod() ->
    {"old_behaviour.erl", "
-module(old_behaviour).
-export([f/1, behaviour_info/1]).
f(X) -> X.
behaviour_info(callbacks) -> [{f,1}].
    "}.

old_behavior_mod() ->
    {"old_behaviour.erl", "
-module(old_behaviour).
-export([f/1, behaviour_info/1]).
f(X) -> X.
behavior_info(callbacks) -> [{f,1}].
    "}.

empty_mod() ->
    {"empty.erl", ""}.

bad_mod() ->
    {"badmod.erl", "
-module(bad_mod). % wrong name!
f(x) -> X+1. % bad vars
f((x)cv) -> bad syntax.
    "}.

