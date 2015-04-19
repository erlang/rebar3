%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_xref_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         xref_test/1,
         xref_ignore_test/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%% ===================================================================
%% common_test callbacks
%% ===================================================================

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(Case, Config) ->
    UpdConfig = rebar_test_utils:init_rebar_state(Config),
    AppDir = ?config(apps, UpdConfig),
    file:set_cwd(AppDir),
    Name = rebar_test_utils:create_random_name("xrefapp_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_empty_app(AppDir, Name, Vsn, [kernel, stdlib]),
    AppModules = [behaviour1, behaviour2, mymod, othermod],
    [write_src_file(AppDir, Name, Module, ignore_xref(Case)) || Module <- AppModules],
    RebarConfig = [{erl_opts, [debug_info]},
                   {xref_checks, [deprecated_function_calls,deprecated_functions,
                                  undefined_function_calls,undefined_functions,
                                  exports_not_used,locals_not_used]}],
    [{app_name, Name},
     {rebar_config, RebarConfig} | UpdConfig].

end_per_testcase(_, _Config) ->
    ok.

all() ->
    [xref_test, xref_ignore_test].

%% ===================================================================
%% Test cases
%% ===================================================================

xref_test(Config) ->
    AppDir = ?config(apps, Config),
    State = ?config(state, Config),
    Name = ?config(app_name, Config),
    RebarConfig = ?config(rebar_config, Config),
    Result = rebar3:run(rebar_state:new(State, RebarConfig, AppDir), ["xref"]),
    verify_results(xref_test, Name, Result).

xref_ignore_test(Config) ->
    AppDir = ?config(apps, Config),
    State = ?config(state, Config),
    Name = ?config(app_name, Config),
    RebarConfig = ?config(rebar_config, Config),
    Result = rebar3:run(rebar_state:new(State, RebarConfig, AppDir), ["xref"]),
    verify_results(xref_ignore_test, Name, Result).

%% ===================================================================
%% Helper functions
%% ===================================================================

ignore_xref(xref_ignore_test) ->
    true;
ignore_xref(_) ->
    false.

verify_results(TestCase, AppName, Results) ->
    {error, {rebar_prv_xref,
             {xref_issues, XrefResults, QueryResults}}} = Results,
    verify_test_results(TestCase, AppName, XrefResults, QueryResults).

verify_test_results(xref_test, AppName, XrefResults, _QueryResults) ->
    AppModules = ["behaviour1", "behaviour2", "mymod", "othermod", "somemod"],
    [Behaviour1Mod, Behaviour2Mod, MyMod, OtherMod, SomeMod] =
        [list_to_atom(AppName ++ "_" ++ Mod) || Mod <- AppModules],
    UndefFuns = proplists:get_value(undefined_functions, XrefResults),
    UndefFunCalls = proplists:get_value(undefined_function_calls, XrefResults),
    LocalsNotUsed = proplists:get_value(locals_not_used, XrefResults),
    ExportsNotUsed = proplists:get_value(exports_not_used, XrefResults),
    DeprecatedFuns = proplists:get_value(deprecated_functions, XrefResults),
    DeprecatedFunCalls = proplists:get_value(deprecated_function_calls, XrefResults),
    ?assert(lists:member({SomeMod, notavailable, 1}, UndefFuns)),
    ?assert(lists:member({{OtherMod, somefunc, 0}, {SomeMod, notavailable, 1}},
                         UndefFunCalls)),
    ?assert(lists:member({MyMod, fdeprecated, 0}, DeprecatedFuns)),
    ?assert(lists:member({{OtherMod, somefunc, 0}, {MyMod, fdeprecated, 0}},
                         DeprecatedFunCalls)),
    ?assert(lists:member({MyMod, localfunc2, 0}, LocalsNotUsed)),
    ?assert(lists:member({Behaviour1Mod, behaviour_info, 1}, ExportsNotUsed)),
    ?assert(lists:member({Behaviour2Mod, behaviour_info, 1}, ExportsNotUsed)),
    ?assert(lists:member({MyMod, other2, 1}, ExportsNotUsed)),
    ?assert(lists:member({OtherMod, somefunc, 0}, ExportsNotUsed)),
    ?assertNot(lists:member({MyMod, bh1_a, 1}, ExportsNotUsed)),
    ?assertNot(lists:member({MyMod, bh1_b, 1}, ExportsNotUsed)),
    ?assertNot(lists:member({MyMod, bh2_a, 1}, ExportsNotUsed)),
    ?assertNot(lists:member({MyMod, bh2_b, 1}, ExportsNotUsed)),
    ok;
verify_test_results(xref_ignore_test, AppName, XrefResults, _QueryResults) ->
    AppModules = ["behaviour1", "behaviour2", "mymod", "othermod", "somemod"],
    [_Behaviour1Mod, _Behaviour2Mod, _MyMod, _OtherMod, SomeMod] =
        [list_to_atom(AppName ++ "_" ++ Mod) || Mod <- AppModules],
    UndefFuns = proplists:get_value(undefined_functions, XrefResults),
    ?assertNot(lists:keymember(undefined_function_calls, 1, XrefResults)),
    ?assertNot(lists:keymember(locals_not_used, 1, XrefResults)),
    ?assertNot(lists:keymember(exports_not_used, 1, XrefResults)),
    ?assertNot(lists:keymember(deprecated_functions, 1, XrefResults)),
    ?assertNot(lists:keymember(deprecated_function_calls, 1, XrefResults)),
    ?assert(lists:member({SomeMod, notavailable, 1}, UndefFuns)),
    ok.

write_src_file(Dir, AppName, Module, IgnoreXref) ->
    Erl = filename:join([Dir, "src", module_name(AppName, Module)]),
    ok = filelib:ensure_dir(Erl),
    ok = ec_file:write(Erl, get_module_body(Module, AppName, IgnoreXref)).

module_name(AppName, Module) ->
    lists:flatten([AppName, "_", atom_to_list(Module), ".erl"]).

get_module_body(behaviour1, AppName, IgnoreXref) ->
    ["-module(", AppName, "_behaviour1).\n",
     "-export([behaviour_info/1]).\n",
     ["-ignore_xref({behaviour_info,1}).\n" || X <- [IgnoreXref], X =:= true],
     "behaviour_info(callbacks) -> [{bh1_a,1},{bh1_b,1}];\n",
     "behaviour_info(_Other) -> undefined.\n"];
get_module_body(behaviour2, AppName, IgnoreXref) ->
    ["-module(", AppName, "_behaviour2).\n",
     "-export([behaviour_info/1]).\n",
     ["-ignore_xref({behaviour_info,1}).\n" || X <- [IgnoreXref], X =:= true],
     "behaviour_info(callbacks) -> [{bh2_a,1},{bh2_b,1}];\n",
     "behaviour_info(_Other) -> undefined.\n"];
get_module_body(mymod, AppName, IgnoreXref) ->
    ["-module(", AppName, "_mymod).\n",
     "-export([bh1_a/1,bh1_b/1,bh2_a/1,bh2_b/1,"
     "other1/1,other2/1,fdeprecated/0]).\n",
     ["-ignore_xref([{other2,1},{localfunc2,0},{fdeprecated,0}]).\n"
      || X <- [IgnoreXref], X =:= true],
     "-behaviour(", AppName, "_behaviour1).\n",     % 2 behaviours
     "-behaviour(", AppName, "_behaviour2).\n",
     "-deprecated({fdeprecated,0}).\n",      % deprecated function
     "bh1_a(A) -> localfunc1(bh1_a, A).\n", % behaviour functions
     "bh1_b(A) -> localfunc1(bh1_b, A).\n",
     "bh2_a(A) -> localfunc1(bh2_a, A).\n",
     "bh2_b(A) -> localfunc1(bh2_b, A).\n",
     "other1(A) -> localfunc1(other1, A).\n", % regular exported functions
     "other2(A) -> localfunc1(other2, A).\n",
     "localfunc1(A, B) -> {A, B}.\n",       % used local
     "localfunc2() -> ok.\n",               % unused local
     "fdeprecated() -> ok.\n"              % deprecated function
    ];
get_module_body(othermod, AppName, IgnoreXref) ->
    ["-module(", AppName, "_othermod).\n",
     "-export([somefunc/0]).\n",
     [["-ignore_xref([{", AppName, "_somemod,notavailable,1},{somefunc,0}]).\n",
       "-ignore_xref({", AppName, "_mymod,fdeprecated,0}).\n"]
      || X <- [IgnoreXref], X =:= true],
     "somefunc() ->\n",
     "   ", AppName, "_mymod:other1(arg),\n",
     "   ", AppName, "_somemod:notavailable(arg),\n",
     "   ", AppName, "_mymod:fdeprecated().\n"].
