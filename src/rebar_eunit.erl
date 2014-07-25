%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
%% @author Dave Smith <dizzyd@dizzyd.com>
%% @doc rebar_eunit supports the following commands:
%% <ul>
%%   <li>eunit - runs eunit tests</li>
%%   <li>clean - remove ?EUNIT_DIR directory</li>
%%   <li>reset_after_eunit::boolean() - default = true.
%%       If true, try to "reset" VM state to approximate state prior to
%%       running the EUnit tests:
%%       <ul>
%%        <li>Stop net_kernel if it was started</li>
%%        <li>Stop OTP applications not running before EUnit tests were run</li>
%%        <li>Kill processes not running before EUnit tests were run</li>
%%        <li>Reset OTP application environment variables</li>
%%       </ul>
%%   </li>
%% </ul>
%% The following Global options are supported:
%% <ul>
%%   <li>verbose=1 - show extra output from the eunit test</li>
%%   <li>
%%      suites="foo,bar" - runs tests in foo.erl, test/foo_tests.erl and
%%      tests in bar.erl, test/bar_tests.erl
%%   </li>
%%   <li>
%%      suites="foo,bar" tests="baz"- runs first test with name starting
%%      with 'baz' in foo.erl, test/foo_tests.erl and tests in bar.erl,
%%      test/bar_tests.erl
%%   </li>
%%   <li>
%%      tests="baz"- For every existing suite, run the first test whose
%%      name starts with bar and, if no such test exists, run the test
%%      whose name starts with bar in the suite's _tests module
%%   </li>
%% </ul>
%% Additionally, for projects that have separate folders for the core
%% implementation, and for the unit tests, then the following
%% <code>rebar.config</code> option can be provided:
%% <code>{eunit_compile_opts, [{src_dirs, ["src", "dir"]}]}.</code>.
%% @copyright 2009, 2010 Dave Smith
%% -------------------------------------------------------------------
-module(rebar_eunit).

-export([eunit/2,
         clean/2]).

%% for internal use only
-export([info/2]).

-include("rebar.hrl").

-define(EUNIT_DIR, ".eunit").

%% ===================================================================
%% Public API
%% ===================================================================

eunit(Config, _AppFile) ->
    ok = ensure_dirs(),
    %% Save code path
    CodePath = setup_code_path(),
    CompileOnly = rebar_config:get_global(Config, compile_only, false),
    {ok, SrcErls} = rebar_erlc_compiler:test_compile(Config, "eunit",
                                                     ?EUNIT_DIR),
    case CompileOnly of
        "true" ->
            true = code:set_path(CodePath),
            ?CONSOLE("Compiled modules for eunit~n", []);
        false ->
            run_eunit(Config, CodePath, SrcErls)
    end.

clean(_Config, _File) ->
    rebar_file_utils:rm_rf(?EUNIT_DIR).

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, eunit) ->
    info_help("Run eunit tests");
info(help, clean) ->
    Description = ?FMT("Delete eunit test dir (~s)", [?EUNIT_DIR]),
    info_help(Description).

info_help(Description) ->
    ?CONSOLE(
       "~s.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  ~p~n"
       "  ~p~n"
       "  ~p~n"
       "  ~p~n"
       "  ~p~n"
       "  ~p~n"
       "Valid command line options:~n"
       "  suite[s]=\"foo,bar\" (Run tests in foo.erl, test/foo_tests.erl and~n"
       "                    tests in bar.erl, test/bar_tests.erl)~n"
       "  test[s]=\"baz\" (For every existing suite, run the first test whose~n"
       "               name starts with bar and, if no such test exists,~n"
       "               run the test whose name starts with bar in the~n"
       "               suite's _tests module)~n"
       "  random_suite_order=true (Run tests in random order)~n"
       "  random_suite_order=Seed (Run tests in random order,~n"
       "                           with the PRNG seeded with Seed)~n"
       "  compile_only=true (Compile but do not run tests)",
       [
        Description,
        {eunit_opts, []},
        {eunit_compile_opts, []},
        {eunit_first_files, []},
        {cover_enabled, false},
        {cover_print_enabled, false},
        {cover_export_enabled, false}
       ]).

run_eunit(Config, CodePath, SrcErls) ->
    %% Build a list of all the .beams in ?EUNIT_DIR -- use this for
    %% cover and eunit testing. Normally you can just tell cover
    %% and/or eunit to scan the directory for you, but eunit does a
    %% code:purge in conjunction with that scan and causes any cover
    %% compilation info to be lost.

    AllBeamFiles = rebar_utils:beams(?EUNIT_DIR),
    {BeamFiles, TestBeamFiles} =
        lists:partition(fun(N) -> string:str(N, "_tests.beam") =:= 0 end,
                        AllBeamFiles),
    OtherBeamFiles = TestBeamFiles --
        [filename:rootname(N) ++ "_tests.beam" || N <- AllBeamFiles],
    ModuleBeamFiles = randomize_suites(Config, BeamFiles ++ OtherBeamFiles),

    %% Get matching tests and modules
    AllModules = [rebar_utils:beam_to_mod(?EUNIT_DIR, N) || N <- AllBeamFiles],
    {Tests, FilteredModules} =
        get_tests_and_modules(Config, ModuleBeamFiles, AllModules),

    SrcModules = [rebar_utils:erl_to_mod(M) || M <- SrcErls],

    {ok, CoverLog} = rebar_cover_utils:init(Config, ModuleBeamFiles,
                                            eunit_dir()),

    StatusBefore = status_before_eunit(),
    EunitResult = perform_eunit(Config, Tests),

    rebar_cover_utils:perform_cover(Config, FilteredModules, SrcModules,
                                    eunit_dir()),
    rebar_cover_utils:close(CoverLog),

    case proplists:get_value(reset_after_eunit, get_eunit_opts(Config),
                             true) of
        true ->
            reset_after_eunit(StatusBefore);
        false ->
            ok
    end,

    %% Stop cover to clean the cover_server state. This is important if we want
    %% eunit+cover to not slow down when analyzing many Erlang modules.
    ok = rebar_cover_utils:exit(),

    case EunitResult of
        ok ->
            ok;
        _ ->
            ?ABORT("One or more eunit tests failed.~n", [])
    end,

    %% Restore code path
    true = code:set_path(CodePath),
    ok.

ensure_dirs() ->
    %% Make sure ?EUNIT_DIR/ and ebin/ directory exists (append dummy module)
    ok = filelib:ensure_dir(filename:join(eunit_dir(), "dummy")),
    ok = filelib:ensure_dir(filename:join(rebar_utils:ebin_dir(), "dummy")).

eunit_dir() ->
    filename:join(rebar_utils:get_cwd(), ?EUNIT_DIR).

setup_code_path() ->
    %% Setup code path prior to compilation so that parse_transforms
    %% and the like work properly. Also, be sure to add ebin_dir()
    %% to the END of the code path so that we don't have to jump
    %% through hoops to access the .app file
    CodePath = code:get_path(),
    true = code:add_patha(eunit_dir()),
    true = code:add_pathz(rebar_utils:ebin_dir()),
    CodePath.

%%
%% == get matching tests ==
%%
get_tests_and_modules(Config, ModuleBeamFiles, AllModules) ->
    SelectedSuites = get_selected_suites(Config, AllModules),
    {Tests, QualifiedTests} = get_qualified_and_unqualified_tests(Config),
    Modules = get_test_modules(SelectedSuites, Tests,
                               QualifiedTests, ModuleBeamFiles),
    FilteredModules = get_matching_modules(AllModules, Modules, QualifiedTests),
    MatchedTests = get_matching_tests(Modules, Tests, QualifiedTests),
    {MatchedTests, FilteredModules}.

%%
%% == get suites specified via 'suites' option ==
%%
get_selected_suites(Config, Modules) ->
    RawSuites = get_suites(Config),
    Suites = [list_to_atom(Suite) || Suite <- string:tokens(RawSuites, ",")],
    [M || M <- Suites, lists:member(M, Modules)].

get_suites(Config) ->
    case rebar_config:get_global(Config, suites, "") of
        "" ->
            rebar_config:get_global(Config, suite, "");
        Suites ->
            Suites
    end.

get_qualified_and_unqualified_tests(Config) ->
    RawFunctions = rebar_config:get_global(Config, tests, ""),
    FunctionNames = [FunctionName ||
                        FunctionName <- string:tokens(RawFunctions, ",")],
    get_qualified_and_unqualified_tests1(FunctionNames, [], []).

get_qualified_and_unqualified_tests1([], Functions, QualifiedFunctions) ->
    {Functions, QualifiedFunctions};
get_qualified_and_unqualified_tests1([TestName|TestNames], Functions,
                                     QualifiedFunctions) ->
    case string:tokens(TestName, ":") of
        [TestName] ->
            Function = list_to_atom(TestName),
            get_qualified_and_unqualified_tests1(
              TestNames, [Function|Functions], QualifiedFunctions);
        [ModuleName, FunctionName] ->
            M = list_to_atom(ModuleName),
            F = list_to_atom(FunctionName),
            get_qualified_and_unqualified_tests1(TestNames, Functions,
                                                 [{M, F}|QualifiedFunctions]);
        _ ->
            ?ABORT("Unsupported test function specification: ~s~n", [TestName])
    end.

%% Provide modules which are to be searched for tests.
%% Several scenarios are possible:
%%
%% == randomize suites ==
%%

randomize_suites(Config, Modules) ->
    case rebar_config:get_global(Config, random_suite_order, undefined) of
        undefined ->
            Modules;
        "true" ->
            Seed = crypto:rand_uniform(1, 65535),
            randomize_suites1(Modules, Seed);
        String ->
            try list_to_integer(String) of
                Seed ->
                    randomize_suites1(Modules, Seed)
            catch
                error:badarg ->
                    ?ERROR("Bad random seed provided: ~p~n", [String]),
                    ?FAIL
            end
    end.

randomize_suites1(Modules, Seed) ->
    _ = random:seed(35, Seed, 1337),
    ?CONSOLE("Randomizing suite order with seed ~b~n", [Seed]),
    [X||{_,X} <- lists:sort([{random:uniform(), M} || M <- Modules])].

%%
%% == get matching tests ==
%% 1) Specific tests have been provided and/or
%% no unqualified tests have been specified and
%% there were some qualified tests, then we can search for
%% functions in specified suites (or in empty set of suites).
%%
%% 2) Neither specific suites nor qualified test names have been
%% provided use ModuleBeamFiles which filters out "*_tests"
%% modules so EUnit won't doubly run them and cover only
%% calculates coverage on production code. However,
%% keep "*_tests" modules that are not automatically
%% included by EUnit.
%%
%% From 'Primitives' in the EUnit User's Guide
%% http://www.erlang.org/doc/apps/eunit/chapter.html
%% "In addition, EUnit will also look for another
%% module whose name is ModuleName plus the suffix
%% _tests, and if it exists, all the tests from that
%% module will also be added. (If ModuleName already
%% contains the suffix _tests, this is not done.) E.g.,
%% the specification {module, mymodule} will run all
%% tests in the modules mymodule and mymodule_tests.
%% Typically, the _tests module should only contain
%% test cases that use the public interface of the main
%% module (and no other code)."
get_test_modules(SelectedSuites, Tests, QualifiedTests, ModuleBeamFiles) ->
    SuitesProvided = SelectedSuites =/= [],
    OnlyQualifiedTestsProvided = QualifiedTests =/= [] andalso Tests =:= [],
    if
        SuitesProvided orelse OnlyQualifiedTestsProvided ->
            SelectedSuites;
        true ->
            [rebar_utils:beam_to_mod(?EUNIT_DIR, N) ||
                N <- ModuleBeamFiles]
    end.

get_matching_modules(AllModules, Modules, QualifiedTests) ->
    ModuleFilterMapper =
        fun({M, _}) ->
                case lists:member(M, AllModules) of
                    true -> {true, M};
                    _-> false
                end
        end,
    ModulesFromQualifiedTests = lists:zf(ModuleFilterMapper, QualifiedTests),
    lists:usort(Modules ++ ModulesFromQualifiedTests).

get_matching_tests(Modules, [], []) ->
    Modules;
get_matching_tests(Modules, [], QualifiedTests) ->
    FilteredQualifiedTests = filter_qualified_tests(Modules, QualifiedTests),
    lists:merge(Modules, make_test_primitives(FilteredQualifiedTests));
get_matching_tests(Modules, Tests, QualifiedTests) ->
    AllTests = lists:merge(QualifiedTests,
                           get_matching_tests1(Modules, Tests, [])),
    make_test_primitives(AllTests).

filter_qualified_tests(Modules, QualifiedTests) ->
    TestsFilter = fun({Module, _Function}) ->
                          lists:all(fun(M) -> M =/= Module end, Modules) end,
    lists:filter(TestsFilter, QualifiedTests).

get_matching_tests1([], _Functions, TestFunctions) ->
    TestFunctions;

get_matching_tests1([Module|TModules], Functions, TestFunctions) ->
    %% Get module exports
    ModuleStr = atom_to_list(Module),
    ModuleExports = get_beam_test_exports(ModuleStr),
    %% Get module _tests exports
    TestModuleStr = string:concat(ModuleStr, "_tests"),
    TestModuleExports = get_beam_test_exports(TestModuleStr),
    %% Build tests {M, F} list
    Tests = get_matching_tests2(Functions, {Module, ModuleExports},
                                {list_to_atom(TestModuleStr),
                                 TestModuleExports}),
    get_matching_tests1(TModules, Functions,
                        lists:merge([TestFunctions, Tests])).

get_matching_tests2(Functions, {Mod, ModExports}, {TestMod, TestModExports}) ->
    %% Look for matching functions into ModExports
    ModExportsStr = [atom_to_list(E1) || E1 <- ModExports],
    TestModExportsStr = [atom_to_list(E2) || E2 <- TestModExports],
    get_matching_exports(Functions, {Mod, ModExportsStr},
                         {TestMod, TestModExportsStr}, []).

get_matching_exports([], _, _, Matched) ->
    Matched;
get_matching_exports([Function|TFunctions], {Mod, ModExportsStr},
                     {TestMod, TestModExportsStr}, Matched) ->

    FunctionStr = atom_to_list(Function),
    %% Get matching Function in module, otherwise look in _tests module
    NewMatch = case get_matching_export(FunctionStr, ModExportsStr) of
                   [] ->
                       {TestMod, get_matching_export(FunctionStr,
                                                     TestModExportsStr)};
                   MatchingExport ->
                       {Mod, MatchingExport}
               end,
    case NewMatch of
        {_, []} ->
            get_matching_exports(TFunctions, {Mod, ModExportsStr},
                                 {TestMod, TestModExportsStr}, Matched);
        _ ->
            get_matching_exports(TFunctions, {Mod, ModExportsStr},
                                 {TestMod, TestModExportsStr},
                                 [NewMatch|Matched])
    end.

get_matching_export(_FunctionStr, []) ->
    [];
get_matching_export(FunctionStr, [ExportStr|TExportsStr]) ->
    case string:str(ExportStr, FunctionStr) of
        1 ->
            list_to_atom(ExportStr);
        _ ->
            get_matching_export(FunctionStr, TExportsStr)
    end.

get_beam_test_exports(ModuleStr) ->
    FilePath = filename:join(eunit_dir(),
                             string:concat(ModuleStr, ".beam")),
    case filelib:is_regular(FilePath) of
        true ->
            {beam_file, _, Exports0, _, _, _} = beam_disasm:file(FilePath),
            Exports1 = [FunName || {FunName, FunArity, _} <- Exports0,
                                   FunArity =:= 0],
            F = fun(FName) ->
                        FNameStr = atom_to_list(FName),
                        re:run(FNameStr, "_test(_)?") =/= nomatch
                end,
            lists:filter(F, Exports1);
        _ ->
            []
    end.

make_test_primitives(RawTests) ->
    %% Use {test,M,F} and {generator,M,F} if at least R15B02. Otherwise,
    %% use eunit_test:function_wrapper/2 fallback.
    %% eunit_test:function_wrapper/2 was renamed to eunit_test:mf_wrapper/2
    %% in R15B02; use that as >= R15B02 check.
    %% TODO: remove fallback and use only {test,M,F} and {generator,M,F}
    %% primitives once at least R15B02 is required.
    {module, eunit_test} = code:ensure_loaded(eunit_test),
    MakePrimitive = case erlang:function_exported(eunit_test, mf_wrapper, 2) of
                        true  -> fun eunit_primitive/3;
                        false -> fun pre15b02_eunit_primitive/3
                    end,

    ?CONSOLE("    Running test function(s):~n", []),
    F = fun({M, F2}, Acc) ->
                ?CONSOLE("      ~p:~p/0~n", [M, F2]),
                FNameStr = atom_to_list(F2),
                NewFunction =
                    case re:run(FNameStr, "_test_") of
                        nomatch ->
                            %% Normal test
                            MakePrimitive(test, M, F2);
                        _ ->
                            %% Generator
                            MakePrimitive(generator, M, F2)
                    end,
                [NewFunction|Acc]
        end,
    lists:foldl(F, [], RawTests).

eunit_primitive(Type, M, F) ->
    {Type, M, F}.

pre15b02_eunit_primitive(test, M, F) ->
    eunit_test:function_wrapper(M, F);
pre15b02_eunit_primitive(generator, M, F) ->
    {generator, eunit_test:function_wrapper(M, F)}.

%%
%% == run tests ==
%%

perform_eunit(Config, Tests) ->
    EunitOpts = get_eunit_opts(Config),

    %% Move down into ?EUNIT_DIR while we run tests so any generated files
    %% are created there (versus in the source dir)
    Cwd = rebar_utils:get_cwd(),
    ok = file:set_cwd(?EUNIT_DIR),

    EunitResult = (catch eunit:test(Tests, EunitOpts)),

    %% Return to original working dir
    ok = file:set_cwd(Cwd),

    EunitResult.

get_eunit_opts(Config) ->
    %% Enable verbose in eunit if so requested..
    BaseOpts = case rebar_log:is_verbose(Config) of
                   true ->
                       [verbose];
                   false ->
                       []
               end,

    BaseOpts ++ rebar_config:get_list(Config, eunit_opts, []).

%%
%% == reset_after_eunit ==
%%

status_before_eunit() ->
    Apps = get_app_names(),
    AppEnvs = [{App, application:get_all_env(App)} || App <- Apps],
    {erlang:processes(), erlang:is_alive(), AppEnvs, ets:tab2list(ac_tab)}.

get_app_names() ->
    [AppName || {AppName, _, _} <- application:loaded_applications()].

reset_after_eunit({OldProcesses, WasAlive, OldAppEnvs, _OldACs}) ->
    IsAlive = erlang:is_alive(),
    if not WasAlive andalso IsAlive ->
            ?DEBUG("Stopping net kernel....\n", []),
            erl_epmd:stop(),
            _ = net_kernel:stop(),
            pause_until_net_kernel_stopped();
       true ->
            ok
    end,

    OldApps = [App || {App, _} <- OldAppEnvs],
    Apps = get_app_names(),
    _ = [begin
             _ = case lists:member(App, OldApps) of
                     true  -> ok;
                     false -> application:stop(App)
                 end,
             ok = application:unset_env(App, K)
         end || App <- Apps, App /= rebar,
                {K, _V} <- application:get_all_env(App),
                K =/= included_applications],

    reconstruct_app_env_vars(Apps),

    Processes = erlang:processes(),
    _ = kill_extras(Processes -- OldProcesses),

    ok.

kill_extras(Pids) ->
    %% Killing any of the procs below will either:
    %% 1. Interfere with stuff that we don't want interfered with, or
    %% 2. May/will force the 'kernel' app to shutdown, which *will*
    %%    interfere with rebar's ability To Do Useful Stuff(tm).
    %% This list may require changes as OTP versions and/or
    %% rebar use cases change.
    KeepProcs = [cover_server, eunit_server,
                 eqc, eqc_license, eqc_locked,
                 %% inet_gethost_native is started on demand, when
                 %% doing name lookups. It is under kernel_sup, under
                 %% a supervisor_bridge.
                 inet_gethost_native],
    Killed = [begin
                  Info = case erlang:process_info(Pid) of
                             undefined -> [];
                             Else      -> Else
                         end,
                  Keep1 = case proplists:get_value(registered_name, Info) of
                              undefined ->
                                  false;
                              Name ->
                                  lists:member(Name, KeepProcs)
                          end,
                  Keep2 = case proplists:get_value(dictionary, Info) of
                              undefined ->
                                  false;
                              Ds ->
                                  case proplists:get_value('$ancestors', Ds) of
                                      undefined ->
                                          false;
                                      As ->
                                          lists:member(kernel_sup, As)
                                  end
                          end,
                  if Keep1 orelse Keep2 ->
                          ok;
                     true ->
                          ?DEBUG("Kill ~p ~p\n", [Pid, Info]),
                          exit(Pid, kill),
                          Pid
                  end
              end || Pid <- Pids],
    case lists:usort(Killed) -- [ok] of
        [] ->
            ?DEBUG("No processes to kill\n", []),
            [];
        Else ->
            lists:foreach(fun(Pid) -> wait_until_dead(Pid) end, Else),
            Else
    end.

reconstruct_app_env_vars([App|Apps]) ->
    CmdLine0 = proplists:get_value(App, init:get_arguments(), []),
    CmdVars = [{list_to_atom(K), list_to_atom(V)} || {K, V} <- CmdLine0],
    AppFile = (catch filename:join([code:lib_dir(App),
                                    "ebin",
                                    atom_to_list(App) ++ ".app"])),
    AppVars = case file:consult(AppFile) of
                  {ok, [{application, App, Ps}]} ->
                      proplists:get_value(env, Ps, []);
                  _ ->
                      []
              end,

    %% App vars specified in config files override those in the .app file.
    %% Config files later in the args list override earlier ones.
    AppVars1 = case init:get_argument(config) of
                   {ok, ConfigFiles} ->
                       {App, MergedAppVars} = lists:foldl(fun merge_app_vars/2,
                                                          {App, AppVars},
                                                          ConfigFiles),
                       MergedAppVars;
                   error ->
                       AppVars
               end,
    AllVars = CmdVars ++ AppVars1,
    ?DEBUG("Reconstruct ~p ~p\n", [App, AllVars]),
    lists:foreach(fun({K, V}) -> application:set_env(App, K, V) end, AllVars),
    reconstruct_app_env_vars(Apps);
reconstruct_app_env_vars([]) ->
    ok.

merge_app_vars(ConfigFile, {App, AppVars}) ->
    File = ensure_config_extension(ConfigFile),
    FileAppVars = app_vars_from_config_file(File, App),
    Dict1 = dict:from_list(AppVars),
    Dict2 = dict:from_list(FileAppVars),
    Dict3 = dict:merge(fun(_Key, _Value1, Value2) -> Value2 end, Dict1, Dict2),
    {App, dict:to_list(Dict3)}.

ensure_config_extension(File) ->
    %% config files must end with .config on disk but when specifying them
    %% via the -config option the extension is optional
    BaseFileName = filename:basename(File, ".config"),
    DirName = filename:dirname(File),
    filename:join(DirName, BaseFileName ++ ".config").

app_vars_from_config_file(File, App) ->
    case file:consult(File) of
        {ok, [Env]} ->
            proplists:get_value(App, Env, []);
        _ ->
            []
    end.

wait_until_dead(Pid) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, _Obj, Info} ->
            Info
    after 10*1000 ->
            exit({timeout_waiting_for, Pid})
    end;
wait_until_dead(_) ->
    ok.

pause_until_net_kernel_stopped() ->
    pause_until_net_kernel_stopped(10).

pause_until_net_kernel_stopped(0) ->
    exit(net_kernel_stop_failed);
pause_until_net_kernel_stopped(N) ->
    case node() of
        'nonode@nohost' ->
            ?DEBUG("Stopped net kernel.\n", []),
            ok;
        _ ->
            timer:sleep(100),
            pause_until_net_kernel_stopped(N - 1)
    end.
