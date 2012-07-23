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
%%   <li>clean - remove ?TEST_DIR directory</li>
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
%%   <li>suites="foo,bar" - runs test/foo_tests.erl and test/bar_tests.erl</li>
%% </ul>
%% Additionally, for projects that have separate folders for the core
%% implementation, and for the unit tests, then the following
%% <code>rebar.config</code> option can be provided:
%% <code>{eunit_compile_opts, [{src_dirs, ["dir"]}]}.</code>.
%% @copyright 2009, 2010 Dave Smith
%% -------------------------------------------------------------------
-module(rebar_eunit).

-export([eunit/2,
         clean/2,
         'test-compile'/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

eunit(Config, _AppFile) ->
    ok = ensure_dirs(),
    %% Save code path
    CodePath = setup_code_path(),

    {ok, SrcErls} = rebar_erlc_compiler:test_compile(Config),

    %% Build a list of all the .beams in ?TEST_DIR -- use this for
    %% cover and eunit testing. Normally you can just tell cover
    %% and/or eunit to scan the directory for you, but eunit does a
    %% code:purge in conjunction with that scan and causes any cover
    %% compilation info to be lost.  Filter out "*_tests" modules so
    %% eunit won't doubly run them and so cover only calculates
    %% coverage on production code.  However, keep "*_tests" modules
    %% that are not automatically included by eunit.
    AllBeamFiles = rebar_utils:beams(?TEST_DIR),
    {BeamFiles, TestBeamFiles} =
        lists:partition(fun(N) -> string:str(N, "_tests.beam") =:= 0 end,
                        AllBeamFiles),
    OtherBeamFiles = TestBeamFiles --
        [filename:rootname(N) ++ "_tests.beam" || N <- AllBeamFiles],
    ModuleBeamFiles = BeamFiles ++ OtherBeamFiles,
    Modules = [rebar_utils:beam_to_mod(?TEST_DIR, N) || N <- ModuleBeamFiles],
    SrcModules = [rebar_utils:erl_to_mod(M) || M <- SrcErls],
    FilteredModules = filter_modules(Config, Modules),

    {ok, CoverLog} = cover_init(Config, ModuleBeamFiles),

    StatusBefore = status_before_eunit(),
    EunitResult = perform_eunit(Config, FilteredModules),
    perform_cover(Config, FilteredModules, SrcModules),

    cover_close(CoverLog),

    case proplists:get_value(reset_after_eunit, get_eunit_opts(Config),
                             true) of
        true ->
            reset_after_eunit(StatusBefore);
        false ->
            ok
    end,

    case EunitResult of
        ok ->
            ok;
        _ ->
            ?ABORT("One or more eunit tests failed.~n", [])
    end,

    %% Restore code path
    true = code:set_path(CodePath),
    ok.

clean(_Config, _File) ->
    rebar_file_utils:rm_rf(?TEST_DIR).

'test-compile'(Config, _File) ->
    ok = ensure_dirs(),
    %% Save code path
    CodePath = setup_code_path(),
    {ok, _SrcErls} = rebar_erlc_compiler:test_compile(Config),
    %% Restore code path
    true = code:set_path(CodePath),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_dirs() ->
    %% Make sure ?TEST_DIR/ and ebin/ directory exists (append dummy module)
    ok = filelib:ensure_dir(filename:join(rebar_utils:test_dir(), "dummy")),
    ok = filelib:ensure_dir(filename:join(rebar_utils:ebin_dir(), "dummy")).

setup_code_path() ->
    %% Setup code path prior to compilation so that parse_transforms
    %% and the like work properly. Also, be sure to add ebin_dir()
    %% to the END of the code path so that we don't have to jump
    %% through hoops to access the .app file
    CodePath = code:get_path(),
    true = code:add_patha(rebar_utils:test_dir()),
    true = code:add_pathz(rebar_utils:ebin_dir()),
    CodePath.

filter_modules(Config, Modules) ->
    RawSuites = rebar_config:get_global(Config, suites, ""),
    Suites = [list_to_atom(Suite) || Suite <- string:tokens(RawSuites, ",")],
    filter_modules1(Modules, Suites).

filter_modules1(Modules, []) ->
    Modules;
filter_modules1(Modules, Suites) ->
    [M || M <- Modules, lists:member(M, Suites)].

perform_eunit(Config, FilteredModules) ->
    EunitOpts = get_eunit_opts(Config),

    %% Move down into ?TEST_DIR while we run tests so any generated files
    %% are created there (versus in the source dir)
    Cwd = rebar_utils:get_cwd(),
    ok = file:set_cwd(?TEST_DIR),

    EunitResult = (catch eunit:test(FilteredModules, EunitOpts)),

    %% Return to original working dir
    ok = file:set_cwd(Cwd),

    EunitResult.

get_eunit_opts(Config) ->
    %% Enable verbose in eunit if so requested..
    BaseOpts = case rebar_config:is_verbose(Config) of
                   true ->
                       [verbose];
                   false ->
                       []
               end,

    BaseOpts ++ rebar_config:get_list(Config, eunit_opts, []).

perform_cover(Config, BeamFiles, SrcModules) ->
    perform_cover(rebar_config:get(Config, cover_enabled, false),
                  Config, BeamFiles, SrcModules).

perform_cover(false, _Config, _BeamFiles, _SrcModules) ->
    ok;
perform_cover(true, Config, BeamFiles, SrcModules) ->
    cover_analyze(Config, BeamFiles, SrcModules).

cover_analyze(_Config, [], _SrcModules) ->
    ok;
cover_analyze(Config, FilteredModules, SrcModules) ->
    %% Generate coverage info for all the cover-compiled modules
    Coverage = lists:flatten([cover_analyze_mod(M) || M <- FilteredModules]),

    %% Write index of coverage info
    cover_write_index(lists:sort(Coverage), SrcModules),

    %% Write coverage details for each file
    lists:foreach(fun({M, _, _}) ->
                          {ok, _} = cover:analyze_to_file(M, cover_file(M),
                                                          [html])
                  end, Coverage),

    Index = filename:join([rebar_utils:get_cwd(), ?TEST_DIR, "index.html"]),
    ?CONSOLE("Cover analysis: ~s\n", [Index]),

    %% Print coverage report, if configured
    case rebar_config:get(Config, cover_print_enabled, false) of
        true ->
            cover_print_coverage(lists:sort(Coverage));
        false ->
            ok
    end.

cover_close(not_enabled) ->
    ok;
cover_close(F) ->
    ok = file:close(F).

cover_init(false, _BeamFiles) ->
    {ok, not_enabled};
cover_init(true, BeamFiles) ->
    %% Attempt to start the cover server, then set it's group leader to
    %% ?TEST_DIR/cover.log, so all cover log messages will go there instead of
    %% to stdout. If the cover server is already started we'll reuse that
    %% pid.
    {ok, CoverPid} = case cover:start() of
                         {ok, _P} = OkStart ->
                             OkStart;
                         {error,{already_started, P}} ->
                             {ok, P};
                         {error, _Reason} = ErrorStart ->
                             ErrorStart
                     end,

    {ok, F} = OkOpen = file:open(
                         filename:join([?TEST_DIR, "cover.log"]),
                         [write]),

    group_leader(F, CoverPid),

    %% Make sure any previous runs of cover don't unduly influence
    cover:reset(),

    ?INFO("Cover compiling ~s\n", [rebar_utils:get_cwd()]),

    Compiled = [{Beam, cover:compile_beam(Beam)} || Beam <- BeamFiles],
    case [Module || {_, {ok, Module}} <- Compiled] of
        [] ->
            %% No modules compiled successfully...fail
            ?ERROR("Cover failed to compile any modules; aborting.~n", []),
            ?ABORT;
        _ ->
            %% At least one module compiled successfully

            %% It's not an error for cover compilation to fail partially,
            %% but we do want to warn about them
            PrintWarning =
                fun(Beam, Desc) ->
                        ?CONSOLE("Cover compilation warning for ~p: ~p",
                                 [Beam, Desc])
                end,
            _ = [PrintWarning(Beam, Desc) || {Beam, {error, Desc}} <- Compiled],
            OkOpen
    end;
cover_init(Config, BeamFiles) ->
    cover_init(rebar_config:get(Config, cover_enabled, false), BeamFiles).

cover_analyze_mod(Module) ->
    case cover:analyze(Module, coverage, module) of
        {ok, {Module, {Covered, NotCovered}}} ->
            %% Modules that include the eunit header get an implicit
            %% test/0 fun, which cover considers a runnable line, but
            %% eunit:test(TestRepresentation) never calls.  Decrement
            %% NotCovered in this case.
            [align_notcovered_count(Module, Covered, NotCovered,
                                    is_eunitized(Module))];
        {error, Reason} ->
            ?ERROR("Cover analyze failed for ~p: ~p ~p\n",
                   [Module, Reason, code:which(Module)]),
            []
    end.

is_eunitized(Mod) ->
    has_eunit_test_fun(Mod) andalso
        has_header(Mod, "include/eunit.hrl").

has_eunit_test_fun(Mod) ->
    [F || {exports, Funs} <- Mod:module_info(),
          {F, 0} <- Funs, F =:= test] =/= [].

has_header(Mod, Header) ->
    Mod1 = case code:which(Mod) of
               cover_compiled ->
                   {file, File} = cover:is_compiled(Mod),
                   File;
               non_existing -> Mod;
               preloaded -> Mod;
               L -> L
           end,
    {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(Mod1,
                                                            [abstract_code]),
    [F || {attribute, 1, file, {F, 1}} <- AC,
          string:str(F, Header) =/= 0] =/= [].

align_notcovered_count(Module, Covered, NotCovered, false) ->
    {Module, Covered, NotCovered};
align_notcovered_count(Module, Covered, NotCovered, true) ->
    {Module, Covered, NotCovered - 1}.

cover_write_index(Coverage, SrcModules) ->
    {ok, F} = file:open(filename:join([?TEST_DIR, "index.html"]), [write]),
    ok = file:write(F, "<html><head><title>Coverage Summary</title></head>\n"),
    IsSrcCoverage = fun({Mod,_C,_N}) -> lists:member(Mod, SrcModules) end,
    {SrcCoverage, TestCoverage} = lists:partition(IsSrcCoverage, Coverage),
    cover_write_index_section(F, "Source", SrcCoverage),
    cover_write_index_section(F, "Test", TestCoverage),
    ok = file:write(F, "</body></html>"),
    ok = file:close(F).

cover_write_index_section(_F, _SectionName, []) ->
    ok;
cover_write_index_section(F, SectionName, Coverage) ->
    %% Calculate total coverage %
    {Covered, NotCovered} = lists:foldl(fun({_Mod, C, N}, {CAcc, NAcc}) ->
                                                {CAcc + C, NAcc + N}
                                        end, {0, 0}, Coverage),
    TotalCoverage = percentage(Covered, NotCovered),

    %% Write the report
    ok = file:write(F, ?FMT("<body><h1>~s Summary</h1>\n", [SectionName])),
    ok = file:write(F, ?FMT("<h3>Total: ~s</h3>\n", [TotalCoverage])),
    ok = file:write(F, "<table><tr><th>Module</th><th>Coverage %</th></tr>\n"),

    FmtLink =
        fun(Module, Cov, NotCov) ->
                ?FMT("<tr><td><a href='~s.COVER.html'>~s</a></td><td>~s</td>\n",
                     [Module, Module, percentage(Cov, NotCov)])
        end,
    lists:foreach(fun({Module, Cov, NotCov}) ->
                          ok = file:write(F, FmtLink(Module, Cov, NotCov))
                  end, Coverage),
    ok = file:write(F, "</table>\n").

cover_print_coverage(Coverage) ->
    {Covered, NotCovered} = lists:foldl(fun({_Mod, C, N}, {CAcc, NAcc}) ->
                                                {CAcc + C, NAcc + N}
                                        end, {0, 0}, Coverage),
    TotalCoverage = percentage(Covered, NotCovered),

    %% Determine the longest module name for right-padding
    Width = lists:foldl(fun({Mod, _, _}, Acc) ->
                                case length(atom_to_list(Mod)) of
                                    N when N > Acc ->
                                        N;
                                    _ ->
                                        Acc
                                end
                        end, 0, Coverage) * -1,

    %% Print the output the console
    ?CONSOLE("~nCode Coverage:~n", []),
    lists:foreach(fun({Mod, C, N}) ->
                          ?CONSOLE("~*s : ~3s~n",
                                   [Width, Mod, percentage(C, N)])
                  end, Coverage),
    ?CONSOLE("~n~*s : ~s~n", [Width, "Total", TotalCoverage]).

cover_file(Module) ->
    filename:join([?TEST_DIR, atom_to_list(Module) ++ ".COVER.html"]).

percentage(0, 0) ->
    "not executed";
percentage(Cov, NotCov) ->
    integer_to_list(trunc((Cov / (Cov + NotCov)) * 100)) ++ "%".

get_app_names() ->
    [AppName || {AppName, _, _} <- application:loaded_applications()].

status_before_eunit() ->
    Apps = get_app_names(),
    AppEnvs = [{App, application:get_all_env(App)} || App <- Apps],
    {erlang:processes(), erlang:is_alive(), AppEnvs, ets:tab2list(ac_tab)}.

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
                {K, _V} <- application:get_all_env(App)],

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
    try
        _ = net_kernel:i(),
        timer:sleep(100),
        pause_until_net_kernel_stopped(N - 1)
    catch
        error:badarg ->
            ?DEBUG("Stopped net kernel.\n", []),
            ok
    end.
