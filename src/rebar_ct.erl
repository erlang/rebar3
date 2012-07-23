%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
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
%%
%% Targets:
%% test - runs common test suites in ./test
%% int_test - runs suites in ./int_test
%% perf_test - runs suites inm ./perf_test
%%
%% Global options:
%% verbose=1 - show output from the common_test run as it goes
%% suites="foo,bar" - runs <test>/foo_SUITE and <test>/bar_SUITE
%% case="mycase" - runs individual test case foo_SUITE:mycase
%% -------------------------------------------------------------------
-module(rebar_ct).

-export([ct/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

ct(Config, File) ->
    TestDir = rebar_config:get_local(Config, ct_dir, "test"),
    run_test_if_present(TestDir, Config, File).

%% ===================================================================
%% Internal functions
%% ===================================================================
run_test_if_present(TestDir, Config, File) ->
    case filelib:is_dir(TestDir) of
        false ->
            ?WARN("~s directory not present - skipping\n", [TestDir]),
            ok;
        true ->
            run_test(TestDir, Config, File)
    end.

run_test(TestDir, Config, _File) ->
    {Cmd, RawLog} = make_cmd(TestDir, Config),
    clear_log(RawLog),
    case rebar_config:is_verbose(Config) of
        false ->
            Output = " >> " ++ RawLog ++ " 2>&1";
        true ->
            Output = " 2>&1 | tee -a " ++ RawLog
    end,

    rebar_utils:sh(Cmd ++ Output, [{env,[{"TESTDIR", TestDir}]}]),
    check_log(Config, RawLog).


clear_log(RawLog) ->
    case filelib:ensure_dir("logs/index.html") of
        ok ->
            NowStr = rebar_utils:now_str(),
            LogHeader = "--- Test run on " ++ NowStr ++ " ---\n",
            ok = file:write_file(RawLog, LogHeader);
        {error, Reason} ->
            ?ERROR("Could not create log dir - ~p\n", [Reason]),
            ?ABORT
    end.

%% calling ct with erl does not return non-zero on failure - have to check
%% log results
check_log(Config, RawLog) ->
    {ok, Msg} =
        rebar_utils:sh("grep -e 'TEST COMPLETE' -e '{error,make_failed}' "
                       ++ RawLog, [{use_stdout, false}]),
    MakeFailed = string:str(Msg, "{error,make_failed}") =/= 0,
    RunFailed = string:str(Msg, ", 0 failed") =:= 0,
    if
        MakeFailed ->
            show_log(Config, RawLog),
            ?ERROR("Building tests failed\n",[]),
            ?ABORT;

        RunFailed ->
            show_log(Config, RawLog),
            ?ERROR("One or more tests failed\n",[]),
            ?ABORT;

        true ->
            ?CONSOLE("DONE.\n~s\n", [Msg])
    end.

%% Show the log if it hasn't already been shown because verbose was on
show_log(Config, RawLog) ->
    ?CONSOLE("Showing log\n", []),
    case rebar_config:is_verbose(Config) of
        false ->
            {ok, Contents} = file:read_file(RawLog),
            ?CONSOLE("~s", [Contents]);
        true ->
            ok
    end.

make_cmd(TestDir, Config) ->
    Cwd = rebar_utils:get_cwd(),
    LogDir = filename:join(Cwd, "logs"),
    EbinDir = filename:absname(filename:join(Cwd, "ebin")),
    IncludeDir = filename:join(Cwd, "include"),
    Include = case filelib:is_dir(IncludeDir) of
                  true ->
                      " -include \"" ++ IncludeDir ++ "\"";
                  false ->
                      ""
              end,

    %% Add the code path of the rebar process to the code path. This
    %% includes the dependencies in the code path. The directories
    %% that are part of the root Erlang install are filtered out to
    %% avoid duplication
    R = code:root_dir(),
    NonLibCodeDirs = [P || P <- code:get_path(), not lists:prefix(R, P)],
    CodeDirs = [io_lib:format("\"~s\"", [Dir]) ||
                   Dir <- [EbinDir|NonLibCodeDirs]],
    CodePathString = string:join(CodeDirs, " "),
    Cmd = case get_ct_specs(Cwd) of
              undefined ->
                  ?FMT("erl " % should we expand ERL_PATH?
                       " -noshell -pa ~s ~s"
                       " ~s"
                       " -logdir \"~s\""
                       " -env TEST_DIR \"~s\""
                       " ~s"
                       " -s ct_run script_start -s erlang halt",
                       [CodePathString,
                        Include,
                        build_name(Config),
                        LogDir,
                        filename:join(Cwd, TestDir),
                        get_extra_params(Config)]) ++
                      get_cover_config(Config, Cwd) ++
                      get_ct_config_file(TestDir) ++
                      get_config_file(TestDir) ++
                      get_suites(Config, TestDir) ++
                      get_case(Config);
              SpecFlags ->
                  ?FMT("erl " % should we expand ERL_PATH?
                       " -noshell -pa ~s ~s"
                       " ~s"
                       " -logdir \"~s\""
                       " -env TEST_DIR \"~s\""
                       " ~s"
                       " -s ct_run script_start -s erlang halt",
                       [CodePathString,
                        Include,
                        build_name(Config),
                        LogDir,
                        filename:join(Cwd, TestDir),
                        get_extra_params(Config)]) ++
                      SpecFlags ++ get_cover_config(Config, Cwd)
          end,
    RawLog = filename:join(LogDir, "raw.log"),
    {Cmd, RawLog}.

build_name(Config) ->
    case rebar_config:get_local(Config, ct_use_short_names, false) of
        true -> "-sname test";
        false -> " -name test@" ++ net_adm:localhost()
    end.

get_extra_params(Config) ->
    rebar_config:get_local(Config, ct_extra_params, "").

get_ct_specs(Cwd) ->
    case collect_glob(Cwd, ".*\.test\.spec\$") of
        [] -> undefined;
        [Spec] ->
            " -spec " ++ Spec;
        Specs ->
            " -spec " ++
                lists:flatten([io_lib:format("~s ", [Spec]) || Spec <- Specs])
    end.

get_cover_config(Config, Cwd) ->
    case rebar_config:get_local(Config, cover_enabled, false) of
        false ->
            "";
        true ->
            case collect_glob(Cwd, ".*cover\.spec\$") of
                [] ->
                    ?DEBUG("No cover spec found: ~s~n", [Cwd]),
                    "";
                [Spec] ->
                    ?DEBUG("Found cover file ~w~n", [Spec]),
                    " -cover " ++ Spec;
                Specs ->
                    ?ABORT("Multiple cover specs found: ~p~n", [Specs])
            end
    end.

collect_glob(Cwd, Glob) ->
    filelib:fold_files(Cwd, Glob, true, fun collect_files/2, []).

collect_files(F, Acc) ->
    %% Ignore any specs under the deps/ directory. Do this pulling
    %% the dirname off the the F and then splitting it into a list.
    Parts = filename:split(filename:dirname(F)),
    case lists:member("deps", Parts) of
        true ->
            Acc;                % There is a directory named "deps" in path
        false ->
            [F | Acc]           % No "deps" directory in path
    end.

get_ct_config_file(TestDir) ->
    Config = filename:join(TestDir, "test.config"),
    case filelib:is_regular(Config) of
        false ->
            " ";
        true ->
            " -ct_config " ++ Config
    end.

get_config_file(TestDir) ->
    Config = filename:join(TestDir, "app.config"),
    case filelib:is_regular(Config) of
        false ->
            " ";
        true ->
            " -config " ++ Config
    end.

get_suites(Config, TestDir) ->
    case rebar_config:get_global(Config, suites, undefined) of
        undefined ->
            " -dir " ++ TestDir;
        Suites ->
            Suites1 = string:tokens(Suites, ","),
            Suites2 = [find_suite_path(Suite, TestDir) || Suite <- Suites1],
            string:join([" -suite"] ++ Suites2, " ")
    end.

find_suite_path(Suite, TestDir) ->
    Path = filename:join(TestDir, Suite ++ "_SUITE.erl"),
    case filelib:is_regular(Path) of
        false ->
            ?ERROR("Suite ~s not found\n", [Suite]),
            ?ABORT;
        true ->
            Path
    end.

get_case(Config) ->
    case rebar_config:get_global(Config, 'case', undefined) of
        undefined ->
            "";
        Case ->
            " -case " ++ Case
    end.
