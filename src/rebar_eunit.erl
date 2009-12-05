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
%% eunit - runs eunit tests (with suffix _tests.erl) in ./test
%%
%% Global options:
%% verbose=1 - show extra output from the eunit test
%% suite="foo"" - runs test/foo_tests.erl
%% -------------------------------------------------------------------
-module(rebar_eunit).

-export([eunit/2]).

-compile([export_all]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

eunit(Config, File) ->
    run_test_if_present("test", Config, File).

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
    case rebar_erlc_compiler:do_compile(Config, "test/*.erl", "test", ".erl", ".beam",
                                        fun compile_test/2, []) of
        ok ->
            Cwd = rebar_utils:get_cwd(),
            CodePath = code:get_path(),
            true = code:add_patha(filename:join(Cwd, "test")),
            
            case rebar_config:get_global(verbose, "0") of
                "0" ->
            Verbose = [];
                _ ->
                    Verbose = [verbose]
            end,
            
            Tests = find_tests(TestDir),
            Opts = Verbose,
            ?INFO("Running tests: ~p\n", [Tests]),

            change_to_work_dir(Cwd),

            case catch eunit:test(Tests, Opts) of
                ok ->
                    ok;
                _ ->
                    ?CONSOLE("One or more tests failed\n", []),
                    ?FAIL
            end,
            
            code:set_path(CodePath),
            file:set_cwd(Cwd),
            ok;

        _Other ->
            ?ERROR("Compiling eunit tests failed\n",[]),
            ?FAIL
    end.


compile_test(Source, Config) ->
    Opts = [{i, "include"}, {outdir, "test"}, debug_info, report] ++ 
        rebar_erlc_compiler:compile_opts(Config, erl_opts),
    case compile:file(Source, Opts) of
        {ok, _} ->
            ok;
        error ->
            ?FAIL
    end.


find_tests(TestDir) ->
    case rebar_config:get_global(suite, undefined) of
        undefined ->
            {ok, Files} = file:list_dir(TestDir),
            Filter = fun(Filename) ->
                             lists:suffix("_tests.erl", Filename)
                     end,
            Erls = lists:filter(Filter, Files),
            [list_to_atom(filename:basename(F, ".erl")) || F <- Erls];
        Suite ->
            %% Add the _tests suffix if missing
            case lists:suffix("_tests", Suite) of
                true ->
                    [list_to_atom(Suite)];
                false ->
                    [list_to_atom(Suite ++ "_tests")]
            end
    end.

%% Clear the contents of the work dir and change to it
%%                
change_to_work_dir(MainDir) ->                  
    WorkDir = filename:join([MainDir, "logs", "eunit_work"]),
    rebar_file_utils:rm_rf(WorkDir),
    ok = filelib:ensure_dir(filename:join(WorkDir, "x")),
    ok = file:set_cwd(WorkDir).


    

                          

