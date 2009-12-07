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
%% eunit - runs eunit tests
%% clean - remove .eunit directory
%%
%% Global options:
%% verbose=1 - show extra output from the eunit test
%% suite="foo"" - runs test/foo_tests.erl
%% -------------------------------------------------------------------
-module(rebar_eunit).

-export([eunit/2]).

-compile([export_all]).

-include("rebar.hrl").

-define(EUNIT_DIR, ".eunit").

%% ===================================================================
%% Public API
%% ===================================================================

eunit(Config, File) ->
    %% Make sure ?EUNIT_DIR/ directory exists (tack on dummy module)
    ok = filelib:ensure_dir(?EUNIT_DIR ++ "/foo"),

    %% Compile all erlang from src/ into ?EUNIT_DIR
    rebar_erlc_compiler:do_compile(Config, "src/*.erl", ?EUNIT_DIR, ".erl", ".beam", fun compile_erl/2,
                                   rebar_config:get_list(Config, erl_first_files, [])),

    %% TODO: If there are other wildcards specified in eunit_sources, compile them

    %% Save current code path and then prefix ?EUNIT_DIR on it so that our modules
    %% are found there
    InitCodePath = code:get_path(),
    true = code:add_patha(?EUNIT_DIR),

    %% Enable verbose in eunit if so requested..
    case rebar_config:is_verbose() of
        true ->
            BaseOpts = [verbose];
        false ->
            BaseOpts = []
    end,

    %% Run eunit
    EunitOpts = BaseOpts ++ rebar_config:get_list(Config, eunit_opts, []),
    case catch(eunit:test({dir, ?EUNIT_DIR}, EunitOpts)) of
        ok ->
            ok;
        _ ->
            ?CONSOLE("One or more eunit tests failed.\n", []),
            ?FAIL
    end,

    %% Restore code path
    true = code:set_path(InitCodePath),
    ok.

clean(Config, File) ->
    rebar_file_utils:rm_rf(?EUNIT_DIR).
    

%% ===================================================================
%% Internal functions
%% ===================================================================

compile_erl(Source, Config) ->
    ErlOpts = rebar_config:get_list(Config, erl_opts, []),
    EunitOpts = rebar_config:get_list(Config, eunit_compile_opts, []),
    Opts = [{i, "include"}, {outdir, ?EUNIT_DIR}, {d, 'TEST'}, debug_info, report] ++ ErlOpts ++ EunitOpts,
    case compile:file(Source, Opts) of
        {ok, _} ->
            ok;
        error ->
            ?FAIL
    end.


                          

