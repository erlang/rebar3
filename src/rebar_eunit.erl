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

eunit(Config, _File) ->
    %% Make sure ?EUNIT_DIR/ directory exists (tack on dummy module)
    ok = filelib:ensure_dir(?EUNIT_DIR ++ "/foo"),

    %% Compile all erlang from src/ into ?EUNIT_DIR
    rebar_erlc_compiler:do_compile(Config, "src/*.erl", ?EUNIT_DIR, ".erl", ".beam",
                                   fun compile_erl/2,
                                   rebar_config:get_list(Config, erl_first_files, [])),

    %% Build a list of all the .beams in ?EUNIT_DIR -- use this for cover
    %% and eunit testing. Normally you can just tell cover and/or eunit to
    %% scan the directory for you, but eunit does a code:purge in conjunction
    %% with that scan and causes any cover compilation info to be lost. So,
    %% we do it by hand. :(
    Modules = [list_to_atom(filename:basename(N, ".beam")) ||
                  N <- filelib:wildcard("*.beam", "ebin")],

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

    %% If cover support is requested, set it up
    case rebar_config:get(Config, cover_enabled, false) of
        true ->
            cover_init(Config);
        _ ->
            ok
    end,

    %% Move down into ?EUNIT_DIR while we run tests so any generated files
    %% are created there (versus in the source dir)
    Cwd = rebar_utils:get_cwd(),
    file:set_cwd(?EUNIT_DIR),

    %% Run eunit
    EunitOpts = BaseOpts ++ rebar_config:get_list(Config, eunit_opts, []),
    EunitResult = (catch eunit:test(Modules, EunitOpts)),

    %% Return to original working dir
    file:set_cwd(Cwd),

    %% Analyze cover modules
    cover_analyze(Config, cover:modules()),

    case EunitResult of
        ok ->
            ok;
        _ ->
            ?CONSOLE("One or more eunit tests failed.\n", []),
            ?FAIL
    end,


    %% Restore code path
    true = code:set_path(InitCodePath),
    ok.

clean(_Config, _File) ->
    rebar_file_utils:rm_rf(?EUNIT_DIR).
    

%% ===================================================================
%% Internal functions
%% ===================================================================

compile_erl(Source, Config) ->
    case is_quickcheck_avail() of
        true ->
            EqcOpts = [{d, 'EQC'}];
        false ->
            EqcOpts = []
    end,
    
    ErlOpts = rebar_config:get_list(Config, erl_opts, []),
    EunitOpts = rebar_config:get_list(Config, eunit_compile_opts, []),
    Opts = [{i, "include"}, {outdir, ?EUNIT_DIR}, {d, 'TEST'}, debug_info, report] ++
        ErlOpts ++ EunitOpts ++ EqcOpts,
    case compile:file(Source, Opts) of
        {ok, _} ->
            ok;
        error ->
            ?FAIL
    end.

is_quickcheck_avail() ->
    case erlang:get(is_quickcheck_avail) of
        undefined ->
            case code:lib_dir(eqc, include) of
                {error, bad_name} ->
                    IsAvail = false;
                Dir ->
                    IsAvail = filelib:is_file(filename:join(Dir, "eqc.hrl"))
            end,
            erlang:put(is_quickcheck_avail, IsAvail),
            ?DEBUG("Quickcheck availability: ~p\n", [IsAvail]),
            IsAvail;
        IsAvail ->
            IsAvail
    end.
                          
cover_init(_Config) ->
    %% Make sure any previous runs of cover don't unduly influence
    cover:reset(),

    ?INFO("Cover compiling ~s\n", [rebar_utils:get_cwd()]),
    
    case cover:compile_beam_directory(?EUNIT_DIR) of
        {error, Reason2} ->
            ?ERROR("Cover compilation failed: ~p\n", [Reason2]),
            ?FAIL;
        Modules ->
            %% It's not an error for cover compilation to fail partially, but we do want
            %% to warn about them
            [?CONSOLE("Cover compilation warning: ~p", [Desc]) || {error, Desc} <- Modules],

            %% Identify the modules that were compiled successfully
            case [ M || {ok, M} <- Modules] of
                [] ->
                    %% No modules compiled successfully...fail
                    ?ERROR("Cover failed to compile any modules; aborting.\n", []),
                    ?FAIL;
                _ ->
                    %% At least one module compiled successfully
                    ok
            end
    end.

cover_analyze(_Config, []) ->
    ok;
cover_analyze(_Config, Modules) ->    
    %% Generate coverage info for all the cover-compiled modules 
    Coverage = [cover_analyze_mod(M) || M <- Modules],

    %% Write index of coverage info
    cover_write_index(lists:sort(Coverage)),

    %% Write coverage details for each file
    [{ok, _} = cover:analyze_to_file(M, cover_file(M), [html]) || {M, _, _} <- Coverage],

    Index = filename:join([rebar_utils:get_cwd(), ?EUNIT_DIR, "index.html"]),
    ?CONSOLE("Cover analysis: ~s\n", [Index]).
    

cover_analyze_mod(Module) ->
    case cover:analyze(Module, coverage, module) of
        {ok, {Module, {Covered, NotCovered}}} ->
            {Module, Covered, NotCovered};
        {error, Reason} ->
            ?ERROR("Cover analyze failed for ~p: ~p ~p\n",
                   [Module, Reason, code:which(Module)]),
            {0,0}
    end.

cover_write_index(Coverage) ->
    %% Calculate total coverage %
    {Covered, NotCovered} = lists:foldl(fun({_Mod, C, N}, {CAcc, NAcc}) ->
                                                {CAcc + C, NAcc + N}
                                        end, {0, 0}, Coverage),
    TotalCoverage = percentage(Covered, NotCovered),

    %% Write the report
    {ok, F} = file:open(filename:join([?EUNIT_DIR, "index.html"]), [write]),
    ok = file:write(F, "<html><head><title>Coverage Summary</title></head>\n"
                    "<body><h1>Coverage Summary</h1>\n"),
    ok = file:write(F, ?FMT("<h3>Total: ~w%</h3>\n", [TotalCoverage])),
    ok = file:write(F, "<table><tr><th>Module</th><th>Coverage %</th></tr>\n"),

    [ok = file:write(F, ?FMT("<tr><td><a href='~s.COVER.html'>~s</a></td><td>~w%</td>\n",
                             [Module, Module, percentage(Cov, NotCov)])) ||
        {Module, Cov, NotCov} <- Coverage],
    ok = file:write(F, "</table></body></html>"),
    file:close(F).

cover_file(Module) ->    
    filename:join([?EUNIT_DIR, atom_to_list(Module) ++ ".COVER.html"]).
    
    
percentage(Cov, NotCov) ->
    trunc((Cov / (Cov + NotCov)) * 100).
