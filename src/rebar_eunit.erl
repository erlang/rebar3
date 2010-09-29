%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
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
%%   <li>clean - remove .eunit directory</li>
%% </ul>
%% The following Global options are supported:
%% <ul>
%%   <li>verbose=1 - show extra output from the eunit test</li>
%%   <li>suite="foo"" - runs test/foo_tests.erl</li>
%% </ul>
%% Additionally, for projects that have separate folders for the core
%% implementation, and for the unit tests, then the following <code>rebar.config</code>
%% option can be provided: <code>{eunit_compile_opts, [{src_dirs, ["dir"]}]}.</code>.
%% @copyright 2009, 2010 Dave Smith
%% -------------------------------------------------------------------
-module(rebar_eunit).

-export([eunit/2]).

-compile([export_all]).

-include("rebar.hrl").

-define(EUNIT_DIR, ".eunit").

%% ===================================================================
%% Public API
%% ===================================================================

eunit(Config, AppFile) ->
    %% Check for app global parameter; this is a comma-delimited list
    %% of apps on which we want to run eunit
    case rebar_config:get_global(app, undefined) of
        undefined ->
            %% No app parameter specified, run everything..
            ok;

        Apps ->
            TargetApps = [list_to_atom(A) || A <- string:tokens(Apps, ",")],
            ThisApp = rebar_app_utils:app_name(AppFile),
            case lists:member(ThisApp, TargetApps) of
                true ->
                    ok;
                false ->
                    ?DEBUG("Skipping eunit on app: ~p\n", [ThisApp]),
                    throw(ok)
            end
    end,

    %% Make sure ?EUNIT_DIR/ directory exists (tack on dummy module)
    ok = filelib:ensure_dir(?EUNIT_DIR ++ "/foo"),

    %% Setup code path prior to compilation so that parse_transforms and the like
    %% work properly. Also, be sure to add ebin_dir() to the END of the code path
    %% so that we don't have to jump through hoops to access the .app file
    CodePath = code:get_path(),
    true = code:add_patha(eunit_dir()),
    true = code:add_pathz(ebin_dir()),

    %% Obtain all the test modules for inclusion in the compile stage.
    %% Notice: this could also be achieved with the following rebar.config option:
    %% {eunit_compile_opts, [{src_dirs, ["test"]}]}
    TestErls = rebar_utils:find_files("test", ".*\\.erl\$"),

    %% Copy source files to eunit dir for cover in case they are not directly
    %% in src but in a subdirectory of src. Cover only looks in cwd and ../src
    %% for source files.
    SrcErls = rebar_utils:find_files("src", ".*\\.erl\$"),
    ok = rebar_file_utils:cp_r(SrcErls ++ TestErls, ?EUNIT_DIR),

    %% Compile erlang code to ?EUNIT_DIR, using a tweaked config
    %% with appropriate defines for eunit, and include all the test modules
    %% as well.
    rebar_erlc_compiler:doterl_compile(eunit_config(Config), ?EUNIT_DIR, TestErls),

    %% Build a list of all the .beams in ?EUNIT_DIR -- use this for cover
    %% and eunit testing. Normally you can just tell cover and/or eunit to
    %% scan the directory for you, but eunit does a code:purge in conjunction
    %% with that scan and causes any cover compilation info to be lost.
    %% Filter out "*_tests" modules so eunit won't doubly run them and
    %% so cover only calculates coverage on production code.
    BeamFiles = [N || N <- rebar_utils:beams(?EUNIT_DIR),
                      string:str(N, "_tests.beam") =:= 0],
    Modules = [rebar_utils:beam_to_mod(?EUNIT_DIR, N) || N <- BeamFiles],
    SrcModules = [rebar_utils:erl_to_mod(M) || M <- SrcErls],
    
    cover_init(Config, BeamFiles),
    EunitResult = perform_eunit(Config, Modules),
    perform_cover(Config, Modules, SrcModules),

    case EunitResult of
        ok ->
            ok;
        _ ->
            ?ABORT("One or more eunit tests failed.~n", [])
    end,

    %% Restore code path
    code:set_path(CodePath),
    ok.

clean(_Config, _File) ->
    rebar_file_utils:rm_rf(?EUNIT_DIR).

%% ===================================================================
%% Internal functions
%% ===================================================================

eunit_dir() ->
    filename:join(rebar_utils:get_cwd(), ?EUNIT_DIR).

ebin_dir() ->
    filename:join(rebar_utils:get_cwd(), "ebin").

perform_eunit(Config, Modules) ->
    %% suite defined, so only specify the module that relates to the
    %% suite (if any)
    Suite = rebar_config:get_global(suite, undefined),
    EunitOpts = get_eunit_opts(Config),

    %% Move down into ?EUNIT_DIR while we run tests so any generated files
    %% are created there (versus in the source dir)
    Cwd = rebar_utils:get_cwd(),
    file:set_cwd(?EUNIT_DIR),

    EunitResult = perform_eunit(EunitOpts, Modules, Suite),

    %% Return to original working dir
    file:set_cwd(Cwd),

    EunitResult.

perform_eunit(EunitOpts, Modules, undefined) ->
    (catch eunit:test(Modules, EunitOpts));
perform_eunit(EunitOpts, _Modules, Suite) ->
    (catch eunit:test(list_to_atom(Suite), EunitOpts)).

get_eunit_opts(Config) ->
    %% Enable verbose in eunit if so requested..
    BaseOpts = case rebar_config:is_verbose() of
                   true ->
                       [verbose];
                   false ->
                       []
               end,

    BaseOpts ++ rebar_config:get_list(Config, eunit_opts, []).

eunit_config(Config) ->
    EqcOpts = case is_quickcheck_avail() of
                  true ->
                      [{d, 'EQC'}];
                  false ->
                      []
              end,

    ErlOpts = rebar_config:get_list(Config, erl_opts, []),
    EunitOpts = rebar_config:get_list(Config, eunit_compile_opts, []),
    Opts = [{d, 'TEST'}, debug_info] ++
        ErlOpts ++ EunitOpts ++ EqcOpts,
    rebar_config:set(Config, erl_opts, Opts).

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

perform_cover(Config, BeamFiles, SrcModules) ->
    perform_cover(rebar_config:get(Config, cover_enabled, false), 
                  Config, BeamFiles, SrcModules).

perform_cover(false, _Config, _BeamFiles, _SrcModules) ->
    ok;
perform_cover(true, Config, BeamFiles, SrcModules) ->
    cover_analyze(Config, BeamFiles, SrcModules).

cover_analyze(_Config, [], _SrcModules) ->
    ok;
cover_analyze(_Config, Modules, SrcModules) ->
    Suite = list_to_atom(rebar_config:get_global(suite, "")),
    FilteredModules = [M || M <- Modules, M =/= Suite],

    %% Generate coverage info for all the cover-compiled modules
    Coverage = [cover_analyze_mod(M) || M <- FilteredModules],

    %% Write index of coverage info
    cover_write_index(lists:sort(Coverage), SrcModules),

    %% Write coverage details for each file
    [{ok, _} = cover:analyze_to_file(M, cover_file(M), [html]) || {M, _, _} <- Coverage],

    Index = filename:join([rebar_utils:get_cwd(), ?EUNIT_DIR, "index.html"]),
    ?CONSOLE("Cover analysis: ~s\n", [Index]).

cover_init(false, _BeamFiles) ->
    ok;
cover_init(true, BeamFiles) ->
    %% Make sure any previous runs of cover don't unduly influence
    cover:reset(),

    ?INFO("Cover compiling ~s\n", [rebar_utils:get_cwd()]),

    Compiled = [{Beam, cover:compile_beam(Beam)} || Beam <- BeamFiles],
    case [Module || {_, {ok, Module}} <- Compiled] of
        [] ->
            %% No modules compiled successfully...fail
            ?ERROR("Cover failed to compile any modules; aborting.~n", []),
            ?FAIL;
        _ ->
            %% At least one module compiled successfully

            %% It's not an error for cover compilation to fail partially, but we do want
            %% to warn about them
            [?CONSOLE("Cover compilation warning for ~p: ~p", [Beam, Desc]) || {Beam, {error, Desc}} <- Compiled]
    end,
    ok;
cover_init(Config, BeamFiles) ->
    cover_init(rebar_config:get(Config, cover_enabled, false), BeamFiles).

cover_analyze_mod(Module) ->
    case cover:analyze(Module, coverage, module) of
        {ok, {Module, {Covered, NotCovered}}} ->
            %% Modules that include the eunit header get an implicit
            %% test/0 fun, which cover considers a runnable line, but
            %% eunit:test(TestRepresentation) never calls.  Decrement
            %% NotCovered in this case.
            align_notcovered_count(Module, Covered, NotCovered,
                                   is_eunitized(Module));
        {error, Reason} ->
            ?ERROR("Cover analyze failed for ~p: ~p ~p\n",
                   [Module, Reason, code:which(Module)]),
            {0,0}
    end.

is_eunitized(Mod) ->
    has_eunit_test_fun(Mod) andalso
        has_header(Mod, "include/eunit.hrl").

has_eunit_test_fun(Mod) ->
    length([F || {exports, Funs} <- Mod:module_info(),
                    {F, 0} <- Funs,
                    F == test]) =/= 0.

has_header(Mod, Header) ->
    Mod1 = case code:which(Mod) of 
               cover_compiled -> 
                   {file, File} = cover:is_compiled(Mod),
                   File;
               non_existing -> Mod;
               preloaded -> Mod;
               L -> L
           end,
    {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(Mod1, [abstract_code]),
    length([F || {attribute, 1, file, {F, 1}} <- AC,
                 string:str(F, Header) =/= 0]) =/= 0.

align_notcovered_count(Module, Covered, NotCovered, false) ->
    {Module, Covered, NotCovered};
align_notcovered_count(Module, Covered, NotCovered, true) ->
    {Module, Covered, NotCovered - 1}.

cover_write_index(Coverage, SrcModules) ->
    {ok, F} = file:open(filename:join([?EUNIT_DIR, "index.html"]), [write]),
    ok = file:write(F, "<html><head><title>Coverage Summary</title></head>\n"),
    IsSrcCoverage = fun({Mod,_C,_N}) -> lists:member(Mod, SrcModules) end, 
    {SrcCoverage, TestCoverage} = lists:partition(IsSrcCoverage, Coverage),
    cover_write_index_section(F, "Source", SrcCoverage),
    cover_write_index_section(F, "Test", TestCoverage),
    ok = file:write(F, "</body></html>"),
    file:close(F).

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

    [ok = file:write(F, ?FMT("<tr><td><a href='~s.COVER.html'>~s</a></td><td>~s</td>\n",
                             [Module, Module, percentage(Cov, NotCov)])) ||
        {Module, Cov, NotCov} <- Coverage],
    ok = file:write(F, "</table>\n").

cover_file(Module) ->
    filename:join([?EUNIT_DIR, atom_to_list(Module) ++ ".COVER.html"]).

percentage(0, 0) ->
    "not executed";
percentage(Cov, NotCov) ->
    integer_to_list(trunc((Cov / (Cov + NotCov)) * 100)) ++ "%".
