%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
%% Copyright (c) 2013 Andras Horvath (andras.horvath@erlang-solutions.com)
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
-module(rebar_cover_utils).

%% for internal use only
-export([init/3,
         perform_cover/4,
         close/1]).

-include("rebar.hrl").

%% ====================================================================
%% Internal functions
%% ====================================================================

perform_cover(Config, BeamFiles, SrcModules, TargetDir) ->
    perform_cover(rebar_config:get(Config, cover_enabled, false),
                  Config, BeamFiles, SrcModules, TargetDir).

perform_cover(false, _Config, _BeamFiles, _SrcModules, _TargetDir) ->
    ok;
perform_cover(true, Config, BeamFiles, SrcModules, TargetDir) ->
    analyze(Config, BeamFiles, SrcModules, TargetDir).

close(not_enabled) ->
    ok;
close(F) ->
    ok = file:close(F).

init(false, _BeamFiles, _TargetDir) ->
    {ok, not_enabled};
init(true, BeamFiles, TargetDir) ->
    %% Attempt to start the cover server, then set its group leader to
    %% TargetDir/cover.log, so all cover log messages will go there instead of
    %% to stdout. If the cover server is already started, we'll kill that
    %% server and start a new one in order not to inherit a polluted
    %% cover_server state.
    {ok, CoverPid} = case whereis(cover_server) of
                         undefined ->
                             cover:start();
                         _         ->
                             cover:stop(),
                             cover:start()
                     end,

    {ok, F} = OkOpen = file:open(
                         filename:join([TargetDir, "cover.log"]),
                         [write]),

    group_leader(F, CoverPid),

    ?INFO("Cover compiling ~s\n", [rebar_utils:get_cwd()]),

    Compiled = [{Beam, cover:compile_beam(Beam)} || Beam <- BeamFiles],
    case [Module || {_, {ok, Module}} <- Compiled] of
        [] ->
            %% No modules compiled successfully...fail
            ?ERROR("Cover failed to compile any modules; aborting.~n", []),
            ?FAIL;
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
init(Config, BeamFiles, TargetDir) ->
    init(rebar_config:get(Config, cover_enabled, false), BeamFiles, TargetDir).

analyze(_Config, [], _SrcModules, _TargetDir) ->
    ok;
analyze(Config, FilteredModules, SrcModules, TargetDir) ->
    %% Generate coverage info for all the cover-compiled modules
    Coverage = lists:flatten([analyze_mod(M)
                              || M <- FilteredModules,
                                 cover:is_compiled(M) =/= false]),

    %% Write index of coverage info
    write_index(lists:sort(Coverage), SrcModules, TargetDir),

    %% Write coverage details for each file
    lists:foreach(
      fun({M, _, _}) ->
              {ok, _} = cover:analyze_to_file(M,
                                              cover_file(M, TargetDir),
                                              [html])
      end, Coverage),

    Index = filename:join([rebar_utils:get_cwd(), TargetDir, "index.html"]),
    ?CONSOLE("Cover analysis: ~s\n", [Index]),

    %% Export coverage data, if configured
    case rebar_config:get(Config, cover_export_enabled, false) of
        true ->
            export_coverdata(TargetDir);
        false ->
            ok
    end,

    %% Print coverage report, if configured
    case rebar_config:get(Config, cover_print_enabled, false) of
        true ->
            print_coverage(lists:sort(Coverage));
        false ->
            ok
    end.

analyze_mod(Module) ->
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
    {ok, {_, [{abstract_code, {_, AC}}]}} =
        beam_lib:chunks(Mod1, [abstract_code]),
    [F || {attribute, 1, file, {F, 1}} <- AC,
          string:str(F, Header) =/= 0] =/= [].

align_notcovered_count(Module, Covered, NotCovered, false) ->
    {Module, Covered, NotCovered};
align_notcovered_count(Module, Covered, NotCovered, true) ->
    {Module, Covered, NotCovered - 1}.

write_index(Coverage, SrcModules, TargetDir) ->
    {ok, F} = file:open(filename:join([TargetDir, "index.html"]), [write]),
    ok = file:write(F, "<!DOCTYPE HTML><html>\n"
                    "<head><meta charset=\"utf-8\">"
                    "<title>Coverage Summary</title></head>\n"
                    "<body>\n"),
    IsSrcCoverage = fun({Mod,_C,_N}) -> lists:member(Mod, SrcModules) end,
    {SrcCoverage, TestCoverage} = lists:partition(IsSrcCoverage, Coverage),
    write_index_section(F, "Source", SrcCoverage),
    write_index_section(F, "Test", TestCoverage),
    ok = file:write(F, "</body></html>"),
    ok = file:close(F).

write_index_section(_F, _SectionName, []) ->
    ok;
write_index_section(F, SectionName, Coverage) ->
    %% Calculate total coverage
    {Covered, NotCovered} = lists:foldl(fun({_Mod, C, N}, {CAcc, NAcc}) ->
                                                {CAcc + C, NAcc + N}
                                        end, {0, 0}, Coverage),
    TotalCoverage = percentage(Covered, NotCovered),

    %% Write the report
    ok = file:write(F, ?FMT("<h1>~s Summary</h1>\n", [SectionName])),
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

print_coverage(Coverage) ->
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

cover_file(Module, TargetDir) ->
    filename:join([TargetDir, atom_to_list(Module) ++ ".COVER.html"]).

export_coverdata(TargetDir) ->
    ExportFile = filename:join(TargetDir, "cover.coverdata"),
    case cover:export(ExportFile) of
        ok ->
            ?CONSOLE("Coverdata export: ~s~n", [ExportFile]);
        {error, Reason} ->
            ?ERROR("Coverdata export failed: ~p~n", [Reason])
    end.

percentage(0, 0) ->
    "not executed";
percentage(Cov, NotCov) ->
    integer_to_list(trunc((Cov / (Cov + NotCov)) * 100)) ++ "%".
