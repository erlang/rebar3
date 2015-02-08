%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_cover).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, cover).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, "rebar cover eunit --verbose, ct"},
                                                               {short_desc, "Perform coverage analysis for tasks."},
                                                               {desc, ""},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    State1 = rebar_state:set(State, coverage, true),
    {ok, CoverPid} = start_coverage(),
    CoverDir = rebar_state:get(State1, cover_dir, "coverage"),
    ok = rebar_dir:ensure_dir(filename:join([CoverDir, "dummy.beam"])),
    true = redirect_cover_output(CoverPid, CoverDir),
    Tasks = rebar_utils:args_to_tasks(rebar_state:command_args(State1)),
    {ok, State2} = do_tasks(Tasks, State1),
    ?INFO("Performing cover analysis...", []),
    _ = analyze(Tasks, State2, CoverDir),
    {ok, State2}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

do_tasks([], State) ->
    {ok, State};
do_tasks([{TaskStr, Args}|Tail], State) ->
    Task = list_to_atom(TaskStr),
    State1 = rebar_state:set(State, task, Task),
    State2 = rebar_state:command_args(State1, Args),
    case rebar_core:process_command(State2, Task) of
        {ok, State3} ->
            CoverDir = rebar_state:get(State, cover_dir, "coverage"),
            export_coverdata(TaskStr, CoverDir),
            do_tasks(Tail, State3);
        Error ->
            Error
    end.

start_coverage() ->
    case cover:start() of
        {ok, Pid}                       -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
    end.

redirect_cover_output(CoverPid, CoverDir) ->
    %% redirect cover console output to file
    {ok, F} = file:open(filename:join([CoverDir, "cover.log"]),
                        [write]),
    group_leader(F, CoverPid).

analyze(Tasks, State, TargetDir) ->
    ok = import_coverdata(Tasks, TargetDir),
    Mods = mods_to_analyze(),
    %% Generate coverage info for all the cover-compiled modules
    Coverage = lists:flatten([analyze_mod(M) || M <- Mods]),
    %% Write index of coverage info
    write_index(lists:sort(Coverage), TargetDir),
    %% Write coverage details for each file
    lists:foreach(
      fun({M, _, _}) ->
              {ok, _} = cover:analyze_to_file(M,
                                              cover_file(M, TargetDir),
                                              [html])
      end, Coverage),
    Index = filename:join([rebar_dir:get_cwd(), TargetDir, "index.html"]),
    ?CONSOLE("Cover analysis: ~s\n", [Index]),
    %% Print coverage report, if configured
    case rebar_state:get(State, cover_print_enabled, false) of
        true ->
            print_coverage(lists:sort(Coverage));
        false ->
            ok
    end.

mods_to_analyze() ->
    Mods = cover:modules(),
    %% filter out _checkouts
    lists:dropwhile(fun(M) -> is_checkapp(M) end, Mods).

is_checkapp(M) ->
    CheckoutsDir = filename:absname("_checkouts"),
    PathToSrc = get_value([compile, source], M:module_info()),
    lists:prefix(CheckoutsDir, PathToSrc).

get_value([], Value) -> Value;
get_value([Prop|Rest], PropList) ->
    get_value(Rest, proplists:get_value(Prop, PropList)).

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

write_index(Coverage, TargetDir) ->
    {ok, F} = file:open(filename:join([TargetDir, "index.html"]), [write]),
    ok = file:write(F, "<!DOCTYPE HTML><html>\n"
                    "<head><meta charset=\"utf-8\">"
                    "<title>Coverage Summary</title></head>\n"
                    "<body>\n"),
    write_index_section(F, "Modules", Coverage),
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
    ?CONSOLE("~nCode Coverage:", []),
    lists:foreach(fun({Mod, C, N}) ->
                          ?CONSOLE("~*s : ~4s",
                                   [Width, Mod, percentage(C, N)])
                  end, Coverage),
    ?CONSOLE("~n~*s : ~s", [Width, "Total", TotalCoverage]).

cover_file(Module, TargetDir) ->
    filename:join([TargetDir, atom_to_list(Module) ++ ".COVER.html"]).

export_coverdata(Task, TargetDir) ->
    ExportFile = filename:join([TargetDir, "cover." ++ Task ++ ".coverdata"]),
    case cover:export(ExportFile) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR("Coverdata export failed: ~p", [Reason])
    end.

import_coverdata([], _) -> ok;
import_coverdata([{Task, _}|Rest], TargetDir) ->
    ImportFile = filename:join([TargetDir, "cover." ++ Task ++ ".coverdata"]),
    case cover:import(ImportFile) of
        ok -> import_coverdata(Rest, TargetDir);
        {error, Reason} ->
            ?ERROR("Coverdata import failed: ~p", [Reason])
    end.

percentage(0, 0) ->
    "not executed";
percentage(Cov, NotCov) ->
    integer_to_list(trunc((Cov / (Cov + NotCov)) * 100)) ++ "%".

