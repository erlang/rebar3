%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_cover).

-behaviour(provider).

-export([init/1,
         do/1,
         maybe_cover_compile/1,
         maybe_cover_compile/2,
         maybe_write_coverdata/2,
         format_error/1]).

-include_lib("providers/include/providers.hrl").
-include("rebar.hrl").

-define(PROVIDER, cover).
-define(DEPS, [lock]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 cover"},
                                                               {short_desc, "Perform coverage analysis."},
                                                               {desc, "Perform coverage analysis."},
                                                               {opts, cover_opts(State)},
                                                               {profiles, [test]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(reset, Opts, false) of
        true  -> reset(State);
        false -> analyze(State)
    end.

-spec maybe_cover_compile(rebar_state:t()) -> ok.
maybe_cover_compile(State) ->
    maybe_cover_compile(State, apps).

-spec maybe_cover_compile(rebar_state:t(), [file:name()] | apps) -> ok.
maybe_cover_compile(State, Dirs) ->
    case rebar_state:get(State, cover_enabled, false) of
        true  -> cover_compile(State, Dirs);
        false -> ok
    end.

-spec maybe_write_coverdata(rebar_state:t(), atom()) -> ok.
maybe_write_coverdata(State, Task) ->
    case cover:modules() of
        %% no coverdata collected, skip writing anything out
        [] -> ok;
        _  -> write_coverdata(State, Task)
    end.

-spec format_error(any()) -> iolist().
format_error({min_coverage_failed, {PassRate, Total}}) ->
    io_lib:format("Requiring ~p% coverage to pass. Only ~p% obtained",
                  [PassRate, Total]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

reset(State) ->
    ?INFO("Resetting collected cover data...", []),
    CoverDir = cover_dir(State),
    CoverFiles = get_all_coverdata(CoverDir),
    F = fun(File) ->
        case file:delete(File) of
            {error, Reason} ->
                ?WARN("Error deleting ~p: ~p", [Reason, File]);
            _ -> ok
        end
    end,
    ok = lists:foreach(F, CoverFiles),
    {ok, State}.

analyze(State) ->
    %% modules have to be compiled and then cover compiled
    %% in order for cover data to be reloaded
    %% this maybe breaks if modules have been deleted
    %% since code coverage was collected?
    {ok, S} = rebar_prv_compile:do(State),
    ok = cover_compile(S, apps),
    do_analyze(State).

do_analyze(State) ->
    ?INFO("Performing cover analysis...", []),
    %% figure out what coverdata we have
    CoverDir = cover_dir(State),
    CoverFiles = get_all_coverdata(CoverDir),
    %% start the cover server if necessary
    {ok, CoverPid} = start_cover(),
    %% redirect cover output
    true = redirect_cover_output(State, CoverPid),
    %% analyze!
    case analyze(State, CoverFiles) of
        [] -> {ok, State};
        Analysis ->
            print_analysis(Analysis, verbose(State)),
            write_index(State, Analysis),
            maybe_fail_coverage(Analysis, State)
    end.

get_all_coverdata(CoverDir) ->
    ok = filelib:ensure_dir(filename:join([CoverDir, "dummy.log"])),
    {ok, Files} = rebar_utils:list_dir(CoverDir),
    rebar_utils:filtermap(fun(FileName) ->
        case filename:extension(FileName) == ".coverdata" of
            true  -> {true, filename:join([CoverDir, FileName])};
            false -> false
        end
    end, Files).

analyze(_State, []) ->
    ?WARN("No coverdata found", []),
    [];
analyze(State, CoverFiles) ->
    %% reset any existing cover data
    ok = cover:reset(),
    %% import all coverdata files
    ok = lists:foreach(fun(M) -> import(M) end, CoverFiles),
    [{"aggregate", CoverFiles, analysis(State, "aggregate")}] ++
        analyze(State, CoverFiles, []).

analyze(_State, [], Acc) -> lists:reverse(Acc);
analyze(State, [F|Rest], Acc) ->
    %% reset any existing cover data
    ok = cover:reset(),
    %% extract taskname from the CoverData file
    Task = filename:basename(F, ".coverdata"),
    %% import task cover data and process it
    ok = import(F),
    analyze(State, Rest, [{Task, [F], analysis(State, Task)}] ++ Acc).

import(CoverData) ->
    case cover:import(CoverData) of
        {error, {cant_open_file, F, _Reason}} ->
            ?WARN("Can't import cover data from ~ts.", [F]),
            error;
        ok -> ok
    end.

analysis(State, Task) ->
    OldPath = code:get_path(),
    ok = restore_cover_paths(State),
    Mods = cover:imported_modules(),
    Analysis = lists:map(fun(Mod) ->
                  {ok, Answer} = cover:analyze(Mod, coverage, line),
                  {ok, File} = analyze_to_file(Mod, State, Task),
                  {Mod, process(Answer), File}
              end,
              Mods),
    true = rebar_utils:cleanup_code_path(OldPath),
    lists:sort(Analysis).

restore_cover_paths(State) ->
    lists:foreach(fun(App) ->
        AppDir = rebar_app_info:out_dir(App),
        _ = code:add_path(filename:join([AppDir, "ebin"])),
        _ = code:add_path(filename:join([AppDir, "test"]))
    end, rebar_state:project_apps(State)),
    _ = code:add_path(filename:join([rebar_dir:base_dir(State), "test"])),
    ok.

analyze_to_file(Mod, State, Task) ->
    CoverDir = cover_dir(State),
    TaskDir = filename:join([CoverDir, Task]),
    ok = filelib:ensure_dir(filename:join([TaskDir, "dummy.html"])),
    case code:ensure_loaded(Mod) of
        {module, _} ->
            write_file(Mod, mod_to_filename(TaskDir, Mod));
        {error, _}  ->
            ?WARN("Can't load module ~ts.", [Mod]),
            {ok, []}
    end.

write_file(Mod, FileName) ->
    case cover:analyze_to_file(Mod, FileName, [html]) of
        {ok, File} -> {ok, File};
        {error, Reason} ->
            ?WARN("Couldn't write annotated file for module ~p for reason ~p", [Mod, Reason]),
            {ok, []}
    end.

mod_to_filename(TaskDir, M) ->
    filename:join([TaskDir, atom_to_list(M) ++ ".html"]).

process(Coverage) -> process(Coverage, {0, 0}).

process([], Acc) -> Acc;
%% line 0 is a line added by eunit and never executed so ignore it
process([{{_, 0}, _}|Rest], Acc) -> process(Rest, Acc);
process([{_, {Cov, Not}}|Rest], {Covered, NotCovered}) ->
    process(Rest, {Covered + Cov, NotCovered + Not}).

print_analysis(_, false) -> ok;
print_analysis(Analysis, true) ->
    {_, CoverFiles, Stats} = lists:keyfind("aggregate", 1, Analysis),
    Table = format_table(Stats, CoverFiles),
    io:format("~ts", [Table]).

format_table(Stats, CoverFiles) ->
    MaxLength = lists:max([20 | lists:map(fun({M, _, _}) -> mod_length(M) end, Stats)]),
    Header = header(MaxLength),
    Separator = separator(MaxLength),
    TotalLabel = format("total", MaxLength),
    TotalCov = format(calculate_total_string(Stats), 8),
    [io_lib:format("~ts~n~ts~n~ts~n", [Separator, Header, Separator]),
        lists:map(fun({Mod, Coverage, _}) ->
            Name = format(Mod, MaxLength),
            Cov = format(percentage_string(Coverage), 8),
            io_lib:format("  |  ~ts  |  ~ts  |~n", [Name, Cov])
        end, Stats),
        io_lib:format("~ts~n", [Separator]),
        io_lib:format("  |  ~ts  |  ~ts  |~n", [TotalLabel, TotalCov]),
        io_lib:format("~ts~n", [Separator]),
        io_lib:format("  coverage calculated from:~n", []),
        lists:map(fun(File) ->
            io_lib:format("    ~ts~n", [File])
        end, CoverFiles)].

mod_length(Mod) when is_atom(Mod) -> mod_length(atom_to_list(Mod));
mod_length(Mod) -> length(Mod).

header(Width) ->
    ["  |  ", format("module", Width), "  |  ", format("coverage", 8), "  |"].

separator(Width) ->
    ["  |--", io_lib:format("~*c", [Width, $-]), "--|------------|"].

format(String, Width) -> io_lib:format("~*.ts", [Width, String]).

calculate_total_string(Stats) ->
    integer_to_list(calculate_total(Stats))++"%".

calculate_total(Stats) ->
    percentage(lists:foldl(
        fun({_Mod, {Cov, Not}, _File}, {CovAcc, NotAcc}) ->
            {CovAcc + Cov, NotAcc + Not}
        end,
        {0, 0},
        Stats
    )).

percentage_string(Data) -> integer_to_list(percentage(Data))++"%".

percentage({_, 0}) -> 100;
percentage({Cov, Not}) -> trunc((Cov / (Cov + Not)) * 100).

write_index(State, Coverage) ->
    CoverDir = cover_dir(State),
    FileName = filename:join([CoverDir, "index.html"]),
    {ok, F} = file:open(FileName, [write]),
    ok = file:write(F, "<!DOCTYPE HTML><html>\n"
                    "<head><meta charset=\"utf-8\">"
                    "<title>Coverage Summary</title></head>\n"
                    "<body>\n"),
    {Aggregate, Rest} = lists:partition(fun({"aggregate", _, _}) -> true; (_) -> false end,
                                        Coverage),
    ok = write_index_section(F, Aggregate),
    ok = write_index_section(F, Rest),
    ok = file:write(F, "</body></html>"),
    ok = file:close(F),
    io:format("  cover summary written to: ~ts~n", [filename:absname(FileName)]).

write_index_section(_F, []) -> ok;
write_index_section(F, [{Section, DataFile, Mods}|Rest]) ->
    %% Write the report
    ok = file:write(F, ?FMT("<h1>~ts summary</h1>\n", [Section])),
    ok = file:write(F, "coverage calculated from:\n<ul>"),
    ok = lists:foreach(fun(D) -> ok = file:write(F, io_lib:format("<li>~ts</li>", [D])) end,
                       DataFile),
    ok = file:write(F, "</ul>\n"),
    ok = file:write(F, "<table><tr><th>module</th><th>coverage %</th></tr>\n"),
    FmtLink =
        fun({Mod, Cov, Report}) ->
                ?FMT("<tr><td><a href='~ts'>~ts</a></td><td>~ts</td>\n",
                     [strip_coverdir(Report), Mod, percentage_string(Cov)])
        end,
    lists:foreach(fun(M) -> ok = file:write(F, FmtLink(M)) end, Mods),
    ok = file:write(F, ?FMT("<tr><td><strong>Total</strong></td><td>~ts</td>\n",
                     [calculate_total_string(Mods)])),
    ok = file:write(F, "</table>\n"),
    write_index_section(F, Rest).

maybe_fail_coverage(Analysis, State) ->
    {_, _CoverFiles, Stats} = lists:keyfind("aggregate", 1, Analysis),
    Total = calculate_total(Stats),
    PassRate = min_coverage(State),
    ?DEBUG("Comparing ~p to pass rate ~p", [Total, PassRate]),
    if Total >= PassRate ->
        {ok, State}
    ;  Total < PassRate ->
        ?PRV_ERROR({min_coverage_failed, {PassRate, Total}})
    end.

%% fix for r15b which doesn't put the correct path in the `source` section
%%  of `module_info(compile)`
strip_coverdir([]) -> "";
strip_coverdir(File) ->
    filename:join(lists:reverse(lists:sublist(lists:reverse(filename:split(File)),
                                              2))).

cover_compile(State, apps) ->
    ExclApps = [rebar_utils:to_binary(A) || A <- rebar_state:get(State, cover_excl_apps, [])],
    Apps = filter_checkouts_and_excluded(rebar_state:project_apps(State), ExclApps),
    AppDirs = app_dirs(Apps),
    cover_compile(State, lists:filter(fun(D) -> ec_file:is_dir(D) end, AppDirs));
cover_compile(State, Dirs) ->
    rebar_paths:set_paths([deps], State),
    %% start the cover server if necessary
    {ok, CoverPid} = start_cover(),
    %% redirect cover output
    true = redirect_cover_output(State, CoverPid),
    ExclMods = rebar_state:get(State, cover_excl_mods, []),
    ?DEBUG("Ignoring cover compilation of modules in {cover_excl_mods, ~p}", [ExclMods]),
    lists:foreach(fun(Dir) ->
        case file:list_dir(Dir) of
            {ok, Files} ->
                ?DEBUG("cover compiling ~p", [Dir]),
                [cover_compile_file(filename:join(Dir, File))
                 || File <- Files,
                    filename:extension(File) == ".beam",
                    not is_ignored(Dir, File, ExclMods)],
                ok;
            {error, eacces} ->
                ?WARN("Directory ~p not readable, modules will not be included in coverage", [Dir]);
            {error, enoent} ->
                ?WARN("Directory ~p not found", [Dir]);
            {error, Reason} ->
                ?WARN("Directory ~p error ~p", [Dir, Reason])
        end
    end, Dirs),
    ok.

is_ignored(Dir, File, ExclMods) ->
    Ignored = lists:any(fun(Excl) ->
                             File =:= atom_to_list(Excl) ++ ".beam"
                        end,
                        ExclMods),
    Ignored andalso ?DEBUG("cover ignoring ~p ~p", [Dir, File]),
    Ignored.

cover_compile_file(FileName) ->
    case catch(cover:compile_beam(FileName)) of
        {error, Reason} ->
            ?WARN("Cover compilation failed: ~p", [Reason]);
        {ok, _} ->
            ok
    end.

app_dirs(Apps) ->
    lists:foldl(fun app_ebin_dirs/2, [], Apps).

app_ebin_dirs(App, Acc) ->
    [rebar_app_info:ebin_dir(App)|Acc].

filter_checkouts_and_excluded(Apps, ExclApps) ->
    filter_checkouts_and_excluded(Apps, ExclApps, []).

filter_checkouts_and_excluded([], _ExclApps, Acc) -> lists:reverse(Acc);
filter_checkouts_and_excluded([App|Rest], ExclApps, Acc) ->
    case rebar_app_info:is_checkout(App) orelse lists:member(rebar_app_info:name(App), ExclApps) of
        true  -> filter_checkouts_and_excluded(Rest, ExclApps, Acc);
        false -> filter_checkouts_and_excluded(Rest, ExclApps, [App|Acc])
    end.

start_cover() ->
    case cover:start() of
        {ok, Pid}                       -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid}
    end.

redirect_cover_output(State, CoverPid) ->
    %% redirect cover console output to file
    DataDir = cover_dir(State),
    ok = filelib:ensure_dir(filename:join([DataDir, "dummy.log"])),
    {ok, F} = file:open(filename:join([DataDir, "cover.log"]),
                        [append]),
    group_leader(F, CoverPid).

write_coverdata(State, Name) ->
    DataDir = cover_dir(State),
    ok = filelib:ensure_dir(filename:join([DataDir, "dummy.log"])),
    ExportFile = filename:join([DataDir, rebar_utils:to_list(Name) ++ ".coverdata"]),
    case cover:export(ExportFile) of
        ok ->
            %% dump accumulated coverdata after writing
            ok = cover:reset(),
            ?DEBUG("Cover data written to ~p.", [ExportFile]);
        {error, Reason} ->
            ?WARN("Cover data export failed: ~p", [Reason])
    end.

command_line_opts(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    Opts.

config_opts(State) ->
    rebar_state:get(State, cover_opts, []).

verbose(State) ->
    Command = proplists:get_value(verbose, command_line_opts(State), undefined),
    Config = proplists:get_value(verbose, config_opts(State), undefined),
    case {Command, Config} of
        {undefined, undefined} -> false;
        {undefined, Verbose}   -> Verbose;
        {Verbose, _}           -> Verbose
    end.

min_coverage(State) ->
    Command = proplists:get_value(min_coverage, command_line_opts(State), undefined),
    Config = proplists:get_value(min_coverage, config_opts(State), undefined),
    case {Command, Config} of
        {undefined, undefined} -> 0;
        {undefined, Rate}   -> Rate;
        {Rate, _}           -> Rate
    end.

cover_dir(State) ->
    filename:join([rebar_dir:base_dir(State), "cover"]).

cover_opts(_State) ->
    [{reset, $r, "reset", boolean, help(reset)},
     {verbose, $v, "verbose", boolean, help(verbose)},
     {min_coverage, $m, "min_coverage", integer, help(min_coverage)}].

help(reset) -> "Reset all coverdata.";
help(verbose) -> "Print coverage analysis.";
help(min_coverage) -> "Mandate a coverage percentage required to succeed (0..100)".
