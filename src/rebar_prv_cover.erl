%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_cover).

-behaviour(provider).

-export([init/1,
         do/1,
         maybe_cover_compile/2,
         maybe_write_coverdata/2,
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
                                                               {example, "rebar cover"},
                                                               {short_desc, "Perform coverage analysis."},
                                                               {desc, ""},
                                                               {opts, cover_opts(State)}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(reset, Opts, false) of
        true  -> reset(State);
        false -> analyze(State)
    end.

-spec maybe_cover_compile(rebar_state:t(), file:name_all()) -> ok.
maybe_cover_compile(State, BeamDir) ->
    case rebar_state:get(State, cover_enabled, false) of
        true  -> cover_compile(State, BeamDir);
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
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

reset(State) ->
    ?INFO("Reseting collected cover data...", []),
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
    ?INFO("Performing cover analysis...", []),
    %% figure out what coverdata we have
    CoverDir = cover_dir(State),
    CoverFiles = get_all_coverdata(CoverDir),
    %% start the cover server if necessary
    {ok, CoverPid} = start_cover(),
    %% redirect cover output
    true = redirect_cover_output(State, CoverPid),
    %% preserve old path
    Path = code:get_path(),
    %% add the `ct` and `eunit` beam file paths
    ok = add_test_paths(State),
    %% analyze!
    ok = case analyze(State, CoverFiles) of
        [] -> ok;
        Analysis ->
            print_analysis(Analysis, verbose(State)),
            write_index(State, Analysis)
    end,
    %% restore old path
    true = code:set_path(Path),
    {ok, State}.

get_all_coverdata(CoverDir) ->
    ok = filelib:ensure_dir(filename:join([CoverDir, "dummy.log"])),
    {ok, Files} = file:list_dir(CoverDir),
    rebar_utils:filtermap(fun(FileName) ->
        case filename:extension(FileName) == ".coverdata" of
            true  -> {true, filename:join([CoverDir, FileName])};
            false -> false
        end
    end, Files).

add_test_paths(State) ->
    Tmp = rebar_file_utils:system_tmpdir(),
    Root = filename:join([rebar_state:dir(State), Tmp]),
    Project = filename:basename(rebar_state:dir(State)),
    lists:foreach(fun(TestDir) ->
        Path = filename:join([Root, Project ++ "_rebar3_" ++ TestDir]),
        code:add_patha(Path)
    end, ["ct", "eunit"]).

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
    Mods = cover:imported_modules(),
    lists:map(
        fun(Mod) ->
            {ok, Answer} = cover:analyze(Mod, coverage, line),
            {ok, File} = analyze_to_file(Mod, State, Task),
            {Mod, process(Answer), File}
        end,
        Mods).

analyze_to_file(Mod, State, Task) ->
    CoverDir = cover_dir(State),
    TaskDir = filename:join([CoverDir, Task]),
    ok = filelib:ensure_dir(filename:join([TaskDir, "dummy.html"])),
    case code:ensure_loaded(Mod) of
        {module, _} ->
            cover:analyze_to_file(Mod, mod_to_filename(TaskDir, Mod), [html]);
        {error, _}  ->
            ?WARN("Can't load module ~ts.", [Mod]),
            {ok, []}
    end.

mod_to_filename(TaskDir, M) ->
    filename:join([TaskDir, atom_to_list(M) ++ ".html"]).

process(Coverage) -> process(Coverage, {0, 0}).

process([], {0, 0}) ->
    "0.0%";
process([], {Cov, Not}) ->
    float_to_list((Cov / (Cov + Not)) * 100, [{decimals, 1}]) ++ "%";
%% line 0 is a line added by eunit and never executed so ignore it
process([{{_, 0}, _}|Rest], Acc) -> process(Rest, Acc);
process([{_, {Cov, Not}}|Rest], {Covered, NotCovered}) ->
    process(Rest, {Covered + Cov, NotCovered + Not}).

print_analysis(_, false) -> ok;
print_analysis(Analysis, true) ->
    {_, CoverFiles, Stats} = lists:keyfind("aggregate", 1, Analysis),
    ConsoleStats = [ {atom_to_list(M), C} || {M, C, _} <- Stats ],
    Table = format_table(ConsoleStats, CoverFiles),
    io:format("~ts", [Table]).

format_table(Stats, CoverFiles) ->
    MaxLength = max(lists:foldl(fun max_length/2, 0, Stats), 20),
    Header = header(MaxLength),
    Seperator = seperator(MaxLength),
    [io_lib:format("~ts~n~ts~n~ts~n", [Seperator, Header, Seperator]),
        lists:map(fun({Mod, Coverage}) ->
            Name = format(Mod, MaxLength),
            Cov = format(Coverage, 8),
            io_lib:format("  |  ~ts  |  ~ts  |~n", [Name, Cov])
        end, Stats),
        io_lib:format("~ts~n", [Seperator]),
        io_lib:format("  coverage calculated from:~n", []),
        lists:map(fun(File) ->
            io_lib:format("    ~ts~n", [File])
        end, CoverFiles)].

max_length({ModName, _}, Min) ->
    Length = length(lists:flatten(ModName)),
    case Length > Min of
        true  -> Length;
        false -> Min
    end.

header(Width) ->
    ["  |  ", format("module", Width), "  |  ", format("coverage", 8), "  |"].

seperator(Width) ->
    ["  |--", io_lib:format("~*c", [Width, $-]), "--|------------|"].

format(String, Width) -> io_lib:format("~*.ts", [Width, String]).

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
    io:format("  cover summary written to: ~ts~n", [FileName]).

write_index_section(_F, []) -> ok;
write_index_section(F, [{Section, DataFile, Mods}|Rest]) ->
    %% Write the report
    ok = file:write(F, ?FMT("<h1>~s summary</h1>\n", [Section])),
    ok = file:write(F, "coverage calculated from:\n<ul>"),
    ok = lists:foreach(fun(D) -> ok = file:write(F, io_lib:format("<li>~ts</li>", [D])) end,
                       DataFile),
    ok = file:write(F, "</ul>\n"),
    ok = file:write(F, "<table><tr><th>module</th><th>coverage %</th></tr>\n"),
    FmtLink =
        fun({Mod, Cov, Report}) ->
                ?FMT("<tr><td><a href='~ts'>~ts</a></td><td>~ts</td>\n",
                     [strip_coverdir(Report), Mod, Cov])
        end,
    lists:foreach(fun(M) -> ok = file:write(F, FmtLink(M)) end, Mods),
    ok = file:write(F, "</table>\n"),
    write_index_section(F, Rest).

strip_coverdir(File) ->
    filename:join(lists:reverse(lists:sublist(lists:reverse(filename:split(File)),
                                              2))).

cover_compile(State, BeamDir) ->
    %% start the cover server if necessary
    {ok, CoverPid} = start_cover(),
    %% redirect cover output
    true = redirect_cover_output(State, CoverPid),
    %% cover compile the modules we just compiled
    CompileResult = cover:compile_beam_directory(BeamDir),
    %% print any warnings about modules that failed to cover compile
    lists:foreach(fun print_cover_warnings/1, CompileResult).

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

print_cover_warnings({ok, _}) -> ok;
print_cover_warnings({error, File}) ->
    ?WARN("Cover compilation of ~p failed, module is not included in cover data.",
          [File]).

write_coverdata(State, Task) ->
    DataDir = cover_dir(State),
    ok = filelib:ensure_dir(filename:join([DataDir, "dummy.log"])),
    ExportFile = filename:join([DataDir, atom_to_list(Task) ++ ".coverdata"]),
    case cover:export(ExportFile) of
        ok ->
            ?DEBUG("Cover data written to ~p.", [ExportFile]),
            ok;
        {error, Reason} ->
            ?WARN("Cover data export failed: ~p", [Reason])
    end.

verbose(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(verbose, Opts, missing) of
        missing -> rebar_state:get(State, cover_print_enabled, false);
        Else -> Else
    end.

cover_dir(State) ->
    rebar_state:get(State, cover_data_dir, "cover").

cover_opts(_State) ->
    [{reset, $r, "reset", boolean, help(reset)},
     {verbose, $v, "verbose", boolean, help(verbose)}].

help(reset) -> "Reset all coverdata.";
help(verbose) -> "Print coverage analysis.".