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

-include("rebar.hrl").

-define(PROVIDER, cover).
-define(DEPS, [app_discovery]).

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
    ?INFO("Performing cover analysis...", []),
    %% figure out what coverdata we have
    CoverDir = cover_dir(State),
    CoverFiles = get_all_coverdata(CoverDir),
    %% start the cover server if necessary
    {ok, CoverPid} = start_cover(),
    %% redirect cover output
    true = redirect_cover_output(State, CoverPid),
    %% analyze!
    ok = case analyze(State, CoverFiles) of
        [] -> ok;
        Analysis ->
            print_analysis(Analysis, verbose(State)),
            write_index(State, Analysis)
    end,
    {ok, State}.

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
    Analysis.

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

process([], {0, 0}) ->
    "0%";
process([], {Cov, Not}) ->
    integer_to_list(trunc((Cov / (Cov + Not)) * 100)) ++ "%";
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
    TotalLabel = format("total", MaxLength),
    TotalCov = format(calculate_total(Stats), 8),
    [io_lib:format("~ts~n~ts~n~ts~n", [Seperator, Header, Seperator]),
        lists:map(fun({Mod, Coverage}) ->
            Name = format(Mod, MaxLength),
            Cov = format(Coverage, 8),
            io_lib:format("  |  ~ts  |  ~ts  |~n", [Name, Cov])
        end, Stats),
        io_lib:format("~ts~n", [Seperator]),
        io_lib:format("  |  ~ts  |  ~ts  |~n", [TotalLabel, TotalCov]),
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

calculate_total(Stats) when length(Stats) =:= 0 ->
    "0%";
calculate_total(Stats) ->
    TotalStats = length(Stats),
    TotalCovInt = round(lists:foldl(
                        fun({_Mod, Coverage, _File}, Acc) ->
                            Acc + (list_to_integer(string:strip(Coverage, right, $%)) / TotalStats);
                        ({_Mod, Coverage}, Acc) ->
                            Acc + (list_to_integer(string:strip(Coverage, right, $%)) / TotalStats)
    end, 0, Stats)),
    integer_to_list(TotalCovInt) ++ "%".

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
    ok = file:write(F, ?FMT("<tr><td><strong>Total</strong></td><td>~ts</td>\n",
                     [calculate_total(Mods)])),
    ok = file:write(F, "</table>\n"),
    write_index_section(F, Rest).

%% fix for r15b which doesn't put the correct path in the `source` section
%%  of `module_info(compile)`
strip_coverdir([]) -> "";
strip_coverdir(File) ->
    filename:join(lists:reverse(lists:sublist(lists:reverse(filename:split(File)),
                                              2))).

cover_compile(State, apps) ->
    Apps = filter_checkouts(rebar_state:project_apps(State)),
    AppDirs = app_dirs(Apps),
    cover_compile(State, lists:filter(fun(D) -> ec_file:is_dir(D) end, AppDirs));
cover_compile(State, Dirs) ->
    rebar_utils:update_code(rebar_state:code_paths(State, all_deps), [soft_purge]),
    %% start the cover server if necessary
    {ok, CoverPid} = start_cover(),
    %% redirect cover output
    true = redirect_cover_output(State, CoverPid),
    lists:foreach(fun(Dir) ->
        ?DEBUG("cover compiling ~p", [Dir]),
        case catch(cover:compile_beam_directory(Dir)) of
            {error, eacces} ->
                ?WARN("Directory ~p not readable, modules will not be included in coverage", [Dir]);
            {error, enoent} ->
                ?WARN("Directory ~p not found", [Dir]);
            {'EXIT', {Reason, _}} ->
                ?WARN("Cover compilation for directory ~p failed: ~p", [Dir, Reason]);
            Results ->
                %% print any warnings about modules that failed to cover compile
                lists:foreach(fun print_cover_warnings/1, lists:flatten(Results))
        end
    end, Dirs),
    rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)),
    ok.

app_dirs(Apps) ->
    lists:foldl(fun app_ebin_dirs/2, [], Apps).

app_ebin_dirs(App, Acc) ->
    [rebar_app_info:ebin_dir(App)|Acc].

filter_checkouts(Apps) -> filter_checkouts(Apps, []).

filter_checkouts([], Acc) -> lists:reverse(Acc);
filter_checkouts([App|Rest], Acc) ->
    case rebar_app_info:is_checkout(App) of
        true  -> filter_checkouts(Rest, Acc);
        false -> filter_checkouts(Rest, [App|Acc])
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

print_cover_warnings({ok, _}) -> ok;
print_cover_warnings({error, Error}) ->
    ?WARN("Cover compilation failed: ~p", [Error]).

write_coverdata(State, Task) ->
    DataDir = cover_dir(State),
    ok = filelib:ensure_dir(filename:join([DataDir, "dummy.log"])),
    ExportFile = filename:join([DataDir, atom_to_list(Task) ++ ".coverdata"]),
    case cover:export(ExportFile) of
        ok ->
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

cover_dir(State) ->
    filename:join([rebar_dir:base_dir(State), "cover"]).

cover_opts(_State) ->
    [{reset, $r, "reset", boolean, help(reset)},
     {verbose, $v, "verbose", boolean, help(verbose)}].

help(reset) -> "Reset all coverdata.";
help(verbose) -> "Print coverage analysis.".
