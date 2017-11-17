%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_relx).

-export([do/4,
         format_error/1]).

-ifdef(TEST).
-export([merge_overlays/1]).
-endif.

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec do(atom(), string(), atom(), rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(Module, Command, Provider, State) ->
    %% We set the color mode for relx as a application env
    application:set_env(relx, color_intensity, rebar_log:intensity()),
    LogLevel = rebar_log:get_level(),
    Options = rebar_state:command_args(State),
    DepsDir = rebar_dir:deps_dir(State),
    ProjectAppDirs = lists:delete(".", ?DEFAULT_PROJECT_APP_DIRS),
    LibDirs = rebar_utils:filtermap(fun ec_file:exists/1,
                                   [rebar_dir:checkouts_dir(State), DepsDir | ProjectAppDirs]),
    OutputDir = filename:join(rebar_dir:base_dir(State), ?DEFAULT_RELEASE_DIR),
    AllOptions = rebar_string:join([Command | Options], " "),
    Cwd = rebar_state:dir(State),
    Providers = rebar_state:providers(State),
    RebarOpts = rebar_state:opts(State),
    ErlOpts = rebar_opts:erl_opts(RebarOpts),
    rebar_hooks:run_project_and_app_hooks(Cwd, pre, Provider, Providers, State),
    try
        case rebar_state:get(State, relx, []) of
            [] ->
                relx:main([{lib_dirs, LibDirs}
                          ,{caller, api}
                          ,{log_level, LogLevel} | output_dir(OutputDir, Options)] ++ ErlOpts, AllOptions);
            Config ->
                Config1 = merge_overlays(Config),
                relx:main([{lib_dirs, LibDirs}
                          ,{config, Config1}
                          ,{caller, api}
                          ,{log_level, LogLevel} | output_dir(OutputDir, Options)] ++ ErlOpts, AllOptions)
        end,
        rebar_hooks:run_project_and_app_hooks(Cwd, post, Provider, Providers, State),
        {ok, State}
    catch
        throw:T ->
            {error, {Module, T}}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Don't override output_dir if the user passed one on the command line
output_dir(OutputDir, Options) ->
    [{output_dir, OutputDir} || not(lists:member("-o", Options))
                                    andalso not(lists:member("--output-dir", Options))].

merge_overlays(Config) ->
    {Overlays, Others} =
        lists:partition(fun(C) when element(1, C) =:= overlay -> true;
                           (_) -> false
                        end, Config),
    %% Have profile overlay entries come before others to match how profiles work elsewhere
    NewOverlay = lists:flatmap(fun({overlay, Overlay}) -> Overlay end, lists:reverse(Overlays)),
    [{overlay, NewOverlay} | Others].
