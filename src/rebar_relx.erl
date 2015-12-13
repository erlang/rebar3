%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_relx).

-export([do/4,
         format_error/1]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec do(atom(), string(), atom(), rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(Module, Command, Provider, State) ->
    Options = rebar_state:command_args(State),
    DepsDir = rebar_dir:deps_dir(State),
    ProjectAppDirs = lists:delete(".", ?DEFAULT_PROJECT_APP_DIRS),
    LibDirs = rebar_utils:filtermap(fun ec_file:exists/1,
                                   [rebar_dir:checkouts_dir(State), DepsDir | ProjectAppDirs]),
    OutputDir = filename:join(rebar_dir:base_dir(State), ?DEFAULT_RELEASE_DIR),
    AllOptions = string:join([Command | Options], " "),
    Cwd = rebar_state:dir(State),
    Providers = rebar_state:providers(State),
    rebar_hooks:run_all_hooks(Cwd, pre, Provider, Providers, State),
    try
        case rebar_state:get(State, relx, []) of
            [] ->
                relx:main([{lib_dirs, LibDirs}
                          ,{caller, api} | output_dir(OutputDir, Options)], AllOptions);
            Config ->
                relx:main([{lib_dirs, LibDirs}
                          ,{config, Config}
                          ,{caller, api} | output_dir(OutputDir, Options)], AllOptions)
        end,
        rebar_hooks:run_all_hooks(Cwd, post, Provider, Providers, State),
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
