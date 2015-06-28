%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_relup).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, relup).
-define(DEPS, [release]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 relup"},
                                 {short_desc, "Create relup of releases."},
                                 {desc, "Create relup of releases."},
                                 {opts, relx:opt_spec_list()}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Options = rebar_state:command_args(State),
    DepsDir = rebar_dir:deps_dir(State),
    ProjectAppDirs = lists:delete(".", ?DEFAULT_PROJECT_APP_DIRS),
    LibDirs = rebar_utils:filtermap(fun ec_file:exists/1,
                                   [?DEFAULT_CHECKOUTS_DIR, DepsDir | ProjectAppDirs]),
    OutputDir = filename:join(rebar_dir:base_dir(State), ?DEFAULT_RELEASE_DIR),
    AllOptions = string:join(["relup" | Options], " "),
    Cwd = rebar_state:dir(State),
    Providers = rebar_state:providers(State),
    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State),
    try
        case rebar_state:get(State, relx, []) of
            [] ->
                relx:main([{lib_dirs, LibDirs}
                          ,{output_dir, OutputDir}
                          ,{caller, api}], AllOptions);
            Config ->
                relx:main([{lib_dirs, LibDirs}
                          ,{config, Config}
                          ,{output_dir, OutputDir}
                          ,{caller, api}], AllOptions)
        end,
        rebar_hooks:run_all_hooks(Cwd, post, ?PROVIDER, Providers, State),
        {ok, State}
    catch
        throw:T ->
            {error, {rlx_prv_release, T}}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
