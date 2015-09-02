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
                                   [?DEFAULT_CHECKOUTS_DIR, DepsDir | ProjectAppDirs]),
    OutputDir = filename:join(rebar_dir:base_dir(State), ?DEFAULT_RELEASE_DIR),
    AllOptions = string:join([Command | Options], " "),
    Cwd = rebar_state:dir(State),
    Providers = rebar_state:providers(State),
    {ok, State1} = rebar_hooks:run_all_hooks(Cwd, pre, Provider, Providers, State),
    try
        case rebar_state:get(State1, relx, []) of
            [] ->
                relx:main([{lib_dirs, LibDirs}
                          ,{caller, api} | output_dir(OutputDir, Options)], AllOptions);
            Config ->
                Config1 = update_config(Config),
                relx:main([{lib_dirs, LibDirs}
                          ,{config, Config1}
                          ,{caller, api} | output_dir(OutputDir, Options)], AllOptions)
        end,
        rebar_hooks:run_all_hooks(Cwd, post, Provider, Providers, State1)
    catch
        throw:T ->
            {error, {Module, T}}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% To handle profiles rebar3 expects the provider to use the first entry
%% in a configuration key-value list as the value of a key if dups exist.
%% This does not work with relx. Some config options must not lose their
%% order (release which has an extends option is one). So here we pull out
%% options that are special so we can reverse the rest so what we expect
%% from a rebar3 profile is what we get on the relx side.
-define(SPECIAL_KEYS, [release, vm_args, sys_config, overlay_vars, lib_dirs]).

update_config(Config) ->
    {Special, Other} =
        lists:foldl(fun(Tuple, {SpecialAcc, OtherAcc}) when is_tuple(Tuple) ->
                            case lists:member(element(1, Tuple), ?SPECIAL_KEYS) of
                                true ->
                                    {[Tuple | SpecialAcc], OtherAcc};
                                false ->
                                    {SpecialAcc, [Tuple | OtherAcc]}
                            end
                    end, {[], []}, Config),
    lists:reverse(Special) ++ Other.

%% Don't override output_dir if the user passed one on the command line
output_dir(OutputDir, Options) ->
    [{output_dir, OutputDir} || not(lists:member("-o", Options))
                                    andalso not(lists:member("--output-dir", Options))].
