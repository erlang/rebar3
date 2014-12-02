%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_tar).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, tar).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, "rebar tar"},
                                                               {short_desc, "Tar archive of release built of project."},
                                                               {desc, ""},
                                                               {opts, relx:opt_spec_list()}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    OutputDir = filename:join(rebar_dir:profile_dir(State), ?DEFAULT_RELEASE_DIR),
    Options = rebar_state:command_args(State),
    DepsDir = rebar_dir:deps_dir(State),
    AllOptions = string:join(["release", "tar" | Options], " "),
    case rebar_state:get(State, relx, []) of
        [] ->
            relx:main([{lib_dirs, [DepsDir]
                       ,{output_dir, OutputDir}}], AllOptions);
        Config ->
            relx:main([{lib_dirs, [DepsDir]}
                      ,{config, Config}
                      ,{output_dir, OutputDir}], AllOptions)
    end,
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
