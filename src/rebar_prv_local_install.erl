%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_local_install).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-export([extract_escript/2]).

-include("rebar.hrl").
-include_lib("kernel/include/file.hrl").

-define(PROVIDER, install).
-define(NAMESPACE, local).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 =
        rebar_state:add_provider(State,
                                providers:create([{name, ?PROVIDER},
                                                  {module, ?MODULE},
                                                  {bare, true},
                                                  {namespace, ?NAMESPACE},
                                                  {deps, ?DEPS},
                                                  {example, "rebar3 unstable install"},
                                                  {short_desc, "Extract libs from rebar3 escript along with a run script."},
                                                  {desc, ""},
                                                  {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case os:type() of
        {win32, _} ->
            ?ERROR("Sorry, this feature is not yet available on Windows.", []),
            {ok, State};
        _ ->
            case rebar_state:escript_path(State) of
                undefined ->
                    ?INFO("Already running from an unpacked rebar3. Nothing to do...", []),
                    {ok, State};
                ScriptPath ->
                    extract_escript(State, ScriptPath)
            end
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

bin_contents(OutputDir) ->
    <<"#!/usr/bin/env sh

erl -pz ", (rebar_utils:to_binary(OutputDir))/binary,"/*/ebin +sbtu +A0 -noshell -boot start_clean -s rebar3 main $REBAR3_ERL_ARGS -extra \"$@\"
">>.

extract_escript(State, ScriptPath) ->
    {ok, Escript} = escript:extract(ScriptPath, []),
    {archive, Archive} = lists:keyfind(archive, 1, Escript),

    %% Extract contents of Archive to ~/.cache/rebar3/lib
    %% And add a rebar3 bin script to ~/.cache/rebar3/bin
    Opts = rebar_state:opts(State),
    OutputDir = filename:join(rebar_dir:global_cache_dir(Opts), "lib"),
    filelib:ensure_dir(filename:join(OutputDir, "empty")),

    ?INFO("Extracting rebar3 libs to ~ts...", [OutputDir]),
    zip:extract(Archive, [{cwd, OutputDir}]),

    BinDir = filename:join(rebar_dir:global_cache_dir(Opts), "bin"),
    BinFile = filename:join(BinDir, "rebar3"),
    filelib:ensure_dir(BinFile),

    {ok, #file_info{mode = _,
                    uid = Uid,
                    gid = Gid}} = file:read_file_info(ScriptPath),

    ?INFO("Writing rebar3 run script ~ts...", [BinFile]),
    file:write_file(BinFile, bin_contents(OutputDir)),
    ok = file:write_file_info(BinFile, #file_info{mode=33277,
                                                  uid=Uid,
                                                  gid=Gid}),

    ?INFO("Add to $PATH for use: export PATH=~ts:$PATH", [BinDir]),

    {ok, State}.
