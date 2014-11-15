%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_update).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, update).
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
                                                               {example, "rebar update"},
                                                               {short_desc, "Update package index."},
                                                               {desc, ""},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?INFO("Updating package index...", []),
    try
        Url = url(State),
        TmpDir = ec_file:insecure_mkdtemp(),
        TmpFile = filename:join(TmpDir, "packages"),
        Home = rebar_utils:home_dir(),
        PackagesFile = filename:join([Home, ?CONFIG_DIR, "packages"]),
        filelib:ensure_dir(PackagesFile),
        {ok, _RequestId} = httpc:request(get, {Url, []}, [], [{stream, TmpFile}
                                                             ,{sync, true}]),
        ok = ec_file:copy(TmpFile, PackagesFile)
    catch
        _:_ ->
            {error, {?MODULE, package_index_write}}
    end,

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(package_index_write) ->
    "Failed to write package index.".

url(State) ->
    ErtsVsn = erlang:system_info(version),

    Qs = [io_lib:format("~s=~s", [X, Y]) || {X, Y} <- [{"erts", ErtsVsn}]],
    Url = rebar_state:get(State, rebar_packages_url, "http://packages.rebar3.org/packages"),
    Url ++ "?" ++ string:join(Qs, "&").
