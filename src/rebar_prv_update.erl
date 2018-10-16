%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_update).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, update).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 update"},
                                                               {short_desc, "Update package index."},
                                                               {desc, "Update package index."},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Names = rebar_packages:get_all_names(State),
    Resources = rebar_state:resources(State),
    #{repos := RepoConfigs} = rebar_resource_v2:find_resource_state(pkg, Resources),
    [[update_package(Name, RepoConfig, State)
      || Name <- Names]
     || RepoConfig <- RepoConfigs],
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error({package_parse_cdn, Uri}) ->
    io_lib:format("Failed to parse CDN url: ~p", [Uri]);
format_error(package_index_download) ->
    "Failed to download package index.";
format_error(package_index_write) ->
    "Failed to write package index.".


update_package(Name, RepoConfig, State) ->
    case rebar_packages:update_package(Name, RepoConfig, State) of
        fail ->
            ?WARN("Failed to fetch updates for package ~ts from repo ~ts", [Name, maps:get(name, RepoConfig)]);
        _ ->
            ok
    end.
