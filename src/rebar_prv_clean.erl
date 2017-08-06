%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_clean).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, clean).
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
                                                               {example, "rebar3 clean"},
                                                               {short_desc, "Remove compiled beam files from apps."},
                                                               {desc, "Remove compiled beam files from apps."},
                                                               {opts, [{all, $a, "all", undefined, "Clean all apps include deps"},
                                                                       {profile, $p, "profile", string, "Clean under profile. Equivalent to `rebar3 as <profile> clean`"}]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Providers = rebar_state:providers(State),
    {All, Profiles} = handle_args(State),

    State1 = rebar_state:apply_profiles(State, [list_to_atom(X) || X <- Profiles]),

    Cwd = rebar_dir:get_cwd(),
    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State1),

    case All of
        true ->
            DepsDir = rebar_dir:deps_dir(State1),
            DepsDirs = filelib:wildcard(filename:join(DepsDir, "*")),
            AllApps = rebar_app_discover:find_apps(DepsDirs, all),
            clean_apps(State1, Providers, AllApps);
        false ->
            ProjectApps = rebar_state:project_apps(State1),
            clean_apps(State1, Providers, ProjectApps)
    end,

    clean_extras(State1),

    rebar_hooks:run_all_hooks(Cwd, post, ?PROVIDER, Providers, State1),

    {ok, State1}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

clean_apps(State, Providers, Apps) ->
    [begin
         ?INFO("Cleaning out ~ts...", [rebar_app_info:name(AppInfo)]),
         AppDir = rebar_app_info:dir(AppInfo),
         AppInfo1 = rebar_hooks:run_all_hooks(AppDir, pre, ?PROVIDER, Providers, AppInfo, State),
         rebar_erlc_compiler:clean(AppInfo1),
         rebar_hooks:run_all_hooks(AppDir, post, ?PROVIDER, Providers, AppInfo1, State)
     end || AppInfo <- Apps].

clean_extras(State) ->
    BaseDir = rebar_dir:base_dir(State),
    rebar_file_utils:rm_rf(filename:join([BaseDir, "extras"])).

handle_args(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    All = proplists:get_value(all, Args, false),
    Profiles = proplists:get_all_values(profile, Args),
    {All, Profiles}.
