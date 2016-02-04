-module(rebar_prv_app_builder).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-export([build_app_file/2,
         build_app_file/3]).

-include_lib("providers/include/providers.hrl").
-include("rebar.hrl").

-define(PROVIDER, compile).
-define(DEPS, [erlc_compile]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 compile"},
                                                               {short_desc, "Compile apps .app.src and .erl files."},
                                                               {desc, "Compile apps .app.src and .erl files."},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ProjectApps = rebar_state:project_apps(State),
    Deps = rebar_state:deps_to_build(State),
    Providers = rebar_state:providers(State),
    Cwd = rebar_state:dir(State),

    ?INFO("Deps: ~p", [Deps]),

    build_app_files(State, Providers, Deps),
    {ok, ProjectApps1} = rebar_digraph:compile_order(ProjectApps),

    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State),

    ProjectApps2 = build_app_files(State, Providers, ProjectApps1),
    State2 = rebar_state:project_apps(State, ProjectApps2),

    ?INFO("ProjectApps: ~p", [ProjectApps2]),

    rebar_hooks:run_all_hooks(Cwd, post, ?PROVIDER, Providers, State2),

    case rebar_state:has_all_artifacts(State2) of
        {false, File} ->
            throw(?PRV_ERROR({missing_artifact, File}));
        true ->
            true
    end,
    rebar_utils:cleanup_code_path(rebar_state:code_paths(State2, default)
                                 ++ rebar_state:code_paths(State, all_plugin_deps)),

    {ok, State2}.

-spec format_error(any()) -> iolist().
format_error({missing_artifact, File}) ->
    io_lib:format("Missing artifact ~s", [File]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

build_app_files(State, Providers, Apps) ->
    [ build_app_file(State, Providers, AppInfo) || AppInfo <- Apps ].

build_app_file(State, AppInfo) ->
    build_app_file(State, rebar_state:providers(State), AppInfo).

build_app_file(State, Providers, AppInfo) ->
    ?INFO("Packaging ~p", [rebar_app_info:name(AppInfo)]),
    AppDir = rebar_app_info:dir(AppInfo),
    AppInfo1 = rebar_hooks:run_all_hooks(AppDir, pre, ?PROVIDER, Providers, AppInfo, State),
    case rebar_otp_app:compile(State, AppInfo1) of
        {ok, AppInfo2} ->
            AppInfo3 = rebar_hooks:run_all_hooks(AppDir, post, ?PROVIDER, Providers, AppInfo2, State),
            has_all_artifacts(AppInfo3),
            AppInfo3;
        Error ->
            throw(Error)
    end.

has_all_artifacts(AppInfo) ->
    case rebar_app_info:has_all_artifacts(AppInfo) of
        {false, File} ->
            throw(?PRV_ERROR({missing_artifact, File}));
        true ->
            true
    end.
