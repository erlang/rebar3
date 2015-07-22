-module(rebar_prv_edoc).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, edoc).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 edoc"},
                                                               {short_desc, "Generate documentation using edoc."},
                                                               {desc, "Generate documentation using edoc."},
                                                               {opts, []},
                                                               {profiles, [docs]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    code:add_pathsa(rebar_state:code_paths(State, all_deps)),
    ProjectApps = rebar_state:project_apps(State),
    EDocOpts = rebar_state:get(State, edoc_opts, []),
    lists:foreach(fun(AppInfo) ->
                          AppName = ec_cnv:to_list(rebar_app_info:name(AppInfo)),
                          ?INFO("Running edoc for ~s", [AppName]),
                          AppDir = rebar_app_info:dir(AppInfo),
                          ok = edoc:application(list_to_atom(AppName), AppDir, EDocOpts)
                  end, ProjectApps),
    rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================
