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
    Providers = rebar_state:providers(State),
    EdocOpts = rebar_state:get(State, edoc_opts, []),
    ShouldAccPaths = not has_configured_paths(EdocOpts),
    Cwd = rebar_state:dir(State),
    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State),
    lists:foldl(fun(AppInfo, EdocOptsAcc) ->
                    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, AppInfo, State),
                    AppName = ec_cnv:to_list(rebar_app_info:name(AppInfo)),
                    ?INFO("Running edoc for ~s", [AppName]),
                    AppDir = rebar_app_info:dir(AppInfo),
                    ok = edoc:application(list_to_atom(AppName), AppDir, EdocOptsAcc),
                    rebar_hooks:run_all_hooks(Cwd, post, ?PROVIDER, Providers, AppInfo, State),
                    case ShouldAccPaths of
                        true ->
                            %% edoc wants / on all OSes
                            add_to_paths(EdocOptsAcc, AppDir++"/doc");
                        false ->
                            EdocOptsAcc
                    end
                end, EdocOpts, ProjectApps),
    rebar_hooks:run_all_hooks(Cwd, post, ?PROVIDER, Providers, State),
    rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================
has_configured_paths(EdocOpts) ->
    proplists:get_value(dir, EdocOpts) =/= undefined.

add_to_paths([], Path) ->
    [{doc_path, [Path]}];
add_to_paths([{doc_path, Paths}|T], Path) ->
    [{doc_path, [Path | Paths]} | T];
add_to_paths([H|T], Path) ->
    [H | add_to_paths(Path, T)].
