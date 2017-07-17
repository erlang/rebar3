-module(rebar_prv_edoc).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

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

-spec do(rebar_state:t()) ->
    {ok, rebar_state:t()} | {error, string()} | {error, {module(), any()}}.
do(State) ->
    code:add_pathsa(rebar_state:code_paths(State, all_deps)),
    ProjectApps = rebar_state:project_apps(State),
    Providers = rebar_state:providers(State),
    EdocOpts = rebar_state:get(State, edoc_opts, []),
    ShouldAccPaths = not has_configured_paths(EdocOpts),
    Cwd = rebar_state:dir(State),
    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State),
    Res = try
        lists:foldl(fun(AppInfo, EdocOptsAcc) ->
                    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, AppInfo, State),
                    AppName = ec_cnv:to_list(rebar_app_info:name(AppInfo)),
                    ?INFO("Running edoc for ~s", [AppName]),
                    AppDir = rebar_app_info:dir(AppInfo),
                    AppRes = (catch edoc:application(list_to_atom(AppName), AppDir, EdocOptsAcc)),
                    rebar_hooks:run_all_hooks(Cwd, post, ?PROVIDER, Providers, AppInfo, State),
                    case {AppRes, ShouldAccPaths} of
                        {ok, true} ->
                            %% edoc wants / on all OSes
                            add_to_paths(EdocOptsAcc, AppDir++"/doc");
                        {ok, false} ->
                            EdocOptsAcc;
                        {{'EXIT', error}, _} ->
                            %% EDoc is not very descriptive
                            %% in terms of failures
                            throw({app_failed, AppName})
                    end
                end, EdocOpts, ProjectApps)
    catch
        {app_failed, AppName} ->
            {app_failed, AppName}
    end,
    rebar_hooks:run_all_hooks(Cwd, post, ?PROVIDER, Providers, State),
    rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)),
    case Res of
        {app_failed, App} ->
            ?PRV_ERROR({app_failed, App});
        _ ->
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error({app_failed, AppName}) ->
    io_lib:format("Failed to generate documentation for app '~s'", [AppName]);
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
    [H | add_to_paths(T, Path)].
