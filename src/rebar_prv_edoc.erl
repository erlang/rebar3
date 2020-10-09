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
    rebar_paths:set_paths([deps, plugins], State),
    ProjectApps = rebar_state:project_apps(State),
    Providers = rebar_state:providers(State),
    EdocOpts = rebar_state:get(State, edoc_opts, []),
    ShouldAccPaths = not has_configured_paths(EdocOpts),
    Cwd = rebar_state:dir(State),
    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State),
    Res = try
        lists:foldl(fun(AppInfo, EdocOptsAcc) ->
                    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, AppInfo, State),
                    AppName = rebar_utils:to_list(rebar_app_info:name(AppInfo)),
                    ?INFO("Running edoc for ~ts", [AppName]),
                    AppDir = rebar_app_info:dir(AppInfo),
                    AppOpts = rebar_app_info:opts(AppInfo),
                    %% order of the merge is important to allow app opts overrides
                    AppEdocOpts = merge_opts(rebar_opts:get(AppOpts, edoc_opts, []), EdocOptsAcc),
                    ?DEBUG("{edoc_opts, ~p}.", [AppEdocOpts]),
                    AppRes = (catch edoc:application(list_to_atom(AppName), AppDir, AppEdocOpts)),
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
    rebar_paths:set_paths([plugins, deps], State),
    case Res of
        {app_failed, App} ->
            ?PRV_ERROR({app_failed, App});
        _ ->
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error({app_failed, AppName}) ->
    io_lib:format("Failed to generate documentation for app '~ts'", [AppName]);
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

merge_opts(AppOpts, BaseOpts) ->
    merge_epp_macros(rebar_utils:tup_umerge(AppOpts, BaseOpts)).

%% @private the `{macros, ...}' definitions for epp can't be
%% containing duplicate definitions even if multiple macro lists
%% are supported, so we need to manually remove duplicates
%% and merge the many lists into a single one.
merge_epp_macros([]) ->
    [];
merge_epp_macros([{macros, M1}, {macros, M2} | Rest]) ->
    NewMacros = dedupe_macros(lists:usort(M1), lists:usort(M2)),
    merge_epp_macros( [{macros, NewMacros} | Rest]);
merge_epp_macros([H | T]) ->
    [H | merge_epp_macros(T)].

dedupe_macros([], Bs) -> Bs;
dedupe_macros(As, []) -> As;
dedupe_macros([{K, V1} | As], [{K, _} | Bs]) ->
    [{K, V1} | dedupe_macros(As, Bs)];
dedupe_macros([{KA, VA} | As], [{KB, VB} | Bs]) ->
    if KA < KB -> [{KA, VA} | dedupe_macros(As, [{KB, VB} | Bs])];
       KA > KB -> [{KB, VB} | dedupe_macros([{KA, VA} | As], Bs)]
    end.
