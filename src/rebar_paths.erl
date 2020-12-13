-module(rebar_paths).
-include("rebar.hrl").

-type target() :: deps | plugins | runtime.
-type targets() :: [target(), ...].
-export_type([target/0, targets/0]).
-export([set_paths/2, unset_paths/2]).
-export([clashing_apps/2]).

-ifdef(TEST).
-export([misloaded_modules/2]).
-endif.

-spec set_paths(targets(), rebar_state:t()) -> ok.
set_paths(UserTargets, State) ->
    Targets = normalize_targets(UserTargets),
    ?DIAGNOSTIC("Setting paths to ~p", [Targets]),
    GroupPaths = path_groups(Targets, State),
    Paths = lists:append(lists:reverse([P || {_, P} <- GroupPaths])),
    code:add_pathsa(Paths),
    AppGroups = app_groups(Targets, State),
    purge_and_load(AppGroups, sets:new()),
    ok.

-spec unset_paths(targets(), rebar_state:t()) -> ok.
unset_paths(UserTargets, State) ->
    Targets = normalize_targets(UserTargets),
    ?DIAGNOSTIC("Removing ~p paths", [Targets]),
    GroupPaths = path_groups(Targets, State),
    Paths = lists:append([P || {_, P} <- GroupPaths]),
    [code:del_path(P) || P <- Paths],
    purge(Paths, code:all_loaded()),
    ok.

-spec clashing_apps(targets(), rebar_state:t()) -> [{target(), [binary()]}].
clashing_apps(Targets, State) ->
    AppGroups = app_groups(Targets, State),
    AppNames = [{G, sets:from_list(
                    [rebar_app_info:name(App) || App <- Apps]
                )} || {G, Apps} <- AppGroups],
    clashing_app_names(sets:new(), AppNames, []).

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%% The paths are to be set in the reverse order; i.e. the default
%% path is always last when possible (minimize cases where a build
%% tool version clashes with an app's), and put the highest priorities
%% first.
-spec normalize_targets(targets()) -> targets().
normalize_targets(List) ->
    %% Plan for the eventuality of getting values piped in
    %% from future versions of rebar3, possibly from plugins and so on,
    %% which means we'd risk failing kind of violently. We only support
    %% deps, plugins and runtime deps.
    TmpList = lists:foldl(
      fun(deps, [deps | _] = Acc) -> Acc;
         (plugins, [plugins | _] = Acc) -> Acc;
         (runtime, [runtime | _] = Acc) -> Acc;
         (deps, Acc) -> [deps | Acc -- [deps]];
         (plugins, Acc) -> [plugins | Acc -- [plugins]];
         (runtime, Acc) -> [runtime | Acc -- [runtime]];
         (_, Acc) -> Acc
      end,
      [],
      List
    ),
    lists:reverse(TmpList).

purge_and_load([], _) ->
    ok;
purge_and_load([{_Group, Apps}|Rest], Seen) ->
    %% We have: a list of all applications in the current priority group,
    %% a list of all loaded modules with their active path, and a list of
    %% seen applications.
    %%
    %% We do the following:
    %% 1. identify the apps that have not been solved yet
    %% 2. find the paths for all apps in the current group
    %% 3. unload and reload apps that may have changed paths in order
    %%    to get updated module lists and specs
    %%    (we ignore started apps and apps that have not run for this)
    %%    This part turns out to be the bottleneck of this module, so
    %%    to speed it up, using clash detection proves useful:
    %%    only reload apps that clashed since others are unlikely to
    %%    conflict in significant ways
    %% 4. create a list of modules to check from that app list—only loaded
    %%    modules make sense to check.
    %% 5. check the modules to match their currently loaded paths with
    %%    the path set from the apps in the current group; modules
    %%    that differ must be purged; others can stay

    %% 1)
    AppNames = [AppName || App <- Apps,
                           AppName <- [rebar_app_info:name(App)],
                           not sets:is_element(AppName, Seen)],
    GoodApps = [App || AppName <- AppNames,
                       App <- Apps,
                       rebar_app_info:name(App) =:= AppName],
    %% 2)
    %% (no need for extra_src_dirs since those get put into ebin;
    %%  also no need for OTP libs; we want to allow overtaking them)
    GoodAppPaths = [rebar_app_info:ebin_dir(App) || App <- GoodApps],
    %% 3)
    [begin
         AtomApp = binary_to_atom(AppName, utf8),
         %% blind load/unload won't interrupt an already-running app,
         %% preventing odd errors, maybe!
         case application:unload(AtomApp) of
             ok -> application:load(AtomApp);
             _ -> ok
         end
     end || AppName <- AppNames,
            %% Shouldn't unload ourselves; rebar runs without ever
            %% being started and unloading breaks logging!
            AppName =/= <<"rebar">>],
    %% 4)
    CandidateMods = lists:append(
        %% Start by asking the currently loaded app (if loaded)
        %% since it would be the primary source of conflicting modules
        [case application:get_key(AppName, modules) of
             {ok, Mods} ->
                 Mods;
             undefined ->
                 %% if not found, parse the app file on disk, in case
                 %% the app's modules are used without it being loaded;
                 %% invalidate the cache in case we're proceeding during
                 %% compilation steps by setting the app details to `[]', which
                 %% is its empty value; the details will then be reloaded
                 %% from disk when found
                 case rebar_app_info:app_details(rebar_app_info:app_details(App, [])) of
                     [] -> [];
                     Details -> proplists:get_value(modules, Details, [])
                 end
         end || App <- GoodApps,
                AppName <- [binary_to_atom(rebar_app_info:name(App), utf8)]]
    ),
    ModPaths = [{Mod,Path} || Mod <- CandidateMods,
                              erlang:function_exported(Mod, module_info, 0),
                              {file, Path} <- [code:is_loaded(Mod)]],

    %% 5)
    Mods = misloaded_modules(GoodAppPaths, ModPaths),
    [purge_mod(Mod) || Mod <- Mods],

    purge_and_load(Rest, sets:union(Seen, sets:from_list(AppNames))).

purge(Paths, ModPaths) ->
    SortedPaths = lists:sort(Paths),
    lists:map(fun purge_mod/1,
              [Mod || {Mod, Path} <- ModPaths,
                      is_list(Path), % not 'preloaded' or mocked
                      any_prefix(Path, SortedPaths)]
             ).

misloaded_modules(GoodAppPaths, ModPaths) ->
    %% Identify paths that are invalid; i.e. app paths that cover an
    %% app in the desired group, but are not in the desired group.
    lists:usort(
        [Mod || {Mod, Path} <- ModPaths,
                is_list(Path), % not 'preloaded' or mocked
                not any_prefix(Path, GoodAppPaths)]
    ).

any_prefix(Path, Paths) ->
    lists:any(fun(P) -> lists:prefix(P, Path) end, Paths).

%% assume paths currently set are good; only unload a module so next call
%% uses the correctly set paths
purge_mod(Mod) ->
    code:soft_purge(Mod) andalso code:delete(Mod).


%% This is a tricky O(n²) check since we want to
%% know whether an app clashes with any of the top priority groups.
%%
%% For example, let's say we have `[deps, plugins]', then we want
%% to find the plugins that clash with deps:
%%
%% `[{deps, [ClashingPlugins]}, {plugins, []}]'
%%
%% In case we'd ever have alternative or additional types, we can
%% find all clashes from other 'groups'.
clashing_app_names(_, [], Acc) ->
    lists:reverse(Acc);
clashing_app_names(PrevNames, [{G,AppNames} | Rest], Acc) ->
    CurrentNames = sets:subtract(AppNames, PrevNames),
    NextNames = sets:subtract(sets:union([A || {_, A} <- Rest]), PrevNames),
    Clashes = sets:intersection(CurrentNames, NextNames),
    NewAcc = [{G, sets:to_list(Clashes)} | Acc],
    clashing_app_names(sets:union(PrevNames, CurrentNames), Rest, NewAcc).

path_groups(Targets, State) ->
    [{Target, get_paths(Target, State)} || Target <- Targets].

app_groups(Targets, State) ->
    [{Target, get_apps(Target, State)} || Target <- Targets].

get_paths(deps, State) ->
    rebar_state:code_paths(State, all_deps);
get_paths(plugins, State) ->
    rebar_state:code_paths(State, all_plugin_deps);
get_paths(runtime, State) ->
    RuntimeApps = get_apps(runtime, State),
    [rebar_app_info:ebin_dir(App) || App <- RuntimeApps].

get_apps(deps, State) ->
    %% The code paths for deps also include the top level apps
    %% and the extras, which we don't have here; we have to
    %% add the apps by hand
    rebar_state:project_apps(State) ++ rebar_state:all_deps(State);
get_apps(plugins, State) ->
    rebar_state:all_plugin_deps(State);
get_apps(runtime, State) ->
    %% We get all project apps and for each of them we find
    %% their runtime deps (i.e., `applications' and `included_applications').
    ProjectApps = rebar_state:project_apps(State),
    AppsList = rebar_state:project_apps(State) ++ rebar_state:all_deps(State),
    get_runtime_apps(ProjectApps, sets:new(), AppsList).

get_runtime_apps([], RuntimeApps, _AppsList) -> sets:to_list(RuntimeApps);
%% We skip those apps that are not AppInfos.
get_runtime_apps([App|Rest], AppsAcc0, AppsList) when is_atom(App) orelse is_binary(App) ->
    get_runtime_apps(Rest, AppsAcc0, AppsList);
get_runtime_apps([App|Rest0], AppsAcc0, AppsList) ->
    Apps = rebar_app_info:applications(App),
    IncludedApps = rebar_app_info:included_applications(App),
    TotalApps0 = [atom_to_binary(A, utf8) || A <- (Apps ++ IncludedApps)],
    TotalApps = TotalApps0 -- [rebar_app_info:name(A) || A <- sets:to_list(AppsAcc0)],

    {Rest1, AppsAcc1} =
        lists:foldl(
            fun(AppName, {Rest, Acc}) ->
                %% We only care about those apps we ccould find in the state.
                case rebar_app_utils:find(AppName, AppsList) of
                    {ok, AppInfo} -> {[AppInfo|Rest], sets:add_element(AppInfo, Acc)};
                    error -> {Rest, Acc}
                end
            end, {Rest0, sets:add_element(App, AppsAcc0)}, TotalApps),
    get_runtime_apps(Rest1 ++ TotalApps, AppsAcc1, AppsList).
