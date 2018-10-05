-module(rebar_paths).
-include("rebar.hrl").

-type target() :: deps | plugins.
-type targets() :: [target(), ...].
-export_type([target/0, targets/0]).
-export([set_paths/2, unset_paths/2]).

-spec set_paths(targets(), rebar_state:t()) -> ok.
set_paths(UserTargets, State) ->
    Targets = normalize_targets(UserTargets),
    GroupPaths = path_groups(Targets, State),
    Paths = lists:append([P || {_, P} <- GroupPaths]),
    [code:del_path(P) || P <- Paths],
    code:add_pathsa(lists:reverse(Paths)),
    % set path breaks with escripts
    %true = code:set_path(lists:append([P || {_, P} <- GroupPaths])),
    AppGroups = app_groups(Targets, State),
    purge_and_load(AppGroups, code:all_loaded(), sets:new()),
    ok.

-spec unset_paths(targets(), rebar_state:t()) -> ok.
unset_paths(UserTargets, State) ->
    Targets = normalize_targets(UserTargets),
    GroupPaths = path_groups(Targets, State),
    Paths = lists:append([P || {_, P} <- GroupPaths]),
    [code:del_path(P) || P <- Paths],
    purge(Paths, code:all_loaded()),
    ok.


%% The paths are to be set in the reverse order; i.e. the default
%% path is always last when possible (minimize cases where a build
%% tool version clashes with an app's), and put the highest priorities
%% first.
-spec normalize_targets(targets()) -> targets().
normalize_targets(List) ->
    %% Plan for the eventuality of getting values piped in
    %% from future versions of rebar3, possibly from plugins and so on,
    %% which means we'd risk failing kind of violently. We only support
    %% deps and plugins
    TmpList = lists:foldl(
      fun(deps, [deps | _] = Acc) -> Acc;
         (plugins, [plugins | _] = Acc) -> Acc;
         (deps, Acc) -> [deps | Acc -- [deps]];
         (plugins, Acc) -> [plugins | Acc -- [plugins]];
         (_, Acc) -> Acc
      end,
      [],
      List
    ),
    lists:reverse(TmpList).

purge_and_load([], _, _) ->
    ok;
purge_and_load([{_Group, Apps}|Rest], ModPaths, Seen) ->
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
    %% 4. create a list of modules to check from that app list
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
    %% TODO: add extra dirs (and test), and possibly the stdlib
    GoodAppPaths = [rebar_app_info:ebin_dir(App) || App <- GoodApps],
                %% ++ [code:lib_dir()],
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
                 %% the app's modules are used without it being loaded
                 case rebar_app_info:app_details(App) of
                     [] -> [];
                     Details -> proplists:get_value(modules, Details, [])
                 end
         end || App <- GoodApps,
                AppName <- [binary_to_atom(rebar_app_info:name(App), utf8)]]
    ),

    %% 5)
    Mods = misloaded_modules(CandidateMods, GoodAppPaths, ModPaths),
    [purge_mod(Mod) || Mod <- Mods],
    purge_and_load(Rest, ModPaths,
                   sets:union(Seen, sets:from_list(AppNames))).


purge(Paths, ModPaths) ->
    lists:map(fun purge_mod/1, lists:usort(
        [Mod || {Mod, Path} <- ModPaths,
                is_list(Path), % not 'preloaded' or mocked
                any_prefix(Path, Paths)]
    )).

misloaded_modules(Mods, GoodAppPaths, ModPaths) ->
    %% Identify paths that are invalid; i.e. app paths that cover an
    %% app in the desired group, but are not in the desired group.
    lists:usort(
        [purge_mod(Mod)
         || Mod <- Mods,
            {_, Path} <- [lists:keyfind(Mod, 1, ModPaths)],
            is_list(Path), % not 'preloaded' or mocked
            not any_prefix(Path, GoodAppPaths)]
    ).

any_prefix(Path, Paths) ->
    lists:any(fun(P) -> lists:prefix(P, Path) end, Paths).

%% assume paths currently set are good; only unload a module so next call
%% uses the correctly set paths
purge_mod(Mod) ->
    case erlang:check_process_code(self(), Mod) of
        false ->
            code:purge(Mod),
            code:delete(Mod);
        _ ->
            %% cannot purge safely without killing ourselves
            code:soft_purge(Mod) andalso
            code:delete(Mod)
    end.

path_groups(Targets, State) ->
    [{Target, get_paths(Target, State)} || Target <- Targets].

app_groups(Targets, State) ->
    [{Target, get_apps(Target, State)} || Target <- Targets].

get_paths(deps, State) ->
    rebar_state:code_paths(State, all_deps);
get_paths(plugins, State) ->
    rebar_state:code_paths(State, all_plugin_deps).

get_apps(deps, State) ->
    %% The code paths for deps also include the top level apps
    %% and the extras, which we don't have here; we have to
    %% add the apps by hand
    rebar_state:all_deps(State) ++
    case rebar_state:project_apps(State) of
        undefined -> [];
        List -> List
    end;
get_apps(plugins, State) ->
    rebar_state:all_plugin_deps(State).
