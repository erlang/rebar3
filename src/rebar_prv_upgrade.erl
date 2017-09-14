%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_upgrade).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, upgrade).
-define(DEPS, [lock]).
%% Also only upgrade top-level (0) deps. Transitive deps shouldn't be
%% upgradable -- if the user wants this, they should declare it at the
%% top level and then upgrade.

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 =
        rebar_state:add_provider(State,
                                 providers:create([{name, ?PROVIDER},
                                                   {module, ?MODULE},
                                                   {bare, true},
                                                   {deps, ?DEPS},
                                                   {example, "rebar3 upgrade [cowboy[,ranch]]"},
                                                   {short_desc, "Upgrade dependencies."},
                                                   {desc, "Upgrade project dependencies. Mentioning no application "
                                                          "will upgrade all dependencies. To upgrade specific dependencies, "
                                                          "their names can be listed in the command."},
                                                   {opts, [
                                                          {package, undefined, undefined, string,
                                                           "List of packages to upgrade. If not specified, all dependencies are upgraded."}
                                                          ]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Locks = rebar_state:get(State, {locks, default}, []),
    %% We have 3 sources of dependencies to upgrade from:
    %% 1. the top-level rebar.config (in `deps', dep name is an atom)
    %% 2. the app-level rebar.config in umbrella apps (in `{deps, default}',
    %%    where the dep name is an atom)
    %% 3. the formatted sources for all after app-parsing (in `{deps, default}',
    %%    where the reprocessed app name is a binary)
    %%
    %% The gotcha with these is that the selection of dependencies with a
    %% binary name (those that are stable and usable internally) is done with
    %% in the profile deps only, but while accounting for locks.
    %% Because our job here is to unlock those that have changed, we must
    %% instead use the atom-based names, both in `deps' and `{deps, default}',
    %% as those are the dependencies that may have changed but have been
    %% disregarded by locks.
    %%
    %% As such, the working set of dependencies is the addition of
    %% `deps' and `{deps, default}' entries with an atom name, as those
    %% disregard locks and parsed values post-selection altogether.
    %% Packages without versions can of course be a single atom.
    TopDeps = rebar_state:get(State, deps, []),
    ProfileDeps = rebar_state:get(State, {deps, default}, []),
    Deps = [Dep || Dep <- TopDeps ++ ProfileDeps, % TopDeps > ProfileDeps
                   is_atom(Dep) orelse is_atom(element(1, Dep))],
    Names = parse_names(rebar_utils:to_binary(proplists:get_value(package, Args, <<"">>)), Locks),
    DepsDict = deps_dict(rebar_state:all_deps(State)),
    AltDeps = find_non_default_deps(Deps, State),
    FilteredNames = cull_default_names_if_profiles(Names, Deps, State),
    case prepare_locks(FilteredNames, Deps, Locks, [], DepsDict, AltDeps) of
        {error, Reason} ->
            {error, Reason};
        {Locks0, _Unlocks0} ->
            Deps0 = top_level_deps(Deps, Locks),
            State1 = rebar_state:set(State, {deps, default}, Deps0),
            DepsDir = rebar_prv_install_deps:profile_dep_dir(State, default),
            D = rebar_app_utils:parse_deps(root, DepsDir, Deps0, State1, Locks0, 0),
            State2 = rebar_state:set(State1, {parsed_deps, default}, D),
            State3 = rebar_state:set(State2, {locks, default}, Locks0),
            State4 = rebar_state:set(State3, upgrade, true),
            UpdatedLocks = [L || L <- rebar_state:lock(State4),
                                 lists:keymember(rebar_app_info:name(L), 1, Locks0)],
            Res = rebar_prv_install_deps:do_(rebar_state:lock(State4, UpdatedLocks)),
            case Res of
                {ok, State5} ->
                    rebar_utils:info_useless(
                      [element(1,Lock) || Lock <- Locks],
                      [rebar_app_info:name(App) || App <- rebar_state:lock(State5)]
                     ),
                    rebar_prv_lock:do(State5);
                _ ->
                    Res
            end
    end.

-spec format_error(any()) -> iolist().
format_error({unknown_dependency, Name}) ->
    io_lib:format("Dependency ~ts not found", [Name]);
format_error({transitive_dependency, Name}) ->
    io_lib:format("Dependency ~ts is transitive and cannot be safely upgraded. "
                 "Promote it to your top-level rebar.config file to upgrade it.",
                 [Name]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

parse_names(Bin, Locks) ->
    case lists:usort(re:split(Bin, <<" *, *">>, [trim, unicode])) of
        %% Nothing submitted, use *all* apps
        [<<"">>] -> [Name || {Name, _, 0} <- Locks];
        [] -> [Name || {Name, _, 0} <- Locks];
        %% Regular options
        Other -> Other
    end.

%% Find alternative deps in non-default profiles since they may
%% need to be passed through (they are never locked)
find_non_default_deps(Deps, State) ->
    AltProfiles = rebar_state:current_profiles(State) -- [default],
    AltProfileDeps = lists:append([
        rebar_state:get(State, {deps, Profile}, []) || Profile <- AltProfiles]
    ),
    [Dep || Dep <- AltProfileDeps,
            is_atom(Dep) orelse is_atom(element(1, Dep))
            andalso not lists:member(Dep, Deps)].

%% If any alt profiles are used, remove the default profiles from
%% the upgrade list and warn about it.
cull_default_names_if_profiles(Names, Deps, State) ->
    case rebar_state:current_profiles(State) of
        [default] ->
            Names;
        _ ->
            ?INFO("Dependencies in the default profile will not be upgraded", []),
            lists:filter(fun(Name) ->
                AtomName = binary_to_atom(Name, utf8),
                rebar_utils:tup_find(AtomName, Deps) == false
            end, Names)
    end.

prepare_locks([], _, Locks, Unlocks, _Dict, _AltDeps) ->
    {Locks, Unlocks};
prepare_locks([Name|Names], Deps, Locks, Unlocks, Dict, AltDeps) ->
    AtomName = binary_to_atom(Name, utf8),
    case lists:keyfind(Name, 1, Locks) of
        {_, _, 0} = Lock ->
            case rebar_utils:tup_find(AtomName, Deps) of
                false ->
                    ?WARN("Dependency ~ts has been removed and will not be upgraded", [Name]),
                    prepare_locks(Names, Deps, Locks, Unlocks, Dict, AltDeps);
                Dep ->
                    {Source, NewLocks, NewUnlocks} = prepare_lock(Dep, Lock, Locks, Dict),
                    prepare_locks(Names, Deps, NewLocks,
                                  [{Name, Source, 0} | NewUnlocks ++ Unlocks], Dict, AltDeps)
            end;
        {_, _, Level} = Lock when Level > 0 ->
            case rebar_utils:tup_find(AtomName, Deps) of
                false ->
                    ?PRV_ERROR({transitive_dependency, Name});
                Dep -> % Dep has been promoted
                    {Source, NewLocks, NewUnlocks} = prepare_lock(Dep, Lock, Locks, Dict),
                    prepare_locks(Names, Deps, NewLocks,
                                  [{Name, Source, 0} | NewUnlocks ++ Unlocks], Dict, AltDeps)
            end;
        false ->
            case rebar_utils:tup_find(AtomName, AltDeps) of
                false ->
                    ?PRV_ERROR({unknown_dependency, Name});
                _ -> % non-default profile dependency found, pass through
                    prepare_locks(Names, Deps, Locks, Unlocks, Dict, AltDeps)
            end
    end.

prepare_lock(Dep, Lock, Locks, Dict) ->
    {Name1, Source} = case Dep of
        {Name, SrcOrVsn} -> {Name, SrcOrVsn};
        {Name, _, Src} -> {Name, Src};
        _ when is_atom(Dep) ->
            %% version-free package. Must unlock whatever matches in locks
            {_, Vsn, _} = lists:keyfind(rebar_utils:to_binary(Dep), 1, Locks),
            {Dep, Vsn}
    end,
    Children = all_children(Name1, Dict),
    {NewLocks, NewUnlocks} = unlock_children(Children, Locks -- [Lock]),
    {Source, NewLocks, NewUnlocks}.

top_level_deps(Deps, Locks) ->
    [Dep || Dep <- Deps, lists:keymember(0, 3, Locks)].

unlock_children(Children, Locks) ->
    unlock_children(Children, Locks, [], []).

unlock_children(_, [], Locks, Unlocks) ->
    {Locks, Unlocks};
unlock_children(Children, [App = {Name,_,_} | Apps], Locks, Unlocks) ->
    case lists:member(rebar_utils:to_binary(Name), Children) of
        true ->
            unlock_children(Children, Apps, Locks, [App | Unlocks]);
        false ->
            unlock_children(Children, Apps, [App | Locks], Unlocks)
    end.

deps_dict(Deps) ->
    lists:foldl(fun(App, Dict) ->
                        Name = rebar_app_info:name(App),
                        Parent = rebar_app_info:parent(App),
                        dict:append_list(Parent, [Name], Dict)
                end, dict:new(), Deps).

all_children(Name, Dict) ->
    lists:flatten(all_children_(Name, Dict)).

all_children_(Name, Dict) ->
    case dict:find(rebar_utils:to_binary(Name), Dict) of
        {ok, Children} ->
            [Children | [all_children_(Child, Dict) || Child <- Children]];
        error ->
            []
    end.
