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
                                                   {desc, "Upgrade project dependecies. Mentioning no application "
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
    Deps = rebar_state:get(State, deps, []),
    Names = parse_names(ec_cnv:to_binary(proplists:get_value(package, Args, <<"">>)), Locks),
    case prepare_locks(Names, Deps, Locks, []) of
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
            Res = rebar_prv_install_deps:do(rebar_state:lock(State4, UpdatedLocks)),
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
    io_lib:format("Dependency ~ts is transient and cannot be safely upgraded. "
                 "Promote it to your top-level rebar.config file to upgrade it.",
                 [Name]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

parse_names(Bin, Locks) ->
    case lists:usort(re:split(Bin, <<" *, *">>, [trim])) of
        %% Nothing submitted, use *all* apps
        [<<"">>] -> [Name || {Name, _, 0} <- Locks];
        [] -> [Name || {Name, _, 0} <- Locks];
        %% Regular options
        Other -> Other
    end.

prepare_locks([], _, Locks, Unlocks) ->
    {Locks, Unlocks};
prepare_locks([Name|Names], Deps, Locks, Unlocks) ->
    AtomName = binary_to_atom(Name, utf8),
    case lists:keyfind(Name, 1, Locks) of
        {_, _, 0} = Lock ->
            case rebar_utils:tup_find(AtomName, Deps) of
                false ->
                    ?WARN("Dependency ~s has been removed and will not be upgraded", [Name]),
                    prepare_locks(Names, Deps, Locks, Unlocks);
                Dep ->
                    {Source, NewLocks, NewUnlocks} = prepare_lock(Dep, Lock, Locks),
                    prepare_locks(Names, Deps, NewLocks,
                                  [{Name, Source, 0} | NewUnlocks ++ Unlocks])
            end;
        {_, _, Level} = Lock when Level > 0 ->
            case rebar_utils:tup_find(AtomName, Deps) of
                false ->
                    ?PRV_ERROR({transitive_dependency, Name});
                Dep -> % Dep has been promoted
                    {Source, NewLocks, NewUnlocks} = prepare_lock(Dep, Lock, Locks),
                    prepare_locks(Names, Deps, NewLocks,
                                  [{Name, Source, 0} | NewUnlocks ++ Unlocks])
            end;
        false ->
            ?PRV_ERROR({unknown_dependency, Name})
    end.

prepare_lock(Dep, Lock, Locks) ->
    Source = case Dep of
        {_, SrcOrVsn} -> SrcOrVsn;
        {_, _, Src} -> Src;
        _ when is_atom(Dep) ->
            %% version-free package. Must unlock whatever matches in locks
            {_, Vsn, _} = lists:keyfind(ec_cnv:to_binary(Dep), 1, Locks),
            Vsn
    end,
    {NewLocks, NewUnlocks} = unlock_higher_than(0, Locks -- [Lock]),
    {Source, NewLocks, NewUnlocks}.

top_level_deps(Deps, Locks) ->
    [Dep || Dep <- Deps, lists:keymember(0, 3, Locks)].

unlock_higher_than(Level, Locks) -> unlock_higher_than(Level, Locks, [], []).

unlock_higher_than(_, [], Locks, Unlocks) ->
    {Locks, Unlocks};
unlock_higher_than(Level, [App = {_,_,AppLevel} | Apps], Locks, Unlocks) ->
    if AppLevel > Level  -> unlock_higher_than(Level, Apps, Locks, [App | Unlocks]);
       AppLevel =< Level -> unlock_higher_than(Level, Apps, [App | Locks], Unlocks)
    end.
