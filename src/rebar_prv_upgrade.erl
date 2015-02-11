%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_upgrade).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, upgrade).
-define(DEPS, []).
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
                                                   {bare, false},
                                                   {deps, ?DEPS},
                                                   {example, "rebar upgrade cowboy[,ranch]"},
                                                   {short_desc, "Upgrade dependency."},
                                                   {desc, ""},
                                                   {opts, [
                                                          {package, undefined, undefined, string, "Packages to upgrade."}
                                                          ]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Locks = rebar_state:get(State, {locks, default}, []),
    Deps = rebar_state:get(State, deps),
    Names = parse_names(ec_cnv:to_binary(proplists:get_value(package, Args)), Locks),
    case prepare_locks(Names, Deps, Locks, []) of
        {error, Reason} ->
            {error, Reason};
        {Locks0, _Unlocks0} -> % unlocks may be useful for deletions
            Deps0 = top_level_deps(Deps, Locks),
            State1 = rebar_state:set(State, {deps, default}, Deps0),
            State2 = rebar_state:set(State1, {locks, default}, Locks0),
            State3 = rebar_state:set(State2, upgrade, true),
            Res = rebar_prv_install_deps:do(State3),
            case Res of
                {ok, State4} ->
                    rebar_prv_lock:do(State4);
                _ ->
                    Res
            end
    end.

-spec format_error(any()) -> iolist().
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
    case lists:keyfind(Name, 1, Locks) of
        {_, _, 0} = Lock ->
            AtomName = binary_to_atom(Name, utf8),
            case lists:keyfind(AtomName, 1, Deps) of
                false ->
                    {error, {unknown_dependency, Name}};
                Dep ->
                    Source = case Dep of
                        {_, Src} -> Src;
                        {_, _, Src} -> Src
                    end,
                    {NewLocks, NewUnlocks} = unlock_higher_than(0, Locks -- [Lock]),
                    prepare_locks(Names,
                                  %deps_like_locks(Deps, [{Name,Source,0} | NewLocks]),
                                  Deps,
                                  NewLocks,
                                  [{Name, Source, 0} | NewUnlocks ++ Unlocks])
            end;
        {_, _, Level} when Level > 0 ->
            {error, {transitive_dependency,Name}};
        false ->
            {error, {unknown_dependency,Name}}
    end.

top_level_deps(Deps, Locks) ->
    [Dep || Dep <- Deps, lists:keymember(0, 3, Locks)].

unlock_higher_than(Level, Locks) -> unlock_higher_than(Level, Locks, [], []).

unlock_higher_than(_, [], Locks, Unlocks) ->
    {Locks, Unlocks};
unlock_higher_than(Level, [App = {_,_,AppLevel} | Apps], Locks, Unlocks) ->
    if AppLevel > Level  -> unlock_higher_than(Level, Apps, Locks, [App | Unlocks]);
       AppLevel =< Level -> unlock_higher_than(Level, Apps, [App | Locks], Unlocks)
    end.

