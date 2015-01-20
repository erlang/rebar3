%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_upgrade).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

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
                                                   {bare, false},
                                                   {deps, ?DEPS},
                                                   {example, "rebar upgrade cowboy"},
                                                   {short_desc, "Upgrade dependency."},
                                                   {desc, ""},
                                                   {opts, [
                                                          {package, undefined, undefined, string, "Package to upgrade."}
                                                          ]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Name = ec_cnv:to_binary(proplists:get_value(package, Args)),
    Locks = rebar_state:lock(State),
    %% TODO: optimize by running the find + unlock in one sweep
    case find_app(Name, Locks) of
        {AppInfo, 0} ->
            %% Unlock the app and all those with a lock level higher than
            %% it has
            NewLocks = unlock_higher_than(0, Locks -- [AppInfo]),
            {ok, rebar_state:lock(State, NewLocks)};
        {_AppInfo, Level} when Level > 0 ->
            {error, transitive_dependency};
        false ->
            {error, unknown_dependency}
    end.

find_app(_Name, []) -> false;
find_app(Name, [App|Apps]) ->
    case rebar_app_info:name(App) of
        Name -> {App, rebar_app_info:dep_level(App)};
        _ -> find_app(Name, Apps)
    end.

%% Because we operate on a lock list, removing the app from the list
%% unlocks it.
unlock_higher_than(_, []) -> [];
unlock_higher_than(Level, [App | Apps]) ->
    case rebar_app_info:dep_level(App) of
        N when N > Level ->
            unlock_higher_than(Level, Apps);
        N when N =< Level ->
            [App | unlock_higher_than(Level, Apps)]
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
