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
    Locks = rebar_state:get(State, {locks, default}, []),
    case lists:keyfind(Name, 1, Locks) of
        {_, _, 0} = Lock ->
            Deps = rebar_state:get(State, deps),
            case lists:keyfind(binary_to_atom(Name, utf8), 1, Deps) of
                false ->
                    {error, unknown_dependency};
                Dep ->
                    Source = case Dep of
                        {_, Src} -> Src;
                        {_, _, Src} -> Src
                    end,
                    NewLocks = unlock_higher_than(0, Locks -- [Lock]),
                    State1 = rebar_state:set(State, {deps, default}, [{Name, Source, 0} | NewLocks]),
                    State2 = rebar_state:set(State1, {locks, default}, NewLocks),
                    rebar_prv_install_deps:do(State2)
            end;
        {_, _, Level} when Level > 0 ->
            {error, transitive_dependency};
        false ->
            ct:pal("deps: ~p", [{Name,Locks}]),
            {error, unknown_dependency}
    end.


unlock_higher_than(_, []) -> [];
unlock_higher_than(Level, [App = {_,_,AppLevel} | Apps]) ->
    if AppLevel > Level  -> unlock_higher_than(Level, Apps);
       AppLevel =< Level -> [App | unlock_higher_than(Level, Apps)]
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

