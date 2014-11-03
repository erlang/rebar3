%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_upgrade).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/2]).

-include("rebar.hrl").

-define(PROVIDER, upgrade).
-define(DEPS, []).

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
    Locks = rebar_state:get(State, locks, []),
    case lists:keyfind(Name, 1, Locks) of
        {_, _, _, Level} ->
            Deps = rebar_state:get(State, deps),
            Dep = lists:keyfind(binary_to_atom(Name, utf8), 1, Deps),
            rebar_prv_install_deps:handle_deps(State, [Dep], {true, Name, Level}),
            {ok, State};
        _ ->
            {error, io_lib:format("No such dependency ~s~n", [Name])}
    end.

-spec format_error(any(), rebar_state:t()) ->  {iolist(), rebar_state:t()}.
format_error(Reason, State) ->
    {io_lib:format("~p", [Reason]), State}.
