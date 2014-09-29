%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_upgrade).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, upgrade).
-define(DEPS, [lock]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, #provider{name = ?PROVIDER,
                                                       provider_impl = ?MODULE,
                                                       bare = false,
                                                       deps = ?DEPS,
                                                       example = "rebar upgrade cowboy",
                                                       short_desc = "Upgrade dependency.",
                                                       desc = "",
                                                       opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:command_args(State) of
        [Name] ->
            ?INFO("Updating ~s~n", [Name]),
            Locks = rebar_state:get(State, locks, []),
            case lists:keyfind(ec_cnv:to_binary(Name), 1, Locks) of
                {_, _, _, Level} ->
                    Deps = rebar_state:get(State, deps),
                    Dep = lists:keyfind(list_to_atom(Name), 1, Deps),
                    rebar_prv_install_deps:handle_deps(State, [Dep], {true, ec_cnv:to_binary(Name), Level}),
                    {ok, State};
                false ->
                    {error, io_lib:format("No such dependency ~s~n", [Name])}
            end;
        [] ->
            {error, "No package given to upgrade."}
    end.
