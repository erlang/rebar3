-module(rebar_prv_lock).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, lock).
-define(DEPS, [install_deps]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, #provider{name = ?PROVIDER,
                                                       provider_impl = ?MODULE,
                                                       bare = false,
                                                       deps = ?DEPS,
                                                       example = "",
                                                       short_desc = "Locks dependencies",
                                                       desc = info("Locks dependencies"),
                                                       opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    AllDeps = rebar_state:get(State, all_deps, []),
    Locks = lists:map(fun(Dep) ->
                              Dir = rebar_app_info:dir(Dep),
                              {rebar_app_info:name(Dep)
                              ,rebar_app_info:original_vsn(Dep)
                              ,rebar_fetch:lock_source(Dir, rebar_app_info:source(Dep))}
                      end, AllDeps),
    Dir = rebar_state:dir(State),
    file:write_file(filename:join(Dir, "rebar.lock"), io_lib:format("~p.~n", [Locks])),
    {ok, rebar_state:set(State, locks, Locks)}.

info(_) ->
    "".
