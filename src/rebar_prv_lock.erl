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
                                                       bare = true,
                                                       deps = ?DEPS,
                                                       example = "",
                                                       short_desc = "Locks dependencies.",
                                                       desc = info("Locks dependencies"),
                                                       opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    case rebar_state:get(State, locks, []) of
        [] ->
            AllDeps = rebar_state:get(State, all_deps, []),
            Locks = lists:map(fun(Dep) ->
                                      Dir = rebar_app_info:dir(Dep),

                                      %% If source is tuple it is a source dep
                                      %% e.g. {git, "git://github.com/ninenines/cowboy.git", "master"}
                                      case rebar_app_info:source(Dep) of
                                          Source when is_tuple(Source) ->
                                              {rebar_app_info:name(Dep)
                                              ,rebar_app_info:original_vsn(Dep)
                                              ,rebar_fetch:lock_source(Dir, Source)};
                                          _Source ->
                                              {rebar_app_info:name(Dep)
                                              ,rebar_app_info:original_vsn(Dep)}
                                      end
                              end, AllDeps),
            Dir = rebar_state:dir(State),
            file:write_file(filename:join(Dir, "rebar.lock"), io_lib:format("~p.~n", [Locks])),
            {ok, rebar_state:set(State, locks, Locks)};
        _Locks ->
            {ok, State}
    end.

info(_) ->
    "".
