-module(rebar_prv_lock).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, lock).
-define(DEPS, [install_deps]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, ""},
                                                               {short_desc, "Locks dependencies."},
                                                               {desc, "Locks dependencies"},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% Only lock default profile run
    case rebar_state:current_profiles(State) of
        [default] ->
            OldLocks = rebar_state:get(State, {locks, default}, []),
            Locks = lists:keysort(1, build_locks(State)),
            Dir = rebar_state:dir(State),
            rebar_config:write_lock_file(filename:join(Dir, ?LOCK_FILE), Locks),
            State1 = rebar_state:set(State, {locks, default}, Locks),

            OldLockNames = [element(1,L) || L <- OldLocks],
            NewLockNames = [element(1,L) || L <- Locks],
            rebar_utils:info_useless(OldLockNames, NewLockNames),

            {ok, State1};
        _ ->
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

build_locks(State) ->
    AllDeps = rebar_state:lock(State),
    [begin
        Dir = rebar_app_info:dir(Dep),
        Source = rebar_app_info:source(Dep),

        %% If source is tuple it is a source dep
        %% e.g. {git, "git://github.com/ninenines/cowboy.git", "master"}
        {rebar_app_info:name(Dep)
        ,rebar_fetch:lock_source(Dir, Source, State)
        ,rebar_app_info:dep_level(Dep)}
     end || Dep <- AllDeps, not(rebar_app_info:is_checkout(Dep))].
