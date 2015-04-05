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
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, ""},
                                                               {short_desc, "Locks dependencies."},
                                                               {desc, info("Locks dependencies")},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    AllDeps = rebar_state:lock(State),
    Locks = [begin
                 Dir = rebar_app_info:dir(Dep),
                 Source = rebar_app_info:source(Dep),

                 %% If source is tuple it is a source dep
                 %% e.g. {git, "git://github.com/ninenines/cowboy.git", "master"}
                 {rebar_app_info:name(Dep)
                 ,rebar_fetch:lock_source(Dir, Source, State)
                 ,rebar_app_info:dep_level(Dep)}
             end || Dep <- AllDeps, not(rebar_app_info:is_checkout(Dep))],
    Dir = rebar_state:dir(State),
    file:write_file(filename:join(Dir, ?LOCK_FILE), io_lib:format("~p.~n", [Locks])),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

info(_) ->
    "".
