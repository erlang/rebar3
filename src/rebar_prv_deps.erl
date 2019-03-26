-module(rebar_prv_deps).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, deps).
-define(DEPS, [app_discovery]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(
            State,
            providers:create([
                    {name, ?PROVIDER},
                    {module, ?MODULE},
                    {bare, true},
                    {deps, ?DEPS},
                    {example, "rebar3 deps"},
                    {short_desc, "List dependencies"},
                    {desc, "List dependencies. Those not matching lock files "
                           "are followed by an asterisk (*)."},
                    {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Profiles = rebar_state:current_profiles(State),
    [display(State, Profile, rebar_state:get(State, {parsed_deps, Profile}, []))
     || Profile <- Profiles],
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

display(State, default, Deps) ->
    display_deps(State, Deps),
    ?CONSOLE("", []);
display(State, Profile, Deps) ->
    ?CONSOLE("-- ~p --", [Profile]),
    display(State, default, Deps).

display_deps(State, Deps) ->
    lists:foreach(fun(Dep) -> display_dep(State, Dep) end, Deps).

display_dep(State, Dep) ->
    DepWithSource = rebar_app_utils:expand_deps_sources(Dep, State),

    Name = rebar_utils:to_binary(rebar_app_info:name(DepWithSource)),
    NeedsUpdateSuffix = case rebar_fetch:needs_update(DepWithSource, State) of
                            true -> "*";
                            false -> ""
                        end,
    IsLockedPrefix = case rebar_app_info:is_lock(DepWithSource) of
                         true -> "locked ";
                         _ -> ""
                     end,
    CommonConsoleArgs = [Name, NeedsUpdateSuffix, IsLockedPrefix],

    Source = rebar_app_info:source(DepWithSource),
    case Source of
        {pkg, _Name, Vsn, _Hash, _RepoConfig} ->
            VsnBinary = rebar_utils:to_binary(Vsn),
            ?CONSOLE("~ts~ts (~tspackage ~ts)", CommonConsoleArgs ++ [VsnBinary]);
        _ ->
            ?CONSOLE("~ts~ts (~ts~ts source)", CommonConsoleArgs ++ [type(Source)])
    end.

type(Source) when is_tuple(Source) -> element(1, Source).

