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
    List = [{Profile, rebar_state:get(State, {deps, Profile}, [])}
           || Profile <- Profiles],
    [display(State, Profile, Deps) || {Profile, Deps} <- List],
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

display(State, default, Deps) ->
    NewDeps = merge(Deps, rebar_state:get(State, deps, [])),
    display_deps(State, NewDeps),
    ?CONSOLE("", []);
display(State, Profile, Deps) ->
    ?CONSOLE("-- ~p --", [Profile]),
    display_deps(State, Deps),
    ?CONSOLE("", []).

merge(Deps, SourceDeps) ->
    merge1(dedup([normalize(Dep) || Dep <- Deps]),
           [normalize(Dep) || Dep <- SourceDeps]).

normalize(Name) when is_binary(Name) ->
    Name;
normalize(Name) when is_atom(Name) ->
    atom_to_binary(Name, unicode);
normalize(Dep) when is_tuple(Dep) ->
    Name = element(1, Dep),
    setelement(1, Dep, normalize(Name)).

merge1(Deps, SourceDeps) ->
    Names = [name(Dep) || Dep <- Deps],
    ToAdd = [Dep || Dep <- SourceDeps,
                    not lists:member(name(Dep), Names)],
    Deps ++ ToAdd.

%% Keep the latter one as locks come after regular deps in the list.
%% This is totally not safe as an assumption, but it's what we got.
%% We do this by comparing the current element and looking if a
%% similar named one happens later. If so, drop the current one.
dedup(Deps) -> dedup(Deps, [name(Dep) || Dep <- Deps]).

dedup([], []) -> [];
dedup([Dep|Deps], [Name|DepNames]) ->
    case lists:member(Name, DepNames) of
        true -> dedup(Deps, DepNames);
        false -> [Dep | dedup(Deps, DepNames)]
    end.

name(T) when is_tuple(T) -> element(1, T);
name(B) when is_binary(B) -> B.

display_deps(State, Deps) ->
    lists:foreach(fun(Dep) -> display_dep(State, Dep) end, Deps).

%% packages
display_dep(_State, {Name, Vsn}) when is_list(Vsn) ->
    ?CONSOLE("~ts* (package ~ts)", [rebar_utils:to_binary(Name), rebar_utils:to_binary(Vsn)]);
display_dep(_State, Name) when is_binary(Name) ->
    ?CONSOLE("~ts* (package)", [Name]);
display_dep(_State, {Name, Source}) when is_tuple(Source) ->
    ?CONSOLE("~ts* (~ts source)", [rebar_utils:to_binary(Name), type(Source)]);
display_dep(_State, {Name, _Vsn, Source}) when is_tuple(Source) ->
    ?CONSOLE("~ts* (~ts source)", [rebar_utils:to_binary(Name), type(Source)]);
display_dep(_State, {Name, _Vsn, Source, _Opts}) when is_tuple(Source) ->
    ?CONSOLE("~ts* (~ts source)", [rebar_utils:to_binary(Name), type(Source)]);
%% Locked
display_dep(State, {Name, Source={pkg, _, Vsn}, Level}) when is_integer(Level) ->
    DepsDir = rebar_dir:deps_dir(State),
    AppDir = filename:join([DepsDir, rebar_utils:to_binary(Name)]),
    NeedsUpdate = case rebar_fetch:needs_update(AppDir, Source, State) of
        true -> "*";
        false -> ""
    end,
    ?CONSOLE("~ts~ts (locked package ~ts)", [Name, NeedsUpdate, Vsn]);
display_dep(State, {Name, Source, Level}) when is_tuple(Source), is_integer(Level) ->
    DepsDir = rebar_dir:deps_dir(State),
    AppDir = filename:join([DepsDir, rebar_utils:to_binary(Name)]),
    NeedsUpdate = case rebar_fetch:needs_update(AppDir, Source, State) of
        true -> "*";
        false -> ""
    end,
    ?CONSOLE("~ts~ts (locked ~ts source)", [Name, NeedsUpdate, type(Source)]).

type(Source) when is_tuple(Source) -> element(1, Source).
