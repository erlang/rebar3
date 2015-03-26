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
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 deps"},
                                                               {short_desc, "List dependencies"},
                                                               {desc, info("List dependencies")},
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

merge(Deps, Deps) ->
    Deps;
merge(Deps, SourceDeps) ->
    ToAdd = [Dep || Dep <- SourceDeps,
                    not lists:keymember(ec_cnv:to_binary(element(1,Dep)), 1, Deps)],
    Deps ++ ToAdd.

display_deps(State, Deps) ->
    lists:foreach(fun(Dep) -> display_dep(State, Dep) end, Deps).

%% packages
display_dep(_State, {Name, Vsn}) when is_list(Vsn) ->
    ?CONSOLE("~s* (package ~s)", [ec_cnv:to_binary(Name), ec_cnv:to_binary(Vsn)]);
display_dep(_State, Name) when is_atom(Name) ->
    ?CONSOLE("~s* (package)", [ec_cnv:to_binary(Name)]);
%% git source
display_dep(_State, {Name, Source}) when is_tuple(Source), element(1, Source) =:= git ->
    ?CONSOLE("~s* (git source)", [ec_cnv:to_binary(Name)]);
display_dep(_State, {Name, _Vsn, Source}) when is_tuple(Source), element(1, Source) =:= git ->
    ?CONSOLE("~s* (git source)", [ec_cnv:to_binary(Name)]);
display_dep(_State, {Name, _Vsn, Source, _Opts}) when is_tuple(Source), element(1, Source) =:= git ->
    ?CONSOLE("~s* (git soutce)", [ec_cnv:to_binary(Name)]);
%% unknown source
display_dep(_State, {Name, Source}) when is_tuple(Source) ->
    ?CONSOLE("~s* (source ~p)", [ec_cnv:to_binary(Name), Source]);
display_dep(_State, {Name, _Vsn, Source}) when is_tuple(Source) ->
    ?CONSOLE("~s* (source ~p)", [ec_cnv:to_binary(Name), Source]);
display_dep(_State, {Name, _Vsn, Source, _Opts}) when is_tuple(Source) ->
    ?CONSOLE("~s* (source ~p)", [ec_cnv:to_binary(Name), Source]);
%% Locked
display_dep(State, {Name, Source={pkg, _, Vsn}, Level}) when is_integer(Level) ->
    DepsDir = rebar_dir:deps_dir(State),
    AppDir = filename:join([DepsDir, ec_cnv:to_binary(Name)]),
    NeedsUpdate = case rebar_fetch:needs_update(AppDir, Source) of
        true -> "*";
        false -> ""
    end,
    ?CONSOLE("~s~s (locked package ~s)", [Name, NeedsUpdate, Vsn]);
display_dep(State, {Name, Source, Level}) when is_tuple(Source), is_integer(Level), element(1, Source) =:= git ->
    DepsDir = rebar_dir:deps_dir(State),
    AppDir = filename:join([DepsDir, ec_cnv:to_binary(Name)]),
    NeedsUpdate = case rebar_fetch:needs_update(AppDir, Source) of
        true -> "*";
        false -> ""
    end,
    ?CONSOLE("~s~s (locked git source)", [Name, NeedsUpdate]);
display_dep(State, {Name, Source, Level}) when is_tuple(Source), is_integer(Level) ->
    DepsDir = rebar_dir:deps_dir(State),
    AppDir = filename:join([DepsDir, ec_cnv:to_binary(Name)]),
    NeedsUpdate = case rebar_fetch:needs_update(AppDir, Source) of
        true -> "*";
        false -> ""
    end,
    ?CONSOLE("~s~s (locked ~p)", [Name, NeedsUpdate, Source]).

info(Description) ->
    io_lib:format("~s.~n"
                 "~n"
                 "Valid rebar.config options:~n"
                 "  ~p~n"
                 "  ~p~n"
                 "Valid command line options:~n"
                 "  deps_dir=\"deps\" (override default or rebar.config deps_dir)~n",
                 [
                 Description,
                 {deps_dir, "deps"},
                 {deps,
                  [app_name,
                   {rebar, "1.0.*"},
                   {rebar, ".*",
                    {git, "git://github.com/rebar/rebar.git"}},
                   {rebar, ".*",
                    {git, "git://github.com/rebar/rebar.git", "Rev"}},
                   {rebar, "1.0.*",
                    {git, "git://github.com/rebar/rebar.git", {branch, "master"}}},
                   {rebar, "1.0.0",
                    {git, "git://github.com/rebar/rebar.git", {tag, "1.0.0"}}},
                   {rebar, "",
                    {git, "git://github.com/rebar/rebar.git", {branch, "master"}},
                    [raw]},
                   {app_name, ".*", {hg, "https://www.example.org/url"}},
                   {app_name, ".*", {rsync, "Url"}},
                   {app_name, ".*", {svn, "https://www.example.org/url"}},
                   {app_name, ".*", {svn, "svn://svn.example.org/url"}},
                   {app_name, ".*", {bzr, "https://www.example.org/url", "Rev"}},
                   {app_name, ".*", {fossil, "https://www.example.org/url"}},
                   {app_name, ".*", {fossil, "https://www.example.org/url", "Vsn"}},
                   {app_name, ".*", {p4, "//depot/subdir/app_dir"}}]}
                 ]).
