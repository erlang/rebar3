%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_prv_install_deps).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-export([handle_deps/2,
         handle_deps/3]).

-define(PROVIDER, install_deps).
-define(DEPS, [app_discovery]).

-type src_dep() :: {atom(), string(), {atom(), string(), string()}}.
-type pkg_dep() :: {atom(), binary()} | atom().

-type dep() :: src_dep() | pkg_dep().

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, undefined},
                                                               {short_desc, "Install dependencies"},
                                                               {desc, info("Install dependencies")},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Profile = rebar_state:current_profile(State),
    ProjectApps = rebar_state:project_apps(State),
    try
        {ok, State1} = case rebar_state:get(State, locks, []) of
                           [] ->
                               handle_deps(State, rebar_state:get(State, {deps, Profile}, []));
                           Locks ->
                               handle_deps(State, Locks)
                       end,

        Source = ProjectApps ++ rebar_state:src_apps(State1),
        case rebar_digraph:compile_order(Source) of
            {ok, Sort} ->
                {ok, rebar_state:set(State1, deps_to_build,
                                     lists:dropwhile(fun rebar_app_info:valid/1, Sort -- ProjectApps))};
            {error, Error} ->
                {error, Error}
        end
    catch
        %% maybe_fetch will maybe_throw an exception to break out of some loops
        _:Reason ->
            {error, Reason}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec handle_deps(rebar_state:t(), [dep()]) -> {ok, rebar_state:t()}.
handle_deps(State, Deps) ->
    handle_deps(State, Deps, false).

-spec handle_deps(rebar_state:t(), [dep()], boolean() | {true, binary(), integer()})
                 -> {ok, rebar_state:t()} | {error, string()}.
handle_deps(State, [], _) ->
    {ok, State};
handle_deps(State, Deps, Update) ->
    %% Read in package index and dep graph
    {Packages, Graph} = rebar_packages:get_packages(State),

    %% Split source deps from pkg deps, needed to keep backwards compatibility
    DepsDir = rebar_utils:deps_dir(State),
    {SrcDeps, PkgDeps} = parse_deps(State, DepsDir, Deps),
    State1 = rebar_state:src_deps(rebar_state:pkg_deps(State, PkgDeps),
                                  SrcDeps),

    %% Fetch transitive src deps
    {State2, _Seen} = update_src_deps(0, State1, Update, sets:new()),

    Solved = case rebar_state:pkg_deps(State2) of
                 [] -> %% No pkg deps
                     [];
                 PkgDeps1 ->
                     %% Find pkg deps needed
                     S = case rebar_digraph:cull_deps(Graph, PkgDeps1) of
                             {ok, []} ->
                                 throw({rebar_digraph, no_solution});
                             {ok, Solution} ->
                                 Solution;
                             [] ->
                                 throw({rebar_digraph, no_solution})
                         end,

                     %% Create app_info record for each pkg dep
                     [AppInfo || Pkg <- S,
                                 AppInfo <- package_to_app(DepsDir
                                                          ,Packages
                                                          ,Pkg),
                                 maybe_fetch(State2, AppInfo, Update, sets:new())]
             end,

    AllDeps = lists:ukeymerge(2
                             ,lists:ukeysort(2, rebar_state:src_apps(State2))
                             ,lists:ukeysort(2, Solved)),
    %% Sort all apps to build order
    State3 = rebar_state:set(State2, all_deps, AllDeps),
    State4 = rebar_state:set(State3, {all_deps, rebar_state:current_profile(State)}, AllDeps),
    {ok, State4}.

%% ===================================================================
%% Internal functions
%% ===================================================================

package_to_app(DepsDir, Packages, {Name, Vsn}) ->
    case dict:find({Name, Vsn}, Packages) of
        error ->
            [];
        {ok, P} ->
            PkgDeps = proplists:get_value(<<"deps">>, P, []),
            Link = proplists:get_value(<<"link">>, P, ""),
            {ok, AppInfo} = rebar_app_info:new(Name, Vsn),
            AppInfo1 = rebar_app_info:deps(AppInfo, PkgDeps),
            AppInfo2 = rebar_app_info:dir(AppInfo1, rebar_utils:deps_dir(DepsDir, Name)),
            [rebar_app_info:source(AppInfo2, {pkg, Name, Vsn, Link})]
    end.

-spec update_src_deps(integer(), rebar_state:t(), boolean(), sets:set(binary())) ->
                             {rebar_state:t(), [binary()]}.
update_src_deps(Level, State, Update, Seen) ->
    SrcDeps = rebar_state:src_deps(State),
    case lists:foldl(fun(AppInfo, {SrcDepsAcc, PkgDepsAcc, StateAcc, SeenAcc}) ->
                             SeenAcc1 = sets:add_element(rebar_app_info:name(AppInfo), SeenAcc),
                             {SrcDepsAcc1, PkgDepsAcc1, StateAcc1} =
                                 case Update of
                                     {true, UpdateName, UpdateLevel} ->
                                         handle_update(AppInfo
                                                      ,UpdateName
                                                      ,UpdateLevel
                                                      ,SrcDepsAcc
                                                      ,PkgDepsAcc
                                                      ,Level
                                                      ,StateAcc);
                                     _ ->
                                         maybe_fetch(StateAcc, AppInfo, false, SeenAcc),
                                         handle_dep(AppInfo
                                                   ,SrcDepsAcc
                                                   ,PkgDepsAcc
                                                   ,Level
                                                   ,StateAcc)

                                 end,
                             {SrcDepsAcc1, PkgDepsAcc1, StateAcc1, SeenAcc1}
                     end, {[], rebar_state:pkg_deps(State), State, Seen}, SrcDeps) of
        {[], NewPkgDeps, State1, Seen1} ->
            {rebar_state:pkg_deps(State1, NewPkgDeps), Seen1};
        {NewSrcDeps, NewPkgDeps, State1, Seen1} ->
            State2 = rebar_state:pkg_deps(State1, NewPkgDeps),
            State3 = rebar_state:src_deps(State2, NewSrcDeps),
            update_src_deps(Level+1, State3, Update, Seen1)
    end.

handle_update(AppInfo, UpdateName, UpdateLevel, SrcDeps, PkgDeps, Level, State) ->
    Name = rebar_app_info:name(AppInfo),
    Locks = rebar_state:get(State, locks, []),
    {_, _, _, DepLevel} = lists:keyfind(Name, 1, Locks),
    case UpdateLevel < DepLevel
        orelse Name =:= UpdateName of
        true ->
            case maybe_fetch(State, AppInfo, true, []) of
                true ->
                    handle_dep(AppInfo
                              ,SrcDeps
                              ,PkgDeps
                              ,Level
                              ,State);

                false ->
                    {SrcDeps, PkgDeps, State}
            end;
        false ->
            {SrcDeps, PkgDeps, State}
    end.

handle_dep(AppInfo, SrcDeps, PkgDeps, Level, State) ->
    DepsDir = rebar_utils:deps_dir(State),
    {AppInfo1, NewSrcDeps, NewPkgDeps} =
        handle_dep(State, DepsDir, AppInfo),
    AppInfo2 = rebar_app_info:dep_level(AppInfo1, Level),
    {NewSrcDeps ++ SrcDeps
    ,NewPkgDeps++PkgDeps
    ,rebar_state:src_apps(State, AppInfo2)}.

-spec handle_dep(rebar_state:t(), file:filename_all(), rebar_app_info:t()) ->
                        {rebar_app_info:t(), [rebar_app_info:t()], [pkg_dep()]}.
handle_dep(State, DepsDir, AppInfo) ->
    Profile = rebar_state:current_profile(State),
    C = rebar_config:consult(rebar_app_info:dir(AppInfo)),
    S = rebar_state:new(rebar_state:new(), C, rebar_app_info:dir(AppInfo)),
    S1 = rebar_state:apply_profile(S, Profile),
    Deps = case Profile of
               default ->
                   rebar_state:get(S1, {deps, Profile}, []);
               _ ->
                   rebar_state:get(S1, {deps, default}, []) ++
                       rebar_state:get(S1, {deps, Profile}, [])
           end,
    AppInfo1 = rebar_app_info:deps(AppInfo, rebar_state:deps_names(Deps)),
    {SrcDeps, PkgDeps} = parse_deps(State, DepsDir, Deps),
    {AppInfo1, SrcDeps, PkgDeps}.

-spec maybe_fetch(rebar_state:t(), rebar_app_info:t(), boolean() | {true, binary(), integer()},
                 sets:set(binary())) -> boolean().
maybe_fetch(State, AppInfo, Update, Seen) ->
    AppDir = ec_cnv:to_list(rebar_app_info:dir(AppInfo)),
    DefaultProfileDeps = rebar_utils:default_profile_deps(State),
    Apps = rebar_app_discover:find_apps(["_checkouts", DefaultProfileDeps], all),
    case rebar_app_utils:find(rebar_app_info:name(AppInfo), Apps) of
        {ok, _} ->
            %% Don't fetch dep if it exists in the _checkouts dir
            false;
        error ->
            Exists = case rebar_app_utils:is_app_dir(filename:absname(AppDir)++"-*") of
                         {true, _} ->
                             true;
                         _ ->
                             case rebar_app_utils:is_app_dir(filename:absname(AppDir)) of
                                 {true, _} ->
                                     true;
                                 _ ->
                                     false
                             end
                     end,

            case not Exists orelse Update of
                true ->
                    ?INFO("Fetching ~s", [rebar_app_info:name(AppInfo)]),
                    Source = rebar_app_info:source(AppInfo),
                    case rebar_fetch:download_source(AppDir, Source) of
                        {error, Reason} ->
                            throw(Reason);
                        Result ->
                            Result
                    end;
                _ ->
                    case sets:is_element(rebar_app_info:name(AppInfo), Seen) of
                        true ->
                            false;
                        false ->
                            Source = rebar_app_info:source(AppInfo),
                            case rebar_fetch:needs_update(AppDir, Source) of
                                true ->
                                    ?INFO("Updating ~s", [rebar_app_info:name(AppInfo)]),
                                    case rebar_fetch:download_source(AppDir, Source) of
                                        {error, Reason} ->
                                            throw(Reason);
                                        Result ->
                                            Result
                                    end;
                                false ->
                                    false
                            end
                    end
            end
    end.

-spec parse_deps(rebar_state:t(), binary(), [dep()]) -> {[rebar_app_info:t()], [pkg_dep()]}.
parse_deps(State, DepsDir, Deps) ->
    lists:foldl(fun({Name, Vsn}, {SrcDepsAcc, PkgDepsAcc}) ->
                        {SrcDepsAcc, [parse_goal(ec_cnv:to_binary(Name)
                                                ,ec_cnv:to_binary(Vsn)) | PkgDepsAcc]};
                   (Name, {SrcDepsAcc, PkgDepsAcc}) when is_atom(Name) ->
                        {SrcDepsAcc, [ec_cnv:to_binary(Name) | PkgDepsAcc]};
                   ({Name, Vsn, Source}, {SrcDepsAcc, PkgDepsAcc}) when is_tuple (Source) ->
                        Dep = new_dep(State, DepsDir, Name, Vsn, Source),
                        {[Dep | SrcDepsAcc], PkgDepsAcc};
                   ({Name, Vsn, Source, _Level}, {SrcDepsAcc, PkgDepsAcc}) when is_tuple (Source) ->
                        Dep = new_dep(State, DepsDir, Name, Vsn, Source),
                        {[Dep | SrcDepsAcc], PkgDepsAcc}
                end, {[], []}, Deps).

new_dep(State, DepsDir, Name, Vsn, Source) ->
    Dirs = [ec_cnv:to_list(filename:join(rebar_utils:default_profile_deps(State), Name)),
            ec_cnv:to_list(filename:join(DepsDir, Name))],
    {ok, Dep} = case ec_lists:search(fun(Dir) ->
                                             rebar_app_info:discover(Dir)
                                     end, Dirs) of
                    {ok, App, _} ->
                        {ok, App};
                    not_found ->
                        rebar_app_info:new(Name, Vsn, ec_cnv:to_list(filename:join(DepsDir, Name)))
                end,
    rebar_app_info:source(Dep, Source).

-spec parse_goal(binary(), binary()) -> pkg_dep().
parse_goal(Name, Constraint) ->
    case re:run(Constraint, "([^\\d]*)(\\d.*)", [{capture, [1,2], binary}]) of
        {match, [<<>>, Vsn]} ->
            {Name, Vsn};
        {match, [Op, Vsn]} ->
            {Name, Vsn, binary_to_atom(Op, utf8)};
        nomatch ->
            fail
    end.

info(Description) ->
    io_lib:format("~s.~n"
                 "~n"
                 "Valid rebar.config options:~n"
                 "  ~p~n"
                 "  ~p~n",
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
