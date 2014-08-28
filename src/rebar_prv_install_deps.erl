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

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-export([setup_env/1]).

%% for internal use only
-export([get_deps_dir/1]).
-export([get_deps_dir/2]).

-define(PROVIDER, install_deps).
-define(DEPS, [app_discovery]).

-record(dep, {name :: binary(),
              vsn :: binary(),
              source :: binary()}).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, #provider{name = ?PROVIDER,
                                                       provider_impl = ?MODULE,
                                                       bare = false,
                                                       deps = ?DEPS,
                                                       example = "rebar deps",
                                                       short_desc = "Install dependencies",
                                                       desc = info("Install dependencies"),
                                                       opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    %% Read in package index and dep graph
    {Packages, Graph} = rebar_packages:get_packages(State),

    case rebar_state:get(State, deps, []) of
        [] ->
            {ok, State};
        Deps ->
            %% Split source deps form binary deps, needed to keep backwards compatibility
            {SrcDeps, Goals} = parse_deps(Deps),
            case update_src_deps(State, SrcDeps, Goals, []) of
                {State1, SrcDeps1, [], Locked} ->
                    {ok, rebar_state:set(State1, deps, Locked)};
                {State1, SrcDeps1, Goals1, Locked} ->
                    {ok, Solved} = rlx_depsolver:solve(Graph, Goals1),
                    M = lists:map(fun({Name, Vsn}) ->
                                          FmtVsn = ec_cnv:to_binary(rlx_depsolver:format_version(Vsn)),
                                          {ok, P} = dict:find({Name, FmtVsn}, Packages),
                                          Link = proplists:get_value(<<"link">>, P),
                                          #dep{name=Name,
                                              vsn=FmtVsn,
                                              source={Name
                                                     ,FmtVsn
                                                     ,Link}}
                                  end, Solved),
                    {State2, Deps1, Locked2} = update_deps(State1, M),
                    State3 = rebar_state:set(State2, deps, Locked++Locked2),
                    {ok, rebar_state:set(State3, goals, Goals1)}
            end
    end.

%% set REBAR_DEPS_DIR and ERL_LIBS environment variables
setup_env(State) ->
    DepsDir = get_deps_dir(State),
    %% include rebar's DepsDir in ERL_LIBS
    Separator = case os:type() of
                    {win32, nt} ->
                        ";";
                    _ ->
                        ":"
                end,
    ERL_LIBS = case os:getenv("ERL_LIBS") of
                   false ->
                       {"ERL_LIBS", DepsDir};
                   PrevValue ->
                       {"ERL_LIBS", DepsDir ++ Separator ++ PrevValue}
               end,
    [{"REBAR_DEPS_DIR", DepsDir}, ERL_LIBS].


get_deps_dir(State) ->
    BaseDir = rebar_state:get(State, base_dir, ""),
    get_deps_dir(BaseDir, "deps").

get_deps_dir(DepsDir, App) ->
    filename:join(DepsDir, App).

%% ===================================================================
%% Internal functions
%% ===================================================================

new({Name, Vsn, Source})->
    #dep{name=ec_cnv:to_binary(Name), vsn=ec_cnv:to_binary(Vsn), source=Source};
new(Name) ->
    #dep{name=ec_cnv:to_binary(Name)}.

%% Fetch missing binary deps
update_deps(State, Deps) ->
    DepsDir = get_deps_dir(State),

    %% Find available apps to fulfill dependencies
    %% Should only have to do this once, not every iteration
    UnbuiltApps = rebar_app_discover:find_unbuilt_apps([DepsDir]),
    FoundApps = rebar_app_discover:find_apps([DepsDir]),

    download_missing_deps(State, DepsDir, FoundApps, Deps).

%% Find source deps to build and download
update_src_deps(State, Deps, Goals, Locked) ->
    DepsDir = get_deps_dir(State),

    %% Find available apps to fulfill dependencies
    %% Should only have to do this once, not every iteration
    %UnbuiltApps = rebar_app_discover:find_unbuilt_apps([DepsDir]),
    FoundApps = rebar_app_discover:find_apps([DepsDir]),

    %% Resolve deps and their dependencies
    {Deps1, NewGoals} = handle_src_deps(Deps, FoundApps, Goals),
    case download_missing_deps(State, DepsDir, FoundApps, Deps1) of
        {State1, [], []} ->
            {State1, Deps1, NewGoals, Locked};
        {State1, Missing, Locked1} ->
            update_src_deps(State1, Missing, NewGoals, Locked1++Locked)
    end.

%% Collect deps of new deps
handle_src_deps(Deps, Found, Goals) ->
    lists:foldl(fun(X, {DepsAcc, GoalsAcc}) ->
                        C = rebar_config:consult(rebar_app_info:dir(X)),
                        S = rebar_state:new(rebar_state:new(), C, rebar_app_info:dir(X)),
                        {ParsedDeps, NewGoals} = parse_deps(rebar_state:get(S, deps, [])),
                        {ParsedDeps++DepsAcc, NewGoals++GoalsAcc}
                end, {Deps, Goals}, Found).

%% Fetch missing deps from source
download_missing_deps(State, DepsDir, Found, Deps) ->
    Missing =
        lists:filter(fun(#dep{name=Name}) ->
                            not lists:any(fun(F) ->
                                                  Name =:= rebar_app_info:name(F)
                                          end, Found)
                     end, Deps),
    Locked = lists:map(fun(Dep=#dep{name=Name, source=Source}) ->
                               TargetDir = get_deps_dir(DepsDir, Name),
                               ?INFO("Fetching ~s ~s~n", [Name
                                                         ,element(2, Source)]),
                               rebar_fetch:download_source(TargetDir, Source),
                               case rebar_app_discover:find_unbuilt_apps([TargetDir]) of
                                   [AppSrc] ->
                                       C = rebar_config:consult(rebar_app_info:dir(AppSrc)),
                                       S = rebar_state:new(rebar_state:new()
                                                          ,C
                                                          ,rebar_app_info:dir(AppSrc)),
                                       AppInfo = rebar_prv_app_builder:build(S, AppSrc),
                                       Ref = rebar_fetch:current_ref(binary_to_list(TargetDir), Source),
                                       {Name
                                       ,ec_cnv:to_binary(rebar_app_info:original_vsn(AppInfo))
                                       ,erlang:setelement(3, Source, Ref)};
                                   [] ->
                                       Source
                               end
                       end, Missing),

    {State, Missing, Locked}.

parse_deps(Deps) ->
    lists:foldl(fun({Name, Vsn}, {SrcDepsAcc, GoalsAcc}) ->
                        {SrcDepsAcc, [parse_goal(ec_cnv:to_binary(Name)
                                                ,ec_cnv:to_binary(Vsn)) | GoalsAcc]};
                   (Name, {SrcDepsAcc, GoalsAcc}) when is_atom(Name) ->
                        {SrcDepsAcc, [ec_cnv:to_binary(Name) | GoalsAcc]};
                   (SrcDep, {SrcDepsAcc, GoalsAcc}) ->
                        Dep = new(SrcDep),
                        {[Dep | SrcDepsAcc], GoalsAcc}
                end, {[], []}, Deps).

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
