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
-module(rebar_prv_deps).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-export([setup_env/1]).

%% for internal use only
-export([get_deps_dir/1]).
-export([get_deps_dir/2]).

-define(PROVIDER, deps).
-define(DEPS, [app_discovery]).

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
                                                       desc = info_help("Install dependencies"),
                                                       opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    %% Read in package index and dep graph
    {Packages, Graph} = get_packages(State),
    PtDeps = rebar_state:get(State, pt_deps, []),
    SrcDeps = rebar_state:get(State, src_deps, []),
    {State1, _PtDeps1} = update_deps(State, PtDeps),
    {State1, SrcDeps1} = update_deps(State, SrcDeps),

    case rebar_state:get(State1, deps, []) of
        [] ->
            {ok, rebar_state:set(State1, deps, SrcDeps1)};
        Deps ->
            Goals = lists:map(fun({Name, Vsn}) ->
                                      {atom_to_binary(Name, utf8), Vsn};
                                 (Name) ->
                                      atom_to_binary(Name, utf8)
                              end, Deps),
            {ok, Solved} = rlx_depsolver:solve(Graph, Goals),
            M = lists:map(fun({Name, Vsn}) ->
                                  FmtVsn = to_binary(rlx_depsolver:format_version(Vsn)),
                                  {ok, P} = dict:find({Name, FmtVsn}, Packages),
                                  Link = proplists:get_value(<<"link">>, P),
                                  {Name, Vsn, {Name
                                              ,FmtVsn
                                              ,Link}}
                          end, Solved),

            {State2, Deps1} = update_deps(State1, M),
            State3 = rebar_state:set(State2, deps, Deps1),

            {ok, rebar_state:set(State3, goals, Goals)}
    end.

update_deps(State, Deps) ->
    DepsDir = get_deps_dir(State),

    %% Find available apps to fulfill dependencies
    %% Should only have to do this once, not every iteration
    UnbuiltApps = rebar_app_discover:find_unbuilt_apps([DepsDir]),
    FoundApps = rebar_app_discover:find_apps([DepsDir]),

    %% Resolve deps and their dependencies
    Deps1 = handle_deps(Deps, UnbuiltApps++FoundApps),

    case download_missing_deps(State, DepsDir, FoundApps, UnbuiltApps, Deps1) of
        {State1, []} ->
            {State1, Deps1};
        {State1, _} ->
            update_deps(State1, Deps1)
    end.

handle_deps(Deps, Found) ->
    NewDeps =
        lists:foldl(fun(X, DepsAcc) ->
                            C = rebar_config:consult(rebar_app_info:dir(X)),
                            S = rebar_state:new(rebar_state:new(), C, rebar_app_info:dir(X)),
                            LocalDeps = rebar_state:get(S, deps, []),
                            [LocalDeps | DepsAcc]
                    end, [], Found),
    NewDeps1 = lists:flatten(NewDeps),

    %% Weed out duplicates
    lists:umerge(fun(A, B) ->
                         dep_name(A) =:= dep_name(B)
                 end, lists:usort(Deps), lists:usort(NewDeps1)).

dep_name({Name, _, _}) ->
    Name;
dep_name({Name, _}) ->
    Name;
dep_name(Name) ->
    Name.


to_binary(X) when is_binary(X) ->
    X;
to_binary(X) when is_atom(X) ->
    atom_to_binary(X, utf8);
to_binary(X) when is_list(X) ->
    iolist_to_binary(X).

download_missing_deps(State, DepsDir, Found, Unbuilt, Deps) ->
    Missing = lists:filter(fun(X) ->
                               not lists:any(fun(F) ->
                                                     to_binary(dep_name(X)) =:= to_binary(rebar_app_info:name(F))
                                             end, Found++Unbuilt)
                       end, Deps),
    lists:map(fun({DepName, _DepVsn, DepSource}) ->
                          TargetDir = get_deps_dir(DepsDir, DepName),
                          case filelib:is_dir(TargetDir) of
                              true ->
                                  ok;
                              false ->
                                  ?INFO("Fetching ~s ~s~n", [DepName
                                                            ,element(2, DepSource)]),
                                  rebar_fetch:download_source(TargetDir, DepSource),
                                  case rebar_app_discover:find_unbuilt_apps([TargetDir]) of
                                      [AppSrc] ->
                                          C = rebar_config:consult(rebar_app_info:dir(AppSrc)),
                                          S = rebar_state:new(rebar_state:new()
                                                             ,C
                                                             ,rebar_app_info:dir(AppSrc)),
                                          _AppInfo1 = rebar_prv_app_builder:build(S, AppSrc);
                                      [] ->
                                          []
                                  end
                          end
                  end, Missing),

    {State, []}.

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

get_packages(State) ->
    RebarDir = rebar_state:get(State, global_rebar_dir, filename:join(os:getenv("HOME"), ".rebar")),
    PackagesFile = filename:join(RebarDir, "packages"),
    case ec_file:exists(PackagesFile) of
        true ->
            try
                {ok, Binary} = file:read_file(PackagesFile),
                binary_to_term(Binary)
            catch
                _:_ ->
                    ?ERROR("Bad packages index, try to fix with `rebar update`~n", []),
                    {[], rlx_depsolver:new()}
            end;
        false ->
            {[], rlx_depsolver:new()}
    end.

info_help(Description) ->
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
