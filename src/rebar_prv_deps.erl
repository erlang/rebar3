-module(rebar_prv_deps).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, deps).
-define(DEPS, []).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, #provider{name = ?PROVIDER,
                                                       provider_impl = ?MODULE,
                                                       bare = true,
                                                       deps = ?DEPS,
                                                       example = "rebar deps",
                                                       short_desc = "List dependencies",
                                                       desc = info("List dependencies"),
                                                       opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    {ok, State}.

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
