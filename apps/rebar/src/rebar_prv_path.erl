%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_path).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, path).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 path"},
                                                               {short_desc, "Print paths to build dirs in current profile."},
                                                               {desc, "Print paths to build dirs in current profile."},
                                                               {opts, path_opts(State)}])),

    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    %% retrieve apps to filter by for other args
    Apps = filter_apps(RawOpts, State),
    %% remove apps and separator opts from options
    Paths = lists:filter(fun({app, _}) -> false; ({separator, _}) -> false; (_) -> true end, RawOpts),
    %% if no paths requested in opts print the base_dir instead
    P = case Paths of [] -> [{ebin, true}]; _ -> Paths end,
    paths(P, Apps, State, []),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

filter_apps(RawOpts, State) ->
    RawApps = proplists:get_all_values(app, RawOpts),
    Apps = lists:foldl(fun(String, Acc) -> rebar_string:lexemes(String, ",") ++ Acc end, [], RawApps),
    case Apps of
        [] ->
            ProjectDeps = project_deps(State),
            ProjectApps = rebar_state:project_apps(State),
            lists:map(fun(A) -> binary_to_list(rebar_app_info:name(A)) end, ProjectApps) ++ ProjectDeps;
        _  -> Apps
    end.


paths([], _, State, Acc) -> print_paths_if_exist(lists:reverse(Acc), State);
paths([{base, true}|Rest], Apps, State, Acc) ->
    paths(Rest, Apps, State, [base_dir(State)|Acc]);
paths([{bin, true}|Rest], Apps, State, Acc) ->
    paths(Rest, Apps, State, [bin_dir(State)|Acc]);
paths([{ebin, true}|Rest], Apps, State, Acc) ->
    paths(Rest, Apps, State, ebin_dirs(Apps, State) ++ Acc);
paths([{lib, true}|Rest], Apps, State, Acc) ->
    paths(Rest, Apps, State, [lib_dir(State)|Acc]);
paths([{priv, true}|Rest], Apps, State, Acc) ->
    paths(Rest, Apps, State, priv_dirs(Apps, State) ++ Acc);
paths([{src, true}|Rest], Apps, State, Acc) ->
    paths(Rest, Apps, State, src_dirs(Apps, State) ++ Acc);
paths([{rel, true}|Rest], Apps, State, Acc) ->
    paths(Rest, Apps, State, [rel_dir(State)|Acc]).

base_dir(State) -> io_lib:format("~ts", [rebar_dir:base_dir(State)]).
bin_dir(State)  -> io_lib:format("~ts/bin", [rebar_dir:base_dir(State)]).
lib_dir(State)  -> io_lib:format("~ts", [rebar_dir:deps_dir(State)]).
rel_dir(State)  -> io_lib:format("~ts/rel", [rebar_dir:base_dir(State)]).

ebin_dirs(Apps, State) ->
    lists:flatmap(fun(App) -> [io_lib:format("~ts/~ts/ebin", [rebar_dir:checkouts_out_dir(State), App]),
                               io_lib:format("~ts/~ts/apps/~ts/ebin", [rebar_dir:deps_dir(State), App, App]),
                               io_lib:format("~ts/~ts/ebin", [rebar_dir:deps_dir(State), App]) ] end, Apps).
priv_dirs(Apps, State) ->
    lists:flatmap(fun(App) -> [io_lib:format("~ts/~ts/priv", [rebar_dir:checkouts_out_dir(State), App]),
                               io_lib:format("~ts/~ts/apps/~ts/priv", [rebar_dir:deps_dir(State), App, App]),
                               io_lib:format("~ts/~ts/priv", [rebar_dir:deps_dir(State), App]) ] end, Apps).
src_dirs(Apps, State) ->
    lists:flatmap(fun(App) -> [io_lib:format("~ts/~ts/src", [rebar_dir:checkouts_out_dir(State), App]),
                               io_lib:format("~ts/~ts/apps/~ts/src", [rebar_dir:deps_dir(State), App, App]),
                               io_lib:format("~ts/~ts/src", [rebar_dir:deps_dir(State), App]) ] end, Apps).

print_paths_if_exist(Paths, State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    Sep = proplists:get_value(separator, RawOpts, " "),
    RealPaths = lists:filter(fun(P) -> ec_file:is_dir(P) end, Paths),
    io:format("~ts", [rebar_string:join(RealPaths, Sep)]).

project_deps(State) ->
    Profiles = rebar_state:current_profiles(State),
    DepList = lists:foldl(fun(Profile, Acc) -> rebar_state:get(State, {deps, Profile}, []) ++ Acc end, [], Profiles),
    LockList = lists:foldl(fun(Profile, Acc) -> rebar_state:get(State, {locks, Profile}, []) ++ Acc end, [], Profiles),
    Deps = [normalize(name(Dep)) || Dep <- DepList++LockList],
    lists:usort(Deps).

name(App) when is_tuple(App) -> element(1, App);
name(Name) when is_binary(Name); is_list(Name); is_atom(Name) -> Name.

normalize(AppName) when is_list(AppName) -> AppName;
normalize(AppName) when is_atom(AppName) -> atom_to_list(AppName);
normalize(AppName) when is_binary(AppName) -> binary_to_list(AppName).

path_opts(_State) ->
    [{app, undefined, "app", string, help(app)},
     {base, undefined, "base", boolean, help(base)},
     {bin, undefined, "bin", boolean, help(bin)},
     {ebin, undefined, "ebin", boolean, help(ebin)},
     {lib, undefined, "lib", boolean, help(lib)},
     {priv, undefined, "priv", boolean, help(priv)},
     {separator, $s, "separator", string, help(separator)},
     {src, undefined, "src", boolean, help(src)},
     {rel, undefined, "rel", boolean, help(rel)}].

help(app)       -> "Comma separated list of applications to return paths for.";
help(base)      -> "Return the `base' path of the current profile.";
help(bin)       -> "Return the `bin' path of the current profile.";
help(ebin)      -> "Return all `ebin' paths of the current profile's applications.";
help(lib)       -> "Return the `lib' path of the current profile.";
help(priv)      -> "Return the `priv' path of the current profile's applications.";
help(separator) -> "In case of multiple return paths, the separator character to use to join them.";
help(src)       -> "Return the `src' path of the current profile's applications.";
help(rel)       -> "Return the `rel' path of the current profile.".
