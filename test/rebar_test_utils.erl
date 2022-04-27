-module(rebar_test_utils).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([init_rebar_state/1, init_rebar_state/2, run_and_check/3, run_and_check/4,
         check_results/3, check_results/4]).
-export([expand_deps/2, flat_deps/1, top_level_deps/1]).
-export([create_app/4, create_plugin/4, create_eunit_app/4, create_empty_app/4,
         create_config/2, create_config/3, package_app/4]).
-export([create_random_name/1, create_random_vsn/0, write_src_file/2,
         random_element/1]).

%%%%%%%%%%%%%%
%%% Public %%%
%%%%%%%%%%%%%%

%% @doc {@see init_rebar_state/2}
init_rebar_state(Config) -> init_rebar_state(Config, "apps_dir1_").

%% @doc Takes a common test config and a name (string) and sets up
%% a basic OTP app directory with a pre-configured rebar state to
%% run tests with.
init_rebar_state(Config, Name) ->
    application:load(rebar),
    DataDir = ?config(priv_dir, Config),
    AppsDir = filename:join([DataDir, create_random_name(Name)]),
    CheckoutsDir = filename:join([AppsDir, "_checkouts"]),
    ok = ec_file:mkdir_p(AppsDir),
    ok = ec_file:mkdir_p(CheckoutsDir),
    Verbosity = rebar3:log_level(),
    rebar_log:init(command_line, Verbosity),
    GlobalDir = filename:join([DataDir, "cache"]),
    Repos = proplists:get_value(repos, Config, []),
    State = rebar_state:new([{base_dir, filename:join([AppsDir, "_build"])}
                            ,{global_rebar_dir, GlobalDir}
                            ,{hex, [{repos, [#{name => R} || R <- Repos]}]}
                            ,{root_dir, AppsDir}]),
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, State} | Config].

%% @doc Takes common test config, a rebar config ([] if empty), a command to
%% run ("install_deps", "compile", etc.), and a list of expected applications
%% and/or dependencies to be present, and verifies whether they are all in
%% place.
%%
%% The expectation list takes elements of the form:
%% - `{app, Name :: string()}': checks that the app is properly built.
%% - `{dep, Name :: string()}': checks that the dependency has been fetched.
%%   Ignores the build status of the dependency.
%% - `{dep, Name :: string(), Vsn :: string()}': checks that the dependency
%%   has been fetched, and that a given version has been chosen. Useful to
%%   test for conflict resolution. Also ignores the build status of the
%%   dependency.
%%
%% This function assumes `init_rebar_state/1-2' has run before, in order to
%% fetch the `apps' and `state' values from the CT config.
run_and_check(Config, RebarConfig, Command, Expect) ->
    %% Assumes init_rebar_state has run first
    AppDir = ?config(apps, Config),
    State = ?config(state, Config),
    try
        Res = rebar3:run(rebar_state:new(State, RebarConfig, AppDir), Command),
        case Expect of
            {error, Reason} ->
                ?assertEqual({error, Reason}, Res);
            {ok, Expected} ->
                {ok, _} = Res,
                check_results(AppDir, Expected, "*"),
                Res;
            {ok, Expected, ProfileRun} ->
                {ok, _} = Res,
                check_results(AppDir, Expected, ProfileRun),
                Res;
            return ->
                Res
        end
    catch
        rebar_abort when Expect =:= rebar_abort -> rebar_abort
    end.

run_and_check(Config, Command, Expect) ->
    %% Assumes init_rebar_state has run first
    AppDir = ?config(apps, Config),
    {ok, Cwd} = file:get_cwd(),
    try
        ok = file:set_cwd(AppDir),
        Res = rebar3:run(Command),
        case Expect of
            {error, Reason} ->
                ?assertEqual({error, Reason}, Res);
            {ok, Expected} ->
                {ok, _} = Res,
                check_results(AppDir, Expected, "*"),
                Res;
            {ok, Expected, ProfileRun} ->
                {ok, _} = Res,
                check_results(AppDir, Expected, ProfileRun),
                Res;
            return ->
                Res
        end
    catch
        rebar_abort when Expect =:= rebar_abort -> rebar_abort
    after
        ok = file:set_cwd(Cwd)
    end.

%% @doc Creates a dummy application including:
%% - src/<file>.erl
%% - src/<file>.app.src
%% And returns a `rebar_app_info' object.
create_app(AppDir, Name, Vsn, Deps) ->
    write_src_file(AppDir, Name ++ ".erl"),
    write_src_file(AppDir, "not_a_real_src_" ++ Name ++ ".erl"),
    write_app_src_file(AppDir, Name, Vsn, Deps),
    rebar_app_info:new(Name, Vsn, AppDir, Deps).

%% @doc Creates a dummy plugin including:
%% - src/<file>.erl
%% - src/<file>.app.src
%% And returns a `rebar_app_info' object.
create_plugin(AppDir, Name, Vsn, Deps) ->
    write_plugin_file(AppDir, Name ++ ".erl"),
    write_src_file(AppDir, "not_a_real_src_" ++ Name ++ ".erl"),
    write_app_src_file(AppDir, Name, Vsn, Deps),
    rebar_app_info:new(Name, Vsn, AppDir, Deps).

%% @doc Creates a dummy application including:
%% - src/<file>.erl
%% - src/<file>.app.src
%% - test/<file>_tests.erl
%% And returns a `rebar_app_info' object.
create_eunit_app(AppDir, Name, Vsn, Deps) ->
    write_eunitized_src_file(AppDir, Name),
    write_eunit_suite_file(AppDir, Name),
    write_app_src_file(AppDir, Name, Vsn, Deps),
    rebar_app_info:new(Name, Vsn, AppDir, Deps).

%% @doc Creates a dummy application including:
%% - ebin/<file>.app
%% And returns a `rebar_app_info' object.
create_empty_app(AppDir, Name, Vsn, Deps) ->
    write_app_file(AppDir, Name, Vsn, Deps),
    rebar_app_info:new(Name, Vsn, AppDir, Deps).

%% @doc Creates a rebar.config file. The function accepts a list of terms,
%% each of which will be dumped as a consult file. For example, the list
%% `[a, b, c]' will return the consult file `a. b. c.'.
create_config(AppDir, Contents) ->
    ConfFilename = filename:join([AppDir, "rebar.config"]),
    create_config(AppDir, ConfFilename, Contents).

create_config(_AppDir, ConfFilename, Contents) ->
    ok = filelib:ensure_dir(ConfFilename),
    Config = lists:flatten([io_lib:fwrite("~p.~n", [Term]) || Term <- Contents]),
    ok = ec_file:write(ConfFilename, Config),
    ConfFilename.

%% @doc Util to create a random variation of a given name.
create_random_name(Name) ->
    Name ++ erlang:integer_to_list(rand:uniform(1000000)).

%% @doc Util to create a random variation of a given version.
create_random_vsn() ->
    lists:flatten([erlang:integer_to_list(rand:uniform(100)),
                   ".", erlang:integer_to_list(rand:uniform(100)),
                   ".", erlang:integer_to_list(rand:uniform(100))]).

expand_deps(_, []) -> [];
expand_deps(git_subdir, [{Name, Deps} | Rest]) ->
    Dep = {Name, {git_subdir, "https://example.org/user/"++Name++".git", {branch, "main"}, filename:join("appsubdir", Name)}},
    [{Dep, expand_deps(git_subdir, Deps)} | expand_deps(git_subdir, Rest)];
expand_deps(git_subdir, [{Name, Vsn, Deps} | Rest]) ->
    Dep = {Name, Vsn, {git_subdir, "https://example.org/user/"++Name++".git", {tag, Vsn}, filename:join("appsubdir", Name)}},
    [{Dep, expand_deps(git_subdir, Deps)} | expand_deps(git_subdir, Rest)];
expand_deps(git, [{Name, Deps} | Rest]) ->
    Dep = {Name, ".*", {git, "https://example.org/user/"++Name++".git", "main"}},
    [{Dep, expand_deps(git, Deps)} | expand_deps(git, Rest)];
expand_deps(git, [{Name, Vsn, Deps} | Rest]) ->
    Dep = {Name, Vsn, {git, "https://example.org/user/"++Name++".git", {tag, Vsn}}},
    [{Dep, expand_deps(git, Deps)} | expand_deps(git, Rest)];
expand_deps(pkg, [{Name, Deps} | Rest]) ->
    Dep = {pkg, Name, "0.0.0", undefined, undefined},
    [{Dep, expand_deps(pkg, Deps)} | expand_deps(pkg, Rest)];
expand_deps(pkg, [{Name, Vsn, Deps} | Rest]) ->
    Dep = {pkg, Name, Vsn, undefined, undefined},
    [{Dep, expand_deps(pkg, Deps)} | expand_deps(pkg, Rest)];
expand_deps(mixed, [{Name, Deps} | Rest]) ->
    Dep = if hd(Name) >= $a, hd(Name) =< $z ->
            {pkg, rebar_string:uppercase(Name), "0.0.0", undefined, undefined}
           ; hd(Name) >= $A, hd(Name) =< $Z ->
            {Name, ".*", {git, "https://example.org/user/"++Name++".git", "main"}}
    end,
    [{Dep, expand_deps(mixed, Deps)} | expand_deps(mixed, Rest)];
expand_deps(mixed, [{Name, Vsn, Deps} | Rest]) ->
    Dep = if hd(Name) >= $a, hd(Name) =< $z ->
            {pkg, rebar_string:uppercase(Name), Vsn, undefined, undefined}
           ; hd(Name) >= $A, hd(Name) =< $Z ->
            {Name, Vsn, {git, "https://example.org/user/"++Name++".git", {tag, Vsn}}}
    end,
    [{Dep, expand_deps(mixed, Deps)} | expand_deps(mixed, Rest)].

%% Source deps can depend on both source and package dependencies;
%% package deps can only depend on package deps.
%% For things to work we have to go down the dep tree and find all
%% lineages of pkg deps and return them, whereas the source deps
%% can be left as is.
flat_deps(Deps) -> flat_deps(Deps, [], []).

flat_deps([], Src, Pkg) -> {Src, Pkg};
flat_deps([{{pkg, Name, Vsn, undefined, undefined}, PkgDeps} | Rest], Src, Pkg) ->
    Current = {{iolist_to_binary(Name), iolist_to_binary(Vsn)},
               top_level_deps(PkgDeps)},
    {[], FlatPkgDeps} = flat_deps(PkgDeps),
    flat_deps(Rest,
              Src,
              Pkg ++ [Current | FlatPkgDeps]);
flat_deps([{{Name,_Vsn,Ref}, Deps} | Rest], Src, Pkg) ->
    Current = {{Name,vsn_from_ref(Ref)}, top_level_deps(Deps)},
    {FlatDeps, FlatPkgDeps} = flat_deps(Deps),
    flat_deps(Rest,
              Src ++ [Current | FlatDeps],
              Pkg ++ FlatPkgDeps);
flat_deps([{{Name,Ref}, Deps} | Rest], Src, Pkg) ->
    Current = {{Name,vsn_from_ref(Ref)}, top_level_deps(Deps)},
    {FlatDeps, FlatPkgDeps} = flat_deps(Deps),
    flat_deps(Rest,
              Src ++ [Current | FlatDeps],
              Pkg ++ FlatPkgDeps).

vsn_from_ref({git, _, {_, Vsn}}) -> Vsn;
vsn_from_ref({git, _, Vsn}) -> Vsn;
vsn_from_ref({git_subdir, _, {_, Vsn}, _}) -> Vsn;
vsn_from_ref({git_subdir, _, Vsn, _}) -> Vsn.

top_level_deps([]) -> [];
top_level_deps([{{pkg, Name, Vsn, undefined, undefined}, _} | Deps]) ->
    [{list_to_atom(Name), Vsn} | top_level_deps(Deps)];
top_level_deps([{{Name, Vsn, Ref}, _} | Deps]) ->
    [{list_to_atom(Name), Vsn, Ref} | top_level_deps(Deps)].

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
check_results(AppDir, Expected, ProfileRun) ->
    State = rebar_state:new(),
    check_results(AppDir, Expected, ProfileRun, State).

check_results(AppDir, Expected, ProfileRun, State) ->
    BuildDirs = filelib:wildcard(filename:join([AppDir, "_build", ProfileRun, "lib", "*"])),
    BuildSubDirs = [D || D <- filelib:wildcard(filename:join([AppDir, "_build", ProfileRun, "lib", "*", "*", "*"])),
                         filelib:is_dir(D)],
    PluginDirs = filelib:wildcard(filename:join([AppDir, "_build", ProfileRun, "plugins", "*"])),
    GlobalPluginDirs = filelib:wildcard(filename:join([AppDir, "global", "plugins", "*"])),
    CheckoutsDirs = filelib:wildcard(filename:join([AppDir, "_build", ProfileRun, "checkouts", "*"])),
    LockFile = filename:join([AppDir, "rebar.lock"]),
    Locks = lists:flatten(rebar_config:consult_lock_file(LockFile)),

    InvalidApps = rebar_app_discover:find_apps(BuildDirs, invalid, State),
    ValidApps = rebar_app_discover:find_apps(BuildDirs, valid, State),

    InvalidDepsNames = [{ec_cnv:to_list(rebar_app_info:name(App)), App} || App <- InvalidApps],
    ValidDepsNames = [{ec_cnv:to_list(rebar_app_info:name(App)), App} || App <- ValidApps],

    Deps = rebar_app_discover:find_apps(BuildDirs, all, State),
    SubDeps = rebar_app_discover:find_apps(BuildSubDirs, all, State),
    DepsNames = [{ec_cnv:to_list(rebar_app_info:name(App)), App} || App <- Deps],
    SubDirDepsNames = [{ec_cnv:to_list(rebar_app_info:name(App)), App} || App <- SubDeps],
    Checkouts = rebar_app_discover:find_apps(CheckoutsDirs, all, State),
    CheckoutsNames = [{ec_cnv:to_list(rebar_app_info:name(App)), App} || App <- Checkouts],
    Plugins = rebar_app_discover:find_apps(PluginDirs, all, State),
    PluginsNames = [{ec_cnv:to_list(rebar_app_info:name(App)), App} || App <- Plugins],
    GlobalPlugins = rebar_app_discover:find_apps(GlobalPluginDirs, all, State),
    GlobalPluginsNames = [{ec_cnv:to_list(rebar_app_info:name(App)), App} || App <- GlobalPlugins],

    lists:foreach(
        fun({app, Name}) ->
                ct:pal("App Name: ~p", [Name]),
                case lists:keyfind(Name, 1, DepsNames) of
                    false ->
                        error({app_not_found, Name});
                    {Name, _App} ->
                        ok
                end
        ; ({app, Name, invalid}) ->
                ct:pal("Invalid Name: ~p", [Name]),
                case lists:keyfind(Name, 1, InvalidDepsNames) of
                    false ->
                        error({app_not_found, Name});
                    {Name, _App} ->
                        ok
                end
        ; ({app, Name, valid}) ->
                ct:pal("Valid Name: ~p", [Name]),
                case lists:keyfind(Name, 1, ValidDepsNames) of
                    false ->
                        error({app_not_found, Name});
                    {Name, _App} ->
                        ok
                end
        ; ({dep_not_exist, Name}) ->
                ct:pal("Dep Not Exist Name: ~p", [Name]),
                case lists:keyfind(Name, 1, DepsNames) of
                    false ->
                        ok;
                    {Name, _App} ->
                        error({app_found, Name})
                end
        ; ({app_not_exist, Name}) ->
                ct:pal("App Not Exist Name: ~p", [Name]),
                case lists:keyfind(Name, 1, DepsNames) of
                    false ->
                        ok;
                    {Name, _App} ->
                        error({app_found, Name})
                end
        ;  ({checkout, Name}) ->
                ct:pal("Checkout Name: ~p", [Name]),
                ?assertNotEqual(false, lists:keyfind(Name, 1, CheckoutsNames))
        ;  ({dep, Name}) ->
                ct:pal("Dep Name: ~p", [Name]),
                ?assertNotEqual(false, lists:keyfind(Name, 1, DepsNames))
        ;  ({dep, Name, Vsn}) ->
                ct:pal("Dep Name: ~p, Vsn: ~p", [Name, Vsn]),
                case lists:keyfind(Name, 1, DepsNames) of
                    false ->
                        error({dep_not_found, Name});
                    {Name, App} ->
                        ?assertEqual(iolist_to_binary(Vsn),
                                     iolist_to_binary(rebar_app_info:original_vsn(App)))
                end
        ;  ({subdir_dep, Name}) ->
                ct:pal("Subdir Dep Name: ~p", [Name]),
                ?assertNotEqual(false, lists:keyfind(Name, 1, SubDirDepsNames))
        ;  ({subdir_dep, Name, Vsn}) ->
                ct:pal("Subdir Dep Name: ~p, Vsn: ~p", [Name, Vsn]),
                case lists:keyfind(Name, 1, SubDirDepsNames) of
                    false ->
                        error({dep_not_found, Name});
                    {Name, App} ->
                        ?assertEqual(iolist_to_binary(Vsn),
                                     iolist_to_binary(rebar_app_info:original_vsn(App)))
                end
        ;  ({plugin, Name}) ->
                ct:pal("Plugin Name: ~p", [Name]),
                ?assertNotEqual(false, lists:keyfind(Name, 1, PluginsNames))
        ;  ({plugin, Name, Vsn}) ->
                ct:pal("Plugin Name: ~p, Vsn: ~p", [Name, Vsn]),
                case lists:keyfind(Name, 1, PluginsNames) of
                    false ->
                        error({plugin_not_found, Name});
                    {Name, App} ->
                        ?assertEqual(iolist_to_binary(Vsn),
                                     iolist_to_binary(rebar_app_info:original_vsn(App)))
                end
        ; ({plugin_not_exist, Name}) ->
                ct:pal("Plugin Not Exist Name: ~p", [Name]),
                case lists:keyfind(Name, 1, PluginsNames) of
                    false ->
                        ok;
                    {Name, _App} ->
                        error({plugin_found, Name})
                end
        ;  ({global_plugin, Name}) ->
                ct:pal("Global Plugin Name: ~p", [Name]),
                ?assertNotEqual(false, lists:keyfind(Name, 1, GlobalPluginsNames))
        ;  ({global_plugin, Name, Vsn}) ->
                ct:pal("Global Plugin Name: ~p, Vsn: ~p", [Name, Vsn]),
                case lists:keyfind(Name, 1, GlobalPluginsNames) of
                    false ->
                        error({global_plugin_not_found, Name});
                    {Name, App} ->
                        ?assertEqual(iolist_to_binary(Vsn),
                                     iolist_to_binary(rebar_app_info:original_vsn(App)))
                end
        ;  ({lock, Name}) ->
                ct:pal("Lock Name: ~p", [Name]),
                ?assertNotEqual(false, lists:keyfind(iolist_to_binary(Name), 1, Locks))
        ;  ({lock, Name, Vsn}) ->
                ct:pal("Lock Name: ~p, Vsn: ~p", [Name, Vsn]),
                case lists:keyfind(iolist_to_binary(Name), 1, Locks) of
                    false ->
                        error({lock_not_found, Name});
                    {_LockName, {pkg, _, LockVsn, _InnerHash, OuterHash}, _} ->
                        ?assertEqual(iolist_to_binary(Vsn),
                                     iolist_to_binary(LockVsn)),
                        ?assertNotEqual(undefined, OuterHash);
                    {_LockName, {_, _, {ref, LockVsn}}, _} ->
                        ?assertEqual(iolist_to_binary(Vsn),
                                     iolist_to_binary(LockVsn))
                end
        ;  ({lock, pkg, Name, Vsn}) ->
                ct:pal("Pkg Lock Name: ~p, Vsn: ~p", [Name, Vsn]),
                case lists:keyfind(iolist_to_binary(Name), 1, Locks) of
                    false ->
                        error({lock_not_found, Name});
                    {_LockName, {pkg, _, LockVsn, _InnerHash, OuterHash}, _} ->
                        ?assertEqual(iolist_to_binary(Vsn),
                                     iolist_to_binary(LockVsn)),
                        ?assertNotEqual(undefined, OuterHash);
                    {_LockName, {_, _, {ref, LockVsn}}, _} ->
                        error({source_lock, {Name, LockVsn}})
                end
        ;  ({lock, src, Name, Vsn}) ->
                ct:pal("Src Lock Name: ~p, Vsn: ~p", [Name, Vsn]),
                case lists:keyfind(iolist_to_binary(Name), 1, Locks) of
                    false ->
                        error({lock_not_found, Name});
                    {_LockName, {pkg, _, LockVsn, _}, _} ->
                        error({pkg_lock, {Name, LockVsn}});
                    {_LockName, {_, _, {ref, LockVsn}}, _} ->
                        ?assertEqual(iolist_to_binary(Vsn),
                                     iolist_to_binary(LockVsn))
                end
        ;  ({release, Name, Vsn, ExpectedDevMode}) ->
                ct:pal("Release: ~p-~ts", [Name, Vsn]),
                {ok, Cwd} = file:get_cwd(),
                try
                    file:set_cwd(AppDir),
                    [ReleaseDir] = filelib:wildcard(filename:join([AppDir, "_build", "*", "rel"])),

                    LibDir = filename:join([ReleaseDir, Name, "lib"]),
                    {ok, RelLibs} = rebar_utils:list_dir(LibDir),
                    IsSymLinkFun =
                        fun(X) ->
                                ec_file:is_symlink(filename:join(LibDir, X))
                        end,
                    DevMode = lists:all(IsSymLinkFun, RelLibs),
                    ?assertEqual(ExpectedDevMode, DevMode),
                    ?assert(ec_file:exists(filename:join([ReleaseDir, Name, "releases", Vsn]))),

                    %% throws not_found if it doesn't exist
                    ok
                catch
                    _ ->
                        ct:fail(release_not_found)
                after
                    file:set_cwd(Cwd)
                end
        ;  ({tar, Name, Vsn}) ->
                ct:pal("Tarball: ~ts-~ts", [Name, Vsn]),
                Tarball = filename:join([AppDir, "_build", "rel", Name, Name++"-"++Vsn++".tar.gz"]),
                ?assertNotEqual([], filelib:is_file(Tarball))
        ;  ({file, Filename}) ->
                ct:pal("Filename: ~ts", [Filename]),
                ?assert(filelib:is_file(Filename))
        ;  ({dir, Dirname}) ->
                ct:pal("Directory: ~ts", [Dirname]),
                ?assert(filelib:is_dir(Dirname))
        end, Expected).

write_plugin_file(Dir, Name) ->
    Erl = filename:join([Dir, "src", Name]),
    ok = filelib:ensure_dir(Erl),
    ok = ec_file:write(Erl, plugin_src_file(Name)).

write_src_file(Dir, Name) ->
    Erl = filename:join([Dir, "src", Name]),
    ok = filelib:ensure_dir(Erl),
    ok = ec_file:write(Erl, erl_src_file(Name)).

write_eunitized_src_file(Dir, Name) ->
    Erl = filename:join([Dir, "src", "not_a_real_src_" ++ Name ++ ".erl"]),
    ok = filelib:ensure_dir(Erl),
    ok = ec_file:write(Erl, erl_eunitized_src_file("not_a_real_src_" ++ Name ++ ".erl")).

write_eunit_suite_file(Dir, Name) ->
    Erl = filename:join([Dir, "test", "not_a_real_src_" ++ Name ++ "_tests.erl"]),
    ok = filelib:ensure_dir(Erl),
    ok = ec_file:write(Erl, erl_eunit_suite_file("not_a_real_src_" ++ Name ++ ".erl")).

write_app_file(Dir, Name, Version, Deps) ->
    Filename = filename:join([Dir, "ebin", Name ++ ".app"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, get_app_metadata(ec_cnv:to_list(Name), Version, Deps)).

write_app_src_file(Dir, Name, Version, Deps) ->
    Filename = filename:join([Dir, "src", Name ++ ".app.src"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, get_app_metadata(ec_cnv:to_list(Name), Version, Deps)).

erl_src_file(Name) ->
    io_lib:format("-module('~s').\n"
                  "-export([main/0]).\n"
                  "main() -> ok.\n", [filename:basename(Name, ".erl")]).

plugin_src_file(Name) ->
    io_lib:format("-module('~s').\n"
                 "-export([init/1, do/1]).\n"
                 "init(State) -> \n"
                 "Provider = providers:create([\n"
                 "{name, '~s'},\n"
                 "{module, '~s'}\n"
                 "]),\n"
                 "{ok, rebar_state:add_provider(State, Provider)}.\n"
                 "do(State) -> {ok, State}.\n", [filename:basename(Name, ".erl"),
                                                 filename:basename(Name, ".erl"),
                                                 filename:basename(Name, ".erl")]).

erl_eunitized_src_file(Name) ->
    io_lib:format("-module('~s').\n"
                  "-export([main/0]).\n"
                  "main() -> ok.\n"
                  "-ifdef(TEST).\n"
                  "-include_lib(\"eunit/include/eunit.hrl\").\n"
                  "some_test_() -> ?_assertEqual(ok, main()).\n"
                  "-endif.\n", [filename:basename(Name, ".erl")]).

erl_eunit_suite_file(Name) ->
    BaseName = filename:basename(Name, ".erl"),
    io_lib:format("-module('~s_tests').\n"
                  "-compile(export_all).\n"
                  "-ifndef(some_define).\n"
                  "-define(some_define, false).\n"
                  "-endif.\n"
                  "-ifdef(TEST).\n"
                  "-include_lib(\"eunit/include/eunit.hrl\").\n"
                  "some_test_() -> ?_assertEqual(ok, ~s:main()).\n"
                  "define_test_() -> ?_assertEqual(true, ?some_define).\n"
                  "-endif.\n", [BaseName, BaseName]).

get_app_metadata(Name, Vsn, Deps) ->
    {application, erlang:list_to_atom(Name),
     [{description, ""},
      {vsn, Vsn},
      {modules, []},
      {included_applications, []},
      {registered, []},
      {applications, Deps}]}.

package_app(AppDir, DestDir, PkgName, PkgVsn) ->
    AppSrc = filename:join(AppDir, "src"),
    {ok, Fs} = rebar_utils:list_dir(AppSrc),
    Files = lists:zip([filename:join("src", F) || F <- Fs], [filename:join(AppSrc,F) || F <- Fs]),
    Metadata = #{<<"app">> => list_to_binary(PkgName),
                 <<"version">> => list_to_binary(PkgVsn)},
    {ok, #{tarball := Tarball, outer_checksum := <<Checksum:256/big-unsigned-integer>>}} = r3_hex_tarball:create(Metadata, Files),

    Name = PkgName++"-"++PkgVsn++".tar",
    Archive = filename:join(DestDir, Name),
    file:write_file(Archive, Tarball),

    <<E:128/big-unsigned-integer>> = crypto:hash(md5, Tarball),

    Checksum1 = list_to_binary(
                  rebar_string:uppercase(
                    lists:flatten(io_lib:format("~64.16.0b", [Checksum])))),
    {Checksum1, E}.

random_element(Repos) ->
    Index = rand:uniform(length(Repos)),
    lists:nth(Index, Repos).
