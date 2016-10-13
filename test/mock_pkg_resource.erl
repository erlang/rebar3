%%% Mock a package resource and create an app magically for each URL submitted.
-module(mock_pkg_resource).
-export([mock/0, mock/1, unmock/0]).
-define(MOD, rebar_pkg_resource).

%%%%%%%%%%%%%%%%%
%%% Interface %%%
%%%%%%%%%%%%%%%%%

%% @doc same as `mock([])'.
mock() -> mock([]).

%% @doc Mocks a fake version of the git resource fetcher that creates
%% empty applications magically, rather than trying to download them.
%% Specific config options are explained in each of the private functions.
-spec mock(Opts) -> ok when
    Opts :: [Option],
    Option :: {upgrade, [App]}
            | {cache_dir, string()}
            | {default_vsn, Vsn}
            | {override_vsn, [{App, Vsn}]}
            | {not_in_index, [{App, Vsn}]}
            | {pkgdeps, [{{App,Vsn}, [Dep]}]},
    App :: string(),
    Dep :: {App, string(), {pkg, App, Vsn, Hash}},
    Vsn :: string(),
    Hash :: string() | undefined.
mock(Opts) ->
    meck:new(?MOD, [no_link]),
    mock_lock(Opts),
    mock_update(Opts),
    mock_vsn(Opts),
    mock_download(Opts),
    mock_pkg_index(Opts),
    ok.

unmock() ->
    meck:unload(?MOD),
    meck:unload(rebar_packages).

%%%%%%%%%%%%%%%
%%% Private %%%
%%%%%%%%%%%%%%%

%% @doc creates values for a lock file.
mock_lock(_) ->
    meck:expect(?MOD, lock, fun(_AppDir, Source) -> Source end).

%% @doc The config passed to the `mock/2' function can specify which apps
%% should be updated on a per-name basis: `{update, ["App1", "App3"]}'.
mock_update(Opts) ->
    ToUpdate = proplists:get_value(upgrade, Opts, []),
    meck:expect(
        ?MOD, needs_update,
        fun(_Dir, {pkg, App, _Vsn, _Hash}) ->
            lists:member(binary_to_list(App), ToUpdate)
        end).

%% @doc Replicated an unsupported call.
mock_vsn(_Opts) ->
    meck:expect(
        ?MOD, make_vsn,
        fun(_Dir) ->
            {error, "Replacing version of type pkg not supported."}
        end).

%% @doc For each app to download, create a dummy app on disk instead.
%% The configuration for this one (passed in from `mock/1') includes:
%%
%% - Specify a version with `{pkg, _, Vsn, _}'
%% - Dependencies for each application must be passed of the form:
%%   `{pkgdeps, [{"app1", [{app2, ".*", {pkg, ...}}]}]}' -- basically
%%   the `pkgdeps' option takes a key/value list of terms to output directly
%%   into a `rebar.config' file to describe dependencies.
mock_download(Opts) ->
    Deps = proplists:get_value(pkgdeps, Opts, []),
    Config = proplists:get_value(config, Opts, []),
    meck:expect(
        ?MOD, download,
        fun (Dir, {pkg, AppBin, Vsn, _}, _) ->
            App = binary_to_list(AppBin),
            filelib:ensure_dir(Dir),
            AppDeps = proplists:get_value({App,Vsn}, Deps, []),
            {ok, AppInfo} = rebar_test_utils:create_app(
                Dir, App, binary_to_list(Vsn),
                [kernel, stdlib] ++ [element(1,D) || D  <- AppDeps]
            ),
            rebar_test_utils:create_config(Dir, [{deps, AppDeps}]++Config),
            TarApp = App++"-"++binary_to_list(Vsn)++".tar",
            Tarball = filename:join([Dir, TarApp]),
            Contents = filename:join([Dir, "contents.tar.gz"]),
            Files = all_files(rebar_app_info:dir(AppInfo)),
            ok = erl_tar:create(Contents,
                                archive_names(Dir, App, Vsn, Files),
                                [compressed]),
            ok = erl_tar:create(Tarball,
                                [{"contents.tar.gz", Contents}],
                                []),
            Cache = proplists:get_value(cache_dir, Opts, filename:join(Dir,"cache")),
            Cached = filename:join([Cache, TarApp]),
            filelib:ensure_dir(Cached),
            rebar_file_utils:mv(Tarball, Cached),
            {ok, true}
        end).

%% @doc On top of the pkg resource mocking, we need to mock the package
%% index.
%%
%% A special option, `{not_in_index, [App]}' lets the index leave out
%% specific applications otherwise listed.
mock_pkg_index(Opts) ->
    Deps = proplists:get_value(pkgdeps, Opts, []),
    Skip = proplists:get_value(not_in_index, Opts, []),
    %% Dict: {App, Vsn}: [{<<"link">>, <<>>}, {<<"deps">>, []}]
    %% Index: all apps and deps in the index

    Dict = find_parts(Deps, Skip),
    meck:new(rebar_packages, [passthrough, no_link]),
    meck:expect(rebar_packages, packages,
                fun(_State) -> to_index(Deps, Dict) end),
    meck:expect(rebar_packages, verify_table,
                fun(_State) -> to_index(Deps, Dict), true end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

all_files(Dir) ->
    filelib:wildcard(filename:join([Dir, "**"])).

archive_names(Dir, _App, _Vsn, Files) ->
    [{(F -- Dir) -- "/", F} || F <- Files].

find_parts(Apps, Skip) -> find_parts(Apps, Skip, dict:new()).

find_parts([], _, Acc) -> Acc;
find_parts([{AppName, Deps}|Rest], Skip, Acc) ->
    case lists:member(AppName, Skip) orelse dict:is_key(AppName,Acc) of
        true -> find_parts(Rest, Skip, Acc);
        false ->
            AccNew = dict:store(AppName,
                                Deps,
                                Acc),
            find_parts(Rest, Skip, AccNew)
    end.

to_index(AllDeps, Dict) ->
    catch ets:delete(package_index),
    ets:new(package_index, [named_table, public]),
    dict:fold(
      fun(K, Deps, _) ->
              DepsList = [{DKB, {pkg, DKB, DVB, undefined}}
                          || {DK, DV} <- Deps,
                             DKB <- [ec_cnv:to_binary(DK)],
                             DVB <- [ec_cnv:to_binary(DV)]],
              ets:insert(package_index, {K, DepsList, <<"checksum">>})
      end, ok, Dict),
    ets:insert(package_index, {package_index_version, 3}),
    lists:foreach(fun({{Name, Vsn}, _}) ->
                          case ets:lookup(package_index,  ec_cnv:to_binary(Name)) of
                              [{_, Vsns}] ->
                                  ets:insert(package_index, {ec_cnv:to_binary(Name), [ec_cnv:to_binary(Vsn) | Vsns]});
                              _ ->
                                  ets:insert(package_index, {ec_cnv:to_binary(Name), [ec_cnv:to_binary(Vsn)]})
                          end
                  end, AllDeps).
