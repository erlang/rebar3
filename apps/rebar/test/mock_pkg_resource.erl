%%% Mock a package resource and create an app magically for each URL submitted.
-module(mock_pkg_resource).
-export([mock/0, mock/1, unmock/0]).
-define(MOD, rebar_pkg_resource).

-include("rebar.hrl").

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
    Dep :: {App, string(), {pkg, App, Vsn, InnerHash, OuterHash}},
    Vsn :: string(),
    InnerHash :: string() | undefined,
    OuterHash :: string() | undefined.
mock(Opts) ->
    meck:new(?MOD, [no_link, passthrough]),
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
    meck:expect(?MOD, lock, fun(AppInfo, _) ->
                                {pkg, Name, Vsn, InnerHash, OuterHash, _RepoConfig} = rebar_app_info:source(AppInfo),
                                {pkg, Name, Vsn, InnerHash, OuterHash}
                            end).

%% @doc The config passed to the `mock/2' function can specify which apps
%% should be updated on a per-name basis: `{update, ["App1", "App3"]}'.
mock_update(Opts) ->
    ToUpdate = proplists:get_value(upgrade, Opts, []),
    meck:expect(
        ?MOD, needs_update,
        fun(AppInfo, _) ->
            {pkg, App, _Vsn, _InnerHash, _OuterHash, _} = rebar_app_info:source(AppInfo),
            lists:member(binary_to_list(App), ToUpdate)
        end).

%% @doc Replicated an unsupported call.
mock_vsn(_Opts) ->
    meck:expect(
        ?MOD, make_vsn,
        fun(_AppInfo, _) ->
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
        fun (Dir, AppInfo, _, _) ->
            {pkg, AppBin, Vsn, _, _, _} = rebar_app_info:source(AppInfo),
            App = rebar_utils:to_list(AppBin),
            filelib:ensure_dir(Dir),
            AppDeps = proplists:get_value({App,Vsn}, Deps, []),
            {ok, AppInfo1} = rebar_test_utils:create_app(
                Dir, App, rebar_utils:to_list(Vsn),
                [kernel, stdlib] ++ [element(1,D) || D  <- AppDeps]
            ),
            rebar_test_utils:create_config(Dir, [{deps, AppDeps}]++Config),

            TarApp = App++"-"++rebar_utils:to_list(Vsn)++".tar",

            Metadata = #{<<"app">> => AppBin,
                         <<"version">> => Vsn},

            Files = all_files(rebar_app_info:dir(AppInfo1)),
            {ok, #{tarball := Tarball}} = r3_hex_tarball:create(Metadata, archive_names(Dir, Files)),
            Archive = filename:join([Dir, TarApp]),
            file:write_file(Archive, Tarball),

            Cache = proplists:get_value(cache_dir, Opts, filename:join(Dir,"cache")),
            Cached = filename:join([Cache, TarApp]),
            filelib:ensure_dir(Cached),
            rebar_file_utils:mv(Archive, Cached),
            ok
        end).

%% @doc On top of the pkg resource mocking, we need to mock the package
%% index.
%%
%% A special option, `{not_in_index, [App]}' lets the index leave out
%% specific applications otherwise listed.
mock_pkg_index(Opts) ->
    Deps = proplists:get_value(pkgdeps, Opts, []),
    Repos = proplists:get_value(repos, Opts, [<<"hexpm">>]),
    Skip = proplists:get_value(not_in_index, Opts, []),
    %% Dict: {App, Vsn}: [{<<"link">>, <<>>}, {<<"deps">>, []}]
    %% Index: all apps and deps in the index

    Dict = find_parts(Deps, Skip),
    to_index(Deps, Dict, Repos),
    meck:new(rebar_packages, [passthrough, no_link]),
    meck:expect(rebar_packages, update_package,
                fun(_, _, _State) -> ok end),
    meck:expect(rebar_packages, verify_table,
                fun(_State) -> true end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

all_files(Dir) ->
    filelib:wildcard(filename:join([Dir, "**"])).

archive_names(Dir, Files) ->
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

parse_deps(Deps) ->
    [{maps:get(app, D, Name), {pkg, Name, Constraint, undefined, undefined}} || D=#{package := Name,
                                                                         requirement := Constraint} <- Deps].

to_index(AllDeps, Dict, Repos) ->
    catch ets:delete(?PACKAGE_TABLE),
    rebar_packages:new_package_table(),

    dict:fold(
      fun({N, V}, Deps, _) ->
              DepsList = [#{package => DKB,
                            app => DKB,
                            requirement => DVB,
                            source => {pkg, DKB, DVB, undefined, undefined}}
                          || {DK, DV} <- Deps,
                             DKB <- [ec_cnv:to_binary(DK)],
                             DVB <- [ec_cnv:to_binary(DV)]],
              Repo = rebar_test_utils:random_element(Repos),

              ets:insert(?PACKAGE_TABLE, #package{key={N, ec_semver:parse(V), Repo},
                                                  dependencies=parse_deps(DepsList),
                                                  retired=false,
                                                  inner_checksum = <<"inner_checksum">>,
                                                  outer_checksum = <<"checksum">>})
      end, ok, Dict),

    lists:foreach(fun({{Name, Vsn}, _}) ->
                          case lists:any(fun(R) ->
                                                 ets:member(?PACKAGE_TABLE, {ec_cnv:to_binary(Name), ec_semver:parse(Vsn), R})
                                         end, Repos) of
                              false ->
                                  Repo = rebar_test_utils:random_element(Repos),
                                  ets:insert(?PACKAGE_TABLE, #package{key={ec_cnv:to_binary(Name), ec_semver:parse(Vsn), Repo},
                                                                      dependencies=[],
                                                                      retired=false,
                                                                      inner_checksum = <<"inner_checksum">>,
                                                                      outer_checksum = <<"checksum">>});
                              true ->
                                  ok
                          end
                  end, AllDeps).

