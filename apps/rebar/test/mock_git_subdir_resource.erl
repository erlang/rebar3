%%% Mock a git_subdir resource and create an app magically for each
%%% URL and subdirectory submitted.
-module(mock_git_subdir_resource).
-export([mock/0, mock/1, mock/2, unmock/0]).
-define(MOD, rebar_git_subdir_resource).

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
    Option :: {update, [App]}
            | {default_vsn, Vsn}
            | {override_vsn, [{App, Vsn}]}
            | {deps, [{App, [Dep]}]},
    App :: string(),
    Dep :: {App, {git_subdir, string(), term(), string()}}
         | {pkg, App, term()},
    Vsn :: string().
mock(Opts) ->
    mock(Opts, create_app).

mock(Opts, CreateType) ->
    meck:new(?MOD, [no_link, passthrough]),
    mock_lock(Opts),
    mock_update(Opts),
    mock_vsn(Opts),
    mock_download(Opts, CreateType),
    ok.

unmock() ->
    meck:unload(?MOD).

%%%%%%%%%%%%%%%
%%% Private %%%
%%%%%%%%%%%%%%%

%% @doc creates values for a lock file. The refs are fake, but
%% tags and existing refs declared for a dependency are preserved.
mock_lock(_) ->
    meck:expect(
        ?MOD, lock,
        fun(AppInfo, _) ->
            case rebar_app_info:source(AppInfo) of
                {git_subdir, Url, {tag, Ref}, Dir} -> {git_subdir, Url, {ref, Ref}, Dir};
                {git_subdir, Url, {ref, Ref}, Dir} -> {git_subdir, Url, {ref, Ref}, Dir};
                {git_subdir, Url, Dir} -> {git_subdir, Url, {ref, "0.0.0"}, Dir};
                {git_subdir, Url, _, Dir} -> {git_subdir, Url, {ref, "0.0.0"}, Dir}
            end
        end).

%% @doc The config passed to the `mock/2' function can specify which apps
%% should be updated on a per-name basis: `{update, ["App1", "App3"]}'.
mock_update(Opts) ->
    ToUpdate = proplists:get_value(upgrade, Opts, []),
%    ct:pal("TOUp: ~p", [ToUpdate]),
    meck:expect(
        ?MOD, needs_update,
        fun(AppInfo, _) ->
            {git_subdir, Url, _Ref} = rebar_app_info:source(AppInfo),
            App = app(Url),
%            ct:pal("Needed update? ~p (~p) -> ~p", [App, {Url,_Ref}, lists:member(App, ToUpdate)]),
            lists:member(App, ToUpdate)
        end).

%% @doc Tries to fetch a version from the `*.app.src' file or otherwise
%% just returns random stuff, avoiding to check for the presence of git.
%% This probably breaks the assumption that stable references are returned.
%%
%% This function can't respect the `override_vsn' option because if the
%% .app.src file isn't there, we can't find the app name either.
mock_vsn(Opts) ->
    Default = proplists:get_value(default_vsn, Opts, "0.0.0"),
    meck:expect(
        ?MOD, make_vsn,
      fun(AppInfo, _) ->
            Dir = rebar_app_info:dir(AppInfo),
            case filelib:wildcard("*.app.src", filename:join([Dir,"src"])) of
                [AppSrc] ->
                    {ok, App} = file:consult(AppSrc),
                    Vsn = proplists:get_value(vsn, App),
                    {plain, Vsn};
                _ ->
                    {plain, Default}
            end
        end).

%% @doc For each app to download, create a dummy app on disk instead.
%% The configuration for this one (passed in from `mock/1') includes:
%%
%% - Specify a version, branch, ref, or tag via the `{git_subdir, URL, {_, Vsn}'
%%   format to specify a path.
%% - If there is no version submitted (`{git_subdir, URL}'), the function instead
%%   reads from the `override_vsn' proplist (`{override_vsn, {"App1","1.2.3"}'),
%%   and otherwise uses the value associated with `default_vsn'.
%% - Dependencies for each application must be passed of the form:
%%   `{deps, [{"app1", [{app2, ".*", {git_subdir, ...}}]}]}' -- basically
%%   the `deps' option takes a key/value list of terms to output directly
%%   into a `rebar.config' file to describe dependencies.
mock_download(Opts, CreateType) ->
    Deps = proplists:get_value(deps, Opts, []),
    Config = proplists:get_value(config, Opts, []),
    Default = proplists:get_value(default_vsn, Opts, "0.0.0"),
    Overrides = proplists:get_value(override_vsn, Opts, []),
    meck:expect(
        ?MOD, download,
        fun (Dir, AppInfo, _, _) ->
            Git = rebar_app_info:source(AppInfo),
            filelib:ensure_dir(Dir),
            {git_subdir, Url, {_, Vsn}, SubDir} = normalize_git(Git, Overrides, Default),
            FullSubDir = filename:join(Dir, SubDir),
            filelib:ensure_dir(FullSubDir),
            App = app(Url),
            AppDeps = proplists:get_value({App,Vsn}, Deps, []),
            rebar_test_utils:CreateType(
                FullSubDir, App, Vsn,
                [kernel, stdlib] ++ [element(1,D) || D  <- AppDeps]
            ),
            rebar_test_utils:create_config(FullSubDir, [{deps, AppDeps}]++Config),
            ok
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
app(Path) ->
    filename:basename(Path, ".git").

normalize_git({git_subdir, Url, SubDir}, Overrides, Default) ->
    Vsn = proplists:get_value(app(Url), Overrides, Default),
    {git, Url, {tag, Vsn}, SubDir};
normalize_git({git_subdir, Url, Branch, SubDir}, _, _) when is_list(Branch) ->
    {git, Url, {branch, Branch}, SubDir};
normalize_git(Git, _, _) ->
    Git.
