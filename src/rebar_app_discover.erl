-module(rebar_app_discover).

-export([do/2,
         format_error/1,
         find_unbuilt_apps/1,
         find_apps/1,
         find_apps/2,
         find_app/2]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

do(State, LibDirs) ->
    BaseDir = rebar_state:dir(State),
    Dirs = [filename:join(BaseDir, LibDir) || LibDir <- LibDirs],
    Apps = find_apps(Dirs, all),
    ProjectDeps = rebar_state:deps_names(State),
    DepsDir = rebar_dir:deps_dir(State),

    %% Sort apps so we get the same merged deps config everytime
    SortedApps = rebar_utils:sort_deps(Apps),
    lists:foldl(fun(AppInfo, StateAcc) ->
                        Name = rebar_app_info:name(AppInfo),
                        case enable(State, AppInfo) of
                            true ->
                                {AppInfo1, StateAcc1} = merge_deps(AppInfo, StateAcc),
                                OutDir = filename:join(DepsDir, Name),
                                AppInfo2 = rebar_app_info:out_dir(AppInfo1, OutDir),
                                ProjectDeps1 = lists:delete(Name, ProjectDeps),
                                rebar_state:project_apps(StateAcc1
                                                        ,rebar_app_info:deps(AppInfo2, ProjectDeps1));
                            false ->
                                ?INFO("Ignoring ~s", [Name]),
                                StateAcc
                        end
                end, State, SortedApps).

format_error({module_list, File}) ->
    io_lib:format("Error reading module list from ~p~n", [File]);
format_error({missing_module, Module}) ->
    io_lib:format("Module defined in app file missing: ~p~n", [Module]).

merge_deps(AppInfo, State) ->
    Default = rebar_state:default(State),
    CurrentProfiles = rebar_state:current_profiles(State),
    Name = rebar_app_info:name(AppInfo),
    C = project_app_config(AppInfo, State),

    %% We reset the opts here to default so no profiles are applied multiple times
    AppState = rebar_state:apply_overrides(
                 rebar_state:apply_profiles(
                   rebar_state:new(reset_hooks(rebar_state:opts(State, Default)), C,
                                  rebar_app_info:dir(AppInfo)), CurrentProfiles), Name),
    AppState1 = rebar_state:set(AppState, artifacts, []),
    AppInfo1 = rebar_app_info:state(AppInfo, AppState1),

    State1 = lists:foldl(fun(Profile, StateAcc) ->
                                 AppProfDeps = rebar_state:get(AppState, {deps, Profile}, []),
                                 TopLevelProfDeps = rebar_state:get(StateAcc, {deps, Profile}, []),
                                 ProfDeps2 = dedup(rebar_utils:tup_umerge(
                                                     rebar_utils:tup_sort(TopLevelProfDeps)
                                                     ,rebar_utils:tup_sort(AppProfDeps))),
                                 rebar_state:set(StateAcc, {deps, Profile}, ProfDeps2)
                         end, State, lists:reverse(CurrentProfiles)),

    {AppInfo1, State1}.

project_app_config(AppInfo, State) ->
    C = rebar_config:consult(rebar_app_info:dir(AppInfo)),
    Dir = rebar_app_info:dir(AppInfo),
    maybe_reset_hooks(C, Dir, State).

%% Here we check if the app is at the root of the project.
%% If it is, then drop the hooks from the config so they aren't run twice
maybe_reset_hooks(C, Dir, State) ->
    case ec_file:real_dir_path(rebar_dir:root_dir(State)) of
        Dir ->
            C1 = proplists:delete(provider_hooks, C),
            proplists:delete(post_hooks, proplists:delete(pre_hooks, C1));
        _ ->
            C
    end.

reset_hooks(State) ->
    lists:foldl(fun(Key, StateAcc) ->
                        rebar_state:set(StateAcc, Key, [])
                end, State, [post_hooks, pre_hooks, provider_hooks]).

-spec all_app_dirs(list(file:name())) -> list(file:name()).
all_app_dirs(LibDirs) ->
    lists:flatmap(fun(LibDir) ->
                          app_dirs(LibDir)
                  end, LibDirs).

app_dirs(LibDir) ->
    Path1 = filename:join([LibDir,
                           "src",
                           "*.app.src"]),

    Path2 = filename:join([LibDir,
                           "ebin",
                           "*.app"]),

    lists:usort(lists:foldl(fun(Path, Acc) ->
                                    Files = filelib:wildcard(ec_cnv:to_list(Path)),
                                    [app_dir(File) || File <- Files] ++ Acc
                            end, [], [Path1, Path2])).

find_unbuilt_apps(LibDirs) ->
    find_apps(LibDirs, invalid).

-spec find_apps([file:filename_all()]) -> [rebar_app_info:t()].
find_apps(LibDirs) ->
    find_apps(LibDirs, valid).

-spec find_apps([file:filename_all()], valid | invalid | all) -> [rebar_app_info:t()].
find_apps(LibDirs, Validate) ->
    rebar_utils:filtermap(fun(AppDir) ->
                                  find_app(AppDir, Validate)
                          end, all_app_dirs(LibDirs)).

-spec find_app(file:filename_all(), valid | invalid | all) -> {true, rebar_app_info:t()} | false.
find_app(AppDir, Validate) ->
    AppFile = filelib:wildcard(filename:join([AppDir, "ebin", "*.app"])),
    AppSrcFile = filelib:wildcard(filename:join([AppDir, "src", "*.app.src"])),
    AppInfo = try_handle_app_file(AppFile, AppDir, AppSrcFile, Validate),
    AppInfo.

app_dir(AppFile) ->
    filename:join(rebar_utils:droplast(filename:split(filename:dirname(AppFile)))).

-spec create_app_info(file:name(), file:name()) -> rebar_app_info:t() | {error, term()}.
create_app_info(AppDir, AppFile) ->
    [{application, AppName, AppDetails}] = rebar_config:consult_app_file(AppFile),
    AppVsn = proplists:get_value(vsn, AppDetails),
    Applications = proplists:get_value(applications, AppDetails, []),
    IncludedApplications = proplists:get_value(included_applications, AppDetails, []),
    {ok, AppInfo} = rebar_app_info:new(AppName, AppVsn, AppDir, []),
    AppInfo1 = rebar_app_info:applications(
                 rebar_app_info:app_details(AppInfo, AppDetails),
                 IncludedApplications++Applications),
    Valid = case rebar_app_utils:validate_application_info(AppInfo1) of
                true ->
                    true;
                _ ->
                    false
            end,
    rebar_app_info:dir(rebar_app_info:valid(AppInfo1, Valid), AppDir).

dedup([]) -> [];
dedup([A]) -> [A];
dedup([H,H|T]) -> dedup([H|T]);
dedup([H|T]) -> [H|dedup(T)].

%% Read in and parse the .app file if it is availabe. Do the same for
%% the .app.src file if it exists.
try_handle_app_file([], AppDir, AppSrcFile, Validate) ->
    try_handle_app_src_file([], AppDir, AppSrcFile, Validate);
try_handle_app_file([File], AppDir, AppSrcFile, Validate) ->
    try create_app_info(AppDir, File) of
        AppInfo ->
            AppInfo1 = rebar_app_info:app_file(AppInfo, File),
            AppInfo2 = case AppSrcFile of
                           [F] ->
                               rebar_app_info:app_file_src(AppInfo1, F);
                           [] ->
                               AppInfo1;
                           Other when is_list(Other) ->
                               throw({error, {multiple_app_files, Other}})
                      end,
            case Validate of
                valid ->
                    case rebar_app_utils:validate_application_info(AppInfo2) of
                        true ->
                            {true, AppInfo2};
                        _ ->
                            false
                    end;
                invalid ->
                    case rebar_app_utils:validate_application_info(AppInfo2) of
                        true ->
                            false;
                        _ ->
                            {true, AppInfo2}
                    end;
                all ->
                    {true, AppInfo2}
            end
    catch
        throw:{error, {Module, Reason}} ->
            ?DEBUG("Falling back to app.src file because .app failed: ~s", [Module:format_error(Reason)]),
            try_handle_app_src_file(File, AppDir, AppSrcFile, Validate)
    end;
try_handle_app_file(Other, _AppDir, _AppSrcFile, _Validate) ->
    throw({error, {multiple_app_files, Other}}).

%% Read in the .app.src file if we aren't looking for a valid (already built) app
try_handle_app_src_file(_, _AppDir, [], _Validate) ->
    false;
try_handle_app_src_file(_, _AppDir, _AppSrcFile, valid) ->
    false;
try_handle_app_src_file(_, AppDir, [File], Validate) when Validate =:= invalid
                                                        ; Validate =:= all ->
    AppInfo = create_app_info(AppDir, File),
    case AppInfo of
        {error, Reason} ->
            throw({error, {invalid_app_file, File, Reason}});
        _ ->
            {true, rebar_app_info:app_file_src(AppInfo, File)}
    end;
try_handle_app_src_file(_, _AppDir, Other, _Validate) ->
    throw({error, {multiple_app_files, Other}}).

enable(State, AppInfo) ->
    not lists:member(to_atom(rebar_app_info:name(AppInfo)),
             rebar_state:get(State, excluded_apps, [])).

to_atom(Bin) ->
    list_to_atom(binary_to_list(Bin)).
