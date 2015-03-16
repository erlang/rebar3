-module(rebar_app_discover).

-export([do/2,
         format_error/1,
         find_unbuilt_apps/1,
         find_apps/1,
         find_apps/2,
         find_app/2]).

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
                        {AppInfo1, StateAcc1} = merge_deps(AppInfo, StateAcc),
                        Name = rebar_app_info:name(AppInfo),
                        OutDir = filename:join(DepsDir, Name),
                        AppInfo2 = rebar_app_info:out_dir(AppInfo1, OutDir),
                        ProjectDeps1 = lists:delete(Name, ProjectDeps),
                        rebar_state:project_apps(StateAcc1
                                                ,rebar_app_info:deps(AppInfo2, ProjectDeps1))
                end, State, SortedApps).

format_error({module_list, File}) ->
    io_lib:format("Error reading module list from ~p~n", [File]);
format_error({missing_module, Module}) ->
    io_lib:format("Module defined in app file missing: ~p~n", [Module]).

merge_deps(AppInfo, State) ->
    Default = rebar_state:default(State),
    CurrentProfiles = rebar_state:current_profiles(State),
    Name = rebar_app_info:name(AppInfo),
    C = rebar_config:consult(rebar_app_info:dir(AppInfo)),

    %% We reset the opts here to default so no profiles are applied multiple times
    AppState = rebar_state:apply_overrides(
                 rebar_state:apply_profiles(
                   rebar_state:new(rebar_state:opts(State, Default), C, rebar_app_info:dir(AppInfo)), CurrentProfiles), Name),
    AppInfo1 = rebar_app_info:state(AppInfo, AppState),

    State1 = lists:foldl(fun(Profile, StateAcc) ->
                                 AppProfDeps = rebar_state:get(AppState, {deps, Profile}, []),
                                 TopLevelProfDeps = rebar_state:get(StateAcc, {deps, Profile}, []),
                                 ProfDeps2 = dedup(lists:keymerge(1,
                                                                  lists:keysort(1, TopLevelProfDeps),
                                                                  lists:keysort(1, AppProfDeps))),
                                 rebar_state:set(StateAcc, {deps, Profile}, ProfDeps2)
                         end, State, lists:reverse(CurrentProfiles)),

    {AppInfo1, State1}.

-spec all_app_dirs(list(file:name())) -> list(file:name()).
all_app_dirs(LibDirs) ->
    lists:flatmap(fun(LibDir) ->
                          app_dirs(LibDir)
                  end, LibDirs).

app_dirs(LibDir) ->
    Path1 = filename:join([LibDir,
                           "*",
                           "src",
                           "*.app.src"]),
    Path2 = filename:join([LibDir,
                           "src",
                           "*.app.src"]),

    Path3 = filename:join([LibDir,
                           "*",
                           "ebin",
                           "*.app"]),
    Path4 = filename:join([LibDir,
                           "ebin",
                           "*.app"]),

    lists:usort(lists:foldl(fun(Path, Acc) ->
                                    Files = filelib:wildcard(ec_cnv:to_list(Path)),
                                    [app_dir(File) || File <- Files] ++ Acc
                            end, [], [Path1, Path2, Path3, Path4])).

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
    case AppFile of
        [File] ->
            AppInfo = create_app_info(AppDir, File),
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
            end;
        [] ->
            case AppSrcFile of
                [File] ->
                    case Validate of
                        V when V =:= invalid ; V =:= all ->
                            AppInfo = create_app_info(AppDir, File),
                            {true, rebar_app_info:app_file_src(AppInfo, File)};
                        valid ->
                            false
                    end;
                [] ->
                    false;
                Other when is_list(Other) ->
                    throw({error, {multiple_app_files, Other}})
            end;
        Other when is_list(Other) ->
            throw({error, {multiple_app_files, Other}})
    end.

app_dir(AppFile) ->
    filename:join(rebar_utils:droplast(filename:split(filename:dirname(AppFile)))).

-spec create_app_info(file:name(), file:name()) -> rebar_app_info:t() | error.
create_app_info(AppDir, AppFile) ->
    case file:consult(AppFile) of
        {ok, [{application, AppName, AppDetails}]} ->
            AppVsn = proplists:get_value(vsn, AppDetails),
            Applications = proplists:get_value(applications, AppDetails, []),
            IncludedApplications = proplists:get_value(included_applications, AppDetails, []),
            {ok, AppInfo} = rebar_app_info:new(AppName, AppVsn, AppDir, []),
            AppInfo1 = rebar_app_info:applications(
                         rebar_app_info:app_details(AppInfo, AppDetails),
                         IncludedApplications++Applications),
            rebar_app_info:dir(AppInfo1, AppDir);
        _ ->
            error
    end.

dedup([]) -> [];
dedup([A]) -> [A];
dedup([H,H|T]) -> dedup([H|T]);
dedup([H|T]) -> [H|dedup(T)].
