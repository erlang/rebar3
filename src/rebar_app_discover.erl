%%% @doc utility functions to do the basic discovery of apps
%%% and layout for the project.
-module(rebar_app_discover).

-export([do/2,
         format_error/1,
         find_unbuilt_apps/1,
         find_apps/1,
         find_apps/2,
         find_apps/3,
         find_app/2,
         find_app/3,
         find_app/4]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

%% @doc from the base directory, find all the applications
%% at the top level and their dependencies based on the configuration
%% and profile information.
-spec do(rebar_state:t(), [file:filename()]) -> rebar_state:t().
do(State, LibDirs) ->
    BaseDir = rebar_state:dir(State),
    Dirs = [filename:join(BaseDir, LibDir) || LibDir <- LibDirs],
    RebarOpts = rebar_state:opts(State),
    SrcDirs = rebar_dir:src_dirs(RebarOpts, ["src"]),
    Apps = find_apps(Dirs, SrcDirs, all),
    ProjectDeps = rebar_state:deps_names(State),
    DepsDir = rebar_dir:deps_dir(State),
    CurrentProfiles = rebar_state:current_profiles(State),

    %% There may be a top level src which is an app and there may not
    %% Find it here if there is, otherwise define the deps parent as root
    TopLevelApp = define_root_app(Apps, State),

    %% Handle top level deps
    State1 = lists:foldl(fun(Profile, StateAcc) ->
                                 ProfileDeps = rebar_state:get(StateAcc, {deps, Profile}, []),
                                 ProfileDeps2 = rebar_utils:tup_dedup(ProfileDeps),
                                 StateAcc1 = rebar_state:set(StateAcc, {deps, Profile}, ProfileDeps2),
                                 ParsedDeps = parse_profile_deps(Profile
                                                                ,TopLevelApp
                                                                ,ProfileDeps2
                                                                ,rebar_state:opts(StateAcc1)
                                                                ,StateAcc1),
                                 rebar_state:set(StateAcc1, {parsed_deps, Profile}, ParsedDeps)
                         end, State, lists:reverse(CurrentProfiles)),

    %% Handle sub project apps deps
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
                                ?INFO("Ignoring ~ts", [Name]),
                                StateAcc
                        end
                end, State1, SortedApps).

%% @doc checks whether there is an app at the top level (and returns its
%% name) or the 'root' atom in case we're in an umbrella project.
-spec define_root_app([rebar_app_info:t()], rebar_state:t()) ->
    root | binary().
define_root_app(Apps, State) ->
    RootDir = rebar_dir:root_dir(State),
    case ec_lists:find(fun(X) ->
                               ec_file:real_dir_path(rebar_app_info:dir(X)) =:=
                                   ec_file:real_dir_path(RootDir)
                       end, Apps) of
        {ok, App} ->
            rebar_app_info:name(App);
        error ->
            root
    end.

%% @doc formatting errors from the module.
-spec format_error(term()) -> iodata().
format_error({module_list, File}) ->
    io_lib:format("Error reading module list from ~p~n", [File]);
format_error({missing_module, Module}) ->
    io_lib:format("Module defined in app file missing: ~p~n", [Module]).

%% @doc handles the merging and application of profiles and overrides
%% for a given application, within its own context.
-spec merge_deps(rebar_app_info:t(), rebar_state:t()) ->
    {rebar_app_info:t(), rebar_state:t()}.
merge_deps(AppInfo, State) ->
    %% These steps make sure that hooks and artifacts are run in the context of
    %% the application they are defined at. If an umbrella structure is used and
    %% they are deifned at the top level they will instead run in the context of
    %% the State and at the top level, not as part of an application.
    CurrentProfiles = rebar_state:current_profiles(State),
    Default = reset_hooks(rebar_state:default(State), CurrentProfiles),
    {C, State1} = project_app_config(AppInfo, State),
    AppInfo0 = rebar_app_info:update_opts(AppInfo, Default, C),

    Name = rebar_app_info:name(AppInfo0),

    %% We reset the opts here to default so no profiles are applied multiple times
    AppInfo1 = rebar_app_info:apply_overrides(rebar_state:get(State1, overrides, []), AppInfo0),
    AppInfo2 = rebar_app_info:apply_profiles(AppInfo1, CurrentProfiles),

    %% Will throw an exception if checks fail
    rebar_app_info:verify_otp_vsn(AppInfo2),

    State2 = lists:foldl(fun(Profile, StateAcc) ->
                                 handle_profile(Profile, Name, AppInfo2, StateAcc)
                         end, State1, lists:reverse(CurrentProfiles)),

    {AppInfo2, State2}.

%% @doc Applies a given profile for an app, ensuring the deps
%% match the context it will require.
-spec handle_profile(atom(), binary(), rebar_app_info:t(), rebar_state:t()) ->
    rebar_state:t().
handle_profile(Profile, Name, AppInfo, State) ->
    TopParsedDeps = rebar_state:get(State, {parsed_deps, Profile}, {[], []}),
    TopLevelProfileDeps = rebar_state:get(State, {deps, Profile}, []),
    AppProfileDeps = rebar_app_info:get(AppInfo, {deps, Profile}, []),
    AppProfileDeps2 = rebar_utils:tup_dedup(AppProfileDeps),
    ProfileDeps2 = rebar_utils:tup_dedup(rebar_utils:tup_umerge(TopLevelProfileDeps
                                                               ,AppProfileDeps2)),
    State1 = rebar_state:set(State, {deps, Profile}, ProfileDeps2),

    %% Only deps not also specified in the top level config need
    %% to be included in the parsed deps
    NewDeps = ProfileDeps2 -- TopLevelProfileDeps,
    ParsedDeps = parse_profile_deps(Profile, Name, NewDeps, rebar_app_info:opts(AppInfo), State1),
    State2 = rebar_state:set(State1, {deps, Profile}, ProfileDeps2),
    rebar_state:set(State2, {parsed_deps, Profile}, TopParsedDeps++ParsedDeps).

%% @doc parses all the known dependencies for a given profile
-spec parse_profile_deps(Profile, Name, Deps, Opts, rebar_state:t()) -> [rebar_app_info:t()] when
      Profile :: atom(),
      Name :: binary(),
      Deps :: [term()], % TODO: refine types
      Opts :: term(). % TODO: refine types
parse_profile_deps(Profile, Name, Deps, Opts, State) ->
    DepsDir = rebar_prv_install_deps:profile_dep_dir(State, Profile),
    Locks = rebar_state:get(State, {locks, Profile}, []),
    rebar_app_utils:parse_deps(Name
                              ,DepsDir
                              ,Deps
                              ,rebar_state:opts(State, Opts)
                              ,Locks
                              ,1).

%% @doc Find the app-level config and return the state updated
%% with the relevant app-level data.
-spec project_app_config(rebar_app_info:t(), rebar_state:t()) ->
    {Config, rebar_state:t()} when
      Config :: [any()].
project_app_config(AppInfo, State) ->
    C = rebar_config:consult(rebar_app_info:dir(AppInfo)),
    Dir = rebar_app_info:dir(AppInfo),
    Opts = maybe_reset_hooks(Dir, rebar_state:opts(State), State),
    {C, rebar_state:opts(State, Opts)}.

%% @private Check if the app is at the root of the project.
%% If it is, then drop the hooks from the config so they aren't run twice
-spec maybe_reset_hooks(file:filename(), Opts, rebar_state:t()) -> Opts when
      Opts :: rebar_dict().
maybe_reset_hooks(Dir, Opts, State) ->
    case ec_file:real_dir_path(rebar_dir:root_dir(State)) of
        Dir ->
            CurrentProfiles = rebar_state:current_profiles(State),
            reset_hooks(Opts, CurrentProfiles);
        _ ->
            Opts
    end.

%% @doc make the hooks empty for a given set of options
-spec reset_hooks(Opts, Profiles) ->
    Opts when
      Opts :: rebar_dict(),
      Profiles :: [atom()].
reset_hooks(Opts, CurrentProfiles) ->
    AllHooks = [post_hooks, pre_hooks, provider_hooks, artifacts],
    Opts1 = lists:foldl(fun(Key, OptsAcc) ->
                            rebar_opts:set(OptsAcc, Key, [])
                        end, Opts, AllHooks),
    Profiles = rebar_opts:get(Opts1, profiles, []),
    Profiles1 = lists:map(fun({P, ProfileOpts}) ->
                              case lists:member(P, CurrentProfiles) of
                                  true ->
                                      {P, [X || X={Key, _} <- ProfileOpts,
                                                not lists:member(Key, AllHooks)]};
                                  false ->
                                      {P, ProfileOpts}
                              end
                          end, Profiles),
    rebar_opts:set(Opts1, profiles, Profiles1).

%% @private find the directories for all apps, while detecting their source dirs
%% Returns the app dir with the respective src_dirs for them, in that order,
%% for every app found.
-spec all_app_dirs([file:name()]) -> [{file:name(), [file:name()]}].
all_app_dirs(LibDirs) ->
    lists:flatmap(fun(LibDir) ->
                        SrcDirs = find_config_src(LibDir, ["src"]),
                        app_dirs(LibDir, SrcDirs)
                  end, LibDirs).

%% @private find the directories for all apps based on their source dirs
%% Returns the app dir with the respective src_dirs for them, in that order,
%% for every app found.
-spec all_app_dirs([file:name()], [file:name()]) -> [{file:name(), [file:name()]}].
all_app_dirs(LibDirs, SrcDirs) ->
    lists:flatmap(fun(LibDir) -> app_dirs(LibDir, SrcDirs) end, LibDirs).

%% @private find the directories based on the library directories.
%% Returns the app dir with the respective src_dirs for them, in that order,
%% for every app found.
%%
%% The function returns the src directories since they might have been
%% detected in a top-level loop and we want to skip further detection
%% starting now.
-spec app_dirs([file:name()], [file:name()]) -> [{file:name(), [file:name()]}].
app_dirs(LibDir, SrcDirs) ->
    Paths = lists:append([
        [filename:join([LibDir, SrcDir, "*.app.src"]),
         filename:join([LibDir, SrcDir, "*.app.src.script"])]
        || SrcDir <- SrcDirs
    ]),
    EbinPath = filename:join([LibDir, "ebin", "*.app"]),

    lists:usort(lists:foldl(fun(Path, Acc) ->
                                Files = filelib:wildcard(rebar_utils:to_list(Path)),
                                [{app_dir(File), SrcDirs}
                                 || File <- Files] ++ Acc
                            end, [], [EbinPath | Paths])).

%% @doc find all apps that haven't been built in a list of directories
-spec find_unbuilt_apps([file:filename_all()]) -> [rebar_app_info:t()].
find_unbuilt_apps(LibDirs) ->
    find_apps(LibDirs, invalid).

%% @doc for each directory passed, find all apps that are valid.
%% Returns all the related app info records.
-spec find_apps([file:filename_all()]) -> [rebar_app_info:t()].
find_apps(LibDirs) ->
    find_apps(LibDirs, valid).

%% @doc for each directory passed, find all apps according
%% to the validity rule passed in. Returns all the related
%% app info records.
-spec find_apps([file:filename_all()], valid | invalid | all) -> [rebar_app_info:t()].
find_apps(LibDirs, Validate) ->
    rebar_utils:filtermap(
      fun({AppDir, AppSrcDirs}) ->
            find_app(rebar_app_info:new(), AppDir, AppSrcDirs, Validate)
      end,
      all_app_dirs(LibDirs)
    ).

%% @doc for each directory passed, with the configured source directories,
%% find all apps according to the validity rule passed in.
%% Returns all the related app info records.
-spec find_apps([file:filename_all()], [file:filename_all()], valid | invalid | all) -> [rebar_app_info:t()].
find_apps(LibDirs, SrcDirs, Validate) ->
    rebar_utils:filtermap(
      fun({AppDir, AppSrcDirs}) ->
            find_app(rebar_app_info:new(), AppDir, AppSrcDirs, Validate)
      end,
      all_app_dirs(LibDirs, SrcDirs)
    ).

%% @doc check that a given app in a directory is there, and whether it's
%% valid or not based on the second argument. Returns the related
%% app info record.
-spec find_app(file:filename_all(), valid | invalid | all) -> {true, rebar_app_info:t()} | false.
find_app(AppDir, Validate) ->
    SrcDirs = find_config_src(AppDir, ["src"]),
    find_app(rebar_app_info:new(), AppDir, SrcDirs, Validate).

%% @doc check that a given app in a directory is there, and whether it's
%% valid or not based on the second argument. Returns the related
%% app info record.
-spec find_app(rebar_app_info:t(), file:filename_all(), valid | invalid | all) ->
    {true, rebar_app_info:t()} | false.
find_app(AppInfo, AppDir, Validate) ->
    %% if no src dir is passed, figure it out from the app info, with a default
    %% of src/
    AppOpts = rebar_app_info:opts(AppInfo),
    SrcDirs = rebar_dir:src_dirs(AppOpts, ["src"]),
    find_app(AppInfo, AppDir, SrcDirs, Validate).

%% @doc check that a given app in a directory is there, and whether it's
%% valid or not based on the second argument. The third argument includes
%% the directories where source files can be located. Returns the related
%% app info record.
-spec find_app(rebar_app_info:t(), file:filename_all(),
               [file:filename_all()], valid | invalid | all) ->
    {true, rebar_app_info:t()} | false.
find_app(AppInfo, AppDir, SrcDirs, Validate) ->
    AppFile = filelib:wildcard(filename:join([AppDir, "ebin", "*.app"])),
    AppSrcFile = lists:append(
        [filelib:wildcard(filename:join([AppDir, SrcDir, "*.app.src"]))
         || SrcDir <- SrcDirs]
    ),
    AppSrcScriptFile = lists:append(
        [filelib:wildcard(filename:join([AppDir, SrcDir, "*.app.src.script"]))
         || SrcDir <- SrcDirs]
    ),
    try_handle_app_file(AppInfo, AppFile, AppDir, AppSrcFile, AppSrcScriptFile, Validate).

%% @doc find the directory that an appfile has
-spec app_dir(file:filename()) -> file:filename().
app_dir(AppFile) ->
    filename:join(rebar_utils:droplast(filename:split(filename:dirname(AppFile)))).

%% @doc populates an app info record based on an app directory and its
%% app file.
-spec create_app_info(rebar_app_info:t(), file:name(), file:name()) -> rebar_app_info:t().
create_app_info(AppInfo, AppDir, AppFile) ->
    [{application, AppName, AppDetails}] = rebar_config:consult_app_file(AppFile),
    AppVsn = proplists:get_value(vsn, AppDetails),
    Applications = proplists:get_value(applications, AppDetails, []),
    IncludedApplications = proplists:get_value(included_applications, AppDetails, []),
    AppInfo1 = rebar_app_info:name(
                 rebar_app_info:original_vsn(
                   rebar_app_info:dir(AppInfo, AppDir), AppVsn), AppName),
    AppInfo2 = rebar_app_info:applications(
                 rebar_app_info:app_details(AppInfo1, AppDetails),
                 IncludedApplications++Applications),
    Valid = case rebar_app_utils:validate_application_info(AppInfo2) =:= true
                andalso rebar_app_info:has_all_artifacts(AppInfo2) =:= true of
                true ->
                    true;
                _ ->
                    false
            end,
    rebar_app_info:dir(rebar_app_info:valid(AppInfo2, Valid), AppDir).

%% @doc Read in and parse the .app file if it is availabe. Do the same for
%% the .app.src file if it exists.
-spec try_handle_app_file(AppInfo, AppFile, AppDir, AppSrcFile, AppSrcScriptFile, valid | invalid | all) ->
    {true, AppInfo} | false when
      AppInfo :: rebar_app_info:t(),
      AppFile :: file:filename(),
      AppDir :: file:filename(),
      AppSrcFile :: file:filename(),
      AppSrcScriptFile :: file:filename().
try_handle_app_file(AppInfo, [], AppDir, [], AppSrcScriptFile, Validate) ->
    try_handle_app_src_file(AppInfo, [], AppDir, AppSrcScriptFile, Validate);
try_handle_app_file(AppInfo, [], AppDir, AppSrcFile, _, Validate) ->
    try_handle_app_src_file(AppInfo, [], AppDir, AppSrcFile, Validate);
try_handle_app_file(AppInfo0, [File], AppDir, AppSrcFile, _, Validate) ->
    try create_app_info(AppInfo0, AppDir, File) of
        AppInfo ->
            AppInfo1 = rebar_app_info:app_file(AppInfo, File),
            AppInfo2 = case AppSrcFile of
                           [F] ->
                               rebar_app_info:app_file_src(AppInfo1, F);
                           [] ->
                               %% Set to undefined in case AppInfo previous had a .app.src
                               rebar_app_info:app_file_src(AppInfo1, undefined);
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
            ?DEBUG("Falling back to app.src file because .app failed: ~ts", [Module:format_error(Reason)]),
            try_handle_app_src_file(AppInfo0, File, AppDir, AppSrcFile, Validate)
    end;
try_handle_app_file(_AppInfo, Other, _AppDir, _AppSrcFile, _, _Validate) ->
    throw({error, {multiple_app_files, Other}}).

%% @doc Read in the .app.src file if we aren't looking for a valid (already
%% built) app.
-spec try_handle_app_src_file(AppInfo, AppFile, AppDir, AppSrcFile, valid | invalid | all) ->
    {true, AppInfo} | false when
      AppInfo :: rebar_app_info:t(),
      AppFile :: file:filename(),
      AppDir :: file:filename(),
      AppSrcFile :: file:filename().
try_handle_app_src_file(_AppInfo, _, _AppDir, [], _Validate) ->
    false;
try_handle_app_src_file(_AppInfo, _, _AppDir, _AppSrcFile, valid) ->
    false;
try_handle_app_src_file(AppInfo, _, AppDir, [File], Validate) when Validate =:= invalid
                                                                 ; Validate =:= all ->
    AppInfo1 = create_app_info(AppInfo, AppDir, File),
    case filename:extension(File) of
        ".script" ->
            {true, rebar_app_info:app_file_src_script(AppInfo1, File)};
        _ ->
            {true, rebar_app_info:app_file_src(AppInfo1, File)}
    end;
try_handle_app_src_file(_AppInfo, _, _AppDir, Other, _Validate) ->
    throw({error, {multiple_app_files, Other}}).

%% @doc checks whether the given app is not blacklisted in the config.
-spec enable(rebar_state:t(), rebar_app_info:t()) -> boolean().
enable(State, AppInfo) ->
    not lists:member(to_atom(rebar_app_info:name(AppInfo)),
             rebar_state:get(State, excluded_apps, [])).

%% @private convert a binary to an atom.
-spec to_atom(binary()) -> atom().
to_atom(Bin) ->
    list_to_atom(binary_to_list(Bin)).

%% @private when looking for unknown apps, it's possible they have a
%% rebar.config file specifying non-standard src_dirs. Check for a
%% possible config file and extract src_dirs from it.
find_config_src(AppDir, Default) ->
    case rebar_config:consult(AppDir) of
        [] ->
            Default;
        Terms ->
            %% TODO: handle profiles I guess, but we don't have that info
            proplists:get_value(src_dirs, Terms, Default)
    end.
