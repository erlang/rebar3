%%% @doc utility functions to do the basic discovery of apps
%%% and layout for the project.
-module(rebar_app_discover).

-export([do/2,
         format_error/1,
         find_unbuilt_apps/2,
         find_apps/2,
         find_apps/3,
         find_apps/4,
         find_app/3,
         find_app/4]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-type app_resource_type() :: app |  app_src | script | mix_exs.

%% @doc from the base directory, find all the applications
%% at the top level and their dependencies based on the configuration
%% and profile information.
-spec do(rebar_state:t(), [file:filename()]) -> rebar_state:t() | no_return().
do(State, LibDirs) ->
    BaseDir = rebar_state:dir(State),
    Dirs = [filename:join(BaseDir, LibDir) || LibDir <- LibDirs],
    RebarOpts = rebar_state:opts(State),
    SrcDirs = rebar_dir:src_dirs(RebarOpts, ["src"]),
    Apps = find_apps(Dirs, SrcDirs, all, State),
    ProjectDeps = rebar_state:deps_names(State),
    DepsDir = rebar_dir:deps_dir(State),
    CurrentProfiles = rebar_state:current_profiles(State),

    %% There may be a top level src which is an app and there may not
    %% Find it here if there is, otherwise define the deps parent as root
    TopLevelApp = define_root_app(Apps, State),
    ?DEBUG("Found top-level apps: [~ts]~n\tusing config: ~p",
           [case lists:flatten([[",",rebar_app_info:name(App)] || App <- Apps]) of
                [] -> "";
                Str -> tl(Str)
            end,
            [{src_dirs, SrcDirs}, {lib_dirs, LibDirs}]]),

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
    %% Sort apps so we get the same merged deps config every time
    SortedApps = rebar_utils:sort_deps(Apps),
    lists:foldl(fun(AppInfo, StateAcc) ->
                        Name = rebar_app_info:name(AppInfo),
                        case enable(State, AppInfo) of
                            true ->
                                {AppInfo1, StateAcc1} = merge_opts(TopLevelApp, AppInfo, StateAcc),
                                OutDir = filename:join(DepsDir, Name),
                                AppInfo2 = rebar_app_info:out_dir(AppInfo1, OutDir),
                                ProjectDeps1 = lists:delete(Name, ProjectDeps),
                                rebar_state:project_apps(StateAcc1,
                                                         rebar_app_info:deps(AppInfo2, ProjectDeps1));
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
    io_lib:format("Module defined in app file missing: ~p~n", [Module]);
format_error({cannot_read_app_file, AppFile}) ->
    io_lib:format("Cannot read app file: ~p~n", [AppFile]);
format_error({bad_term_file, _File, _Reason} = Error) ->
    rebar_file_utils:format_error(Error).


%% @doc merges configuration of a project app and the top level state
%% some configuration like erl_opts must be merged into a subapp's opts
%% while plugins and hooks need to be kept defined to only either the
%% top level state or an individual application.
-spec merge_opts(root | binary(), rebar_app_info:t(), rebar_state:t()) ->
    {rebar_app_info:t(), rebar_state:t()}.
merge_opts(TopLevelApp, AppInfo, State) ->
    %% These steps make sure that hooks and artifacts are run in the context of
    %% the application they are defined at. If an umbrella structure is used and
    %% they are defined at the top level they will instead run in the context of
    %% the State and at the top level, not as part of an application.
    CurrentProfiles = rebar_state:current_profiles(State),
    {AppInfo1, State1} = maybe_reset_hooks_plugins(AppInfo, State),

    Name = rebar_app_info:name(AppInfo1),

    %% We reset the opts here to default so no profiles are applied multiple times
    AppInfo2 = case TopLevelApp of
        Name -> % don't apply to the root app
            AppInfo;
        _ -> % apply overrides when in an umbrella project or on deps
            rebar_app_info:apply_overrides(rebar_state:get(State1, overrides, []), AppInfo1)
    end,
    AppInfo3 = rebar_app_info:apply_profiles(AppInfo2, CurrentProfiles),

    %% Will throw an exception if checks fail
    rebar_app_info:verify_otp_vsn(AppInfo3),

    State2 = lists:foldl(fun(Profile, StateAcc) ->
                                 handle_profile(Profile, Name, AppInfo3, StateAcc)
                         end, State1, lists:reverse(CurrentProfiles)),

    {AppInfo3, State2}.

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

%% reset the State hooks if there is a top level application
-spec maybe_reset_hooks_plugins(AppInfo, State) ->  {AppInfo, State} when
      AppInfo :: rebar_app_info:t(),
      State :: rebar_state:t().
maybe_reset_hooks_plugins(AppInfo, State) ->
    Dir = rebar_app_info:dir(AppInfo),
    CurrentProfiles = rebar_state:current_profiles(State),
    case ec_file:real_dir_path(rebar_dir:root_dir(State)) of
        Dir ->
            Opts = reset_hooks(rebar_state:opts(State), CurrentProfiles),
            State1 = rebar_state:opts(State, Opts),

            %% set plugins to empty since this is an app at the top level
            %% and top level plugins are installed in run_aux
            AppInfo1 = rebar_app_info:set(rebar_app_info:set(AppInfo, {plugins,default}, []), plugins, []),

            {AppInfo1, State1};
        _ ->
            %% if not in the top root directory then we need to merge in the
            %% default state opts to this subapp's opts
            Default = reset_hooks(rebar_state:default(State), CurrentProfiles),
            AppInfo1 = rebar_app_info:update_opts(AppInfo, Default),
            {AppInfo1, State}
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
-spec all_app_dirs([file:name()], rebar_state:t()) -> [{file:name(), [file:name()]}].
all_app_dirs(LibDirs, State) ->
    lists:flatmap(fun(LibDir) ->
                          case filelib:is_dir(LibDir) of
                              true ->
                                  {_, SrcDirs} = find_config_src(LibDir, ["src"]),
                                  app_dirs(LibDir, SrcDirs, State);
                              false ->
                                  []
                          end
                  end, LibDirs).

%% @private find the directories for all apps based on their source dirs
%% Returns the app dir with the respective src_dirs for them, in that order,
%% for every app found.
-spec all_app_dirs([file:name()], [file:name()], rebar_state:t()) -> [{file:name(), [file:name()]}].
all_app_dirs(LibDirs, SrcDirs, State) ->
    lists:flatmap(fun(LibDir) -> app_dirs(LibDir, SrcDirs, State) end, LibDirs).

%% @private find the directories based on the library directories.
%% Returns the app dir with the respective src_dirs for them, in that order,
%% for every app found.
%%
%% The function returns the src directories since they might have been
%% detected in a top-level loop and we want to skip further detection
%% starting now.
-spec app_dirs([file:name()], [file:name()], rebar_state:t()) -> [{file:name(), [file:name()]}].
app_dirs(LibDir, SrcDirs, State) ->
    Extensions = rebar_state:get(State, application_resource_extensions, ?DEFAULT_APP_RESOURCE_EXT),
    Paths = lists:append([
        [filename:join([LibDir, SrcDir, "*" ++ Ext]) || Ext <- Extensions ]
        || SrcDir <- SrcDirs
    ]),

    EbinPath = filename:join([LibDir, "ebin", "*.app"]),
    MixExsPath = filename:join([LibDir, "mix.exs"]),

    lists:usort(lists:foldl(fun(Path, Acc) ->
                                Files = filelib:wildcard(rebar_utils:to_list(Path)),
                                [{app_dir(File), SrcDirs}
                                 || File <- Files] ++ Acc
                            end, [], [EbinPath, MixExsPath | Paths])).

%% @doc find all apps that haven't been built in a list of directories
-spec find_unbuilt_apps([file:filename_all()], rebar_state:t()) -> [rebar_app_info:t()].
find_unbuilt_apps(LibDirs, State) ->
    find_apps(LibDirs, invalid, State).

%% @doc for each directory passed, find all apps that are valid.
%% Returns all the related app info records.
-spec find_apps([file:filename_all()], rebar_state:t()) -> [rebar_app_info:t()].
find_apps(LibDirs, State) ->
    find_apps(LibDirs, valid, State).

%% @doc for each directory passed, find all apps according
%% to the validity rule passed in. Returns all the related
%% app info records.
-spec find_apps([file:filename_all()], valid | invalid | all, rebar_state:t()) -> [rebar_app_info:t()].
find_apps(LibDirs, Validate, State) ->
    rebar_utils:filtermap(
      fun({AppDir, AppSrcDirs}) ->
            find_app(rebar_app_info:new(), AppDir, AppSrcDirs, Validate, State)
      end,
      all_app_dirs(LibDirs, State)
    ).

%% @doc for each directory passed, with the configured source directories,
%% find all apps according to the validity rule passed in.
%% Returns all the related app info records.
-spec find_apps([file:filename_all()], [file:filename_all()], valid | invalid | all, rebar_state:t()) -> [rebar_app_info:t()].
find_apps(LibDirs, SrcDirs, Validate, State) ->
    rebar_utils:filtermap(
      fun({AppDir, AppSrcDirs}) ->
            find_app(rebar_app_info:new(), AppDir, AppSrcDirs, Validate, State)
      end,
      all_app_dirs(LibDirs, SrcDirs, State)
    ).

%% @doc check that a given app in a directory is there, and whether it's
%% valid or not based on the second argument. Returns the related
%% app info record.
-spec find_app(file:filename_all(), valid | invalid | all, rebar_state:t()) -> {true, rebar_app_info:t()} | false.
find_app(AppDir, Validate, State) ->
    {Config, SrcDirs} = find_config_src(AppDir, ["src"]),
    AppInfo = rebar_app_info:update_opts(rebar_app_info:dir(rebar_app_info:new(), AppDir),
                                         dict:new(), Config),
    find_app_(AppInfo, AppDir, SrcDirs, Validate, State).

%% @doc check that a given app in a directory is there, and whether it's
%% valid or not based on the second argument. Returns the related
%% app info record.
-spec find_app(rebar_app_info:t(), file:filename_all(), valid | invalid | all, rebar_state:t()) ->
    {true, rebar_app_info:t()} | false.
find_app(AppInfo, AppDir, Validate, State) ->
    %% if no src dir is passed, figure it out from the app info, with a default
    %% of src/
    AppOpts = rebar_app_info:opts(AppInfo),
    SrcDirs = rebar_dir:src_dirs(AppOpts, ["src"]),
    find_app_(AppInfo, AppDir, SrcDirs, Validate, State).

%% @doc check that a given app in a directory is there, and whether it's
%% valid or not based on the second argument. The third argument includes
%% the directories where source files can be located. Returns the related
%% app info record.
-spec find_app(rebar_app_info:t(), file:filename_all(),
               [file:filename_all()], valid | invalid | all, rebar_state:t()) ->
    {true, rebar_app_info:t()} | false.
find_app(AppInfo, AppDir, SrcDirs, Validate, State) ->
    AppInfo1 = case ec_file:real_dir_path(rebar_dir:root_dir(State)) of
                   AppDir ->
                       Opts = rebar_state:opts(State),
                       rebar_app_info:default(rebar_app_info:opts(AppInfo, Opts), Opts);
                   _ ->
                       Config = rebar_config:consult(AppDir),
                       rebar_app_info:update_opts(AppInfo, rebar_app_info:opts(AppInfo), Config)
               end,
    find_app_(AppInfo1, AppDir, SrcDirs, Validate, State).

-spec find_app_(rebar_app_info:t(), file:filename_all(),
               [file:filename_all()], valid | invalid | all,  rebar_state:t()) ->
    {true, rebar_app_info:t()} | false.
find_app_(AppInfo, AppDir, SrcDirs, Validate, State) ->
    Extensions = rebar_state:get(State, application_resource_extensions, ?DEFAULT_APP_RESOURCE_EXT),
    NormSrcDirs = [case SrcDir of
                       {ActualSrcDir, _Opts} -> ActualSrcDir;
                       _ -> SrcDir
                   end || SrcDir <- SrcDirs],
    ResourceFiles = [
        {app, filelib:wildcard(filename:join([AppDir, "ebin", "*.app"]))},
        {mix_exs, filelib:wildcard(filename:join([AppDir, "mix.exs"]))}
        | [{extension_type(Ext),
            lists:append([filelib:wildcard(filename:join([AppDir, SrcDir, "*" ++ Ext]))
                          || SrcDir <- NormSrcDirs])}
           || Ext <- Extensions]
    ],
    FlattenedResourceFiles = flatten_resource_files(ResourceFiles),
    try_handle_resource_files(AppInfo, AppDir, FlattenedResourceFiles, Validate).

-spec extension_type(string()) -> app_resource_type().
extension_type(Extension) ->
    Mapping = [
        {".app", app},
        {".src", app_src},
        {".script", script},
        {".exs", mix_exs}
    ],
    extension_type(Mapping, Extension).

-spec extension_type([{string(), Type}], string()) -> Type
    when Type :: app_resource_type().
extension_type([], _) ->
    %% default to app_src
    app_src;
extension_type([{Pattern, Type} | Rest], Extension) ->
    case lists:suffix(Pattern, Extension) of
        true ->
            Type;
        false ->
            extension_type(Rest, Extension)
    end.

-spec flatten_resource_files(ResourceFiles) -> FlattenedResourceFiles
    when ResourceFiles :: [{app_resource_type(), [file:filename()]}],
         FlattenedResourceFiles :: [{app_resource_type(), file:filename()}].
flatten_resource_files(ResourceFiles) ->
    {Flattened, _} =
        lists:foldl(
            fun flatten_resource_impl/2,
            {[], []},
            ResourceFiles),
    lists:reverse(Flattened).

flatten_resource_impl({Type, Files}, Acc = {ResAcc, Used}) ->
    NewFiles = [F || F <- Files, not lists:member(F, Used)],
    case NewFiles of
        [] -> Acc;
        [File] -> {[{Type, File} | ResAcc], [File | Used]};
        Others -> throw({error, {multiple_app_files, Others}})
    end.

%% @doc find the directory that an appfile has
-spec app_dir(file:filename()) -> file:filename().
app_dir(AppFile) ->
    filename:join(rebar_utils:droplast(filename:split(filename:dirname(AppFile)))).

%% @doc populates an app info record based on an app directory and its
%% app file.
-spec create_app_info(rebar_app_info:t(), file:name(), file:name()) -> rebar_app_info:t().
create_app_info(AppInfo, AppDir, AppFile) ->
    try rebar_config:consult_app_file(AppFile) of
        [{application, AppName, AppDetails}] ->
            AppVsn = proplists:get_value(vsn, AppDetails),
            Applications = proplists:get_value(applications, AppDetails, []),
            IncludedApplications = proplists:get_value(included_applications, AppDetails, []),
            OptionalApplications = proplists:get_value(optional_applications, AppDetails, []),
            AppInfo1 = rebar_app_info:name(
                         rebar_app_info:vsn(
                           rebar_app_info:original_vsn(
                             rebar_app_info:dir(AppInfo, AppDir), AppVsn), AppVsn), AppName),
            AppInfo2 = rebar_app_info:applications(
                         rebar_app_info:app_details(AppInfo1, AppDetails), Applications),
            AppInfo3 = rebar_app_info:included_applications(AppInfo2, IncludedApplications),
            AppInfo4 = rebar_app_info:optional_applications(AppInfo3, OptionalApplications),
            Valid = case rebar_app_utils:validate_application_info(AppInfo4) =:= true
                        andalso rebar_app_info:has_all_artifacts(AppInfo4) =:= true of
                        true ->
                            true;
                        _ ->
                            false
                    end,
            rebar_app_info:dir(rebar_app_info:valid(AppInfo4, Valid), AppDir);
        _Invalid ->
            throw({error, {?MODULE, {cannot_read_app_file, AppFile}}})
    catch
        throw:{error, {rebar_file_utils, Err = {bad_term_file, _File, _Reason}}} ->
            throw({error, {?MODULE, Err}}) % wrap this
    end.


%% @doc Read in and parse the .app file if it is available. Do the same for
%% the .app.src file if it exists.
-spec try_handle_resource_files(AppInfo, AppDir, ResourceFiles, valid | invalid | all) ->
    {true, AppInfo} | false when
      AppInfo :: rebar_app_info:t(),
      AppDir :: file:filename(),
      ResourceFiles :: [{app_resource_type(), file:filename()}].
try_handle_resource_files(AppInfo, AppDir, [{app, AppFile} | Rest], Validate) ->
    AppSrcFile = proplists:get_value(app_src, Rest),
    try_handle_app_file(AppInfo, AppDir, AppFile, AppSrcFile, Validate);
try_handle_resource_files(AppInfo, AppDir, [{Type, AppSrcFile} | _Rest], Validate)
    when Type =:= app_src orelse Type =:= script ->
    try_handle_app_src_file(AppInfo, AppDir, AppSrcFile, Validate);
try_handle_resource_files(AppInfo, AppDir, [{mix_exs, _MixExs} | Rest], Validate) ->
    %% prefer a rebar3 buildable app if both are found
    case try_handle_resource_files(AppInfo, AppDir, Rest, Validate) of
        false ->
            {true, rebar_app_info:project_type(AppInfo, mix)};
        {true, _}=Result ->
            Result
    end;
try_handle_resource_files(_AppInfo, _AppDir, [], _Validate) ->
    false.


%% @doc Read in and parse the .app file if it is available. Do the same for
%% the .app.src file if it exists.
-spec try_handle_app_file(AppInfo, AppDir, File, AppSrcFile, valid | invalid | all) ->
    {true, AppInfo} | false when
      AppInfo :: rebar_app_info:t(),
      AppDir :: file:filename(),
      File :: file:filename(),
      AppSrcFile :: file:filename().
try_handle_app_file(AppInfo0, AppDir, File, AppSrcFile, Validate) ->
    try create_app_info(AppInfo0, AppDir, File) of
        AppInfo ->
            AppInfo1 = rebar_app_info:app_file(AppInfo, File),
            AppInfo2 = rebar_app_info:app_file_src(AppInfo1, AppSrcFile),
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
            try_handle_app_src_file(AppInfo0, AppDir, AppSrcFile, Validate)
    end.

%% @doc Read in the .app.src file if we aren't looking for a valid (already
%% built) app.
-spec try_handle_app_src_file(AppInfo, AppDir, AppSrcFile, valid | invalid | all) ->
    {true, AppInfo} | false when
      AppInfo :: rebar_app_info:t(),
      AppDir :: file:filename(),
      AppSrcFile :: file:filename().
try_handle_app_src_file(_AppInfo, _AppDir, undefined, valid) ->
    false;
try_handle_app_src_file(_AppInfo, _AppDir, _AppSrcFile, valid) ->
    false;
try_handle_app_src_file(AppInfo, AppDir, AppSrcFile, _) ->
    AppInfo1 = rebar_app_info:app_file(AppInfo, undefined),
    AppInfo2 = create_app_info(AppInfo1, AppDir, AppSrcFile),
    case filename:extension(AppSrcFile) of
        ".script" ->
            {true, rebar_app_info:app_file_src_script(AppInfo2, AppSrcFile)};
        _ ->
            {true, rebar_app_info:app_file_src(AppInfo2, AppSrcFile)}
    end.


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
            {[], Default};
        Terms ->
            %% TODO: handle profiles I guess, but we don't have that info
            {Terms, proplists:get_value(src_dirs, Terms, Default)}
    end.
