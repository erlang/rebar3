-module(rlx_resolve).

-export([solve_release/2,
         format_error/1]).

-include("relx.hrl").
-include("rlx_log.hrl").

-spec format_error(ErrorDetail::term()) -> iolist().
format_error({no_goals_specified, {RelName, RelVsn}}) ->
    io_lib:format("No applications configured to be included in release ~s-~s", [RelName, RelVsn]);
format_error({release_erts_error, Dir}) ->
    io_lib:format("Unable to find erts in ~s", [Dir]);
format_error({app_not_found, AppName, undefined}) ->
    io_lib:format("Application needed for release not found: ~p", [AppName]);
format_error({app_not_found, AppName, AppVsn}) ->
    io_lib:format("Application needed for release not found: ~p-~s", [AppName, AppVsn]).

solve_release(Release, State0) ->
    RelName = rlx_release:name(Release),
    RelVsn = rlx_release:vsn(Release),
    ?log_debug("Solving Release ~p-~s", [RelName, RelVsn]),
    AllApps = rlx_state:available_apps(State0),

    %% get per release config values and override the State with them
    Config = rlx_release:config(Release),
    {ok, State1} = lists:foldl(fun rlx_config:load/2, {ok, State0}, Config),
    case rlx_release:goals(Release) of
        [] ->
            erlang:error(?RLX_ERROR({no_goals_specified, {RelName, RelVsn}}));
        Goals ->
            LibDirs = rlx_state:lib_dirs(State1),


            {CheckCodeLibDirs, LibDirs1} =
                case rlx_state:system_libs(State0) of
                    B when is_boolean(B) ->
                        %% even if we don't include system libs (`system_libs' being false)
                        %% here we still want to check for the system apps in the code path.
                        %% so return `true' for `CheckCodeLibDirs'
                        {true, LibDirs};
                    SystemLibs ->
                        ?log_debug("System libs dir to search for apps ~ts", [SystemLibs]),
                        {false, [SystemLibs | LibDirs]}
                end,

            ExcludeApps = rlx_state:exclude_apps(State1),
            Pkgs = subset(Goals, AllApps, LibDirs1, CheckCodeLibDirs, ExcludeApps),
            set_resolved(Release, Pkgs, State1)
    end.

%% find the app_info records for each application and its deps needed for the release
subset(Goals, World, LibDirs, CheckCodeLibDirs, ExcludeApps) ->
    {Apps, _} = fold_apps(Goals, World, sets:new(), LibDirs, CheckCodeLibDirs, [], ExcludeApps),
    Apps.

subset(Goal, World, Seen, LibDirs, CheckCodeLibDirs, OptionalApplications, ExcludeApps) ->
    {Name, Vsn} = name_version(Goal),
    case sets:is_element(Name, Seen) of
        true ->
            {[], Seen};
        _ ->
            case find_app(Name, Vsn, World, LibDirs, CheckCodeLibDirs) of
                not_found ->
                    case lists:member(Name, OptionalApplications) of
                        true ->
                            %% don't add to Seen since optional applications are only
                            %% per-application and not global, so another app could
                            %% depend on this dependency
                           {[], Seen};
                        false ->
                            erlang:error(?RLX_ERROR({app_not_found, Name, Vsn}))
                    end;
                AppInfo=#{applications := Applications,
                          included_applications := IncludedApplications,
                          optional_applications := OptionalApplications0} ->
                    {Apps, Seen2} = fold_apps(Applications ++ IncludedApplications ++ OptionalApplications0,
                                              World,
                                              sets:add_element(Name, Seen),
                                              LibDirs,
                                              CheckCodeLibDirs,
                                              OptionalApplications0,
                                              ExcludeApps),

                    %% don't add excluded apps
                    %% TODO: should an excluded app's deps also be excluded?
                    case lists:member(Name, ExcludeApps) of
                        true ->
                            {Apps, Seen2};
                        false ->
                            %% place the deps of the App before it
                            {Apps ++ [AppInfo], Seen2}
                    end
            end
    end.

fold_apps(Apps, World, Seen, LibDirs, CheckCodeLibDirs, OptionalApplications, ExcludeApps) ->
    lists:foldl(fun(App, {AppAcc, SeenAcc}) ->
                        {NewApps, SeenAcc1} = subset(App, World, SeenAcc, LibDirs,
                                                     CheckCodeLibDirs, OptionalApplications, ExcludeApps),
                        %% put new apps after the existing list to keep the user defined order
                        {AppAcc ++ NewApps, SeenAcc1}
                end, {[], Seen}, Apps).

set_resolved(Release0, Pkgs, State) ->
    case rlx_release:realize(Release0, Pkgs) of
        {ok, Release1} ->
            ?log_debug("Resolved ~p-~s", [rlx_release:name(Release1), rlx_release:vsn(Release1)]),
            ?log_debug("~s", [rlx_release:format(Release1)]),
            case rlx_state:include_erts(State) of
                IncludeErts when is_atom(IncludeErts) ->
                    {ok, Release1, rlx_state:add_realized_release(State, Release1)};
                ErtsDir ->
                    try
                        %% figure out erts version from the path given
                        [Erts | _] = filelib:wildcard(filename:join(ErtsDir, "erts-*")),
                        [_, ErtsVsn] = rlx_string:lexemes(filename:basename(Erts), "-"),
                        Release2 = rlx_release:erts(Release1, ErtsVsn),
                        {ok, Release2, rlx_state:add_realized_release(State, Release2)}
                    catch
                        _:_ ->
                            erlang:error(?RLX_ERROR({release_erts_error, ErtsDir}))
                    end
            end
    end.

name_version(Name) when is_atom(Name) ->
    {Name, undefined};
name_version({Name, #{vsn := Vsn}}) ->
    {Name, Vsn}.

%% Applications are first searched for in the `Apps' variable which is a map
%% of application name to `rlx_app_info' map. This variable is passed to `relx'
%% function calls to build the release or tarball
%%
%% If the application isn't found in the `Apps' map then the `lib_dirs' list of
%% directories is searched for the application under `<dir>/*/ebin/Name.app'.
%% Lastly, if the application still isn't found then the code path is checked
%% using `code:lib_dir'.
find_app(Name, Vsn, Apps, LibDirs, CheckCodeLibDirs) ->
    case maps:find(Name, Apps) of
        {ok, AppInfo} ->
            %% verify the app is the version we want and if not try
            %% finding the app-vsn needed in the configured paths
            case check_app(Name, Vsn, AppInfo) of
                true ->
                    AppInfo;
                false ->
                    search_for_app(Name, Vsn, LibDirs, CheckCodeLibDirs)
            end;
        error ->
            search_for_app(Name, Vsn, LibDirs, CheckCodeLibDirs)
    end.

search_for_app(Name, Vsn, LibDirs, CheckCodeLibDirs) ->
    case find_app_in_dir(Name, Vsn, LibDirs) of
        not_found when CheckCodeLibDirs =:= true ->
            find_app_in_code_path(Name, Vsn);
        not_found when CheckCodeLibDirs =:= false ->
            %% app not found in any lib dir we are configured to search
            %% and user set a custom `system_libs' directory so we do
            %% not look in `code:lib_dir'
            not_found;
        AppInfo ->
            case check_app(Name, Vsn, AppInfo) of
                true ->
                    AppInfo;
                false when CheckCodeLibDirs =:= true  ->
                    find_app_in_code_path(Name, Vsn)
            end
    end.

find_app_in_dir(_Name, _Vsn, []) ->
    not_found;
find_app_in_dir(Name, Vsn, [Dir | Rest]) ->
    AppFile = filename:join([Dir, "*", "ebin", [Name, ".app"]]),
    case filelib:wildcard(AppFile) of
        [] ->
            find_app_in_dir(Name, Vsn, Rest);
        Matches ->
            case rlx_util:list_search(fun(AppFilePath) ->
                                              to_app(Name, Vsn, AppFilePath)
                                      end, Matches) of
                {value, App} ->
                    App;
                false ->
                    find_app_in_dir(Name, Vsn, Rest)
            end
    end.

find_app_in_code_path(Name, Vsn) ->
    case code:lib_dir(Name) of
        {error, bad_name} ->
            not_found;
        Dir ->
            case to_app(Name, Vsn, filename:join([Dir, "ebin", [Name, ".app"]])) of
                {true, AppInfo} ->
                    AppInfo;
                false ->
                    not_found
            end
    end.

%% returns true if the AppInfo is the same name and version as the
%% arguments where `undefined' means any version.
check_app(Name, undefined, App) ->
    rlx_app_info:name(App) =:= Name;
check_app(Name, Vsn, App) ->
    rlx_app_info:name(App) =:= Name
        andalso rlx_app_info:vsn(App) =:= Vsn.

to_app(Name, Vsn, AppFilePath) ->
    AppData = case file:consult(AppFilePath) of
                  {ok, [{application, _Name, Data}]} -> Data;
                  Other -> erlang:error(?RLX_ERROR({bad_app_file, AppFilePath, Other}))
              end,
    Applications = proplists:get_value(applications, AppData, []),
    IncludedApplications = proplists:get_value(included_applications, AppData, []),
    OptionalApplications = proplists:get_value(optional_applications, AppData, []),

    case lists:keyfind(vsn, 1, AppData) of
        {_, Vsn1} when Vsn =:= undefined ;
                       Vsn =:= Vsn1 ->
            {true, #{name => Name,
                     vsn => Vsn1,

                     applications => Applications,
                     included_applications => IncludedApplications,
                     optional_applications => OptionalApplications,

                     dir => filename:dirname(filename:dirname(AppFilePath)),
                     link => false}};
        _ ->
            false
    end.
