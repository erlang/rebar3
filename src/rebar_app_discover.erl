-module(rebar_app_discover).

-export([do/2,
         find_apps/1]).

do(State, LibDirs) ->
    Apps = find_apps(LibDirs),
    lists:foldl(fun(AppInfo, StateAcc) ->
                        rebar_state:add_app(StateAcc, AppInfo)
            end, State, Apps).

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
                                    Files = filelib:wildcard(Path),
                                    [app_dir(File) || File <- Files] ++ Acc
                            end, [], [Path1, Path2, Path3, Path4])).

find_apps(LibDirs) ->
    lists:map(fun(AppDir) ->
                      AppFile = filelib:wildcard(filename:join([AppDir, "ebin", "*.app"])),
                      AppSrcFile = filelib:wildcard(filename:join([AppDir, "src", "*.app.src"])),
                      case AppFile of
                          [File] ->
                              AppInfo = create_app_info(AppDir, File),
                              AppInfo1 = rebar_app_info:app_file(AppInfo, File),
                              case AppSrcFile of
                                  [F] ->
                                      rebar_app_info:app_file_src(AppInfo1, F);
                                  [] ->
                                      AppInfo1
                              end;
                          [] ->
                              case AppSrcFile of
                                  [File] ->
                                      AppInfo = create_app_info(AppDir, File),
                                      rebar_app_info:app_file_src(AppInfo, File);
                                  [] ->
                                      error
                              end
                      end
              end, all_app_dirs(LibDirs)).

app_dir(AppFile) ->
    filename:join(lists:droplast(filename:split(filename:dirname(AppFile)))).

create_app_info(AppDir, AppFile) ->
    case file:consult(AppFile) of
        {ok, [{application, AppName, AppDetails}]} ->
            AppVsn = proplists:get_value(vsn, AppDetails),
            AbsCwd = filename:absname(rebar_utils:get_cwd()),
            {ok, AppInfo} = rebar_app_info:new(AppName, AppVsn, AppDir),
            RebarConfig = filename:join(AppDir, "rebar.config"),
            AppState = case filelib:is_file(RebarConfig) of
                            true ->
                                Terms = rebar_config:consult_file(RebarConfig),
                                rebar_state:new(Terms);
                            false ->
                                rebar_state:new()
                        end,
            AppState1 = rebar_state:set(AppState, base_dir, AbsCwd),
            AppInfo1 = rebar_app_info:config(AppInfo, AppState1),
            rebar_app_info:dir(AppInfo1, AppDir)
    end.
