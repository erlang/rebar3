%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% -------------------------------------------------------------------
-module(rebar_fetch).

-export([lock_source/2,
         download_source/2,
         needs_update/2]).

-export([format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-spec lock_source(rebar_app_info:t(), rebar_state:t())
                 -> rebar_resource_v2:source() | {error, string()}.
lock_source(AppInfo, State)      ->
    rebar_resource_v2:lock(AppInfo, State).

-spec download_source(rebar_app_info:t(), rebar_state:t())
                     -> rebar_app_info:t() | {error, any()}.
download_source(AppInfo, State)  ->
    AppDir = rebar_app_info:dir(AppInfo),
    try download_source_(AppInfo, State) of
        ok ->
            %% freshly downloaded, update the app info opts to reflect the new config
            Config = rebar_config:consult(AppDir),
            AppInfo1 = rebar_app_info:update_opts(AppInfo, rebar_app_info:opts(AppInfo), Config),
            case rebar_app_discover:find_app(AppInfo1, AppDir, all) of
                {true, AppInfo2} ->
                    rebar_app_info:is_available(AppInfo2, true);
                false ->
                    throw(?PRV_ERROR({dep_app_not_found, rebar_app_info:name(AppInfo1)}))
            end;
        {error, Reason} ->
            throw(?PRV_ERROR(Reason))
    catch
        throw:{no_resource, Type, Location} ->
            throw(?PRV_ERROR({no_resource, Location, Type}));
        ?WITH_STACKTRACE(C,T,S)
            ?DEBUG("rebar_fetch exception ~p ~p ~p", [C, T, S]),
            throw(?PRV_ERROR({fetch_fail, rebar_app_info:source(AppInfo)}))
    end.

download_source_(AppInfo, State) ->
    AppDir = rebar_app_info:dir(AppInfo),
    TmpDir = ec_file:insecure_mkdtemp(),
    AppDir1 = rebar_utils:to_list(AppDir),
    case rebar_resource_v2:download(TmpDir, AppInfo, State) of
        ok ->
            ec_file:mkdir_p(AppDir1),
            code:del_path(filename:absname(filename:join(AppDir1, "ebin"))),
            ok = rebar_file_utils:rm_rf(filename:absname(AppDir1)),
            ?DEBUG("Moving checkout ~p to ~p", [TmpDir, filename:absname(AppDir1)]),
            rebar_file_utils:mv(TmpDir, filename:absname(AppDir1));
        Error ->
            Error
    end.

-spec needs_update(rebar_app_info:t(), rebar_state:t())
                  -> boolean() | {error, string()}.
needs_update(AppInfo, State) ->
    try
        rebar_resource_v2:needs_update(AppInfo, State)
    catch
        _:_ ->
            true
    end.

format_error({fetch_fail, Name, Vsn}) ->
    io_lib:format("Failed to fetch and copy dep: ~ts-~ts", [Name, Vsn]);
format_error({fetch_fail, Source}) ->
    io_lib:format("Failed to fetch and copy dep: ~p", [Source]);
format_error({dep_app_not_found, AppName}) ->
    io_lib:format("Dependency failure: source for ~ts does not contain a "
                  "recognizable project and can not be built", [AppName]).
