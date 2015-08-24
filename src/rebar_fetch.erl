%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% -------------------------------------------------------------------
-module(rebar_fetch).

-export([lock_source/3,
         download_source/3,
         needs_update/3]).

-export([format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-spec lock_source(file:filename_all(), rebar_resource:resource(), rebar_state:t()) ->
                         rebar_resource:resource() | {error, string()}.
lock_source(AppDir, Source, State) ->
    Resources = rebar_state:resources(State),
    Module = get_resource_type(Source, Resources),
    Module:lock(AppDir, Source).

-spec download_source(file:filename_all(), rebar_resource:resource(), rebar_state:t()) ->
                             true | {error, any()}.
download_source(AppDir, Source, State) ->
    try download_source_(AppDir, Source, State) of
        true ->
            true;
        Error ->
            throw(?PRV_ERROR(Error))
    catch
        C:T ->
            ?DEBUG("rebar_fetch exception ~p ~p ~p", [C, T, erlang:get_stacktrace()]),
            throw(?PRV_ERROR({fetch_fail, Source}))
    end.

download_source_(AppDir, Source, State) ->
    Resources = rebar_state:resources(State),
    Module = get_resource_type(Source, Resources),
    TmpDir = ec_file:insecure_mkdtemp(),
    AppDir1 = ec_cnv:to_list(AppDir),
    case Module:download(TmpDir, Source, State) of
        {ok, _} ->
            ec_file:mkdir_p(AppDir1),
            code:del_path(filename:absname(filename:join(AppDir1, "ebin"))),
            ec_file:remove(filename:absname(AppDir1), [recursive]),
            ?DEBUG("Moving checkout ~p to ~p", [TmpDir, filename:absname(AppDir1)]),
            ok = rebar_file_utils:mv(TmpDir, filename:absname(AppDir1)),
            true;
        Error ->
            Error
    end.

-spec needs_update(file:filename_all(), rebar_resource:resource(), rebar_state:t()) -> boolean() | {error, string()}.
needs_update(AppDir, Source, State) ->
    Resources = rebar_state:resources(State),
    Module = get_resource_type(Source, Resources),
    try
        Module:needs_update(AppDir, Source)
    catch
        _:_ ->
            true
    end.

format_error({bad_download, CachePath}) ->
    io_lib:format("Download of package does not match md5sum from server: ~s", [CachePath]);
format_error({failed_extract, CachePath}) ->
    io_lib:format("Failed to extract package: ~s", [CachePath]);
format_error({bad_etag, Source}) ->
    io_lib:format("MD5 Checksum comparison failed for: ~s", [Source]);
format_error({fetch_fail, Name, Vsn}) ->
    io_lib:format("Failed to fetch and copy dep: ~s-~s", [Name, Vsn]);
format_error({fetch_fail, Source}) ->
    io_lib:format("Failed to fetch and copy dep: ~p", [Source]);
format_error({bad_checksum, File}) ->
    io_lib:format("Checksum mismatch against tarball in ~s", [File]);
format_error({bad_registry_checksum, File}) ->
    io_lib:format("Checksum mismatch against registry in ~s", [File]).

get_resource_type({Type, Location}, Resources) ->
    find_resource_module(Type, Location, Resources);
get_resource_type({Type, Location, _}, Resources) ->
    find_resource_module(Type, Location, Resources);
get_resource_type({Type, _, _, Location}, Resources) ->
    find_resource_module(Type, Location, Resources);
get_resource_type(_, _) ->
    rebar_pkg_resource.

find_resource_module(Type, Location, Resources) ->
    case lists:keyfind(Type, 1, Resources) of
        false ->
            case code:which(Type) of
                non_existing ->
                    {error, io_lib:format("Cannot handle dependency ~s.~n"
                                         "     No module for resource type ~p", [Location, Type])};
                _ ->
                    Type
            end;
        {Type, Module} ->
            Module
    end.
