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

-include("rebar.hrl").

%% map short versions of resources to module names
-define(RESOURCES, [{git, rebar_git_resource}, {pkg, rebar_pkg_resource}]).

-spec lock_source(file:filename_all(), rebar_resource:resource()) ->
                         rebar_resource:resource() | {error, string()}.
lock_source(AppDir, Source) ->
    case get_resource_type(Source) of
        {error, _}=Error ->
            Error;
        Module ->
            Module:lock(AppDir, Source)
    end.

-spec download_source(file:filename_all(), rebar_resource:resource()) -> true | {error, any()}.
download_source(AppDir, Source) ->
    case get_resource_type(Source) of
        {error, _}=Error ->
            Error;
        Module ->
            TmpDir = ec_file:insecure_mkdtemp(),
            AppDir1 = ec_cnv:to_list(AppDir),
            ec_file:mkdir_p(AppDir1),
            case Module:download(TmpDir, Source) of
                {ok, _} ->
                    code:del_path(filename:absname(filename:join(AppDir1, "ebin"))),
                    ec_file:remove(filename:absname(AppDir1), [recursive]),
                    ok = ec_file:copy(TmpDir, filename:absname(AppDir1), [recursive]),
                    true;
                {tarball, File} ->
                    ok = erl_tar:extract(File, [{cwd, TmpDir}
                                               ,compressed]),
                    BaseName = filename:basename(AppDir1),
                    [FromDir] = filelib:wildcard(filename:join(TmpDir, BaseName++"-*")),
                    code:del_path(filename:absname(filename:join(AppDir1, "ebin"))),
                    ec_file:remove(filename:absname(AppDir1), [recursive]),
                    ok = ec_file:copy(FromDir, filename:absname(AppDir1), [recursive]),
                    true
            end
    end.

-spec needs_update(file:filename_all(), rebar_resource:resource()) -> boolean() | {error, string()}.
needs_update(AppDir, Source) ->
    case get_resource_type(Source) of
        {error, _}=Error ->
            Error;
        Module ->
            try
                Module:needs_update(AppDir, Source)
            catch
                _:_ ->
                    true
            end
    end.

get_resource_type({Type, Location, _}) ->
    find_resource_module(Type, Location);
get_resource_type({Type, _, _, Location}) ->
    find_resource_module(Type, Location);
get_resource_type(_) ->
    rebar_pkg_resource.

find_resource_module(Type, Location) ->
    case lists:keyfind(Type, 1, ?RESOURCES) of
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
