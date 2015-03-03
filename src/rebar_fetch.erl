%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% -------------------------------------------------------------------
-module(rebar_fetch).

-export([lock_source/2,
         download_source/3,
         needs_update/2]).

-export([format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

%% map short versions of resources to module names
-define(RESOURCES, [{git, rebar_git_resource}, {pkg, rebar_pkg_resource},
                    {hg, rebar_hg_resource}]).

-spec lock_source(file:filename_all(), rebar_resource:resource()) ->
                         rebar_resource:resource() | {error, string()}.
lock_source(AppDir, Source) ->
    Module = get_resource_type(Source),
    Module:lock(AppDir, Source).

-spec download_source(file:filename_all(), rebar_resource:resource(), rebar_state:t()) ->
                             true | {error, any()}.
download_source(AppDir, Source, State) ->
    try
        Module = get_resource_type(Source),
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
            {tarball, File} ->
                Contents = filename:join(TmpDir, "contents"),
                ec_file:mkdir_p(AppDir1),
                ec_file:mkdir_p(Contents),
                ok = erl_tar:extract(File, [{cwd, TmpDir}]),
                ok = erl_tar:extract(filename:join(TmpDir, "contents.tar.gz"),
                                    [{cwd, Contents}, compressed]),
                code:del_path(filename:absname(filename:join(AppDir1, "ebin"))),
                ec_file:remove(filename:absname(AppDir1), [recursive]),

                ?DEBUG("Moving contents ~p to ~p", [Contents, filename:absname(AppDir1)]),
                ok = rebar_file_utils:mv(Contents, filename:absname(AppDir1)),

                ?DEBUG("Removing tmp dir ~p", [TmpDir]),
                ec_file:remove(TmpDir, [recursive]),
                true
        end
    catch
        C:T ->
            ?DEBUG("rebar_fetch exception ~p ~p ~p", [C, T, erlang:get_stacktrace()]),
            throw(?PRV_ERROR({fetch_fail, Source}))
    end.

-spec needs_update(file:filename_all(), rebar_resource:resource()) -> boolean() | {error, string()}.
needs_update(AppDir, Source) ->
    Module = get_resource_type(Source),
    try
        Module:needs_update(AppDir, Source)
    catch
        _:_ ->
            true
    end.

format_error({fetch_fail, Source}) ->
    io_lib:format("Failed to fetch and copy dep: ~p", [Source]).

get_resource_type({Type, Location}) ->
    find_resource_module(Type, Location);
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
