%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% -------------------------------------------------------------------
-module(rebar_fetch).

-export([lock_source/2,
         download_source/2]).

-include("rebar.hrl").

lock_source(AppDir, Source) ->
    rebar_git_resource:lock(AppDir, Source).

download_source(AppDir, Source) ->
    TmpDir = ec_file:insecure_mkdtemp(),
    AppDir1 = ec_cnv:to_list(AppDir),
    ec_file:mkdir_p(AppDir1),
    case rebar_git_resource:download(TmpDir, Source) of
        {ok, _} ->
            ok = ec_file:copy(TmpDir, filename:absname(AppDir1), [recursive]);
        {tarball, File} ->
            ok = erl_tar:extract(File, [{cwd, TmpDir}
                                       ,compressed]),
            BaseName = filename:basename(AppDir1),
            [FromDir] = filelib:wildcard(filename:join(TmpDir, BaseName++"-*")),
            ec_file:copy(FromDir, AppDir1, [recursive])
    end.
