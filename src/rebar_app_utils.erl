%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_app_utils).

-export([is_app_dir/0, is_app_dir/1,
         is_app_src/1,
         app_src_to_app/1,
         app_name/1,
         app_applications/1,
         app_vsn/1]).

-export([load_app_file/1]). % TEMPORARY

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

is_app_dir() ->
    is_app_dir(rebar_utils:get_cwd()).

is_app_dir(Dir) ->
    SrcDir = filename:join([Dir, "src"]),
    AppSrc = filename:join([SrcDir, "*.app.src"]),
    case filelib:wildcard(AppSrc) of
        [AppSrcFile] ->
            {true, AppSrcFile};
        [] ->
            EbinDir = filename:join([Dir, "ebin"]),
            App = filename:join([EbinDir, "*.app"]),
            case filelib:wildcard(App) of
                [AppFile] ->
                    {true, AppFile};
                [] ->
                    false;
                _ ->
                    ?ERROR("More than one .app file in ~s~n", [EbinDir]),
                    false
            end;
        _ ->
            ?ERROR("More than one .app.src file in ~s~n", [SrcDir]),
            false
    end.


is_app_src(Filename) ->
    %% If removing the extension .app.src yields a shorter name,
    %% this is an .app.src file.
    Filename =/= filename:rootname(Filename, ".app.src").

app_src_to_app(Filename) ->
    filename:join("ebin", filename:basename(Filename, ".app.src") ++ ".app").

app_name(AppFile) ->
    case load_app_file(AppFile) of
        {ok, AppName, _} ->
            AppName;
        {error, Reason} ->
            ?ABORT("Failed to extract name from ~s: ~p\n",
                   [AppFile, Reason])
    end.

app_applications(AppFile) ->
    case load_app_file(AppFile) of
        {ok, _, AppInfo} ->
            get_value(applications, AppInfo, AppFile);
        {error, Reason} ->
            ?ABORT("Failed to extract applications from ~s: ~p\n",
                   [AppFile, Reason])
    end.

app_vsn(AppFile) ->
    case load_app_file(AppFile) of
        {ok, _, AppInfo} ->
            AppDir = filename:dirname(filename:dirname(AppFile)),
            vcs_vsn(get_value(vsn, AppInfo, AppFile), AppDir);
        {error, Reason} ->
            ?ABORT("Failed to extract vsn from ~s: ~p\n",
                   [AppFile, Reason])
    end.


%% ===================================================================
%% Internal functions
%% ===================================================================

load_app_file(Filename) ->
    AppFile = {app_file, Filename},
    case erlang:get(AppFile) of
        undefined ->
            case file:consult(Filename) of
                {ok, [{application, AppName, AppData}]} ->
                    erlang:put(AppFile, {AppName, AppData}),
                    {ok, AppName, AppData};
                {error, _} = Error ->
                    Error;
                Other ->
                    {error, {unexpected_terms, Other}}
            end;
        {AppName, AppData} ->
            {ok, AppName, AppData}
    end.

get_value(Key, AppInfo, AppFile) ->
    case proplists:get_value(Key, AppInfo) of
        undefined ->
            ?ABORT("Failed to get app value '~p' from '~s'~n", [Key, AppFile]);
        Value ->
            Value
    end.

vcs_vsn(Vcs, Dir) ->
    case vcs_vsn_cmd(Vcs) of
        {unknown, VsnString} ->
            ?DEBUG("vcs_vsn: Unknown VCS atom in vsn field: ~p\n", [Vcs]),
            VsnString;
        Cmd ->
            %% If there is a valid VCS directory in the application directory,
            %% use that version info
            Extension = lists:concat([".", Vcs]),
            case filelib:is_dir(filename:join(Dir, Extension)) of
                true ->
                    ?DEBUG("vcs_vsn: Primary vcs used for ~s\n", [Dir]),
                    vcs_vsn_invoke(Cmd, Dir);
                false ->
                    %% No VCS directory found for the app. Depending on source
                    %% tree structure, there may be one higher up, but that can
                    %% yield unexpected results when used with deps. So, we
                    %% fallback to searching for a priv/vsn.Vcs file.
                    VsnFile = filename:join([Dir, "priv", "vsn" ++ Extension]),
                    case file:read_file(VsnFile) of
                        {ok, VsnBin} ->
                            ?DEBUG("vcs_vsn: Read ~s from priv/vsn.~p\n",
                                   [VsnBin, Vcs]),
                            string:strip(binary_to_list(VsnBin), right, $\n);
                        {error, enoent} ->
                            ?DEBUG("vcs_vsn: Fallback to vcs for ~s\n", [Dir]),
                            vcs_vsn_invoke(Cmd, Dir)
                    end
            end
    end.

vcs_vsn_cmd(git) ->
    %% Explicitly git-describe a committish to accommodate for projects
    %% in subdirs which don't have a GIT_DIR. In that case we will
    %% get a description of the last commit that touched the subdir.
    "git describe --always --tags `git log -n 1 --pretty=format:%h .`";
vcs_vsn_cmd(hg)  -> "hg identify -i";
vcs_vsn_cmd(bzr) -> "bzr revno";
vcs_vsn_cmd(svn) -> "svnversion";
vcs_vsn_cmd(Version) -> {unknown, Version}.

vcs_vsn_invoke(Cmd, Dir) ->
    {ok, VsnString} = rebar_utils:sh(Cmd, [{cd, Dir}, {use_stdout, false}]),
    string:strip(VsnString, right, $\n).
