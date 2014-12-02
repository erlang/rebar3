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

-export([find/2,
         find/3,
         is_app_dir/0, is_app_dir/1,
         is_app_src/1,
         app_src_to_app/1,
         app_name/2,
         app_applications/2,
         app_vsn/2]).

-export([load_app_file/2]). % TEMPORARY

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec find(binary(), [rebar_app_info:t()]) -> {ok, rebar_app_info:t()} | error.
find(Name, Apps) ->
    ec_lists:find(fun(App) -> rebar_app_info:name(App) =:= Name end, Apps).

-spec find(binary(), binary(), [rebar_app_info:t()]) -> {ok, rebar_app_info:t()} | error.
find(Name, Vsn, Apps) ->
    ec_lists:find(fun(App) ->
                          rebar_app_info:name(App) =:= Name
                              andalso rebar_app_info:original_vsn(App) =:= Vsn
                  end, Apps).

-spec is_app_dir() -> {true, file:name()} | false.
is_app_dir() ->
    is_app_dir(rebar_dir:get_cwd()).

-spec is_app_dir(file:name()) -> {true, file:name()} | false.
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
    Path = filename:join(rebar_utils:droplast(filename:split(filename:dirname(Filename)))),
    AppFile = filename:join([Path, "ebin", filename:basename(Filename, ".app.src") ++ ".app"]),
    filelib:ensure_dir(AppFile),
    AppFile.

app_name(Config, AppFile) ->
    case load_app_file(Config, AppFile) of
        {ok, NewConfig, AppName, _} ->
            {NewConfig, AppName};
        {error, Reason} ->
            ?ABORT("Failed to extract name from ~s: ~p\n",
                   [AppFile, Reason])
    end.

app_applications(Config, AppFile) ->
    case load_app_file(Config, AppFile) of
        {ok, NewConfig, _, AppInfo} ->
            {NewConfig, get_value(applications, AppInfo, AppFile)};
        {error, Reason} ->
            ?ABORT("Failed to extract applications from ~s: ~p\n",
                   [AppFile, Reason])
    end.

app_vsn(Config, AppFile) ->
    case load_app_file(Config, AppFile) of
        {ok, Config1, _, AppInfo} ->
            AppDir = filename:dirname(filename:dirname(AppFile)),
            rebar_utils:vcs_vsn(Config1, get_value(vsn, AppInfo, AppFile),
                                AppDir);
        {error, Reason} ->
            ?ABORT("Failed to extract vsn from ~s: ~p\n",
                   [AppFile, Reason])
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

load_app_file(State, Filename) ->
    AppFile = {app_file, Filename},
    case rebar_state:get(State, {appfile, AppFile}, undefined) of
        undefined ->
            case consult_app_file(Filename) of
                {ok, [{application, AppName, AppData}]} ->
                    State1 = rebar_state:set(State,
                                             {appfile, AppFile},
                                             {AppName, AppData}),
                    {ok, State1, AppName, AppData};
                {error, _} = Error ->
                    Error;
                Other ->
                    {error, {unexpected_terms, Other}}
            end;
        {AppName, AppData} ->
            {ok, State, AppName, AppData}
    end.

%% In the case of *.app.src we want to give the user the ability to
%% dynamically script the application resource file (think dynamic version
%% string, etc.), in a way similar to what can be done with the rebar
%% config. However, in the case of *.app, rebar should not manipulate
%% that file. This enforces that dichotomy between app and app.src.
consult_app_file(Filename) ->
    case lists:suffix(".app.src", Filename) of
        false ->
            file:consult(Filename);
        true ->
            {ok, rebar_config:consult_file(Filename)}
    end.

get_value(Key, AppInfo, AppFile) ->
    case proplists:get_value(Key, AppInfo) of
        undefined ->
            ?ABORT("Failed to get app value '~p' from '~s'~n", [Key, AppFile]);
        Value ->
            Value
    end.
