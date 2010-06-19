%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
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
    AppSrc = filename:join(Dir, "src/*.app.src"),
    case filelib:wildcard(AppSrc) of
        [AppSrcFile] ->
            {true, AppSrcFile};
        _ ->
            App = filename:join([Dir, "ebin/*.app"]),
            case filelib:wildcard(App) of
                [AppFile] ->
                    {true, AppFile};
                _ ->
                    false
            end
    end.


is_app_src(Filename) ->
    %% If removing the extension .app.src yields a shorter name,
    %% this is an .app.src file.
    Filename /= filename:rootname(Filename, ".app.src").

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
            proplists:get_value(applications, AppInfo);
        {error, Reason} ->
            ?ABORT("Failed to extract applications from ~s: ~p\n",
                   [AppFile, Reason])
    end.

app_vsn(AppFile) ->
    case load_app_file(AppFile) of
        {ok, _, AppInfo} ->
            proplists:get_value(vsn, AppInfo);
        {error, Reason} ->
            ?ABORT("Failed to extract vsn from ~s: ~p\n",
                   [AppFile, Reason])
    end.


%% ===================================================================
%% Internal functions
%% ===================================================================

load_app_file(Filename) ->
    case erlang:get({app_file, Filename}) of
        undefined ->
            case file:consult(Filename) of
                {ok, [{application, AppName, AppData}]} ->
                    erlang:put({app_file, Filename}, {AppName, AppData}),
                    {ok, AppName, AppData};
                {error, Reason} ->
                    {error, Reason};
                Other ->
                    {error, {unexpected_terms, Other}}
            end;
        {AppName, AppData} ->
            {ok, AppName, AppData}
    end.
