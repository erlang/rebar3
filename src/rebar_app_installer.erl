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
-module(rebar_app_installer).

-export([install/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

install(Config, File) ->
    %% Load the app name and version from the .app file and construct
    %% the app identifier
    {ok, AppName, AppData} = rebar_app_utils:load_app_file(File),
    Vsn = proplists:get_value(vsn, AppData),
    AppId = ?FMT("~s-~s", [AppName, Vsn]),
    ?CONSOLE("Installing: ~s\n", [AppId]),

    %% Check the erlang lib directory to see if this app identifier
    %% is already present.
    AppDir = filename:join([code:lib_dir(), AppId]),
    case filelib:is_dir(AppDir) of
        true ->
            %% Already exists -- check for force=1 global flag and only
            %% continue if it's set
            case rebar_config:get_global(force, "0") of
                "0" ->
                    ?ERROR("~s already exists. Installation failed.", []),
                    ?FAIL;
                "1" ->
                    ?WARN("~s already exists, but forcibly overwriting.", [])
            end;
        false ->
            ok
    end.

    %% Wipe out any previous versions
%    ok = rebar_file_utils:rm_rf(Appdir),

    %% Re-create target
%    ok = rebar_file_utils:mkdir_p(AppDir).

    %% By default we copy the ebin, include, src and priv directories
    
