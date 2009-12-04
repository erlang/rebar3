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
         load_app_file/1]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

is_app_dir() ->
    is_app_dir(rebar_util:get_cwd()).

is_app_dir(Dir) ->
    Fname = filename:join([Dir, "ebin/*.app"]),
    case filelib:wildcard(Fname) of
        [AppFile] ->
            {true, AppFile};
        _ ->
            false
    end.

load_app_file(Filename) ->
    case file:consult(Filename) of
        {ok, [{application, AppName, AppData}]} ->
            {ok, AppName, AppData};
        {error, Reason} ->
            ?ERROR("Failed to load app file from ~s: ~p\n", [Filename, Reason]),
            ?FAIL;
        Other ->
            ?ERROR("Unexpected terms from app file ~s: ~p\n", [Filename, Other]),
            ?FAIL
    end.
