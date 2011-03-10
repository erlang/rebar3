%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2010 Dave Smith (dizzyd@dizzyd.com)
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
%% @author Dave Smith <dizzyd@dizzyd.com>
%% @doc rebar_edoc supports the following command:
%% <ul>
%%   <li>doc (essentially erl -noshell -run edoc_run application
%% "'$(&lt;app_name&gt;)'"
%% '"."' '[&lt;options&gt;]')</li>
%% </ul>
%% EDoc options can be given in the <code>edoc_opts</code> option in
%% <code>rebar.config</code>.
%% @copyright 2010 Dave Smith
%% -------------------------------------------------------------------
-module(rebar_edoc).

-export([doc/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Generate Erlang program documentation.
-spec doc(Config::rebar_config:config(), File::file:filename()) -> ok.
doc(Config, File) ->
    {ok, AppName, _AppData} = rebar_app_utils:load_app_file(File),
    EDocOpts = rebar_config:get(Config, edoc_opts, []),
    ok = edoc:application(AppName, ".", EDocOpts),
    ok.
