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
-module(rebar_escripter).

-export([escriptize/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

escriptize(_Config, AppFile) ->
    %% Extract the application name from the archive -- this will be be what
    %% we call the output script
    AppName = rebar_app_utils:app_name(AppFile),

    %% Construct the archive of everything in ebin/ dir -- put it on the
    %% top-level of the zip file so that code loading works properly.
    Files = filelib:wildcard("*", "ebin"),
    case zip:create("mem", Files, [{cwd, "ebin"}, memory]) of
        {ok, {"mem", ZipBin}} ->
            %% Archive was successfully created. Prefix that binary with our
            %% header and write to our escript file
            Script = <<"#!/usr/bin/env escript\n", ZipBin/binary>>,
            case file:write_file(AppName, Script) of
                ok ->
                    ok;
                {error, WriteError} ->
                    ?ERROR("Failed to write ~p script: ~p\n", [AppName, WriteError]),
                    ?FAIL
            end;
        {error, ZipError} ->
            ?ERROR("Failed to construct ~p escript: ~p\n", [AppName, ZipError]),
            ?FAIL
    end,

    %% Finally, update executable perms for our script
    [] = os:cmd(?FMT("chmod u+x ~p", [AppName])),
    ok.

