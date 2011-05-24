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
-module(rebar_rel_utils).

-export([is_rel_dir/0,
         is_rel_dir/1,
         get_reltool_release_info/1,
         get_rel_release_info/1,
         get_rel_release_info/2,
         get_rel_apps/1,
         get_rel_apps/2,
         get_previous_release_path/0,
         get_rel_file_path/2]).

-include("rebar.hrl").

is_rel_dir() ->
    is_rel_dir(rebar_utils:get_cwd()).

is_rel_dir(Dir) ->
    Fname = filename:join([Dir, "reltool.config"]),
    case filelib:is_regular(Fname) of
        true ->
            {true, Fname};
        false ->
            false
    end.

%% Get release name and version from a reltool.config
get_reltool_release_info(ReltoolFile) ->
    %% expect sys to be the first proplist in reltool.config
    case file:consult(ReltoolFile) of
        {ok, [{sys, Config}| _]} ->
            %% expect the first rel in the proplist to be the one you want
            {rel, Name, Ver, _} = proplists:lookup(rel, Config),
            {Name, Ver};
        _ ->
            ?ABORT("Failed to parse ~s~n", [ReltoolFile])
    end.

%% Get release name and version from a rel file
get_rel_release_info(RelFile) ->
    case file:consult(RelFile) of
        {ok, [{release, {Name, Ver}, _, _}]} ->
            {Name, Ver};
        _ ->
            ?ABORT("Failed to parse ~s~n", [RelFile])
    end.

%% Get release name and version from a name and a path
get_rel_release_info(Name, Path) ->
    RelPath = get_rel_file_path(Name, Path),
    get_rel_release_info(RelPath).

%% Get list of apps included in a release from a rel file
get_rel_apps(RelFile) ->
    case file:consult(RelFile) of
        {ok, [{release, _, _, Apps}]} ->
            make_proplist(Apps, []);
        _ ->
            ?ABORT("Failed to parse ~s~n", [RelFile])
    end.

%% Get list of apps included in a release from a name and a path
get_rel_apps(Name, Path) ->
    RelPath = get_rel_file_path(Name, Path),
    get_rel_apps(RelPath).

%% Get rel file path from name and path
get_rel_file_path(Name, Path) ->
    [RelFile] = filelib:wildcard(filename:join([Path, "releases", "*",
                                                Name ++ ".rel"])),
    [BinDir|_] = re:replace(RelFile, Name ++ "\\.rel", ""),
    filename:join([binary_to_list(BinDir), Name ++ ".rel"]).

%% Get the previous release path from a global variable
get_previous_release_path() ->
    case rebar_config:get_global(previous_release, false) of
        false ->
            ?ABORT("previous_release=PATH is required to "
                   "create upgrade package~n", []);
        OldVerPath ->
            OldVerPath
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

make_proplist([{_,_}=H|T], Acc) ->
     make_proplist(T, [H|Acc]);
make_proplist([H|T], Acc) ->
     App = erlang:element(1, H),
     Ver = erlang:element(2, H),
     make_proplist(T, [{App,Ver}|Acc]);
make_proplist([], Acc) ->
     Acc.
