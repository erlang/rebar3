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
         get_rel_file_path/2,
         load_config/1,
         get_sys_tuple/1,
         get_target_dir/1,
         get_target_parent_dir/1]).

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
get_reltool_release_info([{sys, Config}| _]) ->
    {rel, Name, Ver, _} = proplists:lookup(rel, Config),
    {Name, Ver};
get_reltool_release_info(ReltoolFile) when is_list(ReltoolFile) ->
    case file:consult(ReltoolFile) of
        {ok, ReltoolConfig} ->
            get_reltool_release_info(ReltoolConfig);
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

%%
%% Load terms from reltool.config
%%
load_config(ReltoolFile) ->
    case file:consult(ReltoolFile) of
        {ok, Terms} ->
            Terms;
        Other ->
            ?ABORT("Failed to load expected config from ~s: ~p\n",
                   [ReltoolFile, Other])
    end.

%%
%% Look for the {sys, [...]} tuple in the reltool.config file.
%% Without this present, we can't run reltool.
%%
get_sys_tuple(ReltoolConfig) ->
    case lists:keyfind(sys, 1, ReltoolConfig) of
        {sys, _} = SysTuple ->
            SysTuple;
        false ->
            ?ABORT("Failed to find {sys, [...]} tuple in reltool.config.", [])
    end.

%%
%% Look for {target_dir, TargetDir} in the reltool config file; if none is
%% found, use the name of the release as the default target directory.
%%
get_target_dir(ReltoolConfig) ->
    case rebar_config:get_global(target_dir, undefined) of
        undefined ->
            case lists:keyfind(target_dir, 1, ReltoolConfig) of
                {target_dir, TargetDir} ->
                    filename:absname(TargetDir);
                false ->
                    {sys, SysInfo} = get_sys_tuple(ReltoolConfig),
                    case lists:keyfind(rel, 1, SysInfo) of
                        {rel, Name, _Vsn, _Apps} ->
                            filename:absname(Name);
                        false ->
                            filename:absname("target")
                    end
            end;
        TargetDir ->
            filename:absname(TargetDir)
    end.

get_target_parent_dir(ReltoolConfig) ->
    case lists:reverse(tl(lists:reverse(filename:split(get_target_dir(ReltoolConfig))))) of
        [] -> ".";
        Components -> filename:join(Components)
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

make_proplist([{_,_}=H|T], Acc) ->
     make_proplist(T, [H|Acc]);
make_proplist([H|T], Acc) ->
     App = element(1, H),
     Ver = element(2, H),
     make_proplist(T, [{App,Ver}|Acc]);
make_proplist([], Acc) ->
     Acc.