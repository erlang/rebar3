%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% SPDX-FileCopyrightText: Copyright 2015-2026 Rebar3 and its contributors
%%
%% SPDX-FileCopyrightText: Copyright 2026 Dipl. Phys. Peer Stritzinger GmbH
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

-module(rebar_resource_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [{group, git}, {group, pkg}, {group, hg}].

groups() ->
    [{all, [], [change_type_upgrade]},
     {git, [], [{group, all}]},
     {pkg, [], [{group, all}]},
     {hg, [], [{group, all}]}].

init_per_group(all, Config) ->
    State = rebar_state:resources(rebar_state:new(), [{git, rebar_git_resource},
                                                      {pkg, rebar_pkg_resource},
                                                      {hg, rebar_hg_resource}]),
    [{state, State} | Config];
init_per_group(Name, Config) ->
    [{type, Name},
     {resource, {Name, "https://example.org/user/app", "vsn"}} | Config].

end_per_group(_, _Config) ->
    ok.

%% Changing the resource type is seen as an upgrade
init_per_testcase(change_type_upgrade, Config) ->
    Type = ?config(type, Config),
    TypeStr = atom_to_list(Type),
    DirName = filename:join([?config(priv_dir, Config), "resource_"++TypeStr]),
    filelib:ensure_path(DirName),

    {ok, AppInfo} = rebar_app_info:new(test_app, "0.0.1", DirName),
    AppInfo1 = rebar_app_info:source(AppInfo, ?config(resource, Config)),

    [{app, AppInfo1} | Config].

end_per_testcase(_, Config) ->
    Config.

change_type_upgrade(Config) ->
    ?assert(rebar_fetch:needs_update(?config(app, Config),
                                     ?config(state, Config))).
