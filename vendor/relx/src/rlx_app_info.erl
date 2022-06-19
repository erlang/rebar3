%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%%% Copyright 2012 Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012 Erlware, LLC.
%%%
%%% @doc This module represents useful, relevant information about an
%%% application. The relevant information is.
%%%
%%% <ul>
%%%  <li> `name' - The application name as an atom. </li>
%%%  <li> `vsn' - The application version as a list. </li>
%%%  <li> `dir' - The root directory of the application. The directory that contains the
%%%  ebin/src/priv </li>
%%%  <li> `link' - Whether to symlink or copy the application directory. </li>
%%%  <li> `applications' - The `application' dependencies of the OTP
%%%  App. That is the things in the `applications' property of the application
%%%  metadata </li>
%%%  <li> `included_applications' - The apps in the `included_applications' property
%%%  of the application metadata.</li>
%%% </ul>
%%%
-module(rlx_app_info).

-export([new/5,
         new/6,
         new/7,
         name/1,
         vsn/1,
         dir/1,
         applications/1,
         included_applications/1,
         optional_applications/1,
         link/1,
         link/2,
         format_error/1,
         format/2,
         format/1]).

-include("relx.hrl").

-type app_type() :: project | dep | checkout | system.
-type t() :: #{name                  := atom() | undefined,
               vsn                   := string() | undefined,

               applications          := [atom()],
               included_applications := [atom()],
               optional_applications := [atom()],

               dir                   := file:name() | undefined,
               link                  := boolean() | undefined,

               %% `project' app is one the user is actively developing on
               %% `dep' is dependency fetched by rebar3
               %% `checkout' is a dependency linked to from the _checkouts dir
               %% and treated like a project app
               %% `system' applications are dependencies from Erlang/OTP
               app_type              := app_type()}.

-export_type([t/0,
              app_type/0]).

-spec new(atom(), string(), file:name(), [atom()], [atom()]) -> t().
new(Name, Vsn, Dir, Applications, IncludedApplications) ->
    new(Name, Vsn, Dir, Applications, IncludedApplications, [], dep).

-spec new(atom(), string(), file:name(), [atom()], [atom()], [atom()] | atom()) -> t().
new(Name, Vsn, Dir, Applications, IncludedApplications, OptionalApplications)
  when is_list(OptionalApplications) ->
    new(Name, Vsn, Dir, Applications, IncludedApplications, OptionalApplications, dep);
new(Name, Vsn, Dir, Applications, IncludedApplications, AppType) when is_atom(AppType) ->
    new(Name, Vsn, Dir, Applications, IncludedApplications, [], AppType).

-spec new(atom(), string(), file:name(), [atom()], [atom()], [atom()], app_type()) -> t().
new(Name, Vsn, Dir, Applications, IncludedApplications, OptionalApplications, AppType) ->
    #{name => Name,
      vsn => Vsn,

      applications => Applications,
      included_applications => IncludedApplications,
      optional_applications => OptionalApplications,

      dir => Dir,
      link => false,

      app_type => AppType}.

-spec name(t()) -> atom().
name(#{name := Name}) ->
    Name.

-spec vsn(t()) -> string().
vsn(#{vsn := Vsn}) ->
    Vsn.

-spec dir(t()) -> binary().
dir(#{dir := Dir}) ->
    Dir.

-spec applications(t()) -> [atom()].
applications(#{applications := Deps}) ->
    Deps.

-spec included_applications(t()) -> [atom()].
included_applications(#{included_applications := Deps}) ->
    Deps.

-spec optional_applications(t()) -> [atom()].
optional_applications(#{included_applications := Deps}) ->
    Deps.

-spec link(t()) -> boolean().
link(#{link := Link}) ->
    Link.

-spec link(t(), boolean()) -> t().
link(AppInfo, NewLink) ->
    AppInfo#{link => NewLink}.

-spec format_error(Reason::term()) -> iolist().
format_error(Error) ->
    io_lib:format("~p", [Error]).

-spec format(t()) -> iolist().
format(AppInfo) ->
    format(0, AppInfo).

-spec format(non_neg_integer(), t()) -> iolist().
format(Indent, #{name := Name,
                 vsn := Vsn,
                 dir := Dir,
                 applications := Deps,
                 included_applications := IncDeps,
                 link := Link}) ->
    [rlx_util:indent(Indent), erlang:atom_to_list(Name), "-", Vsn, ": ", Dir, "\n",
     rlx_util:indent(Indent + 1), "Symlink: ", erlang:atom_to_list(Link), "\n",
     rlx_util:indent(Indent + 1), "Applications:\n",
     [[rlx_util:indent(Indent + 2), erlang:atom_to_list(Dep), "\n"] || Dep <- Deps],
     rlx_util:indent(Indent + 1), "Included Applications:\n",
     [[rlx_util:indent(Indent + 2), erlang:atom_to_list(IncDep), "\n"] || IncDep <- IncDeps]].
