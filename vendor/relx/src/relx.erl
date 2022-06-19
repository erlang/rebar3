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
%%% @author Tristan Sloughter <t@crashfast.com>
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012 Erlware, LLC.
%%% @doc
-module(relx).

-export([release/3,
         build_release/2,
         build_release/3,

         build_tar/2,
         build_tar/3,

         build_relup/4,
         format_error/1]).

-include("relx.hrl").

-type error() :: {error, {Module::module(), Reason::term()}}.
-type goal() :: rlx_release:name() |
                {rlx_release:name(), rlx_release:vsn() | rlx_release:type()} |
                {rlx_release:name(), rlx_release:vsn(), rlx_release:type() | rlx_release:incl_apps()} |
                {rlx_release:name(), rlx_release:vsn(), rlx_release:type(), rlx_release:incl_apps()}.

-export_type([goal/0,
              error/0]).

-type release() :: #{name := atom(),
                     vsn := string(),

                     %% top level application list to include in release
                     %% referred to as goals because it is not the complete
                     %% list of applications.
                     goals := [goal()],

                     relfile_path := file:filename_all() | undefined}.

-spec release(rlx_release:name(), rlx_release:vsn(), [goal()]) -> release().
release(Name, Vsn, Goals) ->
    #{name => Name,
      vsn => Vsn,
      goals => Goals,
      relfile_path => undefined}.

-spec build_release(Release, Config) -> {ok, rlx_state:t()} | {error, term()} when
      Release :: atom() | {atom(), string()} | release(),
      Config :: rlx_config:t().
build_release(Release, Config) ->
    {ok, State} = rlx_config:to_state(Config),
    build_release(Release, #{}, State).

-spec build_release(Release, Apps, State) -> {ok, rlx_state:t()} | {error, term()} when
      Release :: atom() | {atom(), string()} | release() | undefined,
      Apps :: #{atom() => rlx_app_info:t()},
      State :: rlx_state:t().
build_release(RelNameOrUndefined, Apps, State) when is_atom(RelNameOrUndefined) ->
    {RelName, RelVsn} = pick_release_version(RelNameOrUndefined, State),
    Release = #{name => RelName,
                vsn  => RelVsn},
    RealizedRelease = build_release_(Release, Apps, State),
    {ok, rlx_state:add_realized_release(State, RealizedRelease)};
build_release({RelName, RelVsn}, Apps, State) when is_atom(RelName) ,
                                                   is_list(RelVsn) ->
    Release = #{name => RelName,
                vsn => RelVsn},
    RealizedRelease = build_release_(Release, Apps, State),
    {ok, rlx_state:add_realized_release(State, RealizedRelease)};
build_release(Release=#{name := _RelName,
                        vsn  := _RelVsn}, Apps, State) ->
    RealizedRelease = build_release_(Release, Apps, State),
    {ok, rlx_state:add_realized_release(State, RealizedRelease)};
build_release(Release, _, _) ->
    ?RLX_ERROR({unrecognized_release, Release}).

-spec build_tar(Release, Config) -> {ok, rlx_release:t()} when
      Release :: atom() | {atom(), string()} | release() | undefined,
      Config :: rlx_config:t().
build_tar(Release, Config) when is_list(Config) ->
    {ok, State} = rlx_config:to_state(Config),
    build_tar(Release, #{}, State).

-spec build_tar(Release, Apps, State) -> {ok, rlx_release:t()} when
      Release :: atom() | {atom(), string()} | release() | undefined,
      Apps :: #{atom() => rlx_app_info:t()},
      State :: rlx_state:t().
build_tar(undefined, Apps, State) ->
    {RelName, RelVsn} = pick_release(State),
    Release = #{name => RelName,
                vsn => RelVsn},
    RealizedRelease = build_release_(Release, Apps, State),
    build_tar_(RealizedRelease, State),
    {ok, RealizedRelease};
build_tar(Release=#{name := RelName,
                    vsn := RelVsn}, Apps, State) when is_atom(RelName) ,
                                                      is_list(RelVsn) ->
    RealizedRelease = build_release_(Release, Apps, State),
    build_tar_(RealizedRelease, State),
    {ok, RealizedRelease};
build_tar({RelName, RelVsn}, Apps, State) when is_atom(RelName) ->
    Release = #{name => RelName,
                vsn => RelVsn},
    RealizedRelease = build_release_(Release, Apps, State),
    build_tar_(RealizedRelease, State),
    {ok, RealizedRelease};
build_tar(RelName, Apps, State) when is_atom(RelName) ->
    {RelName, RelVsn} = pick_release_version(RelName, State),
    Release = #{name => RelName,
                vsn => RelVsn},
    RealizedRelease = build_release_(Release, Apps, State),
    build_tar_(RealizedRelease, State),
    {ok, RealizedRelease}.

-spec build_relup(rlx_release:name(), rlx_release:vsn(), rlx_release:vsn(), rlx_config:t() | rlx_state:t())
                 -> {ok, rlx_state:t()} | {error, term()}.
build_relup(RelName, ToVsn, UpFromVsn, Config) when is_list(Config) ->
    {ok, State} = rlx_config:to_state(Config),
    build_relup(RelName, ToVsn, UpFromVsn, State);
build_relup(RelName, ToVsn, UpFromVsn, State) ->
    {RelName, ToVsn} = pick_release_version(RelName, State),
    rlx_relup:do(RelName, ToVsn, UpFromVsn, State).

-spec format_error(Reason::term()) -> string().
format_error({unrecognized_release, Release}) ->
    io_lib:format("Could not understand release argument ~p~n", [Release]);
format_error({error, {relx, Reason}}) ->
    format_error(Reason);
format_error({no_release_name, Vsn}) ->
    io_lib:format("A target release version was specified (~s) but no name", [Vsn]);
format_error({invalid_release_info, Info}) ->
    io_lib:format("Target release information is in an invalid format ~p", [Info]);
format_error({multiple_release_names, _, _}) ->
    "Must specify the name of the release to build when there are multiple releases in the config";
format_error(no_releases_in_system) ->
    "No releases have been specified in the system!";
format_error({no_releases_for, RelName}) ->
    io_lib:format("No releases exist in the system for ~s!", [RelName]);
format_error({release_not_found, {RelName, RelVsn}}) ->
    io_lib:format("No releases exist in the system for ~p:~s!", [RelName, RelVsn]);
format_error({error, {Module, Reason}}) ->
    io_lib:format("~s~n", [Module:format_error(Reason)]).

%%

build_tar_(RealizedRelease, State) ->
    OutputDir = filename:join(rlx_state:base_output_dir(State),
                              rlx_release:name(RealizedRelease)),
    rlx_tar:make_tar(RealizedRelease, OutputDir, State).

build_release_(#{name := RelName,
                 vsn := RelVsn}, Apps, State) ->
    Release = rlx_state:get_configured_release(State, RelName, RelVsn),
    {ok, RealizedRelease, State1} =
        rlx_resolve:solve_release(Release, rlx_state:available_apps(State, Apps)),
    {ok, State2} = rlx_assemble:do(RealizedRelease, State1),
    _ = rlx_overlay:render(RealizedRelease, State2),
    RealizedRelease.

pick_release(State) ->
    %% Here we will just get the highest versioned release and run that.
    case lists:sort(fun release_sort/2, maps:to_list(rlx_state:configured_releases(State))) of
        [{{RelName, RelVsn}, _} | _] ->
            {RelName, RelVsn};
        [] ->
            erlang:error(?RLX_ERROR(no_releases_in_system))
    end.

pick_release_version(undefined, State) ->
    pick_release(State);
pick_release_version(RelName, State) ->
    %% Here we will just get the latest version for name RelName and run that.
    AllReleases = maps:to_list(rlx_state:configured_releases(State)),
    SpecificReleases = [Rel || Rel={{PossibleRelName, _}, _} <- AllReleases, PossibleRelName =:= RelName],
    case lists:sort(fun release_sort/2, SpecificReleases) of
        [{{RelName, RelVsn}, _} | _] ->
            {RelName, RelVsn};
        [] ->
            erlang:error(?RLX_ERROR({no_releases_for, RelName}))
    end.

-spec release_sort({{rlx_release:name(),rlx_release:vsn()}, term()},
                   {{rlx_release:name(),rlx_release:vsn()}, term()}) ->
                          boolean().
release_sort({{RelName, RelVsnA}, _},
             {{RelName, RelVsnB}, _}) ->
    rlx_util:parsed_vsn_lte(rlx_util:parse_vsn(RelVsnB), rlx_util:parse_vsn(RelVsnA));
release_sort({{RelA, _}, _}, {{RelB, _}, _}) ->
    %% The release names are different. When the releases are named differently
    %% we can not just take the latest version. You *must* provide a default
    %% release name at least. So we throw an error here that the top can catch
    %% and return
    error(?RLX_ERROR({multiple_release_names, RelA, RelB})).
