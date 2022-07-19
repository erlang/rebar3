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
%%% @doc This module represents a release and its metadata and is used to
%%% manipulate the release metadata.
-module(rlx_release).

-export([new/2,
         relfile/2,
         erts/2,
         erts/1,
         parse_goals/1,
         goals/2,
         goals/1,
         parsed_goals/2,
         name/1,
         vsn/1,
         realize/2,
         applications/1,
         app_specs/1,
         metadata/1,
         start_clean_metadata/1,
         no_dot_erlang_metadata/1,
         canonical_name/1,
         config/1,
         config/2,
         format/1,
         format_error/1]).

-export_type([t/0,
              name/0,
              vsn/0,
              type/0,
              incl_apps/0,
              application_spec/0,
              release_spec/0,
              parsed_goal/0]).

-include("relx.hrl").

-record(release_t, {name :: atom(),
                    vsn :: string(),
                    erts :: undefined | string(),
                    goals = undefined :: parsed_goals() | undefined,

                    %% when `realized' is `true' `applications' must be a list of all
                    %% `app_info's needed to fulfill the `goals' and `app_specs'
                    %% must be the full list that goes in the `.rel' file.
                    realized = false :: boolean(),

                    app_specs = [] ::  [application_spec()],
                    applications = [] :: [rlx_app_info:t()],

                    relfile :: undefined | string(),
                    config = []}).

%%============================================================================
%% types
%%============================================================================
-type name() :: atom().
-type vsn() :: string().
-type type() :: permanent | transient | temporary | load | none.
-type incl_apps() :: [name()].

-type parsed_goal() :: #{name := name(),
                         vsn => vsn() | undefined,
                         type => type(),
                         included_applications => incl_apps()}.

-type parsed_goals() :: [{name(), parsed_goal()}].

-type application_spec() :: {name(), vsn()} |
                            {name(), vsn(), type() | incl_apps()} |
                            {name(), vsn(), type(), incl_apps()}.

-type release_spec() :: {release, {string(), vsn()}, {erts, vsn()},
                         [application_spec()]}.

-type t() :: #release_t{}.

-spec new(atom(), string(), undefined | file:name()) -> t().
new(ReleaseName, ReleaseVsn, Relfile) ->
    #release_t{name=ReleaseName,
               vsn=ReleaseVsn,
               relfile = Relfile}.

-spec new(atom(), string()) -> t().
new(ReleaseName, ReleaseVsn) ->
    new(ReleaseName, ReleaseVsn, undefined).

-spec relfile(t(), file:name()) -> t().
relfile(Release, Relfile) ->
    Release#release_t{relfile=Relfile}.

-spec name(t()) -> atom().
name(#release_t{name=Name}) ->
    Name.

-spec vsn(t()) -> string().
vsn(#release_t{vsn=Vsn}) ->
    Vsn.

-spec erts(t(), vsn()) -> t().
erts(Release, Vsn) ->
    Release#release_t{erts=Vsn}.

-spec erts(t()) -> vsn().
erts(#release_t{erts=Vsn}) ->
    Vsn.

-spec goals(t(), [relx:goal()]) -> t().
goals(Release, ConfigGoals) ->
    Release#release_t{goals=parse_goals(ConfigGoals)}.

-spec goals(t()) -> parsed_goals().
goals(#release_t{goals=Goals}) ->
    Goals.

-spec parsed_goals(t(), parsed_goals()) -> t().
parsed_goals(Release, ParsedGoals) ->
    Release#release_t{goals=ParsedGoals}.

-spec realize(t(), [rlx_app_info:t()]) ->
                     {ok, t()}.
realize(Rel, Pkgs0) ->
    process_specs(realize_erts(Rel), Pkgs0).

applications(#release_t{applications=Apps}) ->
    Apps.

app_specs(#release_t{app_specs=AppSpecs}) ->
    AppSpecs.

-spec metadata(t()) -> release_spec().
metadata(#release_t{name=Name,
                    vsn=Vsn,
                    erts=ErtsVsn,
                    app_specs=Apps,
                    realized=Realized}) ->
    case Realized of
        true ->
            {release, {rlx_util:to_string(Name), Vsn}, {erts, ErtsVsn}, Apps};
        false ->
            erlang:error(?RLX_ERROR({not_realized, Name, Vsn}))
    end.

%% Include all apps in the release as `none' type so they are not
%% loaded or started but are available in the path when a the
%% `start_clean' is used, like is done with command `console_clean'
-spec start_clean_metadata(t()) -> release_spec().
start_clean_metadata(#release_t{erts=ErtsVsn,
                                app_specs=Apps}) ->
    {value, Kernel, Apps1} = lists:keytake(kernel, 1, Apps),
    {value, StdLib, Apps2} = lists:keytake(stdlib, 1, Apps1),
    {release, {"start_clean", "1.0"}, {erts, ErtsVsn}, [Kernel, StdLib | none_type_apps(Apps2)]}.

none_type_apps([]) ->
    [];
none_type_apps([{Name, Version} | Rest]) ->
    [{Name, Version, none} | none_type_apps(Rest)];
none_type_apps([{Name, Version, _} | Rest]) ->
    [{Name, Version, none} | none_type_apps(Rest)];
none_type_apps([{Name, Version, _, _} | Rest]) ->
    [{Name, Version, none} | none_type_apps(Rest)].

%% no_dot_erlang.boot goes in the root bin dir of the release_handler
%% so should not have anything specific to the release version in it
%% Here it only has kernel and stdlib.
-spec no_dot_erlang_metadata(t()) -> release_spec().
no_dot_erlang_metadata(#release_t{erts=ErtsVsn,
                                  app_specs=Apps}) ->
    {value, Kernel, Apps1} = lists:keytake(kernel, 1, Apps),
    {value, StdLib, _Apps2} = lists:keytake(stdlib, 1, Apps1),
    {release, {"no_dot_erlang", "1.0"}, {erts, ErtsVsn}, [Kernel, StdLib]}.

%% @doc produce the canonical name `<name>-<vsn>' for this release
-spec canonical_name(t()) -> string().
canonical_name(#release_t{name=Name, vsn=Vsn}) ->
    erlang:binary_to_list(erlang:iolist_to_binary([erlang:atom_to_list(Name), "-", Vsn])).


-spec config(t(), list()) -> t().
config(Release, Config) ->
    Release#release_t{config=Config}.

-spec config(t()) -> list().
config(#release_t{config=Config}) ->
    Config.

-spec format(t()) -> iolist().
format(Release) ->
    format(0, Release).

-spec format(non_neg_integer(), t()) -> iolist().
format(Indent, #release_t{name=Name,
                          vsn=Vsn,
                          erts=ErtsVsn,
                          realized=Realized,
                          goals = Goals,
                          app_specs=Apps}) ->
    BaseIndent = rlx_util:indent(Indent),
    [BaseIndent, "release: ", rlx_util:to_string(Name), "-", Vsn, "\n",
     rlx_util:indent(Indent + 1), "erts: ", ErtsVsn, "\n",
     rlx_util:indent(Indent + 1), "goals: \n",
     [[rlx_util:indent(Indent + 2),  format_goal(Goal), "\n"] || {_, Goal} <- Goals],
     case Realized of
         true ->
             [rlx_util:indent(Indent + 1), "applications: \n",
              [[rlx_util:indent(Indent + 2),  io_lib:format("~p", [App]), "\n"] ||
                  App <- Apps]];
         false ->
             []
     end].

-spec format_goal(parsed_goal()) -> iolist().
format_goal(#{name := Name,
              vsn := Vsn}) when Vsn =/= undefined ->
    io_lib:format("{~p, ~s}", [Name, Vsn]);
format_goal(#{name := Name}) ->
    io_lib:format("~p", [Name]).

-spec format_error(Reason::term()) -> iolist().
format_error({failed_to_parse, Con}) ->
    io_lib:format("Failed to parse constraint ~p", [Con]);
format_error({invalid_constraint, _, Con}) ->
    io_lib:format("Invalid constraint specified ~p", [Con]);
format_error({not_realized, Name, Vsn}) ->
    io_lib:format("Unable to produce metadata release: ~p-~s has not been realized",
                  [Name, Vsn]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec realize_erts(t()) -> t().
realize_erts(Rel=#release_t{erts=undefined}) ->
    Rel#release_t{erts=erlang:system_info(version)};
realize_erts(Rel) ->
    Rel.

-spec process_specs(t(), [rlx_app_info:t()]) -> {ok, t()}.
process_specs(Rel=#release_t{goals=Goals}, World) ->
    IncludedApps = lists:foldl(fun(#{included_applications := I}, Acc) ->
                                       sets:union(sets:from_list(I), Acc)
                               end, sets:new(), World),
    Specs = [create_app_spec(App, Goals, IncludedApps) || App <- World],
    {ok, Rel#release_t{goals=Goals,
                       app_specs=Specs,
                       applications=World,
                       realized=true}}.

-spec create_app_spec(rlx_app_info:t(), parsed_goals(), sets:set(atom())) -> application_spec().
create_app_spec(App, Goals, WorldIncludedApps) ->
    %% If the app only exists as a dependency in an included app then it should
    %% get the 'load' annotation unless the release spec has set something
    AppName = rlx_app_info:name(App),
    Vsn = rlx_app_info:vsn(App),

    TypeAnnot = case sets:is_element(AppName, WorldIncludedApps) of
                true ->
                    load;
                false ->
                    permanent
            end,

    #{type := Type,
      included_applications := IncludedApplications} =
        list_find(AppName, Goals, #{type => TypeAnnot,
                                    included_applications => undefined}),

    case {Type, IncludedApplications} of
        {undefined, undefined} ->
            {AppName, Vsn};
        {Type, undefined} when Type =:= permanent ;
                               Type =:= transient ;
                               Type =:= temporary ;
                               Type =:= load ;
                               Type =:= none ->
            maybe_with_type({AppName, Vsn}, Type);
        {undefined, IncludedApplications} ->
            {AppName, Vsn, IncludedApplications};
        {Type, IncludedApplications} ->
            maybe_with_type({AppName, Vsn, Type, IncludedApplications}, Type);
        _ ->
            error(?RLX_ERROR({bad_app_goal, {AppName, Vsn, Type, IncludedApplications}}))
    end.

list_find(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} ->
            Value;
        false ->
            Default
    end.

%% keep a clean .rel file by only included non-defaults
maybe_with_type(Tuple, permanent) ->
    Tuple;
maybe_with_type(Tuple, Type) ->
    erlang:insert_element(3, Tuple, Type).

-spec parse_goals([application_spec()]) -> parsed_goals().
parse_goals(ConfigGoals) ->
    lists:map(fun(ConfigGoal) ->
                      Goal = #{name := Name} = parse_goal(ConfigGoal),
                      {Name, maps:merge(#{vsn=> undefined,
                                          type => undefined,
                                          included_applications => undefined}, Goal)}
              end, ConfigGoals).

-spec parse_goal(relx:goal()) -> parsed_goal().
parse_goal(AppName) when is_atom(AppName) ->
    #{name => AppName};
parse_goal({AppName, Type}) when Type =:= permanent ;
                                 Type =:= transient ;
                                 Type =:= temporary ;
                                 Type =:= load ;
                                 Type =:= none ->
    #{name => AppName,
      type => Type};
parse_goal({AppName, IncludedApplications=[H|_]}) when is_atom(H) ->
    #{name => AppName,
      included_applications => IncludedApplications};
parse_goal({AppName, []}) when is_atom(AppName) ->
    #{name => AppName,
      included_applications => []};
parse_goal({AppName, Vsn}) when is_list(Vsn) ->
    #{name => AppName,
      vsn => Vsn};
parse_goal({AppName, Vsn, Type})
  when is_list(Vsn) andalso (Type =:= permanent orelse
                             Type =:= transient orelse
                             Type =:= temporary orelse
                             Type =:= load orelse
                             Type =:= none) ->
    #{name => AppName,
      vsn => Vsn,
      type => Type};
parse_goal({AppName, Vsn, IncludedApplications}) when is_list(Vsn) ,
                                                      is_list(IncludedApplications) ->
    #{name => AppName,
      vsn => Vsn,
      included_applications => IncludedApplications};
parse_goal({AppName, Vsn, Type, IncludedApplications})
  when is_list(Vsn) andalso is_list(IncludedApplications) andalso (Type =:= permanent orelse
                                                                   Type =:= transient orelse
                                                                   Type =:= temporary orelse
                                                                   Type =:= load orelse
                                                                   Type =:= none) ->
    #{name => AppName,
      vsn => Vsn,
      type => Type,
      included_applications => IncludedApplications};
parse_goal(Goal) ->
    error(?RLX_ERROR({bad_goal, Goal})).
