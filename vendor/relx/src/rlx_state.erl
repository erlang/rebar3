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
%%% @doc Provides state management services for the relx tool. Generally,
%%% those things that are fixed have a direct api. Those things that are mutable
%%% have a more mutable api.
-module(rlx_state).

-export([new/0,
         base_output_dir/1,
         base_output_dir/2,
         lib_dirs/1,
         add_lib_dirs/2,
         overrides/1,
         overrides/2,
         exclude_apps/1,
         exclude_apps/2,
         debug_info/1,
         debug_info/2,
         vm_args/1,
         vm_args/2,
         vm_args_src/1,
         vm_args_src/2,
         sys_config/1,
         sys_config/2,
         sys_config_src/1,
         sys_config_src/2,
         root_dir/1,
         root_dir/2,
         add_configured_release/2,
         get_configured_release/3,
         configured_releases/1,
         add_realized_release/2,
         realized_releases/1,
         update_realized_release/2,
         available_apps/1,
         available_apps/2,
         dev_mode/1,
         dev_mode/2,
         mode/1,
         mode/2,
         include_src/1,
         include_src/2,
         include_erts/1,
         include_erts/2,
         include_nodetool/1,
         include_nodetool/2,
         system_libs/1,
         system_libs/2,
         extended_start_script/1,
         extended_start_script/2,
         extended_start_script_hooks/1,
         extended_start_script_hooks/2,
         extended_start_script_extensions/1,
         extended_start_script_extensions/2,
         include_start_scripts_for/1,
         include_start_scripts_for/2,
         overlay/1,
         overlay/2,
         overlay_vars_values/1,
         overlay_vars_values/2,
         overlay_vars/1,
         overlay_vars/2,
         generate_start_script/1,
         generate_start_script/2,
         upfrom/2,
         format/1,
         format/2,
         exclude_modules/1,
         exclude_modules/2,
         warnings_as_errors/1,
         warnings_as_errors/2,
         src_tests/1,
         src_tests/2,
         exref/1,
         exref/2,
         check_for_undefined_functions/1,
         check_for_undefined_functions/2,
         is_relx_sasl/1,
         filter_xref_warning/1,
         filter_xref_warning/2]).

-type mode() :: dev | prod | minimal.

-export_type([t/0,
              releases/0,
              mode/0]).

-record(state_t, {root_dir :: file:name(),
                  output_dir :: file:name(),
                  lib_dirs=[] :: [file:name()],
                  config_file=[] :: file:filename() | undefined,
                  available_apps=#{} :: #{atom() => rlx_app_info:t()},
                  vm_args :: file:filename() | false | undefined,
                  vm_args_src :: file:filename() | false | undefined,
                  sys_config :: file:filename() | false | undefined,
                  sys_config_src :: file:filename() | false | undefined,
                  overrides=[] :: [{AppName::atom(), Directory::file:filename()}],
                  exclude_apps=[] :: [AppName::atom()],
                  exclude_modules=[] :: [{App::atom(), [Module::atom()]}],
                  debug_info=keep :: keep | strip,
                  configured_releases :: releases(),
                  realized_releases :: releases(),
                  include_src :: boolean() | undefined,
                  include_erts=false :: boolean() | file:filename(),
                  system_libs=true :: boolean() | file:filename(),
                  upfrom :: string() | binary() | undefined,
                  warnings_as_errors=false :: boolean(),
                  src_tests=true :: boolean(),
                  check_for_undefined_functions=true :: boolean(),
                  exref=false :: boolean() | [atom()],
                  overlay=[] :: list(),
                  include_nodetool=true :: boolean(),
                  overlay_vars_values=[] :: list(),
                  overlay_vars=[] :: list(),
                  extended_start_script=true :: boolean(),
                  extended_start_script_hooks=[] :: list(),
                  extended_start_script_extensions=[] :: list(),
                  generate_start_script=true :: boolean(),
                  include_start_scripts_for=undefined :: [atom()] | undefined,
                  filter_xref_warning = fun(Warnings) -> Warnings end :: fun((list({mfa(), mfa()})) -> list({mfa(), mfa()})),

                  %% `dev_mode' is for backwards compatibility
                  dev_mode=false :: boolean(),
                  %% mode toggles multiple configuration values at once
                  mode=prod :: mode() | undefined,

                  %% default check is for sasl 3.5 and above
                  %% version 3.5 of sasl has systools with changes for relx
                  %% related to `make_script', `make_tar' and the extended start script
                  is_relx_sasl=false :: boolean()}).

%%============================================================================
%% types
%%============================================================================

-type releases() :: #{{rlx_release:name(), rlx_release:vsn()} => rlx_release:t()}.
-type t() :: #state_t{}.

%%============================================================================
%% API
%%============================================================================

new() ->
    {ok, Root} = file:get_cwd(),
    #state_t{root_dir=Root,
             output_dir=filename:join(Root, "_rel"),
             configured_releases=#{},
             realized_releases=#{},
             is_relx_sasl=rlx_util:is_sasl_gte(),
             overlay_vars_values=[],
             overlay_vars=[]}.

%% @doc the application overrides for the system
-spec overrides(t()) -> [{AppName::atom(), Directory::file:filename()}].
overrides(#state_t{overrides=Overrides}) ->
    Overrides.

%% @doc the application overrides for the system
-spec overrides(t(), [{AppName::atom(), Directory::file:filename()}]) -> t().
overrides(State, Overrides) ->
    State#state_t{overrides=Overrides}.

-spec exclude_apps(t()) -> [AppName::atom()].
exclude_apps(#state_t{exclude_apps=Apps}) ->
    Apps.

%% @doc applications to exclude from the release and remove from .app files
-spec exclude_apps(t(), [AppName::atom()]) -> t().
exclude_apps(State, ExcludeApps) ->
    State#state_t{exclude_apps=ExcludeApps}.

-spec exclude_modules(t()) -> [{App::atom(), [Module::atom()]}].
exclude_modules(#state_t{exclude_modules=Modules}) ->
    Modules.

%% @doc modules to be excluded from the release 
-spec exclude_modules(t(), [{App::atom(), [Module::atom()]}]) -> t().
exclude_modules(State, SkipModules) ->
    State#state_t{exclude_modules=SkipModules}.

-spec debug_info(t()) -> keep | strip.
debug_info(#state_t{debug_info=DebugInfo}) ->
    DebugInfo.

-spec debug_info(t(), keep | strip) -> t().
debug_info(State, DebugInfo) ->
    State#state_t{debug_info=DebugInfo}.

-spec base_output_dir(t()) -> file:name().
base_output_dir(#state_t{output_dir=OutDir}) ->
    OutDir.

-spec base_output_dir(t(), Directory::file:filename()) -> t().
base_output_dir(State, Directory) ->
    State#state_t{output_dir=Directory}.

-spec lib_dirs(t()) -> [file:name()].
lib_dirs(#state_t{lib_dirs=LibDir}) ->
    LibDir.

-spec add_lib_dirs(t(), [file:name()]) -> t().
add_lib_dirs(State=#state_t{lib_dirs=LibDirs}, Dirs) ->
    %% remove from existing list `LibDirs' any duplicate directory in `Dirs'
    %% and prepend the new list of directories
    State#state_t{lib_dirs=Dirs ++ lists:filter(fun(Dir) ->
                                                        lists:member(Dir, LibDirs)
                                                end, LibDirs)}.

-spec vm_args(t()) -> file:filename() | false | undefined.
vm_args(#state_t{vm_args=VmArgs}) ->
    VmArgs.

-spec vm_args(t(), undefined | false | file:filename()) -> t().
vm_args(State, VmArgs) ->
    State#state_t{vm_args=VmArgs}.

-spec vm_args_src(t()) -> file:filename() | false | undefined.
vm_args_src(#state_t{vm_args_src=VmArgs}) ->
    VmArgs.

-spec vm_args_src(t(), undefined | false | file:filename()) -> t().
vm_args_src(State, VmArgs) ->
    State#state_t{vm_args_src=VmArgs}.

-spec sys_config(t()) -> file:filename() | false | undefined.
sys_config(#state_t{sys_config=SysConfig}) ->
    SysConfig.

-spec sys_config(t(), false | undefined | file:filename()) -> t().
sys_config(State, SysConfig) ->
    State#state_t{sys_config=SysConfig}.

-spec sys_config_src(t()) -> file:filename() | false | undefined.
sys_config_src(#state_t{sys_config_src=SysConfigSrc}) ->
    SysConfigSrc.

-spec sys_config_src(t(), file:filename() | false | undefined) -> t().
sys_config_src(State, SysConfigSrc) ->
    State#state_t{sys_config_src=SysConfigSrc}.

-spec root_dir(t()) -> file:filename() | undefined.
root_dir(#state_t{root_dir=RootDir}) ->
    RootDir.

-spec root_dir(t(), file:filename()) -> t().
root_dir(State, RootDir) ->
    State#state_t{root_dir=filename:absname(RootDir)}.

-spec filter_xref_warning(t()) -> fun((list({mfa(), mfa()})) -> list({mfa(), mfa()})).
filter_xref_warning(#state_t{filter_xref_warning=Filter}) -> Filter.

-spec filter_xref_warning(t(), fun((list({mfa(), mfa()})) -> list({mfa(), mfa()}))) -> t().
filter_xref_warning(State, Filter) -> State#state_t{filter_xref_warning=Filter}.


-spec add_configured_release(t(), rlx_release:t()) -> t().
add_configured_release(M=#state_t{configured_releases=Releases}, Release) ->
    M#state_t{configured_releases=Releases#{{rlx_release:name(Release),
                                             rlx_release:vsn(Release)} => Release}}.

-spec get_configured_release(t(), rlx_release:name(), rlx_release:vsn()) -> rlx_release:t().
get_configured_release(#state_t{configured_releases=Releases}, Name, Vsn) ->
    maps:get({Name, Vsn}, Releases).

-spec configured_releases(t()) -> releases().
configured_releases(#state_t{configured_releases=Releases}) ->
    Releases.

-spec realized_releases(t()) -> releases().
realized_releases(#state_t{realized_releases=Releases}) ->
    Releases.

-spec add_realized_release(t(), rlx_release:t()) -> t().
add_realized_release(State = #state_t{realized_releases=Releases}, Release) ->
    NewReleases = Releases#{{rlx_release:name(Release), rlx_release:vsn(Release)} => Release},
    State#state_t{realized_releases=NewReleases}.

-spec update_realized_release(t(), rlx_release:t()) ->
     t().
update_realized_release(M=#state_t{realized_releases=Releases}, Release) ->
    M#state_t{realized_releases=Releases#{{rlx_release:name(Release),
                                           rlx_release:vsn(Release)} => Release}}.

-spec available_apps(t()) -> #{atom() => rlx_app_info:t()}.
available_apps(#state_t{available_apps=Apps}) ->
    Apps.

-spec available_apps(t(), #{atom() => rlx_app_info:t()}) -> t().
available_apps(M, NewApps) ->
    M#state_t{available_apps=NewApps}.

-spec dev_mode(t()) -> boolean().
dev_mode(#state_t{dev_mode=DevMode}) ->
    DevMode.

-spec dev_mode(t(), boolean()) -> t().
dev_mode(S=#state_t{mode=dev}, false) ->
    S#state_t{dev_mode=false,
              mode=undefined};
dev_mode(S, true) ->
    S#state_t{dev_mode=false,
              mode=dev};
dev_mode(S, DevMode) ->
    S#state_t{dev_mode=DevMode}.

-spec mode(t()) -> mode().
mode(#state_t{mode=Mode}) ->
    Mode.

-spec mode(t(), mode()) -> t().
mode(S, Mode) ->
    S#state_t{mode=Mode}.

-spec include_erts(t()) -> boolean() | file:filename() | undefined.
include_erts(#state_t{include_erts=IncludeErts}) ->
    IncludeErts.

-spec include_erts(t(), boolean() | file:filename()) -> t().
include_erts(S, IncludeErts) ->
    S#state_t{include_erts=IncludeErts}.

-spec include_nodetool(t()) -> boolean() | file:filename() | undefined.
include_nodetool(#state_t{include_nodetool=IncludeNodetool}) ->
    IncludeNodetool.

-spec include_nodetool(t(), boolean() | file:filename()) -> t().
include_nodetool(S, IncludeNodetool) ->
    S#state_t{include_nodetool=IncludeNodetool}.

-spec overlay(t()) -> list() | undefined.
overlay(#state_t{overlay=Overlay}) ->
    Overlay.

-spec overlay(t(), list()) -> t().
overlay(S, Overlay) ->
    S#state_t{overlay=Overlay}.

overlay_vars_values(#state_t{overlay_vars_values=OverlayVarsValues}) ->
    OverlayVarsValues.

overlay_vars_values(S, OverlayVarsValues) ->
    S#state_t{overlay_vars_values=OverlayVarsValues}.

overlay_vars(#state_t{overlay_vars=OverlayVars}) ->
    OverlayVars.

overlay_vars(S, OverlayVars) ->
    S#state_t{overlay_vars=OverlayVars}.

-spec extended_start_script_hooks(t()) -> list() | undefined.
extended_start_script_hooks(#state_t{extended_start_script_hooks=ExtendedStartScriptHooks}) ->
    ExtendedStartScriptHooks.

-spec extended_start_script_hooks(t(), list()) -> t().
extended_start_script_hooks(S, ExtendedStartScriptHooks) ->
    S#state_t{extended_start_script_hooks=ExtendedStartScriptHooks}.

-spec extended_start_script_extensions(t()) -> list() | undefined.
extended_start_script_extensions(#state_t{extended_start_script_extensions=ExtendedStartScriptExtensions}) ->
    ExtendedStartScriptExtensions.

-spec extended_start_script_extensions(t(), list()) -> t().
extended_start_script_extensions(S, ExtendedStartScriptExtensions) ->
    S#state_t{extended_start_script_extensions=ExtendedStartScriptExtensions}.

-spec extended_start_script(t()) -> boolean() | undefined.
extended_start_script(#state_t{extended_start_script=ExtendedStartScript}) ->
    ExtendedStartScript.

-spec extended_start_script(t(), boolean()) -> t().
extended_start_script(S, ExtendedStartScript) ->
    S#state_t{extended_start_script=ExtendedStartScript}.

-spec include_start_scripts_for(t()) -> [atom()] | undefined.
include_start_scripts_for(#state_t{include_start_scripts_for=IncludeStartScriptsFor}) ->
    IncludeStartScriptsFor.

-spec include_start_scripts_for(t(), [atom()]) -> t().
include_start_scripts_for(S, IncludeStartScriptsFor) ->
    S#state_t{include_start_scripts_for=IncludeStartScriptsFor}.

-spec generate_start_script(t()) -> boolean() | undefined.
generate_start_script(#state_t{generate_start_script=GenerateStartScript}) ->
    GenerateStartScript.

-spec generate_start_script(t(), boolean()) -> t().
generate_start_script(S, GenerateStartScript) ->
    S#state_t{generate_start_script=GenerateStartScript}.

-spec include_src(t()) -> boolean() | undefined.
include_src(#state_t{include_src=IncludeSrc}) ->
    IncludeSrc.

-spec include_src(t(), boolean()) -> t().
include_src(S, IncludeSrc) ->
    S#state_t{include_src=IncludeSrc}.

-spec system_libs(t()) -> boolean() | file:filename() | undefined.
system_libs(#state_t{system_libs=SystemLibs}) ->
    SystemLibs.

-spec system_libs(t(), boolean() | file:filename()) -> t().
system_libs(S, SystemLibs) ->
    S#state_t{system_libs=SystemLibs}.

-spec upfrom(t(), string() | binary() | undefined) -> t().
upfrom(State, UpFrom) ->
    State#state_t{upfrom=UpFrom}.

-spec format(t()) -> iolist().
format(Mod) ->
    format(Mod, 0).

-spec format(t(), non_neg_integer()) -> iolist().
format(#state_t{output_dir=OutDir, 
                lib_dirs=LibDirs},
       Indent) ->
    [rlx_util:indent(Indent),
     <<"state:\n">>,
     rlx_util:indent(Indent + 2), "output_dir: ", OutDir, "\n",
     rlx_util:indent(Indent + 2), "lib_dirs: \n",
     [[rlx_util:indent(Indent + 3), LibDir, ",\n"] || LibDir <- LibDirs]].

-spec warnings_as_errors(t()) -> boolean().
warnings_as_errors(#state_t{warnings_as_errors=WarningsAsErrors}) ->
    WarningsAsErrors.

-spec warnings_as_errors(t(), boolean()) -> t().
warnings_as_errors(State, WarningsAsErrors) ->
    State#state_t{warnings_as_errors=WarningsAsErrors}.

-spec src_tests(t()) -> boolean().
src_tests(#state_t{src_tests=SrcTests}) ->
    SrcTests.

-spec src_tests(t(), boolean()) -> t().
src_tests(State, SrcTests) ->
    State#state_t{src_tests=SrcTests}.

-spec check_for_undefined_functions(t()) -> boolean().
check_for_undefined_functions(#state_t{check_for_undefined_functions=CheckForUndefinedFunctions}) ->
    CheckForUndefinedFunctions.

-spec check_for_undefined_functions(t(), boolean()) -> t().
check_for_undefined_functions(State, CheckForUndefinedFunctions) ->
    State#state_t{check_for_undefined_functions=CheckForUndefinedFunctions}.

-spec exref(t()) -> boolean() | [atom()].
exref(#state_t{exref=ExRef}) ->
    ExRef.

-spec exref(t(), boolean() | [atom()]) -> t().
exref(State, ExRef) ->
    State#state_t{exref=ExRef}.

is_relx_sasl(#state_t{is_relx_sasl=IsRelxSasl}) ->
    IsRelxSasl.
