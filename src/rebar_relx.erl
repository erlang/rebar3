%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_relx).

-export([do/2,
         opt_spec_list/0,
         format_error/1]).

-ifdef(TEST).
-export([merge_overlays/1]).
-endif.

-include_lib("providers/include/providers.hrl").
-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec do(atom(), rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(Provider, State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    Release = case proplists:get_value(relname, Opts, undefined) of
                  undefined ->
                      undefined;
                  R ->
                      case proplists:get_value(relvsn, Opts, undefined) of
                          undefined ->
                              list_to_atom(R);
                          RelVsn ->
                              {list_to_atom(R), RelVsn}
                      end
              end,

    %% TODO: read in ./relx.config if it exists or --config value
    RelxConfig = rebar_state:get(State, relx, []),

    ProfileString = rebar_dir:profile_dir_name(State),
    ExtraOverlays = [{profile_string, ProfileString}],

    DefaultOutputDir = filename:join(rebar_dir:base_dir(State), ?DEFAULT_RELEASE_DIR),
    RelxConfig1 = [output_dir(DefaultOutputDir, Opts),
                   {overlay_vars_values, ExtraOverlays},
                   {overlay_vars, [{base_dir, rebar_dir:base_dir(State)}]}
                   | merge_overlays(RelxConfig)],
    {ok, RelxState} = rlx_config:to_state(RelxConfig1),


    Providers = rebar_state:providers(State),
    Cwd = rebar_state:dir(State),
    rebar_hooks:run_project_and_app_hooks(Cwd, pre, Provider, Providers, State),

    case Provider of
        release ->
            relx:build_release(Release, all_apps(State), RelxState);
        tar ->
            relx:build_tar(Release, all_apps(State), RelxState);
        relup ->
            ToVsn = proplists:get_value(relvsn, Opts, undefined),
            UpFromVsn = proplists:get_value(upfrom, Opts, undefined),
            relx:build_relup(Release, ToVsn, UpFromVsn, RelxState)
    end,

    rebar_hooks:run_project_and_app_hooks(Cwd, post, Provider, Providers, State),

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Error) ->
    io_lib:format("~p", [Error]).

%% Don't override output_dir if the user passed one on the command line
output_dir(DefaultOutputDir, Options) ->
    {output_dir, proplists:get_value(output_dir, Options, DefaultOutputDir)}.

merge_overlays(Config) ->
    {Overlays, Others} =
        lists:partition(fun(C) when element(1, C) =:= overlay -> true;
                           (_) -> false
                        end, Config),
    %% Have profile overlay entries come before others to match how profiles work elsewhere
    NewOverlay = lists:flatmap(fun({overlay, Overlay}) -> Overlay end, lists:reverse(Overlays)),
    [{overlay, NewOverlay} | Others].

%%

%% Returns a map of all apps that are part of the rebar3 project.
%% This means the project apps and dependencies but not OTP libraries.
-spec all_apps(rebar_state:t()) -> #{atom() => rlx_app_info:t()}.
all_apps(State) ->
    lists:foldl(fun(AppInfo, Acc) ->
                        Acc#{binary_to_atom(rebar_app_info:name(AppInfo), utf8)
                             => rebar_app_info:app_to_map(AppInfo)}
                end, #{}, rebar_state:project_apps(State) ++ rebar_state:all_deps(State)).

%%

-spec opt_spec_list() -> [getopt:option_spec()].
opt_spec_list() ->
    [{relname,  $n, "relname",  string,
      "Specify the name for the release that will be generated"},
     {relvsn, $v, "relvsn", string, "Specify the version for the release"},
     {upfrom, $u, "upfrom", string,
      "Only valid with relup target, specify the release to upgrade from"},
     {output_dir, $o, "output-dir", string,
      "The output directory for the release. This is `./` by default."},
     {help, $h, "help", undefined,
      "Print usage"},
     {lib_dir, $l, "lib-dir", string,
      "Additional dir that should be searched for OTP Apps"},
     {log_level, $V, "verbose", {integer, 2},
      "Verbosity level, maybe between 0 and 3"},
     {dev_mode, $d, "dev-mode", boolean,
      "Symlink the applications and configuration into the release instead of copying"},
     {include_erts, $i, "include-erts", string,
      "If true include a copy of erts used to build with, if a path include erts at that path. If false, do not include erts"},
     {override, $a, "override", string,
      "Provide an app name and a directory to override in the form <appname>:<app directory>"},
     {config, $c, "config", {string, ""}, "The path to a config file"},
     {overlay_vars, undefined, "overlay_vars", string, "Path to a file of overlay variables"},
     {vm_args, undefined, "vm_args", string, "Path to a file to use for vm.args"},
     {sys_config, undefined, "sys_config", string, "Path to a file to use for sys.config"},
     {system_libs, undefined, "system_libs", string, "Boolean or path to dir of Erlang system libs"},
     {version, undefined, "version", undefined, "Print relx version"},
     {root_dir, $r, "root", string, "The project root directory"}].
