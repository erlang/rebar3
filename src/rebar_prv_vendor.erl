-module(rebar_prv_vendor).
-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, vendor).
-define(NAMESPACE, experimental).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(
        State,
        providers:create([{name, ?PROVIDER},
                          {namespace, ?NAMESPACE},
                          {module, ?MODULE},
                          {bare, true},
                          {deps, ?DEPS},
                          {example, ""},
                          {short_desc, "Turns dependencies into top-level apps"},
                          {desc, "Turns dependencies into top-level applications"},
                          {opts, []}])
    ),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% Only vendor default profile runs
    case rebar_state:current_profiles(State) of
        [default] ->
            case check_project_layout(State) of
                umbrella -> do_(State);
                _ -> ?PRV_ERROR(not_umbrella)
            end;
        Profiles ->
            ?DEBUG("Profiles found: ~p, skipping", [Profiles]),
            {ok, State}
    end.

%% @doc convert a given exception's payload into an io description.
-spec format_error(any()) -> iolist().
format_error(not_umbrella) ->
    io_lib:format("Vendoring can only work on umbrella applications", []).

do_(InitState) ->
    %% TODO: figure out how to vendor and local-load plugins
    %% delete the vendored files (or move them to another place)
    RootDir = rebar_dir:root_dir(InitState),
    VendorDir = filename:join(RootDir, "vendor"),
    VendorBak = filename:join(RootDir, "_vendor"),
    catch rebar_file_utils:rm_rf(VendorBak),
    catch rebar_file_utils:mv(VendorDir, VendorBak),
    filelib:ensure_dir(filename:join(VendorDir, ".touch")),
    %% remove the src_dirs option for vendored files
    CleanDirs = rebar_dir:lib_dirs(InitState) -- ["vendor/*"],
    CleanState = rebar_state:set(InitState, project_app_dirs, CleanDirs),
    %% re-run discovery
    {ok, TmpState1} = rebar_prv_app_discovery:do(CleanState),
    %% run a full fetch (which implicitly upgrades, since the lock file
    %% should be unset for any vendored app)
    {ok, TmpState2} = rebar_prv_install_deps:do(TmpState1),
    %% move the libs to the vendor path
    AllDeps = rebar_state:lock(TmpState2),
    [begin
        AppDir = rebar_app_info:dir(Dep),
        NewAppDir = filename:join(VendorDir, filename:basename(AppDir)),
        rebar_file_utils:mv(AppDir, NewAppDir)
     end || Dep <- AllDeps, not(rebar_app_info:is_checkout(Dep))],
    %% add the src_dirs options to the rebar.config file
    %% -- we don't actually want to mess with the user's file so we have to
    %% let them know what it should be:
    NewAppDirs = CleanDirs ++ ["vendor/*"],
    ?CONSOLE("Vendoring in place. To use the vendored libraries, configure "
             "the source application directories for your project with:~n~n"
             "{project_app_dirs, ~p}.~n~n"
             "and move the {deps, ...} tuple to the rebar.config files "
             "of the proper top-level applications rather than the project root.",
             [NewAppDirs]),
    State1 = rebar_state:set(InitState, project_app_dirs, NewAppDirs),
    {ok, State1}.

%% ignore the badmatch there; the error value is real, but comes from
%% exceptions at the call-sites of sub-functions within
%% `rebar_app_discovery:do/1' and Dialyzer loses track of this being
%% a possibility and elides the `{error, _}' return tuple from the
%% signature while it is real.
-dialyzer({no_match, check_project_layout/1}).
check_project_layout(State) ->
    %% Do a project_app_discover run, look for the project root,
    %% then drop the state.
    %% No need to drop the vendor config here if it exists because it
    %% only exists if we have the right structure.
    case rebar_prv_app_discovery:do(State) of
        {error, Reason} ->
            {error, Reason};
        {ok, TmpState} ->
            Apps = rebar_state:project_apps(TmpState),
            %% Code duplicated from rebar_prv_lock:define_root_app/2
            RootDir = rebar_dir:root_dir(TmpState),
            case ec_lists:find(fun(X) ->
                    ec_file:real_dir_path(rebar_app_info:dir(X)) =:=
                    ec_file:real_dir_path(RootDir)
                 end, Apps) of
                {ok, _App} ->
                    non_umbrella;
                error ->
                    umbrella
            end
    end.

