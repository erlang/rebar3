-module(rebar_app_info).

-export([new/0,
         new/1,
         new/2,
         new/3,
         new/4,
         new/5,
         app_to_map/1,
         update_opts/3,
         update_opts/2,
         update_opts_deps/2,
         discover/2,
         name/1,
         name/2,
         app_file_src/1,
         app_file_src/2,
         app_file_src_script/1,
         app_file_src_script/2,
         app_file/1,
         app_file/2,
         app_details/1,
         app_details/2,
         parent/1,
         parent/2,
         original_vsn/1,
         original_vsn/2,
         vsn/1,
         vsn/2,
         priv_dir/1,
         applications/1,
         applications/2,
         included_applications/1,
         included_applications/2,
         optional_applications/1,
         optional_applications/2,
         profiles/1,
         profiles/2,
         deps/1,
         deps/2,
         dep_level/1,
         dep_level/2,
         fetch_dir/1,
         fetch_dir/2,
         dir/1,
         dir/2,
         out_dir/1,
         out_dir/2,
         ebin_dir/1,
         ebin_dir/2,
         default/1,
         default/2,
         opts/1,
         opts/2,
         get/2,
         get/3,
         set/3,
         source/1,
         source/2,
         project_type/1,
         project_type/2,
         is_lock/1,
         is_lock/2,
         is_checkout/1,
         is_checkout/2,
         valid/1,
         valid/2,
         is_available/1,
         is_available/2,

         verify_otp_vsn/1,
         has_all_artifacts/1,

         apply_overrides/2,
         add_to_profile/3,
         apply_profiles/2,
         deduplicate/1,
         do_deduplicate/2]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-export_type([t/0,
              project_type/0]).

-type project_type() :: rebar3 | mix | undefined.
-type app_vsn() :: binary() | string() | {git, short} | {git, long}.

-record(app_info_t, {name               :: binary() | undefined,
                     app_file_src       :: file:filename_all() | undefined,
                     app_file_src_script:: file:filename_all() | undefined,
                     app_file           :: file:filename_all() | undefined,
                     original_vsn       :: app_vsn() | undefined,
                     vsn                :: app_vsn() | undefined,
                     parent=root        :: binary() | root,
                     app_details=[]     :: list(),
                     applications=[]    :: list(),
                     included_applications=[] :: [atom()],
                     optional_applications=[] :: [atom()],
                     deps=[]            :: list(),
                     profiles=[default] :: [atom()],
                     default=dict:new() :: rebar_dict(),
                     opts=dict:new()    :: rebar_dict(),
                     dep_level=0        :: integer(),
                     fetch_dir          :: file:name(),
                     dir                :: file:name(),
                     out_dir            :: file:name(),
                     ebin_dir           :: file:name(),
                     source             :: string() | tuple() | checkout | undefined,
                     is_lock=false      :: boolean(),
                     is_checkout=false  :: boolean(),
                     valid              :: boolean() | undefined,
                     project_type       :: project_type(),
                     is_available=false :: boolean()}).

%%============================================================================
%% types
%%============================================================================
-type t() :: #app_info_t{}.

%%============================================================================
%% API
%% ============================================================================
%% @doc Build a new, empty, app info value. This is not of a lot of use and you
%% probably won't be doing this much.
-spec new() -> t().
new() ->
    #app_info_t{}.

%% @doc Build a new app info value with only the app name set.
-spec new(atom() | binary() | string()) ->
                 {ok, t()}.
new(AppName) ->
    {ok, #app_info_t{name=rebar_utils:to_binary(AppName)}}.

%% @doc Build a new app info value with only the name and version set.
-spec new(atom() | binary() | string(), app_vsn()) ->
                 {ok, t()}.
new(AppName, Vsn) ->
    {ok, #app_info_t{name=rebar_utils:to_binary(AppName),
                     vsn=Vsn,
                     original_vsn=Vsn}}.

%% @doc build a complete version of the app info with all fields set.
-spec new(atom() | binary() | string(), app_vsn(), file:name()) ->
                 {ok, t()}.
new(AppName, Vsn, Dir) ->
    {ok, #app_info_t{name=rebar_utils:to_binary(AppName),
                     vsn=Vsn,
                     original_vsn=Vsn,
                     fetch_dir=rebar_utils:to_list(Dir),
                     dir=rebar_utils:to_list(Dir),
                     out_dir=rebar_utils:to_list(Dir),
                     ebin_dir=filename:join(rebar_utils:to_list(Dir), "ebin")}}.

%% @doc build a complete version of the app info with all fields set.
-spec new(atom() | binary() | string(), app_vsn(), file:name(), list()) ->
                 {ok, t()}.
new(AppName, Vsn, Dir, Deps) ->
    {ok, #app_info_t{name=rebar_utils:to_binary(AppName),
                     vsn=Vsn,
                     original_vsn=Vsn,
                     fetch_dir=rebar_utils:to_list(Dir),
                     dir=rebar_utils:to_list(Dir),
                     out_dir=rebar_utils:to_list(Dir),
                     ebin_dir=filename:join(rebar_utils:to_list(Dir), "ebin"),
                     deps=Deps}}.

%% @doc build a complete version of the app info with all fields set.
-spec new(atom() | binary(), atom() | binary() | string(), app_vsn(), file:name(), list()) ->
                 {ok, t()}.
new(Parent, AppName, Vsn, Dir, Deps) ->
    {ok, #app_info_t{name=rebar_utils:to_binary(AppName),
                     parent=Parent,
                     vsn=Vsn,
                     original_vsn=Vsn,
                     fetch_dir=rebar_utils:to_list(Dir),
                     dir=rebar_utils:to_list(Dir),
                     out_dir=rebar_utils:to_list(Dir),
                     ebin_dir=filename:join(rebar_utils:to_list(Dir), "ebin"),
                     deps=Deps}}.

-spec app_to_map(t()) -> #{name := atom(),
                           vsn := app_vsn(),
                           applications := [atom()],
                           included_applications := [atom()],
                           optional_applications := [atom()],
                           dir := file:name(),
                           out_dir := file:name(),
                           ebin_dir := file:name(),
                           link := false}.
app_to_map(#app_info_t{name=Name,
                       vsn=Vsn,
                       applications=Applications,
                       included_applications=IncludedApplications,
                       optional_applications=OptionalApplications,
                       out_dir=OutDir,
                       ebin_dir=EbinDir}) ->
    #{name => ec_cnv:to_atom(Name),
      vsn => Vsn,
      applications => Applications,
      included_applications => IncludedApplications,
      optional_applications => OptionalApplications,
      dir => OutDir,
      out_dir => OutDir,
      ebin_dir => EbinDir,
      link => false}.

%% @doc update the opts based on the contents of a config
%% file for the app
-spec update_opts(t(), rebar_dict(), [any()]) -> t().
update_opts(AppInfo, Opts, Config) ->
    LockDeps = case source(AppInfo) of
                   Tuple when is_tuple(Tuple) andalso element(1, Tuple) =:= pkg ->
                       %% Deps are set separate for packages
                       %% instead of making it seem we have no deps
                       %% don't set anything here.
                       [];
                   _ ->
                       deps_from_config(dir(AppInfo), proplists:get_value(deps, Config, []))
               end,

    Plugins = proplists:get_value(plugins, Config, []),
    Terms = LockDeps++[{{plugins, default}, Plugins} | Config],
    true = rebar_config:verify_config_format(Terms),
    LocalOpts = dict:from_list(Terms),

    NewOpts = rebar_opts:merge_opts(LocalOpts, Opts),

    AppInfo#app_info_t{opts=NewOpts,
                       default=NewOpts}.

%% @doc update current app info opts by merging in a new dict of opts
-spec update_opts(t(), rebar_dict()) -> t().
update_opts(AppInfo=#app_info_t{opts=LocalOpts}, Opts) ->
    NewOpts = rebar_opts:merge_opts(LocalOpts, Opts),
    AppInfo#app_info_t{opts=NewOpts,
                       default=NewOpts}.

%% @doc update the opts based on new deps, usually from an app's hex registry metadata
-spec update_opts_deps(t(), [any()]) -> t().
update_opts_deps(AppInfo=#app_info_t{opts=Opts}, Deps) ->
    LocalOpts = dict:from_list([{{locks, default}, Deps}, {{deps, default}, Deps}]),
    NewOpts = rebar_opts:merge_opts(LocalOpts, Opts),
    AppInfo#app_info_t{opts=NewOpts,
                       default=NewOpts,
                       deps=Deps}.


%% @private extract the deps for an app in `Dir' based on its config file data
-spec deps_from_config(file:filename(), [any()]) -> [{tuple(), any()}, ...].
deps_from_config(Dir, ConfigDeps) ->
    case rebar_config:consult_lock_file(filename:join(Dir, ?LOCK_FILE)) of
        [] ->
            [{{deps, default}, ConfigDeps}];
        D ->
            %% We want the top level deps only from the lock file.
            %% This ensures deterministic overrides for configs.
            Deps = [X || X <- D, element(3, X) =:= 0],
            [{{locks, default}, D}, {{deps, default}, Deps}]
    end.

%% @doc discover a complete version of the app info with all fields set.
-spec discover(file:filename_all(), rebar_state:t()) -> {ok, t()} | not_found.
discover(Dir, State) ->
    case rebar_app_discover:find_app(Dir, all, State) of
        {true, AppInfo} ->
            {ok, AppInfo};
        false ->
            not_found
    end.

%% @doc get the name of the app.
-spec name(t()) -> binary().
name(#app_info_t{name=Name}) ->
    Name.

%% @doc set the name of the app.
-spec name(t(), atom() | binary() | string()) -> t().
name(AppInfo=#app_info_t{}, AppName) ->
    AppInfo#app_info_t{name=rebar_utils:to_binary(AppName)}.

%% @doc get the dictionary of options for the app.
-spec opts(t()) -> rebar_dict().
opts(#app_info_t{opts=Opts}) ->
    Opts.

%% @doc set the dictionary of options for the app.
-spec opts(t(), rebar_dict()) -> t().
opts(AppInfo, Opts) ->
    AppInfo#app_info_t{opts=Opts}.

%% @doc get the dictionary of options under the default profile.
%% Represents a root set prior to applying other profiles.
-spec default(t()) -> rebar_dict().
default(#app_info_t{default=Default}) ->
    Default.

%% @doc set the dictionary of options under the default profile.
%% Useful when re-applying profile.
-spec default(t(), rebar_dict()) -> t().
default(AppInfo, Default) ->
    AppInfo#app_info_t{default=Default}.

%% @doc look up a value in the dictionary of options; fails if
%% the key for it does not exist.
-spec get(t(), term()) -> term().
get(AppInfo, Key) ->
    {ok, Value} = dict:find(Key, AppInfo#app_info_t.opts),
    Value.

%% @doc look up a value in the dictionary of options; returns
%% a `Default' value otherwise.
-spec get(t(), term(), term()) -> term().
get(AppInfo, Key, Default) ->
    case dict:find(Key, AppInfo#app_info_t.opts) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.

%% @doc sets a given value in the dictionary of options for the app.
-spec set(t(), any(), any()) -> t().
set(AppInfo=#app_info_t{opts=Opts}, Key, Value) ->
    AppInfo#app_info_t{opts = dict:store(Key, Value, Opts)}.

%% @doc finds the .app.src file for an app, if any.
-spec app_file_src(t()) -> file:filename_all() | undefined.
app_file_src(#app_info_t{app_file_src=undefined, dir=Dir, name=Name, opts=Opts}) ->
    CandidatePaths = [filename:join([rebar_utils:to_list(Dir), Src, rebar_utils:to_list(Name)++".app.src"])
                      || Src <- rebar_opts:get(Opts, src_dirs, ["src"])],
    case lists:dropwhile(fun(Path) -> not filelib:is_file(Path) end, CandidatePaths) of
        [] -> undefined;
        [AppFileSrc|_] -> AppFileSrc
    end;
app_file_src(#app_info_t{app_file_src=AppFileSrc}) ->
    rebar_utils:to_list(AppFileSrc).

%% @doc sets the .app.src file for an app. An app without such a file
%% can explicitly be set with `undefined'.
-spec app_file_src(t(), file:filename_all() | undefined) -> t().
app_file_src(AppInfo=#app_info_t{}, undefined) ->
    AppInfo#app_info_t{app_file_src=undefined};
app_file_src(AppInfo=#app_info_t{}, AppFileSrc) ->
    AppInfo#app_info_t{app_file_src=rebar_utils:to_list(AppFileSrc)}.

%% @doc finds the .app.src.script file for an app, if any.
-spec app_file_src_script(t()) -> file:filename_all() | undefined.
app_file_src_script(#app_info_t{app_file_src_script=undefined, dir=Dir, name=Name}) ->
    AppFileSrcScript = filename:join([rebar_utils:to_list(Dir), "src", rebar_utils:to_list(Name)++".app.src.script"]),
    case filelib:is_file(AppFileSrcScript) of
        true ->
            AppFileSrcScript;
        false ->
            undefined
    end;
app_file_src_script(#app_info_t{app_file_src_script=AppFileSrcScript}) ->
    rebar_utils:to_list(AppFileSrcScript).

%% @doc sets the .app.src.script file for an app. An app without such a file
%% can explicitly be set with `undefined'.
-spec app_file_src_script(t(), file:filename_all()) -> t().
app_file_src_script(AppInfo=#app_info_t{}, undefined) ->
    AppInfo#app_info_t{app_file_src_script=undefined};
app_file_src_script(AppInfo=#app_info_t{}, AppFileSrcScript) ->
    AppInfo#app_info_t{app_file_src_script=rebar_utils:to_list(AppFileSrcScript)}.

%% @doc finds the .app file for an app, if any.
-spec app_file(t()) -> file:filename_all() | undefined.
app_file(#app_info_t{app_file=undefined, out_dir=Dir, name=Name}) ->
    AppFile = filename:join([rebar_utils:to_list(Dir), "ebin", rebar_utils:to_list(Name)++".app"]),
    case filelib:is_file(AppFile) of
        true ->
            AppFile;
        false ->
            undefined
    end;
app_file(#app_info_t{app_file=AppFile}) ->
    AppFile.

%% @doc sets the .app file for an app.
-spec app_file(t(), file:filename_all() | undefined) -> t().
app_file(AppInfo=#app_info_t{}, AppFile) ->
    AppInfo#app_info_t{app_file=AppFile}.

%% @doc returns the information stored in the app's app file,
%% or if none, from the .app.src file.
-spec app_details(t()) -> list().
app_details(AppInfo=#app_info_t{app_details=[]}) ->
    case app_file(AppInfo) of
        undefined ->
            try rebar_config:consult_app_file(app_file_src(AppInfo)) of
                [] -> [];
                [{application, _Name, AppDetails}] -> AppDetails
            catch
                _:_ ->
                    []
            end;
        AppFile ->
            try rebar_file_utils:try_consult(AppFile) of
                [] -> [];
                [{application, _Name, AppDetails}] -> AppDetails
            catch
                throw:{error, {Module, Reason}} ->
                    ?DEBUG("Warning, falling back to .app.src because of: ~ts",
                          [Module:format_error(Reason)]),
                    case rebar_config:consult_app_file(app_file_src(AppInfo)) of
                        [] -> [];
                        [{application, _Name, AppDetails}] -> AppDetails
                    end
            end
    end;
app_details(#app_info_t{app_details=AppDetails}) ->
    AppDetails.

%% @doc stores the information that would be returned from the
%% app file, when reading from `app_details/1'.
-spec app_details(t(), list()) -> t().
app_details(AppInfo=#app_info_t{}, AppDetails) ->
    AppInfo#app_info_t{app_details=AppDetails}.

%% @doc returns the app's parent in the dep tree.
-spec parent(t()) -> root | binary().
parent(#app_info_t{parent=Parent}) ->
    Parent.

%% @doc sets the app's parent.
-spec parent(t(), binary() | root) -> t().
parent(AppInfo=#app_info_t{}, Parent) ->
    AppInfo#app_info_t{parent=Parent}.

%% @doc returns the original version of the app (unevaluated if
%% asking for a semver)
-spec original_vsn(t()) -> app_vsn().
original_vsn(#app_info_t{original_vsn=Vsn}) ->
    Vsn.

%% @doc stores the original version of the app (unevaluated if
%% asking for a semver)
-spec original_vsn(t(), app_vsn()) -> t().
original_vsn(AppInfo=#app_info_t{}, Vsn) ->
    AppInfo#app_info_t{original_vsn=Vsn}.

%% @doc returns the version of the app after evaluation
-spec vsn(t()) -> app_vsn().
vsn(#app_info_t{vsn=Vsn}) ->
    Vsn.

%% @doc sets the evaluated vsn of the app
-spec vsn(t(), app_vsn()) -> t().
vsn(AppInfo=#app_info_t{}, Vsn) ->
    AppInfo#app_info_t{vsn=Vsn}.

%% @doc returns the list of applications the app depends on.
-spec applications(t()) -> list().
applications(#app_info_t{applications=Applications}) ->
    Applications.

%% @doc sets the list of applications the app depends on.
%% Should be obtained from the app file.
-spec applications(t(), list()) -> t().
applications(AppInfo=#app_info_t{}, Applications) ->
    AppInfo#app_info_t{applications=Applications}.

%% @doc returns the list of included_applications the app depends on.
-spec included_applications(t()) -> list().
included_applications(#app_info_t{included_applications=Applications}) ->
    Applications.

%% @doc sets the list of applications the app depends on.
%% Should be obtained from the app file.
-spec included_applications(t(), list()) -> t().
included_applications(AppInfo=#app_info_t{}, Applications) ->
    AppInfo#app_info_t{included_applications=Applications}.

%% @doc returns the list of optional_applications the app depends on.
-spec optional_applications(t()) -> list().
optional_applications(#app_info_t{optional_applications=Applications}) ->
    Applications.

%% @doc sets the list of optional applications the app depends on.
%% Should be obtained from the app file.
-spec optional_applications(t(), list()) -> t().
optional_applications(AppInfo=#app_info_t{}, Applications) ->
    AppInfo#app_info_t{optional_applications=Applications}.

%% @doc returns the list of active profiles
-spec profiles(t()) -> list().
profiles(#app_info_t{profiles=Profiles}) ->
    Profiles.

%% @doc sets the list of active profiles
-spec profiles(t(), list()) -> t().
profiles(AppInfo=#app_info_t{}, Profiles) ->
    AppInfo#app_info_t{profiles=Profiles}.

%% @doc returns the list of dependencies
-spec deps(t()) -> list().
deps(#app_info_t{deps=Deps}) ->
    Deps.

%% @doc sets the list of dependencies.
-spec deps(t(), list()) -> t().
deps(AppInfo=#app_info_t{}, Deps) ->
    AppInfo#app_info_t{deps=Deps}.

%% @doc returns the level the app has in the lock files or in the
%% dep tree.
-spec dep_level(t()) -> non_neg_integer().
dep_level(#app_info_t{dep_level=Level}) ->
    Level.

%% @doc sets the level the app has in the lock files or in the
%% dep tree.
-spec dep_level(t(), non_neg_integer()) -> t().
dep_level(AppInfo=#app_info_t{}, Level) ->
    AppInfo#app_info_t{dep_level=Level}.

%% @doc returns the directory to fetch the dep source to
-spec fetch_dir(t()) -> file:name().
fetch_dir(#app_info_t{fetch_dir=FetchDir}) ->
    FetchDir.

%% @doc returns the directory to fetch the dep source to
 -spec fetch_dir(t(), file:name()) -> t().
fetch_dir(AppInfo=#app_info_t{}, FetchDir) ->
    AppInfo#app_info_t{fetch_dir=FetchDir}.

%% @doc returns the directory that contains the app.
-spec dir(t()) -> file:name().
dir(#app_info_t{dir=Dir}) ->
    Dir.

%% @doc sets the directory that contains the app.
-spec dir(t(), file:name()) -> t().
dir(AppInfo=#app_info_t{out_dir=undefined}, Dir) ->
    AppInfo#app_info_t{dir=rebar_utils:to_list(Dir),
                       out_dir=rebar_utils:to_list(Dir)};
dir(AppInfo=#app_info_t{}, Dir) ->
    AppInfo#app_info_t{dir=rebar_utils:to_list(Dir)}.

%% @doc returns the directory where build artifacts for the app
%% should go
-spec out_dir(t()) -> file:name().
out_dir(#app_info_t{out_dir=OutDir}) ->
    OutDir.

%% @doc sets the directory where build artifacts for the app
%% should go
-spec out_dir(t(), file:name()) -> t().
out_dir(AppInfo=#app_info_t{}, OutDir) ->
    AppInfo#app_info_t{out_dir=rebar_utils:to_list(OutDir),
                       ebin_dir=filename:join(rebar_utils:to_list(OutDir), "ebin")}.

%% @doc gets the directory where ebin files for the app should go
-spec ebin_dir(t()) -> file:name().
ebin_dir(#app_info_t{ebin_dir=undefined,
                     out_dir=OutDir}) ->
    filename:join(rebar_utils:to_list(OutDir), "ebin");
ebin_dir(#app_info_t{ebin_dir=EbinDir}) ->
    EbinDir.

%% @doc sets the directory where beam files should go
-spec ebin_dir(t(), file:name()) -> t().
ebin_dir(AppInfo, EbinDir) ->
    AppInfo#app_info_t{ebin_dir=EbinDir}.

%% @doc gets the directory where private files for the app should go
-spec priv_dir(t()) -> file:name().
priv_dir(#app_info_t{out_dir=OutDir}) ->
    rebar_utils:to_list(filename:join(OutDir, "priv")).

%% @doc finds the source specification for the app
-spec source(t()) -> string() | tuple().
source(#app_info_t{source=Source}) ->
    Source.

%% @doc sets the source specification for the app
-spec source(t(), string() | tuple() | checkout | undefined) -> t().
source(AppInfo=#app_info_t{}, Source) ->
    AppInfo#app_info_t{source=Source}.

%% @doc returns the lock status for the app
-spec is_lock(t()) -> boolean().
is_lock(#app_info_t{is_lock=IsLock}) ->
    IsLock.

%% @doc sets the lock status for the app
-spec is_lock(t(), boolean()) -> t().
is_lock(AppInfo=#app_info_t{}, IsLock) ->
    AppInfo#app_info_t{is_lock=IsLock}.

%% @doc returns whether the app is a checkout app or not
-spec is_checkout(t()) -> boolean().
is_checkout(#app_info_t{is_checkout=IsCheckout}) ->
    IsCheckout.

%% @doc sets whether the app is a checkout app or not
-spec is_checkout(t(), boolean()) -> t().
is_checkout(AppInfo=#app_info_t{}, IsCheckout) ->
    AppInfo#app_info_t{is_checkout=IsCheckout}.

%% @doc returns whether the app source exists in the deps dir
-spec is_available(t()) -> boolean().
is_available(#app_info_t{is_available=IsAvailable}) ->
    IsAvailable.

%% @doc sets whether the app's source is available
%% only set if the app's source is found in the expected dep directory
-spec is_available(t(), boolean()) -> t().
is_available(AppInfo=#app_info_t{}, IsAvailable) ->
    AppInfo#app_info_t{is_available=IsAvailable}.

%% @doc
-spec project_type(t()) -> atom().
project_type(#app_info_t{project_type=ProjectType}) ->
    ProjectType.

%% @doc
-spec project_type(t(), atom()) -> t().
project_type(AppInfo=#app_info_t{}, ProjectType) ->
    AppInfo#app_info_t{project_type=ProjectType}.


%% @doc returns whether the app is valid (built) or not
-spec valid(t()) -> boolean().
valid(AppInfo=#app_info_t{valid=undefined}) ->
    case rebar_app_utils:validate_application_info(AppInfo) =:= true
        andalso has_all_artifacts(AppInfo) =:= true of
        true ->
            true;
        _ ->
            false
    end;
valid(#app_info_t{valid=Valid}) ->
    Valid.

%% @doc sets whether the app is valid (built) or not. If left unset,
%% rebar3 will do the detection of the status itself.
%% Explicitly setting the value to `undefined' can force a re-evaluation.
-spec valid(t(), boolean() | undefined) -> t().
valid(AppInfo=#app_info_t{}, Valid) ->
    AppInfo#app_info_t{valid=Valid}.

%% @doc checks whether the app can be built with the current
%% Erlang/OTP version. If the check fails, the function raises
%% an exception and displays an error.
-spec verify_otp_vsn(t()) -> ok | no_return().
verify_otp_vsn(AppInfo) ->
    rebar_utils:check_min_otp_version(rebar_app_info:get(AppInfo, minimum_otp_vsn, undefined)),
    rebar_utils:check_blacklisted_otp_versions(rebar_app_info:get(AppInfo, blacklisted_otp_vsns, [])).

%% @doc checks whether all the build artifacts for an app to be considered
%% valid are present.
-spec has_all_artifacts(#app_info_t{}) -> true | {false, file:filename()}.
has_all_artifacts(AppInfo) ->
    Artifacts = rebar_app_info:get(AppInfo, artifacts, []),
    OutDir = out_dir(AppInfo),
    Context = [{base_dir, rebar_app_info:get(AppInfo, base_dir, ?DEFAULT_BASE_DIR)}
              ,{profile_dir, rebar_dir:profile_dir(opts(AppInfo), profiles(AppInfo))}
              ,{out_dir, OutDir}],
    all(OutDir, Context, Artifacts).

%% @private checks that all files/artifacts in the directory are found.
%% Template evaluation must happen and a bbmustache context needs to
%% be provided.
-spec all(file:filename(), term(), [string()]) -> true | {false, string()}.
all(_, _, []) ->
    true;
all(Dir, Context, [File|Artifacts]) ->
    FilePath = filename:join(Dir, rebar_templater:render(File, Context)),
    case filelib:is_regular(FilePath) of
        false ->
            ?DEBUG("Missing artifact ~ts", [FilePath]),
            {false, File};
        true ->
            all(Dir, Context, Artifacts)
    end.

%%%%%

%% @doc given a set of override rules, modify the app info accordingly
-spec apply_overrides(list(), t()) -> t().
apply_overrides(Overrides, AppInfo) ->
    Name = binary_to_atom(rebar_app_info:name(AppInfo), utf8),
    Opts = rebar_opts:apply_overrides(opts(AppInfo), Name, Overrides),
    AppInfo#app_info_t{default=Opts, opts=Opts}.

%% @doc adds a new profile with its own config to the app data
-spec add_to_profile(t(), atom(), [{_,_}]) -> t().
add_to_profile(AppInfo, Profile, KVs) when is_atom(Profile), is_list(KVs) ->
    Opts = rebar_opts:add_to_profile(opts(AppInfo), Profile, KVs),
    AppInfo#app_info_t{opts=Opts}.

%% @doc applies and merges the profile configuration in the specified order
%% of profiles (or for a single profile) and returns an app info record
%% with the resulting configuration
-spec apply_profiles(t(), atom() | [atom(),...]) -> t().
apply_profiles(AppInfo, Profile) when not is_list(Profile) ->
    apply_profiles(AppInfo, [Profile]);
apply_profiles(AppInfo, [default]) ->
    AppInfo;
apply_profiles(AppInfo=#app_info_t{default = Defaults, profiles=CurrentProfiles}, Profiles) ->
    AppliedProfiles = case Profiles of
                          %% Head of list global profile is special, only for use by rebar3
                          %% It does not clash if a user does `rebar3 as global...` but when
                          %% it is the head we must make sure not to prepend `default`
                          [global | _] ->
                              Profiles;
                          _ ->
                              deduplicate(CurrentProfiles ++ Profiles)
                      end,

    ConfigProfiles = rebar_app_info:get(AppInfo, profiles, []),

    NewOpts =
        lists:foldl(fun(default, OptsAcc) ->
                            OptsAcc;
                       (Profile, OptsAcc) ->
                            case proplists:get_value(Profile, ConfigProfiles, []) of
                                OptsList when is_list(OptsList) ->
                                    ProfileOpts = dict:from_list(OptsList),
                                    rebar_opts:merge_opts(Profile, ProfileOpts, OptsAcc);
                                Other ->
                                    throw(?PRV_ERROR({profile_not_list, Profile, Other}))
                            end
                    end, Defaults, AppliedProfiles),
    AppInfo#app_info_t{profiles = AppliedProfiles, opts=NewOpts}.

%% @private drops duplicated profile definitions
-spec deduplicate(list()) -> list().
deduplicate(Profiles) ->
    do_deduplicate(lists:reverse(Profiles), []).

%% @private drops duplicated profile definitions
-spec do_deduplicate(list(), list()) -> list().
do_deduplicate([], Acc) ->
    Acc;
do_deduplicate([Head | Rest], Acc) ->
    case lists:member(Head, Acc) of
        true -> do_deduplicate(Rest, Acc);
        false -> do_deduplicate(Rest, [Head | Acc])
    end.
