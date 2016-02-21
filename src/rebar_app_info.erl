-module(rebar_app_info).

-export([new/0,
         new/1,
         new/2,
         new/3,
         new/4,
         new/5,
         update_opts/3,
         discover/1,
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
         ebin_dir/1,
         priv_dir/1,
         applications/1,
         applications/2,
         profiles/1,
         profiles/2,
         deps/1,
         deps/2,
         dep_level/1,
         dep_level/2,
         dir/1,
         dir/2,
         out_dir/1,
         out_dir/2,
         default/1,
         default/2,
         opts/1,
         opts/2,
         get/2,
         get/3,
         set/3,
         resource_type/1,
         resource_type/2,
         source/1,
         source/2,
         is_lock/1,
         is_lock/2,
         is_checkout/1,
         is_checkout/2,
         valid/1,
         valid/2,

         verify_otp_vsn/1,
         has_all_artifacts/1,

         apply_overrides/2,
         add_to_profile/3,
         apply_profiles/2,
         deduplicate/1,
         do_deduplicate/2]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-export_type([t/0]).

-record(app_info_t, {name               :: binary(),
                     app_file_src       :: file:filename_all() | undefined,
                     app_file_src_script:: file:filename_all() | undefined,
                     app_file           :: file:filename_all() | undefined,
                     original_vsn       :: binary() | string() | undefined,
                     parent=root        :: binary() | root,
                     app_details=[]     :: list(),
                     applications=[]    :: list(),
                     deps=[]            :: list(),
                     profiles=[default] :: [atom()],
                     default=dict:new() :: rebar_dict(),
                     opts=dict:new()    :: rebar_dict(),
                     dep_level=0        :: integer(),
                     dir                :: file:name(),
                     out_dir            :: file:name(),
                     resource_type      :: pkg | src,
                     source             :: string() | tuple() | checkout | undefined,
                     is_lock=false      :: boolean(),
                     is_checkout=false  :: boolean(),
                     valid              :: boolean()}).

%%============================================================================
%% types
%%============================================================================
-type t() :: #app_info_t{}.

%%============================================================================
%% API
%% ============================================================================
%% @doc Build a new, empty, app info value. This is not of a lot of use and you
%% probably wont be doing this much.
-spec new() -> t().
new() ->
    #app_info_t{}.

-spec new(atom() | binary() | string()) ->
                 {ok, t()}.
new(AppName) ->
    {ok, #app_info_t{name=ec_cnv:to_binary(AppName)}}.

-spec new(atom() | binary() | string(), binary() | string()) ->
                 {ok, t()}.
new(AppName, Vsn) ->
    {ok, #app_info_t{name=ec_cnv:to_binary(AppName),
                     original_vsn=Vsn}}.

%% @doc build a complete version of the app info with all fields set.
-spec new(atom() | binary() | string(), binary() | string(), file:name()) ->
                 {ok, t()}.
new(AppName, Vsn, Dir) ->
    {ok, #app_info_t{name=ec_cnv:to_binary(AppName),
                     original_vsn=Vsn,
                     dir=ec_cnv:to_list(Dir),
                     out_dir=ec_cnv:to_list(Dir)}}.

%% @doc build a complete version of the app info with all fields set.
-spec new(atom() | binary() | string(), binary() | string(), file:name(), list()) ->
                 {ok, t()}.
new(AppName, Vsn, Dir, Deps) ->
    {ok, #app_info_t{name=ec_cnv:to_binary(AppName),
                     original_vsn=Vsn,
                     dir=ec_cnv:to_list(Dir),
                     out_dir=ec_cnv:to_list(Dir),
                     deps=Deps}}.

%% @doc build a complete version of the app info with all fields set.
-spec new(atom() | binary(), atom() | binary() | string(), binary() | string(), file:name(), list()) ->
                 {ok, t()}.
new(Parent, AppName, Vsn, Dir, Deps) ->
    {ok, #app_info_t{name=ec_cnv:to_binary(AppName),
                     parent=Parent,
                     original_vsn=Vsn,
                     dir=ec_cnv:to_list(Dir),
                     out_dir=ec_cnv:to_list(Dir),
                     deps=Deps}}.

update_opts(AppInfo, Opts, Config) ->
    LockDeps = case resource_type(AppInfo) of
                   pkg ->
                       Deps = deps(AppInfo),
                       [{{locks, default}, Deps}, {{deps, default}, Deps}];
                   _ ->
                       deps_from_config(dir(AppInfo), Config)
               end,

    Plugins = proplists:get_value(plugins, Config, []),
    Terms = LockDeps++[{{plugins, default}, Plugins} | Config],
    true = rebar_config:verify_config_format(Terms),
    LocalOpts = dict:from_list(Terms),

    NewOpts = rebar_opts:merge_opts(LocalOpts, Opts),

    AppInfo#app_info_t{opts=NewOpts
                      ,default=NewOpts}.

deps_from_config(Dir, Config) ->
    case rebar_config:consult_lock_file(filename:join(Dir, ?LOCK_FILE)) of
        [] ->
            [{{deps, default}, proplists:get_value(deps, Config, [])}];
        D ->
            %% We want the top level deps only from the lock file.
            %% This ensures deterministic overrides for configs.
            Deps = [X || X <- D, element(3, X) =:= 0],
            [{{locks, default}, D}, {{deps, default}, Deps}]
    end.

%% @doc discover a complete version of the app info with all fields set.
-spec discover(file:filename_all()) -> {ok, t()} | not_found.
discover(Dir) ->
    case rebar_app_discover:find_app(Dir, all) of
        {true, AppInfo} ->
            {ok, AppInfo};
        false ->
            not_found
    end.

-spec name(t()) -> binary().
name(#app_info_t{name=Name}) ->
    Name.

-spec name(t(), atom() | binary() | string()) -> t().
name(AppInfo=#app_info_t{}, AppName) ->
    AppInfo#app_info_t{name=ec_cnv:to_binary(AppName)}.

opts(#app_info_t{opts=Opts}) ->
    Opts.

opts(AppInfo, Opts) ->
    AppInfo#app_info_t{opts=Opts}.

default(#app_info_t{default=Default}) ->
    Default.

default(AppInfo, Default) ->
    AppInfo#app_info_t{default=Default}.

get(AppInfo, Key) ->
    {ok, Value} = dict:find(Key, AppInfo#app_info_t.opts),
    Value.

get(AppInfo, Key, Default) ->
    case dict:find(Key, AppInfo#app_info_t.opts) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.

-spec set(t(), any(), any()) -> t().
set(AppInfo=#app_info_t{opts=Opts}, Key, Value) ->
    AppInfo#app_info_t{opts = dict:store(Key, Value, Opts)}.

-spec app_file_src(t()) -> file:filename_all() | undefined.
app_file_src(#app_info_t{app_file_src=undefined, dir=Dir, name=Name}) ->
    AppFileSrc = filename:join([ec_cnv:to_list(Dir), "src", ec_cnv:to_list(Name)++".app.src"]),
    case filelib:is_file(AppFileSrc) of
        true ->
            AppFileSrc;
        false ->
            undefined
    end;
app_file_src(#app_info_t{app_file_src=AppFileSrc}) ->
    ec_cnv:to_list(AppFileSrc).

-spec app_file_src(t(), file:filename_all() | undefined) -> t().
app_file_src(AppInfo=#app_info_t{}, undefined) ->
    AppInfo#app_info_t{app_file_src=undefined};
app_file_src(AppInfo=#app_info_t{}, AppFileSrc) ->
    AppInfo#app_info_t{app_file_src=ec_cnv:to_list(AppFileSrc)}.

-spec app_file_src_script(t()) -> file:filename_all() | undefined.
app_file_src_script(#app_info_t{app_file_src_script=undefined, dir=Dir, name=Name}) ->
    AppFileSrcScript = filename:join([ec_cnv:to_list(Dir), "src", ec_cnv:to_list(Name)++".app.src.script"]),
    case filelib:is_file(AppFileSrcScript) of
        true ->
            AppFileSrcScript;
        false ->
            undefined
    end;
app_file_src_script(#app_info_t{app_file_src_script=AppFileSrcScript}) ->
    ec_cnv:to_list(AppFileSrcScript).

-spec app_file_src_script(t(), file:filename_all()) -> t().
app_file_src_script(AppInfo=#app_info_t{}, undefined) ->
    AppInfo#app_info_t{app_file_src_script=undefined};
app_file_src_script(AppInfo=#app_info_t{}, AppFileSrcScript) ->
    AppInfo#app_info_t{app_file_src_script=ec_cnv:to_list(AppFileSrcScript)}.

-spec app_file(t()) -> file:filename_all() | undefined.
app_file(#app_info_t{app_file=undefined, out_dir=Dir, name=Name}) ->
    AppFile = filename:join([ec_cnv:to_list(Dir), "ebin", ec_cnv:to_list(Name)++".app"]),
    case filelib:is_file(AppFile) of
        true ->
            AppFile;
        false ->
            undefined
    end;
app_file(#app_info_t{app_file=AppFile}) ->
    AppFile.

-spec app_file(t(), file:filename_all()) -> t().
app_file(AppInfo=#app_info_t{}, AppFile) ->
    AppInfo#app_info_t{app_file=AppFile}.

-spec app_details(t()) -> list().
app_details(AppInfo=#app_info_t{app_details=[]}) ->
    case app_file(AppInfo) of
        undefined ->
            rebar_file_utils:try_consult(app_file_src(AppInfo));
        AppFile ->
            try
                rebar_file_utils:try_consult(AppFile)
            catch
                throw:{error, {Module, Reason}} ->
                    ?DEBUG("Warning, falling back to .app.src because of: ~s",
                          [Module:format_error(Reason)]),
                    rebar_file_utils:try_consult(app_file_src(AppInfo))
            end
    end;
app_details(#app_info_t{app_details=AppDetails}) ->
    AppDetails.

-spec app_details(t(), list()) -> t().
app_details(AppInfo=#app_info_t{}, AppDetails) ->
    AppInfo#app_info_t{app_details=AppDetails}.

parent(#app_info_t{parent=Parent}) ->
    Parent.

-spec parent(t(), binary() | root) -> t().
parent(AppInfo=#app_info_t{}, Parent) ->
    AppInfo#app_info_t{parent=Parent}.

-spec original_vsn(t()) -> string().
original_vsn(#app_info_t{original_vsn=Vsn}) ->
    Vsn.

-spec original_vsn(t(), string()) -> t().
original_vsn(AppInfo=#app_info_t{}, Vsn) ->
    AppInfo#app_info_t{original_vsn=Vsn}.

-spec applications(t()) -> list().
applications(#app_info_t{applications=Applications}) ->
    Applications.

-spec applications(t(), list()) -> t().
applications(AppInfo=#app_info_t{}, Applications) ->
    AppInfo#app_info_t{applications=Applications}.

-spec profiles(t()) -> list().
profiles(#app_info_t{profiles=Profiles}) ->
    Profiles.

-spec profiles(t(), list()) -> t().
profiles(AppInfo=#app_info_t{}, Profiles) ->
    AppInfo#app_info_t{profiles=Profiles}.

-spec deps(t()) -> list().
deps(#app_info_t{deps=Deps}) ->
    Deps.

-spec deps(t(), list()) -> t().
deps(AppInfo=#app_info_t{}, Deps) ->
    AppInfo#app_info_t{deps=Deps}.

dep_level(AppInfo=#app_info_t{}, Level) ->
    AppInfo#app_info_t{dep_level=Level}.

dep_level(#app_info_t{dep_level=Level}) ->
    Level.

-spec dir(t()) -> file:name().
dir(#app_info_t{dir=Dir}) ->
    Dir.

-spec dir(t(), file:name()) -> t().
dir(AppInfo=#app_info_t{out_dir=undefined}, Dir) ->
    AppInfo#app_info_t{dir=ec_cnv:to_list(Dir),
                       out_dir=ec_cnv:to_list(Dir)};
dir(AppInfo=#app_info_t{}, Dir) ->
    AppInfo#app_info_t{dir=ec_cnv:to_list(Dir)}.

-spec out_dir(t()) -> file:name().
out_dir(#app_info_t{out_dir=OutDir}) ->
    OutDir.

-spec out_dir(t(), file:name()) -> t().
out_dir(AppInfo=#app_info_t{}, OutDir) ->
    AppInfo#app_info_t{out_dir=ec_cnv:to_list(OutDir)}.

-spec ebin_dir(t()) -> file:name().
ebin_dir(#app_info_t{out_dir=OutDir}) ->
    ec_cnv:to_list(filename:join(OutDir, "ebin")).

-spec priv_dir(t()) -> file:name().
priv_dir(#app_info_t{out_dir=OutDir}) ->
    ec_cnv:to_list(filename:join(OutDir, "priv")).

-spec resource_type(t(), pkg | src) -> t().
resource_type(AppInfo=#app_info_t{}, Type) ->
    AppInfo#app_info_t{resource_type=Type}.

-spec resource_type(t()) -> pkg | src.
resource_type(#app_info_t{resource_type=ResourceType}) ->
    ResourceType.

-spec source(t(), string() | tuple() | checkout) -> t().
source(AppInfo=#app_info_t{}, Source) ->
    AppInfo#app_info_t{source=Source}.

-spec source(t()) -> string() | tuple().
source(#app_info_t{source=Source}) ->
    Source.

-spec is_lock(t(), boolean()) -> t().
is_lock(AppInfo=#app_info_t{}, IsLock) ->
    AppInfo#app_info_t{is_lock=IsLock}.

-spec is_lock(t()) -> boolean().
is_lock(#app_info_t{is_lock=IsLock}) ->
    IsLock.

-spec is_checkout(t(), boolean()) -> t().
is_checkout(AppInfo=#app_info_t{}, IsCheckout) ->
    AppInfo#app_info_t{is_checkout=IsCheckout}.

-spec is_checkout(t()) -> boolean().
is_checkout(#app_info_t{is_checkout=IsCheckout}) ->
    IsCheckout.

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

-spec valid(t(), boolean()) -> t().
valid(AppInfo=#app_info_t{}, Valid) ->
    AppInfo#app_info_t{valid=Valid}.

verify_otp_vsn(AppInfo) ->
    rebar_utils:check_min_otp_version(rebar_app_info:get(AppInfo, minimum_otp_vsn, undefined)),
    rebar_utils:check_blacklisted_otp_versions(rebar_app_info:get(AppInfo, blacklisted_otp_vsns, [])).

-spec has_all_artifacts(#app_info_t{}) -> true | {false, file:filename()}.
has_all_artifacts(AppInfo) ->
    Artifacts = rebar_app_info:get(AppInfo, artifacts, []),
    OutDir = out_dir(AppInfo),
    Context = [{base_dir, rebar_app_info:get(AppInfo, base_dir, ?DEFAULT_BASE_DIR)}
              ,{profile_dir, rebar_dir:profile_dir(opts(AppInfo), profiles(AppInfo))}
              ,{out_dir, OutDir}],
    all(OutDir, Context, Artifacts).

all(_, _, []) ->
    true;
all(Dir, Context, [File|Artifacts]) ->
    FilePath = filename:join(Dir, rebar_templater:render(File, Context)),
    case filelib:is_regular(FilePath) of
        false ->
            ?DEBUG("Missing artifact ~s", [FilePath]),
            {false, File};
        true ->
            all(Dir, Context, Artifacts)
    end.

%%%%%

apply_overrides(Overrides, AppInfo) ->
    Name = binary_to_atom(rebar_app_info:name(AppInfo), utf8),
    Opts = rebar_opts:apply_overrides(opts(AppInfo), Name, Overrides),
    AppInfo#app_info_t{default=Opts, opts=Opts}.

add_to_profile(AppInfo, Profile, KVs) when is_atom(Profile), is_list(KVs) ->
    Opts = rebar_opts:add_to_profile(opts(AppInfo), Profile, KVs),
    AppInfo#app_info_t{opts=Opts}.

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

deduplicate(Profiles) ->
    do_deduplicate(lists:reverse(Profiles), []).

do_deduplicate([], Acc) ->
    Acc;
do_deduplicate([Head | Rest], Acc) ->
    case lists:member(Head, Acc) of
        true -> do_deduplicate(Rest, Acc);
        false -> do_deduplicate(Rest, [Head | Acc])
    end.
