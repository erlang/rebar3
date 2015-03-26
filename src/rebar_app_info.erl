-module(rebar_app_info).

-export([new/1,
         new/2,
         new/3,
         new/4,
         discover/1,
         name/1,
         name/2,
         config/1,
         config/2,
         app_file_src/1,
         app_file_src/2,
         app_file/1,
         app_file/2,
         app_details/1,
         app_details/2,
         original_vsn/1,
         original_vsn/2,
         ebin_dir/1,
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
         source/1,
         source/2,
         state/1,
         state/2,
         is_checkout/1,
         is_checkout/2,
         valid/1,
         valid/2]).

-export_type([t/0]).

-record(app_info_t, {name :: binary(),
                     app_file_src :: file:filename_all() | undefined,
                     app_file :: file:filename_all() | undefined,
                     config :: rebar_state:t() | undefined,
                     original_vsn :: binary() | string() | undefined,
                     app_details=[] :: list(),
                     applications=[] :: list(),
                     deps=[] :: list(),
                     profiles=[default] :: [atom()],
                     dep_level=0 :: integer(),
                     dir :: file:name(),
                     out_dir :: file:name(),
                     source :: string() | tuple() | undefined,
                     state :: rebar_state:t() | undefined,
                     is_checkout=false :: boolean(),
                     valid :: boolean()}).

%%============================================================================
%% types
%%============================================================================
-type t() :: record(app_info_t).

%%============================================================================
%% API
%% ============================================================================
%% @doc Build a new, empty, app info value. This is not of a lot of use and you
%% probably wont be doing this much.
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

-spec config(t()) -> rebar_state:t().
config(#app_info_t{config=Config}) ->
    Config.

-spec config(t(), rebar_state:t()) -> t().
config(AppInfo=#app_info_t{}, Config) ->
    AppInfo#app_info_t{config=Config}.

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

-spec app_file_src(t(), file:filename_all()) -> t().
app_file_src(AppInfo=#app_info_t{}, AppFileSrc) ->
    AppInfo#app_info_t{app_file_src=ec_cnv:to_list(AppFileSrc)}.

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
app_details(#app_info_t{app_details=AppDetails}) ->
    AppDetails.

-spec app_details(t(), list()) -> t().
app_details(AppInfo=#app_info_t{}, AppDetails) ->
    AppInfo#app_info_t{app_details=AppDetails}.

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

-spec source(t(), string() | tuple()) -> t().
source(AppInfo=#app_info_t{}, Source) ->
    AppInfo#app_info_t{source=Source}.

-spec source(t()) -> string() | tuple().
source(#app_info_t{source=Source}) ->
    Source.

-spec state(t(), rebar_state:t() | undefined) -> t().
state(AppInfo=#app_info_t{}, State) ->
    AppInfo#app_info_t{state=State}.

-spec state(t()) -> rebar_state:t() | undefined.
state(#app_info_t{state=State}) ->
    State.

-spec is_checkout(t(), boolean()) -> t().
is_checkout(AppInfo=#app_info_t{}, IsCheckout) ->
    AppInfo#app_info_t{is_checkout=IsCheckout}.

-spec is_checkout(t()) -> boolean().
is_checkout(#app_info_t{is_checkout=IsCheckout}) ->
    IsCheckout.

-spec valid(t()) -> boolean().
valid(AppInfo=#app_info_t{valid=undefined}) ->
    case rebar_app_utils:validate_application_info(AppInfo) of
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
