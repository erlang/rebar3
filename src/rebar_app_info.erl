-module(rebar_app_info).

-export([new/0,
         new/3,
         name/1,
         name/2,
         config/1,
         config/2,
         app_file_src/1,
         app_file_src/2,
         app_file/1,
         app_file/2,
         original_vsn/1,
         dir/1,
         dir/2]).

-export_type([t/0]).

-record(app_info_t, {name :: atom(),
                     app_file_src :: file:name() | undefined,
                     app_file :: file:name(),
                     config :: rebar_config:config() | undefined,
                     original_vsn :: string(),
                     dir :: file:name(),
                     source :: string() | undefined}).

%%============================================================================
%% types
%%============================================================================
-opaque t() :: record(app_info_t).

%%============================================================================
%% API
%% ============================================================================
%% @doc Build a new, empty, app info value. This is not of a lot of use and you
%% probably wont be doing this much.
-spec new() -> {ok, t()}.
new() ->
    {ok, #app_info_t{}}.

%% @doc build a complete version of the app info with all fields set.
-spec new(atom(), string(), file:name()) ->
                 {ok, t()}.
new(AppName, Vsn, Dir)
  when erlang:is_atom(AppName) ->
    {ok, #app_info_t{name=AppName,
                     original_vsn=Vsn,
                     dir=Dir}}.

-spec name(t()) -> atom().
name(#app_info_t{name=Name}) ->
    Name.

-spec name(t(), atom()) -> t().
name(AppInfo=#app_info_t{}, AppName)
  when erlang:is_atom(AppName) ->
    AppInfo#app_info_t{name=AppName}.

-spec config(t()) -> rebar_config:confg().
config(#app_info_t{config=Config}) ->
    Config.

-spec config(t(), rebar_config:confg()) -> t().
config(AppInfo=#app_info_t{}, Config) ->
    AppInfo#app_info_t{config=Config}.

-spec app_file_src(t()) -> file:name().
app_file_src(#app_info_t{app_file_src=AppFileSrc}) ->
    AppFileSrc.

-spec app_file_src(t(), file:name()) -> t().
app_file_src(AppInfo=#app_info_t{}, AppFileSrc) ->
    AppInfo#app_info_t{app_file_src=AppFileSrc}.

-spec app_file(t()) -> file:name().
app_file(#app_info_t{app_file=AppFile}) ->
    AppFile.

-spec app_file(t(), file:name()) -> t().
app_file(AppInfo=#app_info_t{}, AppFile) ->
    AppInfo#app_info_t{app_file=AppFile}.

-spec original_vsn(t()) -> string().
original_vsn(#app_info_t{original_vsn=Vsn}) ->
    Vsn.

-spec dir(t()) -> file:name().
dir(#app_info_t{dir=Dir}) ->
    Dir.
-spec dir(t(), file:name()) -> t().
dir(AppInfo=#app_info_t{}, Dir) ->
    AppInfo#app_info_t{dir=Dir}.
