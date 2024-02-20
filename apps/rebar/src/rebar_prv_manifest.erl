%% ===================================================================
%% Manifest Provider
%% ===================================================================

-module(rebar_prv_manifest).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include_lib("providers/include/providers.hrl").

-define(PROVIDER, manifest).
-define(NAMESPACE, experimental).
-define(DEFAULT_FORMAT, erlang).

-type extension() :: string().
-type app_context() :: #{name := binary(),
                         src_dirs := [file:filename()],
                         include_dirs := [file:filename()],
                         src_ext := extension(),
                         out_mappings := [#{extension := extension(),
                                            path := file:filename()}],
                         dependencies_opts => any()}.
-type manifest() :: #{ apps := [app_context()],
                       deps := [app_context()],
                       otp_lib_dir := file:filename(),
                       source_root := file:filename()}.

-type format() :: erlang | eetf.

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State,
                                      providers:create([{name, ?PROVIDER},
                                                        {namespace, ?NAMESPACE},
                                                        {module, ?MODULE},
                                                        {bare, true},
                                                        {deps, [{default, install_deps}]},
                                                        {example, "rebar3 manifest"},
                                                        {short_desc, short_desc()},
                                                        {desc, desc()},
                                                        {opts, options()}
                                                       ])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    Format = proplists:get_value(format, Opts),
    To = proplists:get_value(to, Opts),

    Manifest = get_manifest(State),
    case format(Manifest, Format) of
        {ok, Formatted} ->
            case output_manifest(Formatted, Format, To) of
                ok ->
                    {ok, State};
                {error, Error} ->
                    ?PRV_ERROR({output_error, To, Error})
            end;
        {error, Error} ->
            ?PRV_ERROR(Error)
    end.

-spec format_error(any()) -> iolist().
format_error({format_not_supported, Format}) ->
    io_lib:format("Format '~p' is not supported. Try 'erlang' or 'eetf'.", [Format]);
format_error({output_error, To, Error}) ->
    io_lib:format("Could not output manifest to ~p (~p)", [To, Error]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal Helpers
%% ===================================================================
-spec short_desc() -> string().
short_desc() ->
    "Produce a project manifest".

-spec desc() -> string().
desc() ->
    short_desc().

-spec options() -> [tuple()].
options() ->
    [{format, $f, "format", {atom, ?DEFAULT_FORMAT},
      "Format for the manifest. "
      "Supported formats are: erlang, eetf (Erlang External Binary Format)"},
     {to, $t, "to", {string, undefined},
      "If specified, write the manifest to file"}].

-spec get_manifest(rebar_state:t()) -> manifest().
get_manifest(State) ->
    ProjectApps = rebar_state:project_apps(State),
    DepApps = rebar_state:all_deps(State),
    #{apps => [adapt_context(App) || App <- ProjectApps, is_supported(App)],
      deps => [adapt_context(App) || App <- DepApps, is_supported(App)],
      otp_lib_dir => code:lib_dir(),
      source_root => rebar_state:dir(State)}.

-spec is_supported(rebar_app_info:t()) -> boolean().
is_supported(App) ->
    Type = rebar_app_info:project_type(App),
    Type =:= rebar3 orelse Type =:= undefined.

-spec adapt_context(rebar_app_info:t()) -> app_context().
adapt_context(App) ->
    Context0 = rebar_compiler_erl:context(App),
    Context1 = maps:put(name, rebar_app_info:name(App), Context0),
    OutMappings = [#{extension => Extension, path => Path} ||
                      {Extension, Path} <- maps:get(out_mappings, Context1)],
    maps:put(out_mappings, OutMappings, Context1).

-spec output_manifest(binary(), format(), string() | undefined) -> ok | {error, term()}.
output_manifest(Manifest, Format, undefined) ->
    rebar_log:log(info, "Writing manifest to stdout:~n", []),
    case Format of
      erlang ->
        io:fwrite("~ts~n", [Manifest]);
      eetf ->
        io:fwrite("~s~n", [Manifest])
    end;
output_manifest(Manifest, _Format, File) ->
    rebar_log:log(info, "Build info written to: ~ts~n", [File]),
    file:write_file(File, Manifest).

-spec format(manifest(), format()) -> {ok, binary()} | {error, {format_not_supported, term()}}.
format(Manifest, eetf) ->
    {ok, term_to_binary(Manifest)};
format(Manifest, erlang) ->
    {ok, unicode:characters_to_binary(io_lib:format("~p.", [Manifest]))};
format(_Manifest, Format) ->
    {error, {format_not_supported, Format}}.
