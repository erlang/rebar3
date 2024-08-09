%% ===================================================================
%% Manifest Provider
%% ===================================================================

-module(rebar_prv_manifest).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1,
         is_json_available/0]).

-include_lib("providers/include/providers.hrl").

-define(PROVIDER, manifest).
-define(NAMESPACE, experimental).
-define(DEFAULT_FORMAT, json).

-type app_context() :: #{name := binary(),
                         dir => file:filename_all(),
                         ebin => file:filename_all(),
                         src_dirs := [file:filename_all()],
                         extra_src_dirs := [file:filename_all()],
                         include_dirs := [file:filename_all()],
                         macros => [macro()],
                         parse_transforms => [any()]}.
-type macro() :: #{key := atom(), value => any()}.
-type manifest() :: #{ apps := [app_context()],
                       deps := [app_context()],
                       otp_lib_dir := file:filename_all(),
                       source_root := file:filename_all()}.

-type format() :: erlang | eetf | json.

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
format_error(no_json_module) ->
    io_lib:format("The 'json' module is not available. Either upgrade to OTP 27 or newer, or select a different output format.", []);
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
      "Supported formats are: erlang, eetf (Erlang External Binary Format), json"},
     {to, $t, "to", {string, undefined},
      "If specified, write the manifest to file"}].

-spec get_manifest(rebar_state:t()) -> manifest().
get_manifest(State) ->
    ProjectApps = rebar_state:project_apps(State),
    DepApps = rebar_state:all_deps(State),
    #{apps => [adapt_context(App) || App <- ProjectApps, is_supported(App)],
      deps => [adapt_context(App) || App <- DepApps, is_supported(App)],
      otp_lib_dir => to_binary(code:lib_dir()),
      source_root => to_binary(rebar_state:dir(State))}.

-spec is_supported(rebar_app_info:t()) -> boolean().
is_supported(App) ->
    Type = rebar_app_info:project_type(App),
    Type =:= rebar3 orelse Type =:= undefined.

-spec adapt_context(rebar_app_info:t()) -> app_context().
adapt_context(App) ->
    Context = rebar_compiler_erl:context(App),
    #{src_dirs := SrcDirs,
      include_dirs := IncludeDirs,
      dependencies_opts := DependenciesOpts} = Context,
    Name = rebar_app_info:name(App),
    Dir = rebar_app_info:dir(App),
    EbinDir = rebar_app_info:ebin_dir(App),
    RebarOpts = rebar_app_info:opts(App),
    ExtraSrcDirs = rebar_dir:extra_src_dirs(RebarOpts),
    Macros = proplists:get_value(macros, DependenciesOpts),
    ParseTransforms = proplists:get_value(parse_transforms, DependenciesOpts),
    #{name => Name,
      dir => to_binary(Dir),
      ebin => to_binary(EbinDir),
      src_dirs => [to_binary(D) || D <- SrcDirs],
      extra_src_dirs => [to_binary(D) || D <- ExtraSrcDirs],
      include_dirs => [to_binary(D) || D <- IncludeDirs],
      macros => [to_macro(M) || M <- Macros],
      parse_transforms => ParseTransforms}.

-spec output_manifest(binary(), format(), string() | undefined) -> ok | {error, term()}.
output_manifest(Manifest, Format, undefined) ->
    rebar_log:log(info, "Writing manifest to stdout:~n", []),
    case Format of
        eetf ->
            io:fwrite("~s~n", [Manifest]);
        _ ->
            io:fwrite("~ts~n", [Manifest])
    end;
output_manifest(Manifest, _Format, File) ->
    rebar_log:log(info, "Build info written to: ~ts~n", [File]),
    file:write_file(File, Manifest).

-spec format(manifest(), format()) -> {ok, binary()} | {error, {format_not_supported, term()} | no_json_module}.
format(Manifest, eetf) ->
    {ok, term_to_binary(Manifest)};
format(Manifest, erlang) ->
    {ok, unicode:characters_to_binary(io_lib:format("~p.", [Manifest]))};
format(Manifest, json) ->
    case is_json_available() of
        true ->
            Encoded = erlang:apply(json, encode, [Manifest]),
            {ok, Encoded};
        false ->
            {error, no_json_module}
    end;
format(_Manifest, Format) ->
    {error, {format_not_supported, Format}}.

-spec to_binary(file:filename()) -> file:filename_all().
to_binary(Path) ->
    unicode:characters_to_binary(Path).

-spec to_macro(atom() | {atom() | any()}) -> macro().
to_macro({Key, Value}) when is_atom(Key) ->
    #{key => Key, value => Value};
to_macro(Key) when is_atom(Key) ->
    #{key => Key, value => true}.

-spec is_json_available() -> boolean().
is_json_available() ->
    % Requires OTP 27
    case code:ensure_loaded(json) of
        {module, _} ->
            true;
        {error, _} ->
            false
    end.
