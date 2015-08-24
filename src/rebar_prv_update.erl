%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_update).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-export([hex_to_index/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, update).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 update"},
                                                               {short_desc, "Update package index."},
                                                               {desc, "Update package index."},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        RegistryDir = rebar_packages:registry_dir(State),
        filelib:ensure_dir(filename:join(RegistryDir, "dummy")),
        HexFile = filename:join(RegistryDir, "registry"),
        ?INFO("Updating package registry...", []),
        TmpDir = ec_file:insecure_mkdtemp(),
        TmpFile = filename:join(TmpDir, "packages.gz"),

        Url = rebar_state:get(State, rebar_packages_cdn, ?DEFAULT_HEX_REGISTRY),
        {ok, _RequestId} = httpc:request(get, {Url, []},
                                         [], [{stream, TmpFile}, {sync, true}],
                                         rebar),
        {ok, Data} = file:read_file(TmpFile),
        Unzipped = zlib:gunzip(Data),
        ok = file:write_file(HexFile, Unzipped),
        ?INFO("Writing registry to ~s", [rebar_file_utils:replace_home_dir(HexFile)]),
        hex_to_index(State),
        ok
    catch
        _E:C ->
            ?DEBUG("Error creating package index: ~p ~p", [C, erlang:get_stacktrace()]),
            throw(?PRV_ERROR(package_index_write))
    end,

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(package_index_write) ->
    "Failed to write package index.".

is_supported(<<"make">>) -> true;
is_supported(<<"rebar">>) -> true;
is_supported(<<"rebar3">>) -> true;
is_supported(_) -> false.

hex_to_index(State) ->
    RegistryDir = rebar_packages:registry_dir(State),
    HexFile = filename:join(RegistryDir, "registry"),
    try ets:file2tab(HexFile) of
        {ok, Registry} ->
            try
                PackageIndex = filename:join(RegistryDir, "packages.idx"),
                ?INFO("Generating package index...", []),
                (catch ets:delete(?PACKAGE_TABLE)),
                ets:new(?PACKAGE_TABLE, [named_table, public]),
                ets:foldl(fun({{Pkg, PkgVsn}, [Deps, Checksum, BuildTools | _]}, _) when is_list(BuildTools) ->
                                  case lists:any(fun is_supported/1, BuildTools) of
                                      true ->
                                          DepsList = update_deps_list(Deps, Registry, State),
                                          ets:insert(?PACKAGE_TABLE, {{Pkg, PkgVsn}, DepsList, Checksum});
                                      false ->
                                          true
                                  end;
                             ({Pkg, [Vsns]}, _) when is_binary(Pkg) ->
                                  ets:insert(?PACKAGE_TABLE, {Pkg, Vsns});
                             (_, _) ->
                                  true
                          end, true, Registry),

                ets:insert(?PACKAGE_TABLE, {package_index_version, ?PACKAGE_INDEX_VERSION}),
                ?INFO("Writing index to ~s", [rebar_file_utils:replace_home_dir(PackageIndex)]),
                ets:tab2file(?PACKAGE_TABLE, PackageIndex),
                true
            after
                catch ets:delete(Registry)
            end;
        {error, Reason} ->
            ?DEBUG("Error loading package registry: ~p", [Reason]),
            false
    catch
        _:_ ->
            fail
    end.

update_deps_list(Deps, HexRegistry, State) ->
    lists:foldl(fun([Dep, DepVsn, false, _AppName | _], DepsListAcc) ->
                        case DepVsn of
                            <<"~> ", Vsn/binary>> ->
                                case rebar_packages:find_highest_matching(Dep, Vsn, HexRegistry, State) of
                                    {ok, HighestDepVsn} ->
                                        [{Dep, HighestDepVsn} | DepsListAcc];
                                    none ->
                                        ?WARN("Missing registry entry for package ~s. Try to fix with `rebar3 update`", [Dep]),
                                        DepsListAcc
                                end;
                            Vsn ->
                                [{Dep, Vsn} | DepsListAcc]
                        end;
                   ([_Dep, _DepVsn, true, _AppName | _], DepsListAcc) ->
                        DepsListAcc
                end, [], Deps).
