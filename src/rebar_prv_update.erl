%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_update).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-export([hex_to_index/1]).

-ifdef(TEST).
-export([cmp_/6, cmpl_/6, valid_vsn/1]).
-endif.

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
        case rebar_packages:registry_dir(State) of
            {ok, RegistryDir} ->
                filelib:ensure_dir(filename:join(RegistryDir, "dummy")),
                HexFile = filename:join(RegistryDir, "registry"),
                ?INFO("Updating package registry...", []),
                TmpDir = ec_file:insecure_mkdtemp(),
                TmpFile = filename:join(TmpDir, "packages.gz"),

                CDN = rebar_state:get(State, rebar_packages_cdn, ?DEFAULT_CDN),
                case rebar_utils:url_append_path(CDN, ?REMOTE_REGISTRY_FILE) of
                    {ok, Url} ->
                        ?DEBUG("Fetching registry from ~p", [Url]),
                        case httpc:request(get, {Url, [{"User-Agent", rebar_utils:user_agent()}]},
                                           [], [{stream, TmpFile}, {sync, true}],
                                           rebar) of
                            {ok, saved_to_file} ->
                                {ok, Data} = file:read_file(TmpFile),
                                Unzipped = zlib:gunzip(Data),
                                ok = file:write_file(HexFile, Unzipped),
                                ?INFO("Writing registry to ~s", [HexFile]),
                                hex_to_index(State),
                                {ok, State};
                            _ ->
                                ?PRV_ERROR(package_index_download)
                        end;
                    _ ->
                        ?PRV_ERROR({package_parse_cdn, CDN})
                end;
            {uri_parse_error, CDN} ->
                ?PRV_ERROR({package_parse_cdn, CDN})
        end
    catch
        _E:C ->
            ?DEBUG("Error creating package index: ~p ~p", [C, erlang:get_stacktrace()]),
            throw(?PRV_ERROR(package_index_write))
    end.

-spec format_error(any()) -> iolist().
format_error({package_parse_cdn, Uri}) ->
    io_lib:format("Failed to parse CDN url: ~p", [Uri]);
format_error(package_index_download) ->
    "Failed to download package index.";
format_error(package_index_write) ->
    "Failed to write package index.".

is_supported(<<"make">>) -> true;
is_supported(<<"rebar">>) -> true;
is_supported(<<"rebar3">>) -> true;
is_supported(_) -> false.

hex_to_index(State) ->
    {ok, RegistryDir} = rebar_packages:registry_dir(State),
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
                                          DepsList = update_deps_list(Pkg, PkgVsn, Deps, Registry, State),
                                          ets:insert(?PACKAGE_TABLE, {{Pkg, PkgVsn}, DepsList, Checksum});
                                      false ->
                                          true
                                  end;
                             (_, _) ->
                                  true
                          end, true, Registry),

                ets:foldl(fun({Pkg, [[]]}, _) when is_binary(Pkg) ->
                                  true;
                             ({Pkg, [Vsns=[_Vsn | _Rest]]}, _) when is_binary(Pkg) ->
                                  %% Verify the package is of the right build tool by checking if the first
                                  %% version exists in the table from the foldl above
                                  case [V || V <- Vsns, ets:member(?PACKAGE_TABLE, {Pkg, V})] of
                                      [] ->
                                          true;
                                      Vsns1 ->
                                          ets:insert(?PACKAGE_TABLE, {Pkg, Vsns1})
                                  end;
                             (_, _) ->
                                  true
                          end, true, Registry),
                ets:insert(?PACKAGE_TABLE, {package_index_version, ?PACKAGE_INDEX_VERSION}),
                ?INFO("Writing index to ~s", [PackageIndex]),
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

update_deps_list(Pkg, PkgVsn, Deps, HexRegistry, State) ->
    lists:foldl(fun([Dep, DepVsn, false, _AppName | _], DepsListAcc) ->
                        Dep1 = {Pkg, PkgVsn, Dep},
                        case {valid_vsn(DepVsn), DepVsn} of
                            %% Those are all not perfectly implemented!
                            %% and doubled since spaces seem not to be
                            %% enforced
                            {false, Vsn} ->
                                ?WARN("[~s:~s], Bad dependency version for ~s: ~s.",
                                      [Pkg, PkgVsn, Dep, Vsn]),
                                DepsListAcc;
                            {_, <<"~>", Vsn/binary>>} ->
                                highest_matching(Dep1, rm_ws(Vsn), HexRegistry,
                                                 State, DepsListAcc);
                            {_, <<">=", Vsn/binary>>} ->
                                cmp(Dep1, rm_ws(Vsn), HexRegistry, State,
                                    DepsListAcc, fun ec_semver:gte/2);
                            {_, <<">", Vsn/binary>>} ->
                                cmp(Dep1, rm_ws(Vsn), HexRegistry, State,
                                    DepsListAcc, fun ec_semver:gt/2);
                            {_, <<"<=", Vsn/binary>>} ->
                                cmpl(Dep1, rm_ws(Vsn), HexRegistry, State,
                                     DepsListAcc, fun ec_semver:lte/2);
                            {_, <<"<", Vsn/binary>>} ->
                                cmpl(Dep1, rm_ws(Vsn), HexRegistry, State,
                                     DepsListAcc, fun ec_semver:lt/2);
                            {_, <<"==", Vsn/binary>>} ->
                                [{Dep, Vsn} | DepsListAcc];
                            {_, Vsn} ->
                                [{Dep, Vsn} | DepsListAcc]
                        end;
                   ([_Dep, _DepVsn, true, _AppName | _], DepsListAcc) ->
                        DepsListAcc
                end, [], Deps).

rm_ws(<<" ", R/binary>>) ->
    rm_ws(R);
rm_ws(R) ->
    R.

valid_vsn(Vsn) ->
    %% Regepx from https://github.com/sindresorhus/semver-regex/blob/master/index.js
    SemVerRegExp = "v?(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)(\\.(0|[1-9][0-9]*))?"
        "(-[0-9a-z-]+(\\.[0-9a-z-]+)*)?(\\+[0-9a-z-]+(\\.[0-9a-z-]+)*)?",
    SupportedVersions = "^(>=?|<=?|~>|==)?\\s*" ++ SemVerRegExp ++ "$",
    re:run(Vsn, SupportedVersions) =/= nomatch.

highest_matching({Pkg, PkgVsn, Dep}, Vsn, HexRegistry, State, DepsListAcc) ->
    case rebar_packages:find_highest_matching(Pkg, PkgVsn, Dep, Vsn, HexRegistry, State) of
        {ok, HighestDepVsn} ->
            [{Dep, HighestDepVsn} | DepsListAcc];
        none ->
            ?WARN("[~s:~s] Missing registry entry for package ~s. Try to fix with `rebar3 update`",
                  [Pkg, PkgVsn, Dep]),
            DepsListAcc
    end.

cmp({_Pkg, _PkgVsn, Dep} = Dep1, Vsn, HexRegistry, State, DepsListAcc, CmpFun) ->
    {ok, Vsns}  = rebar_packages:find_all(Dep, HexRegistry, State),
    cmp_(undefined, Vsn, Vsns, DepsListAcc, Dep1, CmpFun).


cmp_(undefined, _MinVsn, [], DepsListAcc, {Pkg, PkgVsn, Dep}, _CmpFun) ->
    ?WARN("[~s:~s] Missing registry entry for package ~s. Try to fix with `rebar3 update`",
          [Pkg, PkgVsn, Dep]),
    DepsListAcc;
cmp_(HighestDepVsn, _MinVsn, [], DepsListAcc, {_Pkg, _PkgVsn, Dep}, _CmpFun) ->
    [{Dep, HighestDepVsn} | DepsListAcc];

cmp_(BestMatch, MinVsn, [Vsn | R], DepsListAcc, Dep, CmpFun) ->
    case CmpFun(Vsn, MinVsn) of
        true ->
            cmp_(Vsn, Vsn, R, DepsListAcc, Dep, CmpFun);
        false  ->
            cmp_(BestMatch, MinVsn, R, DepsListAcc, Dep, CmpFun)
    end.

%% We need to treat this differently since we want a version that is LOWER but
%% the higest possible one.
cmpl({_Pkg, _PkgVsn, Dep} = Dep1, Vsn, HexRegistry, State, DepsListAcc, CmpFun) ->
    {ok, Vsns}  = rebar_packages:find_all(Dep, HexRegistry, State),
    cmpl_(undefined, Vsn, Vsns, DepsListAcc, Dep1, CmpFun).

cmpl_(undefined, _MaxVsn, [], DepsListAcc, {Pkg, PkgVsn, Dep}, _CmpFun) ->
    ?WARN("[~s:~s] Missing registry entry for package ~s. Try to fix with `rebar3 update`",
          [Pkg, PkgVsn, Dep]),
    DepsListAcc;

cmpl_(HighestDepVsn, _MaxVsn, [], DepsListAcc, {_Pkg, _PkgVsn, Dep}, _CmpFun) ->
    [{Dep, HighestDepVsn} | DepsListAcc];

cmpl_(undefined, MaxVsn, [Vsn | R], DepsListAcc, Dep, CmpFun) ->
    case CmpFun(Vsn, MaxVsn) of
        true ->
            cmpl_(Vsn, MaxVsn, R, DepsListAcc, Dep, CmpFun);
        false  ->
            cmpl_(undefined, MaxVsn, R, DepsListAcc, Dep, CmpFun)
    end;

cmpl_(BestMatch, MaxVsn, [Vsn | R], DepsListAcc, Dep, CmpFun) ->
    case CmpFun(Vsn, MaxVsn) of
        true ->
            case ec_semver:gte(Vsn, BestMatch) of
                true ->
                    cmpl_(Vsn, MaxVsn, R, DepsListAcc, Dep, CmpFun);
                false ->
                    cmpl_(BestMatch, MaxVsn, R, DepsListAcc, Dep, CmpFun)
            end;
        false  ->
            cmpl_(BestMatch, MaxVsn, R, DepsListAcc, Dep, CmpFun)
    end.
