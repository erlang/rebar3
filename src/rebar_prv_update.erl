%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_update).

-behaviour(provider).

-export([init/1,
         do/1,
         do/2,
         format_error/1]).

%-export([hex_to_index/1]).

%-ifdef(TEST).
-export([cmp_/6, cmpl_/6, valid_vsn/1]).
%-endif.

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
    update_registries(rebar_state:repos(State), State).

-spec update_registries([string()], rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
update_registries([], State) ->
    {ok, State};
update_registries([Registry | Rest], State) ->
    case do(Registry, State) of
        {ok, State1} ->
            update_registries(Rest, State1);
        Error ->
            Error
    end.

do(Registry, State) ->
    try
        case rebar_packages:registry_dir(Registry, State) of
            {ok, RegistryDir} ->
                filelib:ensure_dir(filename:join(RegistryDir, "dummy")),
                HexFile = filename:join(RegistryDir, "registry"),
                ?INFO("Updating package registry...", []),
                TmpDir = ec_file:insecure_mkdtemp(),
                TmpFile = filename:join(TmpDir, "packages.gz"),
                case rebar_utils:url_append_path(Registry, ?REMOTE_REGISTRY_FILE) of
                    {ok, Url} ->
                        ?DEBUG("Fetching registry from ~p", [Url]),
                        case httpc:request(get, {Url, [{"User-Agent", rebar_utils:user_agent()}]},
                                           [], [{stream, TmpFile}, {sync, true}],
                                           rebar) of
                            {ok, saved_to_file} ->
                                {ok, Data} = file:read_file(TmpFile),
                                case rebar_packages:maybe_verify_registry(Registry,
                                                                          Data,
                                                                          State) of
                                    true ->
                                        Unzipped = zlib:gunzip(Data),
                                        ok = file:write_file(HexFile, Unzipped),
                                        ?INFO("Writing registry to ~s", [HexFile]),
                                        {ok, State};
                                    Error ->
                                        ?PRV_ERROR(Error)
                                end;
                            _ ->
                                ?PRV_ERROR({package_index_download, Registry})
                        end;
                    _ ->
                        ?PRV_ERROR({package_parse_cdn, Registry})
                end;
            {uri_parse_error, Registry} ->
                ?PRV_ERROR({package_parse_cdn, Registry})
        end
    catch
        _E:C ->
            ?DEBUG("Error creating package index: ~p ~p", [C, erlang:get_stacktrace()]),
            ?ERROR("Failed to write package index for registry ~s", [Registry])
    end.

-spec format_error(any()) -> iolist().
format_error({package_parse_cdn, Uri}) ->
    io_lib:format("Failed to parse CDN url: ~p", [Uri]);
format_error({package_index_download, Registry}) ->
    io_lib:format("Failed to download package index for registry ~s", [Registry]);
format_error({package_index_write, Registry}) ->
    io_lib:format("Failed to write package index ~s", [Registry]).

valid_vsn(Vsn) ->
    %% Regepx from https://github.com/sindresorhus/semver-regex/blob/master/index.js
    SemVerRegExp = "v?(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)(\\.(0|[1-9][0-9]*))?"
        "(-[0-9a-z-]+(\\.[0-9a-z-]+)*)?(\\+[0-9a-z-]+(\\.[0-9a-z-]+)*)?",
    SupportedVersions = "^(>=?|<=?|~>|==)?\\s*" ++ SemVerRegExp ++ "$",
    re:run(Vsn, SupportedVersions) =/= nomatch.

cmp_(undefined, _MinVsn, [], DepsListAcc, {Pkg, PkgVsn, Dep, _App}, _CmpFun) ->
    ?WARN("[~s:~s] Missing registry entry for package ~s. Try to fix with `rebar3 update`",
          [Pkg, PkgVsn, Dep]),
    DepsListAcc;
cmp_(HighestDepVsn, _MinVsn, [], DepsListAcc, {_Pkg, _PkgVsn, Dep, App}, _CmpFun) ->
    [{App, {pkg, Dep, HighestDepVsn, undefined}} | DepsListAcc];

cmp_(BestMatch, MinVsn, [Vsn | R], DepsListAcc, Dep, CmpFun) ->
    case CmpFun(Vsn, MinVsn) of
        true ->
            cmp_(Vsn, Vsn, R, DepsListAcc, Dep, CmpFun);
        false  ->
            cmp_(BestMatch, MinVsn, R, DepsListAcc, Dep, CmpFun)
    end.

cmpl_(undefined, _MaxVsn, [], DepsListAcc, {Pkg, PkgVsn, Dep, _App}, _CmpFun) ->
    ?WARN("[~s:~s] Missing registry entry for package ~s. Try to fix with `rebar3 update`",
          [Pkg, PkgVsn, Dep]),
    DepsListAcc;

cmpl_(HighestDepVsn, _MaxVsn, [], DepsListAcc, {_Pkg, _PkgVsn, Dep, App}, _CmpFun) ->
    [{App, {pkg, Dep, HighestDepVsn, undefined}} | DepsListAcc];

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
