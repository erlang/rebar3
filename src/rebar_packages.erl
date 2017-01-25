-module(rebar_packages).

-export([packages/2
        ,close_packages/1
        ,load_and_verify_version/2
        ,deps/3
        ,deps/4
        ,maybe_verify_registry/3
        ,registry_dir/2
        ,package_dir/2
        ,registry_checksum/2
        ,find_package_registry/3
        ,find_highest_matching/5
        ,find_highest_matching/3
        ,find_all/3
        ,verify_table/2
        ,format_error/1]).

-export_type([package/0]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-type pkg_name() :: binary() | atom().
-type vsn() :: binary().
-type package() :: pkg_name() | {pkg_name(), vsn()}.

-spec packages(binary(), rebar_state:t()) -> ets:tid().
packages(Registry, State) ->
    case load_and_verify_version(Registry, State) of
        {ok, T1} ->
            {ok, T1};
        _ ->
            ?DEBUG("Error loading package index.", []),
            handle_bad_index(Registry, State)
    end.

handle_bad_index(Registry, State) ->
    ?ERROR("Bad packages index. Trying to fix by updating the registry from ~s", [Registry]),
    case rebar_prv_update:do(Registry, State) of
        {ok, State1} ->
            case load_and_verify_version(Registry, State1) of
                {ok, T} ->
                    {ok, T};
                false ->
                    %% Still unable to load after an update, create an empty registry
                    ets:new(repo, [public])
            end;
        E ->
            throw(E)
    end.

close_packages(T) ->
    catch ets:delete(T).

load_and_verify_version(Registry, State) ->
    case ?MODULE:registry_dir(Registry, State) of
        {ok, RegistryDir} ->
            case ets:file2tab(filename:join(RegistryDir, ?INDEX_FILE)) of
                {ok, T} ->
                    {ok, T};
                E ->
                    E
            end;
        E ->
            E
    end.

deps(Name, Vsn, State) ->
    try
        find(rebar_state:repos(State),
            fun(Registry) -> deps(Name, Vsn, Registry, State) end)
    catch
        _:_ ->
            handle_missing_package({Name, Vsn}, State, fun(State1) -> deps_(Name, Vsn, State1) end)
    end.

deps_(Name, Vsn, State) ->
    find(rebar_state:repos(State),
         fun(Registry) -> deps(Name, Vsn, Registry, State) end).

deps(Name, Vsn, Registry, State) ->
    {ok, T} = ?MODULE:verify_table(Registry, State),
    case ets:lookup_element(T, {ec_cnv:to_binary(Name), ec_cnv:to_binary(Vsn)}, 2) of
        [Deps | _] ->
            {ok, Deps};
        E ->
            E
    end.

find_package_registry(Name, Vsn, State) ->
    find(rebar_state:repos(State),
         fun(Registry) -> find_package_registry_(Name, Vsn, Registry, State) end).

find_package_registry_(Name, Vsn, Registry, State) ->
    {ok, T} = ?MODULE:verify_table(Registry, State),
    case ets:member(T, {ec_cnv:to_binary(Name), ec_cnv:to_binary(Vsn)}) of
        true ->
            {ok, Registry};
        false ->
            not_found
    end.

find([], _) ->
    error;
find([X | R], Fun) ->
    try Fun(X) of
        {ok, Y, Z} ->
            {ok, Y, Z};
        {ok, Y} ->
            {ok, Y}
    catch
        _:_ ->
            find(R, Fun)
    end.

handle_missing_package(Dep, State, Fun) ->
    case Dep of
        {Name, Vsn} ->
            ?INFO("Package ~s-~s not found. Fetching registry updates and trying again...", [Name, Vsn]);
        _ ->
            ?INFO("Package ~p not found. Fetching registry updates and trying again...", [Dep])
    end,

    {ok, State1} = rebar_prv_update:do(State),
    try
        Fun(State1)
    catch
        _:_ ->
            %% Even after an update the package is still missing, time to error out
            throw(?PRV_ERROR({missing_package, Dep}))
    end.

signature(Registry) ->
    case rebar_utils:url_append_path(Registry, ?REMOTE_REGISTRY_FILE ++ ".signed") of
        {ok, Url} ->
            ?DEBUG("Fetching signature from ~p", [Url]),
            case httpc:request(get, {Url, [{"User-Agent", rebar_utils:user_agent()}]},
                               [], [{sync, true}, {body_format, binary}], rebar) of
                {ok, {{_, 200, _}, _, Signature}} ->
                    {ok, list_to_binary(decode_binary(Signature))};
                _ ->
                    signature_fetch_fail
            end;
        _ ->
            signature_fetch_fail
    end.

decode_binary(Binary) ->
    [erlang:binary_to_integer(<<C:16>>, 16) || <<C:16>> <= Binary].

-spec public_key(string(), rebar_state:t()) -> {ok, binary()} | {error, atom()}.
public_key(Registry, State) ->
    {ok, RegistryDir} = rebar_packages:registry_dir(Registry, State),
    File = filename:join(RegistryDir, "cert.pem"),
    case file:read_file(File) of
        {error, _} when Registry =:= ?DEFAULT_CDN ->
            %% If we are using the default registry we can fall back to
            %% the public key included within rebar3 itself.
            {ok, ?DEFAULT_REPO_PUBLIC_KEY};
        Result ->
            Result
    end.

maybe_verify_registry(Registry, Data, State) ->
    maybe_verify_registry(os:getenv(?HEX_UNSAFE_REGISTRY), Registry, Data, State).

maybe_verify_registry(false, Registry, Data, State) ->
    case signature(Registry) of
        {ok, Signature} ->
            case public_key(Registry, State) of
                {ok, PublicKey} ->
                    [RsaPublicKey] = public_key:pem_decode(PublicKey),
                    Key = public_key:pem_entry_decode(RsaPublicKey),
                    public_key:verify(Data, sha512, Signature, Key)
                        orelse {failed_verify_registry, Registry};
                {error, Reason} ->
                    ?DEBUG("Unable to read public key for registry ~p because ~s", [Registry, file:format_error(Reason)]),
                    {no_public_key, Reason, Registry}
            end;
        signature_fetch_fail ->
            {signature_fetch_fail, Registry}
    end;
maybe_verify_registry(_, _, _, _) ->
    true.

registry_dir(Registry, State) ->
    CacheDir = rebar_dir:global_cache_dir(rebar_state:opts(State)),
    case Registry of
        ?DEFAULT_CDN ->
            RegistryDir = filename:join([CacheDir, "hex", "default"]),
            ok = filelib:ensure_dir(filename:join(RegistryDir, "placeholder")),
            {ok, RegistryDir};
        CDN ->
            case rebar_utils:url_append_path(CDN, ?REMOTE_PACKAGE_DIR) of
                {ok, Parsed} ->
                    {ok, {_, _, Host, _, Path, _}} = http_uri:parse(Parsed),
                    CDNHostPath = lists:reverse(string:tokens(Host, ".")),
                    CDNPath = tl(filename:split(Path)),
                    RegistryDir = filename:join([CacheDir, "hex"] ++ CDNHostPath ++ CDNPath),
                    ok = filelib:ensure_dir(filename:join(RegistryDir, "placeholder")),
                    {ok, RegistryDir};
                _ ->
                    {uri_parse_error, CDN}
            end
    end.

package_dir(Registry, State) ->
    case registry_dir(Registry, State) of
        {ok, RegistryDir} ->
            PackageDir = filename:join([RegistryDir, "packages"]),
            ok = filelib:ensure_dir(filename:join(PackageDir, "placeholder")),
            {ok, PackageDir};
        Error ->
            Error
    end.

registry_checksum({{pkg, Name, Vsn, _Hash}, Registry}, State) ->
    try
        {ok, T} = ?MODULE:verify_table(Registry, State),
        [_, Checksum | _] = ets:lookup_element(T, {Name, Vsn}, 2),
        Checksum
    catch
        _:_ ->
            throw(?PRV_ERROR({missing_package, ec_cnv:to_binary(Name), ec_cnv:to_binary(Vsn)}))
    end.

%% Hex supports use of ~> to specify the version required for a dependency.
%% Since rebar3 requires exact versions to choose from we find the highest
%% available version of the dep that passes the constraint.

%% `~>` will never include pre-release versions of its upper bound.
%% It can also be used to set an upper bound on only the major
%% version part. See the table below for `~>` requirements and
%% their corresponding translation.
%% `~>` | Translation
%% :------------- | :---------------------
%% `~> 2.0.0` | `>= 2.0.0 and < 2.1.0`
%% `~> 2.1.2` | `>= 2.1.2 and < 2.2.0`
%% `~> 2.1.3-dev` | `>= 2.1.3-dev and < 2.2.0`
%% `~> 2.0` | `>= 2.0.0 and < 3.0.0`
%% `~> 2.1` | `>= 2.1.0 and < 3.0.0`
find_highest_matching(Dep, Constraint, State) ->
    find_highest_matching(undefined, undefined, Dep, Constraint, State).

find_highest_matching(Pkg, PkgVsn, Dep, Constraint, State) ->
    try
        find(rebar_state:repos(State),
            fun(Registry) ->
                find_highest_matching_(Pkg, PkgVsn, Dep, Constraint, Registry, State)
            end)
    of
        none ->
            handle_missing_package(Dep, State,
                                   fun(State1) ->
                                       find_highest_matching_(Pkg, PkgVsn, Dep, Constraint, State1)
                                   end);
        Result ->
            Result
    catch
        _:_ ->
            handle_missing_package(Dep, State,
                                   fun(State1) ->
                                       find_highest_matching_(Pkg, PkgVsn, Dep, Constraint, State1)
                                   end)
    end.

find_highest_matching_(Pkg, PkgVsn, Dep, Constraint, State) ->
    try
        find(rebar_state:repos(State),
            fun(Registry) ->
                find_highest_matching_(Pkg, PkgVsn, Dep, Constraint, Registry, State)
            end)
    catch
        _:_ ->
            none
    end.

find_highest_matching_(Pkg, PkgVsn, Dep, Constraint, Registry, State) ->
    try find_all(Dep, Registry, State) of
        {ok, [Vsn]} ->
            {ok, Vsn} = handle_single_vsn(Pkg, PkgVsn, Dep, Vsn, Constraint),
            {ok, Vsn, Registry};
        {ok, [HeadVsn | VsnTail]} ->
            {ok, handle_vsns(Constraint, HeadVsn, VsnTail), Registry};
        _ ->
            none
    catch
        error:badarg ->
            none
    end.

find_all(Dep, Registry, State) ->
    case ?MODULE:verify_table(Registry, State) of
        {ok, T} ->
            try ets:lookup_element(T, Dep, 2) of
                [Vsns | _] when is_list(Vsns)->
                    {ok, Vsns};
                Vsns ->
                    {ok, Vsns}
            catch
                error:badarg ->
                    none
            end;
        E ->
            E
    end.

handle_vsns(Constraint, HeadVsn, VsnTail) ->
    lists:foldl(fun(Version, Highest) ->
                        case ec_semver:pes(Version, Constraint) andalso
                            ec_semver:gt(Version, Highest) of
                            true ->
                                Version;
                            false ->
                                Highest
                        end
                end, HeadVsn, VsnTail).

handle_single_vsn(Pkg, PkgVsn, Dep, Vsn, Constraint) ->
    case ec_semver:pes(Vsn, Constraint) of
        true ->
            {ok, Vsn};
        false ->
            case {Pkg, PkgVsn} of
                {undefined, undefined} ->
                    ?DEBUG("Only existing version of ~s is ~s which does not match constraint ~~> ~s. "
                          "Using anyway, but it is not guaranteed to work.", [Dep, Vsn, Constraint]);
                _ ->
                    ?DEBUG("[~s:~s] Only existing version of ~s is ~s which does not match constraint ~~> ~s. "
                          "Using anyway, but it is not guaranteed to work.", [Pkg, PkgVsn, Dep, Vsn, Constraint])
            end,
            {ok, Vsn}
    end.

format_error({no_public_key, Reason, Registry}) ->
    io_lib:format("Failed to verify registry ~s, public key could be be read because of error: ~s ."
                  "Either install the public key or you can disable registry verification by setting `HEX_UNSAFE_REGISTRY=1`",
                 [Registry, file:format_error(Reason)]);
format_error({signature_fetch_fail, Registry}) ->
    io_lib:format("Failed to download signature for registry ~s", [Registry]);
format_error({failed_verify_registry, Registry}) ->
    io_lib:format("Could not verify authenticity of fetched registry file for ~s", [Registry]);
format_error({package_parse_cdn, Uri}) ->
    io_lib:format("Failed to parse registry url: ~p", [Uri]);
format_error({missing_package, Name, Vsn}) ->
    io_lib:format("Package not found in registry: ~s-~s.", [ec_cnv:to_binary(Name), ec_cnv:to_binary(Vsn)]);
format_error({missing_package, Dep}) ->
    io_lib:format("Package not found in registry: ~p.", [Dep]).

verify_table(Registry, State) ->
    case ets:lookup(?REPOS_TABLE, Registry) of
        [{Registry, T}] ->
            {ok, T};
        _ ->
            case load_and_verify_version(Registry, State) of
                {ok, T} ->
                    ets:insert(?REPOS_TABLE, {Registry, T}),
                    {ok, T};
                {uri_parse_error, Uri} ->
                    throw(?PRV_ERROR({package_parse_cdn, Uri}));
                _ ->
                    handle_bad_index(Registry, State)
            end
    end.
