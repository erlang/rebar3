-module(rebar_packages).

-export([get/2
        ,get_all_names/1
        ,registry_dir/1
        ,package_dir/2
        ,find_highest_matching/5
        ,verify_table/1
        ,format_error/1
        ,update_package/3
        ,resolve_version/6]).

-ifdef(TEST).
-export([new_package_table/0, find_highest_matching_/5, parse_deps/1]).
-endif.

-export_type([package/0]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-type pkg_name() :: binary() | atom().
-type vsn() :: binary().
-type package() :: pkg_name() | {pkg_name(), vsn()}.

format_error({missing_package, Name, Vsn}) ->
    io_lib:format("Package not found in any repo: ~ts ~ts", [rebar_utils:to_binary(Name),
                                                             rebar_utils:to_binary(Vsn)]);
format_error({missing_package, Pkg}) ->
    io_lib:format("Package not found in any repo: ~p", [Pkg]).

-spec get(rebar_hex_repos:repo(), binary()) -> {ok, map()} | {error, term()}.
get(Config, Name) ->
    try r3_hex_api_package:get(Config, Name) of
        {ok, {200, _Headers, PkgInfo}} ->
            {ok, PkgInfo};
        {ok, {404, _, _}} ->
            {error, not_found};
        Error ->
            ?DEBUG("Hex api request failed: ~p", [Error]),
            {error, unknown}
    catch
        error:{badmatch, {error, {failed_connect, _}}} ->
            {error, failed_to_connect};
        _:Exception ->
            ?DEBUG("hex_api_package:get failed: ~p", [Exception]),
            {error, unknown}
    end.


-spec get_all_names(rebar_state:t()) -> [binary()].
get_all_names(State) ->
    verify_table(State),
    lists:usort(ets:select(?PACKAGE_TABLE, [{#package{key={'$1', '_', '_'},
                                                      _='_'},
                                             [], ['$1']}])).

-spec get_package_versions(unicode:unicode_binary(), boolean(),
                           unicode:unicode_binary(),
                           ets:tid(), rebar_state:t()) -> [vsn()].
get_package_versions(Dep, AllowPreRelease, Repo, Table, State) ->
    ?MODULE:verify_table(State),
    AllowPreRelease2 = rebar_state:get(State, deps_allow_prerelease, false)
        orelse AllowPreRelease,
    ets:select(Table, [{#package{key={Dep, {'$1', '$2'}, Repo},
                                 _='_'},
                        [{'==', '$2', {{[],[]}}} || not AllowPreRelease2], [{{'$1', '$2'}}]}]).

-spec get_package(unicode:unicode_binary(), unicode:unicode_binary(),
                  binary() | undefined | '_',
                  [unicode:unicode_binary()] | ['_'], ets:tab(), rebar_state:t())
                 -> {ok, #package{}} | not_found.
get_package(Dep, Vsn, undefined, Repos, Table, State) ->
    get_package(Dep, Vsn, '_', Repos, Table, State);
get_package(Dep, Vsn, Hash, Repos, Table, State) ->
    ?MODULE:verify_table(State),
    case rebar_semver:parse_version(Vsn) of
        {ok, Parsed} ->
            MatchingPackages = ets:select(Table, [{#package{key={Dep, Parsed, Repo},
                                            _='_'}, [], ['$_']} || Repo <- Repos]),
            PackagesWithProperHash = lists:filter(
                fun(#package{key = {_Dep, _Vsn, Repo}, outer_checksum = PkgChecksum}) ->
                    if (PkgChecksum =/= Hash) andalso (Hash =/= '_') ->
                        ?WARN("Checksum mismatch for package ~ts-~ts from repo ~ts", [Dep, Vsn, Repo]),
                        false;
                    true ->
                        true
                    end
                end, MatchingPackages
            ),
            case PackagesWithProperHash of
                %% have to allow multiple matches in the list for cases that Repo is `_`
                [Package | _] ->
                    {ok, Package};
                [] ->
                    not_found
            end;
            
        _ ->
            not_found
    end.

new_package_table() ->
    ?PACKAGE_TABLE = ets:new(?PACKAGE_TABLE, [named_table, public, ordered_set, {keypos, 2}]),
    ets:insert(?PACKAGE_TABLE, {?PACKAGE_INDEX_VERSION, package_index_version}).

load_and_verify_version(State) ->
    {ok, RegistryDir} = registry_dir(State),
    case ets:file2tab(filename:join(RegistryDir, ?INDEX_FILE)) of
        {ok, _} ->
            case ets:lookup_element(?PACKAGE_TABLE, package_index_version, 1) of
                ?PACKAGE_INDEX_VERSION ->
                    true;
                V ->
                    %% no reason to confuse the user since we just start fresh and they
                    %% shouldn't notice, so log as a debug message only
                    ?DEBUG("Package index version mismatch. Current version ~p, this rebar3 expecting ~p",
                           [V, ?PACKAGE_INDEX_VERSION]),
                    (catch ets:delete(?PACKAGE_TABLE)),
                    new_package_table()
            end;
        _ ->
            new_package_table()
    end.

handle_missing_package(PkgKey, Repo, State, Fun) ->
    Name =
        case PkgKey of
            {N, Vsn, _Repo} ->
                ?DEBUG("Package ~ts-~ts not found. Fetching registry updates for "
                       "package and trying again...", [N, Vsn]),
                N;
            _ ->
                ?DEBUG("Package ~p not found. Fetching registry updates for "
                       "package and trying again...", [PkgKey]),
                PkgKey
        end,

    update_package(Name, Repo, State),
    try
        Fun(State)
    catch
        _:_ ->
            %% Even after an update the package is still missing, time to error out
            throw(?PRV_ERROR({missing_package, PkgKey}))
    end.

registry_dir(State) ->
    CacheDir = rebar_dir:global_cache_dir(rebar_state:opts(State)),
    RegistryDir = filename:join([CacheDir, "hex"]),
    case filelib:ensure_dir(filename:join(RegistryDir, "placeholder")) of
        ok -> ok;
        {error, Posix} when Posix == eaccess; Posix == enoent ->
            ?ABORT("Could not write to ~p. Please ensure the path is writeable.",
                   [RegistryDir])
    end,
    {ok, RegistryDir}.

-spec package_dir(rebar_hex_repos:repo(), rebar_state:t()) -> {ok, file:filename_all()}.
package_dir(Repo, State) ->
    {ok, RegistryDir} = registry_dir(State),
    RepoName = maps:get(name, Repo),
    PackageDir = filename:join([RegistryDir, rebar_utils:to_list(RepoName), "packages"]),
    ok = filelib:ensure_dir(filename:join(PackageDir, "placeholder")),
    {ok, PackageDir}.


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
find_highest_matching(Dep, DepVsn, Repo, Table, State) ->
    case find_highest_matching_(Dep, DepVsn, Repo, Table, State) of
        none ->
            handle_missing_package(Dep, Repo, State,
                                   fun(State1) ->
                                       find_highest_matching_(Dep, DepVsn, Repo, Table, State1)
                                   end);
        Result ->
            Result
    end.

find_highest_matching_(Dep, DepVsn, #{name := Repo}, Table, State) when is_tuple(DepVsn) ->
    find_highest_matching_(Dep, rebar_semver:format(DepVsn), Repo, Table, State);
find_highest_matching_(Dep, DepVsn, #{name := Repo}, Table, State) when is_binary(DepVsn) ->
    case rebar_semver:parse_version(DepVsn) of
        {ok, _} ->
            resolve_version_(Dep, <<"~> "/utf8, DepVsn/binary>>, Repo, Table, State);
            
        {error, _} ->
            resolve_version_(Dep, DepVsn, Repo, Table, State)
    end.

verify_table(State) ->
    ets:info(?PACKAGE_TABLE, named_table) =:= true orelse load_and_verify_version(State).

%% Filter out optional dependencies. Rebar3 does not support optional
%% dependencies, so we simply ignore them. For full parity with Mix, we would
%% need to check if the dependency is included elsewhere (even transitively)
%% without the optional flag, and only exclude it if all references are
%% optional.
parse_deps(Deps) ->
    [{maps:get(app, D, Name), {pkg, Name, Constraint, undefined, undefined}}
     || D=#{package := Name,
            requirement := Constraint} <- Deps,
        not maps:get(optional, D, false)].

parse_checksum(<<Checksum:256/big-unsigned>>) ->
    list_to_binary(
      rebar_string:uppercase(
        lists:flatten(io_lib:format("~64.16.0b", [Checksum]))));
parse_checksum(Checksum) ->
    Checksum.

update_package(Name, RepoConfig=#{name := Repo}, State) ->
    ?MODULE:verify_table(State),
    ?DEBUG("Getting definition for package ~ts from repo ~ts",
           [Name, rebar_hex_repos:format_repo(RepoConfig)]),
    try r3_hex_repo:get_package(get_package_repo_config(RepoConfig), Name) of
        {ok, {200, _Headers, Package}} ->
            #{releases := Releases} = Package,
            _ = insert_releases(Name, Releases, Repo, ?PACKAGE_TABLE),
            {ok, RegistryDir} = rebar_packages:registry_dir(State),
            PackageIndex = filename:join(RegistryDir, ?INDEX_FILE),
            case ets:tab2file(?PACKAGE_TABLE, PackageIndex) of
                ok -> ok;
                {error, Error} ->
                    ?WARN("Failed to update package index at ~p: ~p", [PackageIndex, Error])
            end;
        {error, unverified} ->
            ?WARN(unverified_repo_message(), [Repo]),
            fail;
        Error ->
            ?DEBUG("Hex get_package request failed: ~p", [Error]),
            %% TODO: add better log message. r3_hex_core should export a format_error
            ?WARN("Failed to update package ~ts from repo ~ts", [Name, Repo]),
            fail
    catch
        _:Exception ->
            ?DEBUG("hex_repo:get_package failed for package ~p: ~p", [Name, Exception]),
            fail
    end.

get_package_repo_config(RepoConfig=#{mirror_of := _}) ->
    get_package_repo_config(maps:remove(mirror_of, RepoConfig));
get_package_repo_config(RepoConfig=#{name := _}) ->
    get_package_repo_config(maps:remove(name, RepoConfig));
get_package_repo_config(RepoConfig) ->
    RepoConfig.

unverified_repo_message() ->
    "The registry repository ~ts uses a record format that has been deprecated for "
    "security reasons. The repository should be updated in order to be safer. "
    "You can disable this check by setting REBAR_NO_VERIFY_REPO_ORIGIN=1".

insert_releases(_, [], _, _) -> nil;
insert_releases(Name, [Release|Releases], Repo, Table) ->
    #{
        inner_checksum := InnerChecksum,
        outer_checksum := OuterChecksum,
        version := Version,
        dependencies := Dependencies
    } = Release,
    {ok, Parsed} = rebar_semver:parse_version(Version),
    Package = #package{
        key={Name, Parsed, Repo},
        inner_checksum=parse_checksum(InnerChecksum),
        outer_checksum=parse_checksum(OuterChecksum),
        retired=maps:get(retired, Release, false),
        dependencies=parse_deps(Dependencies)
    },
    true = ets:insert(Table, Package),
    insert_releases(Name, Releases, Repo, Table).

-spec resolve_version(unicode:unicode_binary(), unicode:unicode_binary() | undefined,
                      binary() | undefined,
                      binary() | undefined,
                      ets:tab(), rebar_state:t())
                     -> {error, {invalid_vsn, unicode:unicode_binary()}} |
                        not_found |
                        {ok, #package{}, map()}.
%% if checksum is defined search for any matching repo matching pkg-vsn and checksum
resolve_version(Dep, DepVsn, _OldHash, Hash, HexRegistry, State) when is_binary(Hash) ->
    Resources = rebar_state:resources(State),
    #{repos := RepoConfigs} = rebar_resource_v2:find_resource_state(pkg, Resources),
    RepoNames = [RepoName || #{name := RepoName} <- RepoConfigs],

    %% allow retired packages when we have a checksum
    case get_package(Dep, DepVsn, Hash, RepoNames, HexRegistry, State) of
        {ok, Package=#package{key={_, _, RepoName}}} ->
            {ok, RepoConfig} = rebar_hex_repos:get_repo_config(RepoName, RepoConfigs),
            {ok, Package, RepoConfig};
        _ ->
            resolve_version_no_package(Dep, DepVsn, Hash, HexRegistry, State)
    end;

resolve_version(Dep, DepVsn, _OldHash, Hash, HexRegistry, State) ->
    resolve_version_no_package(Dep, DepVsn, Hash, HexRegistry, State).

resolve_version_no_package(Dep, DepVsn, Hash, HexRegistry, State) ->
    case rebar_semver:parse_constraint(DepVsn) of
        {ok, _} ->
            Fun = fun(Repo) ->
                case resolve_version_(Dep, DepVsn, Repo, HexRegistry, State) of
                    none ->
                        not_found;
                    {ok, Vsn} ->
                        get_package(Dep, Vsn, Hash, [Repo], HexRegistry, State)
                end
            end,
            handle_missing_no_exception(Fun, Dep, State);
        
        Error ->
            Error
    end.

    
check_all_repos(Fun, RepoConfigs) ->
    ec_lists:search(fun(#{name := R}) ->
                            Fun(R)
                    end, RepoConfigs).

handle_missing_no_exception(Fun, Dep, State) ->
    Resources = rebar_state:resources(State),
    #{repos := RepoConfigs} = rebar_resource_v2:find_resource_state(pkg, Resources),

    %% first check all repos in order for a local match
    %% if none is found then we step through checking after updating the repo registry
    case check_all_repos(Fun, RepoConfigs) of
        not_found ->
            ec_lists:search(fun(Config=#{name := R}) ->
                                    case ?MODULE:update_package(Dep, Config, State) of
                                        ok ->
                                            Fun(R);
                                        _ ->
                                            not_found
                                    end
                            end, RepoConfigs);
        Result ->
            Result
    end.

resolve_version_(Dep, Constraint, Repo, HexRegistry, State) ->
    case rebar_semver:parse_constraint(Constraint) of
        {ok, Match} ->
            AllowPreRelease = rebar_semver:is_prerelease_or_build(Constraint),
            AllVersions = get_package_versions(Dep, AllowPreRelease, Repo, HexRegistry, State),
            resolve_version_loop(Match, AllVersions, none);
    
        Error ->
            Error
    end.
    
resolve_version_loop(_Constraint, [], none) -> none;
resolve_version_loop(_Constraint, [], BestMatch) -> {ok, BestMatch};
resolve_version_loop(Constraint, [Vsn|R], none) ->
    case rebar_semver:match(Vsn, Constraint) of
        true -> resolve_version_loop(Constraint, R, Vsn);
        _ -> resolve_version_loop(Constraint, R, none)
    end;
resolve_version_loop(Constraint, [Vsn|R], BestMatch) ->
    case rebar_semver:match(Vsn, Constraint) of
        true ->
            case rebar_semver:cmp(Vsn, BestMatch) of
                gt -> resolve_version_loop(Constraint, R, Vsn);
                _ -> resolve_version_loop(Constraint, R, BestMatch)
            end;
        _ -> resolve_version_loop(Constraint, R, BestMatch)
    end.
