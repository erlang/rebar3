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
-export([new_package_table/0, find_highest_matching_/5, valid_vsn/1]).
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

-spec get_package_versions(unicode:unicode_binary(), verl:semver(),
                           unicode:unicode_binary(),
                           ets:tid(), rebar_state:t()) -> [vsn()].
get_package_versions(Dep, DepVsn, Repo, Table, State) ->
    _AllowPreRelease = rebar_state:get(State, deps_allow_prerelease, false),
    case rebar_verl:parse_requirement(DepVsn) of
        {error, _} ->
            none;
        {ok, #{matchspec := [{Head, [Match], _}]}} ->
            ?MODULE:verify_table(State),
            Vsns = ets:select(Table, [{#package{key={Dep, Head, Repo}, _='_'},
                                       [Match], [{Head}]}]),
            handle_vsns(Vsns)
    end.

-spec get_package(unicode:unicode_binary(), unicode:unicode_binary(),
                  binary() | undefined | '_',
                  [unicode:unicode_binary()] | ['_'], ets:tab(), rebar_state:t())
                 -> {ok, #package{}} | not_found.
get_package(Dep, Vsn, undefined, Repos, Table, State) ->
    get_package(Dep, Vsn, '_', Repos, Table, State);
get_package(Dep, Vsn, Hash, Repos, Table, State) when is_binary(Vsn) ->
    get_package(Dep, r3_verl:parse(Vsn), Hash, Repos, Table, State);
get_package(Dep, Vsn, Hash, Repos, Table, State) ->
    ?MODULE:verify_table(State),
    MatchingPackages = ets:select(Table, [{#package{key={Dep, Vsn, Repo},
                                      _='_'}, [], ['$_']} || Repo <- Repos]),
    PackagesWithProperHash = lists:filter(
        fun(#package{key = {_Dep, _Vsn, Repo}, outer_checksum = PkgChecksum}) ->
            if (PkgChecksum =/= Hash) andalso (Hash =/= '_') ->
                ?WARN("Checksum mismatch for package ~ts-~ts from repo ~ts", [Dep, rebar_verl:format_version(Vsn), Repo]),
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
find_highest_matching(Dep, Version, Repo, Table, State) ->
    Constraint = verl:add_highest_matching_prefix(Version),
    try find_highest_matching_(Dep, Constraint, Repo, Table, State) of
        none ->
            handle_missing_package(Dep, Repo, State,
                                   fun(State1) ->
                                       find_highest_matching_(Dep, Constraint, Repo, Table, State1)
                                   end);
        Result ->
            Result
    catch
        _:_ ->
            handle_missing_package(Dep, Repo, State,
                                   fun(State1) ->
                                       find_highest_matching_(Dep, Constraint, Repo, Table, State1)
                                   end)
    end.

find_highest_matching_(Dep, Constraint, #{name := Repo}, Table, State) ->
    try
        get_package_versions(Dep, Constraint, Repo, Table, State)
    catch
        error:badarg ->
            none
    end.

handle_vsns([]) -> none;
handle_vsns(Vsns) ->
    Vsn =
        lists:foldl(
            fun(Version, Highest) ->
                case (Highest =:= none orelse r3_verl:compare(Version, Highest) =:= gt) of
                    true ->
                        Version;
                    false ->
                        Highest
                end
            end, none, Vsns),
    {ok, Vsn}.

verify_table(State) ->
    ets:info(?PACKAGE_TABLE, named_table) =:= true orelse load_and_verify_version(State).

parse_deps(Deps) ->
    [{maps:get(app, D, Name), {pkg, Name, Constraint, undefined, undefined}}
     || D=#{package := Name,
            requirement := Constraint} <- Deps].

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
        {ok, {200, _Headers, Releases}} ->
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

insert_releases(Name, Releases, Repo, Table) ->
    Parse = fun (V) ->
        {ok, Res} = verl:parse(V),
        Res
    end,
    [true = ets:insert(Table,
                       #package{key={Name, Parse(Version), Repo},
                                inner_checksum=parse_checksum(InnerChecksum),
                                outer_checksum=parse_checksum(OuterChecksum),
                                retired=maps:get(retired, Release, false),
                                dependencies=parse_deps(Dependencies)})
     || Release=#{inner_checksum := InnerChecksum,
                  outer_checksum := OuterChecksum,
                  version := Version,
                  dependencies := Dependencies} <- Releases].

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
            Fun = fun(Repo) ->
                      case get_package_versions(Dep, DepVsn, Repo, HexRegistry, State) of
                          none ->
                              not_found;
                          {ok, Vsn} ->
                              get_package(Dep, Vsn, Hash, [Repo], HexRegistry, State)
                      end
                  end,
            handle_missing_no_exception(Fun, Dep, State)
    end;
resolve_version(Dep, undefined, _OldHash, Hash, HexRegistry, State) ->
    Fun = fun(Repo) ->
              case get_latest_version(Dep, Repo, HexRegistry, State) of
                  none ->
                      not_found;
                  {ok, Vsn} ->
                      get_package(Dep, Vsn, Hash, [Repo], HexRegistry, State)
              end
          end,
    handle_missing_no_exception(Fun, Dep, State);
resolve_version(Dep, DepVsn, _OldHash, Hash, HexRegistry, State) ->
    case valid_vsn(DepVsn) of
        false ->
            {error, {invalid_vsn, DepVsn}};
        _ ->
            Fun = fun(Repo) ->
                      case get_package_versions(Dep, DepVsn, Repo, HexRegistry, State) of
                          none ->
                              not_found;
                          {ok, Vsn} ->
                              get_package(Dep, Vsn, Hash, [Repo], HexRegistry, State)
                      end
                  end,
            handle_missing_no_exception(Fun, Dep, State)
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

valid_vsn(Vsn) ->
    rebar_verl:valid_requirement(Vsn).

get_latest_version(Dep, Repo, HexRegistry, State) ->
    verify_table(State),
    Vsns = ets:select(HexRegistry, [{#package{key={'$1', '$2', '$3'}, _='_'},
                                     [{'==', '$1', Dep}, {'==', '$3', Repo}], ['$2']}]),
    handle_vsns(Vsns).
