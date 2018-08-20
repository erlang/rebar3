-module(rebar_packages).

-export([get/2
        ,get_all_names/1
        ,get_package_versions/3
        ,get_package_deps/3
        ,new_package_table/0
        ,load_and_verify_version/1        
        ,registry_dir/1
        ,package_dir/1
        ,registry_checksum/3
        ,find_highest_matching/6
        ,find_highest_matching/4
        ,find_highest_matching_/6        
        ,verify_table/1
        ,format_error/1
        ,update_package/2
        ,resolve_version/4]).

-ifdef(TEST).
-export([cmp_/4, cmpl_/4, valid_vsn/1]).
-endif.

-export_type([package/0]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-type pkg_name() :: binary() | atom().
-type vsn() :: binary().
-type package() :: pkg_name() | {pkg_name(), vsn()}.

format_error({missing_package, Name, Vsn}) ->
    io_lib:format("Package not found in registry: ~ts-~ts.", [rebar_utils:to_binary(Name), 
                                                              rebar_utils:to_binary(Vsn)]);
format_error({missing_package, Pkg}) ->
    io_lib:format("Package not found in registry: ~p.", [Pkg]).

-spec get(hex_core:config(), binary()) -> {ok, map()} | {error, term()}.
get(Config, Name) ->    
    case hex_api_package:get(Config, Name) of
        {ok, {200, _Headers, PkgInfo}} ->
            {ok, PkgInfo};
        _ ->
            {error, blewup}
    end.

-spec get_all_names(rebar_state:t()) -> [binary()].
get_all_names(State) ->    
    verify_table(State),
    lists:usort(ets:select(?PACKAGE_TABLE, [{#package{key={'$1', '_'},
                                                      _='_'}, 
                                             [], ['$1']}])).

-spec get_package_versions(binary(), ets:tid(), rebar_state:t()) -> [vsn()].
get_package_versions(Dep, Table, State) ->
    ?MODULE:verify_table(State),
    ets:select(Table, [{#package{key={Dep,'$1'},
                                 _='_'}, 
                        [], ['$1']}]).

new_package_table() ->    
    ets:new(?PACKAGE_TABLE, [named_table, public, ordered_set, {keypos, 2}]),
    ets:insert(package_index, {?PACKAGE_INDEX_VERSION, package_index_version}).

-spec get_package_deps(binary(), vsn(), rebar_state:t()) -> [map()].
get_package_deps(Name, Vsn, State) ->
    try_lookup(?PACKAGE_TABLE, {Name, Vsn}, #package.dependencies, State).

-spec registry_checksum(binary(), vsn(), rebar_state:t()) -> binary().
registry_checksum(Name, Vsn, State) ->
    try_lookup(?PACKAGE_TABLE, {Name, Vsn}, #package.checksum, State).

try_lookup(Table, Key, Element, State) ->
    ?MODULE:verify_table(State),
    try
        ets:lookup_element(Table, Key, Element)       
    catch
       _:_ ->
            handle_missing_package(Key, State, fun(_) -> 
                                                       ets:lookup_element(Table, Key, Element) 
                                               end)
    end.

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

handle_missing_package(PkgKey, State, Fun) ->
    Name = 
        case PkgKey of
            {N, Vsn} ->
                ?DEBUG("Package ~ts-~ts not found. Fetching registry updates for "
                       "package and trying again...", [N, Vsn]),
                N;
            _ ->
                ?DEBUG("Package ~p not found. Fetching registry updates for "
                       "package and trying again...", [PkgKey]),
                PkgKey
        end,

    update_package(Name, State),
    try 
        Fun(State) 
    catch
        _:_ ->
            %% Even after an update the package is still missing, time to error out
            throw(?PRV_ERROR({missing_package, PkgKey}))
    end.

registry_dir(State) ->
    CacheDir = rebar_dir:global_cache_dir(rebar_state:opts(State)),
    case rebar_state:get(State, rebar_packages_cdn, ?DEFAULT_CDN) of
        ?DEFAULT_CDN ->
            RegistryDir = filename:join([CacheDir, "hex", "default"]),
            case filelib:ensure_dir(filename:join(RegistryDir, "placeholder")) of
                ok -> ok;
                {error, Posix} when Posix == eaccess; Posix == enoent ->
                    ?ABORT("Could not write to ~p. Please ensure the path is writeable.",
                           [RegistryDir])
            end,
            {ok, RegistryDir};
        CDN ->
            case rebar_utils:url_append_path(CDN, ?REMOTE_PACKAGE_DIR) of
                {ok, Parsed} ->
                    {ok, {_, _, Host, _, Path, _}} = http_uri:parse(Parsed),
                    CDNHostPath = lists:reverse(rebar_string:lexemes(Host, ".")),
                    CDNPath = tl(filename:split(Path)),
                    RegistryDir = filename:join([CacheDir, "hex"] ++ CDNHostPath ++ CDNPath),
                    ok = filelib:ensure_dir(filename:join(RegistryDir, "placeholder")),
                    {ok, RegistryDir};
                _ ->
                    {uri_parse_error, CDN}
            end
    end.

package_dir(State) ->
    case registry_dir(State) of
        {ok, RegistryDir} ->
            PackageDir = filename:join([RegistryDir, "packages"]),
            ok = filelib:ensure_dir(filename:join(PackageDir, "placeholder")),
            {ok, PackageDir};
        Error ->
            Error
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
find_highest_matching(Dep, Constraint, Table, State) ->
    find_highest_matching(undefined, undefined, Dep, Constraint, Table, State).

find_highest_matching(Pkg, PkgVsn, Dep, Constraint, Table, State) ->
    try find_highest_matching_(Pkg, PkgVsn, Dep, Constraint, Table, State) of
        none ->
            handle_missing_package(Dep, State,
                                   fun(State1) ->
                                       find_highest_matching_(Pkg, PkgVsn, Dep, Constraint, Table, State1)
                                   end);
        Result ->
            Result
    catch
        _:_ ->
            handle_missing_package(Dep, State,
                                   fun(State1) ->
                                       find_highest_matching_(Pkg, PkgVsn, Dep, Constraint, Table, State1)
                                   end)
    end.

find_highest_matching_(Pkg, PkgVsn, Dep, Constraint, Table, State) ->    
    try get_package_versions(Dep, Table, State) of
        [Vsn] ->
            handle_single_vsn(Pkg, PkgVsn, Dep, Vsn, Constraint);
        Vsns ->
            case handle_vsns(Constraint, Vsns) of
                none ->
                    none;
                FoundVsn ->
                    {ok, FoundVsn}
            end
    catch
        error:badarg ->
            none
    end.

handle_vsns(Constraint, Vsns) ->
    lists:foldl(fun(Version, Highest) ->
                        case ec_semver:pes(Version, Constraint) andalso
                            (Highest =:= none orelse ec_semver:gt(Version, Highest)) of
                            true ->
                                Version;
                            false ->
                                Highest
                        end
                end, none, Vsns).

handle_single_vsn(Pkg, PkgVsn, Dep, Vsn, Constraint) ->
    case ec_semver:pes(Vsn, Constraint) of
        true ->
            {ok, Vsn};
        false ->
            case {Pkg, PkgVsn} of
                {undefined, undefined} ->
                    ?DEBUG("Only existing version of ~ts is ~ts which does not match constraint ~~> ~ts. "
                          "Using anyway, but it is not guaranteed to work.", [Dep, Vsn, Constraint]);
                _ ->
                    ?DEBUG("[~ts:~ts] Only existing version of ~ts is ~ts which does not match constraint ~~> ~ts. "
                          "Using anyway, but it is not guaranteed to work.", [Pkg, PkgVsn, Dep, Vsn, Constraint])
            end,
            {ok, Vsn}
    end.

verify_table(State) ->
    ets:info(?PACKAGE_TABLE, named_table) =:= true orelse load_and_verify_version(State).

parse_deps(Deps) ->
    [{maps:get(app, D, Name), {pkg, Name, Constraint, undefined}} 
     || D=#{package := Name,
            requirement := Constraint} <- Deps].

parse_checksum(<<Checksum:256/big-unsigned>>) ->
    list_to_binary(
      rebar_string:uppercase(
        lists:flatten(io_lib:format("~64.16.0b", [Checksum]))));
parse_checksum(Checksum) ->
    Checksum.

update_package(Name, State) ->
    Resources = rebar_state:resources(State),
    #{hex_config := HexConfig} = rebar_resource:find_resource_state(pkg, Resources),
    case hex_repo:get_package(HexConfig, Name) of
        {ok, {200, _Headers, #{releases := Releases}}} ->
            _ = insert_releases(Name, Releases, ?PACKAGE_TABLE),
            {ok, RegistryDir} = rebar_packages:registry_dir(State),
            PackageIndex = filename:join(RegistryDir, ?INDEX_FILE),
            ok = ets:tab2file(?PACKAGE_TABLE, PackageIndex);
        _ ->
            fail
    end.

insert_releases(Name, Releases, Table) ->
    [true = ets:insert(Table,
                       #package{key={Name, Version},
                                checksum=parse_checksum(Checksum),
                                dependencies=parse_deps(Dependencies)})
     || #{checksum := Checksum,
          version := Version,
          dependencies := Dependencies} <- Releases].

resolve_version(Dep, undefined, HexRegistry, State) ->
    find_highest_matching(Dep, "0", HexRegistry, State);
resolve_version(Dep, DepVsn, HexRegistry, State) ->
    case {valid_vsn(DepVsn), DepVsn} of
        {false, Vsn} ->
            {error, {invalid_vsn, Vsn}};
        {_, <<"~>", Vsn/binary>>} ->
            highest_matching(Dep, rm_ws(Vsn), HexRegistry, State);
        {_, <<">=", Vsn/binary>>} ->
            cmp(Dep, rm_ws(Vsn), HexRegistry, State, fun ec_semver:gte/2);
        {_, <<">", Vsn/binary>>} ->
            cmp(Dep, rm_ws(Vsn), HexRegistry, State, fun ec_semver:gt/2);
        {_, <<"<=", Vsn/binary>>} ->
            cmpl(Dep, rm_ws(Vsn), HexRegistry, State, fun ec_semver:lte/2);
        {_, <<"<", Vsn/binary>>} ->
            cmpl(Dep, rm_ws(Vsn), HexRegistry, State, fun ec_semver:lt/2);
        {_, <<"==", Vsn/binary>>} ->
            {ok, Vsn};
        {_, Vsn} ->
            {ok, Vsn}
    end.

rm_ws(<<" ", R/binary>>) ->
    rm_ws(R);
rm_ws(R) ->
    R.

valid_vsn(Vsn) ->
    %% Regepx from https://github.com/sindresorhus/semver-regex/blob/master/index.js
    SemVerRegExp = "v?(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)(\\.(0|[1-9][0-9]*))?"
        "(-[0-9a-z-]+(\\.[0-9a-z-]+)*)?(\\+[0-9a-z-]+(\\.[0-9a-z-]+)*)?",
    SupportedVersions = "^(>=?|<=?|~>|==)?\\s*" ++ SemVerRegExp ++ "$",
    re:run(Vsn, SupportedVersions, [unicode]) =/= nomatch.

highest_matching(Dep, Vsn, HexRegistry, State) ->
    case find_highest_matching_(undefined, undefined, Dep, Vsn, HexRegistry, State) of
        {ok, HighestDepVsn} ->
            {ok, HighestDepVsn};
        none ->
            {error, {invalid_vsn, Vsn}}
    end.

cmp(Dep, Vsn, HexRegistry, State, CmpFun) ->
    Vsns  = get_package_versions(Dep, HexRegistry, State),
    cmp_(undefined, Vsn, Vsns, CmpFun).

cmp_(undefined, MinVsn, [], _CmpFun) ->
    MinVsn;
cmp_(HighestDepVsn, _MinVsn, [], _CmpFun) ->
    HighestDepVsn;

cmp_(BestMatch, MinVsn, [Vsn | R], CmpFun) ->
    case CmpFun(Vsn, MinVsn) of
        true ->
            cmp_(Vsn, Vsn, R, CmpFun);
        false  ->
            cmp_(BestMatch, MinVsn, R, CmpFun)
    end.

%% We need to treat this differently since we want a version that is LOWER but
%% the higest possible one.
cmpl(Dep, Vsn, HexRegistry, State, CmpFun) ->
    Vsns  = get_package_versions(Dep, HexRegistry, State),
    cmpl_(undefined, Vsn, Vsns, CmpFun).

cmpl_(undefined, MaxVsn, [], _CmpFun) ->
    MaxVsn;
cmpl_(HighestDepVsn, _MaxVsn, [], _CmpFun) ->
    HighestDepVsn;

cmpl_(undefined, MaxVsn, [Vsn | R], CmpFun) ->
    case CmpFun(Vsn, MaxVsn) of
        true ->
            cmpl_(Vsn, MaxVsn, R, CmpFun);
        false  ->
            cmpl_(undefined, MaxVsn, R, CmpFun)
    end;

cmpl_(BestMatch, MaxVsn, [Vsn | R], CmpFun) ->
    case CmpFun(Vsn, MaxVsn) of
        true ->
            case ec_semver:gte(Vsn, BestMatch) of
                true ->
                    cmpl_(Vsn, MaxVsn, R, CmpFun);
                false ->
                    cmpl_(BestMatch, MaxVsn, R, CmpFun)
            end;
        false  ->
            cmpl_(BestMatch, MaxVsn, R, CmpFun)
    end.
