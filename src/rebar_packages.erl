-module(rebar_packages).

-export([packages/1
        ,close_packages/0
        ,load_and_verify_version/1
        ,deps/3
        ,registry_dir/1
        ,package_dir/1
        ,registry_checksum/2
        ,find_highest_matching/6
        ,find_highest_matching/4
        ,find_all/3
        ,verify_table/1
        ,format_error/1]).

-export_type([package/0]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-type pkg_name() :: binary() | atom().
-type vsn() :: binary().
-type package() :: pkg_name() | {pkg_name(), vsn()}.

-spec packages(rebar_state:t()) -> ets:tid().
packages(State) ->
    catch ets:delete(?PACKAGE_TABLE),
    case load_and_verify_version(State) of
        true ->
            ok;
        false ->
            ?DEBUG("Error loading package index.", []),
            handle_bad_index(State)
    end.

handle_bad_index(State) ->
    ?ERROR("Bad packages index. Trying to fix by updating the registry.", []),
    {ok, State1} = rebar_prv_update:do(State),
    case load_and_verify_version(State1) of
        true ->
            ok;
        false ->
            %% Still unable to load after an update, create an empty registry
            ets:new(?PACKAGE_TABLE, [named_table, public])
    end.

close_packages() ->
    catch ets:delete(?PACKAGE_TABLE).

load_and_verify_version(State) ->
    {ok, RegistryDir} = registry_dir(State),
    case ets:file2tab(filename:join(RegistryDir, ?INDEX_FILE)) of
        {ok, _} ->
            case ets:lookup_element(?PACKAGE_TABLE, package_index_version, 2) of
                ?PACKAGE_INDEX_VERSION ->
                    true;
                _ ->
                    (catch ets:delete(?PACKAGE_TABLE)),
                    rebar_prv_update:hex_to_index(State)
            end;
        _ ->
            rebar_prv_update:hex_to_index(State)
    end.

deps(Name, Vsn, State) ->
    try
        deps_(Name, Vsn, State)
    catch
        _:_ ->
            handle_missing_package({Name, Vsn}, State, fun(State1) -> deps_(Name, Vsn, State1) end)
    end.

deps_(Name, Vsn, State) ->
    ?MODULE:verify_table(State),
    ets:lookup_element(?PACKAGE_TABLE, {rebar_utils:to_binary(Name), rebar_utils:to_binary(Vsn)}, 2).

handle_missing_package(Dep, State, Fun) ->
    case Dep of
        {Name, Vsn} ->
            ?INFO("Package ~ts-~ts not found. Fetching registry updates and trying again...", [Name, Vsn]);
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

registry_dir(State) ->
    CacheDir = rebar_dir:global_cache_dir(rebar_state:opts(State)),
    case rebar_state:get(State, rebar_packages_cdn, ?DEFAULT_CDN) of
        ?DEFAULT_CDN ->
            RegistryDir = filename:join([CacheDir, "hex", "default"]),
            ok = filelib:ensure_dir(filename:join(RegistryDir, "placeholder")),
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

registry_checksum({pkg, Name, Vsn, _Hash}, State) ->
    try
        ?MODULE:verify_table(State),
        ets:lookup_element(?PACKAGE_TABLE, {Name, Vsn}, 3)
    catch
        _:_ ->
            throw(?PRV_ERROR({missing_package, rebar_utils:to_binary(Name), rebar_utils:to_binary(Vsn)}))
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
    try find_all(Dep, Table, State) of
        {ok, [Vsn]} ->
            handle_single_vsn(Pkg, PkgVsn, Dep, Vsn, Constraint);
        {ok, [HeadVsn | VsnTail]} ->
                            {ok, handle_vsns(Constraint, HeadVsn, VsnTail)}
    catch
        error:badarg ->
            none
    end.

find_all(Dep, Table, State) ->
    ?MODULE:verify_table(State),
    try ets:lookup_element(Table, Dep, 2) of
        [Vsns] when is_list(Vsns)->
            {ok, Vsns};
        Vsns ->
            {ok, Vsns}
    catch
        error:badarg ->
            none
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
                    ?DEBUG("Only existing version of ~ts is ~ts which does not match constraint ~~> ~ts. "
                          "Using anyway, but it is not guaranteed to work.", [Dep, Vsn, Constraint]);
                _ ->
                    ?DEBUG("[~ts:~ts] Only existing version of ~ts is ~ts which does not match constraint ~~> ~ts. "
                          "Using anyway, but it is not guaranteed to work.", [Pkg, PkgVsn, Dep, Vsn, Constraint])
            end,
            {ok, Vsn}
    end.

format_error({missing_package, Name, Vsn}) ->
    io_lib:format("Package not found in registry: ~ts-~ts.", [rebar_utils:to_binary(Name), rebar_utils:to_binary(Vsn)]);
format_error({missing_package, Dep}) ->
    io_lib:format("Package not found in registry: ~p.", [Dep]).

verify_table(State) ->
    ets:info(?PACKAGE_TABLE, named_table) =:= true orelse load_and_verify_version(State).
