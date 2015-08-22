-module(rebar_packages).

-export([packages/1
        ,close_packages/0
        ,load_and_verify_version/1
        ,deps/3
        ,package_dir/1
        ,registry_checksum/2
        ,find_highest_matching/4
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
            ?ERROR("Bad packages index, try to fix with `rebar3 update`", []),
            ets:new(?PACKAGE_TABLE, [named_table, public])
    end.

close_packages() ->
    catch ets:delete(?PACKAGE_TABLE).

load_and_verify_version(State) ->
    RegistryDir = package_dir(State),
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
        verify_table(State),
        ets:lookup_element(?PACKAGE_TABLE, {ec_cnv:to_binary(Name), ec_cnv:to_binary(Vsn)}, 2)
    catch
        _:_ ->
            throw(?PRV_ERROR({missing_package, ec_cnv:to_binary(Name), ec_cnv:to_binary(Vsn)}))
    end.

package_dir(State) ->
    CacheDir = rebar_dir:global_cache_dir(State),
    CDN = rebar_state:get(State, rebar_packages_cdn, ?DEFAULT_CDN),
    {ok, {_, _, Host, _, Path, _}} = http_uri:parse(CDN),
    CDNHostPath = lists:reverse(string:tokens(Host, ".")),
    CDNPath = tl(filename:split(Path)),
    PackageDir = filename:join([CacheDir, "hex"] ++ CDNHostPath ++ CDNPath ++ ["packages"]),
    ok = filelib:ensure_dir(filename:join(PackageDir, "placeholder")),
    PackageDir.

registry_checksum({pkg, Name, Vsn}, State) ->
    try
        verify_table(State),
        ets:lookup_element(?PACKAGE_TABLE, {Name, Vsn}, 3)
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
find_highest_matching(Dep, Constraint, Table, State) ->
    verify_table(State),
    case ets:lookup_element(Table, Dep, 2) of
        [[HeadVsn | VsnTail]] ->
            {ok, handle_vsns(Constraint, HeadVsn, VsnTail)};
        [[Vsn]] ->
            handle_single_vsn(Dep, Vsn, Constraint);
        [Vsn] ->
            handle_single_vsn(Dep, Vsn, Constraint);
        [HeadVsn | VsnTail] ->
            {ok, handle_vsns(Constraint, HeadVsn, VsnTail)};
        [] ->
            ?WARN("Missing registry entry for package ~s. Try to fix with `rebar3 update`", [Dep]),
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

handle_single_vsn(Dep, Vsn, Constraint) ->
    case ec_semver:pes(Vsn, Constraint) of
        true ->
            {ok, Vsn};
        false ->
            ?WARN("Only existing version of ~s is ~s which does not match constraint ~~> ~s. "
                 "Using anyway, but it is not guarenteed to work.", [Dep, Vsn, Constraint]),
            {ok, Vsn}
    end.

format_error({missing_package, Package, Version}) ->
    io_lib:format("Package not found in registry: ~s-~s. Try to fix with `rebar3 update`", [Package, Version]).

verify_table(State) ->
    ets:info(?PACKAGE_TABLE, named_table) =:= true orelse ?MODULE:load_and_verify_version(State).
