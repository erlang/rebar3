%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_update).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, update).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, "rebar update"},
                                                               {short_desc, "Update package index."},
                                                               {desc, ""},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?INFO("Updating package index...", []),
    try
        Url = rebar_state:get(State, rebar_packages_cdn, "https://s3.amazonaws.com/s3.hex.pm/registry.ets.gz"),
        TmpDir = ec_file:insecure_mkdtemp(),
        TmpFile = filename:join(TmpDir, "packages.gz"),
        HexFile = filename:join(TmpDir, "packages"),
        {ok, _RequestId} = httpc:request(get, {Url, []},
                                         [], [{stream, TmpFile}, {sync, true}]),
        {ok, Data} = file:read_file(TmpFile),
        Unzipped = zlib:gunzip(Data),
        ok = file:write_file(HexFile, Unzipped),
        {Dict, Graph} = hex_to_graph(HexFile),
        write_registry(Dict, Graph, State),
        ok
    catch
        E:C ->
            io:format("E C ~p ~p~n", [E, C]),
            throw({error, {?MODULE, package_index_write}})
    end,

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(package_index_write) ->
    "Failed to write package index.".

write_registry(Dict, {digraph, Edges, Vertices, Neighbors, _}, State) ->
    Dir = rebar_dir:global_config_dir(State),
    RegistryDir = filename:join(Dir, "packages"),
    filelib:ensure_dir(filename:join(RegistryDir, "dummy")),
    ets:tab2file(Edges, filename:join(RegistryDir, "edges")),
    ets:tab2file(Vertices, filename:join(RegistryDir, "vertices")),
    ets:tab2file(Neighbors, filename:join(RegistryDir, "neighbors")),
    file:write_file(filename:join(RegistryDir, "dict"), term_to_binary(Dict)).

hex_to_graph(Filename) ->
    {ok, T} = ets:file2tab(Filename),
    Graph = digraph:new(),
    ets:foldl(fun({Pkg, [Versions]}, ok) when is_binary(Pkg), is_list(Versions) ->
                      lists:foreach(fun(Version) ->
                                            digraph:add_vertex(Graph, {Pkg, Version}, 1)
                                    end, Versions);
                 (_, ok) ->
                      ok
              end, ok, T),

    Dict1 = ets:foldl(fun({{Pkg, PkgVsn}, [Deps | _]}, Dict) ->
                              DepsList = update_graph(Pkg, PkgVsn, Deps, T, Graph),
                              dict:store({Pkg, PkgVsn}, DepsList, Dict);
                         (_, Dict) ->
                              Dict
                      end, dict:new(), T),
    {Dict1, Graph}.

update_graph(Pkg, PkgVsn, Deps, HexRegistry, Graph) ->
    lists:foldl(fun([Dep, DepVsn, _, _], DepsListAcc) ->
                        case DepVsn of
                            <<"~> ", Vsn/binary>> ->
                                HighestDepVsn = find_highest_matching(Dep, Vsn, HexRegistry),
                                digraph:add_edge(Graph, {Pkg, PkgVsn}, {Dep, HighestDepVsn}),
                                [{Dep, DepVsn} | DepsListAcc];
                            Vsn ->
                                digraph:add_edge(Graph, {Pkg, PkgVsn}, {Dep, Vsn}),
                                [{Dep, Vsn} | DepsListAcc]
                        end
                end, [], Deps).

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
find_highest_matching(Dep, Constraint, T) ->
    case ets:lookup(T, Dep) of
        [{Dep, [[Vsn]]}] ->
            case ec_semver:pes(Vsn, Constraint) of
                true ->
                    Vsn;
                false ->
                    ?WARN("Only existing version of ~s is ~s which does not match constraint ~~> ~s. "
                         "Using anyway, but it is not guarenteed to work.", [Dep, Vsn, Constraint]),
                    Vsn
            end;
        [{Dep, [[HeadVsn | VsnTail]]}] ->
            lists:foldl(fun(Version, Highest) ->
                                case ec_semver:pes(Version, Constraint) andalso
                                    ec_semver:gt(Version, Highest) of
                                    true ->
                                        Version;
                                    false ->
                                        Highest
                                end
                        end, HeadVsn, VsnTail)
    end.
