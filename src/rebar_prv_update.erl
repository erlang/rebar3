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
                              DepsList = lists:foldl(fun([Dep, DepVsn, _, _], DepsListAcc) ->
                                                             case DepVsn of
                                                                 <<"~> ", Vsn/binary>> ->
                                                                     digraph:add_edge(Graph, {Pkg, PkgVsn}, {Dep, Vsn}),
                                                                     [{Dep, DepVsn} | DepsListAcc];
                                                                 Vsn ->
                                                                     digraph:add_edge(Graph, {Pkg, PkgVsn}, {Dep, Vsn}),
                                                                     [{Dep, Vsn} | DepsListAcc]
                                                             end
                                                     end, [], Deps),
                              dict:store({Pkg, PkgVsn}, DepsList, Dict);
                         (_, Dict) ->
                              Dict
                      end, dict:new(), T),
    {Dict1, Graph}.
