-module(rebar_prv_packages).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, pkgs).
-define(DEPS, []).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 pkgs"},
                                                               {short_desc, "List available packages."},
                                                               {desc, info("List available packages")},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Dict, _} = rebar_packages:get_packages(State),
    print_packages(Dict),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(load_registry_fail) ->
    "Failed to load package regsitry. Try running 'rebar3 update' to fix".

print_packages(Dict) ->
    Pkgs = lists:keysort(1, dict:fetch_keys(Dict)),
    SortedPkgs = lists:foldl(fun({Pkg, Vsn}, Acc) ->
                                       orddict:append_list(Pkg, [Vsn], Acc)
                               end, orddict:new(), Pkgs),

    orddict:map(fun(Name, Vsns) ->
                        SortedVsns = lists:sort(fun(A, B) ->
                                                        ec_semver:lte(ec_semver:parse(A)
                                                                     ,ec_semver:parse(B))
                                                end, Vsns),
                        VsnStr = join(SortedVsns, <<", ">>),
                        io:format("~s:~n    Versions: ~s~n~n", [Name, VsnStr])
                end, SortedPkgs).

-spec join([binary()], binary()) -> binary().
join([Bin], _Sep) ->
    <<Bin/binary>>;
join([Bin | T], Sep) ->
    <<Bin/binary, Sep/binary, (join(T, Sep))/binary>>.


info(Description) ->
    io_lib:format("~s.~n", [Description]).
