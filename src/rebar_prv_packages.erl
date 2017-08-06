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
    rebar_packages:packages(State),
    case rebar_state:command_args(State) of
        [Name] ->
            print_packages(get_packages(rebar_utils:to_binary(Name)));
        _ ->
            print_packages(sort_packages())
    end,
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(load_registry_fail) ->
    "Failed to load package regsitry. Try running 'rebar3 update' to fix".

print_packages(Pkgs) ->
    orddict:map(fun(Name, Vsns) ->
                        SortedVsns = lists:sort(fun(A, B) ->
                                                        ec_semver:lte(ec_semver:parse(A)
                                                                     ,ec_semver:parse(B))
                                                end, Vsns),
                        VsnStr = join(SortedVsns, <<", ">>),
                        ?CONSOLE("~ts:~n    Versions: ~ts~n", [Name, VsnStr])
                end, Pkgs).

sort_packages() ->
    ets:foldl(fun({package_index_version, _}, Acc) ->
                      Acc;
                 ({Pkg, Vsns}, Acc) ->
                      orddict:store(Pkg, Vsns, Acc);
                 (_, Acc) ->
                      Acc
              end, orddict:new(), ?PACKAGE_TABLE).

get_packages(Name) ->
    ets:lookup(?PACKAGE_TABLE, Name).


-spec join([binary()], binary()) -> binary().
join([Bin], _Sep) ->
    <<Bin/binary>>;
join([Bin | T], Sep) ->
    <<Bin/binary, Sep/binary, (join(T, Sep))/binary>>.


info(Description) ->
    io_lib:format("~ts.~n", [Description]).
