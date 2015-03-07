-module(rebar_prv_packages).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, pkgs).
-define(DEPS, []).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 pkgs"},
                                                               {short_desc, "List available packages."},
                                                               {desc, info("List available packages")},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Packages, _Graph} = rebar_packages:get_packages(State),
    print_packages(Packages),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

print_packages(Packages) ->
    Keys = lists:keysort(1, dict:fetch_keys(Packages)),
    Pkgs = merge(Keys),
    lists:foreach(fun({Name, Vsns}) ->
                          VsnStr = join(Vsns, <<", ">>),
                          io:format("~s:~n    Versions: ~s~n~n", [Name, VsnStr])
                  end, Pkgs).

-spec merge([{binary(), binary()}]) -> [{binary(), [binary()]}].
merge(List) ->
    merge([], List).

merge(List, []) ->
    List;
merge([{Key, Values} | T], [{Key, Value} | Rest]) ->
    merge([{Key, [Value | Values]} | T], Rest);
merge(List, [{Key, Value} | Rest]) ->
    merge([{Key, [Value]} | List], Rest).

-spec join([binary()], binary()) -> binary().
join([Bin], _Sep) ->
    <<Bin/binary>>;
join([Bin | T], Sep) ->
    <<Bin/binary, Sep/binary, (join(T, Sep))/binary>>.


info(Description) ->
    io_lib:format("~s.~n", [Description]).
