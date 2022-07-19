%%% Common functions to boot/stop distributed setups for
%%% the rebar3 script.
-module(rebar_dist_utils).
-export([either/3, short/2, long/2, find_options/1]).
-include("rebar.hrl").

%%%%%%%%%%%%%%%%%%
%%% PUBLIC API %%%
%%%%%%%%%%%%%%%%%%

%% @doc allows to pick whether to use a short or long name, and
%% starts the distributed mode for it.
-spec either(Name::atom(), SName::atom(), Opts::[{setcookie,term()}]) -> atom().
either(undefined, undefined, _) ->
    'nonode@nohost';
either(Name, undefined, Opts) ->
    long(Name, Opts),
    node();
either(undefined, SName, Opts) ->
    short(SName, Opts),
    node();
either(_, _, _) ->
    ?ABORT("Cannot have both short and long node names defined", []).

%% @doc starts a node with a short name.
-spec short(SName::atom(), Opts::[{setcookie,term()}]) -> term().
short(Name, Opts) ->
    start(Name, shortnames, Opts).

%% @doc starts a node with a long name.
-spec long(Name::atom(), Opts::[{setcookie,term()}]) -> term().
long(Name, Opts) ->
    start(Name, longnames, Opts).

%% @doc utility function to extract all distribution options
%% from a rebar3 state tuple.
-spec find_options(rebar_state:t()) -> {Long, Short, Opts} when
      Long :: atom(),
      Short :: atom(),
      Opts :: [{setcookie,term()}].
find_options(State) ->
    {Long, Short} = find_name_options(State),
    case find_cookie_option(State) of
        nocookie ->
            {Long, Short, []};
        Cookie ->
            {Long, Short, [{setcookie, Cookie}]}
    end.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%
start(Name, Type, Opts) ->
    case net_adm:names() of
        {error, _} ->
            start_epmd();
        {ok, _} ->
            ok
    end,
    dist_up(net_kernel:start([Name, Type])) orelse warn_dist(),
    setup_cookie(Opts).

dist_up({error,{{shutdown,{_,net_kernel,{'EXIT',nodistribution}}},_}}) -> false;
dist_up(_) -> true.

start_epmd() ->
    %% Indirectly boot EPMD through calling Erlang so that we don't risk
    %% attaching it to the current proc
    ?CONSOLE("Attempting to start epmd...", []),
    os:cmd("erl -sname a -noinput -eval \"halt(0).\"").

warn_dist() ->
    ?ERROR("Erlang Distribution failed, falling back to nonode@nohost.", []).


setup_cookie(Opts) ->
    case {node(), proplists:get_value(setcookie, Opts, nocookie)} of
        {'nonode@nohost', _} -> nocookie;
        {_, nocookie} -> nocookie;
        {Node, Name} -> erlang:set_cookie(Node, Name)
    end.

find_name_options(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    %% First try the CLI
    case {proplists:get_value(name, Opts), proplists:get_value(sname, Opts)} of
        {undefined, undefined} ->
            %% Else try the config file
            DistOpts = rebar_state:get(State, dist_node, []),
            %% Pick the first one seen to support profile merges
            find_first_name(DistOpts);
        Res ->
            Res
    end.

find_first_name([]) -> {undefined, undefined};
find_first_name([{sname,Val}|_]) -> {undefined, Val};
find_first_name([{name,Val}|_]) -> {Val, undefined};
find_first_name([_|Opts]) -> find_first_name(Opts).

find_cookie_option(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    %% First try the CLI
    case proplists:get_value(setcookie, Opts) of
        undefined ->
            %% Else try the config file
            DistOpts = rebar_state:get(State, dist_node, []),
            proplists:get_value(setcookie, DistOpts, nocookie);
        Res ->
            Res
    end.
