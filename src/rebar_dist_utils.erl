%%% Common functions to boot/stop distributed setups for
%%% the rebar3 script.
-module(rebar_dist_utils).
-export([either/3, short/2, long/2, find_options/1]).
-include("rebar.hrl").

%%%%%%%%%%%%%%%%%%
%%% PUBLIC API %%%
%%%%%%%%%%%%%%%%%%
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

short(Name, Opts) ->
    start(Name, shortnames, Opts).

long(Name, Opts) ->
    start(Name, longnames, Opts).

-spec find_options(rebar_state:state()) -> {Long, Short, Opts} when
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
    check_epmd(net_kernel:start([Name, Type])),
    setup_cookie(Opts).

check_epmd({error,{{shutdown, {_,net_kernel,{'EXIT',nodistribution}}},_}}) ->
    ?ERROR("Erlang Distribution failed, falling back to nonode@nohost. "
           "Verify that epmd is running and try again.",[]);
check_epmd(_) ->
    ok.

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
