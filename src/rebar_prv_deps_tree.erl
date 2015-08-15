-module(rebar_prv_deps_tree).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, tree).
-define(DEPS, [lock]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(
               State,
               providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 tree"},
                                 {short_desc, "Print dependency tree."},
                                 {desc, ""},
                                 {opts, [{verbose, $v, "verbose", undefined, "Print repo and branch/tag/ref for git and hg deps"}]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Verbose = proplists:get_value(verbose, Args, false),
    print_deps_tree(rebar_state:all_deps(State), Verbose),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Internal functions

print_deps_tree(SrcDeps, Verbose) ->
    D = lists:foldl(fun(App, Dict) ->
                            Name = rebar_app_info:name(App),
                            Vsn = rebar_app_info:original_vsn(App),
                            Source  = rebar_app_info:source(App),
                            Parent = rebar_app_info:parent(App),
                            dict:append_list(Parent, [{Name, Vsn, Source}], Dict)
                    end, dict:new(), SrcDeps),
    case dict:find(root, D) of
        {ok, Children} ->
            print_children(-1, lists:keysort(1, Children), D, Verbose);
        error ->
            none
    end.

print_children(_, [], _, _) ->
    ok;
print_children(Indent, [{Name, Vsn, Source} | Rest], Dict, Verbose) ->

    [io:format("|  ") || _ <- lists:seq(0, Indent, 2)],
    io:format("|- "),
    io:format("~s-~s (~s)~n", [Name, Vsn, type(Source, Verbose)]),
    case dict:find(Name, Dict) of
        {ok, Children} ->
            print_children(Indent+2, lists:keysort(1, Children), Dict, Verbose),
            print_children(Indent, Rest, Dict, Verbose);
        error ->
            print_children(Indent, Rest, Dict, Verbose)
    end.

type(Source, Verbose) when is_tuple(Source) ->
    case {element(1, Source), Verbose} of
        {pkg, _} ->
            "hex package";
        {Other, false} ->
            io_lib:format("~s repo", [Other]);
        {_, true} ->
            io_lib:format("~s", [element(2, Source)])
    end.
