-module(rebar_prv_new).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, new).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 new <template>"},
        {short_desc, "Create new project from templates."},
        {desc, info()},
        {opts, [{force, $f, "force", undefined, "overwrite existing files"}]}
    ]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:command_args(State) of
        ["help"] ->
            ?CONSOLE("Call `rebar3 new help <template>` for a detailed description~n", []),
            show_short_templates(rebar_templater:list_templates(State)),
            {ok, State};
        ["help", TemplateName] ->
            case lists:keyfind(TemplateName, 1, rebar_templater:list_templates(State)) of
                false -> ?CONSOLE("template not found.", []);
                Term -> show_template(Term)
            end,
            {ok, State};
        [TemplateName | Opts] ->
            case lists:keyfind(TemplateName, 1, rebar_templater:list_templates(State)) of
                false ->
                    ?CONSOLE("template not found.", []);
                _ ->
                    Force = is_forced(State),
                    ok = rebar_templater:new(TemplateName, parse_opts(Opts), Force, State)
            end,
            {ok, State};
        [] ->
            show_short_templates(rebar_templater:list_templates(State)),
            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

info() ->
    io_lib:format(
      "Create rebar3 project based on template and vars.~n"
      "~n"
      "Valid command line options:~n"
      "  <template> [var=foo,...]~n", []).

is_forced(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(force, Args) of
        undefined -> false;
        _ -> true
    end.

parse_opts([]) -> [];
parse_opts([Opt|Opts]) -> [parse_first_opt(Opt, "") | parse_opts1(Opts)].

parse_opts1([]) -> [];
parse_opts1([Opt|Opts]) -> [parse_opt(Opt, "") | parse_opts1(Opts)].

%% If the first argument meets no '=', we got a default 'name' argument
parse_first_opt("", Acc) -> {name, lists:reverse(Acc)};
parse_first_opt("="++Rest, Acc) -> parse_opt("="++Rest, Acc);
parse_first_opt([H|Str], Acc) -> parse_first_opt(Str, [H|Acc]).

%% We convert to atoms dynamically. Horrible in general, but fine in a
%% build system's templating tool.
parse_opt("", Acc) -> {list_to_atom(lists:reverse(Acc)), "true"};
parse_opt("="++Rest, Acc) -> {list_to_atom(lists:reverse(Acc)), Rest};
parse_opt([H|Str], Acc) -> parse_opt(Str, [H|Acc]).

show_short_templates(List) ->
    lists:map(fun show_short_template/1, lists:sort(List)).

show_short_template({Name, Type, _Location, Description, _Vars}) ->
    io:format("~s (~s): ~s~n",
              [Name,
               format_type(Type),
               format_description(Description)]).

show_template({Name, Type, Location, Description, Vars}) ->
    io:format("~s:~n"
              "\t~s~n"
              "\tDescription: ~s~n"
              "\tVariables:~n~s~n",
              [Name,
               format_type(Type, Location),
               format_description(Description),
               format_vars(Vars)]).

format_type(escript) -> "built-in";
format_type(file) -> "custom".

format_type(escript, _) ->
    "built-in template";
format_type(file, Loc) ->
    io_lib:format("custom template (~s)", [Loc]).

format_description(Description) ->
    case Description of
        undefined -> "<missing description>";
        _ -> Description
    end.

format_vars(Vars) -> [format_var(Var) || Var <- Vars].

format_var({Var, Default}) ->
    io_lib:format("\t\t~p=~p~n",[Var, Default]);
format_var({Var, Default, Doc}) ->
    io_lib:format("\t\t~p=~p (~s)~n", [Var, Default, Doc]).
