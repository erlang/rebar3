-module(rebar_prv_new).

-behaviour(provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, new).
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
                                                               {example, "rebar new <template>"},
                                                               {short_desc, "Create new project from templates."},
                                                               {desc, info()},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:command_args(State) of
        [TemplateName] ->
            Template = list_to_atom(TemplateName),
            rebar_templater:new(Template, "", State),
            {ok, State};
        [TemplateName, DirName] ->
            Template = list_to_atom(TemplateName),
            rebar_templater:new(Template, DirName, State),
            {ok, State};
        [] ->
            {ok, State}
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

info() ->
    io_lib:format(
      "Create rebar project based on template and vars.~n"
      "~n"
      "Valid command line options:~n"
      "  template= [var=foo,...]~n", []).
