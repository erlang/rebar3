-module(rebar_prv_new).

-behaviour(rebar_provider).

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
    State1 = rebar_state:add_provider(State, #provider{name = ?PROVIDER,
                                                       provider_impl = ?MODULE,
                                                       bare = false,
                                                       deps = ?DEPS,
                                                       example = "rebar new <template>",
                                                       short_desc = "",
                                                       desc = info(create),
                                                       opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
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

info(create) ->
    io_lib:format(
       "Create skel based on template and vars.~n"
       "~n"
       "Valid command line options:~n"
       "  template= [var=foo,...]~n", []);
info(create_app) ->
    io_lib:format(
       "Create simple app skel.~n"
       "~n"
       "Valid command line options:~n"
       "  [appid=myapp]~n", []);
info(create_lib) ->
    io_lib:format(
       "Create simple lib skel.~n"
       "~n"
       "Valid command line options:~n"
       "  [libid=mylib]~n", []);
info(create_node) ->
    io_lib:format(
       "Create simple node skel.~n"
       "~n"
       "Valid command line options:~n"
       "  [nodeid=mynode]~n", []);
info(list_templates) ->
    io_lib:format("List available templates.~n", []).
