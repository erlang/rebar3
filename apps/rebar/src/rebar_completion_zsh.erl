%% @doc Completion file generator for zsh
%% @end
-module(rebar_completion_zsh).

-behavior(rebar_completion).

-export([generate/2]).

-spec generate([rebar_completion:cmpl_cmd()], rebar_completion:cmpl_opts()) -> iolist().
generate(Commands, #{shell:=zsh, aliases:=As}=CmplOpts) ->
    [rebar_completion:prelude(CmplOpts),
        io_lib:nl(),
        main(Commands, CmplOpts),
        io_lib:nl(),
        compdefs(["rebar3" | As])].

compdefs(As) ->
    [["compdef _rebar3 ", A, io_lib:nl()] || A <- As].

main(Commands, CmplOpts) ->
    H = #{short=>$s,
            long=>"help",
            help=>"rebar3 help",
            type=>boolean},
    V = #{short=>$v,
            long=>"version",
            help=>"Version of rebar3",
            type=>boolean},
    Rebar = #{name=>"rebar3",
            cmds=>Commands,
            args=>[H,V],
            help=>"Erlang build tool"},
    cmd_to_fun(Rebar, [], CmplOpts).

cmd_to_fun(#{name:=Name,cmds:=Nested}=Cmd, Prev, CmplOpts) ->
    ["function "++function_name(Prev, Name)++" {\n",
    "  local -a commands\n",
    io_lib:nl(),
    args(Cmd, CmplOpts),
    io_lib:nl(),
    nested_cmds(Nested,[Name|Prev],CmplOpts),
    "}\n",
    io_lib:nl(),
    [cmd_to_fun(C, [Name|Prev], CmplOpts) || C <- Nested]].

function_name(Prev,Name) ->
    ["_",
     string:join(
            lists:reverse([Name | Prev]),
            "_")].

nested_cmds([],_,_) ->
    io_lib:nl();
nested_cmds(Cmds,Prev,CmplOpts) ->
    ["  case $state in\n",
    "  cmnds)\n",
    "    commands=(\n",
    [["      ",cmd_str(Cmd, CmplOpts),io_lib:nl()] || Cmd <- Cmds],
    "    )\n",
    "    _describe \"command\" commands\n",
    "    ;;\n",
    "  esac\n",
    "\n",
    "  case \"$words[1]\" in\n",
    [cmd_call_case(Cmd, Prev, CmplOpts) || Cmd <- Cmds],
    "  esac\n"].

cmd_str(#{name:=N,help:=H}, _CmplOpts) ->
    ["\"",N,":",help(H),"\""].

cmd_call_case(#{name:=Name}, Prev, _CmplOpts) ->
    ["  ",Name,")\n",
    "    ",function_name(Prev, Name),"\n",
    "    ;;\n"].

args(#{args:=Args,cmds:=Cmds}, _CmplOpts) ->
    NoMore = (Args=:=[]) and (Cmds=:=[]),
    case NoMore of
        true ->
            "   _message 'no more arguments'\n";
        false ->
            ["  _arguments \\\n",
             [arg_str(Arg) || Arg <- Args],
            case Cmds of
                [] ->
                    "";
                _ ->
                    ["   \"1: :->cmnds\" \\\n",
                    "   \"*::arg:->args\"\n"]
            end]
    end.

arg_str(#{short:=undefined,long:=undefined,help:=H}) ->
    ["   ","'1:",H,":' \\\n"];
arg_str(#{help:=H}=Arg) ->
    ["   ",spec(Arg),"[",help(H),"]' \\\n"].

spec(#{short:=undefined,long:=L}) ->
    ["'(--",L,")--",L,""];
spec(#{short:=S,long:=undefined}) ->
    ["'(-",[S],")-",[S],""];
spec(#{short:=S,long:=L}) ->
    ["'(-",[S]," --",L,")'{-",[S],",--",L,"}'"].

help(undefined) -> "";
help(H) -> help_escape(H).

help_escape([]) ->
    [];
help_escape([40 | Rest]) ->
    ["\\(",help_escape(Rest)];
help_escape([41 | Rest]) ->
    ["\\)",help_escape(Rest)];
help_escape([91| Rest]) ->
    ["\\[",help_escape(Rest)];
help_escape([93| Rest]) ->
    ["\\]",help_escape(Rest)];
%% escaping single quotes by doubling them
help_escape([$' | Rest]) ->
    ["''",help_escape(Rest)];
help_escape([C | Rest]) ->
    [C | help_escape(Rest)].
