%% @doc Completion file generator for bash
%% @end
-module(rebar_completion_bash).

-behavior(rebar_completion).

-export([generate/2]).

-define(str(N), integer_to_list(N)).

-spec generate([rebar_completion:cmpl_cmd()], rebar_completion:cmpl_opts()) -> iolist().
generate(Commands, #{shell:=bash}=CmplOpts) ->
    [rebar_completion:prelude(CmplOpts),
     io_lib:nl(),
     main(Commands, CmplOpts),
     complete(CmplOpts),
     io_lib:nl()].

cmd_clause(Cmd) ->
    nested_cmd_clause(Cmd, [], 1).

-spec nested_cmd_clause(rebar_completion:cmpl_cmd(), [string()], pos_integer()) -> iolist().
nested_cmd_clause(#{name:=Name,args:=Args,cmds:=Cmds},Prevs,Depth) ->
    Opts = [{S,L} || #{short:=S, long:=L} <- Args],
    {Shorts0,Longs0} = lists:unzip(Opts),
    Defined = fun(Opt) -> Opt =/= undefined end,
    Shorts = lists:filter(Defined, Shorts0),
    Longs = lists:filter(Defined, Longs0),
    SOpts = lists:join(" ",
                    [[$-,S] || S <- Shorts]),
    LOpts = lists:join(" ",
                    ["--"++L || L <- Longs]),
    Cmdsnvars = lists:join(" ",
                    [N || #{name:=N} <- Cmds]),
    IfBody = match_prev_if_body([Name | Prevs]),
    ClauseHead = ["elif [[ ",IfBody," ]] ; then\n"],
    ClauseBody = ["      sopts=\"",SOpts,"\"\n",
                "       lopts=\"",LOpts,"\"\n",
                "       cmdsnvars=\"",Cmdsnvars,"\"\n"],
    Nested = [nested_cmd_clause(C, [Name | Prevs], Depth+1) || C <- Cmds],
    [ClauseHead,ClauseBody,Nested].

match_prev_if_body([P | Rest]) ->
    lists:join(" && ",
        do_match_prev_if_body([P | Rest],1)).

do_match_prev_if_body([],_) ->
    [];
do_match_prev_if_body([P | Rest],Cnt) ->
    [["${prev",?str(Cnt),"} == ",P] | do_match_prev_if_body(Rest,Cnt+1)].

main(Commands, #{shell:=bash, aliases:=Aliases}) ->
    MaxDepth=cmd_depth(Commands,1,0),
    CmdNames = [Name || #{name:=Name} <- Commands],
    Triggers = ["rebar3" | Aliases],
    TriggerConds = [["${prev1} == \"",T,"\""] || T <- Triggers],
    Trigger = lists:join(" || ", TriggerConds),
    IfTriggerThen = ["if [[ ",Trigger," ]] ; then\n"],

    ["_rebar3_ref_idx() {\n",
    "   startc=$1\n",
    "   # is at least one of the two previous words a flag?\n",
    "    prev=${COMP_CWORD}-${startc}+",?str(MaxDepth-1),"\n",
    "    if [[ ${COMP_WORDS[${prev}]} == -* || ${COMP_WORDS[${prev}-1]} == -*  ]] ; then\n",
    "        startc=$((startc+1))\n",
    "        _rebar3_ref_idx $startc\n",
    "    fi\n",
    "    return $startc\n",
    "}\n",
    "\n",
    "_rebar3(){\n",
    "   local cur sopts lopts cmdsnvars refidx \n",
    "   local ",lists:join(" ", ["prev"++?str(I) || I <- lists:seq(1, MaxDepth)]),"\n",
    "   COMPREPLY=()\n",
    "   _rebar3_ref_idx ",?str(MaxDepth),"\n",
    "   refidx=$?\n",
    "   cur=\"${COMP_WORDS[COMP_CWORD]}\"\n",
    prev_definitions(MaxDepth,1),
    "       ",IfTriggerThen,
    "           sopts=\"-h -v\"\n"
    "           lopts=\"--help --version\"\n",
    "           cmdsnvars=\"",lists:join(" \\\n", CmdNames),"\"\n",
    "       ",[cmd_clause(Cmd) || Cmd <- Commands],
    "       fi\n",
    "    COMPREPLY=( $(compgen -W \"${sopts} ${lopts} ${cmdsnvars} \" -- ${cur}) )\n",
    "    if [ -n \"$COMPREPLY\" ] ; then\n",
    "        # append space if matched\n",
    "       COMPREPLY=\"${COMPREPLY} \"\n",
    "        # remove trailing space after equal sign\n",
    "        COMPREPLY=${COMPREPLY/%= /=}\n",
    "    fi\n",
    "    return 0\n",
    "}\n"].

prev_definitions(MaxDepth, Cnt) when (Cnt-1)=:=MaxDepth ->
    [];
prev_definitions(MaxDepth, Cnt) ->
    P = ["   prev",?str(Cnt),"=\"${COMP_WORDS[COMP_CWORD-${refidx}+",?str((MaxDepth-Cnt)),"]}\"\n"],
    [P | prev_definitions(MaxDepth,Cnt+1)].

cmd_depth([], _, Max) ->
    Max;
cmd_depth([#{cmds:=[]} | Rest],Depth,Max) ->
    cmd_depth(Rest,Depth,max(Depth,Max));
cmd_depth([#{cmds:=Cmds} | Rest],Depth, Max) ->
    D = cmd_depth(Cmds, Depth+1, Max),
    cmd_depth(Rest, Depth, max(D,Max));
cmd_depth([_ | Rest],Depth,Max) ->
    cmd_depth(Rest,Depth,max(Depth,Max)).

complete(#{shell:=bash, aliases:=Aliases}) ->
    Triggers = ["rebar3" | Aliases],
    [["complete -o nospace -F _rebar3 ", Trigger, "\n"] || Trigger <- Triggers].
