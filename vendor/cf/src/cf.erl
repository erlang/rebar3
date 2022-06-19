%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@project-fifo.net>
%%% @copyright (C) 2015, Project-FiFo UG
%%% @doc
%%% Printing library for coloured console output, extends the format
%%% strings by adding ~! (forground) ~# (background) and ~_ (underline)
%%% terminal colours.
%%% @end
%%% Created : 22 Sep 2015 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(cf).


%% API exports
-export([format/1, format/2]).
-export([print/1,  print/2]).

%%====================================================================
%% API functions
%%====================================================================
%% @doc Prints a coloured string.
%% Effectively the same as io:format just takes the additional color
%% console text colour can be set by ~!**<colour>**.  ~#**<colour>**
%% will change the background. Both ~# only work with lowercase colours.
%% An uppercase letersindicate bold colours.
%% A `_` can be added after the ~! to make the text underlined.
%%
%% The colour can be one of:
%%
%%   !   - resets the output
%%   ^   - makes text bold
%%   x,X - black
%%   r,R - red
%%   g,G - greeen
%%   y,Y - yellow
%%   b,B - blue
%%   m,M - magenta
%%   c,C - cyan
%%   w,W - white
%%
%%  true color is supported by using
%%  ~!#<rr><gg><bb> as each as hex values so ~!#ff6402
%%
%%  the same can be done for the background by yusign ~##
%%
%%  The function will disable colours on non x term termials
%% @end
print(Fmt, Args) ->
    io:format(cfmt(Fmt), Args).

%% @doc Formates a coloured string
%% Arguments are the same as for print/2, just returns a string as
%% io_lib:format/2 does instead of printing it to stdout.
%% @end
format(Fmt, Args) ->
    io_lib:format(cfmt(Fmt), Args).


print(Fmt) ->
    print(Fmt, []).
format(Fmt) ->
    format(Fmt, []).

%%====================================================================
%% Internal functions
%%====================================================================


-define(NX,  "\033[0;30m").
-define(NR,  "\033[0;31m").
-define(NG,  "\033[0;32m").
-define(NY,  "\033[0;33m").
-define(NB,  "\033[0;34m").
-define(NM,  "\033[0;35m").
-define(NC,  "\033[0;36m").
-define(NW,  "\033[0;37m").
-define(BX,  "\033[1;30m").
-define(BR,  "\033[1;31m").
-define(BG,  "\033[1;32m").
-define(BY,  "\033[1;33m").
-define(BB,  "\033[1;34m").
-define(BM,  "\033[1;35m").
-define(BC,  "\033[1;36m").
-define(BW,  "\033[1;37m").
-define(U,   "\033[4m").
-define(B,   "\033[1m").
-define(BGX, "\033[40m").
-define(BGR, "\033[41m").
-define(BGG, "\033[42m").
-define(BGY, "\033[43m").
-define(BGB, "\033[44m").
-define(BGM, "\033[45m").
-define(BGC, "\033[46m").
-define(BGW, "\033[47m").
-define(R,   "\033[0m").
-define(CFMT(Char, Colour),
        cfmt_([$~, $!, Char | S], Enabled) -> [Colour | cfmt_(S, Enabled)];
        cfmt_([$~, $!,  $_, Char | S], Enabled) -> [Colour, ?U | cfmt_(S, Enabled)]).
-define(CFMT_BG(Char, Colour),
        cfmt_([$~, $#, Char | S], Enabled) -> [Colour | cfmt_(S, Enabled)]).
-define(CFMT_U(Char, Colour),
        cfmt_([$~, $_, Char | S], Enabled) -> [Colour | cfmt_(S, Enabled)]).

colour_term() ->
    case application:get_env(cf, colour_term) of
        {ok, V} ->
            V;
        undefined ->
            Term = os:getenv("TERM"),
            V = cf_term:has_color(Term),
            application:set_env(cf, colour_term, V),
            V
    end.

cfmt(S) ->
    cfmt(S, colour_term()).

cfmt(S, Enabled) ->
    lists:flatten(cfmt_(S, Enabled)).

cfmt_([$~, $!, $#, _R1, _R2, _G1, _G2, _B1, _B2 | S], false) ->
    cfmt_(S, false);
cfmt_([$~, $#, $#, _R1, _R2, _G1, _G2, _B1, _B2 | S], false) ->
    cfmt_(S, false);

cfmt_([$~, $!, $_, _C | S], false) ->
    cfmt_(S, false);
cfmt_([$~, $#, _C | S], false) ->
    cfmt_(S, false);
cfmt_([$~, $!, _C | S], false) ->
    cfmt_(S, false);

cfmt_([$~, $!, $#, R1, R2, G1, G2, B1, B2 | S], Enabled) ->
    R = list_to_integer([R1, R2], 16),
    G = list_to_integer([G1, G2], 16),
    B = list_to_integer([B1, B2], 16),
    ["\033[38;2;",
     integer_to_list(R), $;,
     integer_to_list(G), $;,
     integer_to_list(B), $m |
     cfmt_(S, Enabled)];

cfmt_([$~, $#, $#, R1, R2, G1, G2, B1, B2 | S], Enabled) ->
    R = list_to_integer([R1, R2], 16),
    G = list_to_integer([G1, G2], 16),
    B = list_to_integer([B1, B2], 16),
    ["\033[48;2;",
     integer_to_list(R), $;,
     integer_to_list(G), $;,
     integer_to_list(B), $m |
     cfmt_(S, Enabled)];

cfmt_([$~, $!, $_, $_ | S], Enabled) ->
    [?U |cfmt_(S, Enabled)];
cfmt_([$~,$!, $^ | S], Enabled) ->
    [?B | cfmt_(S, Enabled)];
cfmt_([$~,$!, $_, $^ | S], Enabled) ->
    [?U, ?B | cfmt_(S, Enabled)];

?CFMT($!, ?R);
?CFMT($x, ?NX);
?CFMT($X, ?BX);
?CFMT($r, ?NR);
?CFMT($R, ?BR);
?CFMT($g, ?NG);
?CFMT($G, ?BG);
?CFMT($y, ?NY);
?CFMT($Y, ?BY);
?CFMT($b, ?NB);
?CFMT($B, ?BB);
?CFMT($m, ?NM);
?CFMT($M, ?BM);
?CFMT($c, ?NC);
?CFMT($C, ?BC);
?CFMT($w, ?NW);
?CFMT($W, ?BW);

?CFMT_BG($x, ?BGX);
?CFMT_BG($r, ?BGR);
?CFMT_BG($g, ?BGG);
?CFMT_BG($y, ?BGY);
?CFMT_BG($b, ?BGB);
?CFMT_BG($m, ?BGM);
?CFMT_BG($c, ?BGC);
?CFMT_BG($w, ?BGW);

cfmt_([$~,$~ | S], Enabled) ->
    [$~,$~ | cfmt_(S, Enabled)];

cfmt_([C | S], Enabled) ->
    [C | cfmt_(S, Enabled)];

cfmt_([], false) ->
    "";
cfmt_([], _Enabled) ->
    ?R.
