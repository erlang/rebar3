-module(rebar_colour).

-export([format/1, format/2]).

-define(NR, "\033[0;31m").
-define(NG, "\033[0;32m").
-define(NY, "\033[0;33m").
-define(NB, "\033[0;34m").
-define(NM, "\033[0;35m").
-define(NC, "\033[0;36m").
-define(NW, "\033[0;37m").
-define(BR, "\033[1;31m").
-define(BG, "\033[1;32m").
-define(BY, "\033[1;33m").
-define(BB, "\033[1;34m").
-define(BM, "\033[1;35m").
-define(BC, "\033[1;36m").
-define(BW, "\033[1;37m").
-define(R,  "\033[0m").

format(Fmt) ->
    format(Fmt, []).
format(Fmt, Args) ->
    io_lib:format(cfmt(Fmt), Args).

%% FROM https://github.com/erlware/erlware_commons/blob/49bc69e35a282bde4a0a6a8f211b5f77d8585256/src/ec_cmd_log.erl
%% @doc Query the term enviroment
%% For reasons of simplicity, we don't parse terminal capabilities yet, although
%% a later version could do so. Rather, we provide a simple match-list of terminal
%% capabilities.
%% @end
-spec query_term_env() -> full | dumb.
query_term_env() ->
    term_capabilities(os:getenv("TERM")).

-spec term_capabilities(string()) -> full | dumb.
term_capabilities("xterm") -> full;
term_capabilities("dumb") -> dumb;
term_capabilities(_) -> full. %% Default to the backwards compatible version.


cfmt(S) ->
    cfmt(S, query_term_env() =:= full).

cfmt(S, Enabled) ->
    lists:flatten(cfmt_(S, ?R, Enabled)).

cfmt_([$~,$!,_C | S], Last, false) ->
    cfmt_(S, Last, false);

cfmt_([$~,$!,$! | S], _Last, Enabled) ->
    [?R | cfmt_(S, ?R, Enabled)];

cfmt_([$~,$!,$r | S], _Last, Enabled) ->
    [?NR | cfmt_(S, ?NR, Enabled)];
cfmt_([$~,$!,$R | S], _Last, Enabled) ->
    [?BR | cfmt_(S, ?BR, Enabled)];

cfmt_([$~,$!,$g | S], _Last, Enabled) ->
    [?NG | cfmt_(S, ?NG, Enabled)];
cfmt_([$~,$!,$G | S], _Last, Enabled) ->
    [?BG | cfmt_(S, ?BG, Enabled)];

cfmt_([$~,$!,$y | S], _Last, Enabled) ->
    [?NY | cfmt_(S, ?NY, Enabled)];
cfmt_([$~,$!,$Y | S], _Last, Enabled) ->
    [?BY | cfmt_(S, ?BY, Enabled)];

cfmt_([$~,$!,$b | S], _Last, Enabled) ->
    [?NB | cfmt_(S, ?NB, Enabled)];
cfmt_([$~,$!,$B | S], _Last, Enabled) ->
    [?BB | cfmt_(S, ?BB, Enabled)];

cfmt_([$~,$!,$m | S], _Last, Enabled) ->
    [?NM | cfmt_(S, ?NM, Enabled)];
cfmt_([$~,$!,$M | S], _Last, Enabled) ->
    [?BM | cfmt_(S, ?BM, Enabled)];

cfmt_([$~,$!,$c | S], _Last, Enabled) ->
    [?NC | cfmt_(S, ?NC, Enabled)];
cfmt_([$~,$!,$C | S], _Last, Enabled) ->
    [?BC | cfmt_(S, ?BC, Enabled)];

cfmt_([$~,$!,$w | S], _Last, Enabled) ->
    [?NW | cfmt_(S, ?NW, Enabled)];
cfmt_([$~,$!,$W | S], _Last, Enabled) ->
    [?BW | cfmt_(S, ?BW, Enabled)];

cfmt_([$~,$~ | S], Last, Enabled) ->
    [$~,$~ | cfmt_(S, Last, Enabled)];

cfmt_([$~,$s| S], Last, Enabled) ->
    [$~,$s, Last | cfmt_(S, Last, Enabled)];

cfmt_([C | S], Last, Enabled) ->
    [C | cfmt_(S, Last, Enabled)];

cfmt_([], _Last, false) ->
    "";
cfmt_([], _Last, _Enabled) ->
    ?R.
