-module(rlx_log).

-export([log/3]).

-spec log(atom(), atom() | string() | binary(), [term()]) -> ok.
log(_Level, Msg, Args) ->
    io:format(Msg, Args).
