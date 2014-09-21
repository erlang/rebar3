%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_config).

-export([consult/1
        ,consult_file/1]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec consult(file:name()) -> [any()].
consult(Dir) ->
    consult_file(filename:join(Dir, ?DEFAULT_CONFIG_FILE)).

-spec consult_file(file:name()) -> [any()].
consult_file(File) when is_binary(File) ->
    consult_file(binary_to_list(File));
consult_file(File) ->
    case filename:extension(File) of
        ".script" ->
            consult_and_eval(remove_script_ext(File), File);
        _ ->
            Script = File ++ ".script",
            case filelib:is_regular(Script) of
                true ->
                    {ok, Terms} = consult_and_eval(File, Script),
                    Terms;
                false ->
                    ?DEBUG("Consult config file ~p~n", [File]),
                    try_consult(File)
            end
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

consult_and_eval(File, Script) ->
    ?DEBUG("Evaluating config script ~p~n", [Script]),
    StateData = try_consult(File),
    file:script(Script, bs([{'CONFIG', StateData}, {'SCRIPT', Script}])).

remove_script_ext(F) ->
    filename:rootname(F, ".script").

try_consult(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            ?DEBUG("Consult config file ~p~n", [File]),
            Terms;
        {error, enoent} ->
            [];
        {error, Reason} ->
            ?ABORT("Failed to read config file ~s: ~p~n", [File, Reason])
    end.

bs(Vars) ->
    lists:foldl(fun({K,V}, Bs) ->
                        erl_eval:add_binding(K, V, Bs)
                end, erl_eval:new_bindings(), Vars).
