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
%%
%% -------------------------------------------------------------------

%% -------------------------------------------------------------------
%% This module borrows heavily from http://github.com/etnt/exrefcheck project as
%% written by Torbjorn Tornkvist <tobbe@kreditor.se>, Daniel Luna and others.
%% -------------------------------------------------------------------
-module(rebar_xref).

-include("rebar.hrl").

-export([xref/2]).

%% ===================================================================
%% Public API
%% ===================================================================

xref(Config, _) ->
    %% Spin up xref
    {ok, _} = xref:start(xref),
    ok = xref:set_library_path(xref, code_path()),

    xref:set_default(xref, [{warnings,
                             rebar_config:get(Config, xref_warnings, false)},
                            {verbose, rebar_config:is_verbose(Config)}]),

    {ok, _} = xref:add_directory(xref, "ebin"),

    %% Save the code path prior to doing anything
    OrigPath = code:get_path(),
    true = code:add_path(filename:join(rebar_utils:get_cwd(), "ebin")),

    %% Get list of xref checks we want to run
    XrefChecks = rebar_config:get(Config, xref_checks,
                                  [exports_not_used,
                                   undefined_function_calls]),

    %% Look for exports that are unused by anything
    ExportsNoWarn =
        case lists:member(exports_not_used, XrefChecks) of
            true ->
                check_exports_not_used();
            false ->
                true
        end,

    %% Look for calls to undefined functions
    UndefNoWarn =
        case lists:member(undefined_function_calls, XrefChecks) of
            true ->
                check_undefined_function_calls();
            false ->
                true
        end,

    %% Run custom queries
    QueryChecks = rebar_config:get(Config, xref_queries, []),
    QueryNoWarn = lists:all(fun check_query/1, QueryChecks),

    %% Restore the original code path
    true = code:set_path(OrigPath),

    %% Stop xref
    stopped = xref:stop(xref),

    case lists:member(false, [ExportsNoWarn, UndefNoWarn, QueryNoWarn]) of
        true ->
            ?ABORT;
        false ->
            ok
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

check_exports_not_used() ->
    {ok, UnusedExports0} = xref:analyze(xref, exports_not_used),
    UnusedExports = filter_away_ignored(UnusedExports0),

    %% Report all the unused functions
    display_mfas(UnusedExports, "is unused export (Xref)"),
    UnusedExports =:= [].

check_undefined_function_calls() ->
    {ok, UndefinedCalls0} = xref:analyze(xref, undefined_function_calls),
    UndefinedCalls =
        [{find_mfa_source(Caller), format_fa(Caller), format_mfa(Target)}
         || {Caller, Target} <- UndefinedCalls0],

    lists:foreach(
      fun({{Source, Line}, FunStr, Target}) ->
              ?CONSOLE("~s:~w: Warning ~s calls undefined function ~s\n",
                       [Source, Line, FunStr, Target])
      end, UndefinedCalls),
    UndefinedCalls =:= [].

check_query({Query, Value}) ->
    {ok, Answer} = xref:q(xref, Query),
    case Answer =:= Value of
        false ->
            ?CONSOLE("Query ~s~n answer ~p~n did not match ~p~n",
                     [Query, Answer, Value]),
            false;
        _     ->
            true
    end.

code_path() ->
    [P || P <- code:get_path(),
          filelib:is_dir(P)] ++ [filename:join(rebar_utils:get_cwd(), "ebin")].

%%
%% Ignore behaviour functions, and explicitly marked functions
%%
filter_away_ignored(UnusedExports) ->
    %% Functions can be ignored by using
    %% -ignore_xref([{F, A}, ...]).

    %% Setup a filter function that builds a list of behaviour callbacks and/or
    %% any functions marked to ignore. We then use this list to mask any
    %% functions marked as unused exports by xref
    F = fun(Mod) ->
                Attrs  = kf(attributes, Mod:module_info()),
                Ignore = kf(ignore_xref, Attrs),
                Callbacks =
                    [B:behaviour_info(callbacks) || B <- kf(behaviour, Attrs)],
                [{Mod, F, A} || {F, A} <- Ignore ++ lists:flatten(Callbacks)]
        end,
    AttrIgnore =
        lists:flatten(
          lists:map(F, lists:usort([M || {M, _, _} <- UnusedExports]))),
    [X || X <- UnusedExports, not lists:member(X, AttrIgnore)].


kf(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Value} ->
            Value;
        false ->
            []
    end.

display_mfas([], _Message) ->
    ok;
display_mfas([{_Mod, Fun, Args} = MFA | Rest], Message) ->
    {Source, Line} = find_mfa_source(MFA),
    ?CONSOLE("~s:~w: Warning: function ~s/~w ~s\n",
             [Source, Line, Fun, Args, Message]),
    display_mfas(Rest, Message).

format_mfa({M, F, A}) ->
    ?FMT("~s:~s/~w", [M, F, A]).

format_fa({_M, F, A}) ->
    ?FMT("~s/~w", [F, A]).

%%
%% Extract an element from a tuple, or undefined if N > tuple size
%%
safe_element(N, Tuple) ->
    case catch(element(N, Tuple)) of
        {'EXIT', {badarg, _}} ->
            undefined;
        Value ->
            Value
    end.


%%
%% Given a MFA, find the file and LOC where it's defined. Note that
%% xref doesn't work if there is no abstract_code, so we can avoid
%% being too paranoid here.
%%
find_mfa_source({M, F, A}) ->
    {M, Bin, _} = code:get_object_code(M),
    AbstractCode = beam_lib:chunks(Bin, [abstract_code]),
    {ok, {M, [{abstract_code, {raw_abstract_v1, Code}}]}} = AbstractCode,
    %% Extract the original source filename from the abstract code
    [{attribute, 1, file, {Source, _}} | _] = Code,
    %% Extract the line number for a given function def
    Fn = [E || E <- Code,
               safe_element(1, E) == function,
               safe_element(3, E) == F,
               safe_element(4, E) == A],
    case Fn of
        [{function, Line, F, _, _}] -> {Source, Line};
        %% do not crash if functions are exported, even though they
        %% are not in the source.
        %% parameterized modules add new/1 and instance/1 for example.
        [] -> {Source, function_not_found}
    end.
