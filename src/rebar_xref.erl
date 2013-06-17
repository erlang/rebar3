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
%% written by Torbjorn Tornkvist <tobbe@kreditor.se>, Daniel Luna
%% <daniel@lunas.se> and others.
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
    ok = xref:set_library_path(xref, code_path(Config)),

    xref:set_default(xref, [{warnings,
                             rebar_config:get(Config, xref_warnings, false)},
                            {verbose, rebar_config:is_verbose(Config)}]),

    {ok, _} = xref:add_directory(xref, "ebin"),

    %% Save the code path prior to doing anything
    OrigPath = code:get_path(),
    true = code:add_path(rebar_utils:ebin_dir()),

    %% Get list of xref checks we want to run
    ConfXrefChecks = rebar_config:get(Config, xref_checks,
                                  [exports_not_used,
                                   undefined_function_calls]),

    SupportedXrefs = [undefined_function_calls, undefined_functions,
                        locals_not_used, exports_not_used,
                        deprecated_function_calls, deprecated_functions],

    XrefChecks = sets:to_list(sets:intersection(sets:from_list(SupportedXrefs),
                                sets:from_list(ConfXrefChecks))),

    %% Run xref checks
    XrefNoWarn = xref_checks(XrefChecks),

    %% Run custom queries
    QueryChecks = rebar_config:get(Config, xref_queries, []),
    QueryNoWarn = lists:all(fun check_query/1, QueryChecks),

    %% Restore the original code path
    true = code:set_path(OrigPath),

    %% Stop xref
    stopped = xref:stop(xref),

    case lists:member(false, [XrefNoWarn, QueryNoWarn]) of
        true ->
            ?FAIL;
        false ->
            ok
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

xref_checks(XrefChecks) ->
    XrefWarnCount = lists:foldl(
        fun(XrefCheck, Acc) ->
            {ok, Results} = xref:analyze(xref, XrefCheck),
            FilteredResults =filter_xref_results(XrefCheck, Results),
            lists:foreach(fun(Res) -> display_xrefresult(XrefCheck, Res) end, FilteredResults),
            Acc + length(FilteredResults)
        end,
        0, XrefChecks),
    XrefWarnCount =:= 0.

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

code_path(Config) ->
    %% Slight hack to ensure that sub_dirs get properly included
    %% in code path for xref -- otherwise one gets a lot of undefined
    %% functions, even though those functions are present as part
    %% of compilation. H/t to @dluna. Long term we should tie more
    %% properly into the overall compile code path if possible.
    BaseDir = rebar_config:get_xconf(Config, base_dir),
    [P || P <- code:get_path() ++
              [filename:join(BaseDir, filename:join(SubDir, "ebin"))
               || SubDir <- rebar_config:get(Config, sub_dirs, [])],
          filelib:is_dir(P)].

%%
%% Ignore behaviour functions, and explicitly marked functions
%%
%% Functions can be ignored by using
%% -ignore_xref([{F, A}, {M, F, A}...]).

get_xref_ignorelist(Mod, XrefCheck) ->
    %% Get ignore_xref attribute and combine them in one list
    Attributes =
        try
            Mod:module_info(attributes)
        catch
            _Class:_Error -> []
        end,

    Ignore_xref = keyall(ignore_xref, Attributes),

    Behaviour_callbacks = case XrefCheck of
            exports_not_used -> [B:behaviour_info(callbacks) || B <- keyall(behaviour, Attributes)];
            _ -> []
    end,

    % And create a flat {M,F,A} list
    lists:foldl(
        fun(El,Acc) ->
            case El of
                {F, A} -> [{Mod,F,A} | Acc];
                {M, F, A} -> [{M,F,A} | Acc]
            end
        end, [],lists:flatten([Ignore_xref, Behaviour_callbacks])).

keyall(Key, List) ->
    lists:flatmap(fun({K, L}) when Key =:= K -> L; (_) -> [] end, List).

parse_xref_result(XrefResult) ->
    case XrefResult of
        {_, MFAt} -> MFAt;
        MFAt -> MFAt
    end.

filter_xref_results(XrefCheck, XrefResults) ->
    SearchModules = lists:usort(lists:map(
        fun(Res) ->
            case Res of
                {Mt,_Ft,_At} -> Mt;
                {{Ms,_Fs,_As},{_Mt,_Ft,_At}} -> Ms;
                _ -> undefined
            end
        end, XrefResults)),

    Ignores = lists:flatten([
        get_xref_ignorelist(Module,XrefCheck) || Module <- SearchModules]),

    [Result || Result <- XrefResults,
        not lists:member(parse_xref_result(Result),Ignores)].

display_xrefresult(Type, XrefResult) ->
    { Source, SMFA, TMFA } = case XrefResult of
        {MFASource, MFATarget} ->
            {format_mfa_source(MFASource), format_mfa(MFASource),
                format_mfa(MFATarget)};
        MFATarget ->
            {format_mfa_source(MFATarget), format_mfa(MFATarget),
                undefined}
    end,
    case Type of
        undefined_function_calls ->
            ?CONSOLE("~sWarning: ~s calls undefined function ~s (Xref)\n",
                [Source, SMFA, TMFA]);
        undefined_functions ->
            ?CONSOLE("~sWarning: ~s is undefined function (Xref)\n",
                [Source, SMFA]);
        locals_not_used ->
            ?CONSOLE("~sWarning: ~s is unused local function (Xref)\n",
                [Source, SMFA]);
        exports_not_used ->
            ?CONSOLE("~sWarning: ~s is unused export (Xref)\n",
                [Source, SMFA]);
        deprecated_function_calls ->
            ?CONSOLE("~sWarning: ~s calls deprecated function ~s (Xref)\n",
                [Source, SMFA, TMFA]);
        deprecated_functions ->
            ?CONSOLE("~sWarning: ~s is deprecated function (Xref)\n",
                [Source, SMFA]);
        Other ->
            ?CONSOLE("~sWarning: ~s - ~s xref check: ~s (Xref)\n",
                [Source, SMFA, TMFA, Other])
    end.

format_mfa({M, F, A}) ->
    ?FMT("~s:~s/~w", [M, F, A]).

format_mfa_source(MFA) ->
    case find_mfa_source(MFA) of
        {module_not_found, function_not_found} -> "";
        {Source, function_not_found} -> ?FMT("~s: ", [Source]);
        {Source, Line} -> ?FMT("~s:~w: ", [Source, Line])
    end.

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
    case code:get_object_code(M) of
        error -> {module_not_found, function_not_found};
        {M, Bin, _} -> find_function_source(M,F,A,Bin)
    end.

find_function_source(M, F, A, Bin) ->
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
