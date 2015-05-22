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
        ,consult/2
        ,consult_file/1
        ,consult_file/2
        ,format_error/1

        ,merge_locks/2
        ,merge_opts/2]).

-export_type([
    consult_opt/0
]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-type consult_opt()     :: raw.
-type consult_opts()    :: [ consult_opt() ].

-spec consult(file:name()) -> [any()].
consult(Dir) ->
    consult(Dir, []).

-spec consult(file:name(), consult_opts()) -> [any()].
consult(Dir, Options) ->
    consult_file(filename:join(Dir, ?DEFAULT_CONFIG_FILE), Options).

-spec consult_file(file:name()) -> [any()].
consult_file(File) ->
    consult_file(File, []).

-spec consult_file(file:name(), consult_opts()) -> [any()].
consult_file(File, Opts) when is_binary(File) ->
    consult_file(binary_to_list(File), Opts);
consult_file(File, Opts) ->
    ResultTerms =
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
                        try_consult(File)
                end
        end,
    case proplists:get_bool(raw, Opts) of
        true ->
            ResultTerms;
        false ->
            process_config(ResultTerms, filename:dirname(File))
    end.

%% no lockfile
merge_locks(Config, []) ->
    Config;
%% empty lockfile
merge_locks(Config, [[]]) ->
    Config;
%% lockfile with entries
merge_locks(Config, [Locks]) ->
    ConfigDeps = proplists:get_value(deps, Config, []),
    %% We want the top level deps only from the lock file.
    %% This ensures deterministic overrides for configs.
    %% Then check if any new deps have been added to the config
    %% since it was locked.
    Deps = [X || X <- Locks, element(3, X) =:= 0],
    NewDeps = find_newly_added(ConfigDeps, Locks),
    [{{locks, default}, Locks}, {{deps, default}, NewDeps++Deps} | Config].

merge_opts(NewOpts, OldOpts) ->
    dict:merge(fun(deps, NewValue, _OldValue) ->
        NewValue;
        ({deps, _}, NewValue, _OldValue) ->
            NewValue;
        (profiles, NewValue, OldValue) ->
            dict:to_list(merge_opts(dict:from_list(NewValue), dict:from_list(OldValue)));
        (_Key, NewValue, OldValue) when is_list(NewValue) ->
            case io_lib:printable_list(NewValue) of
                true when NewValue =:= [] ->
                    case io_lib:printable_list(OldValue) of
                        true ->
                            NewValue;
                        false ->
                            OldValue
                    end;
                true ->
                    NewValue;
                false ->
                    rebar_utils:tup_umerge(rebar_utils:tup_sort(NewValue)
                        ,rebar_utils:tup_sort(OldValue))
            end;
        (_Key, NewValue, _OldValue) ->
            NewValue
    end, NewOpts, OldOpts).

format_error({bad_dep_name, Dep}) ->
    io_lib:format("Dependency name must be an atom, instead found: ~p", [Dep]).

%% ===================================================================
%% Internal functions
%% ===================================================================

consult_and_eval(File, Script) ->
    ?DEBUG("Evaluating config script ~p", [Script]),
    StateData = try_consult(File),
    file:script(Script, bs([{'CONFIG', StateData}, {'SCRIPT', Script}])).

remove_script_ext(F) ->
    filename:rootname(F, ".script").

try_consult(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            Terms;
        {error, enoent} ->
            [];
        {error, Reason} ->
            ?ABORT("Failed to read config file ~s:~n ~p", [File, Reason])
    end.

process_config(Terms, BaseDir) ->
    do_process(Terms, dict:new(), BaseDir).

do_process([], Acc, _BaseDir) ->
    dict:to_list(Acc);
do_process([{include_config, Path} | Rest], Acc, BaseDir) ->
    FullPath = filename:absname_join(BaseDir, Path),
    SubconfigTerms = consult_file(FullPath),
    Tmp = merge_opts(dict:from_list(SubconfigTerms), Acc),
    do_process(Rest, Tmp, BaseDir);

do_process([Something | Rest], Acc, BaseDir) ->
    Tmp = merge_opts(dict:from_list([Something]), Acc),
    do_process(Rest, Tmp, BaseDir).


bs(Vars) ->
    lists:foldl(fun({K,V}, Bs) ->
                        erl_eval:add_binding(K, V, Bs)
                end, erl_eval:new_bindings(), Vars).

%% Find deps that have been added to the config after the lock was created
find_newly_added(ConfigDeps, LockedDeps) ->
    rebar_utils:filtermap(fun(Dep) when is_tuple(Dep) ->
                                  check_newly_added(element(1, Dep), LockedDeps);
                             (Dep) ->
                                  check_newly_added(Dep, LockedDeps)
                          end, ConfigDeps).

check_newly_added(Dep, LockedDeps) when is_atom(Dep) ->
    NewDep = ec_cnv:to_binary(Dep),
    case lists:keyfind(NewDep, 1, LockedDeps) of
        false ->
            true;
        Match ->
            case element(3, Match) of
                0 ->
                    true;
                _ ->
                    ?WARN("Newly added dep ~s is locked at a lower level. "
                          "If you really want to unlock it, use 'rebar3 upgrade ~s'",
                          [NewDep, NewDep]),
                    false
            end
    end;
check_newly_added(Dep, _) ->
    throw(?PRV_ERROR({bad_dep_name, Dep})).
