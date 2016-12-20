-module(rebar_opts).

-export([get/2,
         get/3,
         set/3,

         erl_opts/1,

         apply_overrides/3,
         add_to_profile/3,
         merge_opts/2,
         merge_opts/3]).

-include("rebar.hrl").

get(Opts, Key) ->
    {ok, Value} = dict:find(Key, Opts),
    Value.

get(Opts, Key, Default) ->
    case dict:find(Key, Opts) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.

set(Opts, Key, Value) ->
    dict:store(Key, Value, Opts).

%% @doc Return list of erl_opts
-spec erl_opts(rebar_dict()) -> list().
erl_opts(Opts) ->
    RawErlOpts = filter_defines(?MODULE:get(Opts, erl_opts, []), []),
    Defines = [{d, list_to_atom(D)} ||
                  D <- ?MODULE:get(Opts, defines, [])],
    AllOpts = Defines ++ RawErlOpts,
    case proplists:is_defined(no_debug_info, AllOpts) of
        true ->
            [O || O <- AllOpts, O =/= no_debug_info];
        false ->
            [debug_info|AllOpts]
    end.

apply_overrides(Opts, Name, Overrides) ->
    %% Inefficient. We want the order we get here though.
    Opts1 = lists:foldl(fun({override, O}, OptsAcc) ->
                                 lists:foldl(fun({deps, Value}, OptsAcc1) ->
                                                     set(OptsAcc1, {deps,default}, Value);
                                                ({Key, Value}, OptsAcc1) ->
                                                     set(OptsAcc1, Key, Value)
                                             end, OptsAcc, O);
                            (_, OptsAcc) ->
                                 OptsAcc
                         end, Opts, Overrides),

    Opts2 = lists:foldl(fun({override, N, O}, OptsAcc) when N =:= Name ->
                                 lists:foldl(fun({deps, Value}, OptsAcc1) ->
                                                     set(OptsAcc1, {deps,default}, Value);
                                                ({Key, Value}, OptsAcc1) ->
                                                     set(OptsAcc1, Key, Value)
                                             end, OptsAcc, O);
                            (_, OptsAcc) ->
                                 OptsAcc
                         end, Opts1, Overrides),

    lists:foldl(fun({add, N, O}, OptsAcc) when N =:= Name ->
                        lists:foldl(fun({deps, Value}, OptsAcc1) ->
                                            OldValue = ?MODULE:get(OptsAcc1, {deps,default}, []),
                                            set(OptsAcc1, {deps,default}, Value++OldValue);
                                       ({Key, Value}, OptsAcc1) ->
                                            OldValue = ?MODULE:get(OptsAcc1, Key, []),
                                            set(OptsAcc1, Key, Value++OldValue)
                                    end, OptsAcc, O);
                   (_, OptsAcc) ->
                        OptsAcc
                end, Opts2, Overrides).

add_to_profile(Opts, Profile, KVs) when is_atom(Profile), is_list(KVs) ->
    Profiles = ?MODULE:get(Opts, profiles, []),
    ProfileOpts = dict:from_list(proplists:get_value(Profile, Profiles, [])),
    NewOpts = merge_opts(Profile, dict:from_list(KVs), ProfileOpts),
    NewProfiles = [{Profile, dict:to_list(NewOpts)}|lists:keydelete(Profile, 1, Profiles)],
    set(Opts, profiles, NewProfiles).

merge_opts(Profile, NewOpts, OldOpts) ->
    Opts = merge_opts(NewOpts, OldOpts),

    Opts2 = case dict:find(plugins, NewOpts) of
        {ok, Value} ->
            dict:store({plugins, Profile}, Value, Opts);
        error ->
            Opts
    end,

    case dict:find(deps, NewOpts) of
        {ok, Value2} ->
            dict:store({deps, Profile}, Value2, Opts2);
        error ->
            Opts2
    end.

merge_opts(NewOpts, OldOpts) ->
    dict:merge(fun merge_opt/3, NewOpts, OldOpts).

%% Internal functions

%%
%% Function for dict:merge/3 (in merge_opts/2) to merge options by priority.
%%
merge_opt(deps, _NewValue, OldValue) ->
    OldValue;
merge_opt({deps, _}, NewValue, _OldValue) ->
    NewValue;
merge_opt(plugins, NewValue, _OldValue) ->
    NewValue;
merge_opt({plugins, _}, NewValue, _OldValue) ->
    NewValue;
merge_opt(profiles, NewValue, OldValue) ->
    dict:to_list(merge_opts(dict:from_list(NewValue), dict:from_list(OldValue)));
merge_opt(erl_first_files, Value, Value) ->
    Value;
merge_opt(erl_first_files, NewValue, OldValue) ->
    OldValue ++ NewValue;
merge_opt(mib_first_files, Value, Value) ->
    Value;
merge_opt(mib_first_files, NewValue, OldValue) ->
    OldValue ++ NewValue;
merge_opt(relx, NewValue, OldValue) ->
    rebar_utils:tup_umerge(OldValue, NewValue);
merge_opt(Key, NewValue, OldValue)
    when Key == erl_opts; Key == eunit_compile_opts; Key == ct_compile_opts ->
    merge_erl_opts(lists:reverse(OldValue), NewValue);
merge_opt(_Key, NewValue, OldValue) when is_list(NewValue) ->
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
            rebar_utils:tup_umerge(NewValue, OldValue)
    end;
merge_opt(_Key, NewValue, _OldValue) ->
    NewValue.

%%
%% Merge Erlang compiler options such that the result
%%  a)  Doesn't contain duplicates.
%%  b)  Resulting options are ordered by increasing precedence as expected by
%%      the compiler.
%% The first parameter is the lower precedence options, in reverse order, to
%% be merged with the higher-precedence options in the second parameter.
%%
merge_erl_opts([Opt | Opts], []) ->
    merge_erl_opts(Opts, [Opt]);
merge_erl_opts([Opt | Opts], Merged) ->
    case lists:member(Opt, Merged) of
        true ->
            merge_erl_opts(Opts, Merged);
        _ ->
            merge_erl_opts(Opts, [Opt | Merged])
    end;
merge_erl_opts([], Merged) ->
    Merged.

%%
%% Filter a list of erl_opts platform_define options such that only
%% those which match the provided architecture regex are returned.
%%
filter_defines([], Acc) ->
    lists:reverse(Acc);
filter_defines([{platform_define, ArchRegex, Key} | Rest], Acc) ->
    case rebar_utils:is_arch(ArchRegex) of
        true ->
            filter_defines(Rest, [{d, Key} | Acc]);
        false ->
            filter_defines(Rest, Acc)
    end;
filter_defines([{platform_define, ArchRegex, Key, Value} | Rest], Acc) ->
    case rebar_utils:is_arch(ArchRegex) of
        true ->
            filter_defines(Rest, [{d, Key, Value} | Acc]);
        false ->
            filter_defines(Rest, Acc)
    end;
filter_defines([Opt | Rest], Acc) ->
    filter_defines(Rest, [Opt | Acc]).
