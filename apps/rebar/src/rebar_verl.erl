%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_verl).

-export([
         parse_requirement/1,
         valid_requirement/1,
         parse_version/1,
         format_version/1
        ]).

parse_requirement(Vsn) ->
    Vsn1 =
    case verl:parse(Vsn) of
        {ok, _} ->
            list_to_binary([<<"=> ">>, Vsn]);
        _ ->
            Vsn
    end,

    verl:parse_requirement(Vsn1).

valid_requirement(Vsn) ->
    case verl:parse(Vsn) of
        {ok, _} ->
            true;
        _ ->
            case verl:parse_requirement(Vsn) of
                {ok, _} ->
                    true;
                _ ->
                    false
            end
    end.

parse_version(Vsn) ->
    {ok, Res} = verl:parse(Vsn),
    Res.

format_version(#{major := Major, minor := Minor, patch := Patch, pre := Pre, build := Build}) ->
    Base = io_lib:format("~p.~p.~p", [Major, Minor, Patch]),
    WithPre = case Pre of
                [] ->
                    Base;
                _ ->
                    [Base, [$-, Pre]]
              end,
    WithBuild = case Build of
                    undefined ->
                        WithPre;
                    _ ->
                        [WithPre, io_lib:format("+~p", [Build])]
                end,
    WithBuild.

