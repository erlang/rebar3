%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_verl).

-export([
         parse_requirement/1,
         valid_requirement/1,
         parse_as_matchable/1,
         format_version/1
        ]).

parse_requirement(Vsn) ->
    Vsn1 =
        case verl:parse(Vsn) of
            {ok, _} ->
                list_to_binary([<<">= ">>, Vsn]);
            _ ->
                Vsn
        end,

    case verl:parse_requirement(Vsn1) of
        {ok, Requirement} ->
            Requirement;
        {error, Error} ->
            error({Error, Vsn1})
    end.

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

parse_as_matchable(Vsn) when is_list(Vsn) ->
    parse_as_matchable(list_to_binary(Vsn));
parse_as_matchable(Vsn) ->
    case verl:parse(Vsn) of
        {ok, Res} ->
            verl:to_matchable(Res, true);
        {error, Error} ->
            error({Error, Vsn})
    end.

format_version(Binary) when is_binary(Binary) ->
    Binary;
format_version({Major, Minor, Patch, Pre, _}) ->
    format_version(#{major => Major, minor => Minor, patch => Patch, pre => Pre, build => undefined});
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

