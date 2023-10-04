%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_verl).

-export([
         parse_requirement/1,
         parse_req_as_matchspec/1,
         valid_requirement/1,
         parse_as_matchable/1,
         compare/2,
         format_version/1
        ]).

-type version() :: ec_semver:semver().

-export_type([
              version/0
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

parse_req_as_matchspec(Req) ->
    #{matchspec := [{_Head, [Match], _}]} = parse_requirement(Req),
    {{{'$1', '$2', '$3'}, {'$4', '$5'}}, Match}.

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
        {ok, VerlVsn} ->
            to_matchable(VerlVsn);
        {error, Error} ->
            error({Error, Vsn})
    end.

to_matchable(#{major := Major, minor := Minor, patch := Patch, pre := Pre, build := _Build}) ->
    {{Major, Minor, Patch}, {Pre, true}}.

to_verl_vsn(Bin) when is_binary(Bin) ->
    {ok, Vsn} = verl:parse(Bin),
    Vsn;
to_verl_vsn({{Major, Minor, Patch}, {Pre, Build}}) ->
    #{major => Major, minor => Minor, patch => Patch, pre => Pre, build => Build};
to_verl_vsn(Map) when is_map(Map) ->
    Map.

compare(V1, V2) ->
    verl:compare(to_verl_vsn(V1), to_verl_vsn(V2)).

format_version(Vsn) ->
    #{major := Major, minor := Minor, patch := Patch, pre := Pre, build := Build} =
        to_verl_vsn(Vsn),

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

