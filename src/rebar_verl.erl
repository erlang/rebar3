-module(rebar_verl).

-include("rebar.hrl").

-export([parse_complex_version/1]).
-export([select_complex_version/1]).

select_complex_version(Version) ->
    case parse_complex_version(Version) of
        #{isComplex := true, versions := Versions} ->
            {complex, Versions};
        #{isComplex := false} ->
            Version
    end. 

parse_complex_version(Version) ->
    parse_complex_version(Version, #{versions => [<<>>], isComplex => false}).

parse_complex_version(<<>>, Acc) -> Acc;
parse_complex_version(<<" or ", Rest/binary>>, #{versions := Versions} = Acc) ->
    parse_complex_version(Rest, Acc#{versions => [<<>> | Versions], isComplex => true});
parse_complex_version(<<" and ", Rest/binary>>, #{versions := Versions} = Acc) ->
    parse_complex_version(Rest, Acc#{versions => [<<>> | Versions], isComplex => true});
parse_complex_version(<<" ", Rest/binary>>, Acc) ->
    parse_complex_version(Rest, Acc);
parse_complex_version(<<C:1/binary, Rest/binary>>, #{versions := [V | Versions]} = Acc) ->
    parse_complex_version(Rest, Acc#{versions => [<<V/binary, C/binary>> | Versions]}).

% is_and_or_or_in_version(Parsed) ->
%     lists:member('&&', Parsed) orelse lists:member('||', Parsed).

% get_highest_version(#{versions := Versions, parsed := Parsed}) ->
%     Highest = get_highest_version(Versions);


% % TODO >= 2.0.0 and < 2.1.0 would not be satisfied with this approach
% get_highest_version(Versions) when is_list(Versions) ->
%     lists:foldl(
%         fun(Version, Highest) ->
%             case (Highest =:= none orelse ec_semver:gt(Version, Highest)) of
%                 true ->
%                     Version;
%                 false ->
%                     Highest
%             end
%         end, none, Versions).



