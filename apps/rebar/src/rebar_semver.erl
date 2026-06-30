%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% SPDX-FileCopyrightText: Copyright 2015-2026 Rebar3 and its contributors
%%
%% SPDX-FileCopyrightText: Copyright 2026 Dipl. Phys. Peer Stritzinger GmbH
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

-module(rebar_semver).
-export([
    parse_version/1,
    parse_constraint/1,
    is_valid/1,
    is_prerelease_or_build/1,
    match/2,
    cmp/2,
    format/1
]).

-export_type([version/0, constraint/0]).

%% SPDX-SnippetBegin
%% SPDX-License-Identifier: MIT
%%
%% SPDX-SnippetCopyrightText: 2011 Erlware, LLC
%%
%% SPDX-FileCopyrightText: 2026 Dipl. Phys. Peer Stritzinger GmbH
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
%% SPDX-SnippetComment: Types derived from ec_semver.erl in erlware_commons.

-type version_element() :: non_neg_integer() | binary().

-type major_minor_patch_minpatch() ::
        version_element()
      | {version_element(), version_element()}
      | {version_element(), version_element(), version_element()}
      | {version_element(), version_element(),
         version_element(), version_element()}.

-type alpha_part() :: integer() | binary() | string().
-type alpha_info() :: {PreRelease::[alpha_part()],
                       BuildVersion::[alpha_part()]}.

-type version() :: {major_minor_patch_minpatch(), alpha_info()}.
-type constraint() :: fun((any_version()) -> boolean()).

-type version_string() :: string() | binary().

-type any_version() :: version_string() | version().

%% SPDX-SnippetEnd

-spec parse_version(Version) -> {ok, Parsed} | Error when
      Version :: any_version(),
      Parsed  :: version(),
      Error   :: {error, {invalid_vsn, Version}}.
parse_version(Version) ->
    case parse(Version) of
        {C, _} when is_binary(C) ->
            {error, {invalid_vsn, Version}};
        {{A, B}, _} when is_binary(A); is_binary(B) ->
            {error, {invalid_vsn, Version}};
        {{A, B, C}, _} when is_binary(A); is_binary(B); is_binary(C) ->
            {error, {invalid_vsn, Version}};
        {{A, B, C, D}, _} when is_binary(A); is_binary(B); is_binary(C); is_binary(D) ->
            {error, {invalid_vsn, Version}};
        Parsed ->
            {ok, Parsed}
    end.

-spec parse_constraint(undefined | binary()) -> {ok, constraint()} | {error, {invalid_vsn, binary()}}.
parse_constraint(undefined) ->
    {ok, fun (_) -> true end};
parse_constraint(Constraint) ->
    case parse_constraint_ors(binary:split(Constraint, [<<" or ">>, <<"||">>], [global]), []) of
        nomatch -> {error, {invalid_vsn, Constraint}};
        Match -> {ok, Match}
    end.

-spec parse_constraint_ors(Patterns, Constraints) -> Result when
      Patterns    :: [binary()],
      Constraints :: [constraint()],
      Result      :: nomatch | constraint().
parse_constraint_ors([], []) -> nomatch;
parse_constraint_ors([], [Match]) -> Match;
parse_constraint_ors([], Matchers) ->
    fun (Vsn) -> lists:any(fun (Match) -> Match(Vsn) end, Matchers) end;
parse_constraint_ors([And|Ors], Matchers) ->
    case parse_constraint_ands(binary:split(And, [<<" and ">>, <<"&&">>], [global]), []) of
        nomatch -> nomatch;
        Match -> parse_constraint_ors(Ors, [Match|Matchers])
    end.

-spec parse_constraint_ands(Patterns, Constraints) -> Result when
      Patterns    :: [binary()],
      Constraints :: [constraint()],
      Result      :: nomatch | constraint().
parse_constraint_ands([], []) -> nomatch;
parse_constraint_ands([], [Match]) -> Match;
parse_constraint_ands([], Matchers) ->
    fun (Vsn) -> lists:all(fun (Match) -> Match(Vsn) end, Matchers) end;
parse_constraint_ands([Pattern|Ands], Matchers) ->
    case parse_constraint_pattern(Pattern) of
        nomatch -> nomatch;
        Match -> parse_constraint_ands(Ands, [Match|Matchers])
    end.

-spec parse_constraint_pattern(binary()) -> nomatch | constraint().
parse_constraint_pattern(<<" ", Vsn/binary>>) ->
    parse_constraint_pattern(Vsn);
parse_constraint_pattern(<<"==", Vsn/binary>>) ->
    parse_version_constraint(Vsn, fun eql/2);
parse_constraint_pattern(<<">=", Vsn/binary>>) ->
    parse_version_constraint(Vsn, fun gte/2);
parse_constraint_pattern(<<"<=", Vsn/binary>>) ->
    parse_version_constraint(Vsn, fun lte/2);
parse_constraint_pattern(<<">", Vsn/binary>>) ->
    parse_version_constraint(Vsn, fun gt/2);
parse_constraint_pattern(<<"<", Vsn/binary>>) ->
    parse_version_constraint(Vsn, fun lt/2);
parse_constraint_pattern(<<"~>", Vsn/binary>>) ->
    parse_version_constraint(Vsn, fun pes/2);
parse_constraint_pattern(Vsn) ->
    parse_version_constraint(Vsn, fun eql/2).

-spec parse_version_constraint(Vsn, Match) -> Result when
      Vsn    :: any_version(),
      Match  :: fun((any_version(), any_version()) -> boolean()),
      Result :: nomatch | constraint().
parse_version_constraint(Vsn, Match) ->
    case parse_version(string:trim(Vsn)) of
        {ok, Bound} -> fun (V) -> Match(V, Bound) end;
        _ -> nomatch
    end.

-spec is_valid(undefined | binary()) -> boolean().
is_valid(Vsn) ->
    case parse_constraint(Vsn) of
        {ok, _} -> true;
        _ -> false
    end.

-spec is_prerelease_or_build(undefined | binary()) -> boolean().
is_prerelease_or_build(undefined) -> false;
is_prerelease_or_build(Vsn) ->
    binary:match(Vsn, [<<"-">>, <<"+">>]) =/= nomatch.

-spec match(any_version(), constraint()) -> boolean().
match(Version, Constraint) ->
    Constraint(Version).

-spec cmp(any_version(), any_version()) -> gt | lt | eq.
cmp(Vsn1, Vsn2) ->
    case gt(Vsn1, Vsn2) of
        true -> gt;
        false ->
            case lt(Vsn1, Vsn2) of
                true -> lt;
                false -> eq
            end
    end.

%% SPDX-SnippetBegin
%% SPDX-License-Identifier: MIT
%% SPDX-SnippetCopyrightText: 2011 Erlware, LLC
%% SPDX-FileCopyrightText: 2026 Dipl. Phys. Peer Stritzinger GmbH
%% SPDX-SnippetComment: Functions derived from ec_semver.erl in erlware_commons.

-spec format(version()) -> binary().
format({Maj, {AlphaPart, BuildPart}})
  when erlang:is_integer(Maj);
       erlang:is_binary(Maj) ->
    iolist_to_binary([format_version_part(Maj),
                      format_vsn_rest(<<"-">>, AlphaPart),
                      format_vsn_rest(<<"+">>, BuildPart)]);
format({{Maj, Min}, {AlphaPart, BuildPart}}) ->
    iolist_to_binary([format_version_part(Maj), ".",
                      format_version_part(Min),
                      format_vsn_rest(<<"-">>, AlphaPart),
                      format_vsn_rest(<<"+">>, BuildPart)]);
format({{Maj, Min, Patch}, {AlphaPart, BuildPart}}) ->
    iolist_to_binary([format_version_part(Maj), ".",
                      format_version_part(Min), ".",
                      format_version_part(Patch),
                      format_vsn_rest(<<"-">>, AlphaPart),
                      format_vsn_rest(<<"+">>, BuildPart)]);
format({{Maj, Min, Patch, MinPatch}, {AlphaPart, BuildPart}}) ->
    iolist_to_binary([format_version_part(Maj), ".",
                      format_version_part(Min), ".",
                      format_version_part(Patch), ".",
                      format_version_part(MinPatch),
                      format_vsn_rest(<<"-">>, AlphaPart),
                      format_vsn_rest(<<"+">>, BuildPart)]).

-spec format_version_part(integer() | binary()) -> iolist().
format_version_part(Vsn)
  when erlang:is_integer(Vsn) ->
    erlang:integer_to_list(Vsn);
format_version_part(Vsn)
  when erlang:is_binary(Vsn) ->
    Vsn.

%% @doc parse a string or binary into a valid semver representation
-spec parse(any_version()) -> version().
parse(Version) when erlang:is_list(Version) ->
    case rebar_semver_parser:parse(Version) of
        {fail, _} ->
            {erlang:iolist_to_binary(Version), {[],[]}};
        Good ->
            Good
    end;
parse(Version) when erlang:is_binary(Version) ->
    case rebar_semver_parser:parse(Version) of
        {fail, _} ->
            {Version, {[],[]}};
        Good ->
            Good
    end;
parse(Version) ->
    Version.

%% @doc test for quality between semver versions
-spec eql(any_version(), any_version()) -> boolean().
eql(VsnA, VsnB) ->
    NVsnA = normalize(parse(VsnA)),
    NVsnB = normalize(parse(VsnB)),
    NVsnA =:= NVsnB.

%% @doc Test that VsnA is greater than VsnB
-spec gt(any_version(), any_version()) -> boolean().
gt(VsnA, VsnB) ->
    {MMPA, {AlphaA, PatchA}} = normalize(parse(VsnA)),
    {MMPB, {AlphaB, PatchB}} = normalize(parse(VsnB)),
    ((MMPA > MMPB)
     orelse
       ((MMPA =:= MMPB)
        andalso
          ((AlphaA =:= [] andalso AlphaB =/= [])
           orelse
             ((not (AlphaB =:= [] andalso AlphaA =/= []))
              andalso
                (AlphaA > AlphaB))))
     orelse
       ((MMPA =:= MMPB)
        andalso
          (AlphaA =:= AlphaB)
        andalso
          ((PatchB =:= [] andalso PatchA =/= [])
           orelse
           PatchA > PatchB))).

%% @doc Test that VsnA is greater than or equal to VsnB
-spec gte(any_version(), any_version()) -> boolean().
gte(VsnA, VsnB) ->
    NVsnA = normalize(parse(VsnA)),
    NVsnB = normalize(parse(VsnB)),
    gt(NVsnA, NVsnB) orelse eql(NVsnA, NVsnB).

%% @doc Test that VsnA is less than VsnB
-spec lt(any_version(), any_version()) -> boolean().
lt(VsnA, VsnB) ->
    {MMPA, {AlphaA, PatchA}} = normalize(parse(VsnA)),
    {MMPB, {AlphaB, PatchB}} = normalize(parse(VsnB)),
    ((MMPA < MMPB)
     orelse
       ((MMPA =:= MMPB)
        andalso
          ((AlphaB =:= [] andalso AlphaA =/= [])
           orelse
             ((not (AlphaA =:= [] andalso AlphaB =/= []))
              andalso
                (AlphaA < AlphaB))))
     orelse
       ((MMPA =:= MMPB)
        andalso
          (AlphaA =:= AlphaB)
        andalso
          ((PatchA =:= [] andalso PatchB =/= [])
           orelse
           PatchA < PatchB))).

%% @doc Test that VsnA is less than or equal to VsnB
-spec lte(any_version(), any_version()) -> boolean().
lte(VsnA, VsnB) ->
    NVsnA = normalize(parse(VsnA)),
    NVsnB = normalize(parse(VsnB)),
    lt(NVsnA, NVsnB) orelse eql(NVsnA, NVsnB).

%% @doc check that VsnA is Approximately greater than VsnB
%%
%% Specifying ">= 2.6.5" is an optimistic version constraint. All
%% versions greater than the one specified, including major releases
%% (e.g. 3.0.0) are allowed.
%%
%% Conversely, specifying "~> 2.6" is pessimistic about future major
%% revisions and "~> 2.6.5" is pessimistic about future minor
%% revisions.
%%
%%  "~> 2.6" matches cookbooks >= 2.6.0 AND &lt; 3.0.0
%% "~> 2.6.5" matches cookbooks >= 2.6.5 AND &lt; 2.7.0
-spec pes(any_version(), any_version()) -> boolean().
pes(VsnA, VsnB) ->
    internal_pes(parse(VsnA), parse(VsnB)).

-spec to_list(integer() | binary() | string()) -> string() | binary().
to_list(Detail) when erlang:is_integer(Detail) ->
    erlang:integer_to_list(Detail);
to_list(Detail) when erlang:is_list(Detail); erlang:is_binary(Detail) ->
    Detail.

-spec format_vsn_rest(binary() | string(), [integer() | binary() | string()]) -> iolist().
format_vsn_rest(_TypeMark, []) ->
    [];
format_vsn_rest(TypeMark, [Head | Rest]) ->
    [TypeMark, Head |
     [[".", to_list(Detail)] || Detail <- Rest]].

%% @doc normalize the semver so they can be compared
-spec normalize(version()) -> version().
normalize({Vsn, Rest})
  when erlang:is_binary(Vsn);
       erlang:is_integer(Vsn) ->
    {{Vsn, 0, 0, 0}, Rest};
normalize({{Maj, Min}, Rest}) ->
    {{Maj, Min, 0, 0}, Rest};
normalize({{Maj, Min, Patch}, Rest}) ->
    {{Maj, Min, Patch, 0}, Rest};
normalize(Other = {{_, _, _, _}, {_,_}}) ->
    Other.

%% @doc to do the pessimistic compare we need a parsed semver. This is
%% the internal implementation of the of the pessimistic run. The
%% external just ensures that versions are parsed.
-spec internal_pes(version(), version()) -> boolean().
internal_pes(VsnA, {{LM, LMI}, Alpha})
  when erlang:is_integer(LM),
       erlang:is_integer(LMI) ->
    gte(VsnA, {{LM, LMI, 0}, Alpha}) andalso
        lt(VsnA, {{LM + 1, 0, 0, 0}, {[], []}});
internal_pes(VsnA, {{LM, LMI, LP}, Alpha})
    when erlang:is_integer(LM),
         erlang:is_integer(LMI),
         erlang:is_integer(LP) ->
    gte(VsnA, {{LM, LMI, LP}, Alpha})
        andalso
        lt(VsnA, {{LM, LMI + 1, 0, 0}, {[], []}});
internal_pes(VsnA, {{LM, LMI, LP, LMP}, Alpha})
    when erlang:is_integer(LM),
         erlang:is_integer(LMI),
         erlang:is_integer(LP),
         erlang:is_integer(LMP) ->
    gte(VsnA, {{LM, LMI, LP, LMP}, Alpha})
        andalso
        lt(VsnA, {{LM, LMI, LP + 1, 0}, {[], []}});
internal_pes(Vsn, LVsn) ->
    gte(Vsn, LVsn).

%% SPDX-SnippetEnd
