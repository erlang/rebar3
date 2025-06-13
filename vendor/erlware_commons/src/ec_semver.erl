%%% vi:ts=4 sw=4 et
%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware LLC
%%% @doc
%%%  Helper functions for working with semver versioning strings.
%%%  See http://semver.org/ for the spec.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_semver).

-export([parse/1,
         format/1,
         eql/2,
         gt/2,
         gte/2,
         lt/2,
         lte/2,
         pes/2,
         between/3]).

%% For internal use by the ec_semver_parser peg
-export([internal_parse_version/1]).

-export_type([semver/0,
              version_string/0,
              any_version/0]).

%%%===================================================================
%%% Public Types
%%%===================================================================

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

-type semver() :: {major_minor_patch_minpatch(), alpha_info()}.

-type version_string() :: string() | binary().

-type any_version() :: version_string() | semver().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc parse a string or binary into a valid semver representation
-spec parse(any_version()) -> semver().
parse(Version) when erlang:is_list(Version) ->
    case ec_semver_parser:parse(Version) of
        {fail, _} ->
            {erlang:iolist_to_binary(Version), {[],[]}};
        Good ->
            Good
    end;
parse(Version) when erlang:is_binary(Version) ->
    case ec_semver_parser:parse(Version) of
        {fail, _} ->
            {Version, {[],[]}};
        Good ->
            Good
    end;
parse(Version) ->
    Version.

-spec format(semver()) -> iolist().
format({Maj, {AlphaPart, BuildPart}})
  when erlang:is_integer(Maj);
       erlang:is_binary(Maj) ->
    [format_version_part(Maj),
     format_vsn_rest(<<"-">>, AlphaPart),
     format_vsn_rest(<<"+">>, BuildPart)];
format({{Maj, Min}, {AlphaPart, BuildPart}}) ->
    [format_version_part(Maj), ".",
     format_version_part(Min),
     format_vsn_rest(<<"-">>, AlphaPart),
     format_vsn_rest(<<"+">>, BuildPart)];
format({{Maj, Min, Patch}, {AlphaPart, BuildPart}}) ->
    [format_version_part(Maj), ".",
     format_version_part(Min), ".",
     format_version_part(Patch),
     format_vsn_rest(<<"-">>, AlphaPart),
     format_vsn_rest(<<"+">>, BuildPart)];
format({{Maj, Min, Patch, MinPatch}, {AlphaPart, BuildPart}}) ->
    [format_version_part(Maj), ".",
     format_version_part(Min), ".",
     format_version_part(Patch), ".",
     format_version_part(MinPatch),
     format_vsn_rest(<<"-">>, AlphaPart),
     format_vsn_rest(<<"+">>, BuildPart)].

-spec format_version_part(integer() | binary()) -> iolist().
format_version_part(Vsn)
  when erlang:is_integer(Vsn) ->
    erlang:integer_to_list(Vsn);
format_version_part(Vsn)
  when erlang:is_binary(Vsn) ->
    Vsn.



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

%% @doc Test that VsnMatch is greater than or equal to Vsn1 and
%% less than or equal to Vsn2
-spec between(any_version(), any_version(), any_version()) -> boolean().
between(Vsn1, Vsn2, VsnMatch) ->
    NVsnA = normalize(parse(Vsn1)),
    NVsnB = normalize(parse(Vsn2)),
    NVsnMatch = normalize(parse(VsnMatch)),
    gte(NVsnMatch, NVsnA) andalso
        lte(NVsnMatch, NVsnB).

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
pes(VsnA, VsnB) ->
    internal_pes(parse(VsnA), parse(VsnB)).

%%%===================================================================
%%% Friend Functions
%%%===================================================================
%% @doc helper function for the peg grammar to parse the iolist into a semver
-spec internal_parse_version(iolist()) -> semver().
internal_parse_version([MMP, AlphaPart, BuildPart, _]) ->
    {parse_major_minor_patch_minpatch(MMP), {parse_alpha_part(AlphaPart),
                                             parse_alpha_part(BuildPart)}}.

%% @doc helper function for the peg grammar to parse the iolist into a major_minor_patch
-spec parse_major_minor_patch_minpatch(iolist()) -> major_minor_patch_minpatch().
parse_major_minor_patch_minpatch([MajVsn, [], [], []]) ->
    strip_maj_version(MajVsn);
parse_major_minor_patch_minpatch([MajVsn, [<<".">>, MinVsn], [], []]) ->
    {strip_maj_version(MajVsn), MinVsn};
parse_major_minor_patch_minpatch([MajVsn,
                                  [<<".">>, MinVsn],
                                  [<<".">>, PatchVsn], []]) ->
    {strip_maj_version(MajVsn), MinVsn, PatchVsn};
parse_major_minor_patch_minpatch([MajVsn,
                                  [<<".">>, MinVsn],
                                  [<<".">>, PatchVsn],
                                  [<<".">>, MinPatch]]) ->
    {strip_maj_version(MajVsn), MinVsn, PatchVsn, MinPatch}.

%% @doc helper function for the peg grammar to parse the iolist into an alpha part
-spec parse_alpha_part(iolist()) -> [alpha_part()].
parse_alpha_part([]) ->
    [];
parse_alpha_part([_, AV1, Rest]) ->
    [erlang:iolist_to_binary(AV1) |
     [format_alpha_part(Part) || Part <- Rest]].

%% @doc according to semver alpha parts that can be treated like
%% numbers must be. We implement that here by taking the alpha part
%% and trying to convert it to a number, if it succeeds we use
%% it. Otherwise we do not.
-spec format_alpha_part(iolist()) -> integer() | binary().
format_alpha_part([<<".">>, AlphaPart]) ->
    Bin = erlang:iolist_to_binary(AlphaPart),
    try
        erlang:list_to_integer(erlang:binary_to_list(Bin))
    catch
        error:badarg ->
            Bin
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec strip_maj_version(iolist()) -> version_element().
strip_maj_version([<<"v">>, MajVsn]) ->
    MajVsn;
strip_maj_version([[], MajVsn]) ->
    MajVsn;
strip_maj_version(MajVsn) ->
    MajVsn.

-spec to_list(integer() | binary() | string()) -> string() | binary().
to_list(Detail) when erlang:is_integer(Detail) ->
    erlang:integer_to_list(Detail);
to_list(Detail) when erlang:is_list(Detail); erlang:is_binary(Detail) ->
    Detail.

-spec format_vsn_rest(binary() | string(), [integer() | binary()]) -> iolist().
format_vsn_rest(_TypeMark, []) ->
    [];
format_vsn_rest(TypeMark, [Head | Rest]) ->
    [TypeMark, Head |
     [[".", to_list(Detail)] || Detail <- Rest]].

%% @doc normalize the semver so they can be compared
-spec normalize(semver()) -> semver().
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
-spec internal_pes(semver(), semver()) -> boolean().
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
