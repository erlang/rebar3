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

-type version() :: ec_semver:semver().
-type constraint() :: fun((ec_semver:semver()) -> boolean()).

-spec parse_version(binary()) -> {ok, version()} | {error, {invalid_vsn, binary()}}.
parse_version(Version) ->
    case ec_semver:parse(Version) of
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

parse_constraint_ors([], []) -> nomatch;
parse_constraint_ors([], [Match]) -> Match;
parse_constraint_ors([], Matchers) ->
    fun (Vsn) -> lists:any(fun (Match) -> Match(Vsn) end, Matchers) end;
parse_constraint_ors([And|Ors], Matchers) ->
    case parse_constraint_ands(binary:split(And, [<<" and ">>, <<"&&">>], [global]), []) of
        nomatch -> nomatch;
        Match -> parse_constraint_ors(Ors, [Match|Matchers])
    end.

parse_constraint_ands([], []) -> nomatch;
parse_constraint_ands([], [Match]) -> Match;
parse_constraint_ands([], Matchers) ->
    fun (Vsn) -> lists:all(fun (Match) -> Match(Vsn) end, Matchers) end;
parse_constraint_ands([Pattern|Ands], Matchers) ->
    case parse_constraint_pattern(Pattern) of
        nomatch -> nomatch;
        Match -> parse_constraint_ands(Ands, [Match|Matchers])
    end.

parse_constraint_pattern(<<" ", Vsn/binary>>) ->
    parse_constraint_pattern(Vsn);
parse_constraint_pattern(<<"==", Vsn/binary>>) ->
    parse_version_constraint(Vsn, fun ec_semver:eql/2);
parse_constraint_pattern(<<">=", Vsn/binary>>) ->
    parse_version_constraint(Vsn, fun ec_semver:gte/2);
parse_constraint_pattern(<<"<=", Vsn/binary>>) ->
    parse_version_constraint(Vsn, fun ec_semver:lte/2);
parse_constraint_pattern(<<">", Vsn/binary>>) ->
    parse_version_constraint(Vsn, fun ec_semver:gt/2);
parse_constraint_pattern(<<"<", Vsn/binary>>) ->
    parse_version_constraint(Vsn, fun ec_semver:lt/2);
parse_constraint_pattern(<<"~>", Vsn/binary>>) ->
    parse_version_constraint(Vsn, fun ec_semver:pes/2);
parse_constraint_pattern(Vsn) ->
    parse_version_constraint(Vsn, fun ec_semver:eql/2).

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

-spec match(version(), constraint()) -> boolean().
match(Version, Constraint) ->
    Constraint(Version).

-spec cmp(version(), version()) -> gt | lt | eq.
cmp(Vsn1, Vsn2) ->
    case ec_semver:gt(Vsn1, Vsn2) of
        true -> gt;
        false ->
            case ec_semver:lt(Vsn1, Vsn2) of
                true -> lt;
                false -> eq
            end
    end.

-spec format(version()) -> binary().
format(Vsn) ->
    iolist_to_binary(ec_semver:format(Vsn)).
