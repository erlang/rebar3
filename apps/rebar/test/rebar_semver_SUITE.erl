-module(rebar_semver_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(assert_valid(Version), ?assert(rebar_semver:is_valid(Version))).
-define(assert_invalid(Version), ?assertNot(rebar_semver:is_valid(Version))).
-define(assert_matches(Version, Constraint),
        ?assert(begin
                    {ok, ParsedVersion} = rebar_semver:parse_version(Version),
                    {ok, ParsedConstraint} = rebar_semver:parse_constraint(Constraint),
                    rebar_semver:match(ParsedVersion, ParsedConstraint)
                end)).
-define(assert_matchesNot(Version, Constraint),
        ?assertNot(begin
                       {ok, ParsedVersion} = rebar_semver:parse_version(Version),
                       {ok, ParsedConstraint} = rebar_semver:parse_constraint(Constraint),
                       rebar_semver:match(ParsedVersion, ParsedConstraint)
                   end)).

all() ->
    [valid_version,
     invalid_version,
     exact_matches,
     range_matches,
     approximate_matches,
     and_matches,
     or_matches,
     prerelease_ordering,
     prerelease_matching,
     zero_versions,
     tilde_upper_bounds,
     complex_precedence].

valid_version(_Config) ->
    ?assert_valid(<<"0.1">>),
    ?assert_valid(<<"0.1.0">>),
    ?assert_valid(<<" 0.1.0">>),
    ?assert_valid(<<"  0.1.0">>),
    ?assert_valid(<<"<0.1">>),
    ?assert_valid(<<"<0.1.0">>),
    ?assert_valid(<<"< 0.1.0">>),
    ?assert_valid(<<"<  0.1.0">>),
    ?assert_valid(<<">0.1">>),
    ?assert_valid(<<">0.1.0">>),
    ?assert_valid(<<"> 0.1.0">>),
    ?assert_valid(<<">  0.1.0">>),
    ?assert_valid(<<"<=0.1">>),
    ?assert_valid(<<"<=0.1.0">>),
    ?assert_valid(<<"<= 0.1.0">>),
    ?assert_valid(<<"<=  0.1.0">>),
    ?assert_valid(<<">=0.1">>),
    ?assert_valid(<<">=0.1.0">>),
    ?assert_valid(<<">= 0.1.0">>),
    ?assert_valid(<<">=  0.1.0">>),
    ?assert_valid(<<"==0.1">>),
    ?assert_valid(<<"==0.1.0">>),
    ?assert_valid(<<"== 0.1.0">>),
    ?assert_valid(<<"==  0.1.0">>),
    ?assert_valid(<<"~>0.1">>),
    ?assert_valid(<<"~>0.1.0">>),
    ?assert_valid(<<"~> 0.1.0">>),
    ?assert_valid(<<"~>  0.1.0">>),
    ?assert_valid(<<"~> 0.1 or 0.5">>),
    ?assert_valid(<<"~> 0.1-or-something">>),
    ?assert_valid(<<"> 0.1.0 and < 0.2.0">>),
    ?assert_valid(<<"> 0.1.0&&< 0.2.0">>),
    ?assert_valid(<<"> 0.1.0 && < 0.2.0">>),
    ?assert_valid(<<"> 0.1.0  &&  < 0.2.0">>),
    ?assert_valid(<<"~> 6.6 or ~> 6.7">>),
    ?assert_valid(<<"0.9.9-rc.1 or > 1.0.0 and < 2.0.0">>),
    ?assert_valid(<<"~> 6.6||~> 6.7">>),
    ?assert_valid(<<"~> 6.6 || ~> 6.7">>),
    ?assert_valid(<<"~> 6.6  ||  ~> 6.7">>),
    ok.

invalid_version(_Config) ->
    ?assert_invalid(<<"">>),
    ?assert_invalid(<<"1.2.x">>),
    ?assert_invalid(<<"abc">>),
    ?assert_invalid(<<"1.2.three">>),
    ?assert_invalid(<<"1.-2.3">>),
    ?assert_invalid(<<".1.2.3">>),
    ?assert_invalid(<<"1.2.">>),
    ok.

exact_matches(_) ->
    ?assert_matches(<<"1.0.0">>, <<"1">>),
    ?assert_matches(<<"1.0.0">>, <<"1.0">>),
    ?assert_matches(<<"1.0.0">>, <<"1.0.0">>),
    ?assert_matchesNot(<<"1.0.0-rc.1">>, <<"1.0.0-rc.2">>),
    ?assert_matchesNot(<<"1.0.0">>, <<"1.0.0-rc.2">>),
    ?assert_matchesNot(<<"1.0.0-rc.1">>, <<"1.0.0">>),
    ?assert_matchesNot(<<"1.0.1">>, <<"1.0">>),
    ?assert_matchesNot(<<"1.0.1">>, <<"1">>),
    ok.

range_matches(_) ->
    ?assert_matches(<<"1.0.0">>, <<">=1">>),
    ?assert_matches(<<"1.0.0">>, <<">=1.0">>),
    ?assert_matches(<<"1.0.0">>, <<">=1.0.0">>),
    ?assert_matches(<<"1.0.0">>, <<">=1.0-rc.1">>),
    ?assert_matches(<<"99.0.0">>, <<">=1">>),
    ?assert_matchesNot(<<"1.0.0">>, <<">1">>),
    ?assert_matchesNot(<<"1.0.0">>, <<"<1">>),
    ?assert_matches(<<"0.65.0">>, <<"<2">>),
    ?assert_matchesNot(<<"1.5.5-rc.3">>, <<">2.0.0">>),
    ok.

approximate_matches(_) ->
    ?assert_matches(<<"1.5.6-rc.3">>, <<"~> 1.5.5">>),
    ?assert_matches(<<"1.3.2">>, <<"~> 1.0">>),
    ?assert_matches(<<"1.3.2">>, <<"~> 1">>),
    ?assert_matches(<<"1.3.2">>, <<"~> 1.3">>),
    ?assert_matches(<<"1.3.2">>, <<"~> 1.3.2">>),
    ?assert_matchesNot(<<"1.3.2">>, <<"~> 1.2.2">>),
    ?assert_matches(<<"1.3.2">>, <<"~> 1.3.1-rc.1">>),
    ?assert_matchesNot(<<"1.2.3">>, <<"~> 1.4">>),
    ok.

and_matches(_) ->
    ?assert_matches(<<"1.2.3">>, <<">= 1.2 and < 2">>),
    ?assert_matchesNot(<<"1.2.3">>, <<">= 1.2 and < 1.2">>),
    ?assert_matchesNot(<<"1.2.3">>, <<">= 1.3 and < 2">>),
    ?assert_matchesNot(<<"2.0.0">>, <<">= 1.2 and < 2">>),
    ?assert_matches(<<"1.2.3">>, <<"~> 1.2 and ~>1">>),
    ok.

or_matches(_) ->
    ?assert_matches(<<"3.8.6">>, <<"~> 2 or ~> 3">>),
    ?assert_matches(<<"3.5.3">>, <<" >= 3 and < 4 or ~> 3.5.2">>),
    ok.

prerelease_ordering(_Config) ->
    ?assert_matches(<<"1.0.0-alpha">>, <<">= 1.0.0-alpha">>),
    ?assert_matches(<<"1.0.0-alpha.1">>, <<"> 1.0.0-alpha">>),
    ?assert_matchesNot(<<"1.0.0-alpha">>, <<"> 1.0.0-alpha.1">>),
    ?assert_matches(<<"1.0.0-alpha.beta">>, <<"> 1.0.0-alpha.1">>),
    ?assert_matches(<<"1.0.0-beta">>, <<"> 1.0.0-alpha.beta">>),
    ?assert_matches(<<"1.0.0-beta.2">>, <<"> 1.0.0-beta">>),
    ?assert_matches(<<"1.0.0-beta.11">>, <<"> 1.0.0-beta.2">>),
    ?assert_matches(<<"1.0.0-rc.1">>, <<"> 1.0.0-beta.11">>),
    ?assert_matches(<<"1.0.0">>, <<"> 1.0.0-rc.1">>),
    ok.

prerelease_matching(_Config) ->
    ?assert_matchesNot(<<"1.0.0-alpha">>, <<"1.0.0">>),
    ?assert_matchesNot(<<"1.0.0-rc.1">>, <<"~> 1.0">>),
    ?assert_matches(<<"1.0.0-rc.1">>, <<"~> 1.0.0-alpha">>),
    ?assert_matches(<<"1.0.0-rc.1">>, <<">= 1.0.0-alpha and < 1.0.0">>),
    ok.

zero_versions(_Config) ->
    ?assert_matches(<<"0.0.0">>, <<"0.0.0">>),
    ?assert_matches(<<"0.0.1">>, <<"> 0.0.0">>),
    ?assert_matches(<<"0.1.0">>, <<"> 0.0.1">>),
    ?assert_matches(<<"0.0.5">>, <<"~> 0.0.1">>),
    ?assert_matchesNot(<<"0.1.0">>, <<"~> 0.0.1">>),
    ?assert_matches(<<"0.5.0">>, <<"~> 0.1">>),
    ?assert_matchesNot(<<"1.0.0">>, <<"~> 0.1">>),
    ?assert_matches(<<"0.0.0">>, <<">= 0">>),
    ?assert_matches(<<"0.9.9">>, <<"< 1">>),
    ok.

tilde_upper_bounds(_Config) ->
    ?assert_matches(<<"1.2.9">>, <<"~> 1.2.3">>),
    ?assert_matchesNot(<<"1.3.0">>, <<"~> 1.2.3">>),
    ?assert_matches(<<"1.9.9">>, <<"~> 1.2">>),
    ?assert_matchesNot(<<"2.0.0">>, <<"~> 1.2">>),
    ?assert_matches(<<"1.999.999">>, <<"~> 1.0">>),
    ?assert_matches(<<"2.0.0">>, <<"~> 1">>),
    ?assert_matches(<<"1.2.10">>, <<"~> 1.2.3">>),
    ok.

complex_precedence(_Config) ->
    ?assert_matches(<<"0.4.0">>, <<"~> 1.0 and >= 1.2 or < 0.5">>),
    ?assert_matchesNot(<<"1.1.0">>, <<"~> 1.0 and >= 1.2 or < 0.5">>),
    ?assert_matches(<<"1.3.0">>, <<"~> 1.0 and >= 1.2 or < 0.5">>),
    ?assert_matches(<<"1.5.0">>, <<"~> 1.0 or ~> 2.0 or ~> 3.0">>),
    ?assert_matches(<<"2.5.0">>, <<"~> 1.0 or ~> 2.0 or ~> 3.0">>),
    ?assert_matches(<<"3.5.0">>, <<"~> 1.0 or ~> 2.0 or ~> 3.0">>),
    ?assert_matches(<<"3.2.1">>, <<"~> 3.3 || ~> 3.2.1">>),
    ?assert_matchesNot(<<"4.5.0">>, <<"~> 1.0 or ~> 2.0 or ~> 3.0">>),
    ok.
