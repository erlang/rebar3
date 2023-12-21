# verl 
[![Hex Version](https://img.shields.io/hexpm/v/verl.svg)](https://hex.pm/packages/verl) [![GitHub Actions CI](https://github.com/jelly-beam/verl/workflows/build/badge.svg)](https://github.com/jelly-beam/verl
) [![codecov](https://codecov.io/gh/jelly-beam/verl/branch/main/graph/badge.svg)](https://codecov.io/gh/jelly-beam/verl)

SemVer 2.0 version and requirements parsing, matching, and comparisons.

All parsing of versions and requirements adhere to the [SemVer 2.0 schema](http://semver.org/)

 - [Build](#build)
 -  [Usage](#usage)
    * [Comparisons](#comparisons)
    * [Version, Requirements, and Matching](#version--requirements--and-matching)
      - [Matching](#matching)
      - [Compiled requirements for ludicious speed matching](#compiled-requirements-for-ludicious-speed-matching)
      - [Version parsing](#version-parsing)
        * [Requirements parsing](#requirements-parsing)
 - [Credits](#credits)


## Build

```bash
$ rebar3 compile
```

## Test

```bash
$ rebar3 test
```

## Usage

Add to you deps configuration in rebar.config for your project : 

```erlang
{deps, [{verl, "1.1.0"}]}.
```

### Comparisons

```erlang
1> verl:compare(<<"1.0.0">>, <<"1.0.1">>).
lt
2> verl:compare(<<"1.0.0">>, <<"1.0.0">>).
eq
3> verl:compare(<<"2.0.0">>, <<"1.0.0">>).
gt
4> verl:compare(<<"1.0.0-pre">>, <<"1.0.0">>).
lt
5> verl:compare(<<"1.0.0">>, <<"1.0.0-pre">>).
gt
```

### Version, Requirements, and Matching

#### Matching

```erlang
1> verl:is_match(<<"1.0.0">>, <<"~> 1.0.0">>).
true
2> verl:is_match(<<"1.0.0">>, <<"~> 2.0.0">>).
false
3> verl:is_match(<<"3.2.0">>, <<"~> 3.0.0">>).
false
4> verl:is_match(<<"3.2.0">>, <<"~> 3.0">>).
true
```

#### Compiled requirements for ludicious speed matching

```erlang
1> {ok, Req} = verl:parse_requirement(<<"~> 3.0">>).
{ok,#{compiled => false,
  string => <<"~> 3.0">>,
  matchspec => [{{'$1','$2','$3','$4','$5'}...}],
  string => <<"~> 3.0">>}}
2> verl:is_match(<<"3.0.0-dev">>, Req).
  false
3> verl:is_match(<<"1.2.3">>, Req).
  false
4> verl:is_match(<<"3.1.0">>, Req).
  true
```

#### Version parsing

```erlang
1> verl:parse(<<"1.2.3">>).
#{build => undefined,major => 1,minor => 2,patch => 3,
  pre => []}
2> verl:parse(<<"1.2.3+build">>).
#{build => <<"build">>,major => 1,minor => 2,patch => 3,
  pre => []}
3> verl:parse(<<"1.2.3-pre+build">>).
#{build => <<"build">>,major => 1,minor => 2,patch => 3,
  pre => [<<"pre">>]}
4> verl:parse(<<"1">>).
{error, invalid_version}
5> verl:parse(<<"2">>).
{error, invalid_version}
```

Don't want a map? Use the `verl_parser` module...

```erlang
1> verl_parser:parse_version(<<"1.2.3">>).
{ok,{1,2,3,[],[]}}
2> verl_parser:parse_version(<<"1.2.3+build">>).
{ok,{1,2,3,[],[<<"build">>]}}
3> verl_parser:parse_version(<<"1.2.3-pre+build">>).
{ok,{1,2,3,[<<"pre">>],[<<"build">>]}}
4> verl_parser:parse_version(<<"1">>).
{error, invalid_version}
```

##### Requirements parsing

```erlang
1> verl:parse_requirement(<<"~> 2.1.0-dev">>).
{ok,#{compiled => false,
  string => <<"~> 2.1.0-dev">>,
  matchspec =>
      [{{'$1','$2','$3','$4','$5'}...] }}
2> verl:parse_requirement(<<"~> 2.1.0-">>).
{error,invalid_requirement}
```

Don't want a map? User the `verl_parser` module...

```erlang
1> verl_parser:parse_requirement(<<"~> 2.1.0-dev">>).
{ok, [{{'$1','$2','$3','$4','$5'}...]}
2> verl:parse_requirement(<<"~> 2.1.0-">>).
{error,invalid_requirement}
```

## Credits

- All credit goes to the Elixir team and contributors to Version and
Version.Parser in the Elixir standard lib for the algorithm and original
implementation.
