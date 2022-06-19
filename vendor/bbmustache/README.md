bbmustache
===========
[![CircleCI](https://circleci.com/gh/soranoba/bbmustache/tree/master.svg?style=svg)](https://circleci.com/gh/soranoba/bbmustache/tree/master)
[![hex.pm version](https://img.shields.io/hexpm/v/bbmustache.svg)](https://hex.pm/packages/bbmustache)

Binary pattern match Based Mustache template engine for Erlang/OTP.

## Overview
- Binary pattern match based mustache template engine for Erlang/OTP.
  - It means do not use regular expressions.
- Support maps and associative arrays.
- Officially support is OTP17 or later.

### What is Mustache ?
A logic-less templates.
- [{{mustache}}](http://mustache.github.io/)

## Usage
### Quick start

```bash
$ git clone git://github.com/soranoba/bbmustache.git
$ cd bbmustache
$ make start
Erlang/OTP 17 [erts-6.3] [source-f9282c6] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:true]

Eshell V6.3  (abort with ^G)
1> bbmustache:render(<<"{{name}}">>, #{"name" => "hoge"}).
<<"hoge">>
2> bbmustache:render(<<"{{name}}">>, [{"name", "hoge"}]).
<<"hoge">>
```

### Use as a library
Add the following settings.

```erlang
%% rebar (rebar.config)

{deps,
  [
   {bbmustache, ".*", {git, "git://github.com/soranoba/bbmustache.git", {branch, "master"}}}
  ]}.

%% rebar3 (rebar.config)

{deps, [bbmustache]}.
```

### How to use simple Mustache

Map
```erlang
1> bbmustache:render(<<"{{name}}">>, #{"name" => "hoge"}).
<<"hoge">>

2> Template1 = bbmustache:parse_binary(<<"{{name}}">>).
...
3> bbmustache:compile(Template1, #{"name" => "hoge"}).
<<"hoge">>

4> Template2 = bbmustache:parse_file(<<"./hoge.mustache">>).
...
5> bbmustache:compile(Template2, #{"name" => "hoge"}).
<<"hoge">>
```

Associative array
```erlang
1> bbmustache:render(<<"{{name}}">>, [{"name", "hoge"}]).
<<"hoge">>

2> Template1 = bbmustache:parse_binary(<<"{{name}}">>).
...
3> bbmustache:compile(Template1, [{"name", "hoge"}]).
<<"hoge">>

4> Template2 = bbmustache:parse_file(<<"./hoge.mustache">>).
...
5> bbmustache:compile(Template2, [{"name", "hoge"}]).
<<"hoge">>
```

### Use as a command-line tool

```bash
make escriptize
echo '{"name", "hoge"}.' > vars.config
echo '{{name}}' > template.mustache
./bbmustache -d vars.config template.mustache
hoge
```

Data files (-d) support a single assoc list, a single map, and [consult](https://erlang.org/doc/man/file.html#consult-1) format.<br>
Note: the behind term has a high priority in all cases. it is a result of supporting to allow for embedding relative file paths as in [config](http://erlang.org/doc/man/config.html).

### More information
- For the alias of mustache, Please refer to [ManPage](http://mustache.github.io/mustache.5.html) and [Specification](https://github.com/mustache/spec)
- For the options of this library, please see [doc](doc)
- For the functions supported by this library, please see [here](benchmarks/README.md)

## FAQ

### Avoid http escaping

```erlang
%% Please use `{{{tag}}}`
1> bbmustache:render(<<"<h1>{{{title}}}</h1>">>, #{"title" => "I like Erlang & mustache"}).
<<"<h1>I like Erlang & mustache</h1>">>

%% If you should not want to use `{{{tag}}}`, escape_fun can be use.
1> bbmustache:render(<<"<h1>{{title}}</h1>">>, #{"title" => "I like Erlang & mustache"}, [{escape_fun, fun(X) -> X end}]).
<<"<h1>I like Erlang & mustache</h1>">>
```

### Already used `{` and `}` for other uses (like escript)

```erlang
1> io:format(bbmustache:render(<<"
1> {{=<< >>=}}
1> {deps, [
1>   <<#deps>>
1>   {<<name>>, \"<<version>>\"}<<^last?>>,<</last?>>
1>   <</deps>>
1> ]}.
1> ">>, #{"deps" => [
1>   #{"name" => "bbmustache", "version" => "1.6.0"},
1>   #{"name" => "jsone", "version" => "1.4.6", "last?" => true}
1> ]})).

{deps, [
  {bbmustache, "1.6.0"},
  {jsone, "1.4.6"}
]}.
ok
```

### Want to use something other than string for key

```erlang
1> bbmustache:render(<<"<h1>{{{title}}}</h1>">>, #{title => "I like Erlang & mustache"}, [{key_type, atom}]).
<<"<h1>I like Erlang & mustache</h1>">>

2> bbmustache:render(<<"<h1>{{{title}}}</h1>">>, #{<<"title">> => "I like Erlang & mustache"}, [{key_type, binary}]).
<<"<h1>I like Erlang & mustache</h1>">>
```

### Want to provide a custom serializer for Erlang Terms

```erlang
1> bbmustache:render(<<"<h1>{{title}}</h1>">>, #{title => "I like Erlang & mustache"}, [{key_type, atom}, {value_serializer, fun(X) -> X end}]).
<<"<h1>I like Erlang &amp; mustache</h1>">>

2> bbmustache:render(<<"<h1>{{{title}}}</h1>">>, #{<<"title">> => "I like Erlang & mustache"}, [{key_type, binary}, {value_serializer, fun(X) -> <<"replaced">> end}]).
<<"<h1>replaced</h1>">>

3> bbmustache:render(<<"<h1>{{{title}}}</h1>">>, #{<<"title">> => #{<<"nested">> => <<"value">>}}, [{key_type, binary}, {value_serializer, fun(X) -> jsone:encode(X) end}]).
<<"<h1>{\"nested\": \"value\"}</h1>">>

4> bbmustache:render(<<"<h1>{{title}}</h1>">>, #{<<"title">> => #{<<"nested">> => <<"value">>}}, [{key_type, binary}, {value_serializer, fun(X) -> jsone:encode(X) end}]).
<<"<h1>{&quot;nested&quot;:&quot;value&quot;}</h1>">>
```

## Attention
- Lambda expression is included wasted processing.
  - Because it is optimized to `parse_binary/1` + `compile/2`.

## Comparison with other libraries
[Benchmarks and check the reference implementation](benchmarks/README.md)

## Contribute
Pull request is welcome =D

## License
[MIT License](LICENSE)
