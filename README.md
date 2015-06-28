rebar
=====

rebar [3.0](#30) is an Erlang build tool that makes it easy to compile and test Erlang
applications, port drivers and releases.

[![Build Status](https://travis-ci.org/rebar/rebar3.svg?branch=master)](https://travis-ci.org/rebar/rebar3) [![Windows build status](https://ci.appveyor.com/api/projects/status/yx4oitd9pvd2kab3?svg=true)](https://ci.appveyor.com/project/TristanSloughter/rebar3)

rebar is a self-contained Erlang script, so it's easy to distribute or even
embed directly in a project. Where possible, rebar uses standard Erlang/OTP
conventions for project structures, thus minimizing the amount of build
configuration work. rebar also provides dependency management, enabling
application writers to easily re-use common libraries from a variety of
locations ([hex.pm](http://hex.pm), git, hg, etc).

3.0 Beta-1
====

[DOCUMENTATION](http://www.rebar3.org/v3.0/docs)

### Commands

| Command    | Description |
|----------- |------------ |
| as         | Higher-order provider to run multiple tasks in sequence as certain profiles |
| compile    | Build project |
| clean      | Remove project apps beam files |
| cover      | Generate coverage info from data compiled by `eunit --cover` or `ct --cover` |
| ct         | Run Common Test suites |
| do         | Higher-order provider to run multiple tasks in sequence |
| dialyzer   | Run the Dialyzer analyzer on the project |
| edoc       | Generate documentation using edoc |
| escriptize | Generate escript of project |
| eunit      | Run eunit tests |
| help       | Print help for rebar or task |
| new        | Create new rebar project from templates |
| pkgs       | List available packages |
| plugins    | List or upgrade plugins |
| release    | Build release of project |
| relup      | Creates relup from 2 releases |
| report     | Report on environment and versions for bug reports |
| shell      | Run shell with project apps in path |
| tar        | Package release into tarball |
| unlock     | Unlock dependencies |
| update     | Update package index |
| upgrade    | Fetch latest version of dep |
| version    | Print current version of Erlang/OTP and rebar |
| xref       | Run cross reference analysis on the project |

### Changes

* Fetches and builds deps if missing when running any command that relies on them
* Automatically recognizes `apps` and `lib` directory structure
* Relx for releases

### Gone

* Reltool integeration

### Providers

Providers are the modules that do the work to fulfill a user's command.

Example:

```erlang
-module(rebar_prv_something).

-behaviour(rebar_provider).

-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, something).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:state()) -> {ok, rebar_state:state()}.
init(State) ->
    State1 = rebar_state:add_provider(State, rebar_provider:create([{name, ?PROVIDER},
                                                                    {module, ?MODULE},
                                                                    {bare, false},
                                                                    {deps, ?DEPS},
                                                                    {example, "rebar dummy"},
                                                                    {short_desc, "dummy plugin."},
                                                                    {desc, ""},
                                                                    {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:state()) -> {ok, rebar_state:state()}.
do(State) ->
    %% Do something
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
```


Building
--------

Recommended installation of [Erlang/OTP](http://www.erlang.org) is source built using [erln8](http://metadave.github.io/erln8/) or [kerl](https://github.com/yrashk/kerl). For binary packages use those provided by [Erlang Solutions](https://www.erlang-solutions.com/downloads/download-erlang-otp), but be sure to choose the "Standard" download option or you'll have issues building projects.

### Dependencies

To build rebar you will need a working installation of Erlang R15 (or later).

Should you want to clone the rebar repository, you will also require git.

#### Downloading

You can download a pre-built binary version of rebar3 based on the last commit from:

https://s3.amazonaws.com/rebar3/rebar3

#### Bootstrapping rebar3

```sh
$ git clone https://github.com/rebar/rebar3
$ cd rebar3
$ ./bootstrap
```

### Developing on rebar3

When developing you can simply run `escriptize` to build your changes but the new escript is under `_build/default/bin/rebar3`

```sh
$ ./rebar3 escriptize
$ _build/default/bin/rebar3
```

Contributing to rebar
=====================

Please refer to [CONTRIBUTING](CONTRIBUTING.md).

Community and Resources
-----------------------

In case of problems that cannot be solved through documentation or examples, you
may want to try to contact members of the community for help. The community is
also where you want to go for questions about how to extend rebar, fill in bug
reports, and so on.

The main place to go for questions is the [rebar mailing
list](http://lists.basho.com/pipermail/rebar_lists.basho.com/). If you need
quick feedback, you can try the #rebar channel on
[irc.freenode.net](http://freenode.net). Be sure to check the
[wiki](https://github.com/rebar/rebar/wiki) first, just to be sure you're not
asking about things with well known answers.

For bug reports, roadmaps, and issues, visit the [github issues
page](https://github.com/rebar/rebar/issues).

General rebar community resources and links:

- [Rebar Mailing List](http://lists.basho.com/pipermail/rebar_lists.basho.com/)
- #rebar on [irc.freenode.net](http://freenode.net/)
- [wiki](https://github.com/rebar/rebar/wiki)
- [issues](https://github.com/rebar/rebar/issues)
