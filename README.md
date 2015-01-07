rebar
=====

rebar [3.0](#30) is an Erlang build tool that makes it easy to compile and test Erlang
applications, port drivers and releases.

[![Build Status](https://travis-ci.org/rebar/rebar3.svg?branch=master)](https://travis-ci.org/rebar/rebar3)

rebar is a self-contained Erlang script, so it's easy to distribute or even
embed directly in a project. Where possible, rebar uses standard Erlang/OTP
conventions for project structures, thus minimizing the amount of build
configuration work. rebar also provides dependency management, enabling
application writers to easily re-use common libraries from a variety of
locations (git, hg, etc).

3.0 Pre-Alpha
====

This is an experimental branch, considered to be pre-alpha, and still
very unstable. Use at your own risk, and expect no support. See
[the related announcement](http://lists.basho.com/pipermail/rebar_lists.basho.com/2014-November/002087.html).

Compatibility with rebar 2.0 has been broken, and features removed to
limit scope.

### Commands

| Command    | Description |
|----------- |------------ |
| as         | Higher-order provider to run multiple tasks in sequence as certain profiles |
| compile    | Build project |
| clean      | Remove project apps beam files |
| ct         | Run Common Test suites |
| do         | Higher-order provider to run multiple tasks in sequence |
| dialyzer   | Run the Dialyzer analyzer on the project |
| eunit      | Run eunit tests |
| help       | Print help for rebar or task |
| new        | Create new rebar project from templates |
| pkgs       | List available packages |
| release    | Build release of project |
| shell      | Run shell with project apps in path |
| tar        | Package release into tarball |
| update     | Update package index |
| upgrade    | Fetch latest version of dep |
| version    | Print current version of Erlang/OTP and rebar |

### Commands still to do

| Command    | Description |
|----------- |------------ |
| escriptize | Generate escript of project |


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
         do/1]).

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
```


Building
--------

Recommended installation of [Erlang/OTP](http://www.erlang.org) is binary packages from [Erlang Solutions](https://www.erlang-solutions.com/downloads/download-erlang-otp). For source it is recommended you use [erln8](http://metadave.github.io/erln8/) or [kerl](https://github.com/yrashk/kerl).

### Dependencies

To build rebar you will need a working installation of Erlang R15 (or later).

Should you want to clone the rebar repository, you will also require git.

#### Downloading

You can download a pre-built binary version of rebar3 based on the last commit from:

https://s3.amazonaws.com/rebar3/rebar3

#### Building rebar

```sh
$ git clone https://github.com/rebar/rebar3
$ cd rebar
$ ./bootstrap/bootstrap
==> rebar (compile)
Congratulations! You now have a self-contained script called "rebar" in
your current working directory. Place this script anywhere in your path
and you can use rebar to build OTP-compliant apps.
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
