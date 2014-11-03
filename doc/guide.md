## Why Rebar3?

Rebar was a great step forward for Erlang development but with time has proven to be fragile to change. Rebar3 builds on certain components of rebar which have benefited from years of community collaboration within a new core which is more extendable.

## Creating a New Project

```shell
$ rebar3 new rel myrelease
===> Writing apps/myrelease/src/myrelease_app.erl
===> Writing apps/myrelease/src/myrelease_sup.erl
===> Writing apps/myrelease/src/myrelease.app.src
===> Writing rebar.config
===> Writing relx.config
===> Writing config/sys.config
===> Writing config/vm.args
===> Writing .gitignore
===> Writing LICENSE
===> Writing README.md
```

## Working on an Existing Rebar3 Project

First, checkout the project and change directories to the project root.

```shell
$ git clone git://github.com/tsloughter/minansan.git
$ cd minansan
```

Now we can use rebar3 to fetch all dependencies and build both the dependencies and the project app or apps.

```shell
$ rebar3 compile
===> Fetching gproc
Cloning into '.tmp_dir109479658516'...
===> Fetching ranch
Cloning into '.tmp_dir725384773580'...
===> Fetching cowboy
Cloning into '.tmp_dir285325769000'...
===> Fetching cowlib
Cloning into '.tmp_dir924054839613'...
===> Compiling gproc
/home/tristan/Devel/minasan/_deps/gproc/src/gproc_dist.erl:23: Warning: behaviour gen_leader undefined
===> Compiling cowlib
===> Compiling ranch
===> Compiling cowboy
===> Compiling minasan
```

## Adding Dependencies

Dependencies are listed in `rebar.config` file under the `deps` key:

```erlang
{deps, [
        {cowboy, ".*", {git, "git://github.com/ninenines/cowboy.git", {tag, "1.0.0"}}}
        ]}.
```

Then you'll most likely want to add the dependency to one of your project's application's `.app.src` file under applications.

## Rebar3 Conventions

Rebar3 is entirely based on building OTP applications and releases.

* Directories starting with underscores, e.g. `_deps`, are expected to not be checked in version control.
* Project apps you are working on exist under `apps/` or `lib/`, or is a single app project with `src/` in the root directory.
* `rebar.lock` and `rebar.config` go in the root of the project.
* Tests go in `tests/`.

## rebar.config vs rebar.lock

`rebar.lock` contains the exact reference id to a commit that was used to build the project. Committing this file allows you to specify a branch in `rebar.config` for a dependency and still have reproducable builds because if the `rebar.lock` file exists when a rebar3 project is being built the contents of deps in rebar.config are ignored.

## Checkout Dependencies

## Tests

Rebar3 has the concept of test dependencies. These dependencies will only be fetched when a rebar3 command that runs tests is run by the user.

```erlang
{test_deps, [
             {meck, ".*", {git, "https://github.com/eproxus/meck.git", {tag, "0.8"}}}
             ]}.
```

```shell
$ rebar ct
===> Fetching meck
Cloning into '.tmp_dir772710363032'...
===> Compiling meck
===> Compiling minasan
```
