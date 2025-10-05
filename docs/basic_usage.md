# Basic Usage

## New App or Release

There are two main ways to organize code with rebar3 projects: either as a single application, or as an umbrella project.

Single application projects contain a lone top-level application at the root of the directory, with its Erlang source modules directly inside a `src/` directory. This format is applicable to libraries to be published on GitHub or in hex with the objective of making them shareable to the world, but can also be used with [Releases](deployment/releases.md), which allow to ship an Erlang runtime system that boots the application directly.

Umbrella projects' defining characteristic is that they can contain multiple top-level Erlang/OTP applications, usually within a top-level `apps/` or `lib/` directory. Each of these applications may contain its own rebar.config file. This format is applicable to only for releases with one or more top-level applications.

Rebar3 comes with templates for creating either types of project, callable through the `rebar3 new <template> <project-name>` command. The `<template>` value can be any of:

- `app`: a stateful OTP application with a supervision tree, as a single application project
- `lib`: a library OTP application (without supervision trees), useful for grouping together various modules, as a single application project
- `release`: an umbrella project ready to be released
- `umbrella`: an alias for `release`
- `escript`: a special form of single application project that can be built as a runnable script
- `plugin`: structure for a rebar3 plugin
- `cmake`: generates a `c_src` directory and `Makefile` for building C/C++ code

For example:

```shell
$ rebar3 new app myapp
===> Writing myapp/src/myapp_app.erl
===> Writing myapp/src/myapp_sup.erl
===> Writing myapp/src/myapp.app.src
===> Writing myapp/rebar.config
===> Writing myapp/.gitignore
===> Writing myapp/LICENSE
===> Writing myapp/README.md
```

For more information on `new` and available options check the docs on [commands](commands.md) and to learn how to create and use custom templates go to the [templates tutorial](tutorials/templates.md).

## Adding Dependencies

Dependencies are listed in `rebar.config` file under the `deps` key:

```erlang
{deps, [
        {elli, "~> 3.3.0"}, % package
        {elli, {git, "git://github.com/elli-lib/elli.git", {tag, "3.3.0"}}} % alternatively, source
        ]
}.
```

Now you can add the dep to one of your project's application's .app.src file under applications so that Erlang knows the dependency is required for yours to work:

```erlang
{application, <APPNAME>,
 [{description, ""},
  {vsn, <APPVSN>},
  {registered, []},
  {modules, []},
  {applications, [kernel,
                  stdlib,
                  elli]},
  {mod, {<APPNAME>_app, []}},
  {env, []}
 ]}.
```

The `<APPVSN>` value can be any of:

| Version type | Result |
| --------------------- | ------------------------------------------------------------- |
| `string()` | A string is used, as is, for the version. Example: `"0.1.0"`|
| `git | semver`  | Uses the latest git tag on the repo to construct the version. |
| `{cmd, string()}`     | Uses the result of executing the contents of `string()` in a shell. Example to use a file `VERSION`: `{cmd, "cat VERSION | tr -d '[:space:]'"}` |
| `{git, short | long}` | Uses either the short (8 characters) or the full Git ref. of the current commit. |
| `{file, File}` | Uses the content of a file. For example, a better way to use a `VERSION` file than using `cmd` would be: `{file, "VERSION"}` |

For more information on dependency handling view the [dependency documentation](configuration/dependencies.md)

## Building

Only one command, `compile`, is required to fetch dependencies and compile all applications.

```shell
$ rebar3 compile
===> Verifying dependencies...
===> Fetching elli v3.3.0
===> Analyzing applications...
===> Compiling elli
===> Analyzing applications...
===> Compiling custom_hex_repos
```

## Output Format

Output for installing dependencies, building releases and any other output written to disk is found in the `_build` directory at the root of the project.

```shell
_build/
└── default
  └── lib
    └── elli
```

More about profiles and the `_build` directory can be found in the [profiles documentation page](configuration/profiles.md).

## Testing

Tests by default are expected to be found under the `test/` directory, aside from `eunit` found within individual modules.

Dependencies that are only needed for running tests can be placed in the `test` profile:

```erlang
{profiles, [
    {test, [
        {deps, [
            {meck, "0.9.0"}
        ]}
    ]}
]}.
```

Now the first time `rebar3 ct` is run `meck` will be installed to `_build/test/lib/`. But it will not be added to `rebar.lock`.

```shell
_build/
   └── test
     └── lib
       └── meck
```

## Releases and Target Systems

Releases are built using [relx](https://github.com/erlware/relx).

Creating a new project with a release structure and default `relx` config in the `rebar.config` file run:

```shell
$ rebar3 new release myrel
===> Writing myrel/apps/myrel/src/myrel_app.erl
===> Writing myrel/apps/myrel/src/myrel_sup.erl
===> Writing myrel/apps/myrel/src/myrel.app.src
===> Writing myrel/rebar.config
===> Writing myrel/config/sys.config
===> Writing myrel/config/vm.args
===> Writing myrel/.gitignore
===> Writing myrel/LICENSE
===> Writing myrel/README.md
```

Looking in `rebar.config` we find a couple elements that were not there in our application example.

```erlang
{relx, [{release, {myrel, "0.0.1"},
         [myrel]},

        {dev_mode, true},
        {include_erts, false},

        {extended_start_script, true}
       ]
}.

{profiles, [
    {prod, [{relx, [{dev_mode, false},
                    {include_erts, true}]}
     ]}
]}.
```

This configuration provides some nice defaults for building a release with Relx for development (default profile) and for production (prod profile). When building a release for production we'll most likely want to create a target system (include erts) and definitely will not want the release to contain symlinks to apps (`dev_mode` `false`).

```shell
$ rebar3 release
===> Verifying default dependencies...
===> Compiling myrel
===> Starting relx build process ...
===> Resolving OTP Applications from directories:
          _build/default/lib
          /usr/lib/erlang/lib
===> Resolved myrel-0.1.0
===> Dev mode enabled, release will be symlinked
===> release successfully created!
```

With the default `rebar.config`, creating a compressed archive of the release as a target system is as simple as setting the profile to `prod` and running `tar`:

```shell
$ rebar3 as prod tar
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling relx_overlays
===> Assembling release myrel-0.1.0...
===> Release successfully assembled: _build/prod/rel/myrel
===> Building release tarball myrel-0.1.0.tar.gz...
===> Tarball successfully created: _build/prod/rel/myrel/myrel-0.1.0.tar.gz
```

For more details go to the [release section](deployment/releases.md).
