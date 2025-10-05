# Plugins

Plugins can be installed locally, to a project, and/or globally. To install a plugin locally, specify it under `plugins` in your project's `rebar.config`. Globally installed plugins are configured in `~/.config/rebar3/rebar.config` and automatically installed when you use a Rebar3 command in your project.

## Including Plugins

Plugins required for building an application should be included under `plugins` in the same format as dependencies are listed in `deps`. They are built under the directory `_build/<profile>/plugins/` and are listed in the output of `rebar3 help`, available to use as tasks and in `provider_hooks`:

```erlang
{plugins, [{rebar_erl_vsn, "~> 0.1"}]}.
{provider_hooks, [{pre, [{compile, {default, erl_vsn}}]}]}.
```

The above configuration will result in `erl_vsn` task being run before the `compile` task. This plugin adds defines to the compile configuration, such as if running on an Erlang version above 17 it will include `{d, 'maps'}` in `erl_opts` during compilation.

## Project Plugins and Overriding Commands

Some plugins work best if they are able to override the available commands. However, this isn't something that plugins should be able to do when the project they are included in is used as a dependency. `project_plugins` defines plugins that will only be available when the project is being built directly by `rebar3` commands, as in running `rebar3` from the top level directory of the application/project.

For example the cuttlefish plugin is only necessary when building a release, so not fetching it and having it available per application dependency makes sense. It also works best if it works the same as building a release or tarball. So when included in `project_plugins`:

```erlang
{project_plugins, [rebar3_cuttlefish]}.
```

Running `rebar3 release` or `rebar3 tar` will be running the `rebar3_cuttlefish` providers instead of the built in providers.

Additionally there are cases you only need a plugin for development. Say you have protobufs and commit the generated modules to the repo and include in the Hex package, then there is no need for the protobuf plugin to be available when the application is a dependency and is only needed for development purposes. Before `project_plugins` it was common to see a `dev` profile with plugins added under it, but this then required running `rebar3 as dev protobuf` and dealing with another profile under `_build`. With project plugins the config can instead be:

```erlang
{project_plugins, [rebar3_gpb_plugin]}.
```

Now we need only run `rebar3 protobuf`. We do not include any hooks because we will be committing the generated code to the repository and the plugin will not be fetched if this project is used as a dependency.

## Upgrading Plugins

Plugins work a bit like dependencies (although they are currently not being version-locked); they will not be automatically updated unless you ask for them to be.

- You can upgrade project-local plugins by calling `rebar3 plugins upgrade <plugin_name>`.
- You can upgrade global plugins by calling `rebar3 as global plugins upgrade <plugin_name>`, which invokes a hidden global profile used specifically to switch the upgrade context for plugins.

If you are using Hex packages as plugins and you do not see the version you expected, remember to use call `rebar3 update` to get a fresh Hex index. Once again, since plugins are not locked as part of the lock file, it might be a good idea to always specify a version for them.

## Recommended Plugins

- [Auto-Compile and Load](#auto-compile-and-load)
- [Auto-Test](#auto-test)
- [Hex Package Management](#hex-package-management)
- [Port Compiler](#port-compiler)
- [Run Release](#run-release)
- [Alias](#alias)
- [QuickCheck](#quickcheck)
- [PropEr](#proper)
- [Diameter](#diameter)
- [ErlyDTL](#erlydtl)
- [Neotoma](#neotoma)
- [Protocol buffers](#protocol-buffers)
- [Appup](#appup)
- [Vendoring Dependencies](#vendoring-dependencies)
- [Elixir Dependencies](#elixir-dependencies)

### Auto Compile and Load

For the `auto` plugin it is suggested to place the entry in the global `rebar3` config which should be made as `~/.config/rebar3/rebar.config`.

```erlang
{plugins, [rebar3_auto]}.
```

Running `rebar3 auto` will start the shell the same as running `rebar3 shell` but will be listening for file changes in your project's application source directories. When a file is changed it will message the Rebar3 agent to run compile and reload modules.

### Auto-Test

For the `autotest` plugin it is suggested to place the entry in the global `rebar3` config which should be made as `~/.config/rebar3/rebar.config`.

```erlang
{plugins, [{rebar3_autotest, "0.1.1"}]}.
```

Running `rebar3 as test autotest` will start `eunit` once and set watches for your source, header and test-files, so that it reruns `eunit` on changes on one of the files.

### Hex Package Management

For the `hex` plugin it is suggested to place the entry in the global `rebar3` config which should be made as `~/.config/rebar3/rebar.config`.

```erlang
{plugins, [rebar3_hex]}.
```

For usage go to the [Hex Package Management](package_management/publishing-packages.md) section. To view the package go to [hex.pm](https://hex.pm/packages/rebar3_hex) and to open issues [GitHub](https://github.com/tsloughter/rebar3_hex).

### Port Compiler

This plugin provides the old `rebar` interface, to building C and C++ code, to `rebar3`. The package can be found at [hex.pm](https://hex.pm/packages/pc) and issues at [GitHub](https://github.com/blt/port_compiler).

In your project's `rebar.config`, add the `pc` plugin and calls to it in `provider_hooks` for `compile` and `clean`:

```erlang
{plugins, [pc]}.

{provider_hooks,
 [
  {pre,
   [
    {compile, {pc, compile}},
    {clean, {pc, clean}}
   ]
  }
 ]
}.
```

Configuration variables available:

```erlang
%% Supported configuration variables:
%%
%% * port_specs - Erlang list of tuples of the forms
%%                {ArchRegex, TargetFile, Sources, Options}
%%                {ArchRegex, TargetFile, Sources}
%%                {TargetFile, Sources}
%%
%% * port_env - Erlang list of key/value pairs which will control
%%              the environment when running the compiler and linker.
%%              Variables set in the surrounding system shell are taken
%%              into consideration when expanding port_env.
%%
%%              By default, the following variables are defined:
%%              CC       - C compiler
%%              CXX      - C++ compiler
%%              CFLAGS   - C compiler
%%              CXXFLAGS - C++ compiler
%%              LDFLAGS  - Link flags
%%              ERL_CFLAGS  - default -I paths for erts and ei
%%              ERL_LDFLAGS - default -L and -lerl_interface -lei
%%              DRV_CFLAGS  - flags that will be used for compiling
%%              DRV_LDFLAGS - flags that will be used for linking
%%              EXE_CFLAGS  - flags that will be used for compiling
%%              EXE_LDFLAGS - flags that will be used for linking
%%              ERL_EI_LIBDIR - ei library directory
%%              DRV_CXX_TEMPLATE      - C++ command template
%%              DRV_CC_TEMPLATE       - C command template
%%              DRV_LINK_TEMPLATE     - C Linker command template
%%              DRV_LINK_CXX_TEMPLATE - C++ Linker command template
%%              EXE_CXX_TEMPLATE      - C++ command template
%%              EXE_CC_TEMPLATE       - C command template
%%              EXE_LINK_TEMPLATE     - C Linker command template
%%              EXE_LINK_CXX_TEMPLATE - C++ Linker command template
%%
%%              Note that if you wish to extend (vs. replace) these variables,
%%              you MUST include a shell-style reference in your definition.
%%              e.g. to extend CFLAGS, do something like:
%%
%%              {port_env, [{"CFLAGS", "$CFLAGS -MyOtherOptions"}]}
%%
%%              It is also possible to specify platform specific options
%%              by specifying a triplet where the first string is a regex
%%              that is checked against Erlang's system architecture string.
%%              e.g. to specify a CFLAG that only applies to x86_64 on linux
%%              do:
%%
%%              {port_env, [{"x86_64.*-linux", "CFLAGS",
%%                           "$CFLAGS -X86Options"}]}
%%
%%              Cross-arch environment variables to configure toolchain:
%%              GET_ARCH to set the tool chain name to use
%%              GET_ARCH_WORDSIZE (optional - to determine word size)"
%%              word size is 32
%%              GET_ARCH_VSN (optional - "
%%              l version of CC/CXX is requested),
```

### Run Release

`rebar3 run` will start the release console, instead of having to run `_build/default/rel/<release>/bin/<release> console`. Found at [GitHub](https://github.com/tsloughter/rebar3_run) and [hex.pm](https://hex.pm/packages/rebar3_run).

```erlang
{plugins, [rebar3_run]}.
```

### Alias

The alias plugin has been added to Rebar3 starting with version 3.5.0. See [Create Aliases for Common Tasks](workflow.md#create-aliases-for-common-tasks) for instructions.

For prior versions, the plugin for aliasing a single command to run multiple tasks can be found at [GitHub](https://github.com/tsloughter/rebar_alias) and [Hex.pm](https://hex.pm/packages/rebar_alias).

```erlang
{plugins, [rebar_alias]}.

{alias, [{check, [eunit, {ct, "--sys_config=config/app.config"}]}]}.
```

Arguments (as with a command line) can be passed by replacing `Provider` with `{Provider, Args}`.

### QuickCheck

A Rebar3 plugin to enable the execution of [Erlang QuickCheck](https://www.quviq.com/products/erlang-quickcheck/) properties. Found on [GitHub](https://github.com/kellymclaughlin/rebar3-eqc-plugin) and [Hex.pm](https://hex.pm/packages/rebar3_eqc).

```erlang
{plugins, [rebar3_eqc]}.
```

Config options for the QuickCheck go under `eqc_opts`, for example `{eqc_opts, [{numtests, 500}]}.`:

| Config Option | Type    | Description                                                                                      |
| ------------- | ------- | ------------------------------------------------------------------------------------------------ |
| numtests      | integer | Number of test executions, default 100.                                                          |
| testing_time  | integer | Time in seconds to execute property. If both are specified, the testing_time setting is ignored. |

Similarly configuration can be passed on the command line:

| Option | Type    | Description                                                                                                       |
| ------ | ------- | ----------------------------------------------------------------------------------------------------------------- |
| -n     | integer | Number of test executions, default 100.                                                                           |
| -t     | integer | Time in seconds to execute property. If both are specified, the testing_time setting is ignored.                  |
| -p     | string  | Property to execute. This can be either `module:property` or `property` and the plugin will determine the module. |

### PropEr

[PropEr](https://proper.softlab.ntua.gr/) is a free alternative to Quviq QuickCheck. The plugin is available [on Hex as a package](https://hex.pm/packages/rebar3_proper) or [GitHub](https://github.com/ferd/rebar3_proper/)

```erlang
%% the plugin itself
{plugins, [rebar3_proper]}.

%% The PropEr dependency is still required to compile the test cases
{profiles,
    [{test, [
        {deps, [{proper, "1.1.1-beta"}]}
    ]}
]}.
```

All of PropEr's configuration options can be passed in rebar.config under `{proper_opts, Options}` or as command line arguments:

| rebar.config key          | Command Line       | Description                                                                                                                                  |
| ------------------------- | ------------------ | -------------------------------------------------------------------------------------------------------------------------------------------- |
| {dir, String}             | -d, --dir          | directory where the property tests are located (defaults to "test")                                                                          |
| {module, [Modules]}       | -m, --module       | name of one or more modules to test                                                                                                          |
| {properties, [PropNames]} | -p, --prop         | name of properties to test within a specified module                                                                                         |
| {numtests, N}             | -n, --numtests     | number of tests to run when testing a given property                                                                                         |
| verbose \| quiet          | -v, --verbose      | Whether each property tested shows its output or not (defaults to true/verbose)                                                              |
| {cover, true \| false}    | -c, --cover        | generate cover data (default: false)                                                                                                         |
| long_result               | --long_result      | enables long-result mode, displaying counter-examples on failure rather than just false                                                      |
| {start_size, N}           | --start_size       | specifies the initial value of the size parameter                                                                                            |
| {max_size, N}             | --max_size         | specifies the maximum value of the size parameter                                                                                            |
| {max_shrinks, N}          | --max_shrinks      | specifies the maximum number of times a failing test case should be shrunk before returning                                                  |
| noshrink                  | --noshrink         | instructs PropEr to not attempt to shrink any failing test cases                                                                             |
| {constraint_tries, N}     | --constraint_tries | specifies the maximum number of tries before the generator subsystem gives up on producing an instance that satisfies a ?SUCHTHAT constraint |
| {spec_timeout, Millisecs} | --spec_timeout     | duration, in milliseconds, after which PropEr considers an input to be failing                                                               |
| any_to_integer            | --any_to_integer   | converts instances of the any() type to integers in order to speed up execution                                                              |

### Diameter

The [rebar3_diameter_compiler](https://github.com/carlosedp/rebar3_diameter_compiler) plugin compiles diameter `.dia` files in Rebar3 projects.

```erlang
{plugins, [rebar3_diameter_compiler]}.
```

Add hooks to automatically compile and clean the diameter dictionaries:

```erlang
{provider_hooks, [
    {pre, [
        {compile, {diameter, compile}},
        {clean, {diameter, clean}}
    ]}
]}.
```

Configuration options:

| Config Option   | Type | Description                                                              |
| --------------- | ---- | ------------------------------------------------------------------------ |
| dia_opts        | list | Options from diameter_make:codec/2 supported with exception of inherits. |
| dia_first_files | list | Files in sequence to compile first.                                      |
|                 |      |                                                                          |

### ErlyDTL

The [erlydtl](https://github.com/erlydtl/erlydtl) compiler has been moved to a separate plugin.

```erlang
{plugins, [
    {rebar3_erlydtl_plugin, ".*",
     {git, "https://github.com/tsloughter/rebar3_erlydtl_plugin.git", {branch, "master"}}}
]}.
```

Config options go in a list under `erlydtl_opts` in `rebar.config`:

| Config Option    | Type     | Description                                                                                                                    |
| ---------------- | -------- | ------------------------------------------------------------------------------------------------------------------------------ |
| doc_root         | string   | Where to find templates to compile. "priv/templates" by default.                                                               |
| compiler_options | proplist | Template compilation options to pass to erlydtl. Descriptions [here](https://github.com/erlydtl/erlydtl#template-compilation). |
| out_dir          | string   | Where to put compiled template beam files "ebin" by default.                                                                   |
| source_ext       | string   | The file extension the template sources have ".dtl" by default.                                                                |
| module_ext       | string   | Characters to append to the template's module name "\_dtl" by default.                                                         |
| recursive        | boolean  | Boolean that determines if doc_root(s) need to be scanned recursively for matching template file names. 'true' by default.     |

### Neotoma

Plugin for building PEG files using [Sean Cribbs neotoma app](https://github.com/seancribbs/neotoma). This plugin is published to Hex so can be added to your project with:

```erlang
{plugins, [rebar3_neotoma_plugin]}.
```

The `compile` function is under the `neotoma` namespace. To automatically before the Erlang compiler add the `pre_hook` to `rebar.config`:

```erlang
{provider_hooks, [
    {pre, [{compile, {neotoma, compile}}]}
]}.
```

### Protocol Buffers

### Using gpb

[Plugin](https://github.com/lrascao/rebar3_gpb_plugin) for building `.proto` files using Tomas Abrahamsson's [gpb](https://github.com/tomas-abrahamsson/gpb). This plugin is published to Hex so can be added to your project with:

```erlang
{erl_opts, [{i, "./_build/default/plugins/gpb/include/"}]}.
{plugins, [{rebar3_gpb_plugin, "2.10.0"}]}.

{gpb_opts, [{i, "proto"},
        {o_erl, "src"},
        {o_hrl, "include"}]}.
```

The `compile` function is under the `protobuf` namespace. To automatically build before the Erlang compiler add the `provider` `pre` hook to `rebar.config`:

```erlang
{provider_hooks, [
    {pre, [{compile, {protobuf, compile}}]}
]}.
```

Full documentation available in the plugin's [README](https://github.com/lrascao/rebar3_gpb_plugin/blob/develop/README.md#use).

### Appup

Plugin for generating, compiling and validating `.appup.src` files. This plugin is published to Hex so can be added to your project with:

```erlang
{plugins, [rebar3_appup_plugin]}.
```

The `compile` and `clean` functions are under the `appup` namespace. To automatically build before the Erlang compiler add the `provider` `pre` hook to `rebar.config`:

```erlang
{provider_hooks, [
    {post, [{compile, {appup, compile}},
            {clean, {appup, clean}}]}
]}.
```

To compare two releases and generate the `.appup` with the necessary instructions to execute the release upgrade run:

```shell
git checkout <from version>
rebar3 release
git checkout <to version>
rebar3 release
rebar3 appup generate
rebar3 relup tar
```

| Argument         | Type     | Description                                                                                         |
| ---------------- | -------- | --------------------------------------------------------------------------------------------------- |
| previous         | optional | Path location of the previous release to compare with                                               |
| current          | optional | Path location of the current release to compare with, defaults to _build/\<profile\>/rel/\<app_name\> |
| target_dir       | optional | Location of where to generate the .appup file.                                                      |
| previous_version | optional | Version to update from                                                                              |

Full documentation available in the plugin's [README](https://github.com/lrascao/rebar3_appup_plugin/blob/master/README.md)

### Vendoring dependencies

This section contains plugins for storing vendored dependencies within a project.

Starting with Rebar3 3.7.0, you can make use of [`rebar3_path_deps`](https://github.com/benoitc/rebar3_path_deps) as a plugin, to specify relative vendored paths for dependency retrieval. The local paths should work even when the plugin is used for a dependency.

Let’s start off by making a new OTP application `hello_utils` inside of your  project `hello_world`:

```shell
# inside of hello-world/
$ rebar3 new app hello_utils
```

This will create a new folder `hello_utils` inside of which a `rebar.config` and `src` folder are ready to be used.

In order to tell Rebar3 about this, open up `hello_world/rebar.config` and add `hello_utils` to your dependencies:

```erlang
{deps, [
  {hello_utils, {path, "hello_utils"}},
  ...
]
```

This tells Rebar3 that we depend on an application called `hello_utils` which is found in the `hello_utils` directory (relative to the `rebar.config` file it’s written in).

Then add the plugin to your `rebar.config`:

```erlang
{plugins, [
   rebar3_path_deps
]}.
```

Then just compile your application:

```shell
$ rebar3 compile
===> Compiling rebar3_path_deps
===> Verifying dependencies...
===> Fetching hello_utils ({path,"hello_utils",
                            {mtime,<<"2018-10-17T11:21:18Z">>}})
===> Compiling hello_utils
===> Compiling hello_world
```

This should cover it all.

For versions prior to 3.7.0, the following plugin was preferable, but only worked at the top-level of a project.

```erlang
{plugins, [rebar3_vendor]}.
```

To store the fetched dependencies under `./deps/` for committing:

```shell
rebar3 vendor store
```

To take the vendored dependencies from `./deps/` and place them under the build directory in the appropriate place:

```shell
rebar3 vendor apply
```

### SVN Dependencies

The [rebar3_svn_deps](https://github.com/seanhinde/rebar3_svn_deps) plugin can allow to use SVN repositories for dependencies:

Since SVN follows somewhat different approaches to branching and hierarchies than Git or Hg dependencies, please [follow the plugin instructions](https://github.com/seanhinde/rebar3_svn_deps#use) to use it.

### Elixir Dependencies

Starting with Rebar3 3.7.0, Mix dependencies are supported with the [rebar_mix](https://github.com/tsloughter/rebar_mix) plugin. Requirements and instructions are detailed in its README page, and include features such as protocol consolidation.

Add the plugin to your rebar config:

```erlang
{plugins, [rebar_mix]}.

{provider_hooks, [{post, [{compile, {mix, consolidate_protocols}}]}]}.
```

The `consolidate_protocols` hook places beams in `_build/<profile>/consolidated` that will need to be included in a release when built. Using:

```erlang
{overlay, [{copy, "{{base_dir}}/consolidated", "releases/{{release_version}}/consolidated"}]}
```

And update your `vm.args.src` to include:

```erlang
-pa releases/${REL_VSN}/consolidated
```

> #### Elixir with Older Rebar3 releases {: .warning}
> For Rebar3 versions prior to 3.7.0, the [rebar3_elixir_compile](https://github.com/barrel-db/rebar3_elixir_compile) plugin was preferred, although it required manually hoisting all transitive dependencies to the project root. Full example and configuration instructions are provided on the plugin's [README page](https://github.com/barrel-db/rebar3_elixir_compile).
