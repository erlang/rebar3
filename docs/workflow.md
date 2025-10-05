# Workflow

> Recommended steps and possible default configurations for various tasks.

If you know the basic commands, have gone through [Getting Started](getting-started.md), [Basic Usage](basic_usage.md), and have a brief understanding of [Releases](deployment/releases.md), the next step is possibly to figure out a workflow for your project and your team.

This section is a work in progress of various recommended steps and possible default configurations for various tasks in order to have a good experience with the Erlang toolchain.

## Pick the Right Type of Project

|Type of Project|Recommended Template|Comments|
|----|----|----|
|A short script or util|escript|The user you distribute this to will need to have Erlang installed. Dependencies in C are neither trivially included nor redistributed|
|A full, self-contained, executable system|release or umbrella|This is the recommended production deploy for Erlang systems. See the [Releases](deployment/releases.md) section for more details on releases|
|A library to be used in other systems|lib or app|use `lib` for stateless libraries that contain modules, and `app` for stateful libraries with a supervision tree|
|A collection of multiple libraries|umbrella|This is the one form of project supported where multiple top-level apps are used. These projects generally cannot be used as dependencies. For projects usable as dependencies, see [how to declare`git_subdir` dependencies](configuration/dependencies.md#declaring-dependencies)|

## Setting up Dependencies

The basic configuration of a project should do at least two things:

1. Always track the `rebar.lock` file

2. Ignore the `_build` directory

Tracking the lock file will let you have repeatable builds, and will allow Rebar3 to do things like automatically re-update dependencies when switching branches.

> #### The _build directory should be safe to delete but you shouldn't need to {: .warning}
> Rebar3 tracks all the applications declared in your `rebar.config` files and should be able to track all required changes.
>
> There are a few edge cases where this is not possible and may lead to weird bugs, specifically when you are changing your project structure. If you are moving from a single-app project to an umbrella project (i.e. all source files move from `src/` to `apps/myapp/src`) or the opposite, chances are that various artifacts in the `_build` directory will conflict with each other. Delete it and ask for a fresh build, in this case.

The next thing you'll want to do is add dependencies to your project. See the [Dependencies](configuration/dependencies.md) section for this. Adding dependencies does not automatically integrate them into your project, however.

The `{deps, [...]}` configuration value tells Rebar3 which dependencies to fetch and download and track, but that's as far as it goes. You must then configure your application to make use of the dependency:

- If the dependency is needed at runtime by your application in order for it to work (e.g. you need a web server or call the library directly), add it to your application's `.app.src` file under the `{applications, [stdlib, kernel, ...]}` tuple. This will let the Erlang VM know not to boot your app without the dependency being present
- If the dependency is only needed for releases (for example, `observer` or `recon`, which are debugging tools that your application likely does not depend on but you'd like to bundle it with), then you need to add that application explicitly to the `{releases, ...}` configuration tuple. Any dependency on that tuple will have its transitive dependencies included as well.

Other build tools tend to make no distinction between these types of project inclusions, but Rebar3 tries to be strict with regards to what should be included or not. It can let you make specific builds, such as escripts or test releases that won't require specific toolchains like the Wx graphical toolkit in order to run.

## Upgrading Dependencies

There are two steps required when upgrading dependencies:

1. Update the index cache

2. Update the dependency itself

The first step is required because Rebar3 keeps a cache of the Hex.pm repository packages and versions that you have fetched before. This lets the build tool run faster without useless calls to the network when all known and required versions are present on your computer. However, when a new version exists, Rebar3 won't automatically know about it. To tell it to go fetch new version definitions of known packages, call:

```shell
$ rebar3 update
```

This will bring Hex packages up to date, but will not modify the existing project.

The only way to change the existing project definition is by modifying the lock file. This is easily done by calling:

```shell
$ rebar3 upgrade <depname>
```

This will update the lock file definition, and on the next build, the new copy will be fetched and compiled. If transitive dependencies have been upgraded as well, this will be detected and handled.

You should avoid deleting the lock file when possible, and if you need to upgrade multiple dependencies, you can call `rebar3 upgrade dep1,dep2,dep3`. If you want to update all dependencies, call `rebar3 upgrade --all`, which can be fine on small projects, but you may want to do things progressively on larger projects.

## Create Aliases for common tasks

More complex projects may run multiple tools. For example, you may want to run `xref` to find dead code, `dialyzer` for type analysis, `ct` for Common Test suites, `cover` for coverage analysis, and so on.

Rather than calling all the tasks by hand, an alias can create a simple command to run multiple tasks:

```erlang
{alias, [
    {check, [xref, dialyzer, edoc,
             {proper, "--regressions"},
             {proper, "-c"}, {ct, "-c"}, {cover, "-v --min_coverage=80"}]}
]}.
```

This configuration will allow to call `rebar3 check`, which will run the following in order:

- `xref` analysis for dead code and calls to functions that don't exist
- `dialyzer` checks for inconsistencies and type errors
- build the project documentation, to make sure it can do so without errors
- run regression tests in `proper` (using [the PropEr plugin](configuration/plugins.md#proper))
- run regular properties in PropEr while compiling with coverage analysis
- run Common Test test suites while compiling with coverage analysis
- run `cover` analysis, outputting the results to the shell. This alias also ensures that if code coverage dips below 80%, the command fails

As soon as a task fails, the whole command is interrupted. You can adapt the aliases to your needs.

> #### Optimize for task delays {: .info}
> A tip to save you time is to run tasks that are short first. For example, `xref` will find some issues that `dialyzer` also finds, but is a ton faster. Running `xref` before Dialyzer in your aliases will give you a faster feedback loop.

## Recommended Configurations for Various tools

Some of the Rebar3 configurations and defaults can be either too permissive or too restrictive. However, due to commitments to backwards compatibility, we can't always change and adapt them since it would risk breaking projects that relied on these specific configurations.

The following is a collection of a few configurations that can be useful as new defaults when starting a new project.

```erlang
{dialyzer, [
    {warnings, [
       %% Warn about undefined types and unknown functions
       unknown
    ]}
]}.

{xref_checks,[
    %% enable most checks, but avoid 'unused calls' which is often
    %% very verbose
    undefined_function_calls, undefined_functions, locals_not_used,
    deprecated_function_calls, deprecated_functions
]}.

{profiles, [
    {test, [
        %% Avoid warnings when test suites use `-compile(export_all)`
        {erl_opts, [nowarn_export_all]}
    ]}
]}.
```
