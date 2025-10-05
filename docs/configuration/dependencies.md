# Dependencies

> How to add dependencies to a project.

## Declaring Dependencies

Dependencies can be declared in a top-level `rebar.config` file, and inspected with the `rebar3 tree` command.

In general, Rebar3 supports two types of dependencies:

- Source dependencies (Git, Mercurial)
- Package dependencies

Both types of dependencies work roughly the same way. Rebar3 uses [hex.pm](https://hex.pm) to provide a managed set of packages and their dependencies. They will generally be faster (behind a CDN), can be mirrored, and will be cached locally in `~/.cache/rebar3/`.

All dependencies are to be project-local. This is usually a good choice in order avoid the common problems of global libraries having version conflicts. It also helps with the general Erlang mechanism of [Releases](deployment/releases.md), which builds standalone systems.

Dependencies fit any of the following formats:

```erlang
{deps,[
  %% Packages
  rebar,
  {rebar,"1.0.0"},
  {rebar, {pkg, rebar_fork}}, % rebar app under a different pkg name
  {rebar, "1.0.0", {pkg, rebar_fork}},
  %% Source Dependencies
  {rebar, {git, "git://github.com/erlang/rebar3.git"}},
  {rebar, {git, "https://github.com/erlang/rebar3.git"}},
  {rebar, {git, "git@github.com:erlang/rebar3.git"}},
  {rebar, {hg, "https://othersite.com/erlang/rebar3"}},
  {rebar, {git, "git://github.com/erlang/rebar3.git", {ref, "aef728"}}},
  {rebar, {git, "git://github.com/erlang/rebar3.git", {branch, "master"}}},
  {rebar, {git, "git://github.com/erlang/rebar3.git", {tag, "3.0.0"}}},
  %% Source dependencies (git only) in subdirectories, from version 3.14 onwards
  {rebar, {git_subdir, "git://github.com/erlang/rebar3.git", {branch, "main"}, "subdir"}},
  {rebar, {git_subdir, "git://github.com/erlang/rebar3.git", {tag, "3.14"}, "sub/dir"}},
  {rebar, {git_subdir, "git://github.com/erlang/rebar3.git", {ref, "aeaefd"}, "dir"}}
]}.
```

As the example above shows, for the current versions, only packages, git sources, and mercurial sources are supported. Custom dependency sources can be added by [implementing the resource behaviour](extending/custom_dep_resources.md) and including it like a plugin.

### Runtime Dependencies

However, the dependency handling done by Erlang/OTP to boot and shut down applications, and tools (even part of Rebar3) to build releases and scripts, depend on a more granular dependency declaration, specifying which of each application in a project depend on others.

You should add each dependency to your `app`  or `app.src` files:

```erlang
{application, <APPNAME>,
 [{description, ""},
  {vsn, "<APPVSN>"},
  {registered, []},
  {modules, []},
  {applications, [kernel
                 ,stdlib
                 ,cowboy
                 ]},
  {mod, {<APPNAME>_app, []}},
  {env, []}
 ]}.
```

This will allow the flexibility to write and generate software where various disjoint applications can coexist in a virtual machine without their dependencies being entirely tangled together. For example, you may want your web server to be able to run independently of administrative and debugging tools, even if they should be available in production.

If more formats require support, Rebar3 can be extended via the [`rebar_resource` behaviour](https://github.com/erlang/rebar3/blob/main/apps/rebar/src/rebar_resource.erl), and sent to the maintainers with a [pull request](https://github.com/erlang/rebar3/blob/master/CONTRIBUTING.md).

> #### Dependencies and Profiles {: .warning}
> Dependencies will always be compiled with the `prod` profile applied to their configuration. No other profile (besides `default`, of course) is used on any dependency. Even though they are configured for `prod` the dependency will still be fetched to the profile directory for the profile it is declared under. For example, a dependency in the top level `deps` will be under `_build/default/lib` and dependency under the profile `test` will be fetched to `_build/test/lib`, and both will be compiled with their `prod` profile configuration applied.

## Dependency Version Handling

Rebar3 considers dependency versions to be informational only. Given the existing open source landscape in the Erlang community when Rebar3 was added, trying to impose [semantic versioning](https://semver.org/) or any other similar scheme was considered unpractical:

- People update *some* versions but not all of them (Git tags vs. branch names vs. OTP application versions) and they may contradict each other;
- Some people never update their versions and publish many times under them;
- Not everyone subscribes to the same version schemes;
- People make errors in subscribing to semantic versioning;
- Many applications are stuck in versions smaller than `1.0.0`, and therefore considered unstable forever;
- Source dependencies are frequently used: figuring out version conflicts therefore requires downloading all transitive dependencies from all dependencies to figure out whether they conflict or not, every time, and is costly;
- Strict adherence to semantic versioning ends up causing false conflicts where using a subset of an API which hasn't changed across major versions still requires manual conflict resolution and dependency audits.

Instead, Rebar3 will fetch and download dependencies in a [level-order traversal](#fetching-order). This means that the dependencies closest to the root of the dependency tree are those that will be chosen, regardless of their versions. Any dependency declared in your project's `rebar.config` will never be overwritten by a transitive dependency, and a transitive dependency will never be overridden by a later encountered conflicting transitive dependency.

This also means that if you prefer a version to be used above all else, you can just add it to your `rebar.config` file and pick what will be kept; the same conflict resolution mechanism sometimes required by semantic versioning.

In practice, this has proven to be absolutely adequate as a mechanism.

After each run of dependency fetching and resolving the list of final dependencies are written to `rebar.lock`.

> #### Treating Conflicts as Errors {: .info}
> If you ever want Rebar3 to abort as soon as it detects a dependency conflict, instead of skipping the file and proceeding as usual, add the line `{deps_error_on_conflict, true}.` to your rebar configuration file.

For convenience reasons (and because [hex.pm](https://hex.pm) mandates semver) hex dependencies can be specified using semver-like syntax:

```erlang
{deps,[
  rebar,                   % fetches latest known version, ignoring pre-releases
  {rebar, "~> 2.0.0"},     % >= 2.0.0 and < 2.1.0`
  {rebar, "~> 2.1.2"},     % >= 2.1.2 and < 2.2.0`
  {rebar, "~> 2.1.3-dev"}` % >= 2.1.3-dev and < 2.2.0`
  {rebar, "~> 2.0"}`       % >= 2.0.0 and < 3.0.0`
  {rebar, "~> 2.1"}`       % >= 2.1.0 and < 3.0.0`
]}.
```

To get the latest version of packages available, call:

```shell
$ rebar3 update
===> Updating package index...
```

To use a CDN other than the default, such as one of the [official mirrors](https://hex.pm/docs/mirrors) add to your project's `rebar.config` or to `~/.config/rebar3/rebar.config`:

```erlang
{rebar_packages_cdn, "https://s3-eu-west-1.amazonaws.com/s3-eu.hex.pm"}.
```

## Checkout Dependencies

To handle the case of dependencies you wish to work on locally without having to constantly publish new versions, there is the `_checkouts` directory. Simply make a symlink or copy your dependency to `_checkouts` at the top level of your project:

```shell
_checkouts
└── depA
    └── src
```

Any application or plugin in `_checkouts` will take precedence over the same application if it is additionally listed in the `rebar.config`'s `deps`, `plugins`, or `project_plugins`. This also overrides anything already fetched to `_build`.

Note that `_checkouts` is an override, this means that for it to work a `dep` or `plugin` entry in `rebar.config` _must_ exist.

## Fetching Order

For a regular dependency tree, such as:

```plain
  A
 / \
B   C
```

the dependencies `A`, `B`, and `C` will be fetched.

However, for more complex trees, such as:

```plain
   A
 /   \
B    C1
|
C2
```

The dependencies `A`, `B`, and `C1` will be fetched. When Rebar3 will encounter the requirement for `C2`, it will instead display the warning: `Skipping C2 (from $SOURCE) as an app of the same name has already been fetched`.

Such a message should let the user know which dependency has been skipped.

What about cases where two transitive dependencies have the same name and are on the same level?

```plain
   A
 /   \
B     C
|     |
D1    D2
```

In such a case, `D1` will take over `D2`, because `B` lexicographically sorts before `C`. It's an entirely arbitrary rule, but it is at least a rule that ensures repeatable fetches.

In the event users disagree with the outcome, they can bring `D2` to the top level and ensure it will be chosen early:

```plain
  A D2
 /   \
B     C
|     |
D1    D2
```

Which will yield `A`, `B`, `C`, and `D2`.

Rebar3 will perform that same algorithm with packages, and will also detect circular dependencies and error out on these.

Dependencies in the `_checkouts` directory will be left untouched, and are treated as top-level OTP applications.

## Lock Files

Lock files (`rebar.lock`) are one of the only artifacts that will be generated by Rebar3 that will live outside of `_build/`. They should always be checked into source control. The lock file contains information regarding code dependencies, including immutable references for source dependencies like those in git, and their versions along with expected hashes for packages (which can be used to protect against mirrors being hijacked).

The objective is to use more accurate information regarding found dependencies than what would be obtained through the config file on its own, allowing, for example, to configure a dependency to be updated from `main` on-demand, but to be locked to a stable tested version in the meanwhile. Only unlocking or upgrading the dependency will allow it to move it to a newer or different version. The idea is to allow repeatable builds, even if, for example, Git tags or branches are destructively modified by someone.

Rebar3 will also use the lock file as the true source of authority for dependencies when switching branches or fetching transitive deps (it will pick data from the lock file if any is available) rather than the `rebar.config` file. This way, we can carry a safe tested state into other applications when loose references or versions are used in the `rebar.config` file.

The format is expected to be forwards and backwards compatible. Rebar3 will annotate lock file versions according to the format of metadata stored, and warn when an old version of Rebar3 is used to read a newer version of lock files. This can tell the user that some metadata that is judged important at a later date will be lost by using an older copy of the tool.

## Upgrading Dependencies

Whenever a dependency is fetched and locked, Rebar3 will extract a reference from the source file to pin it to a specific version in time. The dependency should be forced to follow that version on following builds.

Rebar3 can upgrade previously installed dependencies to newer versions in two ways: forwarding a branch's reference to its latest version (e.g. an old `main` branch up to the new `main`'s `HEAD` for source files, or the newest versions for packages that didn't specify one), or crushing an existing locked dependency with a new version from the `rebar.config` file.

In the following dependency tree:

```plain
A  B
|  |
C  D
```

The user can upgrade either dependency (`rebar3 upgrade A` and `rebar3 upgrade B`) or both at once (`rebar3 upgrade A,B` or `rebar3 upgrade`, which upgrades *all* dependencies).

Upgrading only `A` means that `A` and `C` may be upgraded. Upgrades to `B` and `D` will be ignored.

Upgrading dependencies can have surprising effects and interesting corner cases. Consider the following dependency tree:

```plain
    A       B       C1
   / \     / \     / \
  D   E   F   G   H   I2
  |   |
  J   K
  |
  I1
```

After fetching the dependency tree above, `I2` would be chosen before `I1` since it is closer to the project root. However, following an upgrade from `C1` to `C2` where `C2` no longer needs to depend on `I2`, Rebar3 will automatically go fetch `I1` under the `A` tree (even if no upgrade in `A` was required) to provide a correct new tree. The upgraded tree would look like:

```plain
    A       B     C2
   / \     / \    |
  D   E   F   G   H
  |   |
  J   K
  |
  I1
```

Where `I2` no longer exists in the project, and `I1` now does.

Calling `rebar3 unlock` will flush the lock file entirely.

You can inspect things manually with `rebar3 tree`, which will display the current dependencies tree:

```shell
$ rebar3 tree
...
├─ bootstrap-0.0.2 (git repo)
├─ dirmon-0.1.0 (project app)
├─ file_monitor-0.1 (git repo)
├─ peeranha-0.1.0 (git repo)
│  ├─ gproc-git (git repo)
│  ├─ interclock-0.1.2 (git repo)
│  │  ├─ bitcask-1.7.0 (git repo)
│  │  │  └─ lager-2.1.1 (hex package)
│  │  │     └─ goldrush-0.1.6 (hex package)
│  │  └─ itc-1.0.0 (git repo)
│  └─ merklet-1.0.0 (git repo)
├─ recon-2.2.2 (git repo)
└─ uuid-1.5.0 (git repo)
   └─ quickrand-1.5.0 (git repo)
```

## Elixir Dependencies

Elixir Dependencies are supported starting with Rebar3 version 3.7.0, and Elixir version 1.7.4 through the use of a plugin. See [the relevant plugin section](configuration/plugins.md#elixir-dependencies) for details.
