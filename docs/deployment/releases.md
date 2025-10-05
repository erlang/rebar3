# Releases

> #### What are releases and target systems anyway? {: .info}
> A release is a set of applications needed for booting an Erlang VM and starting your project. This is described through a release resource file (`.rel`) which is used to generate a > `.script` and `.boot`. The boot file is the binary form of the script file and is what is used by the Erlang Run-Time System (ERTS) to start an Erlang node, sort of like booting an > operating system. Even running `erl` on the command line is using a boot script.
>
> A target system is an Erlang system capable of being booted on another machine (virtual or otherwise). Often ERTS is bundled along with the target system.
>
> For more information, check out the [chapter on releases](https://adoptingerlang.org/docs/production/releases/) from _Adopting Erlang_.

## Getting Started

Add a `relx` section to your project's `rebar.config`:

```erlang
{relx, [{release, {<release_name>, <release_vsn>},
         [<app>]},
        {release, {<release_name>, <release_vsn>},
         [<app>]},

        {dev_mode, true},
        {include_erts, false},

        {extended_start_script, true}]}.
```

Running `rebar3 release` will build the release and provide a script for starting a node under `_build/<profile>/rel/<release name>/bin/<release name>`.

`<release_name>` must be an atom. `<release_vsn>` can be one of:

| Version type | Result |
| --------------------- | ------------------------------------------------------------- |
| `string()` | A string is used, as is, for the version. Example: `"0.1.0"`|
| `git | semver`  | Uses the latest git tag on the repo to construct the version. |
| `{cmd, string()}`     | Uses the result of executing the contents of `string()` in a shell. Example to use a file `VERSION`: `{cmd, "cat VERSION | tr -d '[:space:]'"}` |
| `{git, short | long}` | Uses either the short (8 characters) or the full Git ref. of the current commit. |
| `{file, File}` | Uses the content of a file. For example, a better way to use a `VERSION` file than using `cmd` would be: `{file, "VERSION"}` |

Each `<app>` is an atom of the app name (e.g., `myapp`), or a tuple. For the tuple syntax of `<app>`, see [App Syntax](#app-syntax).

You can add multiple `release` sections to your project's `rebar.config` under `relx`.

You can either just specify different releases sharing the same configuration:

```erlang
{relx, [{release, {<release name>, "0.0.1"},
         [<app>]},

        {release, {<other release name>, "0.1.0"},
         [<app>]},

         {dev_mode, false},
         {include_erts, true},
       ]}.
```

Or you can also specify releases with independent configurations by using a
4-tuple `release` definition:

```erlang
{relx, [{release, {<release name>, "0.0.1"},
         [<app>],
         [{dev_mode, false},
          {include_erts, true}]},
        {release, {<release name>, "0.1.0"},
         [<app>],
         [{dev_mode, true}]}
       ]}.
```

You can build specific releases using `rebar3 release -n <release_name>`

## Build Configuration

### App Syntax

```erlang
{release, {myrelease, "0.1.0"},
 [<app1>, <app2>]}
```

In a release, each `<app>` is an atom of the app name, or a tuple with the app name and additional options:

```erlang
% No options specified
myapp

% All options specified
{myapp, "1.1.0", transient, [otherapp]}

% Some options specified
{myapp, "1.1.0"}
{myapp, "1.1.0", transient}
{myapp, "1.1.0", [otherapp]}
{myapp, transient}
{myapp, [otherapp]}
{myapp, transient, [otherapp]}
```

In this case, `myapp` is included in the release, its app version is `"1.1.0"`, its start type is `transient`, and its list of included apps is `[otherapp]`. These options directly correspond to the [release resource file (`.rel`)](//erlang.org/doc/man/rel.html):

```erlang
 {<app_name>,       % atom()
  <app_vsn>,        % string()
  <start_type>,     % permanent | transient | temporary | load | none
  <included_apps>}  % [atom()]
```

- Elements after `<app_name>` may be omitted, as long as the order is preserved.
- If `<app_vsn>` is included (uncommon), it must match `.app.src`. Otherwise, `relx` determines it from `.app.src`.
- The release resource file syntax, repeated below for completeness, makes it so that:
    - `<start_type>` defaults to `permanent`.
    - `<included_apps>` defaults to `included_apps` from `.app.src`. If specified, must be a subset `included_apps` from `.app.src`.

### Source Code Inclusion in Release

By default, the release will include source files of your applications, if present.

If you don't want to include the source files, set `include_src` to false.

```erlang
{include_src, false}
```

### Application exclusions

The following allows you to remove specific applications from the output release.

```erlang
{exclude_apps, [app1, app2]}
```

The Applications will be removed from the `.app` file of any Application in the
release that had them listed under `applications`.

### Module exclusions

The following directive allows you to remove application modules from the output release.

```erlang
{exclude_modules, [
    {app1, [app1_mod1, app1_mod2]},
    {app2, [app2_mod1, app2_mod2]}
]}.
```

The modules will be removed from the Application's `.app` file's `module` list.

### Modes

Other modes include `prod` and `minimal`:

```erlang
{relx, [...
        {mode, <mode>},
        ...
       ]
}.
```

| Mode | Expanded Options |
| --------------------- | ------------------------------------------------------------- |
| `dev` | `[{dev_mode, true}, {include_src, true}, {debug_info, keep}, {include_erts, false}]` |
| `prod`  | `[{include_src, false}, {debug_info, strip}, {include_erts, true}, {dev_mode, false}]` |
| `minimal` | `[{include_src, false}, {debug_info, strip}, {include_erts, false}, {dev_mode, false}]` |

While developing you'll likely want all your changes to applications be
immediately available in the release. `relx` provides multiple modes, including
a mode `dev` for this particular use case. Instead of copying the applications
that make up the release to the release structure it creates symlinks, so
compiling and restarting or loading the changed modules is all that is necessary.

The `prod` mode expands to options that are commonly used for production
releases: `include_src` is `false`, so that source code is not included in the release;
`debug_info` is set to `strip`, which makes BEAM files smaller and only removes
data that is used by tools you more than likely aren't including in your
release; and `include_erts` is set to `true`, to bundle in the current Erlang
runtime, making a release you can copy to a compatible target and run without
first installing Erlang.

> #### Rebar3 Prod Profile {: .info}
> When building in the `rebar3` `prod` profile, like with `rebar3 as prod release`
> then the `relx` `prod` mode is enabled automatically.

The `minimal` mode is the same as `prod` except it does not include the Erlang
runtime.

You can override options that the modes expand to by including explicit setting
them. For example, if you wanted to keep the debug info in the BEAM modules
you can use a configuration like:

```erlang
[
  {mode, prod},
  {debug_info, keep}
]
```

## Verification Checks

### Missing Functions

By default `relx` will check that the external functions used in the modules of
the project applications included in a release exist. This means a warning will
be given if a function is called that isn't included in the release, even if it
is a dependency in `rebar.config` or an application included in OTP.

This helps to protect against a common mistake that has bitten us all at some
point, forgetting to add an application to the `.app.src` of the application
that depends on it.

If for some reason you wish to disable this check you can set it to false in the
`relx` config:

```erlang
{check_for_undefined_functions, false}
```

### Stale Modules

Option `src_tests` will issue a warning if the source code for a module is
missing or is newer than the object code:

```erlang
{src_tests, true}
```

This is useful to catch any modifications to dependency source files. Since
`rebar3 release` will automatically compile all changes to the Applications in
your project, the dependencies should be the only modules that could possibly be
stale.

## Runtime Configuration

### VM Configuration

By default `relx` will give a basic `vm.args` file that sets a node name and cookie. For a complete list of options and their use check the [Erlang documentation](https://erlang.org/doc/man/erl.html).

```plain
## Name of the node
-name {{release_name}}@127.0.0.1

## Cookie for distributed Erlang
-setcookie {{release_name}}
```

To provide a custom `vm.args` or `vm.args.src`, simply create the file in the
top level `config/` directory at the root of your project. If you name it
something other than `vm.args` or `vm.args.src` you must add to the `relx`
configuration:

```erlang
{vm_args, "config/vm_prod.args"}
```

or:

```erlang
{vm_args_src, "config/vm_prod.args.src"}
```

### Application Configuration

For passing Application configuration at release runtime there is `sys.config` and `sys.config.src`:

```erlang
[
  {<app_name>, [{<key>, <val>}, ...]}
].
```

If either file `config/sys.config.src` or `config/sys.config` exists in the
project then `relx` will automatically include one of them (`.src` takes
precedence if both exist) in the release.

To set a specific file to use as the Application configuration file it can be
set with either `sys_config` or `sys_config_src`:

```erlang
{sys_config, "config/sys_prod.config"}
```

```erlang
{sys_config_src, "config/sys_prod.config.src"}
```

The files will be renamed to `sys.config` or `sys.config.src` when included into
the release.

If none exists then a file with an empty list is used.

Read more about Erlang configuration in the [config
docs](https://erlang.org/doc/man/config.html) and in the [systools
docs](https://erlang.org/doc/man/systools.html).

### Environment Variable Replacement

#### With OTP-21+ and Rebar3 3.6+

Starting with Erlang/OTP 21 and Rebar3 3.6.0 the configuration options
`sys_config_src` and `vm_args_src` are available for explicitly including
templates that will be rendered at runtime, substituting variables defined
as `${VARIABLE}` with their equivalent value in the shell environment.
Since Rebar3 3.14.0, a default value can be optionally set when using variables
by defining them as `${VARIABLE:-DEFAULT}`.

As of Rebar3 3.14.0 the configs will be included if they exist, so only if the
files are not named `config/sys.config.src` and `config/vm.args.src` do you
need to include `{sys_config_src, <filename>}` or `{vm_args_src, <filename>}`
in the relx config.

```erlang
%% sys.config.src
[
  {appname,
   [
    {port, ${PORT:-8080}},
    {log_level, ${LOG_LEVEL:-info}},
    {log_root, "${LOG_ROOT:-/var/log/appname}"}
   ]}
].
```

```plain
# vm.args.src
-name ${NODE_NAME}
```

```erlang
%% rebar.config
{relx, [{release, {<release name>, "0.0.1"},
         [<app>]},

        {mode, dev}]}.
```

There is no need to set `RELX_REPLACE_OS_VARS=true` when using `.src` files for
configuration. In the following section we'll see older forms of runtime
configuration.

#### Before OTP-21 and Rebar3 3.6

By setting `RELX_REPLACE_OS_VARS=true` both `vm.args` and `sys.config` files may contain OS environment variables that will be replaced with the current value from the environment the node is started in. This means a `vm.args` and `sys.config` for a release that starts a web server listening on a port could look like:

```plain
# vm.args
-name ${NODE_NAME}
```

```erlang
%% sys.config
[
 {appname, [{port, "${PORT}"}]}
].
```

And then be used to start multiple nodes of the same release with different name.

```bash
#!/bin/bash

export RELX_REPLACE_OS_VARS=true

for i in `seq 1 10`;
do
    NODE_NAME=node_$i PORT=808$i _build/default/rel/<release>/bin/<release> foreground &
    sleep 1
done
```

### Overlays: Build-Time Configuration

Overlays provide the ability to define files and templates to include in the target system. For example, custom scripts for managing your node or the Procfile needed for running on Heroku.

```erlang
{relx, [
    ...
    {overlay_vars, "vars.config"},
    {overlay, [{mkdir, "log/sasl"},
               {template, "priv/app.config", "etc/app.config"},
               {copy, "Procfile", "Procfile"}]}
]}.
```

The supported actions are:

- `mkdir` to create a directory within the release
- `copy` to copy a file from a local directory to a location within the release
- `template` to behave the way `copy` does, but with variable expansion in it.

Relx's templating exposes variables along with the full power of a Mustache templating system (see [mustache](https://github.com/soranoba/mustache)). You can look at the documentation there for the full syntax supported.

There is a set of variables made available by default which are described in the next session, and custom variables can otherwise be declared in the file specified in `{overlay_vars, "vars.config"}`, which should have the following format:

```erlang
%% some variables
{key, value}.
{other_key, other_val}.
%% includes variables from another file
"./some_file.config".
```

The default variables are defined below.

#### Predefined Overlay variables

| Name | Description |
|------|-------------|
| `log` | The current log level in the format of `(<logname>:<loglevel>)` |
| `output_dir` | The current output directory for the built release |
| `target_dir` | The same as `output_dir`; exists for backwards compatibility |
| `overridden` | The current list of overridden apps (a list of app names) |
| `goals` | The list of user-specified goals in the system |
| `lib_dirs` | The list of library directories, both user-specified and derived |
| `config_file` | The list of config file used in the system |
| `providers` | The list of provider names used for this run of `relx` |
| `sys_config` | The location of the `sys.config` file |
| `root_dir` | The root dir of the current project |
| `default_release_name` | The current default release name for the `relx` run |
| `default_release_version` | The current default release version for the `relx` run |
| `default_release` | The current default release for the `relx` run |
| `release_erts_version` |  The version of the Erlang Run-Time System in use |
| `erts_vsn` | The same as `release_erts_version` (for backwards compatibility) |
| `release_name` | The currently executing release |
| `release_version` | The currently executing version |
| `rel_vsn` | Same as `release_version`. Exists for backwards compatibility |
| `release_applications` | A list of applications included in the release |

##### Splitting configurations

It is possible to split overlay files to deal with more complex situations. To explain this lets look at a simplified example.

We build our application and want to distinguish between production and developing builds by having the overlay variable `build` spell out either `"prod"` or `"dev"` so the `app.config` file could include it in its configuration and we can either enable or disable features.

For this we build three overlay files:

- `dev.config`
- `prod.config`
- `base.config`

For dev builds we will use `dev.config` as `overlay_vars` and for prod we will be using `prod.config`.

```erlang
%% base.config
{data_dir, "/data/yolo_app"}.
{version, "1.0.0"}.
{run_user, "root"}.
```

```erlang
%% dev.config
%% Include the base config
"./base.config".
%% The build we have
{build, "dev"}.
```

```erlang
%% prod.config
%% Include the base config
"./base.config".
%% The build we have
{build, "prod"}.
```

## Deployable Tarball

### With ERTS Included

A target system can not have symlinks like those created when using `dev_mode` and often we want to include ERTS along with the system so it does not need to be previously installed on the target.

Rebar3 will automatically add `{mode, prod}` to the Relx configuration if the
`prod` profile is used to build the release. For example:

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

Now a tarball `myrel-0.1.0.tar.gz` can be copied to another compatible system and booted:

```shell
$ mkdir myrel
$ mv myrel-0.1.0.tar.gz myrel/
$ cd myrel
$ tar -zxvf myrel-0.1.0.tar.gz
$ bin/myrel console
```

### Without ERTS

When it is required to leave ERTS out of a release the `prod` profile
configuration can be set in `rebar.config` under `profiles`. For example, to use
the ERTS and base applications like `kernel` and `stdlib` on the target, set
`mode` to `minimal` and `system_libs` to `false` in the `relx` configuration tuple:

```erlang
{profiles, [{prod, [{relx, [{mode, minimal},
                            {system_libs, false}]}]}]}.
```

Or manually set `include_erts` to `false`:

```erlang
{profiles, [{prod, [{relx, [{include_erts, false},
                            {system_libs, false}]}]}]}
```

Now when running `rebar3 as prod tar` the generated tarball will not include
ERTS or Applications like `kernel` and `stdlib`.

### With ERTS Built for Another System

If you wish to include an Erlang Run Time System that is not the version you are using to run `rebar3`, for example you are building on MacOS but wish to include an ERTS that was built for a version of GNU/Linux, you can supply a path instead of a boolean for `include_erts` and provide a path for `system_libs`, still within the `relx` configuration tuple:

```erlang
{include_erts, "/path/to/erlang"},
{system_libs, "/path/to/erlang"},
```

Using these paths with profiles can yield easier ways to set up cross-compiling.

## Extended Start Script

### Commands

The extended start script that comes with `relx` provides a few ways of starting
and connecting to your release.

For local development you'll likely use `console`. In production you'll want
`foreground`, no matter if you are starting manually in something like `tmux`,
using an init system like `systemd` or running the release in a Docker container.

To open a console on a node started with `foreground` use `remote_console`.

A full list of commands is below.

| Command | Description |
| --------------------- | ------------------------------------------------------------- |
| `foreground` | Start release with output to stdout |
| `remote` | Connect remote shell to running node |
| `console` | Start the release with an interactive shell |
| `console_clean` | Start an interactive shell without the release's applications |
| `rpc [Mod [Fun [Args]]]]` | Run `apply(Mod, Fun, Args)` on running node |
| `eval [Exprs]` | Run expressions on running node |
| `status` | Verify node is running and then run status hook scripts |
| `restart` | Restart the applications but not the VM |
| `reboot` | Reboot the entire VM |
| `stop` | Stop the running node |
| `pid` | Print the PID of the OS process |
| `ping` | Print pong if the node is alive |
| `daemon` | Start release in the background with `run_erl` (named pipes) |
| `daemon_attach` | Connect to node started as daemon with `to_erl` (named pipes) |

### Release Handling: Install and Upgrade

Additionally, the extended start script contains commands for using
[release_handler](https://erlang.org/doc/man/release_handler.html):

| Command | Description |
| --------------------- | ------------------------------------------------------------- |
| `unpack [Version]` | Unpack a release tarball |
| `install [Version]` | Install a release |
| `uninstall [Version]` | Uninstall a release |
| `upgrade [Version]` | Upgrade the running release to a new version |
| `downgrade [Version]` | Downgrade the running release to a new version |
| `versions` | Print versions of the release available |

To understand how these work see the OTP Design Principles chapter [Release
Handling](https://erlang.org/doc/design_principles/release_handling.html).

For a detailed workflow including version increments and appup generation
checkout Richard Jones [relflow](https://github.com/RJ/relflow) tool built
around `rebar3`.

For the basic release upgrade after installation of a release assume we have a release named `myrel` with a version `0.0.1` and `0.0.2`:

- Installing: Installing a release on a running system will unpack and upgrade the version: `bin/myrel install 0.0.1`
- Listing: you can inspect what versions are currently available: `bin/myrel versions`
- Upgrading: If the version is already unpacked you can simply call `upgrade` to upgrade to the version: `bin/myrel upgrade 0.0.2`
- Downgrading: To downgrade to the previous version use the `downgrade` command: `bin/myrel downgrade 0.0.1`

### Hooks

It is possible to define hooks on specific operations of the extended start script, the operations are `start`, `stop`, and `install_upgrade`; `pre` and `post` hooks for each of these operations are available.

The hooks can either be built-in (ie. they are already included in the release) or custom (scripts written by the user for custom functionality). The built-in scripts, offering pre-packaged functionality, are:

- *pid* : Writes the BEAM pid to a configurable file location (`/var/run/<rel_name>.pid` by default).
- *wait_for_vm_start* : Waits for the VM to start (ie. when it can be pinged).
- *wait_for_process* : Waits for a configurable name to appear in the Erlang process registry.

```erlang
{extended_start_script_hooks, [
  {pre_start, [{custom, "hooks/pre_start"}]},
  {post_start, [
    {pid, "/tmp/foo.pid"},
    {wait_for_process, some_process},
    {custom, "hooks/post_start"}
  ]},
  {pre_stop, [
    {custom, "hooks/pre_stop"}]},
    {post_stop, [{custom, "hooks/post_stop"}]},
  ]},
  {post_stop, [{custom, "hooks/post_stop"}]}
]}.
```

### Extensions

The extended start script that is generated comes with a built-in set of commands that allows you to manage your release: `foreground`, `stop`, `restart`, etc.

Sometimes it's useful to expose some custom commands that are specific to your application. For example, if you're running a game server it would be convenient to just call `bin/gameserver games` that outputs useful information.

Extended start script extensions allow you to create a custom shell script that gets appended to the list of commands available to your start script. The extension shell script can take arguments and has access to all shell variables defined in the start script itself. You begin by defining the extension in your `rebar.config`, for example:

```erlang
%% start script extensions
{extended_start_script_extensions, [
   {status, "extensions/status"}
]}.
```

Here you are adding the `status` script extension that will invoke an `extensions/status` shell script.

This path is relative to the location of the start script on the generated release so you probably will want to use a `overlay` to place it at the correct location:

```erlang
{copy, "scripts/extensions/status", "bin/extensions/status"},
```

The extension script itself is standard shell script, the game server example described could be implemented in the following way:

```bash
#!/bin/bash

case $1 in
    help)
        echo "bin/gameserver status"
        ;;
    *)
        ;;
esac

# get the status tuple from gameserver
Status=$(relx_nodetool eval "pool_debug:status(json).")

# now print it out
code="Json = binary_to_list($Status),
      io:format(\"~p~n\", [Json]),
      halt()."
echo $(erl -boot no_dot_erlang -sasl errlog_type error -noshell -eval "$code")
```

## Other Configurations

The `RELX_RPC_TIMEOUT` environment value can be set on the target system running a release to choose how long the scripts can wait before giving up on contacting the running Erlang system. It defaults to the `NODETOOL_TIMEOUT` value (converted from milliseconds to seconds) if no value is specified. If `NODETOOL_TIMEOUT` itself is not set, the default is 60 seconds.

## References

- [relx](https://github.com/erlware/relx)
- [relflow](https://github.com/RJ/relflow)
- [Releases](https://adoptingerlang.org/docs/production/releases/) chapter from Adopting Erlang
- [Releases](https://learnyousomeerlang.com/release-is-the-word) chapter from Learn You Some Erlang
- OTP [release](https://www.erlang.org/doc/design_principles/release_structure.html) documentation
- OTP [target system](https://www.erlang.org/doc/system_principles/create_target.html) documentation
