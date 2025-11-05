# Commands

> Usage of each Rebar3 command.

Each command represents a task which runs one or more providers to fulfill the task.

## alias

List aliases' definitions (from `rebar.config`).

Example output:

```console
test=eunit,ct --suite=rebar_alias_SUITE,cover
check=xref,dialyzer
```

## as

Higher order task which takes a profile name and list of tasks to run under that profile.

## compile

After ensuring all dependencies are available, and fetching them if they are not, compile will compile the needed dependencies and the project's apps' `.app.src` and `.erl` files.

| Option           | Type | Description                                              |
| ---------------- | ---- | ---------------------------------------------------------|
| `-d/--deps_only` | none | Only compile dependencies, no project apps will be built |

## completion

> #### Since version 3.23.0 {: .info}

Generates a completion file for one of the supported shells: `bash`, `zsh`.

Completion files can be generated based on the project setup, so autocompletion also works for all plugins used in the project, not just for default providers.

To use generated completion files run `source path/to/generated/file`.

| Option | Type | Description |
| ------- | ------- | --------- |
| `-a/--aliases` | Comma separated list of strings | OS level aliases on which rebar3 completion will be triggered (e.g. `"rebar, r3"`) |
| `-f/--file` | string | Completion file name. If not absolute, it's relative to the `_build/<profile>/` directory |
| `-s/--shell` | atom | Specify type of the completion file. By default it is autodected using `$SHELL` variable. Valid values are `bash` and `zsh`. |

> #### zsh specific requirement {: .warning}
> Make sure that `autoload -Uz compinit; compinit` is called in `.zshrc` file.

For better user experience, set default autocompletion and override it when needed.
Default autocompletion can be set by generating a global completion file and loading it in `.bashrc` or `.zshrc`.

To generate a global (project-independent) completion file run `rebar3 completion --file path/to/global/completion/file` outside a `rebar3` project.



## clean

Removes compiled BEAM files from apps.

The `clean` command by default removes the BEAM files for top-level applications. It does so while respecting profiles, which means that 'rebar3 clean' will only clean the default profile, and 'rebar3 as test clean' will only clean the test profile.

| Option         | Type                            | Description                                           |
| -------------- | ------------------------------- | ------------------------------------------------------|
| `--all/-a`     | none                            | Clean all apps, including the dependencies            |
| `--apps`       | Comma separated list of strings | Clean a specific list of apps or dependencies         |
| `--profile/-p` | string                          | Specify a profile (alternative to `rebar3 as  clean`) |

## ct

Runs common tests for the project located under the `test/` directory.

Most Common Test [options](https://www.erlang.org/doc/man/ct_run.html) as described in the Erlang documentation for `ct_run` are available. Some common ones are described below:

| Option                        | Type                                         | Description                                                                                                                                 |
| ----------------------------- | -------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| `--dir`                       | Comma separated list of strings              | Compile and run all test suites in the specified directories.                                                                               |
| `--suite`                     | Comma separated list of strings              | Compile and run all test suites specified. Must be specified by full path, either absolute or relative to the current directory.            |
| `--group`                     | Comma separated list of strings              | Test groups to run. See the [Common Test Documentation.](https://erlang.org/doc/apps/common_test/index.html)                                |
| `--case`                      | Comma separated list of strings              | List of test cases to run. See the [Common Test Documentation.](https://erlang.org/doc/apps/common_test/index.html)                         |
| `--spec`                      | Comma separated list of strings              | List of [Test Specifications](https://erlang.org/doc/apps/common_test/run_test_chapter.html#test_specifications)                            |
| `--join_specs`                | Comma separated list of strings              | Like `--spec` but merges all the specifications into one and does a single run.                                                             |
| `--repeat`                    | Integer                                      | How often to repeat the tests                                                                                                               |
| `--duration`                  | String (format: HHMMSS)                      | Max allowed duration of the test run                                                                                                        |
| `--until`                     | String (format: HHMMSS)                      | Time until which to run the tests                                                                                                           |
| `--force_stop`                | `true \| false \| skip_rest`                   | Force termination on test timeout                                                                                                         |
| `--multiply_timetraps`        | Integer                                      | Extends the timeout values for tests by a given multiplier value                                                                            |
| `--scale_timetraps`           | Boolean                                      | Enables automatic timeout value scaling, when using code coverage or tracing                                                                |
| `--abort_if_missing_suites`   | Boolean                                      | Abort the test run if a test suite is missing (Default: true)                                                                               |
| `--sys_config`                | String                                       | List of OTP application config files (like `sys.config`) that should be applied by Rebar3 before the test run.                              |
| `--config`                    | Comma separated list of strings              | Config files to use when running tests. See the [Common Test Documentation.](https://erlang.org/doc/apps/common_test/index.html)            |
| `--allow_user_terms`          | Boolean                                      | Allow user defined config values in config files. See the [Common Test Documentation.](https://erlang.org/doc/apps/common_test/index.html)  |
| `--decrypt_key`               | String                                       | If the configuration file is encrypted, set the key to decrypt it                                                                           |
| `--decrypt_file`              | String                                       | If the configuration file is encrypted, point to the file containing the key to decrypt it                                                  |
| `--logdir`                    | String                                       | The directory in which test logs will be written. See the [Common Test Documentation.](https://erlang.org/doc/apps/common_test/index.html) Default: `_build/test/logs` |
| `--logopts`                   | Comma separated list of strings              | Set common test logging options. See the [Common Test Documentation.](https://erlang.org/doc/apps/common_test/index.html) Default: `_build/test/logs` |
| `--readable`                  | Boolean                                      | Adds test names with results on a per-test basis, and only displays common-test logs in the terminal on failing tests. Default: `true`   |
| `--verbose`, `-v`             | Boolean                                      | Enable verbose output. Default: false                                                                                                       |
| `--verbosity`                 | Integer                                      | Set the level of Common Test verbosity                                                                                                      |
| `--cover`, `-c`               | Boolean                                      | Generate cover data                                                                                                                         |
| `--cover_export_name`         | String                                       | Change the name of the code coverage file                                                                                                   |
| `--label`                     | String                                       | Set a test label                                                                                                                            |
| `--basic_html`                | Boolean                                      | show basic HTML                                                                                                                             |
| `--stylesheet`                | String                                       | CSS stylesheet to apply to HTML output                                                                                                      |
| `--create_priv_dir`           | `auto_per_run \| auto_per_tc \| manual_per_tc` | change the behaviour of the private (scratch) directories creation done by Common Test                                                    |
| `--include`                   | String                                       | Additional directories containing include files. Option added for parity with ct_run, usually rebar3 should take care of include file paths |
| `--name`, `--sname`           | String                                       | Start a distributed node with a given name                                                                                                  |
| `--setcookie`                 | String                                       | Set a value for the distributed cookie                                                                                                      |
| `--compile_only`              | Boolean                                      | Compile the project with the test configuration specified, but without running the tests                                                    |

Runs in the `test` profile.

## cover

Performs coverage analysis on modules called by Common Test or Eunit test suites. Call as `rebar3 do ct, cover`, `rebar3 do eunit, cover` or the combination of both with `rebar3 do eunit, ct, cover` while the `{cover_enabled, true}` option is in your rebar config file, or if the cover flags were used with these commands individually.

An HTML report is generated.

| Option                 | Type    | Description                                                |
| ---------------------- | ------- | ---------------------------------------------------------- |
| `-m`, `--min_coverage` | Integer | Mandate a coverage percentage required to succeed (0..100) |
| `--reset`, `-r`        | none    | Resets all cover data                                      |
| `--verbose`, `-v`      | none    | Prints coverage analysis in the terminal.                  |

Specific modules can be blacklisted from code coverage by adding `{cover_excl_mods, [Modules]}` to the config file. Specific applications can be blacklisted by adding `{cover_excl_apps, [AppNames]}` to the config file.

## deps

Lists dependencies, whether they're source or package dependencies, and whether they're locked or not. Those that are locked but not matching the lock file are followed by an asterisk (`*`).

## do

Higher order provider for running multiple tasks in a sequence, separated by commas. Example: `rebar3 do a, b, c`.

## dialyzer

Builds and keeps up-to-date a suitable PLT (Persistent Lookup Table), and uses it to carry out success typing analysis on the current project.

| Option                 | Type    | Description                       | Default |
| ---------------------- | ------- | --------------------------------- | ------- |
| `--incremental`, `-i`  | boolean | Enable incremental analysis mode. | false   |
| `--update-plt`, `-u`   | boolean | Enable updating the PLT.          | true    |
| `--succ-typings`, `-s` | boolean | Enable success typing analysis.   | true    |

For instructions on suppressing warnings read the [Requesting or Suppressing Warnings in Source Files](https://erlang.org/doc/man/dialyzer.html) section of the Dialyzer documentation.

PLT files are named `<prefix>_<otp_release>_plt`; The base PLT is a PLT containing the core applications often required for a project's PLT. One base PLT is created per OTP version and stored in `base_plt_location`. A base PLT is then used to build project PLTs.

The following (optional) configurations can be added to a `proplist` of options `dialyzer` in rebar.config:

| Option              | Description                                                                                                                                                                                                                                                                                                   |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `warnings`          | a list of dialyzer warnings                                                                                                                                                                                                                                                                                   |
| `get_warnings`      | display warnings when altering a PLT file (boolean)                                                                                                                                                                                                                                                           |
| `plt_apps`          | the strategy for determining the applications which are included in the PLT file, `top_level_deps` to include just the direct dependencies or `all_deps` to include all nested dependencies (the direct dependent applications are listed in `applications` and `included_applications` of their .app files.) |
| `plt_extra_apps`    | a list of applications to include in the PLT file (the applications in `base_plt_apps` will already be in the list)                                                                                                                                                                                           |
| `plt_location`      | the location of the PLT file, `local` to store in the profile's base directory (default) or a custom directory.                                                                                                                                                                                               |
| `plt_prefix`        | the prefix to the PLT file, defaults to "rebar3"                                                                                                                                                                                                                                                              |
| `base_plt_apps`     | a list of applications to include in the base PLT file                                                                                                                                                                                                                                                        |
| `base_plt_location` | the location of base PLT file, `global` to store in $HOME/.cache/rebar3 (default) or  a custom directory                                                                                                                                                                                                      |
| `base_plt_prefix`   | the prefix to the base PLT file, defaults to "rebar3"                                                                                                                                                                                                                                                         |
| `incremental`       | incremental analysis mode                                                                                                                                                                                                                                                                                     |

## edoc

Generates documentation using doc.

Runs in the `docs` profile.

## escriptize

Generates an [escript](https://www.erlang.org/doc/man/escript.html) executable containing the project's and its dependencies' BEAM files.

| Config Option       | Type          | Description                                                                                                                                                                                                                        |
| ------------------- | ------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `escript_main_app`  | atom          | Name of the application to turn to an escript. Defaults to the top-level app if there is only one. When using an umbrella repository (with multiple top-level apps), this value *must* be specified.                               |
| `escript_name`      | string        | Name of the generated escript, and default module name to boot (`Module:main(_)`). Defaults to the value for `escript_main_app`                                                                                                    |
| `escript_incl_apps` | list of atoms | List of applications to include in the escript archive aside from the main app and its dependencies (from the app file). Defaults to `[]`                                                                                          |
| `escript_emu_args`  | string        | Escript emulator arguments (after `%%!` in escript declarations). The string must begin with `%%!` and end with a line break. An example string would be `"%%! +sbtu +A0\n"`. The Default value is `"%%! -escript main MainApp\n"` |
| `escript_shebang`   | string        | Location of escript file to run. Defaults to `"#!/usr/bin/env escript\n"`. The end of line marker must be included in the string.                                                                                                  |
| `escript_comment`   | string        | Arbitrary comment to put into the generated escript. Must include a newline marker at the end. Defaults to `%%\n`.                                                                                                                 |

To override the default module name for the escript (which is expected to be the same as the `escript_name`), add `-escript main Module` to `escript_emu_args`.

Example escript configuration from `relx`:

```erlang
{escript_emu_args, "%%! +sbtu +A0 -noinput\n"}.
{escript_incl_apps, [getopt, erlware_commons, bbmustache, providers, relx]}.
```

## eunit

Runs EUnit tests on project apps.

| Config Option         | Type                            | Description                                                                                                         |
| --------------------- | ------------------------------- | ------------------------------------------------------------------------------------------------------------------- |
| `--application`       | Comma separated list of strings | Application test suites to run. Equivalent to `[{application, App}]`                                                |
| `-c, --cover`         | Boolean                         | Generate cover data. Defaults to false                                                                              |
| `--cover_export_name` | String                          | Base name of the coverdata file to write                                                                            |
| `-p, --profile`       | Boolean                         | Show the slowest tests. Defaults to false                                                                           |
| `-d, --dir`           | Comma separated list of strings | Dirs to load tests from. Equivalent to `[{dir, Dir}]`                                                               |
| `-f, --file`          | Comma separated list of strings | Files to load tests from. Equivalent to `[{file, File}]`                                                            |
| `-m, --module`        | Comma separated list of strings | Modules to load tests from. Equivalent to `[{module, Module}]`                                                      |
| `-t, --test`          | Comma separated list of strings | Tests to run. The format is `Module:Func1+Func2`. Equivalent to `[{test, Module, Function}]`                        |
| `-g, --generator`     | Comma separated list of strings | Generators to load tests from. The format is `Module:Func1+Func2`. Equivalent to `[{generator, Module, Function}]`. |
| `-v, --verbose`       | Boolean                         | Verbose output. Defaults to false.                                                                                  |
| `--name`              | String                          | Gives a long name to the node                                                                                       |
| `--sname`             | String                          | Gives a short name to the node                                                                                      |
| `--sys_config`        | Comma separated list of strings | List of application config files                                                                                    |
| `--setcookie`         | String                          | Sets the cookie if the node is distributed                                                                          |

For more details, see [EUnit](testing/eunit.md).

Runs in the `test` profile.

## get-deps

> #### Not Required {: .warning}
> Unlike Rebar 2 this command is not required for fetching dependencies. The compile command will result in dependencies being fetched and then built if they aren't already. This command is useful if you have a specific use case that requires fetching dependencies separate from compilation.

Fetch project dependencies.

## help

Displays a list of tasks or help for a given task or subtask.

|Option|Description|
|----|----|
|`<task>`|Task to print help for.|
|`<namespace> <task>` |Task within \<namespace\> to print help for|

## new

Creates a new project from templates. See a list of available templates by providing no arguments.

| Option           | Description |
|------------------|-------------------------------------------------------|
|`--force`, `-f`   | Overwrite existing files.                             |
|`help <template>` | Display all variables and arguments for each template |

## path

Print paths to build dirs in current profile.

| Option              | Type                            | Description                                                                    |
| ------------------- | ------------------------------- | ------------------------------------------------------------------------------ |
| `--app`             | Comma separated list of strings | Comma separated list of applications to return paths for.                      |
| `--base`            | none                            | Return the base path of the current profile.                                   |
| `--bin`             | none                            | Return the bin path of the current profile.                                    |
| `--ebin`            | none                            | Return all ebin paths of the current profile's applications.                   |
| `--lib`             | none                            | Return the lib path of the current profile.                                    |
| `--priv`            | none                            | Return the priv path of the current profile.                                   |
| `--separator`, `-s` | string                          | In case of multiple return paths, the separator character to use to join them. |
| `--src`             | none                            | Return the src path of the current profile's applications.                     |
| `--rel`             | none                            | Return the rel path of the current profile.                                    |

## release

Builds release of project. Call `rebar3 help release` for arguments.

## relup

Creates a relup from two releases that were already built by calling `rebar3 release` without clearing the `_build` directory. Call `rebar3 help relup` for arguments.

## report

Generates contextual data to include in bug reports.

## shell

Runs a shell with project apps and deps in path. Intended for development use only; use [Releases](deployment/releases.md) for production.

| Option              | Type   | Description                                                                                                               |
| ------------------- | ------ | ------------------------------------------------------------------------------------------------------------------------- |
| `--config`          | string | Allows to load a [config file](https://www.erlang.org/doc/man/config.html), if any. Defaults to the sys_config entry defined for relx if present.                       |
| `--name`, `--sname` | atom   | Starts the node in network mode. Equivalent to erl's `-name` and `-sname` options.                                            |
| `--setcookie`       | string | Sets the cookie for a distributed node. Equivalent to erl's `-setcookie` option.                                             |
| `--script`          | string | path to an escript to be evaluated before applications are started.                                                        |
| `--apps`            | string | Comma-separated list of application names to be booted. Defaults to the apps in the relx release if present.              |
| `--start-clean`     |        | When specified, no apps are booted by the shell; useful to override release or shell tuple configurations in rebar.config. |
| `--relname`, `-r`   | atom   | If multiple releases are present, specify which one to pick.                                                               |
| `--relvsn`, `-v`    | string | If multiple releases are present, specify which version to use.                                                            |
| `--env-file`        | string | Path to file of os environment variables to setup before expanding vars in config files.                                   |
| `--user_drv_args`   | string | For versions of Erlang prior to 26, this option can be used to pass arguments to the user_drv start function for creating custom shells. Starting with Erlang 26, the arguments defined with this option are applied to the shell start_interactive function.|
| `--eval`            | string | Erlang expressions to execute during startup. These will run last, just before presenting the user with the Erlang shell prompt. There can be more than one `--eval` switch. Roughly equivalent to erl's `-eval` option.|

The shell booted with this command has a running agent that allows running Rebar3 commands dynamically, such as `r3:compile()` or `r3:upgrade()`, and have new modules automatically reloaded. Specific namespaces can be reached by calling `r3:do(Namespace, Command)`. No arguments can be passed to these commands.

## tar

Builds a compressed tar archive of release built of project. Call `rebar3 help tar` for arguments.

## tree

Prints a tree of dependencies and transitive dependencies of the project.

| Option            | Type | Description                                       |
| ----------------- | ---- | ------------------------------------------------- |
| `-v`, `--verbose` | none | Print repo and branch/tag/ref for git and hg deps |

## lock

Get unbuilt dependencies to be added to the `rebar.lock` file. They will just have been downloaded, but none of their build script should have run. Though this is not necessarily true with pre/post hooks and dep plugins.

## unlock

Unlocks dependencies. Specify a comma separated list of dependencies to unlock and regenerate the `rebar.lock` file, or `-a,--all` to unlock them all and remove the `rebar.lock` file.

This command should be used when one or more dependencies have been taken out of `rebar.config`, but remain in the lock file.

| Option         | Type   | Description                               |
| -------------- | ------ | ----------------------------------------- |
| `<dependency>` | string | Dependencies to unlock (comma-separated). |
| `-a`, `--all`  | none   | Unlock all dependencies.                  |

## update

Updates the package index.

## upgrade

Takes the current dependency specifications in `rebar.config` and fetches the most up-to-date versions satisfying them, while updating the lock file accordingly.

| Option         | Type   | Description                                |
| -------------- | ------ | ------------------------------------------ |
| `<dependency>` | string | Dependencies to upgrade (comma-separated). |
| `-a`, `--all`  | none   | Upgrade all dependencies.                  |


## version

Prints version for rebar3 and current Erlang.

## xref

Runs cross reference analysis.
