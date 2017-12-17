# Rebar3

[![Build Status](https://travis-ci.org/erlang/rebar3.svg?branch=master)](https://travis-ci.org/erlang/rebar3) [![Windows build status](https://ci.appveyor.com/api/projects/status/yx4oitd9pvd2kab3?svg=true)](https://ci.appveyor.com/project/TristanSloughter/rebar3)

1. [What is Rebar3?](#what-is-rebar3)
2. [Why Rebar3?](#why-rebar3)
3. [Should I Use Rebar3?](#should-i-use-rebar3)
4. [Getting Started](#getting-started)
5. [Documentation](#documentation)
6. [Features](#features)
7. [Migrating from rebar2](#migrating-from-rebar2)
8. [Additional Resources](#additional-resources)

## What is Rebar3

Rebar3 is an Erlang tool that makes it easy to create, develop, and
release Erlang libraries, applications, and systems in a repeatable manner.

Rebar3 will:
- respect and enforce standard Erlang/OTP conventions for project
  structure so they are easily reusable by the community;
- manage source dependencies and Erlang [packages](http://hex.pm)
  while ensuring repeatable builds;
- handle build artifacts, paths, and libraries such that standard
  development tools can be used without a headache;
- adapt to projects of all sizes on almost any platform;
- treat [documentation](http://www.rebar3.org/docs/) as a feature,
  and errors or lack of documentation as a bug.

Rebar3 is also a self-contained Erlang script. It is easy to distribute or
embed directly in a project. Tasks or behaviours can be modified or expanded
with a [plugin system](http://www.rebar3.org/docs/using-available-plugins)
[flexible enough](http://www.rebar3.org/docs/plugins) that even other languages
on the Erlang VM will use it as a build tool.

## Why Rebar3

Rebar3 is the spiritual successor to [rebar
2.x](https://github.com/rebar/rebar), which was the first usable build tool
for Erlang that ended up seeing widespread community adoption. It however
had several shortcomings that made it difficult to use with larger projects
or with teams with users new to Erlang.

Rebar3 was our attempt at improving over the legacy of Rebar 2.x, providing the
features we felt it was missing, and to provide a better environment in which
newcomers joining our teams could develop.

## Should I use Rebar3?

If your main language for your system is Erlang, that you value repeatable builds
and want your various tools to integrate together, we do believe Rebar3 is the
best experience you can get.

## Getting Started

A [getting started guide is maintained on the official documentation website](http://www.rebar3.org/docs/getting-started),
but installing rebar3 can be done by any of the ways described below

Latest stable compiled version:
```bash
$ wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
```

From Source (assuming you have a full Erlang install):

```bash
$ git clone https://github.com/erlang/rebar3.git
$ cd rebar3
$ ./bootstrap
```

Stable versions can also be obtained from the [releases page](https://github.com/erlang/rebar3/releases).

The rebar3 escript can also extract itself with a run script under the user's home directory:

```bash
$ ./rebar3 local install
===> Extracting rebar3 libs to ~/.cache/rebar3/lib...
===> Writing rebar3 run script ~/.cache/rebar3/bin/rebar3...
===> Add to $PATH for use: export PATH=$PATH:~/.cache/rebar3/bin
```

To keep it up to date after you've installed rebar3 this way you can use `rebar3 local upgrade` which
fetches the latest stable release and extracts to the same place as above. A [nightly version can
also be obtained](https://s3.amazonaws.com/rebar3-nightly/rebar3) if desired.

Rebar3 may also be available on various OS-specific package managers such as
FreeBSD Ports. Those are maintained by the community and Rebar3 maintainers
themselves are generally not involved in that process.

If you do not have a full Erlang install, we  using [erln8](http://erln8.github.io/erln8/)
or [kerl](https://github.com/yrashk/kerl). For binary packages use those provided
by [Erlang Solutions](https://www.erlang-solutions.com/resources/download.html),
but be sure to choose the "Standard" download option or you'll have issues building
projects.

## Documentation

Rebar3 documentation is maintained on [http://www.rebar3.org/docs](http://www.rebar3.org/docs)

## Features

Rebar3 supports the following features or tools by default, and may provide many
others via the plugin ecosystem:

| features             | Description |
|--------------------- |------------ |
| Command composition  | Rebar3 allows multiple commands to be run in sequence by calling `rebar3 do <task1>,<task2>,...,<taskN>`. |
| Command dependencies | Rebar3 commands know their own dependencies. If a test run needs to fetch dependencies and build them, it will do so. |
| Command namespaces   | Allows multiple tools or commands to share the same name. |
| Compiling            | Build the project, including fetching all of its dependencies by calling `rebar3 compile` |
| Clean up artifacts   | Remove the compiled beam files from a project with `rebar3 clean` or just remove the `_build` directory to remove *all* compilation artifacts |
| Code Coverage        | Various commands can be instrumented to accumulate code coverage data (such as `eunit` or `ct`). Reports can be generated with `rebar3 cover` |
| Common Test          | The test framework can be run by calling `rebar3 ct` |
| Dependencies         | Rebar3 maintains local copies of dependencies on a per-project basis. They are fetched deterministically, can be locked, upgraded, fetched from source, packages, or from local directories. See [Dependencies on the documentation website](http://www.rebar3.org/docs/dependencies). Call `rebar3 tree` to show the whole dependency tree. |
| Documentation        | Print help for rebar3 itself (`rebar3 help`) or for a specific task (`rebar3 help <task>`). Full reference at [www.rebar3.org](http://www.rebar3.org/docs). |
| Dialyzer             | Run the Dialyzer analyzer on the project with `rebar3 dialyzer`. Base PLTs for each version of the language will be cached and reused for faster analysis |
| Edoc                 | Generate documentation using edoc with `rebar3 edoc` |
| Escript generation   | Rebar3 can be used to generate [escripts](http://www.erlang.org/doc/man/escript.html) providing an easy way to run all your applications on a system where Erlang is installed |
| Eunit                | The test framework can be run by calling `rebar3 eunit` |
| Locked dependencies  | Dependencies are going to be automatically locked to ensure repeatable builds. Versions can be changed with `rebar3 upgrade` or `rebar3 upgrade <app>`, or locks can be released altogether with `rebar3 unlock`. |
| Packages             | [Hex packages](http://hex.pm) can be listed with `rebar3 pkgs`. They can be used as dependencies, will be cached locally for faster usage, and a local index will be used and updated with `rebar3 update`. |
| Path                 | While paths are managed automatically, you can print paths to the current build directories with `rebar3 path`. |
| Plugins              | Rebar3 can be fully extended with [plugins](#http://www.rebar3.org/docs/using-available-plugins). List or upgrade plugins by using the plugin namespace (`rebar3 plugins`). |
| Profiles             | Rebar3 can have subconfiguration options for different profiles, such as `test` or `prod`. These allow specific dependencies or compile options to be used in specific contexts. See [Profiles](http://www.rebar3.org/docs/profiles) in the docs. |
| Releases             | Rebar3 supports [building releases](http://www.rebar3.org/docs/releases) with the `relx` tool, providing a way to ship fully self-contained Erlang systems. Release update scripts for live code updates can also be generated. |
| Shell                | A full shell with your applications available can be started with `rebar3 shell`. From there, call tasks as `r3:do(compile)` to automatically recompile and reload the code without interruption |
| Tarballs             | Releases can be packaged into tarballs ready to be deployed. |
| Templates            | Configurable templates ship out of the box (try `rebar3 new` for a list or `rebar3 new help <template>` for a specific one). [Custom templates](http://www.rebar3.org/docs/using-templates) are also supported, and plugins can also add their own. |
| Unstable namespace   | We use a namespace to provide commands that are still in flux, allowing to test more experimental features we are working on. See `rebar3 unstable`. |
| Xref       | Run cross-reference analysis on the project with [xref](http://www.erlang.org/doc/apps/tools/xref_chapter.html) by calling `rebar3 xref`. |

## Migrating From rebar2

The grievances we had with Rebar 2.x were not fixable without breaking
compatibility in some very important ways.

A full guide titled [From Rebar 2.x to Rebar3](http://www.rebar3.org/docs/from-rebar-2x-to-rebar3)
is provided on the documentation website.

Notable modifications include mandating a more standard set of directory
structures, changing the handling of dependencies, moving some compilers (such
as C, Diameter, ErlyDTL, or ProtoBuffs) to
[plugins](http://www.rebar3.org/docs/using-available-plugins) rather than
maintaining them in core rebar, and moving release builds from reltool to
relx.

## Additional Resources

In the case of problems that cannot be solved through documentation or examples, you
may want to try to contact members of the community for help. The community is
also where you want to go for questions about how to extend rebar, fill in bug
reports, and so on.

The main place to go for questions is the [rebar mailing
list](http://lists.basho.com/pipermail/rebar_lists.basho.com/). If you need
quick feedback, you can try the #rebar channel on
[irc.freenode.net](http://freenode.net). Be sure to check the
[documentation](http://www.rebar3.org/v3.0/docs) first, just to be sure you're not
asking about things with well-known answers.

For bug reports, roadmaps, and issues, visit the [github issues
page](https://github.com/erlang/rebar3/issues).

General rebar community resources and links:

- [Rebar Mailing List](http://lists.basho.com/pipermail/rebar_lists.basho.com/)
- #rebar on [irc.freenode.net](http://freenode.net/)
- [issues](https://github.com/erlang/rebar3/issues)
- [Documentation](http://www.rebar3.org/v3.0/docs)

To contribute to rebar3, please refer to [CONTRIBUTING](CONTRIBUTING.md).

