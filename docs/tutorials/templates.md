# Templates

- [Default Variables](#section-default-variables)
- [Global Variables](#section-global-variables)
- [Batteries-Included Templates](#section-batteries-included-templates)
- [Custom Templates](#section-custom-templates)
- [Plugin Templates](#section-plugin-templates)

## Default Variables

- `date`: defaults to today's date, under universal time, printed according to ISO 8601 (for example, `"2014-03-11"`)
- `datetime`: defaults to today's date and time, under universal time, printed according to ISO 8601 (for example, `"2014-03-11T16:06:02+00:00"`).
- `author_name`: Defaults to `"Anonymous"`
- `author_email`: Defaults to `"anonymous@example.org"`
- `apps_dir`: Directory where OTP applications should be created in release projects. Defaults to `"apps/"`.
- `copyright_year`: Defaults to the current year, under universal time.

## Global Variables

Global variables can be set by editing the file at `$HOME/.config/rebar3/templates/globals`:

```erlang
{variables, [
   {author_name, "My Name Is A String"},
   {copyright_year, "2014-2022", "The year or range of years for copyright"},
   {my_custom_var, "hello there"}
]}.
```

This will let you define variables for all templates. Variables left undefined will be ignored and revert to the default value.

The override order for these variables will be: `Defaults < $HOME/.config/rebar3/templates/globals < command line invocation`.

## Batteries-Included Templates

Rebar3 ships with a few templates installed, which can be listed by calling `rebar3 new`:

```shell
$ ./rebar3 new
app (built-in): Complete OTP Application structure.
cmake (built-in): Standalone Makefile for building C/C++ in c_src
escript (built-in): Complete escriptized application structure
lib (built-in): Complete OTP Library application (no processes) structure
plugin (built-in): Rebar3 plugin project structure
release (built-in): OTP Release structure for executable programs
```

Any custom plugins would be followed as `<plugin_name> (custom): <description>`.

Details for each individual plugin can be obtained by calling `rebar3 new help <plugin>`:

```shell
$ ./rebar3 new help plugin
plugin:
        built-in template
        Description: Rebar3 plugin
        Variables:
                name="myplugin" (Name of the plugin)
                desc="A rebar plugin" (Short description of the plugin's purpose)
                date="2014-11-10"
                datetime="2014-11-10T18:29:41+00:00"
                author_name="Anonymous"
                author_email="anonymous@example.org"
                copyright_year="2014"
                apps_dir="apps/" (Directory where applications will be created if needed)
```

All the variables there have their default values shown, and an optional explanation in parentheses. The variables can also be [overriden globally](#global-variables).

A template can be run by calling:

```shell
$ ./rebar3 new plugin name=demo author_name="Fred H."
...
$ ./rebar3 new plugin demo author_name="Fred H."
...
```

Then go to the directory created for the project by rebar3.

## Custom Templates

Custom templates can be added in `$HOME/.config/rebar3/templates/`. Each template is at least two files:

- `my_template.erl`: There can be many of these files. They are regular Erlang files using the mustache template syntax for variable replacements (provided by [soranoba's implementation](https://github.com/soranoba/mustache)).
- `my_template.template`; Called the *template index*, there is one per template callable from `rebar3`. This one will be visible when calling `rebar3 new my_template`. This file regroups the different mustache template files into a more cohesive template.

### File Syntax

#### Template Index

The following options are available:

```erlang
{description, "This template does a thing"}.
{variables, [
    {var1, "default value"},
    {var2, "default", "explain what this does in help files"},
    {app_dir, ".", "The directory where the application goes"}
  ]}.
{dir, "{{appdir}}/src"}.
{file, "mytemplate_README", "README"}.
{chmod, "README", 8#644}.
{template, "myapp/myapp.app.src", "{{appdir}}/src/{{name}}.app.src"}.
```

Specifically:

- `description`: takes a string explaining what the template is for.
- `variables`: takes a list of variables in two forms:
  - `{Name, DefaultString, HelpString}`;
  - `{Name, DefaultString}`.
- `{dir, TemplatablePathString}`: creates a given directory. Variable names can be used in the path name.
- `{file, FilePath, DestFilePath}`: copies a file literally to its destination.
- `{template, FilePath, TemplatablePathString}`: evaluates a given template. The `FilePath` is relative to the template index.
- `{chmod, FilePath, Int}`: changes the permission of a file, using the integer value specified. Octal values can be entered by doing `8#640`.

### Example

As an example, we'll create a template for Common Test test suites. Create the directory structure `~/.config/rebar3/templates/` and then go in there.

We'll start with an index for our template, called `ct_suite.template`:

```erlang
{description, "A basic Common Test suite for an OTP application"}.
{variables, [
    {name, "suite", "Name of the suite, prepended to the standard _SUITE suffix"}
]}.

{dir, "test"}.
{template, "ct_suite.erl", "test/{{name}}_SUITE.erl"}.
```

This tells Rebar3 to create the test directory and to evaluate a [mustache](https://github.com/soranoba/mustache) template. All the paths are relative to the current working directory.

Let's create the template file:

```erlang
-module({{name}}_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl"). % Eunit macros for convenience

-export([all/0
        ,groups/0
       %,init_per_suite/1, end_per_suite/1
       %,init_per_group/2, end_per_group/2
        ,init_per_testcase/2, end_per_testcase/2
        ]).

-export([fail/1]).

all() -> [fail].

groups() -> [].

init_per_testcase(_Name, Config) -> Config.

end_per_testcase(_Name, _Config) -> ok.

fail(_Config) ->
    ?assert(false).
```

This one does very simple variable substitution for the name (using `{{name}}`) and that's all it needs.

Let's get to any existing project you have and try it:

```shell
$ ./rebar3 new
app (built-in): OTP Application
ct_suite (custom): A basic Common Test suite for an OTP application
lib (built-in): OTP Library application (no processes)
release (built-in): OTP Release structure for executable programs
plugin (built-in): Rebar3 plugin
```

The first line shows that our `ct_suite` template has been detected and is usable.

Let's look at the details:

```shell
$ ./rebar3 new help ct_suite
ct_suite:
        custom template (/home/ferd/.config/rebar3/templates/ct_suite.template)
        Description: A basic Common Test suite for an OTP application
        Variables:
                name="suite" (Name of the suite, prepended to the standard _SUITE suffix)
                date="2014-11-10"
                datetime="2014-11-10T18:46:33+00:00"
                author_name="Anonymous"
                author_email="anonymous@example.org"
                copyright_year="2014"
                apps_dir="apps/" (Directory where applications will be created if needed)
```

The documentation from variables and the description are well in place. To apply the template, go to any of your OTP application's top-level directory:

```shell
$ ./rebar3 new ct_suite demo
===> Writing test/demo_SUITE.erl
```

And you will see the code in place.

## Plugin Templates

Plugins can be shipped with their own templates that then get listed along with the rest of templates. The structure is exactly similar to those found in `~/.config/rebar3/templates/`, except they must be placed in the plugin's `priv/` directory.
