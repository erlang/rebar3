# Templates #

- [Default Variables](#default-variables)
- [Global Variables](#global-variables)
- [Batteries-Included Templates](#batteries-included-templates)
- [Custom Templates](#custom-templates)

## Default Variables

- `date`: defaults to today's date, under universal time, printed according to RFC 8601 (for example, `"2014-03-11"`)
- `datetime`: defaults to today's date and time, under universal time, printed according to RFC 8601 (for example, `"2014-03-11T16:06:02+00:00"`).
- `author_name`: Defaults to `"Anonymous"`
- `author_email`: Defaults to `"anonymous@example.org"`
- `apps_dir`: Directory where OTP applications should be created in release projects. Defaults to `"apps/"`.
- `copyright_year`: Defaults to the current year, under universal time.


## Global Variables

Global variables can be set by editing the file at `$HOME/.rebar3/templates/globals`:

    {variables, [
        {author_name, "My Name Is A String"},
        {copyright_year, "2014-2022", "The year or range of years for copyright"},
        {my_custom_var, "hello there"}
    ]}.

This will let you define variables for all templates.

Variables left undefined will be ignored and revert to the default value.

The override order for these variables will be: Defaults < $HOME/.rebar3/templates/globals < command line invocation.

## Batteries-Included Templates ##

Rebar3 ships with a few templates installed, which can be listed by calling `rebar3 new`:

    → ./rebar3 new
    app (built-in): OTP Application
    lib (built-in): OTP Library application (no processes)
    release (built-in): OTP Release structure for executable programs
    plugin (built-in): Rebar3 plugin

Any custom plugins would be followed as `<plugin_name> (custom): <description>`.

Details for each individual plugin can be obtained by calling `rebar3 new help <plugin>`:

    → ./rebar3 new help plugin
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

All the variables there have their default values shown, and an optional explanation in parentheses.

The variables can also be [overriden globally](#global-variables).

A template can be run by calling:

    → ./rebar3 new plugin name=demo author_name="Fred H."
    ...

Alternatively, the `name` variable is special -- if the first argument to a template has no key associated with it, `name` is automatically added. The call above is therefore equivalent to:

    → ./rebar3 new plugin demo author_name="Fred H."
    ...


## Custom Templates ##

Custom templates can be added in `$HOME/.rebar3/templates/`. Each template is at least two files:

- `my_template.dtl`: There can be many of these files. They are regular Erlang files using the django template syntax for variable replacements.
- `my_template.template`; Called the *template index*, there is one per template callable from `rebar3`. This one will be visible when calling `rebar3 new my_template`. This file regroups the different \*.dtl files into a more cohesive template.

### File Syntax ###

#### Template Index ####

The following options are available:

    {description, "This template does a thing"}.
    {variables, [
      {var1, "default value"},
      {var2, "default", "explain what this does in help files"},
      {app_dir, ".", "The directory where the application goes"}
    ]}.
    {dir, "{{appdir}}/src"}.
    {file, "mytemplate_README", "README"}.
    {chmod, "README", 8#644}.
    {template, "myapp/myapp.app.src.dtl", "{{appdir}}/src/{{name}}.app.src"}.

Specifically:

- `description`: takes a string explaining what the template is for.
- `variables`: takes a list of variables in two forms:
  - `{Name, DefaultString, HelpString}`;
  - `{Name, DefaultString}`.
- `{dir, TemplatablePathString}`: creates a given directory. Variable names can be used in the path name.
- `{file, FilePath, DestFilePath}`: copies a file literally to its destination.
- `{template, DtlFilePath, TemplatablePathString}`: evaluates a given template. The `DtlFilePath` is relative to the template index.
- `{chmod, FilePath, Int}`: changes the permission of a file, using the integer value specified. Octal values can be entered by doing `8#640`.

### Example ###

As an example, we'll create a template for Common Test test suites. Create the directory structure `~/.rebar/templates/` and then go in there.

We'll start with an index for our template, called `ct_suite.template`:

```erlang
{description, "A basic Common Test suite for an OTP application"}.
{variables, [
    {name, "suite", "Name of the suite, prepended to the standard _SUITE suffix"}
]}.

{dir, "test"}.
{template, "ct_suite.erl.dtl", "test/{{name}}_SUITE.erl"}.
```

This tells rebar3 to create the test directory and to evaluate an [ErlyDTL](https://github.com/erlydtl/erlydtl) template. All the paths are relative to the current working directory.

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

    → ./rebar3 new
    app (built-in): OTP Application
    ct_suite (custom): A basic Common Test suite for an OTP application
    lib (built-in): OTP Library application (no processes)
    release (built-in): OTP Release structure for executable programs
    plugin (built-in): Rebar3 plugin

The first line shows that our `ct_suite` temlate has been detected and is usable.
Let's look at the details:

    → ./rebar3 new help ct_suite
    ct_suite:
            custom template (/home/ferd/.rebar3/templates/ct_suite.template)
            Description: A basic Common Test suite for an OTP application
            Variables:
                    name="suite" (Name of the suite, prepended to the standard _SUITE suffix)
                    date="2014-11-10"
                    datetime="2014-11-10T18:46:33+00:00"
                    author_name="Anonymous"
                    author_email="anonymous@example.org"
                    copyright_year="2014"
                    apps_dir="apps/" (Directory where applications will be created if needed)

The documentation from variables and the description are well in place. To apply the template, go to any of your OTP application's top-level directory:

    → ./rebar3 new ct_suite demo
    ===> Writing test/demo_SUITE.erl

And you will see the code in place.
