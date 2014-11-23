# Plugins #

Rebar3's system is based on the concept of *[providers](https://github.com/tsloughter/providers)*. A provider has three callbacks:

- `init(State) -> {ok, NewState}`, which helps set up the state required, state dependencies, etc.
- `do(State) -> {ok, NewState} | {error, Error}`, which does the actual work.
- `format_error(Error) -> String`, which allows to print errors when they happen, and to filter out sensitive elements from the state.

A provider should also be an OTP Library application, which can be fetched as any other Erlang dependency, except for Rebar3 rather than your own system or application.

This document contains the following elements:

- [Using a Plugin](#using-a-plugin)
- [Reference](#reference)
  - [Provider Interface](#provider-interface)
  - [List of Possible Dependencies](#list-of-possible-dependencies)
  - [Rebar State Manipulation](#rebar-state-manipulation)
- [Tutorial](#tutorial)

## Using a Plugin ##

To use the a plugin, add it to the rebar.config:

```erlang
{plugins, [
  {plugin_name, ".*", {git, "git@host:user/name-of-plugin.git", {tag, "v1.0.0"}}}
]}.
```

Then you can just call it directly:

```
→ rebar3 plugin_name
===> Fetching plugin_name
===> Compiling plugin_name
<PLUGIN OUTPUT>
```

## Reference ##

TODO

### Provider Interface ###

TODO

### List of Possible Dependencies ###

TODO

### Rebar State Manipulation ###

TODO


## Tutorial ##

### First version ###

In this tutorial, we'll show how to start from scratch, and get a basic plugin written. The plugin will be quite simple: it will look for instances of 'TODO:' lines in comments and report them as warnings. The final code for the plugin can be found on [bitbucket](https://bitbucket.org/ferd/rebar3-todo-plugin).

The first step is to create a new OTP Application that will contain the plugin:

    → rebar3 new plugin provider_todo desc="example rebar3 plugin"
    ...
    → cd provider_todo
    → git init
    Initialized empty Git repository in /Users/ferd/code/self/provider_todo/.git/

Open up the `src/provider_todo.erl` file and make sure you have the following skeleton in place:

```erlang
-module(provider_todo).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, todo).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},          % The 'user friendly' name of the task
            {module, ?MODULE},          % The module implementation of the task
            {bare, true},               % The task can be run by the user, always true
            {deps, ?DEPS},              % The list of dependencies
            {example, "rebar provider_todo"}, % How to use the plugin
            {opts, []}                  % list of options understood by the plugin
            {short_desc, "example rebar3 plugin"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
```

This shows all the basic content needed. Note that we leave the `DEPS` macro to the value `app_discovery`, used to mean that the plugin should at least find the project's source code (excluding dependencies).

In this case, we need to change very little in `init/1`. Here's the new provider description:

```erlang
        Provider = providers:create([
            {name, ?PROVIDER},       % The 'user friendly' name of the task
            {module, ?MODULE},       % The module implementation of the task
            {bare, true},            % The task can be run by the user, always true
            {deps, ?DEPS},           % The list of dependencies
            {example, "rebar todo"}, % How to use the plugin
            {opts, []},              % list of options understood by the plugin
            {short_desc, "Reports TODOs in source code"},
            {desc, "Scans top-level application source and find "
                   "instances of TODO: in commented out content "
                   "to report it to the user."}
    ]),
```

Instead, most of the work will need to be done directly in `do/1`. We'll use the `rebar_state` module to fetch all the applications we need. This can be done by calling the `project_apps/1` function, which returns the list of the project's top-level applications.

```erlang
do(State) ->
    lists:foreach(fun check_todo_app/1, rebar_state:project_apps(State)),
    {ok, State}.
```

This, on a high level, means that we'll check each top-level app one at a time (there may often be more than one top-level application when working with releases)

The rest is filler code specific to the plugin, in charge of reading each app path, go read code in there, and find instances of 'TODO:' in comments in the code:

```erlang
check_todo_app(App) ->
    Path = filename:join(rebar_app_info:dir(App),"src"),
    Mods = find_source_files(Path),
    case lists:foldl(fun check_todo_mod/2, [], Mods) of
        [] -> ok;
        Instances -> display_todos(rebar_app_info:name(App), Instances)
    end.

find_source_files(Path) ->
    [filename:join(Path, Mod) || Mod <- filelib:wildcard("*.erl", Path)].

check_todo_mod(ModPath, Matches) ->
    {ok, Bin} = file:read_file(ModPath),
    case find_todo_lines(Bin) of
        [] -> Matches;
        Lines -> [{ModPath, Lines} | Matches]
    end.

find_todo_lines(File) ->
    case re:run(File, "%+.*(TODO:.*)", [{capture, all_but_first, binary}, global, caseless]) of
        {match, DeepBins} -> lists:flatten(DeepBins);
        nomatch -> []
    end.

display_todos(_, []) -> ok;
display_todos(App, FileMatches) ->
    io:format("Application ~s~n",[App]),
    [begin
      io:format("\t~s~n",[Mod]),
      [io:format("\t  ~s~n",[TODO]) || TODO <- TODOs]
     end || {Mod, TODOs} <- FileMatches],
    ok.
```

Just using `io:format/2` to output is going to be fine.

To test the plugin, push it to a source repository somewhere. Pick one of your projects, and add something  to the rebar.config:

```erlang
{plugins, [
  {provider_todo, ".*", {git, "git@bitbucket.org:ferd/rebar3-todo-plugin.git", {branch, "master"}}}
]}.
```

Then you can just call it directly:

```
→ rebar3 todo
===> Fetching provider_todo
===> Compiling provider_todo
Application merklet
    /Users/ferd/code/self/merklet/src/merklet.erl
      todo: consider endianness for absolute portability
```

Rebar3 will download and install the plugin, and figure out when to run it. Once compiled, it can be run at any time again.

### Optionally Search Deps ###

Let's extend things a bit. Maybe from time to time (when cutting a release), we'd like to make sure none of our dependencies contain 'TODO:'s either.

To do this, we'll need to go parse command line arguments a bit, and change our execution model. The `?DEPS` macro will now need to specify that the `todo` provider can only run *after* dependencies have been installed:

```erlang
-define(DEPS, [install_deps]).
```

We can add the option to the list we use to configure the provider in `init/1`:

```erlang
{opts, [                 % list of options understood by the plugin
    {deps, $d, "deps", undefined, "also run against dependencies"}
]},
```

Meaning that deps can be flagged in by using the option `-d` (or `--deps`), and if it's not defined, well, we get the default value `undefined`. The last element of the 4-tuple is documentation for the option.

And then we can implement the switch to figure out what to search:

```erlang
do(State) ->
    Apps = case discovery_type(State) of
        project -> rebar_state:project_apps(State);
        deps -> rebar_state:project_apps(State) ++ rebar_state:src_deps(State)
    end,
    lists:foreach(fun check_todo_app/1, Apps),
    {ok, State}.

[...]

discovery_type(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(deps, Args) of
        undefined -> project;
        _ -> deps
    end.
```

The `deps` option is found using `rebar_state:command_parsed_args(State)`, which will return a proplist of terms on the command-line after 'todo', and will take care of validating whether the flags are accepted or not. The rest can remain the same.

Push the new code for the plugin, and try it again on a project with dependencies:

```
→ rebar3 todo --deps
===> Fetching provider_todo
===> Compiling provider_todo
===> Fetching bootstrap
===> Fetching file_monitor
===> Fetching recon
[...]
Application dirmon
    /Users/ferd/code/self/figsync/apps/dirmon/src/dirmon_tracker.erl
      TODO: Peeranha should expose the UUID from a node.
Application meck
    /Users/ferd/code/self/figsync/_deps/meck/src/meck_proc.erl
      TODO: What to do here?
      TODO: What to do here?
```

Rebar3 will now go pick dependencies before running the plugin on there.

you can also see that the help will be completed for you:

```
→ rebar3 help todo
Scans top-level application source and find instances of TODO: in commented out content to report it to the user.

Usage: rebar todo [-d]

  -d, --deps  also run against dependencies
```

That's it, the todo plugin is now complete! It's ready to ship and be included in other repositories.
