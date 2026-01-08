# Profiles

In any project, there's invariably a set of options that are desired depending on the task run or the role of the person running the task.

In Erlang projects for example, the most common example is dependencies required only for test runs, such as mocking libraries or specific testing tools or frameworks.

Rebar3 addresses such requirements with the concept of *profiles*. A profile is a set of configuration settings to be used only in one of these specific contexts, overriding or complementing the regular configuration. Their objective is to be able to support multiple development use cases, while keeping things repeatable and without requiring external tools or environment values to do that work.

A profile for a run can be specified in three different ways:

1. calling rebar as `rebar3 as <profile> <command>` or `rebar3 as <profile1>,<profile2> <command>`
2. by a given Rebar3 command. For example, the `eunit` and `ct` commands *always* add a `test` profile to the run.
3. The `REBAR_PROFILE` environment variable

Any of these forms (or even all at once) will let Rebar3 know that it should run as one of the special profiles and modify its configuration accordingly.

The profile configuration can be specified in the main `rebar.config` file as such:

```erlang
{profiles, [{ProfileName1, [Options, ...]},
            {ProfileName2, [Options, ...]}]}.
```

For example, a test profile that adds the `meck` dependency only for test runs could be defined as:

```erlang
{profiles, [{test, [{deps, [meck]}]}]}.
```

Any configuration value can go in profiles, including plugins, compiler options, release options, and so on.

## Example

A more complete example might look like this:

```erlang
{deps, [...]}.
{relx, [
    ...
]}.

{profiles, [
    {prod, [
        {erl_opts, [no_debug_info, warnings_as_errors]},
        {relx, [{dev_mode, false}]}
    ]},
    {native, [
        {erl_opts, [{native, {hipe, o3}}]}
    ]},
    {test, [
        {deps, [meck]},
        {erl_opts, [debug_info]}
    ]}
]}.
```

Such a project therefore has *four* distinct profiles:

1. `default`, the de-facto profile for all runs, corresponding to the overall `rebar.config` file

2. `prod`, in this case probably used to generate full releases without symlinks, and with stricter compiler options

3. `native`, to force compiling with [HiPE](http://www.erlang.org/doc/man/HiPE_app.html), for faster mathematical code

4. `test`, which loads mocking libraries and enables debug information to be kept in files during test runs.

Those might be combined in many ways. Here are example runs:

1. `rebar3 ct`: will run the Common Test suites of the project. In order, the profiles applied will be `default`, and then `test`, because `ct` mandates the usage of a `test` profile.

2. `rebar3 as test ct`: will run the same as before. Profiles are applied only once.

3. `rebar3 as native ct`: will run the tests in native mode. The order of profiles will be `default`, then `native`, and finally `test` (which is specified last, by the command run).

4. `rebar3 as test,native ct`: will be similar as the above, with one slight variation. When applying profiles, Rebar3 first expands them all, and applies them in the right order. So the order here would be `default`, then `test`, then `native`. The last `test` profile (because of the `ct` command) is elided since it was already applied. This is not entirely equivalent to calling `rebar3 as native ct`, because if both the `test` and `native` profile were to set conflicting options, the profile order becomes important.

5. `rebar3 release` will build the release only as the `default` profile.

6. `rebar3 as prod release` will build the release without development mode, with a stricter set of compiler options.

7. `rebar3 as prod, native release` will build the release as with the last command, but while also compiling modules to native mode.

8. `rebar3 as prod release` with `REBAR_PROFILE=native`, in the environment, will build the release as in the last command, but `native` will be applied *before* `prod`.

The order of application of profiles is therefore:

1. `default`
2. The `REBAR_PROFILE` value, if any
3. the profiles specified in the `as` part of the command line
4. the profiles specified by each individual command

Profiles are therefore a composable way to specify configuration subsets in a contextual manner.

> #### Locking Dependencies {: .info}
> Only dependencies listed at the top level of `rebar.config`, the `default` profile, are saved to `rebar.lock`. Other dependencies will not get locked.
>
> If someone wants to "lock for production" (meaning with production-related profiles), the answer is to keep the default profile and to use [releases](deployment/releases.md), which produce compiled artifacts that can be reused at any time.

## Option-Merging Algorithm

It's generally tricky to try and merge all configuration options automatically. Different tools or commands will expect them differently, either as lists of tuples, proplists, or key/value pairs to be transformed into a dictionary of some sort.

To support the most generic form as possible, Rebar3 handles them as a loose combination of proplists and tuple lists. This means the following options are all seen as having the key `native`:

- `native`
- `{native, {hipe, o3}}`
- `{native, still, supported}`

Even though some of them may not be supported by tools. For example, the Erlang compiler supports defining macros as either `{d, 'MACRONAME'}` or `{d, 'MACRONAME', MacroValue}`, but not `d` alone, whereas it does support `native` and `{native, {hipe, o3}}`.

Rebar3 properly supports all these forms and merges them in a functional manner. Let's take the following profiles as an example:

```erlang
{profiles, [
    {prod, [
        {erl_opts, [no_debug_info, warnings_as_errors]},
    ]},
    {native, [
        {erl_opts, [{native, {hipe, o3}}, {d, 'NATIVE'}]}
    ]},
    {test, [
        {erl_opts, [debug_info]}
    ]}
]}.
```

Applying the profiles in various orders will yield different lists of options for `erl_opts`:

- `rebar3 as prod,native,test <command>`: `[debug_info, {d, 'NATIVE'}, {native, {hipe, o3}}, no_debug_info, warnings_as_errors]`
- `rebar3 as test,prod,native <command>`: `[{d, 'NATIVE'}, {native, {hipe, o3}}, no_debug_info, warnings_as_errors, debug_info]`
- `rebar3 as native,test,prod <command>`: `[no_debug_info, warnings_as_errors, debug_info, {d, 'NATIVE'}, {native, {hipe, o3}}]`
- `rebar3 as native,prod,test <command>`: `[debug_info, no_debug_info, warnings_as_errors, {d, 'NATIVE'}, {native, {hipe, o3}}]`

Notice that the last profiles applied yield the first elements in the list, and that the elements within each profile's list will be sorted according to their key.

This will allow Rebar3 commands to pick up elements in the right order, while still supporting multi-value lists that require many elements to share the same key (such as `[{d, 'ABC'}, {d, 'DEF'}]`, which are two independent macros!). Commands that do not support duplicated elements can stop processing them after the first ones, while those that build dictionaries (or maps) out of them may choose to insert them as is, or can safely reverse the list first (if the last elements processed become the final ones in maps).

All profile merging rules are processed safely that way. Plugin writers should be aware of these rules and plan accordingly.

Note that in practice, the Erlang compiler does not play nice with `debug_info` and `no_debug_info` (which isn't even a real option and was added by Rebar3). Rebar3 does some magic to deduplicate these specific values to please the compiler, but does not extend this courtesy to all tools. Designing your plugins to use `{OptionName, true|false}` is generally a good idea.

> #### Dependencies and Profiles {: .warning}
> Dependencies will always be compiled with the `prod` profile applied to their configuration. No other (besides `default`, of course) is used on any dependency. Even though they are configured for `prod` the dependency will still be fetched to the profile directory for the profile it is declared under. For example, a dependency in the top level `deps` will be under `_build/default/lib` and a dependency under the profile `test` will be fetched to `_build/test/lib`. Both will be compiled with their `prod` profile configuration applied.
