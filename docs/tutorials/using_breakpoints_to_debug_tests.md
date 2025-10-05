# Using Breakpoints to Debug Tests

## The Problem

When debugging Erlang applications, it is frequent that additional output is required to understand what is going on.

Standard ways to do this include:

- adding print statement debugging with `io:format/2-3` (or with `ct:pal/2` in common test)
- turning on `sys` module tracing with OTP processes to show more outputs
- trying to attach a [debugger](https://erlang.org/doc/apps/debugger/debugger_chapter.html)

The problem with a debugger is that it can be really difficult to properly observe what goes on in a running system with timers involved; similarly, it is difficult to use any fancier tools such as tracing at more granular level than OTP interactions when you do not have any access to interactive sessions, nor do you have any way to synchronize yourself with test framework execution to attach in the right areas.

In this document, we'll see two possible patterns that can be enabled with the new breakpoints feature introduced in version 3.7.0: breakpoints to enable trace probes, and breakpoints to go poke at a critical section in some code.

> #### Breakpoints are useful for many types of testing {: .tip}
> This page shows breakpoints used with Common Test, but they work with any provider, including plugins for QuickCheck or PropEr, out of the box.
>
> If you are a plugin developer, you should also be able to use these breakpoints. They will only trigger once safely executing asynchronous tasks from a shell and will be ignored > otherwise.
>
> This should prevent any problems where the plugin needs to run before the shell itself starts, or where a user might accidentally hang a build.

## The Base Mechanism

The breakpoint feature requires the following steps:

- the tests are run from an interactive shell with `rebar3 shell`, and using an asynchronous mode. This means that rather than calling `r3:ct()` for Common Test or `r3:eunit()` for EUnit tests (or their longer form `r3:do(Command)`), you need to call `r3:async_do(ct)` or `r3:async_do(eunit)`.
- You must call `r3:break()` from the test you want to execute. This will set a break point that will dynamically pause the code by awaiting for a secret message. The rest of the system will _not_ be paused, and test timeouts enabled by various frameworks will remain active.
- You will have the shell available to you to do anything; a confirmation message is displayed to let you know you are in a paused state
- Once you're done investigating, you can resume execution by calling `r3:resume()`.

Your session might look like:

```shell
$ rebar3 shell
...
1> r3:async_do(ct).
ok
...
Running Common Test suites...
%%% rebar_alias_SUITE: .
=== BREAK ===

2> % <do some checks>
2> r3:resume().
ok
3> .....
%%% rebar_as_SUITE: ...........
%%% rebar_compile_SUITE: ......
...
```

Do note that other debuggers already exist. You can use them with the asynchronous tasks if you want more features. See <https://github.com/hachreak/cedb> for a good debugger example.

## Break to Trace

In a pattern where you break-to-trace, you may want to make use of a tracing tool like `dbg`, `redbug`, or `recon`. We'll use the latter for this section.

First, let's start with a dummy project:

```shell
$ rebar3 new app break_check
===> Writing break_check/src/break_check_app.erl
===> Writing break_check/src/break_check_sup.erl
===> Writing break_check/src/break_check.app.src
===> Writing break_check/rebar.config
===> Writing break_check/.gitignore
===> Writing break_check/LICENSE
===> Writing break_check/README.md
$ cd break_check
```

Open the `rebar.config` file and make sure it looks like this:

```erlang
{deps, [recon]}.

{profiles, [
    {test, [{erl_opts, [nowarn_export_all]}]}
]}.

{shell, [
  % {config, "config/sys.config"},
    {apps, [break_check]}
]}.
```

You can then add the following test suite and save it under the name `test/start_SUITE.erl`:

```erlang
-module(start_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-compile(export_all).

all() -> [start].

start(_Config) ->
    ?assertEqual(ok, application:start(break_check)),
    Props = supervisor:count_children(break_check_sup),
    ?assertEqual(1, proplists:get_value(workers, Props)),
    ok.
```

If you run the tests, you'll see a failure such as:

```shell
$ rebar3 ct
===> Verifying dependencies...
===> Compiling break_check
===> Running Common Test suites...
%%% start_SUITE:
%%% start_SUITE ==> start: FAILED
%%% start_SUITE ==>
Failure/Error: ?assertEqual(1, proplists : get_value ( workers , Props ))
  expected: 1
       got: 0
      line: 11
```

So clearly something broke since fewer workers are seen than expected.

To figure out what happens at this point, looking at the code is sufficient, but let's pretend we have no idea why no children exist.

We'll use the breakpoints to let us set up tracing. Change the test to look like this:

```erlang
-module(start_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-compile(export_all).

all() -> [start].

start(_Config) ->
    r3:break(), % <===== this is the new line
    ?assertEqual(ok, application:start(break_check)),
    Props = supervisor:count_children(break_check_sup),
    ?assertEqual(1, proplists:get_value(workers, Props)),
    ok.
```

```shell
$ rebar3 shell --start-clean
...
1> r3:async_do(ct).
===> This feature is experimental and may be modified or removed at any time.
ok
2> Verifying dependencies...
Compiling break_check
Running Common Test suites...
%%% start_SUITE:
=== BREAK ===
```

At this point, we have a shell, and the test is paused. We can set up our own trace probes:

```plain
%% Set up a trace for all functions in the supervisor, returning their result
2> recon_trace:calls({break_check_sup, '_', fun(_) -> return_trace() end}, 10).
0
%% 0 functions matching on a wildcard means the module is likely not loaded,
%% so let's do that
3> l(break_check_sup).
{module,break_check_sup}
%% Try setting the trace call again
4> recon_trace:calls({break_check_sup, '_', fun(_) -> return_trace() end}, 10).
4  % <-- and now we have a match
```

From this point on, all we have to do is resume the breakpoint and see the traces take place:

```plain
5> r3:resume().
ok

13:34:51.207925 <0.249.0> break_check_sup:start_link()

13:34:51.208075 <0.250.0> break_check_sup:init([])

13:34:51.208197 <0.250.0> break_check_sup:init/1 --> {ok,
                                                      {{one_for_all,0,1},[]}}

13:34:51.208350 <0.249.0> break_check_sup:start_link/0 --> {ok,<0.250.0>}
7>
%%% start_SUITE ==> start: FAILED
%%% start_SUITE ==>
Failure/Error: ?assertEqual(1, proplists : get_value ( workers , Props ))
  expected: 1
       got: 0
      line: 12
```

We still get the same failure, but now we can clearly see that the problem is that the `init/1` function does not return any single child. It is therefore not surprising that no workers can be found in the supervisor.

The essence of breaking to trace is that you will want to stall the test early on; you know what information you need, you just want a way to get it. You do your little observation set up (you could not only trace, but also change logging levels), and then get your data before leaving.

## Break to Poke

This pattern is similar to the previous one, but we'll instead want to interact with the system, poke at it, and possibly change some values to see how things work.

For this section, we'll reuse the same set up as for the previous one, but we'll change the breakpoint's location and the test a bit:

```erlang
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-compile(export_all).

all() -> [start].

start(_Config) ->
    ?assertEqual(ok, application:start(break_check)),
    Props = supervisor:count_children(break_check_sup),
    case proplists:get_value(workers, Props) of
        0 -> r3:break();
        _ -> ignore
    end,
    ?assertEqual(1, proplists:get_value(workers, Props)),
    ok.
```

In this version, we not only set the breakpoint after checking for the children, we also only trigger it when a result is unexpected. This can be useful when you have some tests that do not always fail, only sometimes. Since breakpoints are normal function calls, you can even leave them in place with instructions to start debugging at this point.

Let's run the test to see it stall on our breakpoint:

```shell
$ rebar3 shell --start-clean
...
1> r3:async_do(ct).
===> This feature is experimental and may be modified or removed at any time.
ok
2> Verifying dependencies...
Compiling break_check
Running Common Test suites...
%%% start_SUITE:
=== BREAK ===
```

Now we can look through the system. The application is started, the supervisor is running, and we can manipulate things, such as starting a fake child to see if that would fix the test:

```plain
%% Poke at the supervisor's state and see it has no children
2> sys:get_state(break_check_sup).
{state,{local,break_check_sup},
       one_for_all,
       {[],#{}},
       undefined,0,1,[],0,break_check_sup,[]}

%% Add a child by hand
3> supervisor:start_child(
3>   break_check_sup,
3>   #{id => some_child,
3>     start => {gen_event, start_link, []},
3>     type => worker}
3> ).
{ok,<0.232.0>}

%% Check that it is actually tracked
4> sys:get_state(break_check_sup).
{state,{local,break_check_sup},
       one_for_all,
       {[some_child],
        #{some_child =>
              {child,<0.232.0>,some_child,
                     {gen_event,start_link,[]},
                     permanent,5000,worker,
                     [gen_event]}}},
       undefined,0,1,[],0,break_check_sup,[]}

%% and keep going to see the test pass
5> r3:resume().
ok
All 1 tests passed.
```

It would be a good idea to remove all of your breakpoints after you're done with them, but it's interesting to know you can use features such as Common Test's [repeated executions with groups](https://erlang.org/doc/apps/common_test/write_test_chapter.html#test-case-groups) or randomized runs in order to detect weird conditions and automatically trigger a break point when required.
