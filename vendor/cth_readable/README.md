# `cth_readable`

An OTP library to be used for CT log outputs you want to be readable
around all that noise they contain.

There are currently the following hooks:

1. `cth_readable_shell`, which shows failure stacktraces in the shell and
   otherwise shows successes properly, in color.
2. `cth_readable_compact_shell`, which is similar to the previous ones, but
   only ouputs a period (`.`) for each successful test
3. `cth_readable_failonly`, which only outputs error and SASL logs to the
   shell in case of failures. It also provides `cthr:pal/1-4` functions,
   working like `ct:pal/1-4`, but being silenceable by that hook. A parse
   transform exists to automatically convert `ct:pal/1-3` into `cthr:pal/1-3`.
   Also automatically handles lager. This hook buffers the IO/logging events,
   and the buffer size can be limited with the `max_events` config option. The
   default value is `inf` which means that all events are buffered.
4. `cth_readable_nosasl`, which disables all SASL logging. It however requires
   to be run *before* `cth_readable_failonly` to work.

## What it looks like

![example](http://i.imgur.com/dDFNxZr.png)
![example](http://i.imgur.com/RXZBG7H.png)

## Usage with rebar3

Supported and enabled by default.

## Usage with  rebar2.x

Add the following to your `rebar.config`:

```erlang
{deps, [
    {cth_readable, {git, "https://github.com/ferd/cth_readable.git", {tag, "v1.5.1"}}}
    ]}.

{ct_compile_opts, [{parse_transform, cth_readable_transform}]}.
{ct_opts, [{ct_hooks, [cth_readable_failonly, cth_readable_shell]}]}.
%% Or add limitations to how many messages are buffered with:
%%  {ct_opts, [{ct_hooks, [{cth_readable_failonly, [{max_events, 1000}]}, cth_readable_shell]}]}.
```

## Usage with lager

If your lager handler has a custom formatter and you want that formatter
to take effect, rather than using a configuration such as:

```erlang
{lager, [
  {handlers, [{lager_console_backend,
                [info, {custom_formatter, [{app, "some-val"}]}]}
             ]}
]}.
```

Use:

```erlang
{lager, [
  {handlers, [{cth_readable_lager_backend,
                [info, {custom_formatter, [{app, "some-val"}]}]}
             ]}
]}.
```

It will let you have both proper formatting and support for arbitrary
configurations.

## Changelog
1.5.1:
- Adding support for `cthr:pal/5` (thanks @ashleyjlive)

1.5.0:
- Adding an optional bound buffer in `cth_readable_failonly` (thanks @TheGeorge)
- (published to hex but never to github, ended up with a messy commit tree)

1.4.9:
- No change, re-pushing the hex.pm package since it had an untracked dependency somehow

1.4.8:
- Fixed handling of comments in EUnit macros

1.4.7:
- Fixed handling of the result of an `?assertNot()` macro

1.4.6:
- Reloading formatter config for logs after each test where the information needs to be printed

1.4.5:
- Restoring proper logs for Lager in OTP-21+. A problem existed when `error_logger` was no longer registered by default and lager log lines would silently get lost.

1.4.4:
- Better interactions with Lager; since newer releases, it removes the Logger default interface when starting, which could cause crashes when this happened before the CT hooks would start (i.e. a eunit suite)

1.4.3:
- OTP-21.2 support (Logger interface); importing a function that was de-exported by OTP team

1.4.2:
- OTP-21.0 support (Logger interface)

1.4.1:
- OTP-21-rc2 support (Logger interface); dropping rc1 support.

1.4.0:
- OTP-21-rc1 support (Logger interface)
- Add compact shell output handler

1.3.4:
- Restore proper eunit assertion formatting

1.3.3:
- More fixes due to lager old default config formats

1.3.2:
- Fix deprecation warning on newer lagers due to old default config format

1.3.1:
- Unicode support and OTP-21 readiness.

1.3.0:
- display groups in test output. Thanks to @egobrain for the contribution

1.2.6:
- report `end_per_testcase` errors as a non-critical failure when the test case passes
- add in a (voluntarily failing) test suite to demo multiple output cases required

1.2.5:
- support for `on_tc_skip/4` to fully prevent misreporting of skipped suites

1.2.4:
- unset suite name at the end of hooks run to prevent misreporting

1.2.3:
- correct `syntax_lib` to `syntax_tools` as an app dependency

1.2.2:
- fix output for assertions

1.2.1:
- handle failures of parse transforms by just ignoring the culprit files.

1.2.0:
- move to `cf` library for color output, adding support for 'dumb' terminals

1.1.1:
- fix typo of `poplist -> proplist`, thanks to @egobrain

1.1.0:
- support for better looking EUnit logs
- support for lager backends logging to HTML files

1.0.1:
- support for CT versions in Erlang copies older than R16

1.0.0:
- initial stable release
