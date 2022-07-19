# eunit_formatters

Because eunit's output sucks. Let's make it better.

Here's the progress formatter running with profiling and ANSI colors
turned on:

![neotoma eunit](demo.gif)

## Setup

### Rebar 3

[rebar3](https://github.com/erlang/rebar3) already includes this
library! There's no need for special configuration at all.

### erlang.mk

For erlang.mk, add the following before `include erlang.mk`:

``` Makefile
TEST_DEPS = eunit_formatters

EUNIT_OPTS = no_tty, {report, {eunit_progress, [colored, profile]}}
```

### Rebar 2 (legacy)
Add `eunit_formatters` as a dep in your `rebar.config`. Now configure
eunit to use one of the output formatters (currently only
`eunit_progress`):

```erlang
{eunit_opts, [
     no_tty,  %% This turns off the default output, MUST HAVE
     {report, {eunit_progress, [colored, profile]}} %% Use `profile' to see test timing information
     %% Uses the progress formatter with ANSI-colored output
     ]}.
```

## License

   Copyright 2014 Sean Cribbs

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
