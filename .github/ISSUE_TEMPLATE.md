### Pre-Check ###

- If you are filing for a bug, please do a quick search in current issues first
- For bugs, mention if you are willing or interested in helping fix the issue
- For questions or support, it helps to include context around your project or problem
- Think of a descriptive title (more descriptive than 'feature X is broken' unless it is fully broken)

### Environment ###

- Add the result of `rebar3 report` to your message:

```
$ rebar3 report "my failing command"
...
```

- Verify whether the version of rebar3 you're running is the latest release (see https://github.com/erlang/rebar3/releases)
- If possible, include information about your project and its structure. Open source projects or examples are always easier to debug.
  If you can provide an example code base to reproduce the issue on, we will generally be able to provide more help, and faster.

### Current behaviour ###

Describe the current behaviour. In case of a failure, crash, or exception, please include the result of running the command with debug information:

```
DEBUG=1 rebar3 <my failing command>
```

### Expected behaviour ###

Describe what you expected to happen.
