# Common Test

To run `common_test` suites:

```shell
$ rebar3 ct
```

Rebar3 will look in all your applications' `test` directories and compile and run any source files named `*_SUITE.erl`. Unlike regular source directories, the compilation of test modules will **not** be recursive by default in order to avoid issues with files in [data directories](https://erlang.org/doc/apps/common_test/write_test_chapter.html#data-and-private-directories). This behaviour [can however be turned on manually with the right compilation options](configuration/configuration.md#rebar3-compiler-options).

To run only specific test suites:

```shell
$ rebar3 ct --suite=test/first_SUITE,test/second_SUITE
```

Rebar3 has a built in `common_test` runner that supports most test suites and `common_test` options. If your test suites require use of test specs or cover specs be aware Rebar3 keeps separate build artifacts for each profile so you may need to adjust paths to point to the modules and directories in the relevant profile directory under `_build` for them to work as expected. If you need to use an unsupported `common_test` option, the following command can be used to run `common_test` with the path to the compiled beam files Rebar3 generates:

```shell
$ ct_run -pa `rebar3 path` ...
```

The `ct` command runs as the `test` profile, by default. See [Profiles](configuration/profiles.md) for details.

For available options and their usage see [Commands](commands.md) or:

```shell
$ rebar3 help ct
```
