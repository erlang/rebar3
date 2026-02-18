# Coverage

Test runs from all of the built-in test tools generate cover data. Calling `rebar3 cover` at any later point generates a general code coverage report by merging all the individual reports:

```shell
$ rebar3 ct --dir test/suites1 --cover --cover_export_name=suites1
===> Running Common Test suites...
...
$ rebar3 ct --dir test/suites2 --cover --cover_export_name=suites2
===> Running Common Test suites...
...
$ ls _build/test/cover
cover.log    suite1.coverdata    suite2.coverdata
$ rebar3 cover --verbose
===> Performing cover analysis...
  |----------------------------|------------|
  |                    module  |  coverage  |
  |----------------------------|------------|
  |                     ....   |       Y%   |
  |----------------------------|------------|
  |                     total  |       X%   |
  |----------------------------|------------|
  coverage calculated from:
    _build/test/cover/suites1.coverdata
    _build/test/cover/suites2.coverdata
  cover summary written to: _build/test/cover/index.html
```
