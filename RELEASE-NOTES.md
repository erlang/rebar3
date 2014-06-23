# Rebar 2.5.0

* Reverted rebar/281: [Move include/rebar.hrl to src/rebar.hrl](https://github.com/rebar/rebar/pull/281) as it broke backwards compatibility

# Rebar 2.4.0

* rebar/52: [Slim release support](https://github.com/rebar/rebar/pull/52)
* rebar/112: [Add code coverage analysis functionality to `qc'](https://github.com/rebar/rebar/pull/112)
* rebar/119: [Add qualified name tests specification (see #118)](https://github.com/rebar/rebar/pull/119)
* rebar/130: [ct fixes](https://github.com/rebar/rebar/pull/130)
* rebar/136: [Add support for the Perforce VCS client via the "p4" tool](https://github.com/rebar/rebar/pull/136)
* rebar/195: [Switch template instructions](https://github.com/rebar/rebar/pull/195)
* rebar/229: [Add REBAR to environment before executing hooks](https://github.com/rebar/rebar/pull/229)
* rebar/260: [Quote include/lib paths to handle spaces in Erlang installs (fixes build on windows)](https://github.com/rebar/rebar/pull/260)
* rebar/280: [improve output when using `rebar shell`](https://github.com/rebar/rebar/pull/280)
* rebar/281: [Move include/rebar.hrl to src/rebar.hrl](https://github.com/rebar/rebar/pull/281)
* rebar/284: [Error 'Command not found' when sname is used](https://github.com/rebar/rebar/pull/284)
* rebar/285: [Fix #249 (erlc regression)](https://github.com/rebar/rebar/pull/285)
* rebar/288: [Extend and document contributing rules](https://github.com/rebar/rebar/pull/288)
* rebar/289: [erlc: fix typo in update_erlcinfo/3 clause that would make the function fail](https://github.com/rebar/rebar/pull/289)
* rebar/290: [erlc: replace if expression with case of](https://github.com/rebar/rebar/pull/290)
* rebar/292: [Namespaced types: fix build for 17.0](https://github.com/rebar/rebar/pull/292)
* rebar/296: [Add gen_event template](https://github.com/rebar/rebar/pull/296)


# Rebar 2.3.1

## PR's Merged

* rebar/242: [Extra commits for #129](https://github.com/rebar/rebar/pull/242)
* rebar/244: [Document skip_apps=, apps=, and require_*_vsn](https://github.com/rebar/rebar/pull/244)
* rebar/251: [Make sure that eunit/qc_compile_opts works](https://github.com/rebar/rebar/pull/251)
* rebar/255: [rebar.app: remove superfluous quoting](https://github.com/rebar/rebar/pull/255)
* rebar/274: [Use lowercase for Windows drive name to resolve issue #250](https://github.com/rebar/rebar/pull/274)

# Rebar 2.3.0

## PR's Merged

* rebar/98: [Repetition of environment variable definitions in child processes (ports)](https://github.com/rebar/rebar/pull/98)
* rebar/115: [Incorrect REMSH args when sname is used.](https://github.com/rebar/rebar/pull/115)
* rebar/129: [Speed up the compilation process v5](https://github.com/rebar/rebar/pull/129)
* rebar/139: [Allow specification of module dependencies for appups](https://github.com/rebar/rebar/pull/139)
* rebar/175: [CWD plugins regression](https://github.com/rebar/rebar/pull/175)
* rebar/188: [Xref extra path](https://github.com/rebar/rebar/pull/188)
* rebar/208: [Fix typo in rebar_erlydtl_compiler](https://github.com/rebar/rebar/pull/208)
* rebar/219: [Added R16B01 and R16B02 to travis config.](https://github.com/rebar/rebar/pull/219)
* rebar/221: [Adapt erlydtl compiler plugin to latest version of erlydtl](https://github.com/rebar/rebar/pull/221)
* rebar/223: [Add random_suite_order option to eunit command](https://github.com/rebar/rebar/pull/223)
* rebar/224: [allow suites or tests as options for eunit and ct](https://github.com/rebar/rebar/pull/224)
* rebar/230: [eunit: fix dialyzer warnings introduced in 03da5e0b](https://github.com/rebar/rebar/pull/230)
* rebar/232: [Document support for abbreviated commands](https://github.com/rebar/rebar/pull/232)
* rebar/233: [docs: fix #228](https://github.com/rebar/rebar/pull/233)
* rebar/234: [Fix #220 (Reported-by: Joseph Norton)](https://github.com/rebar/rebar/pull/234)
* rebar/237: [Add partial support for Erlang/OTP 17](https://github.com/rebar/rebar/pull/237)
* rebar/252: [file_utils: properly report errors (fix #95)](https://github.com/rebar/rebar/pull/252)
* rebar/254: [Fix 'rebar generate' regression (#253)](https://github.com/rebar/rebar/pull/254)
* rebar/265: [Fix 'rebar help clean' function_clause error](https://github.com/rebar/rebar/pull/265)
* rebar/268: [Fix #267 (code path regression)](https://github.com/rebar/rebar/pull/268)
* rebar/269: [Update THANKS](https://github.com/rebar/rebar/pull/269)

# Rebar 2.2.0

## PR's Merged

* rebar/152: [Fix erl_opts use](https://github.com/rebar/rebar/pull/152)
* rebar/154: [Fix update-deps with certain forms of the {tag, ...} type](https://github.com/rebar/rebar/pull/154)
* rebar/155: [Fixes for #137 and #142](https://github.com/rebar/rebar/pull/155)
* rebar/157: [Don't over-aggressively clean the code path in the presence of lib_dir directives](https://github.com/rebar/rebar/pull/157)
* rebar/172: [Add missing dep examples and fix existing ones](https://github.com/rebar/rebar/pull/172)
* rebar/173: [Fix false reporting of (plain) vsn strings](https://github.com/rebar/rebar/pull/173)
* rebar/174: [rebar_core: fix Dialyzer warning introduced in aa46d85 (#157)](https://github.com/rebar/rebar/pull/174)
* rebar/177: [Delete unused inttest/retest binary](https://github.com/rebar/rebar/pull/177)
* rebar/179: [Make list of commands (for unabbreviation) easier to maintain](https://github.com/rebar/rebar/pull/179)
* rebar/183: [generate-upgrade can now take target_dir argument](https://github.com/rebar/rebar/pull/183)
* rebar/184: [Fix log levels](https://github.com/rebar/rebar/pull/184)
* rebar/185: [Switch retest dep to upstream (dizzyd/retest.git)](https://github.com/rebar/rebar/pull/185)
* rebar/189: [inttest/rgen1: increase retest timeout (30s -> 60s)](https://github.com/rebar/rebar/pull/189)
* rebar/190: [inttest/rgen_1: double the timeout a second time](https://github.com/rebar/rebar/pull/190)
* rebar/191: [Fix #187 (rename getopt and mustache)](https://github.com/rebar/rebar/pull/191)
* rebar/196: [Print a more appropriate message on 'rebar info'](https://github.com/rebar/rebar/pull/196)
* rebar/198: [Clean up rebar.config.script](https://github.com/rebar/rebar/pull/198)
* rebar/199: [rebar_dia_compiler: fix Dialyzer warnings](https://github.com/rebar/rebar/pull/199)
* rebar/200: [bootstrap: avoid trying to run 'debug' command](https://github.com/rebar/rebar/pull/200)
* rebar/201: [Added a library template.](https://github.com/rebar/rebar/pull/201)
* rebar/210: [Fix #205 (erlydtl:compile/3 returns warnings)](https://github.com/rebar/rebar/pull/210)
* rebar/212: [Fix basho/rebar#388](https://github.com/rebar/rebar/pull/212)
* rebar/214: [Document compile_only=true](https://github.com/rebar/rebar/pull/214)
* rebar/215: [Remove experimental flags](https://github.com/rebar/rebar/pull/215)
