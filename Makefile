.PHONY: clean dialyzer_warnings xref_warnings deps test

REBAR=$(PWD)/rebar3
RETEST=$(PWD)/deps/retest/retest

all:
	./bootstrap/bootstrap

clean:
	@rm -rf rebar3 ebin/*.beam inttest/rt.work rt.work .eunit
	@rm -f .rebarinfo

distclean: clean
	@rm -f dialyzer_warnings
	@rm -rf deps

debug:
	@./bootstrap/bootstrap debug

check: debug xref dialyzer deps test

xref:
	@./rebar3 xref

dialyzer: dialyzer_warnings
	@diff -U0 dialyzer_reference dialyzer_warnings

dialyzer_warnings:
	-@dialyzer -q -nn -n ebin -Wunmatched_returns -Werror_handling \
		-Wrace_conditions > dialyzer_warnings

binary: VSN = $(shell ./rebar3 -V)
binary: clean all
	@cp rebar3 ../rebar.wiki/rebar
	(cd ../rebar.wiki && git commit -m "Update $(VSN)" rebar)

test:
	rebar ct

travis: all test
