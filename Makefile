.PHONY: clean xref_warnings deps test

REBAR=$(PWD)/rebar3
RETEST=$(PWD)/deps/retest/retest

DEPS_PLT=$(CURDIR)/.depsolver_plt

all:
	@./bootstrap/rebar get-deps compile escriptize

clean:
	@rm -rf rebar3 ebin/*.beam inttest/rt.work rt.work .eunit
	@rm -f .rebarinfo

distclean: clean
	@rm -rf deps

debug:
	@./bootstrap/bootstrap debug

check: debug xref deps test

xref:
	@./rebar3 xref

$(DEPS_PLT):
	@echo Building local erts plt at $(DEPS_PLT)
	@echo
	dialyzer --output_plt $(DEPS_PLT) --build_plt \
	--apps erts kernel stdlib -r deps

dialyzer: $(DEPS_PLT)
	dialyzer --fullpath --plt $(DEPS_PLT) -Wno_opaque -Wrace_conditions -r ./ebin

binary: VSN = $(shell ./rebar3 -V)
binary: clean all
	@cp rebar3 ../rebar.wiki/rebar
	(cd ../rebar.wiki && git commit -m "Update $(VSN)" rebar)

test:
	rebar ct

travis: all test
