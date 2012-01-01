.PHONY: dialyzer_warnings xref_warnings

all:
	./bootstrap

clean:
	@rm -rf rebar ebin/*.beam inttest/rt.work

debug:
	@./bootstrap debug

check: debug xref dialyzer

xref: xref_warnings
	@diff -U0 xref_reference xref_warnings

xref_warnings:
	-@./rebar xref > xref_warnings

dialyzer: dialyzer_warnings
	@diff -U0 dialyzer_reference dialyzer_warnings

dialyzer_warnings:
	-@dialyzer -q -n ebin -Wunmatched_returns -Werror_handling \
		-Wrace_conditions > dialyzer_warnings
