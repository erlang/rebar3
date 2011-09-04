all:
	./bootstrap

clean:
	@rm -rf rebar ebin/*.beam inttest/rt.work

debug:
	./bootstrap debug

check: debug
	-@./rebar xref
	-@dialyzer ebin --verbose -Wunmatched_returns -Werror_handling \
		-Wrace_conditions
