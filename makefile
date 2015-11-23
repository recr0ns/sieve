REBAR = `which rebar`

all: clean deps build

deps:
	@( $(REBAR) get-deps )

build:
	@( $(REBAR) compile )

rebuild: clean build

clean:
	@( $(REBAR) clean )

run:
	@( erl -pa ./ebin ./deps/*/ebin -eval "application:start(prime_numbers)" )

test:
	@( $(REBAR) eunit skip_deps=true )

good: all test run

release: all
	@( $(REBAR) generate )

.PHONY: all deps build rebuild clean run test good release
