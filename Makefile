.PHONY: all rel compile clean ct test console

REBAR = ./rebar3

ifdef SUITE
SUITE_OPTS = --suite $$SUITE
endif

all: rel

rel:
	$(REBAR) as prod tar

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

ct:
	$(REBAR) ct --verbose $(SUITE_OPTS)

eunit:
	$(REBAR) eunit

test: compile ct eunit

console:
	rebar3 shell