.PHONY: all rel compile clean ct test integration_test dialyzer xref console

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
	rm -rf priv/scenarios_ebin/*.beam
	$(REBAR) ct --verbose $(SUITE_OPTS)

test: compile ct

integration_test:
	./integration_test/cleanup_containers.sh
	./integration_test/build_docker_image.sh
	./integration_test/test_docker_image.sh
	./integration_test/test_distribute_scenario.sh
	./integration_test/test_run_scenario.sh
	./integration_test/test_add_new_node.sh

dialyzer:
	$(REBAR) as prod dialyzer

xref:
	$(REBAR) as prod xref

console:
	rebar3 shell