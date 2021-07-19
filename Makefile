.PHONY: all rel compile clean ct test integration_test dialyzer xref console lint

REBARVER = 3.16.1

REBAR = rebar3

ifdef SUITE
SUITE_OPTS = --suite $$SUITE
endif

all: rel

rel: rebar3
	./rebar3 as demo tar

compile:
	$(REBAR) as demo compile

clean:
	$(REBAR) clean

ct:
	rm -rf priv/scenarios_ebin/*.beam
	$(REBAR) ct --verbose $(SUITE_OPTS)

test: compile xref lint dialyzer ct

integration_test:
	./integration_test/cleanup_containers.sh
	./integration_test/build_docker_image.sh
	./integration_test/test_docker_image.sh
	./integration_test/test_distribute_scenario.sh
	./integration_test/test_run_scenario.sh
	./integration_test/test_add_new_node.sh

rerun_integration_test:
	./integration_test/cleanup_containers.sh
	./integration_test/test_docker_image.sh
	./integration_test/test_distribute_scenario.sh
	./integration_test/test_run_scenario.sh
	./integration_test/test_add_new_node.sh

rebar3:
	wget https://github.com/erlang/rebar3/releases/download/${REBARVER}/rebar3 &&\
	chmod u+x rebar3

dialyzer:
	$(REBAR) as demo dialyzer

xref:
	$(REBAR) as demo xref

console:
	@echo "tests can be executed manually using ct:run/1 function:\n" \
	      '   ct:run("test").'
	$(REBAR) as test shell

lint:
	$(REBAR) as elvis lint
