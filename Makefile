.PHONY: default rel deps compile clean ct lint dialyzer xref console
.PHONY: test integration_test rerun_integration_test

REBAR = rebar3

ifdef SUITE
SUITE_OPTS = --suite $$SUITE
endif

default: compile

rel:
	$(REBAR) tar

deps:
	$(REBAR) deps
	$(REBAR) compile --deps_only

compile:
	$(REBAR) compile

clean:
	rm -rf _build

ct:
	## in order to run just a single test suite you can override
	## the SUITE variable from the command line:
	##     make ct SUITE=some_test_SUITE
	$(REBAR) ct --verbose $(SUITE_OPTS)

lint:
	$(REBAR) as elvis lint

test: compile xref dialyzer ct lint

integration_test:
	./integration_test/stop_test_cluster.sh
	./integration_test/build_docker_image.sh
	./integration_test/start_test_cluster.sh
	./integration_test/test_amoc_cluster.sh
	./integration_test/test_distribute_scenario.sh
	./integration_test/test_run_scenario.sh
	./integration_test/test_add_new_node.sh

rerun_integration_test:
	./integration_test/stop_test_cluster.sh
	./integration_test/start_test_cluster.sh
	./integration_test/test_amoc_cluster.sh
	./integration_test/test_distribute_scenario.sh
	./integration_test/test_run_scenario.sh
	./integration_test/test_add_new_node.sh

dialyzer:
	$(REBAR) dialyzer

xref:
	$(REBAR) xref

console:
	@echo "tests can be executed manually using ct:run/1 function:\n" \
	      '   ct:run("test").'
	$(REBAR) as test shell
