.PHONY: default rel deps compile clean ct lint dialyzer xref console
.PHONY: test integration_test rerun_integration_test

ifdef SUITE
SUITE_OPTS = --suite $$SUITE
endif

default: compile

rel:
	rebar3 release

deps:
	rebar3 deps
	rebar3 compile --deps_only

compile:
	rebar3 compile

clean:
	rm -rf _build

ct:
	## in order to run some specific test suite(s) you can override
	## the SUITE variable from the command line or as env variable:
	##     make ct SUITE=some_test_SUITE
	##     make ct SUITE=some_test_SUITE,another_test_SUITE
	##     SUITE=some_test_SUITE make ct
	##     SUITE=some_test_SUITE,another_test_SUITE make ct
	@ echo rebar3 ct -c --verbose $(SUITE_OPTS)
	@ rebar3 ct -c --verbose $(SUITE_OPTS)

lint:
	rebar3 lint

test: compile xref dialyzer ct lint codecov

codecov:
	rebar3 as test codecov analyze

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
	rebar3 dialyzer

xref:
	rebar3 xref

console:
	@echo "tests can be executed manually using ct:run/1 function:\n" \
	      '   ct:run("test").'
	rebar3 as test shell
