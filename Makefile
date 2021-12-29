.PHONY: default rel compile clean ct test integration_test dialyzer xref console lint 

REBAR = rebar3

ifdef SUITE
SUITE_OPTS = --suite $$SUITE
endif

## this target is triggered when amoc is used as an erlang.mk dependency
default:
	$(REBAR) compile

rel:
	$(REBAR) as demo tar

compile:
	$(REBAR) as demo compile

clean:
	rm -rf _build
	rm -rfv priv/scenarios_ebin/*.beam

ct:
	rm -rfv priv/scenarios_ebin/*.beam
	## eunit and ct commands always add a test profile to the run
	$(REBAR) ct --verbose $(SUITE_OPTS)

test: compile xref lint dialyzer ct

integration_test:
	./integration_test/stop_demo_cluster.sh
	./integration_test/build_docker_image.sh
	./integration_test/start_demo_cluster.sh
	./integration_test/test_amoc_cluster.sh
	./integration_test/test_distribute_scenario.sh
	./integration_test/test_run_scenario.sh
	./integration_test/test_add_new_node.sh

rerun_integration_test:
	./integration_test/stop_demo_cluster.sh
	./integration_test/start_demo_cluster.sh
	./integration_test/test_amoc_cluster.sh
	./integration_test/test_distribute_scenario.sh
	./integration_test/test_run_scenario.sh
	./integration_test/test_add_new_node.sh

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
