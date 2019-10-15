.PHONY: all rel compile clean deploy prepare deps test ct eunit prop

REBAR = ./rebar3
APPS_EBIN := $(wildcard _build/default/lib/*/ebin)
AMOC_EBIN := $(wildcard _build/default/lib/amoc/ebin)
PROP_FILES := $(shell ls test/ | grep 'prop_.*\.erl')
DIALYZER_WARNING_OPTS = -Wno_return -Wno_match -Wno_unused

ifdef SUITE
SUITE_OPTS = --suite $$SUITE
endif

ifdef PROP
PROP_OPTS = -suite $$PROP
else
PROP_OPTS = -suite $(PROP_FILES)
endif

all: rel deploy

rel: compile
	$(REBAR) tar

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

deploy: rel
	ansible-playbook -i hosts ansible/amoc-master.yml
	ansible-playbook -i hosts ansible/amoc-slaves.yml

prepare:
	ansible-playbook -i hosts ansible/amoc-prepare.yml $(ARGS)

ct:
	$(REBAR) ct --verbose $(SUITE_OPTS)

prop:
	mkdir -p logs
	ct_run -logdir logs -pa $(APPS_EBIN) $(PROP_OPTS)

eunit:
	$(REBAR) eunit $(SUITE_OPTS)

test: compile eunit ct prop

console:
	erl -pa $(APPS_EBIN)

dialyzer/erlang.plt:
	mkdir -p dialyzer
	dialyzer --build_plt \
		--output_plt dialyzer/erlang.plt \
		--output dialyzer/erlang.log \
		--apps kernel stdlib erts crypto compiler ssl

dialyzer/apps.plt:
	mkdir -p dialyzer
	dialyzer --build_plt \
		--output_plt dialyzer/apps.plt \
		--output dialyzer/apps.log \
		$(APPS_EBIN) \
		|| [ $$? -eq 2 ]

erlang_plt: dialyzer/erlang.plt
	dialyzer --check_plt \
		--plt dialyzer/erlang.plt \
		--output dialyzer/erlang.log

apps_plt: dialyzer/apps.plt
	dialyzer --check_plt \
		--plt dialyzer/apps.plt \
		--output dialyzer/apps.log

dialyzer: erlang_plt apps_plt
	dialyzer \
		--plts dialyzer/*.plt \
		--no_check_plt \
		--get_warnings \
		--output dialyzer/error.log \
		$(AMOC_EBIN) \
		$(DIALYZER_WARNING_OPTS)
