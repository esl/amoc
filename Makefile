.PHONY: all rel compile clean deploy prepare deps test ct eunit prop

APPS_EBIN := $(wildcard _build/default/lib/*/ebin)
AMOC_EBIN := $(wildcard _build/default/lib/amoc/ebin)
PROP_FILES := $(shell ls test/ | grep 'prop_.*\.erl')
DIALYZER_WARNING_OPTS = -Wno_return -Wno_match -Wno_unused


all: rel deploy

rel: compile
	./rebar3 tar

compile:
	./rebar3 compile

clean:
	./rebar3 clean

deploy: rel
	ansible-playbook -i hosts ansible/amoc-master.yml;
	ansible-playbook -i hosts ansible/amoc-slaves.yml

prepare:
	ansible-playbook -i hosts ansible/amoc-prepare.yml $(ARGS)

ct:
	@if [ $$SUITE ]; then ./rebar3 ct --verbose --suite $$SUITE; \
		else ./rebar3 ct --verbose; fi

prop:
	@if [ $$PROP ]; then ct_run -logdir logs -pa $(APPS_EBIN) -suite $$PROP; \
		else ct_run -pa $(APPS_EBIN) -suite $(PROP_FILES); fi

eunit:
	@if [ $$SUITE ]; then ./rebar3 eunit --suite $$SUITE; \
		else ./rebar3 eunit; fi

test: compile eunit ct prop

console:
	erl -pa $(APPS_EBIN)

dialyzer/erlang.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/erlang.plt \
		-o dialyzer/erlang.log --apps kernel stdlib erts crypto compiler ssl; \
		status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/apps.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/apps.plt \
	-o dialyzer/apps.log $(APPS_EBIN); \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

erlang_plt: dialyzer/erlang.plt
	@dialyzer --plt dialyzer/erlang.plt --check_plt -o dialyzer/erlang.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

apps_plt: dialyzer/apps.plt
	@dialyzer --plt dialyzer/apps.plt --check_plt -o dialyzer/apps.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer: erlang_plt apps_plt
	@dialyzer --plts dialyzer/*.plt --no_check_plt \
		--get_warnings -o dialyzer/error.log $(AMOC_EBIN) $(DIALYZER_WARNING_OPTS)
