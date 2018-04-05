.PHONY: all rel compile clean deploy prepare deps test ct eunit prop

DEPS := $(wildcard _build/default/rel/amoc/lib/*/ebin)
AMOC := $(wildcard _build/default/rel/amoc/lib/amoc-*/ebin)
PROP_FILES := $(shell ls test/ | grep 'prop_.*\.erl')


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
	@if [ $$PROP ]; then ct_run -logdir logs -pa $(DEPS) -suite $$PROP; \
		else ct_run -pa $(DEPS) -suite $(PROP_FILES); fi

eunit:
	@if [ $$SUITE ]; then ./rebar3 eunit --suite $$SUITE; \
		else ./rebar3 eunit; fi

test: compile eunit ct prop

console:
	erl -pa $(DEPS)

dialyzer/erlang.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/erlang.plt \
		-o dialyzer/erlang.log --apps kernel stdlib erts crypto compiler ssl; \
		status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/deps.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/deps.plt \
	-o dialyzer/deps.log $(DEPS); \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/amoc.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/amoc.plt \
		-o dialyzer/amoc.log $(AMOC); \
		status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

erlang_plt: dialyzer/erlang.plt
	@dialyzer --plt dialyzer/erlang.plt --check_plt -o dialyzer/erlang.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

amoc_plt: dialyzer/amoc.plt
	@dialyzer --plt dialyzer/amoc.plt --check_plt -o dialyzer/amoc.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

deps_plt: dialyzer/deps.plt
	@dialyzer --plt dialyzer/deps.plt --check_plt -o dialyzer/deps.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer: erlang_plt deps_plt amoc_plt
	@dialyzer --plts dialyzer/*.plt --no_check_plt \
	--get_warnings -o dialyzer/error.log $(AMOC)
