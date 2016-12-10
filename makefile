.PHONY: all rel compile clean deploy prepare test ct eunit prop

all: rel deploy

rel: compile
	./rebar3 tar

compile:
	./rebar3 compile

clean:
	./rebar3 clean -a

deploy: rel
	ansible-playbook -i hosts ansible/amoc-master.yml;
	ansible-playbook -i hosts ansible/amoc-slaves.yml

prepare:
	ansible-playbook -i hosts ansible/amoc-prepare.yml $(ARGS)

ct: compile
	@if [ $$SUITE ]; then ./rebar3 ct -v --suite $$SUITE; \
		else ./rebar3 ct -v; fi

prop_files := $(shell ls test/ | grep 'prop_.*\.erl')

prop: compile
	@if [ $$PROP ]; then ct_run -logdir logs -pa ebin/ -pa _build/test/lib/*/ebin/ -suite $$PROP; \
		else ct_run  -pa ebin/ -pa _build/test/lib/*/ebin/ -suite $(prop_files); fi

eunit: compile
	@if [ $$SUITE ]; then ./rebar3 eunit --suite $$SUITE; \
		else ./rebar3 eunit; fi

test: compile eunit ct prop

console:
	erl -pa ebin/ -pa _build/default/lib/*/ebin
