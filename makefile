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
	@if [ $$SUITE ]; then ./rebar3 as test ct -v --suite $$SUITE; \
		else ./rebar3 as test ct -v; fi

prop: compile
	@if [ $$PROP ]; then ./rebar3 as test proper --long_result 100 -m $$PROP; \
		else ./rebar3 as test proper; fi

eunit: compile
	@if [ $$SUITE ]; then ./rebar3 as test eunit --suite $$SUITE; \
		else ./rebar3 as test eunit; fi

test: compile eunit ct prop

console:
	erl -pa ebin/ -pa _build/default/lib/*/ebin
