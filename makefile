.PHONY: all rel compile clean deploy prepare deps test ct eunit prop

all: rel deploy

rel: compile
	./relx tar

compile: deps
	./rebar compile

clean:
	./rebar clean
	rm -rf test/*.beam

deps:
	./rebar get-deps

deps := $(wildcard deps/*/ebin)

deploy: rel
	ansible-playbook -i hosts ansible/amoc-master.yml;
	ansible-playbook -i hosts ansible/amoc-slaves.yml

prepare:
	ansible-playbook -i hosts ansible/amoc-prepare.yml $(ARGS)

ct:
	@if [ $$SUITE ]; then ./rebar skip_deps=true -v ct suite=$$SUITE; \
		else ./rebar skip_deps=true -v ct; fi

prop_files := $(shell ls test/ | grep 'prop_.*\.erl')

prop:
	@if [ $$PROP ]; then ct_run -logdir logs -pa ebin/ -pa deps/*/ebin/ -suite $$PROP; \
		else ct_run -pa ebin/ -pa deps/*/ebin/ -suite $(prop_files); fi

eunit:
	@if [ $$SUITE ]; then ./rebar skip_deps=true eunit suite=$$SUITE; \
		else ./rebar skip_deps=true eunit; fi

test: compile eunit ct prop

dialyzer/erlang.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/erlang.plt \
		-o dialyzer/erlang.log --apps kernel stdlib erts crypto compiler ssl; \
		status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/deps.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/deps.plt \
	-o dialyzer/deps.log $(deps); \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/amoc.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/amoc.plt \
		-o dialyzer/amoc.log ebin; \
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
	--get_warnings -o dialyzer/error.log ebin
