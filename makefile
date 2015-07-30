.PHONY: all rel compile deploy prepare deps

all: rel deploy

rel: compile
	./relx tar

compile: deps
	./rebar compile

deps:
	./rebar get-deps

deploy: rel
	ansible-playbook -i hosts ansible/amoc-master.yml;
	ansible-playbook -i hosts ansible/amoc-slaves.yml

prepare:
	ansible-playbook -i hosts ansible/amoc-prepare.yml $(ARGS)
