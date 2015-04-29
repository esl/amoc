.PHONY: all rel compile do deploy

all: rel deploy

rel: compile
	./relx tar

compile:
	./rebar get-deps compile

do:
	erl -pa ./deps/*/ebin ./ebin -s amoc do 100

deploy:
	ansible-playbook -i hosts ansible/amoc-master.yml;
	ansible-playbook -i hosts ansible/amoc-slaves.yml

prepare:
	ansible-playbook -i hosts ansible/amoc-prepare.yml
