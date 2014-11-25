.PHONY: compile do

compile:
	./rebar get-deps compile

do:
	erl -pa ./deps/*/ebin ./ebin -s amoc do 100
