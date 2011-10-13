all: generate

generate: compile
	cd rel && ../rebar generate -f

compile:
	./rebar compile

run:
	cd rel/beagle && ./bin/beagle console
