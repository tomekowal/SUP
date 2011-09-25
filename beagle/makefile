all: generate

generate: compile
	cd rel && ../rebar generate -f

compile:
	./rebar compile

run: generate
	cd rel/beagle && ./bin/beagle console
