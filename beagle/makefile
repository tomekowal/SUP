all: generate

generate: compile
	cd rel && ../rebar generate -f

compile:
	./rebar compile

generate-upgrade:
	./rebar generate-appups previous_release=$(previous_release)
	./rebar generate-upgrade previous_release=$(previous_release)

run:
	cd rel/beagle && ./bin/beagle console
