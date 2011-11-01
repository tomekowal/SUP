
PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

compile:
	@$(REBAR) compile

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)

