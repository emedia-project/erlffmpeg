REBAR=$(shell which rebar || echo ./rebar)

all: compile

get-deps:
	@$(REBAR) get-deps
	@$(REBAR) check-deps

compile: get-deps
	@$(REBAR) compile

tests: compile
	@$(REBAR) eunit skip_deps=true

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

realclean: clean
	@$(REBAR) delete-deps

gen-doc: clean-doc
	@mkdir doc
	@$(REBAR) doc skip_deps=true

clean-doc: doc
	@rm -rf doc

dev: compile
	@erl -pa ebin include deps/*/ebin deps/*/include apps/*/ebin apps/*/include 
