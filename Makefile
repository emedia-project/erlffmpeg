PROJECT = erlffmpeg

DEPS = ucp jsx
dep_ucp = git https://github.com/glejeune/UnicodeCodePoints.git master
dep_jsx = git https://github.com/talentdeficit/jsx.git master

include erlang.mk

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include 

