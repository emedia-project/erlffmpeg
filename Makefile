PROJECT = erlffmpeg

DEPS = ucp jsx 
DOC_DEPS = edown

dep_ucp = git https://github.com/glejeune/UnicodeCodePoints.git master
dep_jsx = git https://github.com/talentdeficit/jsx.git master

dep_edown = git https://github.com/uwiger/edown.git master

include erlang.mk

EDOC_OPTS = {doclet, edown_doclet} \
						, {app_default, "http://www.erlang.org/doc/man"} \
						, {source_path, ["src"]} \
						, {overview, "overview.edoc"} \
						, {stylesheet, ""} \
						, {image, ""} \
						, {edown_target, github} \
						, {top_level_readme, {"./README.md", "https://github.com/emedia-project/erlffmpeg"}} 

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include 

