compile:
	rebar3 compile

docs:
	env ERL_LIBS=$(shell pwd)/_build/default/lib/edown rebar3 edoc

build:
	make priv
	make compile
	make docs
	make javalib
	make javaerlang

priv:
	mkdir -p priv

javalib:
	(cd _build/default/lib; rm -rf json_schema_validator; git clone https://github.com/fge/json-schema-validator.git json_schema_validator; cd json_schema_validator; ./gradlew build)

javaerlang:
	(cd _build/default/lib/java_erlang; make install)

install:
	make build
	make doinstall

doinstall:
	erl -pa _build/default/lib/jsongen/ebin/ -noshell -run jsongen_install install -run erlang halt
