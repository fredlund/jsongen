build:
	rebar3 compile
	(cd _build/default/lib; rm -rf json_schema_validator; git clone https://github.com/fge/json-schema-validator.git json_schema_validator; cd json_schema_validator; ./gradlew build)

install:
	make build
	make doinstall

doinstall:
	erl -noshell -run jsongen_install install -run erlang halt
