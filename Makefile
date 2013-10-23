SOURCES = $(notdir $(wildcard src/*.erl)) regexp_parser.erl
BEAMS = $(patsubst %.erl,ebin/%.beam,$(SOURCES))

EFLAGS = +debug_info

all: ebin $(BEAMS)

src/regexp_parser.erl: src/regexp_parser.yrl
	(cd src; erl -noshell -run yecc file regexp_parser.yrl -run erlang halt)

ebin/%.beam: src/%.erl
	erlc $(EFLAGS) -o ebin $<

ebin:
	mkdir -p ebin

edoc: 
	erl -noshell -run edoc_run files '["src/jsongen.erl"]' '[{sort_functions,false},{dir,"doc"}]'

dotest:
	erl -pa ebin -run rtest test -run erlang halt
