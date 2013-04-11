SOURCES = $(notdir $(wildcard src/*.erl))
BEAMS = $(patsubst %.erl,ebin/%.beam,$(SOURCES))

EFLAGS = +debug_info

all: ebin $(BEAMS)

ebin/%.beam: src/%.erl
	erlc $(EFLAGS) -o ebin $<

ebin:
	mkdir -p ebin

edoc: 
	erl -noshell -run edoc_run files '["src/jsongen.erl"]' '[{sort_functions,false},{dir,"doc"}]'
