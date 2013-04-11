SOURCES = $(notdir $(wildcard src/*.erl))
BEAMS = $(patsubst %.erl,ebin/%.beam,$(SOURCES))

EFLAGS = +debug_info

all: ebin $(BEAMS)

ebin/%.beam: src/%.erl
	erlc $(EFLAGS) -o ebin $<

ebin:
	mkdir -p ebin
