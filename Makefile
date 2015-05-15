NAME = erlking

REBAR = ./rebar
COMPILECMD = $(REBAR) compile
CLEANCMD   = $(REBAR) clean

ERLANG  = erl
BEAMDIR = ebin

ERLFILES    = $(wildcard src/*.erl)
BEAMFILES   = $(ERLFILES:src/%.erl=$(BEAMDIR)/%.beam)
APPSRCFILES = $(wildcard src/*.app.src)
APPFILES    = $(APPSRCFILES:src/%.src=$(BEAMDIR)/%)

BINFILES    = $(APPFILES) $(BEAMFILES)

all: $(BINFILES)

run: all
	@$(ERLANG) -pa $(BEAMDIR) -eval 'application:ensure_all_started(erlking).' -noshell

name:
	@echo $(NAME)
.PHONY: clean

clean:
	@$(CLEANCMD)
.PHONY: clean

rebuild: clean all
.PHONY: rebuild

ebin/%.beam: src/%.erl
	@$(COMPILECMD)

ebin/%.app: src/%.app.src
	@$(COMPILECMD)