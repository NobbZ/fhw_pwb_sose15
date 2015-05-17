NAME = erlking

REBAR = ./rebar
COMPILECMD = $(REBAR) compile
CLEANCMD   = $(REBAR) clean
DOCCMD     = $(REBAR) doc
ESCRPTCMD  = $(REBAR) escriptize
TESTCMD    = $(REBAR) eunit
REPLCMD    = $(REBAR) shell

DIALYZER = dialyzer

ERLANG  = erl
BEAMDIR = ebin

SRCDIR      = src
ERLFILES    = $(wildcard $(SRCDIR)/*.erl)
BEAMFILES   = $(ERLFILES:$(SRCDIR)/%.erl=$(BEAMDIR)/%.beam)
APPSRCFILES = $(wildcard $(SRCDIR)/*.app.src)
APPFILES    = $(APPSRCFILES:$(SRCDIR)/%.src=$(BEAMDIR)/%)

OBJFILES    = $(APPFILES) $(BEAMFILES)
BIN         = $(NAME)

OTPVERSION  = $(shell erl -noshell -eval 'io:format(erlang:system_info(otp_release)), halt().')
OTPPLTFILE  = .global_plt.$(OTPVERSION)
PLTFILE     = erlking.plt


all: $(BIN) doc

run: $(BIN)
	@./$(BIN)

repl: $(OBJFILES)
	@$(REPLCMD)
.PHONY: repl

test: $(OBJFILES)
	@$(TESTCMD)
.PHONY: test

typecheck: $(PLTFILE) $(OTPPLTFILE)
	@$(DIALYZER) --plts $(OTPPLTFILE) $(PLTFILE) -Wrace_conditions --src $(SRCDIR)

doc:
	@$(DOCCMD)
.PHONY: doc

name:
	@echo $(NAME)
.PHONY: clean

clean:
	@$(CLEANCMD)
	rm -rf doc
.PHONY: clean

rebuild: clean all
.PHONY: rebuild

$(BIN): $(OBJFILES)
	@$(ESCRPTCMD)

ebin/%.beam: $(SRCDIR)/%.erl
	@$(COMPILECMD)

ebin/%.app: $(SRCDIR)/%.app.src
	@$(COMPILECMD)

$(PLTFILE): $(OBJFILES)
	@$(DIALYZER) --output_plt $@ --build_plt -r ebin

$(OTPPLTFILE):
	@$(DIALYZER) --output_plt $@ --build_plt --apps edoc erts eunit kernel mnesia stdlib tools webtool xmerl