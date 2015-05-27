NAME = erlking

REBAR = ./rebar
DEPSLVCMD  = $(REBAR) get-deps
COMPILECMD = $(REBAR) compile
CLEANCMD   = $(REBAR) clean
DOCCMD     = $(REBAR) doc
ESCRPTCMD  = $(REBAR) escriptize
TESTCMD    = $(REBAR) eunit
REPLCMD    = $(REBAR) shell

DEPSDIR = deps
DEPS    = $(wildcard $(DEPSDIR)/*)
DEPSBIN = $(DEPS:%=%/ebin)

DIALYZER = dialyzer

ERLANG  = erl
BEAMDIR = ebin

SYSTHREADS  = 10
NODENAME    = $(NAME)

SRCDIR      = src
ERLFILES    = $(wildcard $(SRCDIR)/*.erl)
BEAMFILES   = $(ERLFILES:$(SRCDIR)/%.erl=$(BEAMDIR)/%.beam)
APPSRCFILES = $(wildcard $(SRCDIR)/*.app.src)
APPFILES    = $(APPSRCFILES:$(SRCDIR)/%.src=$(BEAMDIR)/%)

OBJFILES    = $(APPFILES) $(BEAMFILES)
BIN         = $(NAME)

CLASSPATHS  = $(BEAMDIR) $(DEPSBIN)

APPSTART    = application:ensure_all_started(erlking).

ERLRUNOPTS  = -sname $(NODENAME) -config $(NAME) +A $(SYSTHREADS) $(CLASSPATHS:%=-pa %) -noshell

OTPVERSION  = $(shell erl -noshell -eval 'io:format(erlang:system_info(otp_release)), halt().')
OTPPLTFILE  = .global_plt.$(OTPVERSION)
PLTFILE     = erlking.plt

#all: $(BIN) doc
all: $(REBAR) doc
	@$(DEPSLVCMD)
	@$(COMPILECMD)
.PHONY: all

run:
	@$(ERLANG) $(ERLRUNOPTS) -eval '$(APPSTART)'

repl: $(OBJFILES)
	@$(REPLCMD)
.PHONY: repl

test: all
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
	@rm -rf doc log

.PHONY: clean

rebuild: clean all
.PHONY: rebuild

$(BIN): $(OBJFILES)
	@$(ESCRPTCMD)

# ebin/%.beam: $(SRCDIR)/%.erl rebar.config
# 	@$(DEPSLVCMD)
# 	@$(COMPILECMD)

# ebin/%.app: $(SRCDIR)/%.app.src rebar.config
# 	@$(DEPSLVCMD)
# 	@$(COMPILECMD)

$(PLTFILE): all
	@$(DIALYZER) --output_plt $@ --build_plt -r ebin $(DEPSBIN:%=-r %)

$(OTPPLTFILE):
	@$(DIALYZER) --output_plt $@ --build_plt --apps edoc erts eunit kernel mnesia stdlib tools webtool xmerl

TAGS:	$(ERLFILES)
	etags $(ERLFILES)

$(REBAR): rebar-src
	cd rebar-src && ./bootstrap
	cp rebar-src/rebar $(REBAR)

rebar-src:
	git clone https://github.com/rebar/rebar.git $@
