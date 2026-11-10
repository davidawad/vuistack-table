# vuistack-table -- make deps && make compile && make test
EMACS ?= emacs
DEPS ?= .deps
VUI_URL ?= https://github.com/d12frosted/vui.el
# VUI is the directory holding vui.el.
VUI ?= $(DEPS)/vui.el
LOAD_PATH = -L . -L $(VUI)

.PHONY: all deps compile test clean

all: compile test

deps: $(VUI)/vui.el

$(VUI)/vui.el:
	git clone --depth 1 $(VUI_URL) $(VUI)

compile: deps
	$(EMACS) -Q --batch $(LOAD_PATH) \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile vuistack-table*.el

test: deps
	$(EMACS) -Q --batch $(LOAD_PATH) $(addprefix -l ,$(wildcard test/vuistack-table-*test.el)) \
	  -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
