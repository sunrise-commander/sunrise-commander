EMACS ?= emacs
ELS = sunrise.el sunrise-buttons.el sunrise-checkpoint.el sunrise-loop.el sunrise-mirror.el sunrise-modeline.el sunrise-popviewer.el sunrise-tabs.el sunrise-tree.el sunrise-w32.el

AUTOLOADS = sunrise-autoloads.el
ELCS = $(ELS:.el=.elc)


.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -Q -batch -L . -f package-initialize -f batch-byte-compile $<

all: clean autoloads $(ELCS)

autoloads: $(AUTOLOADS)

$(AUTOLOADS):
	$(EMACS) -Q -batch -L . --eval \
	"(progn \
	   (require 'package) \
	   (normal-top-level-add-subdirs-to-load-path) \
	   (package-generate-autoloads \"sunrise\" default-directory))"

package-lint: $(ELS)
	$(EMACS) -Q -batch -L . -f package-initialize \
	    -f package-lint-batch-and-exit $(ELS) 2>&1

clean:
	rm -f $(ELCS) $(AUTOLOADS)

.PHONY: all autoloads package-lint clean
