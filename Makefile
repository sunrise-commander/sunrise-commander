EMACS ?= emacs
ELS = sunrise-commander.el sunrise-x-buttons.el sunrise-x-checkpoints.el sunrise-x-loop.el sunrise-x-mirror.el sunrise-x-modeline.el sunrise-x-old-checkpoints.el sunrise-x-popviewer.el sunrise-x-tabs.el sunrise-x-tree.el sunrise-x-w32-addons.el

AUTOLOADS = sunrise-commander-autoloads.el
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
	   (package-generate-autoloads \"sunrise-commander\" default-directory))"

package-lint: $(ELS)
	$(EMACS) -Q -batch -L . -f package-initialize \
            -f package-lint-batch-and-exit $(ELS) 2>&1

clean:
	rm -f $(ELCS) $(AUTOLOADS)

.PHONY: all autoloads clean package-lint test
