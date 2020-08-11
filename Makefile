EMACS=emacs

.PHONY: all test test-batch

all:
	${MAKE} compile
	${MAKE} test-batch
	${MAKE} clean

test:
	${EMACS} -Q -nw -L . -l test/moose-test-init.el \
	--eval "(let (pop-up-windows) (ert t))"

test-batch:
	${EMACS} -Q --batch -L . -l test/moose-test-init.el \
	--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

compile:
	${EMACS} -Q --batch -L . -f batch-byte-compile moose-mode.el moose-*.el

clean:
	rm -f *.elc
