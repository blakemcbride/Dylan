# Copyright 2026 Blake McBride.  See LICENSE.
#
# make            build bin/dylan, the Common Lisp implementation as one
#                 executable that starts in the read-eval-print loop
# make clean      remove it and the compiled Lisp files
# make distclean  remove everything built: also the compiler's fasls, the C
#                 runtime, the benchmarks' builds and the manual's LaTeX
#                 files (not its PDF, which is kept in the repository)
#                 (realclean is the same)

bin/dylan: src/*.lisp
	sbcl --script src/build-dylan.lisp

clean:
	rm -rf bin/dylan bin/dylan.exe src/fasl

distclean realclean: clean
	rm -rf compiler/fasl runtime/build bench/build
	$(MAKE) -C manual/UserManual clean

.PHONY: clean distclean realclean
