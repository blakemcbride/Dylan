;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Run the stress tests (CompilerPlan.md, Phase 11):
;;;;
;;;;     sbcl --script tests/stress/run.lisp [mode] [file.lisp ...] [test ...]
;;;;
;;;; cases-*.lisp here, in the form and modes of tests/codegen/runner.lisp:
;;;; 10^8 tail calls through unknown functions, generic functions and
;;;; closures in a 256 KB stack, and many threads at once.  They take
;;;; minutes.

(load (merge-pathnames "../codegen/runner.lisp" (directory-namestring *load-truename*)))

(in-package #:dylan-compiler)

(run-test-files (directory (merge-pathnames "cases*.lisp" (directory-namestring *load-truename*)))
                (rest sb-ext:*posix-argv*))
