;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Run the manual conformance tests (CompilerPlan.md, Phase 11):
;;;;
;;;;     sbcl --script tests/conformance/run.lisp [mode] [file.lisp ...] [test ...]
;;;;
;;;; Each ch*.lisp file here holds the examples of one chapter of the 1992
;;;; manual, in the form of tests/codegen/runner.lisp, with the results the
;;;; manual gives for them -- or, where it gives none, the results its text
;;;; specifies, citing the section.  The mode is as there (default all;
;;;; quick for gcc alone).

(load (merge-pathnames "../codegen/runner.lisp" (directory-namestring *load-truename*)))

(in-package #:dylan-compiler)

(run-test-files (directory (merge-pathnames "ch*.lisp" (directory-namestring *load-truename*)))
                (rest sb-ext:*posix-argv*))
