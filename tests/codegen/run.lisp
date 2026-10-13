;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Run the code generator's tests:
;;;;
;;;;     sbcl --script tests/codegen/run.lisp [mode] [file.lisp ...] [test ...]
;;;;
;;;; the programs of cases*.lisp here, or of the files and tests named, in
;;;; the configurations of the mode (runner.lisp; the default runs gcc and
;;;; clang, with musttail and with trampolines).

(load (merge-pathnames "runner.lisp" (directory-namestring *load-truename*)))

(in-package #:dylan-compiler)

(run-test-files (directory (merge-pathnames "cases*.lisp" (directory-namestring *load-truename*)))
                (rest sb-ext:*posix-argv*))
