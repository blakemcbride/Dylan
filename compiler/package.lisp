;;;; Copyright 2026 Blake McBride.  See ../LICENSE.
;;;;
;;;; The package of the Dylan-to-C compiler.  CompilerPlan.md describes the
;;;; design.
;;;;
;;;; Dylan source is read by the reader of the Common Lisp implementation
;;;; (src/compat.lisp), which interns Dylan identifiers in the DYLAN package
;;;; -- or finds them in COMMON-LISP, which DYLAN uses -- and reads foo: as
;;;; the keyword :FOO.  The compiler therefore spells the Dylan names it
;;;; recognizes DYLAN::BEGIN, DYLAN::DEFINE-METHOD and so on.

(defpackage #:dylan-compiler
  (:nicknames #:dylanc)
  (:use #:common-lisp)
  (:export #:front-end                  ; files -> program of core forms
           #:front-end-forms            ; forms -> program (for tests)
           #:program-units
           #:unit-file
           #:unit-module
           #:unit-forms
           #:program-modules
           #:program-errors
           #:program-warnings
           #:unparse                    ; core form -> readable s-expression
           #:compile-error
           #:to-ir                      ; program -> IR (Phase 2)
           #:ir-program-toplevels
           #:print-ir-program))

(in-package #:dylan-compiler)

(declaim (optimize (speed 1) (safety 3) (debug 2)))
