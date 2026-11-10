;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Test programs for the interactive implementation alone, in the form of
;;;; tests/codegen/cases.lisp: programs the manual allows but the compiler
;;;; to C rejects for what it does not support yet, so that they cannot
;;;; be among its cases.  tests/interpreter/run.lisp runs them with those.

(
 ;; A singleton of an <extended-float> is an extended float of its value,
 ;; before and after the dispatch cache holds the call (chapter 7).  (The
 ;; compiler to C takes only a constant or a class in a singleton
 ;; specializer, and an <extended-float> has no literal.)
 (extended-float-singleton-dispatch
  "(define e (as <extended-float> 1.5))
(define-method sx ((x (singleton e))) 'single)
(define-method sx ((x <object>)) 'other)
(define-method sy ((x (singleton e))) 'single)
(define-method sy ((x <object>)) 'other)
(define-method sz ((x (singleton 1.5)) (y (singleton e))) 'both)
(define-method sz (x y) 'other)
(print (list (sx (as <extended-float> 1.5)) (sx (as <extended-float> 2.5)) (sx 1.5)))
(print (list (sy (as <extended-float> 2.5)) (sy (as <extended-float> 1.5)) (sy 1.5)))
(print (list (sz 1.5 (as <extended-float> 1.5)) (sz (as <extended-float> 1.5) 1.5) (sz 1.5 1.5)))"
  "(single other other)
(other single other)
(both other other)
")
 ;; A module redefined may not come to use itself (chapter 5).  (The
 ;; compiler to C does not redefine modules.)
 (module-use-cycle
  "(define-module m1 (use dylan))
(define-module m2 (use dylan) (use m1))
(define-module m1 (use dylan) (use m2))
(print 'after)"
  ""
  :error "modules use each other")
)
