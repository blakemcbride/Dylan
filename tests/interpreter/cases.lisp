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
  :error "define-module: modules m1 use each other")
 ;; define-slot on a sealed class, a built-in one sealed from the start
 ;; included, is an error when it runs: a sealed class is read-only.  (The
 ;; compiler to C takes a class owner only of the program's own classes.)
 (define-slot-on-sealed-class
  "(define-method try (f)
  (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
(print (try (method () (define-slot <integer> tagx init-value: 7) 'defined)))
(print (try (method () (define-slot <pair> tagp init-value: 7) 'defined)))
(define-class <mine> (<object>))
(seal <mine>)
(print (try (method () (define-slot <mine> tagm init-value: 7) 'defined)))"
  "\"ERR: define-slot: the class is read-only: {the class <integer>}\"
\"ERR: define-slot: the class is read-only: {the class <pair>}\"
\"ERR: define-slot: the class is read-only: {the class <mine>}\"
")
 ;; (setter direct-superclasses) remakes a class (chapter 11): an instance
 ;; keeps the slots that survive, takes the new ones' initial values, and
 ;; dispatch follows the new superclasses.  (A static program cannot
 ;; change its classes: the compiler to C refuses it.)
 (set-direct-superclasses
  "(define-class <a> (<object>) (x init-value: 1))
(define-class <c> (<object>) (z init-value: 3))
(define-class <b> (<a>) (y init-value: 2))
(define-method describe ((o <a>)) \"an <a>\")
(define-method describe ((o <object>)) \"something\")
(define b (make <b>))
(print (list (x b) (describe b) (instance? b <a>)))
(set! (direct-superclasses <b>) (list <c>))
(print (direct-superclasses <b>))
(print (list (z b) (y b) (describe b) (instance? b <a>) (instance? b <c>)))"
  "(1 \"an <a>\" #t)
({the class <c>})
(3 2 \"something\" #f #t)
")
)
