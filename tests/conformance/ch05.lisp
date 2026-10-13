;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conformance tests for chapter 5 of the 1992 manual, Modules.  The
;;;; chapter has one example and no results; every expected value here is
;;;; what its text specifies.  The manual leaves module declarations to
;;;; the programming environment; as README.md says, they are written here
;;;; as design note 32 describes, with a design note 33 module: header.

(
 ;; 5 References to Module Variables: the chapter's example.  + and
 ;; my-number are looked up in the module; x is lexical.
 (ch05-references
  "(define my-number 5)
   (print (bind ((x 10))
            (+ x my-number)))
   (bind ((my-number 1))
     (print (bind ((x 10))
              (+ x my-number))))"
  "15
11
")

 ;; 5 Using Modules: a module imports the exported variables of the
 ;; modules it uses, all of them by default; the import can be
 ;; restricted by an exclude list (all others imported) or an import
 ;; list (all others excluded); a variable can be renamed, per symbol,
 ;; which affects (setter foo) as well as foo; a prefix can be prepended
 ;; to all the names.  A variable imported twice is the same variable.
 ;; Imported variables appear just like variables created in the module.
 ;; A variable excluded from the import leaves its name free for a
 ;; variable of the module's own.
 (ch05-using-modules
  "module: app

(define-module app
  (use dylan exclude: (reverse))
  (use dylan import: (reverse element) rename: ((element => elt)) prefix: \"d-\")
  (use dylan import: ((first => front))))

(define reverse 'my-own-reverse)
(print reverse)
(print (d-reverse '(1 2 3)))
(print (front '(a b c)))
(print (id? front first))
(define v (vector 'a 'b 'c))
(print (elt v 1))
(set! (elt v 1) 'sea)
(print v)
((setter elt) v 0 'zero)
(print v)
(print (id? (setter elt) (setter element)))
(print (id? elt element))"
  "my-own-reverse
(3 2 1)
a
#t
b
#(a sea c)
#(zero sea c)
#t
#t
")

 ;; 5 Overview of Modules: \"If a conflict arises when a module tries to
 ;; ... use a new module ... the conflict is resolved or the operation is
 ;; canceled.\"  Here two different variables would have the name second.
 (ch05-import-conflict
  "module: app

(define-module app
  (use dylan)
  (use dylan import: ((first => second))))

(print (second '(1 2 3)))"
  ""
  :compile-error "second")

 ;; 5 Overview of Modules: a module gets variables from two sources --
 ;; created in it, or imported by using another module.  Here module
 ;; shapes creates (and so exports) its variables, which the module that
 ;; uses it defines (design note 32's create clause), here under a prefix.
 (ch05-module-sources
  "module: app

(define-module shapes
  (create area shape-name))

(define-module app
  (use dylan)
  (use shapes prefix: \"shape-\"))

(define-method shape-area ((side <integer>)) (* side side))
(define shape-shape-name 'square)
(define local-variable 7)
(print (shape-area 3))
(print shape-shape-name)
(print local-variable)"
  "9
square
7
")

 ;; 5 Adding Variables to a Module, Readonly Variables: define-class and
 ;; define-generic-function create read-only variables, which cannot be
 ;; assigned a new value.
 (ch05-read-only-class
  "(define-class <shape> (<object>))
   (set! <shape> 3)
   (print 'not-reached)"
  ""
  :compile-error "<shape>")

 (ch05-read-only-generic
  "(define-generic-function perimeter (shape))
   (set! perimeter 3)
   (print 'not-reached)"
  ""
  :compile-error "perimeter")

 ;; 5 Readonly Variables: \"The object contained in the variable can still
 ;; be modified\"; and a variable made by define can be assigned.
 (ch05-read-only-contents
  "(define-class <shape> (<object>) (sides init-value: 0))
   (define-generic-function perimeter (shape))
   (define-method perimeter ((s <shape>)) (* 2 (sides s)))
   (print (perimeter (make <shape> )))
   (define-method perimeter ((n <integer>)) (* 4 n))
   (print (perimeter 3))
   (define tally 1)
   (set! tally 2)
   (print tally)"
  "0
12
2
")
)
