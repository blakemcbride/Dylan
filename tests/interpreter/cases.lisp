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
 ;; A sealed class is read-only (chapter 11), the built-in classes sealed
 ;; from the start included: its superclasses and slots cannot be changed.
 (sealed-class-not-remade
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
(define-class <a> (<object>))
(print (try (method () (set! (direct-superclasses <integer>) (list <a>)))))
(print (instance? 5 <number>))
(print (try (method () (set! (direct-superclasses <pair>) (list <object>)))))
(define-class <e> (<object>) (s init-value: 1))
(define sd (aref (slot-descriptors <e>) 0))
(seal <e>)
(print (try (method () (remove-slot <e> sd))))
(print (try (method () (set! (direct-superclasses <e>) (list <a>)))))
(print (s (make <e>)))"
  "\"ERR: (setter direct-superclasses) -- class is read-only {the class <integer>}\"
#t
\"ERR: (setter direct-superclasses) -- class is read-only {the class <pair>}\"
\"ERR: remove-slot -- class is read-only {the class <e>}\"
\"ERR: (setter direct-superclasses) -- class is read-only {the class <e>}\"
1
")
 ;; add-slot's debug-name: names the descriptor it makes (chapter 11),
 ;; which otherwise prints with its getter's name, or #f.  (The compiler
 ;; to C has no add-slot.)
 (add-slot-debug-name
  "(define h (make <generic-function> required: 1 debug-name: 'hh))
(define-class <q4> (<object>))
(print (add-slot <q4> getter: h debug-name: 'dbg))
(print (add-slot <q4> getter: (make <generic-function> required: 1)))
(define-class <q8> (<q4>))
(print (slot-descriptors <q8>))"
  "{slot dbg}
{slot #f}
#({slot dbg} {slot #f})
")
 ;; An integer too large for the heap is the error "out of memory" at
 ;; once, which a handler catches (Bugs05 L4).  (The compiler to C has no
 ;; integers past 63 bits.)
 (integer-too-large-for-the-heap
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"~a\" c))))
(print (try (method () (ash 1 100000000000))))
(print (try (method () (expt 3 100000000000))))
(print (try (method () (ash 1 4611686018427387903))))
(print (try (method () (expt 2/3 -100000000000))))
(print (list (ash 1 -100000000000) (expt 1 100000000000) (expt -1 100000000001) (ash 1 100)))"
  "\"out of memory\"
\"out of memory\"
\"out of memory\"
\"out of memory\"
(0 1 -1 1267650600228229401496703205376)
")
 ;; The library's classes keep their superclasses and the slots the
 ;; library gave them, on which its own errors depend; a program's class,
 ;; and a slot a program added, may still change (Bugs05 L6).  (The
 ;; compiler to C rejects these operations when compiling.)
 (library-classes-not-remade
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
(define-class <a> (<object>))
(print (try (method () (set! (direct-superclasses <simple-error>) (list <a>)))))
(print (try (method () (remove-slot <simple-error> (aref (slot-descriptors <simple-error>) 0)))))
(print (try (method () (error \"still ~a\" 'catchable))))
(define-class <my-error> (<simple-error>))
(print (try (method () (set! (direct-superclasses <my-error>) (list <error>)))))
(print (instance? (make <my-error>) <simple-error>))
(define-generic-function extra (x))
(define sd (add-slot <simple-error> getter: extra init-value: 5))
(print (extra (make <simple-error>)))
(remove-slot <simple-error> sd)
(print (try (method () (extra (make <simple-error>)))))
(print (try (method () (error \"and ~a\" 'again))))"
  "\"ERR: (setter direct-superclasses) -- cannot remake a class of the library {the class <simple-error>}\"
\"ERR: remove-slot -- cannot remove a slot of the library {the class <simple-error>} {slot condition-format-string}\"
\"ERR: still catchable\"
({the class <error>})
#f
5
\"ERR: no applicable method for extra\"
\"ERR: and again\"
")
)
