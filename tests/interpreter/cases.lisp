;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Test programs for the interactive implementation alone, in the form of
;;;; tests/codegen/cases.lisp: programs the manual allows but the compiler
;;;; to C rejects for what it does not support yet, so that they cannot
;;;; be among its cases.  tests/interpreter/run.lisp runs them with those.

(
 ;; Chapter 7: a predefined function's variable may be assigned, (setter
 ;; element) among them, and calls see the new value -- the inline fast
 ;; paths and the iteration over vectors and lists as well.  (The
 ;; compiler to C cannot yet assign a variable of the dylan module.)
 (assign-predefined
  "(define old+ +) (define old< <) (define old-element element)
   (define old-modulo modulo) (define old-binary< binary<)
   (define old-initial-state initial-state)
   (define v (vector 1 2 3))
   (define calls '())
   (define old-element-setter (setter element))
   (set! (setter element)
         (method (coll key val) (set! calls (cons key calls)) (old-element-setter coll key val)))
   (set! (element v 0) 'x)
   (print (list v calls))
   (set! + -)
   (print (+ 5 3))
   (set! < (method (a b) 'less))
   (print (< 1 2))
   (set! element (method (c k #key default) (list 'element k)))
   (print (element v 1))
   (set! modulo (method (a b) 'mod))
   (print (modulo 7 2))
   (set! binary< (method (a b) #t))
   (print (> 1 2))
   (set! initial-state (method (c) #f))
   (print (map (method (x) x) '(1 2 3)))
   (for-each ((x v)) () (print x))
   ;; Put them back, for the programs after this one
   (set! + old+) (set! < old<) (set! element old-element)
   (set! (setter element) old-element-setter) (set! modulo old-modulo)
   (set! binary< old-binary<) (set! initial-state old-initial-state)
   (print (list (+ 5 3) (< 2 1) (element v 1) (modulo 7 2) (> 1 2) (map (method (x) x) '(1 2))))"
  "(#(x 2 3) (0))
2
less
(element 1)
mod
#t
()
(8 #f 2 1 #f (1 2))
")
)
