;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conformance tests for chapter 7 of the 1992 manual, Assignment.  Most
;;;; are the chapter's own examples with the results it shows; the rest
;;;; are marked as coming from its text.

(
 ;; 7 set!: the chapter's first example, a module variable.  (The
 ;; manual's transcript shows 10 as the value of (define foo 10); the
 ;; value of define is not tested.)  set! returns the new value.
 (ch07-set-variable
  "(define foo 10)
   (print foo)
   (print (set! foo (+ 10 10)))
   (print foo)"
  "10
20
20
")

 ;; 7 set!: \"new-value is stored in the corresponding variable (which
 ;; may be a lexical or module variable)\".  (Specification.)
 (ch07-set-lexical-variable
  "(define foo 1)
   (print (bind ((foo 10))
            (set! foo (+ foo 5))
            foo))
   (print foo)
   (define-method counter ()
     (bind ((n 0))
       (method () (set! n (+ n 1)))))
   (define c (counter))
   (c)
   (c)
   (print (c))
   (print ((method (x) (set! x (* x 2)) x) 21))"
  "15
1
3
42
")

 ;; 7 set!: the chapter's example of a setter variable used as a place.
 ;; The manual assigns the nonexistent primitive %set-element to (setter
 ;; element); since the dylan module's (setter element) is a read-only
 ;; generic function (chapter 5), a setter variable of the program's own
 ;; is assigned here instead.
 (ch07-set-setter-variable
  "(define (setter my-element) (method (c i v) 'old))
   (define %set-element (method (c i v) 'new))
   (print ((setter my-element) 1 2 3))
   (set! (setter my-element) %set-element)
   (print (id? (setter my-element) %set-element))
   (print ((setter my-element) 1 2 3))"
  "old
#t
new
")

 ;; 7 set!: the chapter's example of the extended form.
 (ch07-extended-form
  "(define foo (vector 'a 'b 'c 'd))
   (print foo)
   (print (element foo 2))
   (print (set! (element foo 2) 'sea))
   (print (element foo 2))
   (print foo)
   ((setter element) foo 2 'sea2)
   (print foo)"
  "#(a b c d)
c
sea
sea
#(a b sea d)
#(a b sea2 d)
")

 ;; 7 set!: (set! (symbol arg1...argn) new-value) means roughly
 ;; ((setter symbol) arg1...argn new-value), \"because set! guarantees
 ;; that the new-value is returned, regardless of what is returned by the
 ;; setter function.\"  (Specification.)
 (ch07-extended-form-value
  "(define-class <box> (<object>) (contents init-value: #f))
   (define-method (setter label) ((b <box>) (name <symbol>) value)
     (set! (contents b) (list name value))
     'setter-result)
   (define b (make <box>))
   (print ((setter label) b 'x 1))
   (print (contents b))
   (print (set! (label b 'y) 2))
   (print (contents b))
   (print (set! (contents b) 'z))"
  "setter-result
(x 1)
2
(y 2)
z
")

 ;; 7 set!: \"If place does not have one of these formats, an error is
 ;; signaled at compile-time.\"
 (ch07-bad-place
  "(set! 3 4)"
  ""
  :compile-error "set!")

 (ch07-bad-place-list
  "(define foo 1)
   (set! (1 foo) 4)"
  ""
  :compile-error "set!")

 ;; 7 set!: \"An error is signaled if there is no lexical or module
 ;; variable corresponding to the place at the time set! is executed.\"
 (ch07-set-unbound-variable
  "(print 'start)
   (set! no-such-variable 4)
   (print 'not-reached)"
  "start
"
  :error "no-such-variable")
)
