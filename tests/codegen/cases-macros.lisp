;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Test programs for a program's own macros (CompilerPlan.md, milestone
;;;; M11: an extension), in the form of cases.lisp: define-macro,
;;;; define-for-macros and gensym (compiler/front/macros.lisp).

(
 ;; A macro's body builds its expansion, usually with backquote; #rest
 ;; collects the rest of the call.
 (macro-basics
  "(define-macro my-unless (test #rest body)
     `(if ,test #f (begin ,@body)))
   (define-macro twice (form) `(begin ,form ,form))
   (my-unless (= 1 2) (print 'one) (print 'two))
   (print (my-unless #t 'never))
   (define n 0)
   (twice (set! n (+ n 1)))
   (print n)"
  "one
two
#f
2
")

 ;; gensym makes names the program cannot write, so an expansion's own
 ;; variables capture nothing; the expansion's other names mean what they
 ;; mean where the macro is used.
 (macro-gensym
  "(define-macro swap! (a b)
     (bind ((tmp (gensym \"tmp\")))
       `(bind ((,tmp ,a)) (set! ,a ,b) (set! ,b ,tmp))))
   (define tmp 1)
   (define other 2)
   (swap! tmp other)
   (print (list tmp other))
   (bind ((x 'a) (y 'b))
     (swap! x y)
     (print (list x y)))"
  "(2 1)
(b a)
")

 ;; Macros may expand into uses of macros, themselves included, and into
 ;; definitions at top level.
 (macro-recursion-and-definitions
  "(define-macro my-or (#rest forms)
     (if (empty? forms)
         #f
         (bind ((v (gensym)))
           `(bind ((,v ,(first forms))) (if ,v ,v (my-or ,@(cdr forms)))))))
   (print (list (my-or) (my-or #f) (my-or #f 2 3)))
   (define-macro define-constant-getter (name value)
     `(define ,name (method () ,value)))
   (define-constant-getter answer 42)
   (define-macro define-both (a b)
     `(begin (define ,a 1) (define ,b 2)))
   (define-both first-one second-one)
   (print (list (answer) first-one second-one))"
  "(#f #f 2)
(42 1 2)
")

 ;; define-for-macros defines what macros' bodies call, while compiling
 ;; only; a macro's parameters are a method's.
 (macro-helpers
  "(define-for-macros
     (define zero-bindings (method (names) (map (method (n) (list n 0)) names)))
     (define *expansions* 0))
   (define-macro with-zeros (names #rest body)
     (set! *expansions* (+ *expansions* 1))
     `(bind ,(zero-bindings names) ,@body))
   (define-macro expansions () *expansions*)
   (print (with-zeros (a b) (list a b)))
   (print (with-zeros (c) c))
   (print (expansions))
   (define-macro keyed (#key (scale 1) value) (* scale value))
   (print (list (keyed value: 5) (keyed scale: 3 value: 5)))"
  "(0 0)
0
2
(5 15)
")

 ;; A local variable of a macro's name hides it.
 (macro-shadowed
  "(define-macro double (x) `(* 2 ,x))
   (print (double 4))
   (define apply-it (method (double) (double 4)))
   (print (apply-it (method (n) (+ n 100))))"
  "8
104
")

 (macro-error-in-body
  "(define-macro m (a) (error \"no good: ~s\" a))
   (m 1)"
  ""
  :compile-error "macro m: no good: 1")

 (macro-wrong-arguments
  "(define-macro m (a) a)
   (m 1 2)"
  ""
  :compile-error "macro m: wrong number of arguments")

 (macro-as-variable
  "(define-macro m (a) a)
   (print m)"
  ""
  :compile-error "m is a macro")

 (macro-not-top-level
  "(define f (method () (define-macro m () 1)))"
  ""
  :compile-error "define-macro must be at top level")

 (macro-never-stops
  "(define-macro forever () '(forever))
   (forever)"
  ""
  :compile-error "one never stops")

 (macro-and-variable
  "(define-macro m () 1)
   (define m 2)"
  ""
  :compile-error "m is a macro")

 ;; ,@ splices the elements of any collection, not only a list's
 (splice-collections
  "(print `(1 ,@(vector 2 3 4) 5))
   (print `(1 ,@(list 2 3) 5))
   (print `(1 ,@(range from: 2 up-to: 5) 5))
   (print `(1 ,@\"ab\" 5))
   (print `(,@(list 1 2) . 3))
   (print `#(1 ,@(vector 2 3)))
   (define l (list 9 8))
   (print (id? (cdr `(0 ,@l)) l))
   (print (handler-case `(1 ,@5) ((<error>) 'error)))"
  "(1 2 3 4 5)
(1 2 3 5)
(1 2 3 4 5)
(1 #\\a #\\b 5)
(1 2 . 3)
#(1 2 3)
#f
error
")

 ;; An error of the argument count in a macro's body is the body's, not
 ;; the call's.
 (macro-body-wrong-arguments
  "(define-macro m (a) (car))
   (m 1)"
  ""
  :compile-error "macro m: wrong number of arguments (0)")

 (macro-wrong-arguments-form
  "(define-macro m (a) a)
   (m 1 2)"
  ""
  :compile-error "macro m: wrong number of arguments in (m 1 2)")
 ;; Each list, string and vector a macro makes is an object of its own,
 ;; however like another it is; one expansion's is one object.
 (macro-made-literals
  "(define-macro mv () (vector 1 2))
(define v1 (mv))
(define v2 (mv))
(set! (element v1 0) 99)
(print (list v1 v2))
(define-macro ms () (concatenate \"a\" \"b\"))
(define-macro two () (bind ((s (concatenate \"x\" \"y\"))) (list 'list s s)))
(define-method f () (mv))
(print (list (id? (f) (f)) (id? (ms) (ms)) (= (ms) (ms))))
(bind ((p (two))) (print (id? (first p) (second p))))"
  "(#(99 2) #(1 2))
(#t #f #t)
#t
")
)
