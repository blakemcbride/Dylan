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
  :compile-error "m is a macro"))
