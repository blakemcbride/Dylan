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

 ;; define-module and define-for-macros are defining forms too (chapters
 ;; 12 and 16).
 (module-not-top-level
  "(define f (method (x) (define-module zz (use dylan)) x))"
  ""
  :compile-error "define-module must be at top level")

 (define-for-macros-not-top-level
  "(define f (method (x) (define-for-macros (define y 1)) x))"
  ""
  :compile-error "define-for-macros must be at top level")

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
 ;; A define-module a macro makes defines the module where the macro is used,
 ;; and a module that uses it can be defined after.
 (macro-made-module
  "(define-macro defmod (name) `(define-module ,name (use dylan) (export foo)))
(defmod mm)
(define-module other (use dylan) (use mm))
(print 'done)"
  "done
")
 ;; Chapter 15: a symbol is spelled as it is first entered. One a macro
 ;; makes while a form is compiled is entered before a later form spells it.
 (macro-made-symbol-spelling
  "(define-macro m () (list 'quote (as <symbol> \"MiXeD\")))
(print (m))
(print 'mixed)
(print (as <symbol> \"MIXED\"))
(define-macro k () (as <keyword> \"KeY\"))
(print (k))
(print key:)
(define-macro n2 () (list 'quote (as <symbol> \"FreSh\")))
(print (n2))"
  "MiXeD
MiXeD
MiXeD
KeY:
KeY:
FreSh
")
 ;; Macro bodies run in a compile-time environment apart from the program,
 ;; in both implementations: the program's own functions are not there ...
 (macro-bodies-apart-from-program
  "(define helper (method (x) (list x x)))
(define-macro use-helper (x) (helper x))
(print (use-helper 3))"
  ""
  :compile-error "macro use-helper: the variable helper@compile-time has no value")
 ;; ... and the program cannot see what define-for-macros defines.
 (define-for-macros-apart-from-program
  "(define-for-macros
  (define counter 0)
  (define twice (method (x) (* 2 x))))
(define-macro next-count () (set! counter (+ counter 1)) (twice counter))
(print (list (next-count) (next-count)))
(define counter 100)
(print counter)
(print (next-count))
(print (twice 5))"
  "(2 4)
100
6
"
  :error "the variable twice@dylan-user has no value")
 ;; A second define-macro replaces the macro for the forms after it (chapter 16).
 (macro-defined-twice
  "(define-macro m () 1)
(print (m))
(define f (method () (m)))
(define-macro m () 2)
(print (m))
(print (f))
(define-macro m (x) `(list ,x ,x))
(print (m 3))"
  "1
2
1
(3 3)
")
 ;; A bad define-module is reported in the same words by both
 ;; implementations.
 (define-module-uses-itself
  "(define-module m1 (use dylan) (use m1))"
  ""
  :compile-error "define-module: modules m1 use each other")
 (define-modules-use-each-other
  "(define-module a (use dylan) (use b))
(define-module b (use dylan) (use a))"
  ""
  :compile-error "define-module: modules a use each other")
 (define-module-exports-imported
  "(define-module m4 (use dylan) (export car))"
  ""
  :compile-error "define-module: cannot export car, which is imported")
 (define-module-creates-imported
  "(define-module m4 (use dylan) (create car))"
  ""
  :compile-error "define-module: cannot create car, which is imported")
 (define-module-dylan
  "(define-module dylan (use dylan))"
  ""
  :compile-error "define-module: the dylan module cannot be redefined")
 (define-module-bad-name
  "(define-module 5)"
  ""
  :compile-error "define-module: bad module name 5")
 (define-module-unknown-clause
  "(define-module m5 (frob))"
  ""
  :compile-error "define-module: unknown clause (frob)")
 (define-module-unknown-module
  "(define-module m5 (use nosuch))"
  ""
  :compile-error "unknown module nosuch")
 (define-module-import-not-exported
  "(define-module m5 (use dylan import: (nosuch)))"
  ""
  :compile-error "define-module: nosuch is not exported from dylan")
 (define-module-prefix-not-a-string
  "(define-module m5 (use dylan prefix: 5))"
  ""
  :compile-error "define-module: prefix: must be a string, not 5")
 (define-module-option-twice
  "(define-module m5 (use dylan import: (car) import: (cdr)))"
  ""
  :compile-error "define-module use clause: option import: given twice")
 (define-module-two-variables-one-name
  "(define-module m5 (use dylan rename: ((car => kar))) (use dylan rename: ((cdr => kar))))"
  ""
  :compile-error "define-module: two variables named kar in m5")
)
