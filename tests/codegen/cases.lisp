;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Test programs for the code generator (Phase 4).  Each is
;;;;
;;;;   (name "dylan source" "expected standard output" [:error "text"])
;;;;
;;;; The program is compiled, built and run; its output must be exactly
;;;; the expected text.  With :error, it must instead stop with an error
;;;; whose message contains the text, after writing the expected output.
;;;; Programs run with a 256 KB stack, so a tail call that is not a jump
;;;; overflows it.
;;;;
;;;; print writes a value and a newline.  Until the library exists
;;;; (Phase 8) programs use the few functions runtime/c/primitives.c
;;;; provides, and define functions with define and method (generic
;;;; functions are Phase 5).

(
 (arithmetic
  "(define fib (method (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))
   (print (fib 20))
   (print (- 7))
   (print (/ 6 4))
   (print (* 1/2 4))
   (print (+ 1 2 3 4 5 6 7))
   (print (< 1 2 3))
   (print (< 1 3 2))"
  "6765
-7
3/2
2
28
#t
#f
")

 ;; Chapter 8: the iteration macros, and that they run in constant
 ;; stack.
 (loops
  "(define sum-to (method (n) (bind ((s 0) (i 0)) (while (< i n) (set! i (+ i 1)) (set! s (+ s i))) s)))
   (print (sum-to 1000000))
   (define fact (method (n) (for ((i n (- i 1)) (v 1 (* v i))) ((<= i 0) v))))
   (print (fact 20))
   (dotimes (i 3) (print i))
   (print (dotimes (i 4 (* i 10))))
   (bind ((n 0)) (until (= n 3) (set! n (+ n 1))) (print n))"
  "500000500000
2432902008176640000
0
1
2
40
3
")

 ;; Proper tail calls: a million calls through module variables, through
 ;; closures passed as arguments, and through apply.
 (tail-calls
  "(define ev? (method (n) (if (= n 0) #t (od? (- n 1)))))
   (define od? (method (n) (if (= n 0) #f (ev? (- n 1)))))
   (print (ev? 1000000))
   (define count-down (method (n k) (if (= n 0) (k 'done) (count-down (- n 1) (method (x) (k x))))))
   (print (count-down 1000000 (method (x) x)))
   (define loop2 (method (n f) (if (= n 0) 'ok (f (- n 1) f))))
   (print (loop2 1000000 loop2))
   (define via-apply (method (n) (if (= n 0) 'applied (apply via-apply (list (- n 1))))))
   (print (via-apply 1000000))"
  "#t
done
ok
applied
")

 ;; Local methods that call each other are loops; ones that escape are
 ;; closures.
 (local-methods
  "(bind-methods ((ev? (n) (if (= n 0) #t (od? (- n 1))))
                  (od? (n) (if (= n 0) #f (ev? (- n 1)))))
     (print (ev? 1000001)))
   (bind-methods ((fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))
     (print (fib 15)))
   (bind-methods ((sq (x) (* x x)))
     (print (list (sq 3) (sq 4))))"
  "#f
610
(9 16)
")

 ;; Closures: shared and separate state; for makes fresh bindings each
 ;; pass (chapter 8), so each closure sees its own i.
 (closures
  "(define make-counter (method () (bind ((c 0)) (method () (set! c (+ c 1)) c))))
   (define a (make-counter))
   (define b (make-counter))
   (a) (a) (b)
   (print (list (a) (b)))
   (define make-pair (method () (bind ((v 0)) (values (method () v) (method (x) (set! v x))))))
   (bind ((get put (make-pair))) (put 42) (print (get)))
   (define fs (for ((i 0 (+ i 1)) (l '() (cons (method () i) l))) ((= i 3) l)))
   (print (list ((car fs)) ((car (cdr fs))) ((car (cdr (cdr fs))))))"
  "(3 2)
42
(2 1 0)
")

 ;; Chapter 6: #rest (the manual's show-rest), #rest with #key (the
 ;; manual's test), defaults that see the parameters before them, the
 ;; leftmost of a repeated keyword winning, and #key alone, which accepts
 ;; no keywords.
 (rest-and-keys
  "(define show-rest (method (a #rest b) (print a) (print b) #t))
   (print (show-rest 10 20 30 40))
   (print (show-rest 10))
   (define test (method (the-req #rest the-rest #key a b) (print the-req) (print the-rest) (print a) (print b)))
   (test 1 a: 2 b: 3 c: 4)
   (define k (method (x #key (y (+ x 1)) z) (list x y z)))
   (print (k 1))
   (print (k 1 z: 5 y: 2 y: 9))
   (define none (method (#key) 'none))
   (print (none))"
  "10
(20 30 40)
#t
10
()
#t
1
(a: 2 b: 3 c: 4)
2
3
(1 2 #f)
(1 2 5)
none
")

 ;; More than four arguments: the rest go in the thread's argument area,
 ;; in calls, tail calls and apply.
 (many-arguments
  "(define f7 (method (a b c d e f g) (list a b c d e f g)))
   (print (f7 1 2 3 4 5 6 7))
   (define g7 (method (a b c d e f g) (if (= a 0) (list b c d e f g) (g7 (- a 1) c d e f g b))))
   (print (g7 6 1 2 3 4 5 6))
   (print (apply f7 1 2 '(3 4 5 6 7)))
   (define r (method (#rest xs) xs))
   (print (r 1 2 3 4 5 6 7 8 9 10))
   (print (+ 1 2 3 4 5 6 7 8 9 10))"
  "(1 2 3 4 5 6 7)
(1 2 3 4 5 6)
(1 2 3 4 5 6 7)
(1 2 3 4 5 6 7 8 9 10)
55
")

 ;; Multiple values (chapter 4): missing ones are #f; a function that
 ;; returns one value after calling values does not pass the others on.
 (multiple-values
  "(bind ((a b c (values 1 2))) (print (list a b c)))
   (bind ((a #rest r (values 1 2 3))) (print (list a r)))
   (bind ((a b (values))) (print (list a b)))
   (define two (method () (values 'x 'y)))
   (define through (method () (two)))
   (bind ((a b (through))) (print (list a b)))
   (define one (method () (two) 'single))
   (bind ((a b (one))) (print (list a b)))
   (bind ((a b (if (< 1 2) (two) 3))) (print (list a b)))
   (bind ((a b (if (< 2 1) (two) 3))) (print (list a b)))
   (bind ((a b (apply values '(5 6)))) (print (list a b)))
   (print (values 7 8))"
  "(1 2 #f)
(1 (2 3))
(#f #f)
(x y)
(single #f)
(x y)
(3 #f)
(5 6)
7
")

 ;; The conditionals of chapter 8, and backquote.
 (conditionals
  "(define classify (method (x) (case x ((1 2) 'small) ((3) 'three) (else: 'big))))
   (print (list (classify 1) (classify 3) (classify 9)))
   (print (cond ((< 2 1) 'no) ((< 1 2) 'yes)))
   (print (cond ((< 2 1) 'no)))
   (print (list (and 1 2) (and 1 #f 2) (or #f 3) (or #f #f)))
   (print (select 5 < ((1 2) 'tiny) ((10) 'medium) (else: 'huge)))
   (print (when (< 1 2) 'w))
   (print (unless (< 1 2) 'u))
   (print `(1 ,(+ 1 1) ,@(list 3 4) . 5))
   (print `#(a ,(+ 1 2)))"
  "(small three big)
yes
#f
(2 #f 3 #f)
medium
w
#f
(1 2 3 4 . 5)
#(a 3)
")

 ;; Chapter 4: typed bindings are checked.
 (typed-bind
  "(bind (((x <integer>) 3)) (print x))
   (bind (((y <integer>) 1.5)) (print y))"
  "3
"
  :error "1.5 is not an instance of")

 ;; Run-time errors
 (wrong-argument-count
  "(define f (method (a b) a)) (print 'before) (f 1)"
  "before
"
  :error "wrong number of arguments")
 (unknown-keyword
  "(define f (method (#key a) a)) (f b: 1)"
  ""
  :error "keyword argument not accepted")
 (key-alone
  "(define f (method (#key) 'none)) (f a: 1)"
  ""
  :error "keyword argument not accepted")
 (odd-keywords
  "(define f (method (#key a) a)) (f a:)"
  ""
  :error "not keyword/value pairs")
 (unbound-variable
  "(print (g 1)) (define g (method (x) x))"
  ""
  :error "the variable g@dylan-user has no value")
 (not-a-function
  "(define f 3) (f 1)"
  ""
  :error "not a function")
 (integer-overflow
  "(define big (method (n) (if (= n 0) 1 (* 2 (big (- n 1)))))) (print (big 61)) (print (big 62))"
  "2305843009213693952
"
  :error "integer overflow"
  :compiler-only t)

 ;; Text that C would misread: ?? in a string or symbol, which C11 takes
 ;; for the start of a trigraph, and a variable name containing */, which
 ;; would end the comment it is written in.
 (c-text
  "(format #t \"~s ~s~%\" \"what??!\" (size \"what??!\"))
   (format #t \"~s ~s~%\" \"a??/b\" (size \"a??/b\"))
   (print '|q??!|)
   (define x*/y 5)
   (define z??/ 6)
   (print (+ x*/y z??/))"
  "\"what??!\" 7
\"a??/b\" 5
|q??!|
11
")

 ;; Each literal the program writes is its own object, as reading it makes
 ;; one: equal literals in two places are two objects; one place is one
 ;; object however often it runs.
 (literal-identity
  "(define a \"abc\") (define b \"abc\")
   (set! (element a 0) #\\x)
   (format #t \"~s ~s~%\" a b)
   (define la '(1 2 3)) (define lb '(9 2 3))
   (set! (car (cdr la)) 'x)
   (print (list la lb))
   (define va #(1 \"s\" (2))) (define vb #(1 \"s\" (2)))
   (set! (element va 0) 'q)
   (print (list va vb))
   (print (id? \"a\" \"a\"))
   (define-method lit () \"x\")
   (define-method f1 () (lit))
   (define-method f2 () (lit))
   (print (id? (f1) (f2)))
   (print (case '(1) (((1)) 'yes) (else: 'no)))
   (define-method s ((x (singleton \"s\"))) 'single)
   (define-method s (x) 'other)
   (print (s \"s\"))"
  "\"xbc\" \"abc\"
((1 x 3) (9 2 3))
(#(q \"s\" (2)) #(1 \"s\" (2)))
#f
#t
no
other
")

 ;; Chapter 8: the end test of for and for-each is one value, even with no
 ;; result forms after it, and the last result form gives the values.
 (end-test-values
  "(define-method done? (i) (values (> i 3) i))
   (for ((i 0 (+ i 1))) ((done? i)) (print i))
   (for-each ((x '(1 2 3 4 5))) ((done? x)) (print x))
   (define-method f () (for ((i 0 (+ i 1))) ((done? i))))
   (bind ((#rest r (f))) (print r))
   (define-method g () (for ((i 0 (+ i 1))) ((done? i) (values 'a 'b))))
   (bind ((#rest r (g))) (print r))
   (define-method h () (for-each ((x '(1 2 3 4 5))) ((done? x) (values 'c x))))
   (bind ((#rest r (h))) (print r))
   (define-method k () (for-each ((x '(1 2 3 4 5))) ((done? x))))
   (bind ((#rest r (k))) (print r))"
  "0
1
2
3
1
2
3
(#f)
(a b)
(c 4)
(#f)
")

 ;; Chapter 7: (set! (name arg ...) value) means ((setter name) arg ...
 ;; value), so its arguments are evaluated left to right, the place's
 ;; before the new value.
 (set-place-order
  "(define v (vector 1 2 3))
   (define order '())
   (define note (method (x tag) (set! order (cons tag order)) x))
   (set! (element (note v 'coll) (note 0 'idx)) (note 9 'val))
   (print (list v (reverse order)))
   (set! order '())
   (define-class <p> (<object>) (x init-value: 0))
   (define p (make <p>))
   (set! (x (note p 'obj)) (note 5 'val))
   (print (list (x p) (reverse order)))
   (print (set! (element v 1) (set! (element v 2) 7)))
   (print v)"
  "(#(9 2 3) (coll idx val))
(5 (obj val))
7
#(9 7 7)
")

 ;; Names that are Lisp's own in the interactive implementation: keyword
 ;; parameters named t, nil and a@b are passed as t:, nil: and a@b:;
 ;; &rest, &key and the like are ordinary names; a parameter may be named
 ;; next-method, which then is not the next method.
 (lisp-like-names
  "(define-method k1 (#key t nil a@b) (list t nil a@b))
   (print (list (k1) (k1 t: 1 nil: 2 a@b: 3)))
   (define-method k2 (#key (t 7) (nil 8) (%%x 9)) (list t nil %%x))
   (print (list (k2) (k2 t: 1 nil: 2 %%x: 3)))
   (define-generic-function gk (x #key t a@b))
   (define-method gk (x #key t a@b) (list t a@b))
   (print (gk 1 t: 2 a@b: 3))
   (bind ((#rest r (function-arguments gk))) (print r))
   (bind ((&rest 2) (&key 3) (&aux 4) (&body 5) (&optional 6))
     (print (list &rest &key &aux &body &optional)))
   (print ((method (&optional &rest) (list &optional &rest)) 6 7))
   (define &key 8)
   (print &key)
   (define-method nm (next-method) next-method)
   (define-method nm2 ((next-method <integer>) #rest r) (list next-method r))
   (define-method nm3 (x #key next-method) (list x next-method))
   (define-method nm4 ((x <integer>)) (list x (next-method)))
   (define-method nm4 (x) (list 'top x))
   (print (list (nm 4) (nm2 4 5) (nm3 4 next-method: 5) (nm4 3)))"
  "((#f #f #f) (1 2 3))
((7 8 9) (1 2 3))
(2 3)
(1 #f (t: a@b:))
(2 3 4 5 6)
(6 7)
8
(4 (4 (5)) (4 5) (3 (top 3)))
")

 ;; #t is not a variable name.
 (true-not-a-variable
  "(define #t 3)"
  ""
  :compile-error "#t")
 (true-not-a-parameter
  "(print ((method (#t) #t) 2))"
  ""
  :compile-error "#t")
 (true-not-bound
  "(bind ((#t 3)) (print #t))"
  ""
  :compile-error "#t")

 ;; bind-methods with no body answers #f, as a method's empty body does.
 (bind-methods-without-body
  "(print (bind-methods ((f () 1))))
   (print (bind-methods ((f () 1) (g () (f))) (g)))"
  "#f
1
")
 ;; A quoted list of 50,000 elements, of integers and of strings, made by a
 ;; macro.
 (long-quoted-list
  "(define-macro long-list (n) (list 'quote (as <list> (range from: 0 size: n))))
(define big (long-list 50000))
(print (list (size big) (first big) (last big) (reduce1 + big)))
(define-macro long-strings (n)
  (list 'quote (map (method (i) (format #f \"s~a\" (modulo i 7))) (range from: 0 size: n))))
(define big2 (long-strings 50000))
(print (list (size big2) (id? (first big2) (element big2 7)) (last big2)))"
  "(50000 0 49999 1249975000)
(50000 #f \"s5\")
")
 ;; A name dylan-user excludes from the dylan module is free for a
 ;; variable of its own (chapter 5), a class here; the library's is unchanged.
 (excluded-class-name-is-free
  "(define-module dylan-user (use dylan exclude: (<range>)))
(define-class <range> (<object>))
(print (as <list> (range from: 1 size: 3)))
(print (instance? (make <range>) <range>))"
  "(1 2 3)
#t
")
 ;; A name dylan-user excludes from the dylan module is free for a
 ;; variable of its own (chapter 5), even one the library holds a class in.
 (excluded-names-are-free
  "(define-module dylan-user (use dylan exclude: (<table> reverse)))
(define <table> 42)
(define reverse 'mine)
(print (list <table> reverse))"
  "(42 mine)
")
 ;; With element renamed, (setter element) in dylan-user is a variable of
 ;; its own (chapter 5), not the library's.
 (renamed-setter-is-free
  "(define-module dylan-user (use dylan rename: ((element => elt))))
(define-method (setter element) (v (x <integer>)) (list 'mine v x))
(print ((setter element) 1 2))
(define v (vector 1 2))
(set! (elt v 0) 9)
(print v)"
  "(mine 1 2)
#(9 2)
")
 ;; A symbol prints as it was first written, and the implementation's own
 ;; names -- the variables the dylan module exports, in lower case -- come
 ;; first (chapter 15).
 (system-name-spellings
  "(print (list 'Element 'SIZE 'Car 'Mine Test: 'mine test: 'STATE 'Format))
(print (as <string> 'Vector))"
  "(element size car Mine Test: Mine Test: STATE format)
\"vector\"
")
)
