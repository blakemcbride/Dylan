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
 ;; Calls, apply and values take any number of arguments and values: past
 ;; the 1024 of the thread's own area, the rest go in its overflow.
 (many-arguments-and-values
  "(define-method try (f)
  (handler-case (f)
    ((<error> condition: c)
     (apply format #f (condition-format-string c) (condition-format-arguments c)))))
(define l (bind-methods ((up (i acc) (if (= i 0) acc (up (- i 1) (cons i acc))))) (up 2000 '())))
(print (apply + l))
(print (size (apply list l)))
(print (apply + 1 2 l))
(print (apply + (as <vector> l)))
(print (size (apply vector 0 (as <vector> l))))
(print (+ 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1024 1025 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1100))
(define-method many (p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40 p41 p42 p43 p44 p45 p46 p47 p48 p49 p50 p51 p52 p53 p54 p55 p56 p57 p58 p59 p60 p61 p62 p63 p64 p65 p66 p67 p68 p69 p70 p71 p72 p73 p74 p75 p76 p77 p78 p79 p80 p81 p82 p83 p84 p85 p86 p87 p88 p89 p90 p91 p92 p93 p94 p95 p96 p97 p98 p99 p100 p101 p102 p103 p104 p105 p106 p107 p108 p109 p110 p111 p112 p113 p114 p115 p116 p117 p118 p119 p120 p121 p122 p123 p124 p125 p126 p127 p128 p129 p130 p131 p132 p133 p134 p135 p136 p137 p138 p139 p140 p141 p142 p143 p144 p145 p146 p147 p148 p149 p150 p151 p152 p153 p154 p155 p156 p157 p158 p159 p160 p161 p162 p163 p164 p165 p166 p167 p168 p169 p170 p171 p172 p173 p174 p175 p176 p177 p178 p179 p180 p181 p182 p183 p184 p185 p186 p187 p188 p189 p190 p191 p192 p193 p194 p195 p196 p197 p198 p199 p200 p201 p202 p203 p204 p205 p206 p207 p208 p209 p210 p211 p212 p213 p214 p215 p216 p217 p218 p219 p220 p221 p222 p223 p224 p225 p226 p227 p228 p229 p230 p231 p232 p233 p234 p235 p236 p237 p238 p239 p240 p241 p242 p243 p244 p245 p246 p247 p248 p249 p250 p251 p252 p253 p254 p255 p256 p257 p258 p259 p260 p261 p262 p263 p264 p265 p266 p267 p268 p269 p270 p271 p272 p273 p274 p275 p276 p277 p278 p279 p280 p281 p282 p283 p284 p285 p286 p287 p288 p289 p290 p291 p292 p293 p294 p295 p296 p297 p298 p299 p300 p301 p302 p303 p304 p305 p306 p307 p308 p309 p310 p311 p312 p313 p314 p315 p316 p317 p318 p319 p320 p321 p322 p323 p324 p325 p326 p327 p328 p329 p330 p331 p332 p333 p334 p335 p336 p337 p338 p339 p340 p341 p342 p343 p344 p345 p346 p347 p348 p349 p350 p351 p352 p353 p354 p355 p356 p357 p358 p359 p360 p361 p362 p363 p364 p365 p366 p367 p368 p369 p370 p371 p372 p373 p374 p375 p376 p377 p378 p379 p380 p381 p382 p383 p384 p385 p386 p387 p388 p389 p390 p391 p392 p393 p394 p395 p396 p397 p398 p399 p400 p401 p402 p403 p404 p405 p406 p407 p408 p409 p410 p411 p412 p413 p414 p415 p416 p417 p418 p419 p420 p421 p422 p423 p424 p425 p426 p427 p428 p429 p430 p431 p432 p433 p434 p435 p436 p437 p438 p439 p440 p441 p442 p443 p444 p445 p446 p447 p448 p449 p450 p451 p452 p453 p454 p455 p456 p457 p458 p459 p460 p461 p462 p463 p464 p465 p466 p467 p468 p469 p470 p471 p472 p473 p474 p475 p476 p477 p478 p479 p480 p481 p482 p483 p484 p485 p486 p487 p488 p489 p490 p491 p492 p493 p494 p495 p496 p497 p498 p499 p500 p501 p502 p503 p504 p505 p506 p507 p508 p509 p510 p511 p512 p513 p514 p515 p516 p517 p518 p519 p520 p521 p522 p523 p524 p525 p526 p527 p528 p529 p530 p531 p532 p533 p534 p535 p536 p537 p538 p539 p540 p541 p542 p543 p544 p545 p546 p547 p548 p549 p550 p551 p552 p553 p554 p555 p556 p557 p558 p559 p560 p561 p562 p563 p564 p565 p566 p567 p568 p569 p570 p571 p572 p573 p574 p575 p576 p577 p578 p579 p580 p581 p582 p583 p584 p585 p586 p587 p588 p589 p590 p591 p592 p593 p594 p595 p596 p597 p598 p599 p600 p601 p602 p603 p604 p605 p606 p607 p608 p609 p610 p611 p612 p613 p614 p615 p616 p617 p618 p619 p620 p621 p622 p623 p624 p625 p626 p627 p628 p629 p630 p631 p632 p633 p634 p635 p636 p637 p638 p639 p640 p641 p642 p643 p644 p645 p646 p647 p648 p649 p650 p651 p652 p653 p654 p655 p656 p657 p658 p659 p660 p661 p662 p663 p664 p665 p666 p667 p668 p669 p670 p671 p672 p673 p674 p675 p676 p677 p678 p679 p680 p681 p682 p683 p684 p685 p686 p687 p688 p689 p690 p691 p692 p693 p694 p695 p696 p697 p698 p699 p700 p701 p702 p703 p704 p705 p706 p707 p708 p709 p710 p711 p712 p713 p714 p715 p716 p717 p718 p719 p720 p721 p722 p723 p724 p725 p726 p727 p728 p729 p730 p731 p732 p733 p734 p735 p736 p737 p738 p739 p740 p741 p742 p743 p744 p745 p746 p747 p748 p749 p750 p751 p752 p753 p754 p755 p756 p757 p758 p759 p760 p761 p762 p763 p764 p765 p766 p767 p768 p769 p770 p771 p772 p773 p774 p775 p776 p777 p778 p779 p780 p781 p782 p783 p784 p785 p786 p787 p788 p789 p790 p791 p792 p793 p794 p795 p796 p797 p798 p799 p800 p801 p802 p803 p804 p805 p806 p807 p808 p809 p810 p811 p812 p813 p814 p815 p816 p817 p818 p819 p820 p821 p822 p823 p824 p825 p826 p827 p828 p829 p830 p831 p832 p833 p834 p835 p836 p837 p838 p839 p840 p841 p842 p843 p844 p845 p846 p847 p848 p849 p850 p851 p852 p853 p854 p855 p856 p857 p858 p859 p860 p861 p862 p863 p864 p865 p866 p867 p868 p869 p870 p871 p872 p873 p874 p875 p876 p877 p878 p879 p880 p881 p882 p883 p884 p885 p886 p887 p888 p889 p890 p891 p892 p893 p894 p895 p896 p897 p898 p899 p900 p901 p902 p903 p904 p905 p906 p907 p908 p909 p910 p911 p912 p913 p914 p915 p916 p917 p918 p919 p920 p921 p922 p923 p924 p925 p926 p927 p928 p929 p930 p931 p932 p933 p934 p935 p936 p937 p938 p939 p940 p941 p942 p943 p944 p945 p946 p947 p948 p949 p950 p951 p952 p953 p954 p955 p956 p957 p958 p959 p960 p961 p962 p963 p964 p965 p966 p967 p968 p969 p970 p971 p972 p973 p974 p975 p976 p977 p978 p979 p980 p981 p982 p983 p984 p985 p986 p987 p988 p989 p990 p991 p992 p993 p994 p995 p996 p997 p998 p999 p1000 p1001 p1002 p1003 p1004 p1005 p1006 p1007 p1008 p1009 p1010 p1011 p1012 p1013 p1014 p1015 p1016 p1017 p1018 p1019 p1020 p1021 p1022 p1023 p1024 p1025 p1026 p1027 p1028 p1029 p1030 p1031 p1032 p1033 p1034 p1035 p1036 p1037 p1038 p1039 p1040 p1041 p1042 p1043 p1044 p1045 p1046 p1047 p1048 p1049 p1050 p1051 p1052 p1053 p1054 p1055 p1056 p1057 p1058 p1059 p1060 p1061 p1062 p1063 p1064 p1065 p1066 p1067 p1068 p1069 p1070 p1071 p1072 p1073 p1074 p1075 p1076 p1077 p1078 p1079 p1080 p1081 p1082 p1083 p1084 p1085 p1086 p1087 p1088 p1089 p1090 p1091 p1092 p1093 p1094 p1095 p1096 p1097 p1098 p1099 p1100) (list p1 p1024 p1025 p1100))
(print (many 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1024 1025 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1100))
(print (apply many (copy-sequence l end: 1100)))
(print (try (method () (apply many l))))
(define-method rest-count (#rest r) (size r))
(print (apply rest-count l))
(bind ((#rest vs (apply values l)))
  (print (list (size vs) (first vs) (last vs))))
(bind ((a b #rest vs (values 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1024 1025 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1100)))
  (print (list a b (size vs) (last vs))))
(define-method mv () (apply values l))
(bind ((#rest vs (mv))) (print (size vs)))
(bind ((#rest vs (bind-exit (k) (apply k l)))) (print (size vs)))
(define-method nm ((x <integer>) #rest r) (list 'int (size r) (next-method)))
(define-method nm ((x <number>) #rest r) (list 'num (size r)))
(print (apply nm l))
(define-method nm2 ((x <integer>) #rest r) (bind ((k next-method)) (list 'int (size r) (k))))
(define-method nm2 ((x <number>) #rest r) (list 'num (size r)))
(print (apply nm2 l))
(print (bind ((#rest vs (unwind-protect (apply values l) (size l)))) (size vs)))
(define-method kw (#key a b) (list a b))
(print (try (method () (apply kw l))))"
  "2001000
2000
2001003
2001000
2001
3150
(1 1024 1025 1100)
(1 1024 1025 1100)
\"wrong number of arguments (2000)\"
2000
(2000 1 2000)
(1 0 1098 1100)
2000
2000
(int 1999 (num 1999))
(int 1999 (num 1999))
2000
\"the arguments after the required ones are not keyword/value pairs\"
")
)
