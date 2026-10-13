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
 ;; leftmost of a repeated keyword winning, and #key alone.
 (rest-and-keys
  "(define show-rest (method (a #rest b) (print a) (print b) #t))
   (print (show-rest 10 20 30 40))
   (print (show-rest 10))
   (define test (method (the-req #rest the-rest #key a b) (print the-req) (print the-rest) (print a) (print b)))
   (test 1 a: 2 b: 3 c: 4)
   (define k (method (x #key (y (+ x 1)) z) (list x y z)))
   (print (k 1))
   (print (k 1 z: 5 y: 2 y: 9))
   (define any (method (#key) 'any))
   (print (any foo: 1 bar: 2))"
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
any
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
  :error "type error")

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
  :error "integer overflow")
)
