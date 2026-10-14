;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Test programs for the optimizations (Phase 9), in the form of
;;;; cases.lisp: what is done inline, on types found or not, gives what the
;;;; generic functions give, errors included.

(
 (float-arithmetic
  "(define-method poly (x) (+ (* 2.0 x x) (* 3 x) 1))
   (define-method half-sum (n)
     (bind ((s 0.0))
       (for ((i 0 (+ i 1))) ((= i n) s)
         (set! s (+ s 0.5)))))
   (print (half-sum 10))
   (print (list (poly 2.0) (poly 2)))
   (print (list (+ 1 2.5) (- 1.5 1) (* 2 0.25) (/ 1 4.0) (/ 3.0 2) (- 2.5)))
   (print (bind ((x 1.0) (y 3.0)) (/ x y)))
   (print (list (< 1.0 2.0) (= 2.0 2.0) (> 1.5 0.5) (= 1 1.0) (< 1 1.5) (< 2.0 1)))
   (define make-adder (method (d) (bind ((x (* d 2.0))) (method (y) (+ x y)))))
   (print ((make-adder 1.5) 1.0))
   (bind ((x 0))
     (for ((i 0 (+ i 1))) ((= i 3)) (set! x (+ x 0.5)))
     (print x))
   (print (handler-case (/ 1.0 0.0) ((<error>) 'caught)))
   (print (handler-case (/ 1.0 0) ((<error>) 'caught)))
   (print (handler-case (+ 1.0 'a) ((<error>) 'caught)))"
  "5.0
(15.0 15.0)
(3.5 0.5 0.5 0.25 1.5 -2.5)
0.3333333333333333
(#t #t #t #t #t #f)
4.0
1.5
caught
caught
caught
")

 (mandelbrot
  "(define-method inside? (cr ci)
     (bind ((zr 0.0) (zi 0.0))
       (for ((i 0 (+ i 1)))
            ((or (= i 50) (> (+ (* zr zr) (* zi zi)) 4.0))
             (= i 50))
         (bind ((new-zr (+ (- (* zr zr) (* zi zi)) cr)))
           (set! zi (+ (* 2.0 zr zi) ci))
           (set! zr new-zr)))))
   (bind ((count 0))
     (for ((y 0 (+ y 1))) ((= y 5))
       (for ((x 0 (+ x 1))) ((= x 5))
         (when (inside? (- (* x 0.5) 1.5) (- (* y 0.5) 1.0))
           (set! count (+ count 1)) #f)))
     (print count))"
  "10
")

 (integer-arithmetic
  "(define big 4611686018427387903)
   (print (list (+ 2 3) (- 2 3) (* -4 5) (< -1 0) (= 7 7) (- 0 5) (- 5)))
   (print (handler-case (+ big 1) ((<error>) 'overflow)))
   (print (handler-case (* 4611686018427387903 2) ((<error>) 'overflow)))
   (print (handler-case (- (- big) 2) ((<error>) 'overflow)))
   (print (handler-case (+ 1 'a) ((<error>) 'caught)))
   (print (if 0 'true 'false))
   (print (if (< 2 1) 'true 'false))
   (print (+ 1/2 1))"
  "(5 -1 -20 #t #t -5 -5)
overflow
overflow
overflow
caught
true
false
3/2
")

 (sequence-inlines
  "(define v (vector 1 2 3))
   (define s \"abc\")
   (print (list (size v) (size s) (element v 2) (element s 1) (size '(1 2)) (size (range from: 0 size: 4))))
   (print (handler-case (element v 3) ((<error>) 'out-of-range)))
   (print (handler-case (element v -1) ((<error>) 'out-of-range)))
   (print (handler-case (element s 3) ((<error>) 'out-of-range)))
   (print (element v 5 default: 'none))
   (bind ((sum 0))
     (for-each ((x v) (y '(10 20 30 40))) () (set! sum (+ sum (* x y))))
     (print sum))
   (bind ((l '()))
     (for-each ((c s)) () (set! l (cons c l)))
     (print l))
   (print (map + '(1 2) #(10 20)))"
  "(3 3 3 #\\b 2 4)
out-of-range
out-of-range
out-of-range
none
140
(#\\c #\\b #\\a)
(11 22)
")

 (slot-inlines
  "(define-class <point> (<object>) (x init-keyword: x: init-value: 0) (y init-keyword: y:))
   (define-class <point3> (<point>) (z init-value: 9))
   (define-class <box> (<object>) (w type: <integer> init-value: 0))
   (define p (make <point> x: 1 y: 2))
   (define q (make <point3> y: 5))
   (set! (x p) 10)
   (set! (x q) (+ (x q) 1))
   (print (list (x p) (y p) (x q) (y q) (z q)))
   (print (handler-case (y (make <point>)) ((<error>) 'unbound)))
   (print (handler-case (z p) ((<error>) 'no-slot)))
   (print (handler-case (x 5) ((<error>) 'not-a-point)))
   (print (handler-case (set! (w (make <box>)) \"no\") ((<error>) 'type-error)))"
  "(10 2 1 5 9)
unbound
no-slot
not-a-point
type-error
")

 ;; A getter with a method of the program's: no longer done inline
 (slot-getter-method
  "(define-class <point> (<object>) (x init-keyword: x: init-value: 0))
   (define-method x ((s <symbol>)) 'symbol-x)
   (define p (make <point> x: 1))
   (print (list (x p) (x 'a)))"
  "(1 symbol-x)
")

 (argument-lists
  "(define sum-all
     (method (#rest xs)
       (bind ((total 0))
         (for-each ((x xs)) () (set! total (+ total x)))
         total)))
   (define count-args
     (method (#rest xs)
       (for ((l xs (cdr l)) (n 0 (+ n 1))) ((id? l '()) n))))
   (define keep (method (#rest xs) xs))
   (define stash #f)
   (define save (method (#rest xs) (set! stash xs) (size xs)))
   (define mk (method (#rest xs) (method () xs)))
   (print (list (sum-all) (sum-all 1) (sum-all 1 2 3 4 5 6 7 8 9 10 11 12)))
   (print (count-args 1 2 3 4 5 6 7 8 9))
   (define kept (keep 1 2 3))
   (keep 4 5 6)
   (save 7 8)
   (print (list kept stash ((mk 1 2))))
   (define keyed (method (a #key (b 10) c) (list a b c)))
   (print (list (keyed 1) (keyed 1 c: 3) (keyed 1 c: 3 b: 4 b: 5)))
   (print (handler-case (keyed 1 b:) ((<error>) 'odd)))
   (print (handler-case (keyed 1 d: 2) ((<error>) 'unknown)))
   (define k6 (method (a b c d e #key f g) (list a b c d e f g)))
   (print (k6 1 2 3 4 5 g: 7 f: 6))
   (define rk (method (#rest r #key a) (list r a)))
   (print (rk a: 1))"
  "(0 1 78)
9
((1 2 3) (7 8) (1 2))
((1 10 #f) (1 10 3) (1 4 3))
odd
unknown
(1 2 3 4 5 6 7)
((a: 1) 1)
")

 (known-functions
  "(define count-down (method (n acc) (if (= n 0) acc (count-down (- n 1) (+ acc 1)))))
   (print (count-down 1000000 0))
   (define even2? (method (n) (if (= n 0) #t (odd2? (- n 1)))))
   (define odd2? (method (n) (if (= n 0) #f (even2? (- n 1)))))
   (print (even2? 100001))
   (define g (method () 1))
   (set! g (method () 2))
   (print (g))
   (print (handler-case (later 1) ((<error>) 'unbound)))
   (define later (method (x) x))
   (print (later 2))"
  "1000000
#f
2
unbound
2
")

 (tak-integers
  "(define-method tak (x y z)
     (if (< y x)
         (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))
         z))
   (print (tak 18 12 6))"
  "7
")

 (tak-mixed
  "(define-method tak (x y z)
     (if (< y x)
         (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))
         z))
   (print (list (tak 18 12 6) (tak 3.0 2 1)))"
  "(7 2)
")

 ;; Calls not in tail position go to the specialized entry, their
 ;; keywords matched when compiling: the leftmost of repeated ones, a
 ;; default seeing the parameters before it, a captured, assigned keyword
 ;; parameter.  Keywords not known when compiling, or not taken, go to the
 ;; uniform entry.
 (spec-keywords
  "(define area (method (w #key (h w) (scale 1)) (* (* w h) scale)))
   (print (list (area 3) (area 3 h: 5) (area 3 scale: 10 h: 4) (area 3 h: 1 h: 9)))
   (define key h:)
   (print (list (area 2 key 7) (apply area (list 2 scale: 3))))
   (print (handler-case (area 2 depth: 1) ((<error>) 'caught)))
   (print (handler-case (area 2 h:) ((<error>) 'caught)))
   (define counter
     (method (start #key (by 1))
       (bind ((bump (method () (set! by (+ by 1)) by)))
         (bump)
         (list start by (bump)))))
   (print (counter 10 by: 5))
   (define-method label ((x <object>) #key (prefix \"#\")) (concatenate prefix (format #f \"~a\" x)))
   (print (list (label 1) (label 2 prefix: \"n\")))
   (print (handler-case (label 3 suffix: \"x\") ((<error>) 'caught)))"
  "(9 15 120 3)
(14 12)
caught
caught
(10 6 7)
(\"#1\" \"n2\")
caught
")

 ;; Doubles passed to the specialized entry unboxed
 (spec-doubles
  "(define norm (method (x y) (sqrt (+ (* x x) (* y y)))))
   (define sum-norms
     (method (n) (for ((i 0 (+ i 1)) (acc 0.0 (+ acc (norm 3.0 4.0)))) ((= i n) acc))))
   (print (sum-norms 1000))"
  "5000.0
")

 ;; A specialized entry whose body makes tail calls has its own copy of
 ;; the body, whose tail calls are ordinary calls of uniform entries:
 ;; entered a million times, and starting a million tail calls, in a 256 KB
 ;; stack.
 (spec-tail-calls
  "(define down (method (n #key (step 1)) (if (<= n 0) 'done (down (- n step) step: step))))
   (define ping (method (n #key (tag 'ping)) (if (= n 0) tag (pong (- n 1) tag: 'pong))))
   (define pong (method (n #key (tag 'pong)) (if (= n 0) tag (ping (- n 1) tag: 'ping))))
   (print (list (down 1000000) (down 1000000 step: 3) (ping 1000001)))
   (bind ((count 0))
     (for ((i 0 (+ i 1))) ((= i 1000000))
       (if (id? (down 2 step: 1) 'done) (set! count (+ count 1)) #f))
     (print count))"
  "(done done pong)
1000000
")

 ;; make of a class the program defines calls its maker; slot types, typed
 ;; binds and instance? tests give types (the last only in their branch).
 (types-make-slots-tests
  "(define-class <point> (<object>) (x type: <integer> init-value: 0 init-keyword: x:) (y init-keyword: y:))
   (define sum-x
     (method (n) (for ((i 0 (+ i 1)) (acc 0 (+ acc (x (make <point> x: i y: 'b))))) ((= i n) acc))))
   (print (sum-x 1000))
   (define twice
     (method (v)
       (if (instance? v <integer>)
           (+ v v)
           (if (not (instance? v <double-float>)) 'other (* v 2.0)))))
   (print (list (twice 3) (twice 1.5) (twice 'a)))
   (define f (method (z) (bind (((k <integer>) (* z 2))) (+ k 1))))
   (print (f 5))
   (print (handler-case (f 'a) ((<error>) 'not-a-number)))
   (print (handler-case (make <point> x: 'no) ((<error>) 'bad-type)))
   (define-class <special> (<object>))
   (define-method make ((c (singleton <special>)) #rest args) 'made-specially)
   (print (make <special>))"
  "499500
(6 3.0 other)
11
not-a-number
bad-type
made-specially
")

 ;; Closures and instances that cannot outlive the function making them are
 ;; on its stack; any that might -- kept, signalled, in a loop's variables,
 ;; passed in a tail call -- are not.
 (stack-objects
  "(define-class <box> (<object>) (v init-keyword: v:))
   (define store #f)
   (define keeper (method (b) (set! store b) 'kept))
   (define peek (method (b) (v b)))
   (define pass-on (method (b) (peek b)))
   (define tail-pass (method (b) (pass-on b)))
   (define run
     (method ()
       (bind ((a (make <box> v: 1)) (b (make <box> v: 2)) (c (make <box> v: 3)))
         (keeper a)
         (list (peek b) (tail-pass c)))))
   (print (run))
   (print (v store))
   (define closures
     (method (n)
       (bind ((fs '()))
         (for ((i 0 (+ i 1))) ((= i n) (map (method (f) (f)) fs))
           (bind ((f (method () (* i 10))))
             (set! fs (cons f fs)))))))
   (print (closures 3))
   (define local-calls
     (method (n)
       (for ((i 0 (+ i 1))
             (acc 0 (bind ((g (method (x) (+ x i)))) (+ acc (g 1) (g 2)))))
            ((= i n) acc))))
   (print (local-calls 100))
   (define caught
     (method ()
       (bind ((b (make <box> v: 9)))
         (handler-case (begin (peek b) (error \"oops ~a\" b))
           ((<error> condition: c) c)))))
   (print (instance? (caught) <error>))
   (define same (method (x y) (id? x y)))
   (define ids (method () (bind ((b (make <box>))) (list (same b b) (same b 1)))))
   (print (ids))
   (define apply-twice (method (f v) (f (f v))))
   (define adder
     (method (k) (bind ((add (method (v) (+ v k)))) (list (apply-twice add 1) (add 10)))))
   (print (adder 5))
   (define keep #f)
   (define leak (method (k) (bind ((c (method () k))) (set! keep c) (c))))
   (print (list (leak 1) (keep)))"
  "(2 3)
1
(20 10 0)
10200
#t
(#t #f)
(11 15)
(1 1)
"))
