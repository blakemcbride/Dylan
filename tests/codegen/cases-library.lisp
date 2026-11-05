;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Test programs for the library (Phase 8), in the form of cases.lisp.
;;;; Most are the manual's own examples, with the results it gives; where
;;;; the manual leaves an order unspecified, the test does not depend on it.

(
 ;; Chapter 9
 (comparisons
  "(print (list (= 1 1.0) (id? 1 1.0) (= 1/2 0.5) (/= 1 2) (= 'a 'a 'a)))
   (print (list (< 1 2 3) (< 1 3 2) (> 3 2 1) (<= 1 1 2) (>= 2 2 3)))
   (print (list (< #\\a #\\b) (< \"abc\" \"abd\") (< \"ab\" \"abc\") (< \"b\" \"abc\")))
   (print (list (= '(1 2) (list 1 2)) (= #(1 2) '(1 2)) (= \"ab\" \"ab\") (= '(1 . 2) (cons 1 2))
                (= '(1 2) '(1 2 3))))
   (print (list (= (=hash 3) (=hash 3.0)) (= (=hash \"xy\") (=hash (concatenate \"x\" \"y\")))))"
  "(#t #f #t #t #t)
(#t #f #t #t #f)
(#t #t #t #f)
(#t #t #t #t #f)
(#t #t)
")

 ;; Chapter 14
 (numbers
  "(print (list (+) (*) (+ 5) (- 5) (/ 2) (+ 1 2 3) (- 10 1 2) (* 2 3 4) (/ 1 2 4)))
   (print (list (+ 1/2 1/3) (* 2 0.5) (+ 1 1.5s0)))
   (bind ((q r (floor/ 7 2))) (print (list q r)))
   (bind ((q r (floor/ -7 2))) (print (list q r)))
   (bind ((q r (ceiling/ 7 2))) (print (list q r)))
   (bind ((q r (round/ 5 2))) (print (list q r)))
   (bind ((q r (truncate/ -7 2))) (print (list q r)))
   (bind ((q r (floor 3.5))) (print (list q r)))
   (bind ((q r (round 2.5))) (print (list q r)))
   (bind ((q r (truncate -3.5))) (print (list q r)))
   (bind ((q r (floor 7/2))) (print (list q r)))
   (print (list (modulo -7 2) (remainder -7 2) (modulo 7.5 2)))
   (print (list (abs -3) (abs 2.5) (min 3 1 2) (max 3 1 2) (gcd 12 18) (lcm 4 6) (gcd) (lcm)))
   (print (list (expt 2 10) (expt 2 -2) (expt 2/3 2) (expt 2.0 3) (sqrt 16) (sqrt 2)))
   (print (list (zero? 0) (positive? -1) (negative? -1) (even? 4) (odd? 4) (integral? 2.0)
                (integral? 1/2)))
   (print (list (numerator 6/4) (denominator 6/4) (rationalize 0.5) (rationalize 0.1)))
   (print (list (logand 12 10) (logior 12 10) (logxor 12 10) (lognot 0) (ash 1 10) (ash -8 -1)
                (logbit? 2 4) (logand) (logior)))
   (print (list (exp 0) (log 1) (sin 0) (atan2 1 1)))
   (print (list (real-part 3) (imag-part 3) (make-rectangular 2 0) (as <single-float> 1/2)))"
  "(0 1 5 -5 1/2 6 7 24 1/8)
(5/6 1.0 2.5s0)
(3 1)
(-4 1)
(4 -1)
(2 1)
(-3 -1)
(3 0.5)
(2 0.5)
(-3 -0.5)
(3 1/2)
(1 -1 1.5)
(3 2.5 1 3 6 12 0 1)
(1024 1/4 4/9 8.0 4.0 1.4142135623730951)
(#t #f #t #t #f #t #f)
(3 2 1/2 1/10)
(8 14 6 -1 1024 -4 #t -1 0)
(1.0 0.0 0.0 0.7853981633974483)
(3 0 2 0.5s0)
")

 ;; Chapters 15 and 16
 (symbols-and-characters
  "(print (as <symbol> \"bar\"))
   (print (id? 'FOO (as <symbol> \"Foo\")))
   (print (as <keyword> \"foo\"))
   (print (list (as <string> 'baz) (as <string> 'bar:)))
   (print (list (as-uppercase #\\a) (as-lowercase #\\A) (as <integer> #\\A) (as <character> 97)))"
  "bar
#t
foo:
(\"baz\" \"bar\")
(#\\A #\\a 65 #\\a)
")

 ;; Chapter 17: the manual's examples
 (functional
  "(define-method sum ((numbers <sequence>)) (reduce1 + numbers))
   (define-method square ((x <number>)) (* x x))
   (define-method square-all ((coords <sequence>)) (map square coords))
   (define distance (compose sqrt sum square-all))
   (print (distance '(3 4 5)))
   (define female? (method (name) (member? name #(michelle anne roseanne))))
   (print (map female? '(michelle arnold roseanne)))
   (print (map (complement female?) '(michelle arnold roseanne)))
   (print (map (curry + 1) '(3 4 5)))
   (define yuppify (rcurry concatenate \", ayup\"))
   (print (yuppify \"I'm from New Hampsha\"))
   (print ((always 1) 'x 'y 'z))
   (print ((always #t) #f #f))
   (print (list ((disjoin even? zero?) 3) ((disjoin odd? zero?) 3) ((conjoin odd? positive?) 3)
                ((conjoin odd? negative?) 3) (identity 'same)))"
  "7.0710678118654755
(#t #f #t)
(#f #t #f)
(4 5 6)
\"I'm from New Hampsha, ayup\"
1
#t
(#f #t #t #f same)
")

 ;; Chapter 12: functions for collections, the manual's examples.  map-into
 ;; passes the function the source collections' elements only, as the
 ;; chapter's prose and design note 27 say; its one example, which also
 ;; passes the target's, disagrees (README.md, "Differences").
 (collections
  "(print (do (method (a b) (print (+ a b))) '(100 100 200 200) '(1 2 3 4)))
   (print (map + '(100 100 200 200) '(1 2 3 4)))
   (print (map-as <vector> + '(100 100 200 200) '(1 2 3 4)))
   (define x (list 100 100 200 200))
   (print (map-into x + '(1 2 3 4)))
   (print x)
   (print (list (any? > '(1 2 3 4) '(5 4 3 2)) (any? even? '(1 3 5 7))))
   (print (list (every? > '(1 2 3 4) '(5 4 3 2)) (every? odd? '(1 3 5 7))))
   (define high-score 10)
   (print (list (reduce max high-score '(3 1 4 1 5 9)) (reduce max high-score '(3 12 9 8 8 6))))
   (print (reduce1 + '(1 2 3 4 5)))
   (define flavors #(chocolate pistachio pumpkin))
   (print (list (member? 'chocolate flavors) (member? 'banana flavors)))
   (define has-nuts? (method (f) (id? f 'pistachio)))
   (print (list (find-key flavors has-nuts?) (element flavors 1)))
   (define double (method (n) (* 2 n)))
   (define numbers (list 10 13 16 19))
   (print (replace-elements! numbers odd? double))
   (define y (list 'a 'b 'c 'd 'e 'f))
   (print (fill! y 3 start: 2))
   (print (list (size '(1 2 3)) (size #(1 2)) (size \"abc\") (empty? '()) (empty? #(1))))
   (print (key-sequence #(a b c)))"
  "101
102
203
204
#f
(101 102 203 204)
#(101 102 203 204)
(1 2 3 4)
(1 2 3 4)
(#t #f)
(#f #t)
(10 12)
15
(#t #f)
(1 pistachio)
(10 26 16 38)
(a b 3 3 3 3)
(3 2 3 #t #f)
{range 0 by 1 size 3}
")

 ;; Chapter 12: functions for sequences, the manual's examples
 (sequences
  "(define numbers '(3 4 5))
   (print (add numbers 1))
   (print numbers)
   (print (add! (list 3 4 5) 1))
   (print (list (add-new '(3 4 5) 1) (add-new '(3 4 5) 4)))
   (print (list (add-new! (list 3 4 5) 1) (add-new! (list 3 4 5) 4)))
   (print (remove '(3 1 4 1 5 9) 1))
   (print (remove! (list 3 1 4 1 5 9) 1))
   (print (remove '(1 1 1 2) 1 count: 2))
   (print (choose even? '(3 1 4 1 5 9)))
   (print (choose-by even? (range from: 1) '(a b c d e f g h i)))
   (print (intersection '(john paul george ringo) '(richard george edward charles john)))
   (print (sort (union '(butter flour sugar salt eggs) '(eggs butter mushrooms onions salt))
                test: (method (a b) (< (as <string> a) (as <string> b)))))
   (print (remove-duplicates '(spam eggs spam sausage spam spam spam)))
   (define hamlet '(to be or not to be))
   (print (list (id? hamlet (copy-sequence hamlet)) (copy-sequence hamlet start: 2 end: 4)))
   (print (concatenate-as <string> '(#\\n #\\o #\\n) '(#\\f #\\a #\\t)))
   (print (concatenate-as <vector> '(0 1 2) '(3 4 5) '(6 7 8)))
   (print (concatenate \"low-\" \"calorie\"))
   (print (concatenate '(0 1 2) '(3 4 5) '(6 7 8)))
   (define phrase (copy-sequence \"I hate oatmeal.\"))
   (print (replace-subsequence! phrase \"like\" start: 2))
   (define bim '(bim bam boom))
   (print (list (reverse bim) bim (reverse! (list 'bim 'bam 'boom))))
   (define digits '(3 1 4 1 5 9))
   (print (list (sort digits) digits (sort! (list 3 1 4 1 5 9)) (sort #(3 1 2) test: >)))
   (print (sort '((b . 1) (a . 1) (c . 0) (d . 1)) test: (method (x y) (< (cdr x) (cdr y)))))
   (print (list (first '(a b c)) (second '(a b c)) (third '(a b c)) (last '(emperor of china))))
   (define v (vector 1 2 3))
   (set! (first v) 'one)
   (set! (last v) 'three)
   (print v)
   (print (subsequence-position \"Ralph Waldo Emerson\" \"Waldo\"))
   (print (subsequence-position \"abcabc\" \"bc\" count: 2))"
  "(1 3 4 5)
(3 4 5)
(1 3 4 5)
((1 3 4 5) (3 4 5))
((1 3 4 5) (3 4 5))
(3 4 5 9)
(3 4 5 9)
(1 2)
(4)
(b d f h)
(john george)
(butter eggs flour mushrooms onions salt sugar)
(spam eggs sausage)
(#f (or not))
\"nonfat\"
#(0 1 2 3 4 5 6 7 8)
\"low-calorie\"
(0 1 2 3 4 5 6 7 8)
\"I like oatmeal.\"
((boom bam bim) (bim bam boom) (boom bam bim))
((1 1 3 4 5 9) (3 1 4 1 5 9) (1 1 3 4 5 9) #(3 2 1))
((c . 0) (b . 1) (a . 1) (d . 1))
(a b c china)
#(one 2 three)
6
4
")

 ;; Chapter 12: lists
 (lists
  "(print (list (cons 1 2) (cons 1 '(2 3 4 5)) (list 1 2 3) (list (+ 4 3) (- 4 3))))
   (print (list* 1 2 3 '(4 5 6)))
   (print (list (car '(4 5 6)) (car '()) (cdr '(4 5 6)) (cdr '())))
   (define x (list 4 5 6))
   (print (set! (car x) 9))
   (print (set! (cdr x) '(a b c)))
   (print x)
   (print (list (caddr '(1 2 3)) (cadr '(1 2 3)) (cddr '(1 2 3)) (caar '((1) 2))))
   (define y (list 1 2 3))
   (set! (caddr y) 'three)
   (print y)
   (print (append '(1 2) '(3) '() '(4 5)))
   (print (find-pair 'b '((a . 1) (b . 2))))
   (print (member? 2 '(1 2 3)))
   (define circular (list 1 2 3))
   (set! (cdr (cddr circular)) circular)
   (print (list (size circular) (size '(1 2 3))))
   (print (make <list> size: 3 fill: 'x))
   (print (list (element '(a b c) 1) (element '(a b c) 5 default: 'none)))"
  "((1 . 2) (1 2 3 4 5) (1 2 3) (7 1))
(1 2 3 4 5 6)
(4 () (5 6) ())
9
(a b c)
(9 a b c)
(3 2 (3) 1)
(1 2 three)
(1 2 3 4 5)
(b . 2)
(2 3)
(#f 3)
(x x x)
(b none)
")

 ;; Chapter 12: vectors, strings, arrays
 (vectors-strings-arrays
  "(define v (make <vector> size: 3 fill: 0))
   (set! (element v 1) 'b)
   (print v)
   (print (list (aref #(7 8 9) 1) (element #(7 8 9) 5 default: 'none)))
   (define w (vector 7 8 9))
   (set! (aref w 1) 5)
   (print w)
   (print (dimensions (make <array> dimensions: '(4 4))))
   (define a (make <array> dimensions: '(2 3) fill: 0))
   (set! (aref a 1 2) 'x)
   (print (list (aref a 1 2) (aref a 0 0) (size a) (element a '(1 2))))
   (define s \"Van Gogh\")
   (print (list (as-lowercase s) (as-uppercase s) s))
   (define t2 (copy-sequence \"Van Gogh\"))
   (as-uppercase! t2)
   (print t2)
   (print (make <string> size: 3 fill: #\\z))
   (print (list (element \"abc\" 1) (size \"\") (reverse \"abc\") (as <list> \"ab\")))
   (display \"display: no quotes\")
   (newline)
   (write #\\a) (write #\\space) (display #\\b) (newline)"
  "#(0 b 0)
(8 none)
#(7 5 9)
(4 4)
(x 0 6 x)
(\"van gogh\" \"VAN GOGH\" \"Van Gogh\")
\"VAN GOGH\"
\"zzz\"
(#\\b 0 \"cba\" (#\\a #\\b))
display: no quotes
#\\a#\\spaceb
")

 ;; Chapter 12: stretchy vectors, deques, ranges, tables
 (more-collections
  "(define sv (make <stretchy-vector>))
   (dotimes (i 20) (add! sv (* i i)))
   (print (list (size sv) (element sv 19) (last sv)))
   (remove! sv 4)
   (print (list (size sv) (element sv 2)))
   (set! (size sv) 3)
   (print sv)
   (define d (make <deque>))
   (push d 2) (push d 1) (push-last d 3) (push-last d 4)
   (print d)
   (print (list (pop d) (pop-last d) (size d)))
   (dotimes (i 20) (push-last d i))
   (print (list (size d) (element d 0) (element d 21)))
   (print (as <list> (range from: 1 up-to: 5)))
   (print (as <list> (range from: 10 through: 1 by: -3)))
   (print (list (size (range)) (size (range up-to: 10 by: 3)) (member? 1000001 (range from: 1 by: 2))
                (member? 1000000 (range from: 1 by: 2))))
   (print (as <list> (reverse (range from: 1 through: 4))))
   (print (as <list> (copy-sequence (range from: 0 by: 5) start: 2 end: 5)))
   (print (as <list> (intersection (range from: 0 up-to: 20 by: 2) (range from: 0 by: 3))))
   (print (= (range from: 1 up-to: 4) (range from: 1 through: 3)))
   (define tb (make <table>))
   (set! (element tb 'apple) 3)
   (set! (element tb \"pear\") 5)
   (set! (element tb 2) 'two)
   (print (list (element tb 'apple) (element tb (concatenate \"pe\" \"ar\")) (element tb 2.0)
                (element tb 'none default: 0) (size tb)))
   (remove-key! tb 'apple)
   (print (list (size tb) (element tb 'apple default: 'gone)))
   (dotimes (i 100) (set! (element tb i) (* i i)))
   (print (list (size tb) (element tb 99)))
   (print (reduce + 0 (map-as <list> (method (k) (if (instance? k <integer>) k 0)) (key-sequence tb))))"
  "(20 361 361)
(19 9)
{<stretchy-vector> 0 1 9}
{<deque> 1 2 3 4}
(1 4 2)
(22 2 19)
(1 2 3 4)
(10 7 4 1)
(#f 4 #t #f)
(4 3 2 1)
(10 15 20)
(0 6 12 18)
#t
(3 5 two 0 3)
(2 gone)
(101 9801)
4950
")

 ;; Printing
 (printing
  "(print (list 1 'sym key: \"str\" #\\c 1.5 1/3 #t #f '() #(1 2) '(a . b) ''quoted))
   (print <integer>)
   (print print)
   (print (make <table>))
   (print (singleton 3))
   (print (list 1.0e20 -0.1 100.0 1.5s0))
   (write-line \"line\")"
  "(1 sym key: \"str\" #\\c 1.5 1/3 #t #f () #(1 2) (a . b) (quote quoted))
{the class <integer>}
{the generic function print}
{table}
{singleton 3}
(1.0e20 -0.1 100.0 1.5s0)
\"line\"
")

 ;; Programs extend the library's generic functions.
 (extending
  "(define-class <money> (<object>) (cents init-keyword: cents:))
   (define-method binary+ ((a <money>) (b <money>)) (make <money> cents: (+ (cents a) (cents b))))
   (define-method binary= ((a <money>) (b <money>)) (= (cents a) (cents b)))
   (define-method binary< ((a <money>) (b <money>)) (< (cents a) (cents b)))
   (define-method =hash ((a <money>)) (cents a))
   (define-method print-object ((a <money>) stream escape?)
     (display \"$\" stream: stream) (display (cents a) stream: stream))
   (define a (make <money> cents: 150))
   (define b (make <money> cents: 250))
   (print (+ a b))
   (print (+ a b a))
   (print (list (= a (make <money> cents: 150)) (< a b) (max a b) (sort (list b a))))
   (define t (make <table>))
   (set! (element t a) 'found)
   (print (element t (make <money> cents: 150)))
   (define-class <countdown> (<sequence>) (from init-keyword: from:))
   (define-method initial-state ((c <countdown>)) (if (zero? (from c)) #f (from c)))
   (define-method next-state ((c <countdown>) state) (if (= state 1) #f (- state 1)))
   (define-method current-element ((c <countdown>) state) state)
   (define c (make <countdown> from: 4))
   (print (list (size c) (map-as <list> identity c) (element c 1) (reduce + 0 c)))
   (for-each ((x c) (y '(a b c d e))) () (display x) (display y))
   (newline)"
  "$400
$550
(#t #t $250 ($150 $250))
found
(4 (4 3 2 1) 3 10)
4a3b2c1d
")
 ;; slot-initialized? (chapter 6), shallow-copy, and what a static program
 ;; cannot do.
 (slots-and-copies
  "(define-class <p> (<object>) (x init-keyword: x:) (y init-value: 1) (z allocation: class)
     (v allocation: virtual))
   (define a (make <p>))
   (print (list (slot-initialized? a x) (slot-initialized? a y) (slot-initialized? a z)
                (slot-initialized? a v)))
   (set! (x a) 5)
   (print (slot-initialized? a x))
   (define b (shallow-copy a))
   (set! (x b) 6)
   (print (list (x a) (x b) (shallow-copy '(1 2)) (magnitude -3)))
   (define-method initialize ((p <p>) #key) (set! (y p) 99))
   (print (y (make <p>)))"
  "(#f #t #f #t)
#t
(5 6 (1 2) 3)
99
")
 (static-program
  "(add-method print (method (x) x))"
  ""
  :compile-error "add-method cannot be used"
  :compiler-only t)
 ;; Chapter 11: slot descriptors.  Chapter 10: function-arguments.
 (slot-descriptors
  "(define-class <p> (<object>) (x init-keyword: x: type: <integer>) (y init-value: 7 allocation: class)
     (v allocation: virtual))
   (define-class <q> (<p>) (x init-keyword: x: init-value: 1 type: <integer>) z)
   (define a (make <q> x: 5))
   (define sd (slot-descriptor a x))
   (print (list sd (slot-getter sd) (slot-setter sd) (slot-type sd) (slot-allocation sd)
                (init-keyword sd)))
   (bind ((v has (init-value sd))) (print (list v has)))
   (print (list (slot-value a sd) (init-function sd)))
   (set! (slot-value a sd) 9)
   (print (x a))
   (print (slot-descriptors <q>))
   (print (list (slot-allocation (slot-descriptor a y)) (slot-descriptor a print)
                (slot-type (slot-descriptor a z))))
   (bind ((r rest keys (function-arguments (method (a b #rest c #key d) a)))) (print (list r rest keys)))
   (bind ((r rest keys (function-arguments x))) (print (list r rest keys)))
   (bind ((r rest keys (function-arguments (method (#key) 1)))) (print (list r rest keys)))
   (set! (slot-value a sd) 'bad)"
  "({slot x} {the generic function x} {the generic function (setter x)} {the class <integer>} instance x:)
(1 #t)
(5 #f)
9
#({slot x} {slot y} {slot v} {slot z})
(class #f {the class <object>})
(2 #t (d:))
(1 #f #f)
(0 #f ())
"
  :error "bad is not an instance of")
 ;; Streams and format (an extension: the manual uses but does not define
 ;; them)
 (streams
  "(format #t \"~a and ~s~%\" \"display\" \"write\")
   (print (format #f \"<~d> ~~ ~a\" 42 'x))
   (define s (make <string-stream>))
   (print 'to-string stream: s)
   (write \"q\" stream: s)
   (format s \"~a\" 1/2)
   (print (stream-contents s))
   (display \"to standard error\" stream: *standard-error*)
   (newline stream: *standard-error*)
   (print (list (instance? *standard-output* <stream>) (instance? s <string-stream>)))"
  "display and \"write\"
\"<42> ~ x\"
\"to-string
\\\"q\\\"1/2\"
(#t #t)
")
 ;; The intersection of unbounded ranges
 (range-intersections
  "(define first5 (method (r) (as <list> (copy-sequence r end: 5))))
   (print (first5 (intersection (range from: 0 by: 4) (range from: 2 by: 6))))
   (print (empty? (intersection (range from: 1 by: 2) (range from: 0 by: 2))))
   (print (first5 (intersection (range from: 10 by: -3) (range from: 20 by: -5))))
   (print (as <list> (intersection (range from: 0 by: 2) (range from: 10 by: -3))))
   (print (first5 (intersection (range from: 1/2 by: 1/2) (range from: 0 by: 1/3))))
   (print (first5 (intersection (range from: 100 by: 7) (range from: 3 by: 5))))"
  "(8 20 32 44 56)
#t
(10 -5 -20 -35 -50)
(4 10)
(1 2 3 4 5)
(128 163 198 233 268)
")
 ;; pp: as print, laid out over lines of 79 columns; an array prints as
 ;; <array> and its elements.  The Common Lisp implementation prints the same.
 (pretty-printing
  "(pp '(define-method foo ((x <integer>))
          (bind ((y (* x x)) (z (+ x x x x x x x x x x x x x x x x x x x x)))
            (list y z y z y z y z y z y z y z y z y z))))
   (pp '(foo: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb ccccccccc))
   (pp (vector '(aaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbbbb) '(cccccccccccccccccccccccccccc dddddddddddddddddddddddddd eeeeeeeeeee) 3))
   (pp '(a \"b\" 'c))
   (print (make <array> dimensions: '(2 2) fill: 0))"
  "(define-method foo ((x <integer>))
  (bind ((y (* x x)) (z (+ x x x x x x x x x x x x x x x x x x x x)))
    (list y z y z y z y z y z y z y z y z y z)))
(foo:
 aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
 bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
 ccccccccc)
#((aaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbbbb)
  (cccccccccccccccccccccccccccc dddddddddddddddddddddddddd eeeeeeeeeee)
  3)
(a \"b\" (quote c))
{<array> 0 0 0 0}
")

 ;; Keys that are sequences of symbols, strings and characters, whose
 ;; hashes are large: hashing them must not overflow.
 (table-sequence-keys
  "(define t (make <table>))
   (set! (element t (list 'x 'y)) 1)
   (set! (element t (list \"ab\" #\\c 'zz 'yy 'xx 'ww 'vv 'uu 'tt)) 2)
   (set! (element t (vector 'p 'q 'r)) 3)
   (print (list (element t (list 'x 'y))
                (element t (list \"ab\" #\\c 'zz 'yy 'xx 'ww 'vv 'uu 'tt))
                (element t (vector 'p 'q 'r))))"
  "(1 2 3)
")

 ;; Chapter 12, "Collection Alignment": collections that are not all
 ;; sequences are aligned by their common keys, compared with =, so that
 ;; the keys of arrays, and of tables keyed by lists or strings, match.
 ;; Mapping into a table stores each result under its key.
 (aligned-keys
  "(define a (make <array> dimensions: '(2 2) fill: 1))
   (define b (make <array> dimensions: '(2 2) fill: 10))
   (print (every? > a b))
   (print (any? < a b))
   (print (as <list> (map + a b)))
   (define n 0)
   (do (method (x y) (set! n (+ n x y))) a b)
   (print n)
   (define t1 (make <table>)) (define t2 (make <table>))
   (set! (element t1 (list 1)) 1) (set! (element t2 (list 1)) 2)
   (set! (element t1 \"k\") 10) (set! (element t2 \"k\") 20)
   (print (sort (as <list> (map + t1 t2))))
   (define t3 (make <table>))
   (map-into t3 + t1 t2)
   (print (list (element t3 (list 1) default: 'none) (element t3 \"k\" default: 'none)))
   (define t4 (make <table>))
   (set! (element t4 0) 'a) (set! (element t4 5) 'b)
   (print (sort (as <list> (map (method (x i) i) t4 (range from: 0)))))"
  "#f
#t
(11 11 11 11)
44
(3 30)
(3 30)
(0 5)
")

 ;; Text goes out as UTF-8, a byte string's characters beyond ASCII too;
 ;; a directive format does not know is written as it is.
 (text-output
  "(format #t \"~a|~s|~a~%\" \"café\" \"café\" #\\é)
   (print #\\é)
   (format #t \"[~X] [~Q]~%\")"
  "café|\"café\"|é
#\\é
[~X] [~Q]
")

 ;; In a condition's message, ~a writes a string's or a character's own
 ;; text, and ~s writes it as print does.
 (text-in-messages
  "(error \"x ~a ~a ~s ~s ~s\" (as <character> 955) (as <unicode-string> \"abc\") #\\é #\\newline \"q\\\"r\")"
  ""
  :error "x λ abc #\\é #\\newline \"q\\\"r\"")

 ;; A name with a ~ in it is not a directive.
 (tilde-in-names
  "(define-generic-function frob~a~s (x)) (frob~a~s 'q)"
  ""
  :error "no applicable method for frob~a~s")

 ;; Chapter 15: symbols and keywords whose names have characters beyond
 ;; Latin-1, and case ignored beyond ASCII -- but only between the cases
 ;; of a pair, so the Kelvin sign is not a K.
 (unicode-names
  "(format #t \"~s ~s~%\" 'λ (as <string> 'λ))
   (print (list 'café 'Straße λ: (as <keyword> \"ζeta\")))
   (define-class <ζ> (<object>))
   (print <ζ>)
   (print (make <ζ>))
   (define-generic-function ξ (x))
   (print ξ)
   (print (id? (as <symbol> \"É\") (as <symbol> \"é\")))
   (print (id? 'Ωmega 'ωMEGA))
   (print (id? (as <symbol> \"λ\") 'λ))
   (print (id? (as <symbol> \"K\") (as <symbol> \"K\")))
   (define λvar 7)
   (print (+ λvar 1))
   (ξ 1)"
  "λ \"λ\"
(café Straße λ: ζeta:)
{the class <ζ>}
{instance of <ζ>}
{the generic function ξ}
#t
#t
#t
#f
8
"
  :error "no applicable method for ξ")

 ;; Chapter 16: as-uppercase and as-lowercase change a character between
 ;; the two cases of a pair that map to each other, as Common Lisp's
 ;; char-upcase and char-downcase do: not the micro sign, the dotless i,
 ;; the long s, the final sigma, the Ohm or the Kelvin sign.
 (character-case
  "(define codes '(65 97 201 233 255 376 181 305 383 962 963 931 453 452 454 7838 223 8486 8490 304 65313))
   (for-each ((c codes)) ()
     (bind ((ch (as <character> c)))
       (format #t \"~d ~d ~d~%\" c (as <integer> (as-uppercase ch)) (as <integer> (as-lowercase ch)))))"
  "65 65 97
97 65 97
201 201 233
233 201 233
255 376 255
376 376 255
181 181 181
305 305 305
383 383 383
962 962 962
963 931 963
931 931 963
453 452 454
452 452 454
454 452 454
7838 7838 7838
223 223 223
8486 8486 8486
8490 8490 8490
304 304 304
65313 65313 65345
")

 ;; Floats print with the fewest digits that read back, in fixed notation
 ;; from 1e-5 up to 1e16, and with an exponent outside; a single-float as
 ;; a single-float's digits; a tie between two shortest goes up.
 (float-printing
  "(for-each ((x (list 1.2345678901234567e-5 0.012345678901234568 12345678901234568.0
                       (as <single-float> 1.2345678e10) 0.0 -0.0 1.0 -1.5 0.1 100.0 1.0e16
                       1.0e-5 9.99e-6 123.456 1.0e21 1.5e-7 1.7976931348623157e308
                       1.5s0 -0.0s0 0.1s0 1.0s10 3.4028235s38 12345.678s0 1.0s-5
                       0.0025 6.02214076e23 1125899906842624.25 1228074218604401.25)))
             ()
     (print x))"
  "0.000012345678901234568
0.012345678901234568
1.2345678901234568e16
12345678000.0s0
0.0
-0.0
1.0
-1.5
0.1
100.0
1.0e16
0.00001
9.99e-6
123.456
1.0e21
1.5e-7
1.7976931348623157e308
1.5s0
-0.0s0
0.1s0
10000000000.0s0
3.4028235s38
12345.678s0
1.0s-5
0.0025
6.02214076e23
1125899906842624.3
1228074218604401.3
")

 ;; Chapter 14: rationalize, as Common Lisp's: the simplest rational
 ;; within the float's precision.
 (rationalize
  "(for-each ((x (list 0.5 0.1 -0.1 0.3333333333333333 831.1704033985734 1.5s0 0.1s0 3.14159s0
                       2.0 -7.0 1.0e10 4.611686018427387e18 1.0e-10 123.456 0.0 1.0e-15
                       2.718281828459045)))
             ()
     (print (rationalize x)))"
  "1/2
1/10
-1/10
1/3
3927118909/4724806
3/2
1/10
9563/3044
2
-7
10000000000
4611686018427386880
1/10000000000
15432/125
0
1/1000000000000000
268876667/98914198
")

 ;; Beyond 63 bits, an error, not a wrong answer
 (rationalize-overflow
  "(print (rationalize 4.611686018427387904e18))"
  ""
  :error "integer overflow"
  :compiler-only t)

 (rationalize-overflow-small
  "(print (rationalize 1.0e-20))"
  ""
  :error "integer overflow"
  :compiler-only t)

 ;; Chapter 14, float contagion as in Common Lisp: a rational is converted
 ;; to the float's format, rounded once, and the operation is done in that
 ;; format.  floor/ and the rest divide in it, and take the remainder by
 ;; the same contagion.
 (float-contagion
  "(print (- 1/3 1.0s0))
   (print (+ 1.0s0 16777217))
   (print (+ 2/3 0.5s0))
   (print (as <double-float> (/ (+ (ash 1 61) 255) (- (ash 1 61) 1))))
   (print (as <single-float> (+ (ash 1 60) (ash 1 36) 1)))
   (print (as <single-float> 1/3))
   (print (/ 2/3 3.0s0))
   (bind ((q r (floor/ 7/3 0.5s0))) (print (list q r)))
   (bind ((q r (floor/ -0.0 1))) (print (list q r)))
   (bind ((q r (round/ 2.5 1))) (print (list q r)))
   (bind ((q r (truncate/ -7.5s0 2))) (print (list q r)))
   (print (modulo 7/3 0.5s0))"
  "-0.6666666s0
16777216.0s0
1.1666667s0
1.0000000000000002
1.1529216s18
0.33333334s0
0.22222222s0
(4 0.33333325s0)
(0 -0.0)
(2 0.5)
(-3 -1.5s0)
0.33333325s0
")

 ;; integral?, numerator and denominator of floats beyond 63-bit integers
 (large-floats
  "(print (integral? 1.0e20))
   (print (integral? 2.5))
   (print (integral? -4503599627370495.5))
   (print (integral? 1.0e300))
   (print (numerator 1.0e20))
   (print (denominator 1.0e20))
   (print (numerator 1.0s30))
   (print (denominator 0.75s0))
   (print (handler-case (denominator 1.0e-310) ((<error>) 'error)))"
  "#t
#f
#f
#t
1.0e20
1.0
1.0s30
4.0s0
error
")

 ;; A size whose bytes wrap around is an error, not a write past the end
 (huge-unicode-string
  "(print (handler-case (make <unicode-string> size: 4611686018427387903 fill: #\\a)
            ((<error>) 'error)))
   (print (size (make <unicode-string> size: 3 fill: #\\a)))"
  "error
3
"
  :compiler-only t)

 ;; Chapter 15's example: a symbol keeps the case it was first spelled
 ;; in, by as or by the program -- a form spells its symbols when it is
 ;; read, after the forms before it have run.
 (symbol-first-spelling
  "(print (as <symbol> \"foo\"))
   (print (id? 'FOO (as <symbol> \"Foo\")))
   (print 'Foo)
   (print (as <keyword> \"foo\"))
   (define Bar 1)
   (print (as <symbol> \"BAR\"))
   (define-method g () 'BAZ)
   (print (as <symbol> \"baz\"))
   (print (as <symbol> \"quux\"))
   (print 'QUUX)
   (print (as <keyword> \"Zip\"))
   (print zip:)"
  "foo
#t
foo
foo:
Bar
BAZ
quux
quux
Zip:
Zip:
")

 ;; Chapter 9: collections of the same size whose keys differ are not =
 (collection-equality
  "(define x (make <table>)) (define y (make <table>))
   (set! (element x 1) 'a) (set! (element y 2) 'a)
   (print (= x y))
   (set! (element y 1) 'a) (set! (element x 2) 'a)
   (print (= x y))
   (define a1 (make <array> dimensions: '(2 2) fill: 0))
   (define a2 (make <array> dimensions: '(2 2) fill: 0))
   (print (= a1 a2))
   (set! (aref a2 1 1) 5)
   (print (= a1 a2))
   (print (= a1 (make <array> dimensions: '(1 4) fill: 0)))"
  "#f
#t
#t
#f
#f
")

 ;; Chapter 9: objects that are = hash alike -- a string and a vector of
 ;; its characters, "" and (), tables made in different orders, equal
 ;; arrays, a range and a list of its elements.
 (hash-agrees-with-equal
  "(define t (make <table>))
   (set! (element t \"a\") 1)
   (print (element t (vector #\\a) default: 'none))
   (print (element t (list #\\a) default: 'none))
   (set! (element t \"\") 2)
   (print (element t '() default: 'none))
   (define t1 (make <table>)) (set! (element t1 'x) 1) (set! (element t1 'y) 2)
   (define t2 (make <table>)) (set! (element t2 'y) 2) (set! (element t2 'x) 1)
   (set! (element t t1) 3)
   (print (element t t2 default: 'none))
   (set! (element t (make <array> dimensions: '(2 2) fill: 7)) 4)
   (print (element t (make <array> dimensions: '(2 2) fill: 7) default: 'none))
   (set! (element t (range from: 0 up-to: 3)) 5)
   (print (element t (list 0 1 2) default: 'none))
   (set! (element t (range from: 3)) 6)
   (print (element t (range from: 3) default: 'none))
   (print (= (=hash \"ab\") (=hash (vector #\\a #\\b))))"
  "1
1
2
3
4
5
6
#t
")

 ;; Chapter 12 and design note 18: member? on a range tests with id?
 ;; unless given a test, and terminates on an unbounded range with id? or
 ;; =; intersection of ranges is a range, run the way the first runs, of
 ;; the points both share; any other test intersects them as sequences.
 (range-membership
  "(print (member? 1.0 (range from: 0 up-to: 10)))
   (print (member? 1.0 (range from: 0 up-to: 10) test: =))
   (print (member? 4 (range from: 0) test: id?))
   (print (member? 4 (range from: 0)))
   (print (member? 1/2 (range from: 0 by: 1/4)))
   (print (member? 0.5 (range from: 0.0 by: 0.25 size: 4)))
   (print (member? 'a (range from: 0)))
   (print (member? 3 (range from: 0 by: 0 size: 2)))
   (print (member? 0 (range from: 0 by: 0 size: 2)))
   (print (member? 7 (range from: 10 by: -1 size: 5)))
   (print (member? 4 (range from: 0 up-to: 10) test: (method (a b) (= a (* 2 b)))))
   (print (intersection (range from: -5 up-to: 10) (range from: 1.0 up-to: 15.0)))
   (print (intersection (range from: -5 up-to: 10) (range from: 1 up-to: 15)))
   (print (intersection (range from: 0 by: 2) (range from: 0 by: 3)))
   (print (intersection (range from: 0 by: 2 size: 10) (range from: 0 by: 3)))
   (print (intersection (range from: 20 by: -2) (range from: 0 by: 3 size: 10)))
   (print (intersection (range from: 0 up-to: 10) (range from: 0 up-to: 10) test: =))
   (print (intersection (range from: 0 up-to: 5) (range from: 0.0 up-to: 5.0) test: =))
   (print (intersection (range from: 5 by: 0 size: 3) (range from: 0 up-to: 10)))
   (print (intersection (range from: 0 up-to: 10) (range from: 5 by: 0 size: 3)))
   (print (intersection (range from: 0 by: 2 size: 3) (range from: 4 by: 3 size: 3)))
   (print (intersection (range from: 0 by: 1/2) (range from: 1/3 by: 1/3 size: 20)))
   (print (intersection (range from: 10 by: -3) (range from: 0 by: 2)))
   (print (intersection (range from: 0 by: 2) (range from: 1 by: 2)))
   (print (intersection (range from: 0.0 up-to: 3.0) (range from: 1.0 up-to: 5.0)))
   (print (as <list> (intersection (range from: 0 up-to: 10) (range from: 0 up-to: 10) test: (method (a b) (= a (* 2 b))))))"
  "#f
#t
#t
#t
#t
#t
#f
#f
#t
#t
#t
{range 0 by 1 size 0}
{range 1 by 1 size 9}
{range 0 by 6}
{range 0 by 6 size 4}
{range 18 by -6 size 4}
{range 0 by 1 size 10}
{range 0 by 1 size 5}
{range 5 by 0 size 3}
{range 5 by 1 size 1}
{range 4 by 6 size 1}
{range 1 by 1 size 6}
{range 10 by -6 size 2}
{range 0 by 1 size 0}
{range 1.0 by 1.0 size 2}
(0 2 4 6 8)
")

 ;; A collection of numbers in steps as a range
 (as-range
  "(print (as <range> '(1 3 5)))
   (print (as <range> (vector 2.0 2.5)))
   (print (as <range> '()))
   (print (as <range> '(4)))"
  "{range 1 by 2 size 3}
{range 2.0 by 0.5 size 2}
{range 0 by 1 size 0}
{range 4 by 1 size 1}
")

 ;; The intersection of a sequence and an unbounded range ends
 (intersection-with-unbounded-range
  "(print (intersection '(1 -9) (range from: 0)))
   (print (intersection '(1 -9 4) (range from: 10 by: -3)))
   (print (intersection '(1 2 3) '(2.0 3) test: =))"
  "(1)
(1 4)
(2 3)
")

 ;; Chapter 12: concatenate-as needs a subclass of <mutable-sequence>
 (concatenate-as-class
  "(print (handler-case (concatenate-as <table> '(a b c)) ((<error>) 'error)))
   (print (handler-case (concatenate-as <range> '(1 2)) ((<error>) 'error)))
   (print (concatenate-as <string> '(#\\n #\\o #\\n) '(#\\f #\\a #\\t)))
   (print (concatenate-as <vector> '(0 1 2) '(3 4 5) '(6 7 8)))
   (print (concatenate-as <list> #(1) '(2)))
   (print (concatenate-as <deque> #(1) '(2)))"
  "error
error
\"nonfat\"
#(0 1 2 3 4 5 6 7 8)
(1 2)
{<deque> 1 2}
")

 ;; A deque's or stretchy vector's size: must be a non-negative integer
 (stretchy-sizes
  "(print (handler-case (make <deque> size: -1) ((<error>) 'error)))
   (print (handler-case (make <stretchy-vector> size: -1) ((<error>) 'error)))
   (print (handler-case (make <deque> size: 1.5) ((<error>) 'error)))
   (print (handler-case (make <stretchy-vector> size: 'a) ((<error>) 'error)))
   (print (make <deque> size: 2 fill: 'x))
   (print (make <stretchy-vector> size: 0))"
  "error
error
error
error
{<deque> x x}
{<stretchy-vector>}
")

 ;; copy-sequence of a range: start: and end: are indexes within it
 (range-copy-bounds
  "(define r (range from: 0 up-to: 5))
   (print (handler-case (copy-sequence r end: 10) ((<error>) 'error)))
   (print (handler-case (copy-sequence r start: -1) ((<error>) 'error)))
   (print (handler-case (copy-sequence r start: 6) ((<error>) 'error)))
   (print (handler-case (copy-sequence r start: 3 end: 2) ((<error>) 'error)))
   (print (copy-sequence r start: 5))
   (print (copy-sequence r start: 1 end: 3))
   (print (copy-sequence r end: 5))
   (print (copy-sequence (range from: 0) start: 2 end: 4))
   (print (copy-sequence (range from: 0) start: 2))"
  "error
error
error
error
{range 5 by 1 size 0}
{range 1 by 1 size 2}
{range 0 by 1 size 5}
{range 2 by 1 size 2}
{range 2 by 1}
")

 ;; replace-subsequence! with too little room changes nothing
 (replace-subsequence-room
  "(define v (vector 1 2 3))
   (print (handler-case (replace-subsequence! v '(a b c) start: 1) ((<error>) 'error)))
   (print v)
   (define phrase \"I hate oatmeal.\")
   (print (replace-subsequence! phrase \"like\" start: 2))
   (define w (vector 1 2 3 4))
   (print (replace-subsequence! w w start: 0))
   (print (replace-subsequence! (list 1 2 3) '() start: 3))"
  "error
#(1 2 3)
\"I like oatmeal.\"
#(1 2 3 4)
(1 2 3)
")

 ;; Chapter 12: a collection stable under iteration gives its keys in its
 ;; natural order; an array's key is a vector of its indexes.
 (key-sequence-order
  "(define a (make <array> dimensions: '(2 2) fill: 0))
   (print (key-sequence a))
   (define t (make <table>))
   (set! (element t 'only) 1)
   (print (key-sequence t))
   (print (element a (list 1 0)))
   (print (element a (vector 1 0)))
   (define b (make <array> dimensions: '(2 3) fill: 1))
   (print (key-sequence b))
   (print (as <list> (map + b b)))
   (do (method (k) (set! (element b k) (reduce + 0 k))) (key-sequence b))
   (print b)"
  "(#(0 0) #(0 1) #(1 0) #(1 1))
(only)
0
0
(#(0 0) #(0 1) #(0 2) #(1 0) #(1 1) #(1 2))
(2 2 2 2 2 2)
{<array> 0 1 2 1 2 3}
")

 ;; find-key calls its procedure once on each element it looks at
 (find-key-calls
  "(define calls 0)
   (define p (method (x) (set! calls (+ calls 1)) (= x 3)))
   (print (find-key '(1 2 3 4) p))
   (print calls)
   (set! calls 0)
   (print (find-key '(3 1 3 4 3) p skip: 1))
   (print calls)
   (set! calls 0)
   (print (find-key '(1 2) p failure: 'none))
   (print calls)
   (define flavors '(chocolate pistachio pumpkin))
   (print (find-key flavors (method (flavor) (id? flavor 'pistachio))))
   (define t (make <table>)) (set! (element t 'k) 3)
   (print (find-key t (method (v) (= v 3))))"
  "2
3
2
3
none
2
1
k
")

 ;; Chapter 14, as Common Lisp: a float result too large for its format is
 ;; an error, floating-point overflow, and so is an invalid operation;
 ;; inline arithmetic on doubles as well.
 (float-overflow
  "(define err (method (thunk) (handler-case (thunk) ((<error>) 'error))))
   (print (err (method () (* 1.0e308 10.0))))
   (print (err (method () (+ 1.0e308 1.0e308))))
   (print (err (method () (* 3.0s38 10.0s0))))
   (print (err (method () (expt 10.0 400))))
   (print (err (method () (expt 0.0 -1))))
   (print (err (method () (exp 1000.0))))
   (print (err (method () (as <single-float> 1.0e300))))
   (print (err (method () (/ 0.0 0.0))))
   (print (err (method () (/ 1.0 0.0))))
   (define-method dmul ((x <double-float>) (y <double-float>)) (* x y))
   (print (err (method () (dmul 1.0e200 1.0e200))))
   (print (* 1.0e-200 1.0e-200))
   (print (dmul 1.5 2.0))"
  "error
error
error
error
error
error
error
error
error
error
0.0
3.0
")

 ;; Chapter 9: a float and its negative zero are = but not id?; abs, as
 ;; Common Lisp's, makes -0.0 0.0.
 (negative-zero
  "(print (id? 0.0 -0.0))
   (print (id? 0.0 0.0))
   (print (id? -0.0 -0.0))
   (print (id? 1.5 1.5))
   (print (id? 0.0s0 -0.0s0))
   (print (= 0.0 -0.0))
   (define-method z ((x (singleton 0.0))) 'zero)
   (define-method z (x) 'other)
   (print (list (z 0.0) (z -0.0)))
   (print (list (abs -0.0) (abs 0.0) (abs -0.0s0) (abs -3) (abs 0) (abs -1/2) (abs -2.5)))
   (bind ((q r (floor/ -0.0 1))) (print (list q r)))
   (print (case -0.0 ((0.0) 'pos) ((-0.0) 'neg) (else: 'none)))
   (print (member? -0.0 (list 0.0)))"
  "#f
#t
#t
#t
#f
#t
(zero other)
(0.0 0.0 0.0s0 3 0 1/2 2.5)
(0 -0.0)
neg
#f
")

 ;; Chapter 10: function-arguments of a generic function made with make,
 ;; and of the predefined functions written in C
 (function-arguments-made
  "(define fa (method (f) (bind ((r rest keys (function-arguments f))) (list r rest keys))))
   (print (fa (make <generic-function> required: 2 rest?: #t)))
   (print (fa (make <generic-function> required: 1 key?: #t)))
   (print (fa (make <generic-function> required: 1 key?: '(a: b:))))
   (print (fa (make <generic-function> required: 1)))
   (for-each ((f (list id? not identity list cons car cdr vector values apply singleton)))
     () (print (fa f)))"
  "(2 #t #f)
(1 #f #())
(1 #f (a: b:))
(1 #f #f)
(2 #t #f)
(1 #f #f)
(1 #f #f)
(0 #t #f)
(2 #f #f)
(1 #f #f)
(1 #f #f)
(0 #t #f)
(0 #t #f)
(1 #t #f)
(1 #f #f)
")

 ;; Chapter 14: angle, as Common Lisp's phase -- a float, of the
 ;; argument's format
 (angle
  "(print (list (angle 1) (angle -1) (angle 1/2) (angle -1/2) (angle 0) (angle 2.0) (angle -2.0) (angle -1.0s0) (angle 1.0s0) (angle 0.0) (angle -0.0)))
   (print (list (magnitude -3) (magnitude -2.5s0)))"
  "(0.0 3.141592653589793 0.0 3.141592653589793 0.0 0.0 3.141592653589793 3.1415927s0 0.0s0 0.0 3.141592653589793)
(3 2.5s0)
")

 ;; Chapter 9: >, <= and the rest compare with binary< alone, with the
 ;; arguments reordered; a partial order's incomparable objects are
 ;; neither > nor <, and both <= and >=.
 (compare-by-binary-less
  "(define-class <ver> (<object>) (n init-keyword: n:))
   (define-method binary< ((a <ver>) (b <ver>)) (< (n a) (n b)))
   (define v1 (make <ver> n: 1))
   (define v2 (make <ver> n: 2))
   (print (list (<= v1 (make <ver> n: 1)) (<= v1 v2) (<= v2 v1) (> v2 v1) (> v1 v2) (> v1 (make <ver> n: 1))))
   (print (list (<= v1 v1 v2) (<= v1 v2 v1) (> v2 v1 v1) (> (make <ver> n: 3) v2 v1)))
   (define-class <set> (<object>) (bits init-keyword: bits:))
   (define-method binary< ((a <set>) (b <set>))
     (and (not (= (bits a) (bits b))) (= (logand (bits a) (bits b)) (bits a))))
   (define a (make <set> bits: 1))
   (define b (make <set> bits: 2))
   (print (list (> a b) (< a b) (<= a b) (>= a b)))
   (print (list (> 3 2 1) (> 3 3) (<= 1 1 2) (<= 2 1) (> #\\b #\\a) (<= #\\a #\\a)))"
  "(#t #t #f #t #f #f)
(#t #f #f #t)
(#f #f #t #t)
(#t #f #t #f #t #t)
")

 ;; Chapter 12: the default sequence functions work on a program's own
 ;; sequence classes through the iteration protocol, answering instances
 ;; of the class-for-copy: one the class names, or a <mutable-sequence>
 ;; of its own, made by make with size: and filled in order.
 (own-sequence-classes
  "(define-class <trio> (<sequence>) (a init-keyword: a:) (b init-keyword: b:) (c init-keyword: c:))
   (define-method initial-state ((t <trio>)) 0)
   (define-method next-state ((t <trio>) s) (if (< s 2) (+ s 1) #f))
   (define-method current-element ((t <trio>) s) (select s id? ((0) (a t)) ((1) (b t)) ((2) (c t))))
   (define-method class-for-copy ((t <trio>)) <vector>)
   (define tr (make <trio> a: 3 b: 1 c: 2))
   (print (list (reverse tr) (sort tr) (map (method (x) (* x 10)) tr) (concatenate tr '(4))))
   (print (list (copy-sequence tr) (add tr 0) (remove tr 1) (choose odd? tr) (union tr '(5))))
   (define-class <box> (<mutable-sequence>) (items init-keyword: items: init-value: (vector)))
   (define-method make ((c (singleton <box>)) #key (size 0) (fill #f))
     (next-method c items: (make <vector> size: size fill: fill)))
   (define-method initial-state ((b <box>)) (if (empty? (items b)) #f 0))
   (define-method next-state ((b <box>) s) (if (< (+ s 1) (size (items b))) (+ s 1) #f))
   (define-method current-element ((b <box>) s) (element (items b) s))
   (define-method (setter current-element) ((b <box>) s v) (set! (element (items b) s) v))
   (define-method element ((b <box>) k #key default) (element (items b) k))
   (define-method (setter element) ((b <box>) k v) (set! (element (items b) k) v))
   (define-method size ((b <box>)) (size (items b)))
   (define bx (as <box> '(3 1 2)))
   (define show (method (x) (if (instance? x <box>) (items x) (list 'not-a-box x))))
   (print (map show (list (reverse bx) (sort bx) (map (method (x) (* x 10)) bx) (concatenate bx '(4)))))
   (print (map show (list (copy-sequence bx) (add bx 0) (remove bx 1) (choose odd? bx))))"
  "(#(2 1 3) #(1 2 3) #(30 10 20) #(3 1 2 4))
(#(3 1 2) #(0 3 1 2) #(3 2) #(3 1) #(3 1 2 5))
(#(2 1 3) #(1 2 3) #(30 10 20) #(3 1 2 4))
(#(3 1 2) #(0 3 1 2) #(3 2) #(3 1))
")

 ;; A <unicode-string> holds characters above code 255 however it is
 ;; made -- make, as, concatenate, map -- and the sequence functions
 ;; answer <unicode-string>s of them.
 (wide-unicode-strings
  "(define try (method (f) (handler-case (f) ((<error>) 'error))))
   (define lam (as <character> 955))
   (print (make <unicode-string> size: 2 fill: lam))
   (print (list (concatenate \"λ\" \"λ\") (object-class (concatenate \"λ\" \"λ\"))))
   (print (try (method () (concatenate \"ab\" \"λ\"))))
   (print (as <unicode-string> (list lam #\\a)))
   (print (map identity (as <unicode-string> (list lam))))
   (print (concatenate (as <unicode-string> \"ab\") (list lam)))
   (print (try (method () (as <byte-string> (list lam)))))
   (print (map-as <unicode-string> (method (c) lam) \"ab\"))
   (print (bind ((s (make <unicode-string> size: 1))) (set! (element s 0) lam) s))
   (print (try (method () (bind ((s (make <byte-string> size: 1))) (set! (element s 0) lam) s))))
   (define u (as <unicode-string> (list #\\b lam #\\a)))
   (print (map (method (x) (list x (object-class x)))
               (list (reverse u) (add u #\\z) (copy-sequence u start: 1) (as-uppercase u)
                     (choose (method (c) #t) u) (remove u #\\a) (sort u)
                     (as <unicode-string> \"ba\"))))"
  "\"λλ\"
(\"λλ\" {the class <unicode-string>})
error
\"λa\"
\"λ\"
\"abλ\"
error
\"λλ\"
\"λ\"
error
((\"aλb\" {the class <unicode-string>}) (\"zbλa\" {the class <unicode-string>}) (\"λa\" {the class <unicode-string>}) (\"BΛA\" {the class <unicode-string>}) (\"bλa\" {the class <unicode-string>}) (\"bλ\" {the class <unicode-string>}) (\"abλ\" {the class <unicode-string>}) (\"ba\" {the class <unicode-string>}))
")

 ;; expt and atan2 of a single-float and an exact number answer a
 ;; single-float, by Common Lisp's contagion; of exact numbers alone, a
 ;; double-float, or for expt with an integer power, an exact number.
 (expt-atan2-formats
  "(define show (method (x) (list x (object-class x))))
   (print (map show (list (expt 2.0s0 2) (expt 2 2.0s0) (expt 2.0s0 1/2) (expt 4 0.5d0) (expt 2 1/2) (expt 1/2 2))))
   (print (map show (list (atan2 1.0s0 1) (atan2 1 1.0s0) (atan2 1 1) (atan2 1 1.0d0))))"
  "((4.0s0 {the class <single-float>}) (4.0s0 {the class <single-float>}) (1.4142135s0 {the class <single-float>}) (2.0 {the class <double-float>}) (1.4142135623730951 {the class <double-float>}) (1/4 {the class <ratio>}))
((0.7853982s0 {the class <single-float>}) (0.7853982s0 {the class <single-float>}) (0.7853981633974483 {the class <double-float>}) (0.7853981633974483 {the class <double-float>}))
")

 ;; Lists and arrays.  An improper list has no size: design note 16 makes
 ;; it a <type-error>, of the tail; a circular one's is #f.  find-pair
 ;; passes over an element that is not a pair.  <pair> is not
 ;; instantiable (chapter 12).  An array of no dimensions has one element.
 (lists-and-arrays
  "(define try (method (f) (handler-case (f) ((<type-error> condition: c) (list 'type-error (type-error-value c))) ((<error>) 'error))))
   (print (map (method (l) (try (method () (size l)))) (list '(1 . 2) '(1 2 . 3) '(1 2 3 . 4) '() '(1) '(1 2 3))))
   (define c (list 1 2 3))
   (set! (cdr (cddr c)) c)
   (print (size c))
   (print (list (find-pair 'b '(() (b . 2))) (find-pair 'c '((a . 1) () (b . 2)))
                (try (method () (find-pair 'c '((a . 1) 5 (b . 2)))))))
   (print (list (try (method () (make <pair>))) (try (method () (make <pair> size: 2))) (make <list> size: 2)))
   (define a (make <array> dimensions: '() fill: 7))
   (print a)
   (print (list (dimensions a) (size a) (aref a)))
   (set! (aref a) 8)
   (print (list (aref a) (as <list> a) (key-sequence a) (map (method (x) (* x 2)) a)))
   (for-each ((x a)) () (set! (aref a) (+ x 1)))
   (print (list (aref a) (empty? a) (try (method () (aref a 0))) (try (method () (element a 0)))))"
  "((type-error 2) (type-error 3) (type-error 4) 0 1 3)
#f
((b . 2) #f error)
(error error (#f #f))
{<array> 7}
(() 1 7)
(8 (8) (#()) {<array> 16})
(9 #f error error)
")

 ;; Printing: a subnormal float with the fewest digits that read back as
 ;; it; a setter's generic function by the name as first spelled
 ;; (chapter 15), however it was made.
 (subnormal-floats-and-setter-names
  "(print (list 4.9e-324 1.0e-310 -4.9e-324 2.2250738585072014e-308
               (as <single-float> 1.0e-40) (as <single-float> 1.4e-45)))
   (define-method ZZ (x) x)
   (define-method (setter ZZ) (v x) v)
   (define-class <Pt> (<object>) (Xc init-value: 1))
   (define-generic-function (setter Yy) (v x))
   (print (list ZZ (setter ZZ) (setter Xc) (setter element) (setter Yy)))"
  "(5.0e-324 1.0e-310 -5.0e-324 2.2250738585072014e-308 1.0s-40 1.0s-45)
({the generic function ZZ} {the generic function (setter ZZ)} {the generic function (setter Xc)} {the generic function (setter element)} {the generic function (setter Yy)})
")

 ;; Chapter 11: as answers an instance of the class, or signals an error;
 ;; a string's elements must be characters; a table gets the keys.
 (as-to-predefined-classes
  "(define-method try (class c)
     (handler-case (as class c)
       ((<error> condition: e)
        (apply format #f (condition-format-string e) (condition-format-arguments e)))))
   (print (try <empty-list> (vector 1 2)))
   (print (try <empty-list> (vector)))
   (print (try <pair> (vector)))
   (print (try <pair> (vector 1)))
   (print (try <string> (vector #\\a 2)))
   (print (try <unicode-string> (vector 1)))
   (print (try <string> (list #\\a #\\b)))
   (print (try <table> (vector 1 2)))
   (print (try <table> (range from: 3 up-to: 5)))
   (print (try <integer> \"abc\"))
   (print (handler-case (concatenate \"ab\" (vector 65)) ((<error>) 'error)))"
  "\"as: #(1 2) cannot be converted to {the class <empty-list>}\"
()
\"as: #() cannot be converted to {the class <pair>}\"
(1)
\"a <byte-string> holds only 8-bit characters: 2\"
\"a string holds only characters: 1\"
\"ab\"
{table 0 => 1 1 => 2}
{table 0 => 3 1 => 4}
\"as: \\\"abc\\\" cannot be converted to {the class <integer>}\"
error
")

 ;; Chapter 9: id? takes object1 object2 #rest objects, as = and < do.
 (id-needs-two-arguments
  "(define-method try (f)
     (handler-case (f 1) ((<error>) 'error)))
   (print (map try (list id? = < > <= >=)))
   (print (handler-case (id? 1) ((<error>) 'error)))
   (print (list (id? 1 1) (id? 1 1 1) (id? 1 1 2)))"
  "(error error error error error error)
error
(#t #t #f)
")

 ;; A real argument never gives a complex result: no compiled program has
 ;; complex numbers, so both signal the same error.
 (real-arguments-real-results
  "(define-method show (thunk)
     (print (handler-case (thunk)
              ((<error> condition: c)
               (apply format #f (condition-format-string c) (condition-format-arguments c))))))
   (show (method () (sqrt -4)))
   (show (method () (sqrt -0.0)))
   (show (method () (log 0)))
   (show (method () (asin 2)))
   (show (method () (acos -1.5s0)))
   (show (method () (acosh 0.5)))
   (show (method () (atanh 1)))
   (show (method () (expt -8 1/3)))
   (show (method () (expt -8 2.0)))
   (show (method () (sqrt (as <extended-float> -4))))"
  "\"sqrt: the result would be complex: -4\"
-0.0
\"log: the argument must be positive: 0\"
\"asin: the result would be complex: 2\"
\"acos: the result would be complex: -1.5s0\"
\"acosh: the result would be complex: 0.5\"
\"atanh: the argument is out of range: 1\"
\"expt: the result would be complex: -8\"
64.0
\"sqrt: the result would be complex: -4.0l0\"
")

 ;; A <byte-string> holds only 8-bit characters: a string made with a wider
 ;; one is a <unicode-string>, or, where the class must be <byte-string>,
 ;; an error.
 (wide-characters-not-in-byte-strings
  "(define wide (as <character> 300))
   (define u (as <unicode-string> (list #\\a wide)))
   (define-method show (thunk)
     (bind ((x (handler-case (thunk)
                 ((<error> condition: c)
                  (apply format #f (condition-format-string c)
                         (condition-format-arguments c))))))
       (print (list (size x) (object-class x)))))
   (show (method () (format #f \"~a\" wide)))
   (show (method () (format #f \"x~ay\" (as <character> 255))))
   (show (method ()
           (bind ((s (make <string-stream>)))
             (format s \"~a~a\" wide u)
             (stream-contents s))))
   (show (method () (as <string> (as <symbol> u))))
   (show (method () (as <string> (as <keyword> u))))
   (show (method () (as <byte-string> 'abc)))
   (show (method () (as <byte-string> (as <symbol> u))))
   (show (method () (as-uppercase (as <unicode-string> (list (as <character> 255))))))
   (show (method () (as-uppercase (as <byte-string> (list (as <character> 255))))))
   (show (method () (as-uppercase! (as <byte-string> (list (as <character> 255))))))"
  "(1 {the class <unicode-string>})
(3 {the class <byte-string>})
(3 {the class <unicode-string>})
(2 {the class <unicode-string>})
(2 {the class <unicode-string>})
(3 {the class <byte-string>})
(48 {the class <unicode-string>})
(1 {the class <unicode-string>})
(48 {the class <unicode-string>})
(48 {the class <unicode-string>})
")

 ;; An argument-count error's message holds the count, and its condition
 ;; has no format arguments.
 (argument-count-condition
  "(define-method show (thunk)
     (print (handler-case (thunk)
              ((<error> condition: c)
               (list (condition-format-string c) (condition-format-arguments c))))))
   (define-method one (x) x)
   (define-generic-function gf (a b))
   (define-method gf ((a <integer>) b) a)
   (show (method () (car 1 2)))
   (show (method () (one)))
   (show (method () (gf 1)))
   (show (method () (gf 1 2 3)))"
  "(\"wrong number of arguments (2)\" ())
(\"wrong number of arguments (0)\" ())
(\"wrong number of arguments (1)\" ())
(\"wrong number of arguments (3)\" ())
")
)
