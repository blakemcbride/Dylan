;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conformance tests for chapter 14 of the 1992 manual, "Numbers"
;;;; (manual/Dylan-1992/book.annotated/ch14.html), in the form of
;;;; tests/codegen/runner.lisp.  The chapter shows no examples, so every
;;;; expected value comes from its specification -- its own text, or the
;;;; Common Lisp (X3J13) and Revised^4 Scheme definitions it defers to --
;;;; and from the design notes it links.  Float results are compared with
;;;; = (or within a tolerance, for the transcendental functions), since the
;;;; manual fixes no printed form for them.  Complex numbers and bignums
;;;; are not supported, so nothing here needs them.

(
 ;; 14 "Numbers", the class hierarchy (figure "Number classes"): <number>
 ;; above <complex> above <real>, which has <rational> (<integer>,
 ;; <ratio>) and <float> (<single-float>, <double-float>,
 ;; <extended-float>).  From the specification (the figure).
 (ch14-class-hierarchy
  "(print (list (subclass? <complex> <number>) (subclass? <real> <complex>)
                (subclass? <rational> <real>) (subclass? <float> <real>)))
   (print (list (subclass? <integer> <rational>) (subclass? <ratio> <rational>)
                (subclass? <single-float> <float>) (subclass? <double-float> <float>)
                (subclass? <extended-float> <float>)))
   (print (list (subclass? <integer> <float>) (subclass? <float> <rational>)
                (subclass? <ratio> <integer>)))
   (print (list (instance? 3 <integer>) (instance? 1/2 <ratio>) (instance? 1/2 <rational>)
                (instance? 1.5 <float>) (instance? 1.5 <real>) (instance? 3 <number>)
                (instance? 3 <float>) (instance? 'a <number>)))"
  "(#t #t #t #t)
(#t #t #t #t #t)
(#f #f #f)
(#t #t #t #t #t #t #f #f)
")

 ;; 14 "Properties": odd?, even?, zero?, positive?, negative?, integral?
 ;; test a number for the property.  From the specification.  odd? and
 ;; even? are specified on integers only, so a float is an error.
 (ch14-properties
  "(print (list (odd? 3) (odd? 4) (odd? -3) (even? 4) (even? 3) (even? 0) (even? -2)))
   (print (list (zero? 0) (zero? 0.0) (zero? 1) (zero? 0/5) (zero? -1/2)))
   (print (list (positive? 1) (positive? 0) (positive? -1) (positive? 1/2) (positive? -0.5)))
   (print (list (negative? -1) (negative? 0) (negative? 1) (negative? -1/2) (negative? 0.5)))
   (print (list (integral? 3) (integral? 1/2) (integral? 2.0) (integral? 2.5)))
   (print (handler-case (odd? 1.5) ((<error>) 'error)))
   (print (handler-case (even? 1/2) ((<error>) 'error)))
   (print (handler-case (positive? 'a) ((<error>) 'error)))"
  "(#t #f #t #t #f #t #t)
(#t #t #f #t #f)
(#t #f #f #t #f)
(#t #f #f #t #f)
(#t #f #t #f)
error
error
error
")

 ;; 14 "Arithmetic Operations", + and *: with no arguments + returns the
 ;; integer 0 and * the integer 1; with one argument both return it; with
 ;; more they combine the arguments in pairs.  - and / with one argument
 ;; return the additive and multiplicative inverse, else combine in pairs.
 ;; From the specification; ratios are canonicalized ("Automatic Type
 ;; Conversion": rational canonicalization).
 (ch14-variadic-arithmetic
  "(print (list (+) (*) (instance? (+) <integer>) (instance? (*) <integer>)))
   (print (list (+ 5) (* 5) (+ 1/2) (* 1/2)))
   (print (list (+ 1 2) (+ 1 2 3 4) (* 2 3) (* 2 3 4)))
   (print (list (- 5) (- -5) (- 1/2) (- 10 1) (- 10 1 2 3)))
   (print (list (/ 2) (/ 1/2) (/ -4) (/ 1 2) (/ 1 2 4) (/ 12 2 3)))
   (print (list (/ 4 2) (instance? (/ 4 2) <integer>) (+ 1/2 1/2) (instance? (+ 1/2 1/2) <integer>)
                (* 2/3 3/2) (- 3/2 1/2) (+ 1/2 1/3) (* 2/3 3/4) (/ 6 4)))"
  "(0 1 #t #t)
(5 5 1/2 1/2)
(3 10 6 24)
(-5 5 -1/2 9 4)
(1/2 2 -1/4 1/2 1/8 2)
(2 #t 1 #t 1 1 5/6 1/2 3/2)
")

 ;; 14 "Arithmetic Operations": binary+, binary*, binary/, binary-,
 ;; unary- and unary/ return the sum, product, quotient, difference,
 ;; additive and multiplicative inverse.  From the specification.
 (ch14-binary-operations
  "(print (list (binary+ 2 3) (binary* 2 3) (binary/ 2 3) (binary- 2 3) (unary- 2) (unary/ 2)
                (unary/ 2/3) (unary- -1/2)))"
  "(5 6 2/3 -1 -2 1/2 3/2 1/2)
")

 ;; 14 "Arithmetic Operations": "To extend the protocol, define methods on
 ;; binary+ and binary*" (and binary-, binary/, unary-, unary/): + and
 ;; the others call them, combining the arguments in pairs.  From the
 ;; specification.
 (ch14-extend-arithmetic
  "(define-class <money> (<object>) (cents init-keyword: cents:))
   (define-method binary+ ((a <money>) (b <money>))
     (make <money> cents: (+ (cents a) (cents b))))
   (define-method binary* ((a <money>) (n <integer>))
     (make <money> cents: (* (cents a) n)))
   (define-method binary- ((a <money>) (b <money>))
     (make <money> cents: (- (cents a) (cents b))))
   (define-method binary/ ((a <money>) (n <integer>))
     (make <money> cents: (/ (cents a) n)))
   (define-method unary- ((a <money>))
     (make <money> cents: (- (cents a))))
   (define-method unary/ ((a <money>))
     (make <money> cents: (/ 1 (cents a))))
   (define m (method (c) (make <money> cents: c)))
   (print (list (cents (+ (m 1) (m 2) (m 3))) (cents (* (m 5) 2 3)) (cents (- (m 10) (m 1) (m 2)))
                (cents (/ (m 12) 2 3)) (cents (- (m 7))) (cents (/ (m 4)))
                (cents (+ (m 9))) (cents (* (m 9)))))"
  "(6 30 7 2 -7 1/4 9 9)
")

 ;; 14 "Automatic Type Conversion": Common Lisp's floating-point contagion
 ;; (a rational with a float gives a float; two floats give the wider
 ;; format), rational contagion for comparisons (a float compares with a
 ;; rational exactly, so the double 0.1 is not 1/10), and coercions in
 ;; left-to-right order.  Chapter 11, as: "Predefined methods allow
 ;; coercion between numeric types."  From the specification.
 (ch14-contagion
  "(print (list (= (+ 1 0.5) 1.5) (instance? (+ 1 0.5) <float>) (instance? (+ 1/2 0.5) <float>)
                (= (* 2 0.5) 1) (instance? (* 2 0.5) <float>) (instance? (- 1.0 1) <float>)
                (instance? (/ 1 2.0) <float>) (= (/ 1 2.0) 1/2)))
   (bind ((s (as <single-float> 1/2)) (d (as <double-float> 1/4)) (e (as <extended-float> 1/8)))
     (print (list (instance? s <single-float>) (instance? d <double-float>)
                  (instance? e <extended-float>) (= s 1/2) (= d 1/4) (= e 1/8)))
     (print (list (instance? (+ s 1) <single-float>) (instance? (+ 1 s) <single-float>)
                  (instance? (+ s d) <double-float>) (instance? (+ d s) <double-float>)
                  (instance? (* d e) <extended-float>) (instance? (- s e) <extended-float>)
                  (= (+ s d) 3/4) (= (+ d e) 3/8))))
   (print (list (= 1 1.0) (= 1/2 0.5) (= 1/10 0.1) (< 1/10 0.1) (= 0.1 1/10) (> 0.1 1/10)))
   (print (list (instance? (as <float> 1/2) <float>) (= (as <float> 1/2) 0.5)
                (bind ((x 1.5)) (id? (as <float> x) x)) (id? (as <integer> 3) 3)))"
  "(#t #t #t #t #t #t #t #t)
(#t #t #t #t #t #t)
(#t #t #t #t #t #t #t #t)
(#t #t #f #t #f #t)
(#t #t #t #t)
")

 ;; 14 floor, ceiling, round, truncate: the one-argument forms of Common
 ;; Lisp's, answering an integer and a remainder (round rounds a tie to
 ;; even).  From the specification.
 (ch14-rounding-one-argument
  "(bind ((q r (floor 5))) (print (list q r)))
   (bind ((q r (floor 7/2))) (print (list q r)))
   (bind ((q r (floor -7/2))) (print (list q r)))
   (bind ((q r (ceiling 7/2))) (print (list q r)))
   (bind ((q r (ceiling -7/2))) (print (list q r)))
   (bind ((q r (round 7/2))) (print (list q r)))
   (bind ((q r (round 5/2))) (print (list q r)))
   (bind ((q r (round -5/2))) (print (list q r)))
   (bind ((q r (round 7/3))) (print (list q r)))
   (bind ((q r (truncate 7/2))) (print (list q r)))
   (bind ((q r (truncate -7/2))) (print (list q r)))
   (bind ((q r (floor 2.5))) (print (list q (instance? q <integer>) (instance? r <float>) (= r 0.5))))
   (bind ((q r (floor -2.5))) (print (list q (instance? q <integer>) (= r 0.5))))
   (bind ((q r (ceiling 2.5))) (print (list q (= r -0.5))))
   (bind ((q r (round 2.5))) (print (list q (= r 0.5))))
   (bind ((q r (round 3.5))) (print (list q (= r -0.5))))
   (bind ((q r (round 2.75))) (print (list q (= r -0.25))))
   (bind ((q r (truncate -2.5))) (print (list q (instance? q <integer>) (= r -0.5))))"
  "(5 0)
(3 1/2)
(-4 1/2)
(4 -1/2)
(-3 -1/2)
(4 -1/2)
(2 1/2)
(-2 -1/2)
(2 1/3)
(3 1/2)
(-3 -1/2)
(2 #t #t #t)
(-3 #t #t)
(3 #t)
(2 #t)
(4 #t)
(3 #t)
(-2 #t #t)
")

 ;; 14 floor/, ceiling/, round/, truncate/: the two-argument forms of
 ;; Common Lisp's floor, ceiling, round and truncate.  From the
 ;; specification.
 (ch14-rounding-two-arguments
  "(define-method qr (f a b) (bind ((q r (f a b))) (list q r)))
   (print (list (qr floor/ 7 2) (qr floor/ -7 2) (qr floor/ 7 -2) (qr floor/ -7 -2) (qr floor/ 6 3)))
   (print (list (qr ceiling/ 7 2) (qr ceiling/ -7 2) (qr ceiling/ 7 -2) (qr ceiling/ -7 -2)))
   (print (list (qr round/ 7 2) (qr round/ 5 2) (qr round/ -5 2) (qr round/ 7 3) (qr round/ -7 3)))
   (print (list (qr truncate/ 7 2) (qr truncate/ -7 2) (qr truncate/ 7 -2) (qr truncate/ -7 -2)))
   (print (list (qr floor/ 1/2 1/3) (qr ceiling/ 1/2 1/3) (qr truncate/ -5/2 1)))
   (bind ((q r (floor/ 7.5 2))) (print (list q (instance? q <integer>) (= r 1.5))))
   (bind ((q r (truncate/ -7.5 2))) (print (list q (= r -1.5))))"
  "((3 1) (-4 1) (-4 -1) (3 -1) (2 0))
((4 -1) (-3 -1) (-3 1) (4 1))
((4 -1) (2 1) (-2 -1) (2 1) (-2 -1))
((3 1) (-3 -1) (-3 1) (3 -1))
((1 1/6) (2 -1/6) (-2 -1/2))
(3 #t #t)
(-3 #t)
")

 ;; 14 modulo, remainder: modulo returns the second value of floor/,
 ;; remainder the second value of truncate/.  From the specification.
 (ch14-modulo-remainder
  "(print (list (modulo 13 4) (modulo -13 4) (modulo 13 -4) (modulo -13 -4) (modulo 12 4)))
   (print (list (remainder 13 4) (remainder -13 4) (remainder 13 -4) (remainder -13 -4)
                (remainder 12 4)))
   (print (list (modulo 7/2 1) (remainder -7/2 1) (modulo -1/2 1/3)))
   (print (list (= (modulo -7.5 2) 0.5) (= (remainder -7.5 2) -1.5) (= (modulo 7.5 -2) -0.5)))"
  "(1 3 -3 -1 0)
(1 -1 1 -1 0)
(1/2 -1/2 1/6)
(#t #t #t)
")

 ;; Design note 24, "Divide by Zero Signals Error" (linked from 14
 ;; "Arithmetic Operations"): division by zero, 0/0 included, signals an
 ;; error in /, unary/, binary/, floor/, ceiling/, round/, truncate/,
 ;; modulo and remainder.  From the specification.
 (ch14-divide-by-zero
  "(define-method try (thunk) (handler-case (thunk) ((<error>) 'error)))
   (print (list (try (method () (/ 1 0))) (try (method () (/ 0 0))) (try (method () (/ 0)))
                (try (method () (unary/ 0))) (try (method () (binary/ 3 0)))
                (try (method () (/ 1/2 0)))))
   (print (list (try (method () (floor/ 7 0))) (try (method () (ceiling/ 7 0)))
                (try (method () (round/ 7 0))) (try (method () (truncate/ 7 0)))
                (try (method () (modulo 7 0))) (try (method () (remainder 7 0)))))"
  "(error error error error error error)
(error error error error error error)
")

 ;; Design note 24 again, for float division: "division by zero signals
 ;; an error".  From the specification.
 (ch14-divide-by-zero-float
  "(define-method try (thunk) (handler-case (thunk) ((<error>) 'error)))
   (print (list (try (method () (/ 1.0 0))) (try (method () (/ 1 0.0))) (try (method () (/ 0.0 0.0)))
                (try (method () (floor/ 7.5 0))) (try (method () (modulo 7 0.0)))))"
  "(error error error error error)
")

 ;; 14: abs, as in Common Lisp.  From the specification.
 (ch14-abs
  "(print (list (abs -3) (abs 3) (abs 0) (abs -1/2) (abs 1/2)))
   (print (list (= (abs -2.5) 2.5) (= (abs 2.5) 2.5) (instance? (abs -2.5) <float>)))"
  "(3 3 0 1/2 1/2)
(#t #t #t)
")

 ;; 14: sin, cos, tan, asin, acos, atan, atan2 (the two-argument atan),
 ;; sinh, cosh, tanh, asinh, acosh, atanh, exp, log, sqrt, as in Common
 ;; Lisp.  From the specification (the values are mathematical
 ;; identities), compared within a tolerance.
 (ch14-transcendental
  "(define close? (method (a b) (< (abs (- a b)) 1.0e-12)))
   (define pi 3.141592653589793)
   (print (list (close? (sin 0) 0) (close? (cos 0) 1) (close? (tan 0) 0)
                (close? (sin (/ pi 2)) 1) (close? (cos pi) -1) (close? (tan (/ pi 4)) 1)
                (close? (sin 0.5) 0.479425538604203)))
   (print (list (close? (asin 1) (/ pi 2)) (close? (acos 1) 0) (close? (acos 0) (/ pi 2))
                (close? (atan 1) (/ pi 4)) (close? (atan -1) (- (/ pi 4)))))
   (print (list (close? (atan2 1 1) (/ pi 4)) (close? (atan2 1 -1) (* 3/4 pi))
                (close? (atan2 -1 -1) (* -3/4 pi)) (close? (atan2 0 1) 0)
                (close? (atan2 1 0) (/ pi 2))))
   (print (list (close? (sinh 0) 0) (close? (cosh 0) 1) (close? (tanh 0) 0)
                (close? (sinh 1) 1.1752011936438014) (close? (cosh 1) 1.5430806348152437)
                (close? (tanh 1) 0.7615941559557649)))
   (print (list (close? (asinh 0) 0) (close? (acosh 1) 0) (close? (atanh 0) 0)
                (close? (asinh (sinh 2)) 2) (close? (acosh (cosh 2)) 2)
                (close? (atanh (tanh 0.5)) 0.5)))
   (print (list (close? (exp 0) 1) (close? (exp 1) 2.718281828459045) (close? (log 1) 0)
                (close? (log (exp 2)) 2) (close? (exp (log 10)) 10)))
   (print (list (close? (sqrt 16) 4) (close? (sqrt 2) 1.4142135623730951) (close? (sqrt 0.25) 0.5)
                (close? (sqrt 1/4) 0.5) (= (sqrt 16) 4)))
   (print (list (instance? (sin 0.5) <float>) (instance? (exp 1.0) <float>)
                (instance? (sqrt 2.0) <float>)))"
  "(#t #t #t #t #t #t #t)
(#t #t #t #t #t)
(#t #t #t #t #t)
(#t #t #t #t #t #t)
(#t #t #t #t #t #t)
(#t #t #t #t #t)
(#t #t #t #t #t)
(#t #t #t)
")

 ;; 14: expt, as in Common Lisp: exact for a rational base and an integer
 ;; power, (expt x 0) is 1, a float base gives a float.  From the
 ;; specification.
 (ch14-expt
  "(print (list (expt 2 10) (expt 2 0) (expt 0 0) (expt -2 3) (expt 2 -2) (expt 2/3 2)
                (expt 2/3 -3) (expt 10 18) (expt 1/2 1)))
   (print (list (= (expt 2.0 3) 8) (instance? (expt 2.0 3) <float>)
                (< (abs (- (expt 2.0 0.5) 1.4142135623730951)) 1.0e-12)
                (< (abs (- (expt 4 1/2) 2)) 1.0e-12)))"
  "(1024 1 1 -8 1/4 4/9 27/8 1000000000000000000 1/2)
(#t #t #t #t)
")

 ;; Integers are 63 bits here and there are no bignums (README.md); an
 ;; integer result that does not fit is an error rather than a wrong
 ;; answer.
 (ch14-integer-overflow
  "(define big (expt 2 61))
   (print big)
   (print (handler-case (+ big big) ((<error>) 'error)))
   (print (handler-case (* big 4) ((<error>) 'error)))
   (print (handler-case (expt 10 30) ((<error>) 'error)))
   (print (handler-case (- (- big) big big) ((<error>) 'error)))"
  "2305843009213693952
error
error
error
error
")

 ;; 14: logior, logxor, logand, logeqv (any number of integers), lognand,
 ;; lognor, logandc1, logandc2, logorc1, logorc2, lognot, ash as in
 ;; Common Lisp; logbit? as Common Lisp's logbitp.  From the
 ;; specification (Common Lisp's identities: (logior) 0, (logxor) 0,
 ;; (logand) -1, (logeqv) -1).
 (ch14-logical
  "(print (list (logior) (logior 5) (logior 1 2 4) (logior 12 10) (logior -1 5)))
   (print (list (logxor) (logxor 5) (logxor 12 10) (logxor 1 3 7)))
   (print (list (logand) (logand 5) (logand 12 10) (logand 15 14 7) (logand -1 9)))
   (print (list (logeqv) (logeqv 5) (logeqv 12 10) (logeqv 3 5)))
   (print (list (lognand 12 10) (lognor 12 10) (logandc1 12 10) (logandc2 12 10)
                (logorc1 12 10) (logorc2 12 10)))
   (print (list (lognot 0) (lognot 5) (lognot -1) (lognot -6)))
   (print (list (logbit? 0 5) (logbit? 1 5) (logbit? 2 5) (logbit? 3 5) (logbit? 40 -1)
                (logbit? 0 0)))
   (print (list (ash 1 10) (ash 1024 -3) (ash -8 -1) (ash -1 -5) (ash 5 0) (ash 3 -2) (ash -3 2)))"
  "(0 5 7 14 -1)
(0 5 6 5)
(-1 5 8 6 9)
(-1 5 -7 -7)
(-9 -15 2 4 -5 -3)
(-1 -6 0 5)
(#t #f #t #f #t #f)
(1024 128 -4 -1 5 0 -12)
")

 ;; 14: numerator and denominator as in R4RS: of the rational in lowest
 ;; terms, with a positive denominator; an integer's denominator is 1.
 ;; From the specification.
 (ch14-numerator-denominator
  "(print (list (numerator 6/4) (denominator 6/4) (numerator -6/4) (denominator -6/4)
                (numerator 5) (denominator 5) (numerator 0) (denominator 0)
                (numerator (/ 6 4)) (denominator (/ 6 -4))))"
  "(3 2 -3 2 5 1 0 1 3 2)
")

 ;; 14: numerator and denominator "as defined in R4RS", which gives
 ;; (denominator (exact->inexact (/ 6 4))) => 2.0: a float's are those of
 ;; the rational it equals, as floats.  From the specification.
 (ch14-numerator-denominator-float
  "(print (list (= (denominator 1.5) 2) (instance? (denominator 1.5) <float>)
                (= (numerator 1.5) 3) (instance? (numerator 1.5) <float>)
                (= (numerator 0.25) 1) (= (denominator 0.25) 4)))"
  "(#t #t #t #t #t #t)
")

 ;; 14: rationalize, as in R4RS: the simplest rational equal to the
 ;; number (within its precision).  From the specification; only values
 ;; a float holds exactly are used, compared with =, since R4RS answers
 ;; an inexact result for an inexact argument.
 (ch14-rationalize
  "(print (list (rationalize 1/3) (rationalize 3) (rationalize -2/7)))
   (print (list (= (rationalize 0.5) 1/2) (= (rationalize 0.75) 3/4) (= (rationalize -0.25) -1/4)
                (= (rationalize 2.0) 2)))"
  "(1/3 3 -2/7)
(#t #t #t #t)
")

 ;; 14: real-part, imag-part and angle as in R4RS, for real numbers: a
 ;; real is its own real part, its imaginary part is zero, and its angle
 ;; is 0 if positive and pi if negative.  make-rectangular with an exact
 ;; zero imaginary part makes a real, by Common Lisp's complex
 ;; canonicalization ("Automatic Type Conversion").  From the
 ;; specification.
 (ch14-real-complex-parts
  "(print (list (real-part 5) (real-part -1/2) (imag-part 5) (imag-part 1/2)
                (= (real-part 2.5) 2.5) (zero? (imag-part 2.5))))
   (print (list (zero? (angle 5)) (zero? (angle 0.5))
                (< (abs (- (angle -1) 3.141592653589793)) 1.0e-12)))
   (print (list (make-rectangular 3 0) (make-rectangular 1/2 0)
                (instance? (make-rectangular 3 0) <integer>)))"
  "(5 -1/2 0 0 #t #t)
(#t #t #t)
(3 1/2 #t)
")

 ;; 14: gcd and lcm: with no arguments gcd returns 0 and lcm 1; with one
 ;; argument both return it; with more they combine the arguments left to
 ;; right with binary-gcd and binary-lcm, which return the greatest
 ;; common divisor and least common multiple.  From the specification.
 (ch14-gcd-lcm
  "(print (list (gcd) (lcm) (gcd 12) (lcm 12) (instance? (gcd) <integer>)))
   (print (list (gcd 12 18) (gcd 12 18 8) (gcd 7 5) (gcd 0 5) (gcd -12 18) (gcd 12 -18)))
   (print (list (lcm 4 6) (lcm 2 3 4) (lcm 5 7) (lcm -4 6) (lcm 0 5)))
   (print (list (binary-gcd 12 18) (binary-gcd 9 0) (binary-lcm 4 6) (binary-lcm 3 5)))"
  "(0 1 12 12 #t)
(6 2 1 5 6 6)
(12 12 35 12 0)
(6 9 12 15)
")

 ;; 14: gcd and lcm "are methods ... To extend the protocol, define
 ;; methods on binary-lcm and binary-gcd."  From the specification.
 (ch14-extend-gcd
  "(define-class <poly> (<object>) (n init-keyword: n:))
   (define-method binary-gcd ((a <poly>) (b <poly>)) (make <poly> n: (gcd (n a) (n b))))
   (define-method binary-lcm ((a <poly>) (b <poly>)) (make <poly> n: (lcm (n a) (n b))))
   (define p (method (k) (make <poly> n: k)))
   (print (list (n (gcd (p 12) (p 18) (p 8))) (n (lcm (p 2) (p 3) (p 4)))))"
  "(2 12)
")

 ;; 14: min and max return the argument that is least or greatest, by
 ;; calling binary<.  From the specification: the argument itself is
 ;; returned, without contagion.
 (ch14-min-max
  "(print (list (min 5) (max 5) (min 3 1 2) (max 3 1 2) (min -1/2 1/3) (max -1/2 1/3)))
   (print (list (min 1 2.0) (instance? (min 1 2.0) <integer>) (= (max 1 2.5) 2.5)
                (instance? (max 3 2.5) <integer>) (min 1/2 1 3/4)))
   (print (handler-case (min 1 'a) ((<error>) 'error)))"
  "(5 5 1 3 -1/2 1/3)
(1 #t #t #t 1/2)
error
")

 ;; 14: min and max "operate by calling binary<", so a class with a
 ;; binary< method works with them.  From the specification.
 (ch14-min-max-binary-less
  "(define-class <weight> (<object>) (grams init-keyword: grams:))
   (define-method binary< ((a <weight>) (b <weight>)) (< (grams a) (grams b)))
   (define w (method (g) (make <weight> grams: g)))
   (print (list (grams (min (w 5) (w 2) (w 9))) (grams (max (w 5) (w 2) (w 9)))))"
  "(2 9)
")

 ;; Chapter 11, as: "Predefined methods allow coercion between numeric
 ;; types", answering "a new instance of class that has the same contents
 ;; as object": a float converts to the exact number it holds, so to an
 ;; integer only when it is integral and to a ratio only when it is not.
 ;; From the specification.
 (ch14-as-exact
  "(define try (method (c x) (handler-case (as c x) ((<error>) 'error))))
   (print (list (try <integer> 3.0) (try <integer> -7.0s0) (try <integer> 2.5) (try <integer> 0.0)))
   (print (list (try <rational> 0.5) (try <rational> 3.0) (try <rational> -0.25s0)))
   (print (list (try <ratio> 0.5) (try <ratio> 2.0) (try <integer> (as <extended-float> 4))))"
  "(3 -7 error 0)
(1/2 3 -1/4)
(1/2 error 4)
")
)
