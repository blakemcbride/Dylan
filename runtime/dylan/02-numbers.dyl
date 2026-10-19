module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; Numbers (chapter 14), without bignums or complex numbers
;;; (CompilerPlan.md).  The arithmetic itself, with Common Lisp's
;;; contagion, is runtime/c/number.c's.

;;; The binary operations, which programs extend

(define-generic-function binary+ (number1 number2))
(define-method binary+ ((x <number>) (y <number>)) (%number-add x y))

(define-generic-function binary- (number1 number2))
(define-method binary- ((x <number>) (y <number>)) (%number-sub x y))

(define-generic-function binary* (number1 number2))
(define-method binary* ((x <number>) (y <number>)) (%number-mul x y))

(define-generic-function binary/ (number1 number2))
(define-method binary/ ((x <number>) (y <number>)) (%number-div x y))

(define-generic-function unary- (number))
(define-method unary- ((x <number>)) (%number-negate x))

(define-generic-function unary/ (number))
(define-method unary/ ((x <number>)) (%number-div 1 x))

;;; The variadic operators, which are not generic.  (The compiler calls
;;; binary+ and the rest directly for a call with two arguments.)

(define +
  (method (#rest numbers)
    (if (id? numbers '())
        0
        (bind-methods ((sum (total more)
                         (if (id? more '())
                             total
                             (sum (binary+ total (car more)) (cdr more)))))
          (sum (car numbers) (cdr numbers))))))

(define *
  (method (#rest numbers)
    (if (id? numbers '())
        1
        (bind-methods ((product (total more)
                         (if (id? more '())
                             total
                             (product (binary* total (car more)) (cdr more)))))
          (product (car numbers) (cdr numbers))))))

(define -
  (method (number #rest numbers)
    (if (id? numbers '())
        (unary- number)
        (bind-methods ((difference (total more)
                         (if (id? more '())
                             total
                             (difference (binary- total (car more)) (cdr more)))))
          (difference number numbers)))))

(define /
  (method (number #rest numbers)
    (if (id? numbers '())
        (unary/ number)
        (bind-methods ((quotient (total more)
                         (if (id? more '())
                             total
                             (quotient (binary/ total (car more)) (cdr more)))))
          (quotient number numbers)))))

;;; Properties

(define-generic-function zero? (number))
(define-method zero? ((x <number>)) (%number= x 0))

(define-generic-function positive? (real))
(define-method positive? ((x <real>)) (%number< 0 x))

(define-generic-function negative? (real))
(define-method negative? ((x <real>)) (%number< x 0))

(define-generic-function odd? (integer))
(define-method odd? ((n <integer>)) (id? (%logand n 1) 1))

(define-generic-function even? (integer))
(define-method even? ((n <integer>)) (id? (%logand n 1) 0))

(define-generic-function integral? (number))
(define-method integral? ((n <rational>)) (%instance? n <integer>))
(define-method integral? ((x <float>))
  (bind ((q r (%divide 3 x 1))) (zero? r)))

;;; Division (chapter 14: as Common Lisp's floor, ceiling, round and
;;; truncate).  %divide answers the quotient and the remainder.

(define-generic-function floor/ (real1 real2))
(define-method floor/ ((x <real>) (y <real>)) (%divide 0 x y))

(define-generic-function ceiling/ (real1 real2))
(define-method ceiling/ ((x <real>) (y <real>)) (%divide 1 x y))

(define-generic-function round/ (real1 real2))
(define-method round/ ((x <real>) (y <real>)) (%divide 2 x y))

(define-generic-function truncate/ (real1 real2))
(define-method truncate/ ((x <real>) (y <real>)) (%divide 3 x y))

(define-generic-function floor (real))
(define-method floor ((x <real>)) (%divide 0 x 1))

(define-generic-function ceiling (real))
(define-method ceiling ((x <real>)) (%divide 1 x 1))

(define-generic-function round (real))
(define-method round ((x <real>)) (%divide 2 x 1))

(define-generic-function truncate (real))
(define-method truncate ((x <real>)) (%divide 3 x 1))

(define-generic-function modulo (real1 real2))
(define-method modulo ((x <real>) (y <real>))
  (bind ((q r (floor/ x y))) r))

(define-generic-function remainder (real1 real2))
(define-method remainder ((x <real>) (y <real>))
  (bind ((q r (truncate/ x y))) r))

(define-generic-function magnitude (number))
(define-method magnitude ((x <real>)) (abs x))

(define-generic-function abs (number))
(define-method abs ((x <real>)) (if (negative? x) (unary- x) x))

;;; The irrational functions, as Common Lisp's; a rational argument gives
;;; a <double-float>.

(define-generic-function sin (number))
(define-method sin ((x <real>)) (%math 0 x))
(define-generic-function cos (number))
(define-method cos ((x <real>)) (%math 1 x))
(define-generic-function tan (number))
(define-method tan ((x <real>)) (%math 2 x))
(define-generic-function asin (number))
(define-method asin ((x <real>)) (%math 3 x))
(define-generic-function acos (number))
(define-method acos ((x <real>)) (%math 4 x))
(define-generic-function atan (number))
(define-method atan ((x <real>)) (%math 5 x))
(define-generic-function sinh (number))
(define-method sinh ((x <real>)) (%math 6 x))
(define-generic-function cosh (number))
(define-method cosh ((x <real>)) (%math 7 x))
(define-generic-function tanh (number))
(define-method tanh ((x <real>)) (%math 8 x))
(define-generic-function asinh (number))
(define-method asinh ((x <real>)) (%math 9 x))
(define-generic-function acosh (number))
(define-method acosh ((x <real>)) (%math 10 x))
(define-generic-function atanh (number))
(define-method atanh ((x <real>)) (%math 11 x))
(define-generic-function exp (number))
(define-method exp ((x <real>)) (%math 12 x))
(define-generic-function log (number))
(define-method log ((x <real>)) (%math 13 x))
(define-generic-function sqrt (number))
(define-method sqrt ((x <real>)) (%math 14 x))
(define-generic-function atan2 (number1 number2))
(define-method atan2 ((y <real>) (x <real>)) (%math 15 y x))

;;; expt: exact for a rational base and an integer power, as Common Lisp.

(define-generic-function expt (number1 number2))

(define-method expt ((base <real>) (power <real>))
  (if (and (%instance? base <rational>) (%instance? power <integer>))
      (bind-methods ((raise (b n result)
                       (cond ((zero? n) result)
                             ((odd? n) (raise b (- n 1) (binary* result b)))
                             (else: (raise (binary* b b) (%ash n -1) result)))))
        (if (negative? power)
            (unary/ (raise base (unary- power) 1))
            (raise base power 1)))
      (%math 16 base power)))

;;; Integers as bits (as Common Lisp's)

(define-generic-function logior (#rest integers))
(define-method logior (#rest integers)
  (bind-methods ((combine (r l) (if (id? l '()) r (combine (%logior r (car l)) (cdr l)))))
    (combine 0 integers)))

(define-generic-function logxor (#rest integers))
(define-method logxor (#rest integers)
  (bind-methods ((combine (r l) (if (id? l '()) r (combine (%logxor r (car l)) (cdr l)))))
    (combine 0 integers)))

(define-generic-function logand (#rest integers))
(define-method logand (#rest integers)
  (bind-methods ((combine (r l) (if (id? l '()) r (combine (%logand r (car l)) (cdr l)))))
    (combine -1 integers)))

(define-generic-function logeqv (#rest integers))
(define-method logeqv (#rest integers)
  (bind-methods ((combine (r l)
                   (if (id? l '()) r (combine (%lognot (%logxor r (car l))) (cdr l)))))
    (combine -1 integers)))

(define-generic-function lognot (integer))
(define-method lognot ((n <integer>)) (%lognot n))

(define-generic-function lognand (integer1 integer2))
(define-method lognand ((m <integer>) (n <integer>)) (%lognot (%logand m n)))

(define-generic-function lognor (integer1 integer2))
(define-method lognor ((m <integer>) (n <integer>)) (%lognot (%logior m n)))

(define-generic-function logandc1 (integer1 integer2))
(define-method logandc1 ((m <integer>) (n <integer>)) (%logand (%lognot m) n))

(define-generic-function logandc2 (integer1 integer2))
(define-method logandc2 ((m <integer>) (n <integer>)) (%logand m (%lognot n)))

(define-generic-function logorc1 (integer1 integer2))
(define-method logorc1 ((m <integer>) (n <integer>)) (%logior (%lognot m) n))

(define-generic-function logorc2 (integer1 integer2))
(define-method logorc2 ((m <integer>) (n <integer>)) (%logior m (%lognot n)))

(define-generic-function logbit? (index integer))
(define-method logbit? ((index <integer>) (n <integer>)) (%logbit? index n))

(define-generic-function ash (integer count))
(define-method ash ((n <integer>) (count <integer>)) (%ash n count))

;;; Parts of numbers (as in the Revised^4 Report on Scheme)

(define-generic-function numerator (number))
(define-method numerator ((x <rational>)) (%numerator x))
(define-method numerator ((x <float>)) (bind ((n d (%float-fraction x))) n))

(define-generic-function denominator (number))
(define-method denominator ((x <rational>)) (%denominator x))
(define-method denominator ((x <float>)) (bind ((n d (%float-fraction x))) d))

;;; A float's numerator and denominator are those of the fraction it holds
;;; exactly, as floats of its class: (denominator 1.5) is 2.0.
(define %float-fraction
  (method (x)
    (when (or (not (= x x)) (and (not (zero? x)) (= x (* x 2))))
      (error "numerator, denominator: ~s is not a finite number" x))
    (for ((n x (* n 2))
          (d (+ (- x x) 1) (* d 2)))
         ((integral? n) (values n d)))))

(define-generic-function rationalize (number))
(define-method rationalize ((x <real>)) (%rationalize x))

(define-generic-function real-part (number))
(define-method real-part ((x <real>)) x)

(define-generic-function imag-part (number))
(define-method imag-part ((x <real>)) 0)

(define-generic-function angle (number))
(define-method angle ((x <real>)) (if (negative? x) (%math 15 0.0 -1.0) 0))

;;; There are no complex numbers in this version: these make only reals.

(define-generic-function make-rectangular (real-part imag-part))
(define-method make-rectangular ((r <real>) (i <real>))
  (if (and (%instance? i <rational>) (zero? i))
      r
      (error "complex numbers are not supported: ~s ~s" r i)))

(define-generic-function make-polar (magnitude angle))
(define-method make-polar ((m <real>) (a <real>))
  (if (and (%instance? a <rational>) (zero? a))
      m
      (error "complex numbers are not supported: ~s ~s" m a)))

;;; gcd and lcm

(define-generic-function binary-gcd (integer1 integer2))
(define-method binary-gcd ((m <integer>) (n <integer>)) (%gcd m n))

(define-generic-function binary-lcm (integer1 integer2))
(define-method binary-lcm ((m <integer>) (n <integer>))
  (if (or (zero? m) (zero? n))
      0
      (abs (binary* (truncate/ m (%gcd m n)) n))))

(define gcd
  (method (#rest integers)
    (bind-methods ((combine (r l) (if (id? l '()) r (combine (binary-gcd r (car l)) (cdr l)))))
      (if (id? integers '()) 0 (combine (car integers) (cdr integers))))))

(define lcm
  (method (#rest integers)
    (bind-methods ((combine (r l) (if (id? l '()) r (combine (binary-lcm r (car l)) (cdr l)))))
      (if (id? integers '()) 1 (combine (car integers) (cdr integers))))))

;;; min and max, by binary<

(define min
  (method (real #rest more)
    (bind-methods ((least (r l)
                     (if (id? l '())
                         r
                         (least (if (binary< (car l) r) (car l) r) (cdr l)))))
      (least real more))))

(define max
  (method (real #rest more)
    (bind-methods ((greatest (r l)
                     (if (id? l '())
                         r
                         (greatest (if (binary< r (car l)) (car l) r) (cdr l)))))
      (greatest real more))))

;;; Conversions between the kinds of float

(define-method as ((class (singleton <float>)) (x <real>))
  (if (%instance? x <float>) x (%float x 1)))
(define-method as ((class (singleton <single-float>)) (x <real>)) (%float x 0))
(define-method as ((class (singleton <double-float>)) (x <real>)) (%float x 1))
(define-method as ((class (singleton <extended-float>)) (x <real>)) (%float x 2))

;;; From a float to an exact number: the number it holds exactly (chapter 11:
;;; a new instance with the same contents), so an integer only from an
;;; integral float and a ratio only from one that is not.

(define %float->rational
  (method (x)
    (bind ((n d (%float-fraction x)))
      (bind ((n-integer r1 (truncate n))
             (d-integer r2 (truncate d)))
        (/ n-integer d-integer)))))

(define-method as ((class (singleton <rational>)) (x <float>)) (%float->rational x))

(define-method as ((class (singleton <integer>)) (x <float>))
  (bind ((r (%float->rational x)))
    (if (%instance? r <integer>)
        r
        (error "as: ~s cannot be converted to ~s" x class))))

(define-method as ((class (singleton <ratio>)) (x <float>))
  (bind ((r (%float->rational x)))
    (if (%instance? r <ratio>)
        r
        (error "as: ~s cannot be converted to ~s" x class))))
