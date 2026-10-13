;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Test programs for the object system (Phase 5), in the form of
;;;; cases.lisp; :compile-error "text" means the program must be rejected
;;;; with a message containing the text.

(
 ;; Chapter 10's double: the most specific method runs; a singleton is
 ;; more specific than any class; next-method calls the next one.
 (double
  "(define-method double ((thing <number>)) (+ thing thing))
   (print (double 5))
   (define-method double ((thing <integer>)) (* thing 2))
   (print (double 5))
   (define-method double ((thing (singleton 'cup))) 'pint)
   (print (double 'cup))
   (define-method double ((num <float>)) (print \"doubling a floating-point number\") (next-method))
   (print (double 10.5))
   (print (double 1/2))"
  "10
10
pint
\"doubling a floating-point number\"
21.0d0
1
")

 ;; With no more methods, next-method is #f.
 (no-next-method
  "(define-method nm ((x <integer>)) (if next-method 'has-next 'none))
   (define-method nm ((x <number>)) 'number)
   (define-method only ((x <integer>)) (if next-method 'has 'none))
   (print (list (nm 1) (nm 1.5) (only 3)))"
  "(has-next number none)
")

 ;; next-method with no arguments passes the method's own; with
 ;; arguments, those.  A next-method kept and called later still has
 ;; them.
 (next-method
  "(define-class <a> (<object>))
   (define-class <b> (<a>))
   (define-class <c> (<b>))
   (define-method describe ((x <a>) #rest more) (list 'a more))
   (define-method describe ((x <b>) #rest more) (list 'b (next-method)))
   (define-method describe ((x <c>) #rest more) (list 'c (next-method x 1 2)))
   (print (describe (make <c>) 9))
   (define-method later ((x <b>)) (method () (next-method)))
   (define-method later ((x <a>)) 'from-a)
   (print ((later (make <c>))))"
  "(c (b (a (1 2))))
from-a
")

 ;; Classes neither of which is a subclass of the other are ordered by
 ;; the precedence list of the argument's class.
 (multiple-inheritance
  "(define-class <p> (<object>))
   (define-class <q> (<object>))
   (define-class <pq> (<p> <q>))
   (define-class <qp> (<q> <p>))
   (define-method which ((x <p>)) 'p)
   (define-method which ((x <q>)) 'q)
   (print (list (which (make <pq>)) (which (make <qp>))))
   (print (all-superclasses <pq>))"
  "(p q)
({class <pq>} {class <p>} {class <q>} {class <object>})
")

 ;; Multimethods: specializers compared left to right.  Singletons of
 ;; numbers, characters, #f, ratios and classes.
 (multimethods-and-singletons
  "(define-method show ((a <integer>) (b <integer>)) 'int-int)
   (define-method show ((a <number>) (b <integer>)) 'num-int)
   (define-method show ((a <integer>) (b <number>)) 'int-num)
   (define-method show ((a (singleton 0)) (b <number>)) 'zero-num)
   (print (list (show 1 2) (show 1.5 2) (show 1 2.5) (show 0 2) (show 0 2.5)))
   (define-method lit ((x (singleton #\\a))) 'char-a)
   (define-method lit ((x (singleton #f))) 'false)
   (define-method lit ((x (singleton 1/2))) 'half)
   (define-method lit ((x (singleton <integer>))) 'the-integer-class)
   (define-method lit ((x <object>)) 'other)
   (print (list (lit #\\a) (lit #f) (lit (/ 1 2)) (lit <integer>) (lit 'z) (lit 0.5)))"
  "(int-int num-int int-num zero-num zero-num)
(char-a false half the-integer-class other other)
")

 ;; Dispatch on the built-in classes.
 (built-in-classes
  "(define-method kind ((x <list>)) 'list)
   (define-method kind ((x <pair>)) 'pair)
   (define-method kind ((x <string>)) 'string)
   (define-method kind ((x <symbol>)) 'symbol)
   (define-method kind ((x <keyword>)) 'keyword)
   (define-method kind ((x <character>)) 'char)
   (define-method kind ((x <float>)) 'float)
   (define-method kind ((x <rational>)) 'rational)
   (define-method kind ((x <function>)) 'function)
   (define-method kind ((x <object>)) 'object)
   (print (list (kind '()) (kind '(1)) (kind \"s\") (kind 'a) (kind b:) (kind #\\c)
                (kind 1.5) (kind 3) (kind 1/3) (kind kind) (kind car) (kind #t)))"
  "(list pair string symbol keyword char float rational rational function function object)
")

 ;; Chapter 6's <rectangle>, to the type error at its end.
 (rectangle
  "(define-class <rectangle> (<object>)
     (top type: <integer> init-value: 0 init-keyword: top:)
     (left type: <integer> init-value: 0 init-keyword: left:)
     (bottom type: <integer> init-value: 100 init-keyword: bottom:)
     (right type: <integer> init-value: 100 init-keyword: right:))
   (define my-rectangle (make <rectangle> top: 50 left: 50))
   (print (top my-rectangle))
   (print (bottom my-rectangle))
   (print (set! (bottom my-rectangle) 55))
   (print (bottom my-rectangle))
   (set! (bottom my-rectangle) 'foo)"
  "50
100
55
55
"
  :error "foo is not an instance of")

 ;; Slot allocations (chapter 6): class slots shared by subclasses'
 ;; instances, each-subclass slots one per class, constant slots, init
 ;; functions called for each instance, and the error of an
 ;; uninitialized slot.
 (slot-allocation
  "(define-class <counter> (<object>)
     (count allocation: class init-value: 0)
     (kind allocation: each-subclass init-value: 'base)
     (version allocation: constant init-value: 1)
     (name init-keyword: name:)
     (stamp init-function: (method () (list 'fresh))))
   (define-class <sub> (<counter>))
   (define a (make <counter> name: 'a))
   (define b (make <sub>))
   (set! (count a) 5)
   (print (count b))
   (set! (kind b) 'sub)
   (print (list (kind a) (kind b)))
   (print (version a))
   (print (name a))
   (print (stamp b))
   (print (id? (stamp a) (stamp b)))
   (print (name b))"
  "5
(base sub)
1
a
(fresh)
#f
"
  :error "the slot name is not initialized")

 (required-init-keyword
  "(define-class <req> (<object>) (x required-init-keyword: x:))
   (print (x (make <req> x: 3)))
   (make <req>)"
  "3
"
  :error "required init-keyword is missing")
 (make-unknown-keyword
  "(define-class <req> (<object>) (x init-keyword: x:))
   (make <req> x: 1 y: 2)"
  ""
  :error "keyword argument not accepted")

 ;; initialize methods, their keywords accepted by make, and the method
 ;; on <object> as their next method.
 (initialize
  "(define-class <point> (<object>)
     (x init-keyword: x: init-value: 0) (y init-keyword: y: init-value: 0) sum)
   (define-method initialize ((p <point>) #key scale)
     (next-method)
     (set! (sum p) (* (+ (x p) (y p)) (if scale scale 1))))
   (define p (make <point> x: 2 y: 3 scale: 10))
   (print (list (x p) (y p) (sum p)))
   (print (sum (make <point>)))"
  "(2 3 50)
0
")

 ;; Chapter 6's filtered and virtual slots: <view>, <displaced-view> and
 ;; <shape>.
 (filtered-slots
  "(define-class <view> (<object>) (position init-keyword: position:))
   (define-class <displaced-view> (<view>) (position allocation: virtual))
   (define-method position ((v <displaced-view>)) (list 'displaced (next-method v)))
   (define-method (setter position) ((v <displaced-view>) new)
     (next-method v (list 'undisplaced new)))
   (define v (make <displaced-view> position: 'p0))
   (print (position v))
   (set! (position v) 'p1)
   (print (position v))
   (print (position (make <view> position: 'plain)))
   (define-class <shape> (<view>) (image allocation: virtual) (cached-image init-value: #f))
   (define-method image ((s <shape>))
     (or (cached-image s) (set! (cached-image s) (list 'image-of (position s)))))
   (define-method (setter image) ((s <shape>) new) (set! (cached-image s) new))
   (define s (make <shape> position: 'here))
   (print (image s))
   (set! (image s) 'replaced)
   (print (image s))"
  "(displaced p0)
(displaced (undisplaced p1))
plain
(image-of here)
replaced
")

 ;; Chapter 6: a generic function accepts the keywords its applicable
 ;; methods accept, or any if one has #rest.
 (generic-keywords
  "(define-generic-function area (shape #key))
   (define-method area ((s <integer>) #key units) (list s units))
   (define-method area ((s <float>) #rest r #key) (list s r))
   (print (area 3 units: 'cm))
   (print (area 1.5 anything: 1))
   (area 3 color: 'red)"
  "(3 cm)
(1.5d0 (anything: 1))
"
  :error "keyword argument not accepted")

 (no-applicable-method
  "(define-method f ((x <integer>)) x)
   (print (f 1))
   (f 'a)"
  "1
"
  :error "no applicable method for f")
 (no-methods
  "(define-generic-function empty (x)) (empty 1)"
  ""
  :error "no applicable method for empty")

 ;; A method called directly checks its specializers.
 (direct-call-checks
  "(define m (method ((x <integer>)) x))
   (print (m 1))
   (define-method f ((x <integer>)) (list 'f x))
   (print ((find-method f (list <integer>)) 4))
   (m 'a)"
  "1
(f 4)
"
  :error "a is not an instance of")

 ;; Reflection (chapters 10 and 11).
 (reflection
  "(define-class <animal> (<object>))
   (define-class <dog> (<animal>))
   (define-class <cat> (<animal>))
   (print (direct-subclasses <animal>))
   (print (direct-superclasses <dog>))
   (print (list (subclass? <dog> <animal>) (subclass? <animal> <dog>)))
   (print (list (instance? 3 <rational>) (instance? 'x (singleton 'x)) (instance? 'y (singleton 'x))))
   (define-method speak ((x <animal>)) 'noise)
   (define-method speak ((x <dog>)) 'woof)
   (print (method-specializers (find-method speak (list <dog>))))
   (print (applicable-method? (find-method speak (list <dog>)) (make <cat>)))
   (print (method-specializers (car (sorted-applicable-methods speak (make <dog>)))))
   (print (find-method speak (list <cat>)))
   (print (object-class (make <dog>)))
   (print (all-superclasses <dog>))
   (print (list (speak (make <dog>)) (speak (make <cat>))))"
  "({class <dog>} {class <cat>})
({class <animal>})
(#t #f)
(#t #t #f)
({class <dog>})
#f
({class <dog>})
#f
{class <dog>}
({class <dog>} {class <animal>} {class <object>})
(woof noise)
")

 ;; Calls through generic functions are proper tail calls too.
 (generic-tail-calls
  "(define-method count-down ((n <integer>)) (if (= n 0) 'done (count-down (- n 1))))
   (print (count-down 1000000))
   (define-method ping ((n <integer>)) (if (= n 0) 'ping (pong (- n 1))))
   (define-method pong ((n <integer>)) (if (= n 0) 'pong (ping (- n 1))))
   (print (ping 1000001))"
  "done
pong
")

 ;; What the static program cannot do.
 (dynamic-superclass
  "(define c (method () <object>)) (define-class <x> ((c)))"
  ""
  :compile-error "a superclass must be a class known at compile time")
 (variable-and-generic
  "(define g 1) (define-method g (x) x)"
  ""
  :compile-error "defined both as a variable and as a generic function")
 (not-congruent
  "(define-method h (x) x) (define-method h (x y) x)"
  ""
  :compile-error "not congruent")
 (dynamic-specializer
  "(define-method i ((x (singleton (list 1)))) x)"
  ""
  :compile-error "a specializer must be a class or a singleton known at compile time")
 (assign-class
  "(define-class <z> (<object>)) (set! <z> 3)"
  ""
  :compile-error "cannot be assigned")
)
