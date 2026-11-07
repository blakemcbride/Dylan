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
21.0
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
({the class <pq>} {the class <p>} {the class <q>} {the class <object>})
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
(1.5 (anything: 1))
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
  "({the class <dog>} {the class <cat>})
({the class <animal>})
(#t #f)
(#t #t #f)
({the class <dog>})
#f
({the class <dog>})
#f
{the class <dog>}
({the class <dog>} {the class <animal>} {the class <object>})
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
  :compile-error "a superclass must be a class known at compile time"
  :compiler-only t)
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
  :compile-error "a specializer must be a class or a singleton known at compile time"
  :compiler-only t)
 (assign-class
  "(define-class <z> (<object>)) (set! <z> 3)"
  ""
  :error "<z> holds a class or generic function and cannot be assigned")
 ;; Chapter 5: assigning a read-only variable -- a class's, or a
 ;; define-generic-function's -- is an error when the set! runs; one that
 ;; never runs is no error, and one before the definition finds no value.
 (read-only-assigned-when-run
  "(define-method try (thunk)
  (handler-case (thunk)
    ((<error> condition: c) (format #t \"error: ~a~%\" (condition-format-string c)))))
(try (method () (print <p>)))
(try (method () (print g)))
(try (method () (set! <p> 3)))
(try (method () (set! g 3)))
(define-class <p> (<object>))
(define-generic-function g (x))
(define-generic-function (setter gg) (v x))
(try (method () (set! <p> 3)))
(try (method () (set! g 3)))
(try (method () (set! (setter gg) 3)))
(try (method () (set! <integer> 3)))
(print (instance? (make <p>) <p>))
(define-method g ((x <integer>)) (* x 2))
(print (g 4))
(print (list <p> g <integer>))
(define-method never () (set! <p> 3))
(print 'ran)"
  "error: the variable <p>@dylan-user has no value
error: the variable g@dylan-user has no value
error: the variable <p>@dylan-user has no value
error: the variable g@dylan-user has no value
error: <p> holds a class or generic function and cannot be assigned
error: g holds a class or generic function and cannot be assigned
error: (setter gg) holds a class or generic function and cannot be assigned
error: <integer> holds a class or generic function and cannot be assigned
#t
8
({the class <p>} {the generic function g} {the class <integer>})
ran
")

 ;; define-slot on a singleton gives the one object a slot (chapter 6):
 ;; instance or constant, initialized when the define-slot runs, more
 ;; specific than its class's slot of the same getter.
 (define-slot-singleton
  "(define-class <point> (<object>) (x init-value: 0 init-keyword: x:))
   (define p (make <point> x: 1))
   (define q (make <point> x: 2))
   (define-slot (singleton p) color init-value: 'red)
   (print (color p))
   (set! (color p) 'blue)
   (print (color p))
   (print (handler-case (color q) ((<error>) 'no-color-for-q)))
   (define-slot (singleton q) weight type: <integer> init-function: (method () 42))
   (print (weight q))
   (print (handler-case (begin (set! (weight q) 'heavy) 'set) ((<error>) 'type-error)))
   (define-slot (singleton p) kind allocation: constant init-value: 'special)
   (print (kind p))
   (define-slot (singleton 7) seven-name init-value: \"seven\")
   (print (seven-name 7))
   (define-slot (singleton p) label)
   (print (list (slot-initialized? p label) (slot-initialized? p color)))
   (set! (label p) \"P\")
   (print (list (label p) (slot-initialized? p label)))
   (define give-p-x (method () (define-slot (singleton p) x init-value: 99)))
   (print (list (x p) (x q)))
   (give-p-x)
   (set! (x q) 3)
   (print (list (x p) (x q)))
   (print (handler-case (define-slot p y) ((<error>) 'not-a-singleton)))"
  "red
blue
no-color-for-q
42
type-error
special
\"seven\"
(#f #t)
(\"P\" #t)
(1 2)
(99 3)
not-a-singleton
")

 (define-slot-singleton-class-allocation
  "(define-slot (singleton 1) z allocation: class)"
  ""
  :compile-error "the slot of a singleton must be instance, constant or virtual")

 (define-slot-singleton-init-keyword
  "(define-slot (singleton 1) z init-keyword: z:)"
  ""
  :compile-error "the slot of a singleton takes no init-keyword")

 (found-by-the-book
  ;; Values of an earlier call not taken for those of a function written
  ;; in C; each-subclass slots start uninitialized; map of an array keeps
  ;; its dimensions.
  "(define-method g () (list (values 1 2) 3))
   (bind ((a b (g))) (print (list a b)))
   (define-method h () (values 1 2) (identity 5))
   (bind ((a b (h))) (print (list a b)))
   (define-class <e> (<object>) (s allocation: each-subclass))
   (define e (make <e>))
   (print (slot-initialized? e s))
   (set! (s e) 4)
   (print (list (slot-initialized? e s) (s e)))
   (define a (make <array> dimensions: '(2 3) fill: 1))
   (set! (aref a 1 2) 5)
   (define b (map (method (x) (+ x 1)) a))
   (print (list (dimensions b) (aref b 0 0) (aref b 1 2)))
   (define-class <f> (<e>))
   (s (make <f>))
   (print 'not-reached)"
  "((1 3) #f)
(5 #f)
#f
(#t 4)
((2 3) 2 6)
"
  :error "")
 (local-method
  ;; A define-method that refers to local variables, in a file with a
  ;; header.
  "module: dylan-user

(bind ((k 1))
  (define-method f ((x <integer>)) (+ x k)))
(print (f 1))"
  "2
")

 ;; Classes whose C names would be names the runtime uses for other things
 (runtime-c-names
  "(define-class <cell> (<object>) (v init-keyword: v:))
   (define-class <count> (<object>) (n init-value: 3))
   (define-class <of> (<object>))
   (define-class <subtype> (<of>))
   (print (list (v (make <cell> v: 1)) (n (make <count>)) (make <of>) (make <subtype>)))
   (print (instance? (make <subtype>) <of>))"
  "(1 3 {instance of <of>} {instance of <subtype>})
#t
")

 ;; Chapter 11: all-superclasses, direct-superclasses and slot-descriptors
 ;; answer an empty sequence for a sealed class -- the manual's sealed
 ;; classes (chapters 12 and 14) as well as one seal has sealed.
 (sealed-reflection
  "(for-each ((c (list <integer> <double-float> <ratio> <real> <list> <pair> <byte-string>
                       <simple-object-vector> <character> <object>)))
             ()
     (format #t \"~s ~s ~s ~s~%\" c (size (all-superclasses c)) (size (direct-superclasses c))
             (size (slot-descriptors c))))
   (define-class <s> (<object>) a)
   (seal <s>)
   (format #t \"~s ~s ~s~%\" (all-superclasses <s>) (direct-superclasses <s>) (size (slot-descriptors <s>)))"
  "{the class <integer>} 0 0 0
{the class <double-float>} 0 0 0
{the class <ratio>} 0 0 0
{the class <real>} 0 0 0
{the class <list>} 0 0 0
{the class <pair>} 0 0 0
{the class <byte-string>} 0 0 0
{the class <simple-object-vector>} 0 0 0
{the class <character>} 2 1 0
{the class <object>} 1 0 0
() () 0
")

 ;; Chapter 6: a generic function's keys must be accepted by each of its
 ;; methods, by naming them after #key or by #rest.
 (generic-keys-accepted
  "(define-generic-function g (a #key foo))
   (define-method g (a #key bar foo) (list bar foo))
   (define-method g ((a <integer>) #rest r) r)
   (print (g 'x foo: 1))
   (print (g 3 foo: 2))
   (define-generic-function h (a #key))
   (define-method h (a #key z) z)
   (print (h 1 z: 5))"
  "(#f 1)
(foo: 2)
5
")

 (generic-keys-not-accepted
  "(define-generic-function g (a #key foo))
   (define-method g (a #key bar) 1)
   (print (g 1))"
  ""
  :compile-error "generic function")

 ;; Chapter 6: a redefined slot's type must be within the inherited one's
 (slot-type-narrowed
  "(define-class <a> (<object>) (v type: <number> init-value: 0 init-keyword: v:))
   (define-class <b> (<a>) (v type: <integer> init-value: 3 init-keyword: v:))
   (print (v (make <b>)))
   (define-class <c> (<object>) (w type: <object> init-value: 1))
   (define-class <d> (<c>) (w type: <real> init-value: 2))
   (print (w (make <d>)))"
  "3
2
")

 (slot-type-widened
  "(define-class <a> (<object>) (v type: <number> init-value: 0 init-keyword: v:))
   (define-class <b> (<a>) (v type: <string> init-value: \"s\" init-keyword: v:))
   (print (v (make <b>)))"
  ""
  :compile-error "not a subclass of")

 ;; Chapter 6: a slot's type and initial value are evaluated after the
 ;; class's variable is defined, so they may name the class.
 (slot-names-its-class
  "(define-class <node> (<object>)
     (next type: <node> init-keyword: next:)
     (kind init-value: <node>)
     (maker init-function: (method () <node>)))
   (define n1 (make <node>))
   (define n2 (make <node> next: n1))
   (print (id? (next n2) n1))
   (print (id? (kind n2) <node>))
   (print (id? (maker n2) <node>))
   (print (handler-case (make <node> next: 5) ((<error>) 'refused)))
   (print (slot-type (first (slot-descriptors <node>))))"
  "#t
#t
#t
refused
{the class <node>}
")

 ;; Chapter 11: slot-value with a superclass's slot descriptor reaches the
 ;; slot where the instance's class keeps it, under multiple inheritance
 ;; too; a descriptor of a class the object is not an instance of, even
 ;; one with the same getter, is an error.
 (slot-value-inherited
  "(define-class <a> (<object>) (aa init-value: 'a-slot))
   (define-class <b> (<object>) (bb init-value: 'b-slot)
     (cc allocation: class init-value: 3) (ee allocation: each-subclass init-value: 4))
   (define-class <c> (<a> <b>))
   (define-class <z> (<object>) (zz init-value: 'z-slot) (aa init-value: 'z-aa))
   (define c (make <c>))
   (define sd (method (class name)
     (find-key (slot-descriptors class) (method (s) (id? (slot-getter s) name)))))
   (define bd (element (slot-descriptors <b>) (sd <b> bb)))
   (print (slot-value c bd))
   (set! (slot-value c bd) 'new-b)
   (print (list (aa c) (bb c)))
   (print (slot-value c (element (slot-descriptors <a>) (sd <a> aa))))
   (define try (method (f) (handler-case (f) ((<error>) 'refused))))
   (print (try (method () (slot-value c (element (slot-descriptors <z>) (sd <z> zz))))))
   (print (try (method () (slot-value c (element (slot-descriptors <z>) (sd <z> aa))))))
   (print (try (method () (set! (slot-value c (element (slot-descriptors <z>) (sd <z> aa))) 1))))
   (print (list (aa c) (bb c)))
   (define bs (slot-descriptors <b>))
   (print (list (slot-value c (element bs (sd <b> cc))) (slot-value c (element bs (sd <b> ee)))))
   (set! (slot-value c (element bs (sd <b> cc))) 30)
   (set! (slot-value c (element bs (sd <b> ee))) 40)
   (print (list (cc c) (ee c) (cc (make <b>)) (ee (make <b>))))
   (define-class <d> (<c>) (dd init-value: 'd-slot))
   (print (slot-value (make <d>) bd))"
  "b-slot
(a-slot new-b)
a-slot
refused
refused
refused
(a-slot new-b)
(3 4)
(30 40 30 4)
b-slot
")

 ;; Chapter 10: define-generic-function after define-method has made the
 ;; generic function declares its parameters, #key and #rest among them,
 ;; if the methods are congruent with them.
 (generic-declared-after-methods
  "(define-method f1 (x #key a) (list x a))
   (define-generic-function f1 (x #key a))
   (define-method f2 (x #rest r) (list x r))
   (define-generic-function f2 (x #rest r))
   (define-method f3 (x) x)
   (define-generic-function f3 (x))
   (define-method f4 (x #key a) (list x a))
   (define-generic-function f4 (x #rest r))
   (define-method f5 (x #key a b) (list x a b))
   (define-generic-function f5 (x #key a))
   (print (list (f1 1 a: 2) (f2 1 2 3) (f3 4) (f4 1 a: 5) (f5 1 a: 5)))
   (bind ((#rest r (function-arguments f1))) (print r))
   (bind ((#rest r (function-arguments f2))) (print r))"
  "((1 2) (1 (2 3)) 4 (1 5) (1 5 #f))
(1 #f (a:))
(1 #t #f)
")

 (generic-declared-after-methods-not-congruent
  "(define-method g1 (x) x)
   (define-generic-function g1 (x y))"
  ""
  :compile-error "is not congruent with its generic function")

 ;; The reflective functions: define-slot answers the slot's name
 ;; (chapter 6); function-arguments of the predefined functions follows
 ;; their signatures (chapter 10); a slot made without a type is of type
 ;; <object>; find-method with a list of the wrong length answers #f;
 ;; as answers an object already of the class unchanged (chapter 11).
 (reflection-results
  "(define-class <pt> (<object>))
   (print (list (define-slot <pt> px init-value: 1) (define-slot <pt> getter: py)))
   (for-each ((f (list make initialize error signal cerror range)))
     ()
     (bind ((#rest r (function-arguments f))) (print r)))
   (print (map slot-type (slot-descriptors <simple-error>)))
   (define-method fm (x y) 1)
   (print (list (find-method fm (list <object>)) (find-method fm (list <object> <object> <object>))
                (instance? (find-method fm (list <object> <object>)) <method>)))
   (define v (vector 1))
   (define l (list 1))
   (print (list (id? (as <collection> v) v) (id? (as <explicit-key-collection> v) v)
                (id? (as <sequence> v) v) (id? (as <collection> l) l)))"
  "(px py)
(1 #t #f)
(1 #t #f)
(1 #t #f)
(1 #t #f)
(2 #t #f)
(0 #f (from: up-to: through: by: size:))
#({the class <object>} {the class <object>})
(#f #f #t)
(#t #t #t #t)
")

 ;; #t and #f are of an internal class, <boolean>, which no program can
 ;; name; <object> is abstract, so make of it is an error.
 (boolean-class-and-abstract-object
  "(define-method msg (thunk)
     (handler-case (thunk)
       ((<error> condition: c)
        (apply format #f (condition-format-string c) (condition-format-arguments c)))))
   (print (list (object-class #t) (object-class #f) (id? (object-class #t) (object-class #f))))
   (print (list (all-superclasses (object-class #t)) (instance? #f <object>)))
   (print (msg (method () (make (object-class #t)))))
   (print (msg (method () (make <object>))))
   (print (msg (method () (make <pair>))))
   (define-class <plain> (<object>))
   (print (object-class (make <plain>)))"
  "({the class <boolean>} {the class <boolean>} #t)
(() #t)
\"cannot make an instance of this class: {the class <boolean>}\"
\"cannot make an instance of this class: {the class <object>}\"
\"cannot make an instance of this class: {the class <pair>}\"
{the class <plain>}
")

 ;; slot-descriptors: the slots of each direct superclass in turn, each
 ;; where it first appears (a more specific definition in its place), then
 ;; the class's own; the runtime's own slots, which no program can name,
 ;; are left out.
 (slot-descriptors-order
  "(define-method names (c)
     (map (method (s) (list (slot-getter s) (slot-type s))) (slot-descriptors c)))
   (define-class <a> (<object>) (a))
   (define-class <b> (<object>) (b))
   (define-class <ab> (<a> <b>) (c))
   (define-class <x> (<a>) (x))
   (define-class <xy> (<x> <b>) (y))
   (define-class <base> (<object>) (s type: <number>))
   (define-class <b1> (<base>) (t1))
   (define-class <b3> (<base>) (s type: <integer>) (t3))
   (define-class <e> (<b1> <b3>) (w))
   (define-class <a2> (<object>) (p) (q))
   (define-class <v2> (<a2>) (p) (r))
   (define-class <my-error> (<simple-error>) (extra))
   (define-class <pc> (<object>) (%secret))
   (for-each ((c (list <ab> <xy> <e> <v2> <my-error> <pc>))) ()
     (print (map first (names c))))
   (print (second (first (names <e>))))
   (print (map (method (c) (size (slot-descriptors c)))
               (list <array> <vector> <table> <deque> <stretchy-vector> <range>
                     <restart> <simple-restart>)))"
  "#({the generic function a} {the generic function b} {the generic function c})
#({the generic function a} {the generic function x} {the generic function b} {the generic function y})
#({the generic function s} {the generic function t1} {the generic function t3} {the generic function w})
#({the generic function p} {the generic function q} {the generic function r})
#({the generic function condition-format-string} {the generic function condition-format-arguments} {the generic function extra})
#({the generic function %secret})
{the class <integer>}
(0 0 0 0 0 0 0 0)
")

 (subclasses-of-library-collections
  "(define-method msg (thunk)
     (handler-case (thunk)
       ((<error> condition: c)
        (apply format #f (condition-format-string c) (condition-format-arguments c)))))
   (define-class <my-table> (<table>) (label init-value: 'x))
   (define-class <my-deque> (<deque>) (label init-value: 'd))
   (define-class <my-sv> (<stretchy-vector>) (label init-value: 's))
   (define-class <my-range> (<range>) (label init-value: 'r))
   (bind ((t (make <my-table>)))
     (set! (element t 'a) 1)
     (print (list (label t) (element t 'a) (size t))))
   (bind ((d (make <my-deque>)))
     (push d 1) (push-last d 2)
     (print (list (label d) (size d) (as <list> d))))
   (bind ((v (make <my-sv>)))
     (add! v 1) (add! v 2)
     (print (list (label v) (size v) (as <list> v))))
   (bind ((r (make <my-range>)))
     (print (list (label r) (size r) (first r) (element r 5))))
   (print (msg (method () (make <my-sv> size: 2))))
   (print (msg (method () (make <my-range> from: 3))))
   (print (as <list> (make <range> from: 3 by: 2 size: 3)))"
  "(x 1 1)
(d 2 (1 2))
(s 2 (1 2))
(r #f 0 5)
\"keyword argument not accepted: size:\"
\"keyword argument not accepted: from:\"
(3 5 7)
")

 (define-class-slot-twice
  "(define-class <a> (<object>) (x) (x init-value: 2))"
  ""
  :compile-error "define-class slots: x appears twice")

 (uninitialized-slot-messages
  "(define-method msg (thunk)
     (handler-case (thunk)
       ((<error> condition: c)
        (list (condition-format-string c) (condition-format-arguments c)))))
   (define-class <b> (<object>) (x))
   (print (msg (method () (x (make <b>)))))
   (print (first (second (msg (method () (slot-value (make <b>) (first (slot-descriptors <b>))))))))"
  "(\"the slot x is not initialized\" ())
{instance of <b>}
")

 ;; Chapter 6, "Adding Slots After Instance Creation": an instance made
 ;; before define-slot added a typed slot takes its initial value when it
 ;; is updated (whenever that is), and the value is checked against the
 ;; slot's type, as make checks it.
 (define-slot-initial-value-type
  "(define-method msg (thunk) (handler-case (thunk) ((<error> condition: c) (format #f \"~a\" c))))
   (define-class <b> (<object>))
   (define b (make <b>))
   (print (msg (method () (define-slot <b> t type: <integer> init-value: 2.5) (+ (t b) 1))))
   (define-class <c> (<object>))
   (define c (make <c>))
   (print (msg (method () (define-slot <c> u type: <integer> init-function: (method () \"str\"))
                          (+ (u c) 1))))"
  "\"2.5 is not an instance of {the class <integer>}\"
\"\\\"str\\\" is not an instance of {the class <integer>}\"
")
 ;; shallow-copy copies collections (an array with its own elements) and
 ;; instances of any class, and signals an error for anything else.
 (shallow-copy-kinds
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
(define-class <pt> (<object>) (x init-keyword: x:))
(print (try (method () (shallow-copy 5))))
(print (try (method () (shallow-copy car))))
(print (try (method () (shallow-copy 'sym))))
(print (try (method () (shallow-copy #\\a))))
(print (try (method () (shallow-copy #t))))
(print (try (method () (shallow-copy <pt>))))
(print (try (method () (shallow-copy (singleton 3)))))
(print (try (method () (bind ((p (make <pt> x: 1)) (q (shallow-copy p))) (list (id? p q) (x q))))))
(print (try (method () (shallow-copy '()))))
(print (try (method () (shallow-copy '(1 2)))))
(print (try (method () (shallow-copy \"ab\"))))
(print (try (method () (shallow-copy (range from: 1 size: 2)))))
(print (try (method () (bind ((t (make <table>))) (set! (element t 'a) 1) (shallow-copy t)))))
(print (try (method () (bind ((a (make <array> dimensions: '(2 2) fill: 0)) (b (shallow-copy a))) (set! (aref b 0 0) 9) (list a b (id? a b))))))
(print (try (method () (bind ((c (make <simple-error> format-string: \"x\")) (d (shallow-copy c))) (list (id? c d) (condition-format-string d))))))
(define-class <coll> (<explicit-key-collection>) (n init-keyword: n:))
(print (try (method () (bind ((c (make <coll> n: 3))) (n (shallow-copy c))))))"
  "\"ERR: shallow-copy: cannot copy: 5\"
\"ERR: shallow-copy: cannot copy: {method car}\"
\"ERR: shallow-copy: cannot copy: sym\"
\"ERR: shallow-copy: cannot copy: #\\\\a\"
\"ERR: shallow-copy: cannot copy: #t\"
\"ERR: shallow-copy: cannot copy: {the class <pt>}\"
\"ERR: shallow-copy: cannot copy: {singleton 3}\"
(#f 1)
()
(1 2)
\"ab\"
{range 1 by 1 size 2}
{table a => 1}
({<array> 0 0 0 0} {<array> 9 0 0 0} #f)
(#f \"x\")
3
")

 ;; Too many arguments to a generic function is an error of the count
 ;; before any method is looked for, unless it takes #rest or #key; a
 ;; generic function made by make honours rest?: and key?:.
 (generic-argument-count
  "(define-method kind (f)
     (handler-case (begin (f) 'ok)
       ((<error> condition: c)
        (if (subsequence-position (format #f \"~a\" c) \"wrong number of arguments\")
            'count
            'other))))
   (define-method f4 ((x <integer>)) x)
   (define-generic-function g2 (x #rest r))
   (define-method g2 ((x <integer>) #rest r) r)
   (define h (make <generic-function> required: 1 rest?: #t))
   (define h2 (make <generic-function> required: 1))
   (define h3 (make <generic-function> required: 1 key?: #t))
   (print (map (method (f) (kind f))
               (list (method () (f4 1 2)) (method () (f4)) (method () (f4 'a))
                     (method () (g2 1 2 3)) (method () (h 1 2 3)) (method () (h))
                     (method () (h2 1 2)) (method () (h3 1 k: 2)))))"
  "(count count other ok other count count other)
")
 ;; An error message that names something with a long name holds all of it.
 (long-names-in-errors
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"~a\" c))))
(define-class <s> (<object>) syyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy)
(print (size (try (method () (syyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy (make <s>))))))
(define v (method () wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww))
(print (size (try v)))"
  "329
337
")
 ;; No applicable method: the message names the generic function, with no
 ;; format arguments (user manual, chapter 11).
 (no-applicable-method-message
  "(define-method f ((x <integer>)) x)
(print (handler-case (f \"s\") ((<error> condition: c) (list (condition-format-string c) (condition-format-arguments c)))))"
  "(\"no applicable method for f\" ())
")
 ;; <, = and > answer #t or #f whatever true value a program's binary< or
 ;; binary= answers (chapter 9).
 (comparisons-answer-booleans
  "(define-class <v> (<object>) (n init-keyword: n:))
(define-method binary< ((a <v>) (b <v>)) (if (binary< (n a) (n b)) 'yes #f))
(define-method binary= ((a <v>) (b <v>)) (if (binary= (n a) (n b)) 'same #f))
(define v1 (make <v> n: 1))
(define v2 (make <v> n: 2))
(print (list (< v1 v2) (= v1 v1) (> v2 v1) (< v1 v2 v2) (<= v1 v2) (< 1 2) (= 3 3)))"
  "(#t #t #t #f #t #t #t)
")
 ;; A definition of (setter x) answers the name (setter x) (chapters 4 and 6).
 (setter-definition-value
  "(print (define (setter ee) 3))
(print (define-method (setter ff) (v x) v))
(print (define-generic-function (setter gg) (v x)))
(print (define-class <c> (<object>) (s setter: (setter s))))"
  "(setter ee)
(setter ff)
(setter gg)
<c>
")
 ;; A virtual slot's getter and setter take one argument and two before
 ;; they have methods (chapter 6).
 (virtual-slot-arity
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
(define-class <c> (<object>) (v allocation: virtual))
(print (function-arguments v))
(print (function-arguments (setter v)))
(print (try (method () (v 1))))"
  "1
2
\"ERR: no applicable method for v\"
")
 ;; A slot inherited from two superclasses must have one definition, or
 ;; the class must define it (chapter 6, "Redefining Slots").
 (inherited-slot-clash
  "(define-class <a> (<object>) (s init-value: 1))
(define-class <b> (<object>) (s init-value: 2))
(define-class <ab> (<a> <b>))
(print (s (make <ab>)))"
  ""
  :compile-error "<ab> inherits two different definitions of the slot s, from <a> and <b>")
 ;; The same definitions -- options whose values are id?, a slot inherited
 ;; along two paths from one class, or overridden on one of them -- are
 ;; one; values known only when the class is defined are compared then.
 (inherited-slot-agreement
  "(define-class <a> (<object>) (s init-value: 1.5 type: <real>))
(define-class <b> (<object>) (s init-value: 1.5 type: <real>))
(define-class <ab> (<a> <b>))
(print (s (make <ab>)))
(define-class <base> (<object>) (t init-value: 'x))
(define-class <l> (<base>))
(define-class <r> (<base>))
(define-class <lr> (<l> <r>))
(print (t (make <lr>)))
(define-class <r2> (<base>) (t init-value: 'y))
(define-class <lr2> (<l> <r2>))
(print (t (make <lr2>)))
(define-method three () 3)
(define-class <c> (<object>) (u init-value: (three)))
(define-class <d> (<object>) (u init-value: (+ 1 2)))
(define-class <cd> (<c> <d>))
(print (u (make <cd>)))
(define-class <e> (<object>) (w init-value: (list 1)))
(define-class <f> (<object>) (w init-value: (list 1)))
(print 'before)
(define-class <ef> (<e> <f>))
(print 'after)"
  "1.5
x
y
3
before
"
  :error "<ef> inherits two different definitions of the slot w, from <e> and <f>")
 ;; make-read-only only forbids changes, and hides nothing; seal hides a
 ;; class's superclasses and slots, and a sealed class cannot be subclassed
 ;; (chapter 11).
 (sealed-and-read-only-classes
  "(define-class <a> (<object>) (s init-value: 1))
(define-class <r> (<a>))
(make-read-only <r>)
(print (all-superclasses <r>))
(print (direct-superclasses <r>))
(print (size (slot-descriptors <r>)))
(define-class <q> (<a>))
(seal <q>)
(print (all-superclasses <q>))
(print (direct-superclasses <q>))
(print (size (slot-descriptors <q>)))
(print (slot-descriptor (make <q>) s))
(seal <a>)
(define-class <c> (<a>))
(print 'subclassed)"
  "({the class <r>} {the class <a>} {the class <object>})
({the class <a>})
1
()
()
0
{slot s}
"
  :error "<a> is sealed: it cannot be subclassed")
 ;; A method's specializers are one for each required argument, <object>
 ;; where none is given, and applicable-method? asks for enough arguments
 ;; (chapter 10).
 (unspecialized-method-specializers
  "(print (method-specializers (method (x y) x)))
(print (method-specializers car))
(print (applicable-method? car 5))
(print (applicable-method? car '(1)))
(print (applicable-method? (method (x) x)))
(print (applicable-method? (method (x) x) 1))
(print (applicable-method? (method (#rest r) r)))
(print (method-specializers apply))
(print (applicable-method? apply car))
(print (applicable-method? apply 5))"
  "({the class <object>} {the class <object>})
({the class <list>})
#f
#t
#f
#t
#t
({the class <function>})
#t
#f
")
 ;; An instance made before define-slot added a slot has it, initialized
 ;; as make would have, by every way of reading it (chapter 6: no program
 ;; sees an instance without the slot).
 (define-slot-on-earlier-instances
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
(define-class <pt> (<object>) x)
(define early (make <pt>))
(define early2 (make <pt>))
(define early3 (make <pt>))
(define-slot <pt> z init-value: 7)
(define-slot <pt> w init-function: (method () (list 'fresh)))
(define-slot <pt> v)
(print (slot-initialized? early z))
(print (try (method () (slot-value early (slot-descriptor early z)))))
(print (slot-value early2 (slot-descriptor early2 w)))
(print (slot-initialized? early3 w))
(print (w early3))
(print (slot-initialized? early v))
(print (try (method () (slot-value early (slot-descriptor early v)))))"
  "#t
7
(fresh)
#t
(fresh)
#f
\"ERR: slot-value: the slot is not initialized: {instance of <pt>}\"
")
 ;; A slot define-slot adds to one object -- an instance, a symbol, an
 ;; integer -- has a descriptor for that object alone (chapter 11).
 (slots-of-one-object
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
(define-class <pt> (<object>) (x init-value: 0))
(define p1 (make <pt>))
(define p2 (make <pt>))
(define-slot (singleton p1) color init-value: 'red)
(define sd (slot-descriptor p1 color))
(print sd)
(print (slot-descriptor p2 color))
(print (slot-value p1 sd))
(set! (slot-value p1 sd) 'blue)
(print (color p1))
(print (slot-initialized? p1 color))
(define-slot (singleton 'sym) mood init-value: 'calm)
(define sd2 (slot-descriptor 'sym mood))
(print sd2)
(print (slot-value 'sym sd2))
(print (slot-initialized? 'sym mood))
(print (slot-descriptor 'other mood))
(define-slot (singleton 42) tag init-value: 'answer)
(print (slot-value 42 (slot-descriptor 42 tag)))
(print (try (method () (slot-value p2 sd))))
(print (try (method () (slot-value 'other sd2))))"
  "{slot color}
#f
red
blue
#t
{slot mood}
calm
#t
#f
answer
\"ERR: slot-value: the object does not have the slot: {instance of <pt>}\"
\"ERR: slot-value: the object does not have the slot: other\"
")
 ;; next-method is a method, of #rest alone (chapter 10: every function is a
 ;; generic function or a method).
 (next-method-is-a-method
  "(define-method try (f) (handler-case (f) ((<error> condition: c) (format #f \"ERR: ~a\" c))))
(define-method g ((x <object>)) 'base)
(define-method g ((x <integer>))
  (print (instance? next-method <method>))
  (print (instance? next-method <function>))
  (print (try (method () (method-specializers next-method))))
  (print (try (method () (list (function-arguments next-method)))))
  (print (try (method () (applicable-method? next-method 1))))
  (print next-method)
  (print (object-class next-method))
  (next-method))
(print (g 1))"
  "#t
#t
()
(0)
#t
{an anonymous method}
{the class <method>}
base
")
 ;; A slot specification's faults are worded alike in both (chapter 6
 ;; gives the options).
 (slot-option-unknown
  "(define-class <c> (<object>) (s foo: 1))
(print 'x)"
  ""
  :compile-error "slot: unknown option foo:")
 (slot-setter-not-a-name
  "(define-class <c> (<object>) (s setter: 5))
(print 'x)"
  ""
  :compile-error "slot setter: 5 is not a variable name")
 (slot-allocation-unknown
  "(define-class <c> (<object>) (s allocation: foo))
(print 'x)"
  ""
  :compile-error "slot s: allocation: must be one of the symbols instance, class, each-subclass, constant and virtual, not foo")
 ;; A superclass that is a constant is never a class.
 (superclass-not-a-class
  "(define-class <c> (5))
(print 'x)"
  ""
  :compile-error "not a class: 5")
 ;; Chapter 6: required-init-keyword: is like init-keyword:, so init-keyword
 ;; answers it (chapter 11).
 (init-keyword-of-required-slot
  "(define-class <p> (<object>) (y required-init-keyword: y:) (z init-keyword: z:) (w init-value: 1))
(print (map init-keyword (slot-descriptors <p>)))"
  "#(y: z: #f)
")
 ;; make <generic-function> takes a symbol (or #f) as its debug-name:.
 (generic-function-debug-name-not-a-symbol
  "(print (make <generic-function> required: 1 debug-name: 'foo))
(make <generic-function> required: 1 debug-name: \"foo\")"
  "{the generic function foo}
"
  :error "make: debug-name: must be a symbol: \"foo\"")
 ;; Both answer the methods in definition order; a redefined method moves
 ;; to the end.
 (generic-function-methods-order
  "(define-generic-function g (x))
(define-method g ((x <integer>)) 1)
(define-method g ((x <string>)) 2)
(define-method g ((x <symbol>)) 3)
(define-method g ((x <integer>)) 4)
(print (map (method (m) (first (method-specializers m))) (generic-function-methods g)))
(print (g 1))"
  "({the class <string>} {the class <symbol>} {the class <integer>})
4
")
 ;; Chapter 10: with too few sample arguments no method is applicable.
 (sorted-applicable-methods-few-arguments
  "(define-generic-function h (x y))
(define-method h ((x <integer>) (y <integer>)) 1)
(print (sorted-applicable-methods h 3))
(print (sorted-applicable-methods h))
(print (size (sorted-applicable-methods h 3 4)))"
  "()
()
1
")
 ;; The reflective functions of chapter 10 say what is wrong with their
 ;; arguments; find-method takes a list.
 (reflection-argument-checks
  "(define-macro try (e)
  `(print (handler-case ,e
            ((<error> condition: c)
             (apply format #f (condition-format-string c) (condition-format-arguments c))))))
(define-generic-function g (x))
(define-method g ((x <string>)) x)
(try (find-method g (list <string>)))
(try (find-method g (vector <string>)))
(try (find-method 5 '()))
(try (generic-function-methods 5))
(try (sorted-applicable-methods 5))
(try (method-specializers 5))
(try (function-arguments 5))
(try (applicable-method? 5))"
  "{a method on g}
\"not a list: #({the class <string>})\"
\"not a generic function: 5\"
\"not a generic function: 5\"
\"not a generic function: 5\"
\"not a method: 5\"
\"function-arguments: not a function: 5\"
\"not a function: 5\"
")
 ;; A definition takes effect when it is evaluated: a generic called before
 ;; its first define-method has no value, and a second define-method with
 ;; the same specializers replaces the first when it runs.
 (definitions-in-program-order
  "(define-method try (f)
  (handler-case (f)
    ((<error> condition: c)
     (apply format #f (condition-format-string c) (condition-format-arguments c)))))
(print (try (method () (double 10))))
(define-method double ((thing <number>)) (+ thing thing))
(define-method z () 'first)
(print (z))
(define-method z () 'second)
(print (z))
(print (try (method () (v))))
(define v (method () 1))"
  "\"the variable double@dylan-user has no value\"
first
second
\"the variable v@dylan-user has no value\"
")
 ;; Each method is added when its define-method runs: until then dispatch,
 ;; next-method and generic-function-methods do not see it.
 (methods-attached-when-run
  "(define-method try (f)
  (handler-case (f)
    ((<error> condition: c)
     (apply format #f (condition-format-string c) (condition-format-arguments c)))))
(define-method g ((x <integer>)) 'int)
(print (try (method () (g \"s\"))))
(define-method g ((x <string>)) 'str)
(print (g \"s\"))
(print (size (generic-function-methods g)))
(define-method h ((x <number>)) (list 'num))
(define-method h ((x <integer>)) (list 'int1 (next-method)))
(print (h 1))
(define-method h ((x <integer>)) (list 'int2 (next-method)))
(print (h 1))
(print (map (method (m) (first (method-specializers m))) (generic-function-methods h)))
(define-class <c> (<object>))
(print (try (method () (c-print (make <c>)))))
(define-method c-print (x) (format #f \"~s\" x))
(print (try (method () (c-print (make <c>)))))
(define-method print-object ((x <c>) s e) (display \"{a c}\" stream: s))
(print (try (method () (c-print (make <c>)))))
(define-method fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(print (fib 20))
(define-method outer () (define-method inner () 'inner) 'outer)
(print (try (method () (inner))))
(print (outer))
(print (inner))"
  "\"no applicable method for g\"
str
2
(int1 (num))
(int2 (num))
({the class <number>} {the class <integer>})
\"the variable c-print@dylan-user has no value\"
\"{instance of <c>}\"
\"{a c}\"
6765
\"the variable inner@dylan-user has no value\"
outer
inner
")
 ;; A class, its slot accessors and a define-generic-function have no value
 ;; until their definition is evaluated.
 (classes-defined-when-run
  "(define-method try (f)
  (handler-case (f)
    ((<error> condition: c)
     (apply format #f (condition-format-string c) (condition-format-arguments c)))))
(print (try (method () <c>)))
(print (try (method () (make <c>))))
(print (try (method () (x 1))))
(define-class <c> (<object>) (x init-value: 1))
(print (x (make <c>)))
(print (try (method () g)))
(define-generic-function g (a))
(print g)"
  "\"the variable <c>@dylan-user has no value\"
\"the variable <c>@dylan-user has no value\"
\"the variable x@dylan-user has no value\"
1
\"the variable g@dylan-user has no value\"
{the generic function g}
")
 ;; A slot's options may name the class being defined.
 (class-names-itself-in-slots
  "(define-method try (f)
  (handler-case (f)
    ((<error> condition: c)
     (apply format #f (condition-format-string c) (condition-format-arguments c)))))
(define-class <a> (<object>) (x init-value: <a>))
(print (x (make <a>)))
(define-class <e> (<object>) (y init-function: (method () <e>)))
(print (y (make <e>)))
(print (try (method () (begin (print <d>) (define-class <d> (<object>))))))"
  "{the class <a>}
{the class <e>}
\"the variable <d>@dylan-user has no value\"
")
 ;; Chapter 5: only define-class and define-generic-function make read-only
 ;; variables.  A define-method variable and the library's functions can
 ;; be assigned, and a call reads the variable when it runs; define-method
 ;; on a variable holding a function that is not generic is an error.
 (set-of-method-variable
  "(define-method try (f)
  (handler-case (f)
    ((<error> condition: c)
     (apply format #f (condition-format-string c) (condition-format-arguments c)))))
(define-method f (x) x)
(define-method call-f () (f 1))
(print (f 1))
(set! f (method (x) 2))
(print (f 1))
(print (call-f))
(print (try (method () (define-method f ((x <integer>)) 3))))
(define-method g (x) 'g)
(define-method h (x) 'h)
(set! f g)
(print (f 1))
(define-generic-function dg (x))
(define old-setter (setter element))
(define old-car car)
(define old-plus +)
(print (setter element))
(print (try (method () (set! (setter element) (method (v i x) (list 'set i x))))))
(print (try (method () (set! (element (vector 1 2) 0) 5))))
(print (try (method () (set! car cdr))))
(print (try (method () (car '(1 2)))))
(print (try (method () (set! + -))))
(print (try (method () (+ 5 2))))
(set! (setter element) old-setter)
(set! car old-car)
(set! + old-plus)
(print (list (car '(1 2)) (+ 5 2) (bind ((v (vector 1 2))) (set! (element v 0) 5) v)))"
  "1
2
2
\"f is defined both as a variable and as a generic function\"
g
{the generic function (setter element)}
{an anonymous method}
5
{method cdr}
(2)
{method -}
3
(1 7 #(5 2))
")
 ;; The library's functions assigned and restored; the library's own calls
 ;; are unaffected.
 (set-of-library-variable
  "(define-method try (f)
  (handler-case (f)
    ((<error> condition: c)
     (apply format #f (condition-format-string c) (condition-format-arguments c)))))
(define-method keep (name f) (print (try (method () (bind ((old (f))) (list name old))))))
(print (try (method () (bind ((old element)) (set! element 5) (set! element old) 'element-ok))))
(print (try (method () (bind ((old make)) (set! make 5) (set! make old) 'make-ok))))
(print (try (method () (bind ((old signal)) (set! signal 5) (set! signal old) 'signal-ok))))
(print (try (method () (bind ((old +)) (set! + -) (bind ((r (list (+ 5 2) (reduce1 + '(1 2 3))))) (set! + old) r)))))
(print (try (method () (bind ((old +)) (set! + -) (bind ((r (list (+ 5 2) (reduce1 + '(1 2 3))))) (set! + old) r)))))
(print (try (method () (bind ((old car)) (set! car cdr) (bind ((r (list (car (quote (1 2))) (first (quote (1 2)))))) (set! car old) r)))))"
  "element-ok
make-ok
signal-ok
(3 -4)
(3 -4)
((2) 1)
")
 ;; A call inside a method reads the variable too, so an assignment changes
 ;; the calls that follow it, even recursive ones; the old function still
 ;; works through another variable.
 (calls-after-set
  "(define-method try (f)
  (handler-case (f)
    ((<error> condition: c)
     (apply format #f (condition-format-string c) (condition-format-arguments c)))))
(print (try (method () (set! h 1))))
(define-method h () 1)
(define-method f (x) (if (> x 0) (f (- x 1)) 'done))
(print (f 3))
(set! f (method (x) 'other))
(print (f 3))
(define-method count-down (x) (if (> x 0) (count-down (- x 1)) 'done))
(define-method user () (count-down 2))
(print (user))
(set! count-down (method (x) (list 'replaced x)))
(print (user))
(define-method size ((x <integer>)) 'int-size)
(print (list (size 5) (size '(1 2))))
(bind ((old size))
  (set! size (method (x) 'replaced))
  (print (list (size 5) (size '(1 2))))
  (set! size old))
(print (list (size 5) (size '(1 2))))
(define-method g ((x <integer>)) 'int)
(define-method g ((x <string>)) 'str)
(define saved g)
(set! g (method (x) 'any))
(print (list (g 1) (saved 1) (saved \"s\")))
(set! g saved)
(print (list (g 1) (g \"s\")))
(print (try (method () (g 1.5))))"
  "\"the variable h@dylan-user has no value\"
done
other
done
(replaced 2)
(int-size 2)
(replaced replaced)
(int-size 2)
(any int str)
(int str)
\"no applicable method for g\"
")
 ;; Chapter 6: a generic function may be declared again with the same
 ;; parameters; a declaration that differs is an error when it runs.
 (generic-declared-again
  "(define-method try (f)
  (handler-case (f)
    ((<error> condition: c)
     (apply format #f (condition-format-string c) (condition-format-arguments c)))))
(define-generic-function g (x #key a))
(define-method g ((x <integer>) #key a) (list 'int a))
(print (define-generic-function g (x #key a)))
(print (g 1 a: 3))
(print (try (method () (define-generic-function g (x y)))))
(print (try (method () (define-generic-function g (x #rest r)))))
(print (try (method () (define-generic-function g (x #key b)))))
(print (g 1 a: 4))
(define-method h (x) x)
(print (define-generic-function h (x)))
(print (try (method () (define-generic-function h (x y)))))
(define-generic-function (setter foo) (v x))
(define-generic-function (setter foo) (v x))
(print (try (method () (define-generic-function (setter foo) (v)))))
(define-method nested () (define-generic-function q (a b)) 'ok)
(define-generic-function q (a b))
(print (nested))"
  "g
(int 3)
\"define-generic-function: g is inconsistent with the existing generic function\"
\"define-generic-function: g is inconsistent with the existing generic function\"
\"define-generic-function: g is inconsistent with the existing generic function\"
(int 4)
h
\"define-generic-function: h is inconsistent with the existing generic function\"
\"define-generic-function: (setter foo) is inconsistent with the existing generic function\"
ok
")
 ;; Chapter 6: define-method makes a method as method does, so one inside
 ;; a method closes over its locals; each time it runs it makes a new one,
 ;; which replaces the last.
 (methods-close-over-locals
  "(define-method make-adder (n)
  (define-method adder (x) (+ x n))
  'made)
(print (make-adder 10))
(print (adder 1))
(print (make-adder 20))
(print (adder 1))
(define-method counter-maker ()
  (bind ((count 0))
    (define-method tick () (set! count (+ count 1)) count)
    'ok))
(counter-maker)
(print (list (tick) (tick)))
(counter-maker)
(print (tick))
(define-method spec-maker (c)
  (define-method sp ((x <integer>)) (list 'int c))
  (define-method sp ((x <string>)) (list 'str c)))
(spec-maker 1)
(print (list (sp 5) (sp \"s\")))
(spec-maker 2)
(print (list (sp 5) (sp \"s\")))"
  "made
11
made
21
(1 2)
1
((int 1) (str 1))
((int 2) (str 2))
")
 ;; Such methods have no value before their definition runs, are found
 ;; by reflection, are called as values, and take part in next-method.
 (method-closures-reflect
  "(define-method try (f)
  (handler-case (f)
    ((<error> condition: c)
     (format #f \"~a\" c))))
(print (try (method () (adder 1))))
(define-method make-adder (n)
  (define-method adder ((x <integer>)) (+ x n))
  'made)
(make-adder 1) (make-adder 2) (make-adder 3)
(print (size (generic-function-methods adder)))
(define m (first (generic-function-methods adder)))
(print (list (m 10) (method-specializers m)))
(print m)
(print (try (method () (m \"s\"))))
(define-method r ((x <number>)) (list 'num x))
(define-method layer (tag)
  (define-method r ((x <integer>)) (list tag (next-method))))
(layer 'a)
(print (r 1))
(layer 'b)
(print (r 1))
(define-method r ((x <integer>)) (list 'top (next-method)))
(print (r 1))
(layer 'c)
(print (r 1))
(print (map (method (m) (first (method-specializers m))) (generic-function-methods r)))
(define-method two-levels (a)
  (bind ((f (method (b) (define-method tl (c) (list a b c)) 'ok)))
    (f 2)))
(two-levels 1)
(print (tl 3))
(define-method keep-next (k)
  (define-method kn ((x <integer>)) (bind ((nm next-method)) (list k (nm))))
  (define-method kn ((x <number>)) (list 'num k)))
(keep-next 7)
(print (kn 1))
(define-method gc-churn (i) (define-method churn (x) (list i x)) (make <vector> size: 1000))
(for ((i 0 (+ i 1))) ((= i 2000)) (gc-churn i))
(print (churn 'z))"
  "\"the variable adder@dylan-user has no value\"
1
(13 ({the class <integer>}))
{a method on adder}
\"\\\"s\\\" is not an instance of {the class <integer>}\"
(a (num 1))
(b (num 1))
(top (num 1))
(c (num 1))
({the class <number>} {the class <integer>})
(1 2 3)
(7 (num 7))
(1999 z)
")
)
