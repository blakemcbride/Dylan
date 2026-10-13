;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conformance tests for chapter 11 of the 1992 manual, "Classes"
;;;; (manual/Dylan-1992/book.annotated/ch11.html), in the form of
;;;; tests/codegen/runner.lisp.  The chapter's own examples are the
;;;; singleton ones at its end; everything else is tested from the
;;;; specification of each class and function.

(
 ;; <class> [Abstract Class]: "All classes (including <class>) are general
 ;; instances of <class>.  <class> is a subclass of <object>."  From the
 ;; specification.
 (ch11-class
  "(define-class <thing> (<object>))
   (print (list (instance? <class> <class>) (instance? <object> <class>)
                (instance? <thing> <class>) (instance? <integer> <class>)))
   (print (list (subclass? <class> <object>) (instance? 3 <class>) (instance? (make <thing>) <class>)))"
  "(#t #t #t #t)
(#t #f #f)
")

 ;; instance? [Generic Function]: true if object is a general instance of
 ;; class.  From the specification.
 (ch11-instance-p
  "(define-class <a> (<object>))
   (define-class <b> (<a>))
   (define x (make <b>))
   (print (list (instance? x <b>) (instance? x <a>) (instance? x <object>)))
   (print (list (instance? (make <a>) <b>) (instance? 3 <a>)))
   (print (list (instance? 3 <integer>) (instance? 3 <number>) (instance? 3 <object>)
                (instance? 3 <string>) (instance? \"abc\" <string>) (instance? 'a <symbol>)))"
  "(#t #t #t)
(#f #f)
(#t #t #t #f #t #t)
")

 ;; as [Generic Function], "Coercing and Copying Objects": coercion between
 ;; numeric types, between integers and characters, and between collection
 ;; types; the new collection has as many elements as the source and,
 ;; between sequences, in the same order; an object already an instance of
 ;; the class is returned unchanged.  From the specification.
 (ch11-as
  "(print (as <list> #(1 2 3)))
   (print (as <vector> '(1 2 3)))
   (print (as <string> '(#\\a #\\b #\\c)))
   (print (as <list> \"abc\"))
   (print (as <double-float> 1/2))
   (print (as <character> (as <integer> #\\a)))
   (print (instance? (as <integer> #\\z) <integer>))
   (bind ((l (list 1 2)) (s \"xy\"))
     (print (list (id? (as <list> l) l) (id? (as <string> s) s) (id? (as <integer> 7) 7))))"
  "(1 2 3)
#(1 2 3)
\"abc\"
(#\\a #\\b #\\c)
0.5
#\\a
#t
(#t #t #t)
")

 ;; shallow-copy and class-for-copy [Generic Functions]: a new object with
 ;; the same contents, which are not copied; Dylan has methods for
 ;; collections, and for other classes the programmer provides one;
 ;; class-for-copy's default calls object-class.  From the specification.
 (ch11-shallow-copy
  "(define inner (list 1 2))
   (define l (list 'a inner))
   (define c (shallow-copy l))
   (print (list c (= c l) (id? c l) (id? (second c) inner)))
   (define v (vector 'x inner))
   (define cv (shallow-copy v))
   (print (list cv (id? cv v) (id? (second cv) inner)))
   (define s \"hello\")
   (define cs (shallow-copy s))
   (set! (element cs 0) #\\j)
   (print (list s cs))
   (define-class <point> (<object>) (px init-keyword: x:) (py init-keyword: y:))
   (define-method shallow-copy ((p <point>))
     (make (class-for-copy p) x: (px p) y: (py p)))
   (define p (make <point> x: 1 y: inner))
   (define q (shallow-copy p))
   (print (list (id? p q) (px q) (id? (py q) inner) (instance? q <point>)))
   (print (list (id? (class-for-copy p) <point>) (id? (class-for-copy p) (object-class p))
                (id? (class-for-copy 'a) (object-class 'a))))"
  "((a (1 2)) #t #f #t)
(#(x (1 2)) #f #t)
(\"hello\" \"jello\")
(#f 1 #t #t)
(#t #t #t)
")

 ;; "Reflective Operations on Classes": subclass? (direct or indirect, or
 ;; the same class), object-class (the class of which the object is a
 ;; direct instance), all-superclasses (the class first, <object> last),
 ;; direct-superclasses (in the order given to define-class) and
 ;; direct-subclasses.  From the specification.
 (ch11-reflection
  "(define-class <a> (<object>))
   (define-class <b> (<a>))
   (define-class <c> (<b>))
   (define-class <m> (<object>))
   (define-class <bm> (<b> <m>))
   (print (list (subclass? <b> <a>) (subclass? <c> <a>) (subclass? <a> <a>) (subclass? <a> <b>)
                (subclass? <m> <a>) (subclass? <integer> <number>) (subclass? <number> <integer>)))
   (print (list (id? (object-class (make <c>)) <c>) (id? (object-class (make <c>)) <b>)
                (id? (object-class #t) (object-class #f))))
   (bind ((s (as <list> (all-superclasses <c>))))
     (print (list (size s) (id? (first s) <c>) (id? (second s) <b>) (id? (third s) <a>)
                  (id? (last s) <object>))))
   (bind ((s (as <list> (all-superclasses <bm>))))
     (print (list (id? (first s) <bm>) (id? (last s) <object>)
                  (every? (method (k) (if (member? k s) #t #f)) (list <b> <a> <m>)))))
   (bind ((s (as <list> (direct-superclasses <bm>))))
     (print (list (size s) (id? (first s) <b>) (id? (second s) <m>))))
   (bind ((s (as <list> (direct-superclasses <c>))))
     (print (list (size s) (id? (first s) <b>))))
   (bind ((s (as <list> (direct-subclasses <b>))))
     (print (list (size s) (if (member? <c> s) #t #f) (if (member? <bm> s) #t #f) (member? <a> s))))
   (print (empty? (direct-subclasses <c>)))"
  "(#t #t #t #f #f #t #f)
(#t #f #t)
(4 #t #t #t #t)
(#t #t #t)
(2 #t #t)
(1 #t)
(2 #t #t #f)
#t
")

 ;; make-read-only [Generic Function]: answers the class; "This does not
 ;; affect the instances of the class (i.e., the slots of instances can
 ;; still be written)", nor class-allocated slots.  From the specification.
 (ch11-make-read-only
  "(define-class <r> (<object>) (rx init-value: 1) (rc allocation: class init-value: 5))
   (print (id? (make-read-only <r>) <r>))
   (define r (make <r>))
   (set! (rx r) 2)
   (set! (rc r) 6)
   (print (list (rx r) (rc r) (rc (make <r>))))"
  "#t
(2 6 6)
")

 ;; seal [Generic Function]: answers the class, and "The functions
 ;; all-superclasses, direct-superclasses, and slot-descriptors will return
 ;; an empty sequence when called on a sealed class."  From the
 ;; specification.
 (ch11-seal
  "(define-class <base> (<object>))
   (define-class <s> (<base>) (sx init-value: 1))
   (print (list (empty? (all-superclasses <s>)) (empty? (slot-descriptors <s>))))
   (print (id? (seal <s>) <s>))
   (print (list (empty? (all-superclasses <s>)) (empty? (direct-superclasses <s>))
                (empty? (slot-descriptors <s>))))
   (print (sx (make <s>)))"
  "(#f #f)
#t
(#t #t #t)
1
")

 ;; "Slots and Slot Descriptors": <slot-descriptor>, slot-descriptors,
 ;; slot-descriptor (#f when the getter does not access a slot of the
 ;; instance; it works for virtual slots), slot-getter, slot-setter,
 ;; slot-type (<object> when none was given), slot-allocation (a symbol),
 ;; init-value (two values; #f and #f when none), init-function (#f when
 ;; none), init-keyword (#f when none).  From the specification.
 (ch11-slot-descriptors
  "(define fresh-list (method () (list 'fresh)))
   (define-class <p> (<object>)
     (sx type: <integer> init-value: 3 init-keyword: x:)
     (sy init-function: fresh-list)
     (sz setter: #f)
     (sc allocation: class init-value: 0)
     (sv allocation: virtual))
   (define-method sv ((p <p>)) 42)
   (define-generic-function unrelated (p))
   (define p (make <p>))
   (define dx (slot-descriptor p sx))
   (define dy (slot-descriptor p sy))
   (define dz (slot-descriptor p sz))
   (define dc (slot-descriptor p sc))
   (define dv (slot-descriptor p sv))
   (print (list (instance? dx <slot-descriptor>) (instance? dv <slot-descriptor>)
                (slot-descriptor p unrelated)))
   (bind ((all (as <list> (slot-descriptors <p>))))
     (print (list (size all) (every? (method (d) (if (member? d all) #t #f)) (list dx dy dz dc dv)))))
   (print (list (id? (slot-getter dx) sx) (id? (slot-setter dx) (setter sx)) (slot-setter dz)))
   (print (list (id? (slot-type dx) <integer>) (id? (slot-type dy) <object>)))
   (print (list (slot-allocation dx) (slot-allocation dc) (slot-allocation dv)))
   (bind ((v s (init-value dx))) (print (list v s)))
   (bind ((v s (init-value dy))) (print (list v s)))
   (print (list (id? (init-function dy) fresh-list) (init-function dx)))
   (print (list (init-keyword dx) (init-keyword dy)))"
  "(#t #t #f)
(5 #t)
(#t #t #f)
(#t #t)
(instance class virtual)
(3 #t)
(#f #f)
(#t #f)
(x: #f)
")

 ;; slot-value and (setter slot-value) [Generic Functions]: access the
 ;; slot's storage; the setter returns the new value and enforces the
 ;; slot's type; both signal an error for a virtual slot.  From the
 ;; specification.
 (ch11-slot-value
  "(define-class <p> (<object>)
     (sx type: <integer> init-value: 3)
     (sc allocation: class init-value: 0)
     (sv allocation: virtual))
   (define-method sv ((p <p>)) 42)
   (define p (make <p>))
   (define dx (slot-descriptor p sx))
   (define dc (slot-descriptor p sc))
   (define dv (slot-descriptor p sv))
   (print (list (slot-value p dx) (slot-value p dc)))
   (print (set! (slot-value p dx) 10))
   (print (list (sx p) (slot-value p dx)))
   (set! (sx p) 11)
   (print (slot-value p dx))
   (set! (slot-value p dc) 7)
   (print (list (sc p) (sc (make <p>))))
   (print (handler-case (set! (slot-value p dx) 'bad) ((<error>) 'error)))
   (print (sx p))
   (print (handler-case (slot-value p dv) ((<error>) 'error)))
   (print (handler-case (set! (slot-value p dv) 1) ((<error>) 'error)))"
  "(3 0)
10
(10 10)
11
(7 7)
error
11
error
error
")

 ;; Singletons, with the chapter's examples.  double's (double 10) => 20
 ;; needs a method for numbers, as chapter 6 defines.  (singleton object)
 ;; is equivalent to (make <singleton> object: object); without object:,
 ;; make signals an error (from the specification).
 (ch11-singleton
  "(define-method double ((thing <number>)) (+ thing thing))
   (define-method double ((thing (singleton 'cup)))
     'pint)
   (print (double 'cup))
   (print (double 10))
   (define-method factorial ((num <integer>))
     (* num (factorial (- num 1))))
   (define-method factorial ((num (singleton 0)))
     1)
   (print (factorial 5))
   (print (list (instance? (singleton 3) <singleton>) (subclass? <singleton> <object>)))"
  "pint
20
120
(#t #t)
")

 ;; <singleton> [Instantiable Class]: init-keyword object:, with no
 ;; default -- "If it is not supplied, an error will be signaled."
 ;; (singleton object) is equivalent to (make <singleton> object: object).
 ;; From the specification.
 (ch11-make-singleton
  "(define s (make <singleton> object: 'tea))
   (print (instance? s <singleton>))
   (print (handler-case (make <singleton>) ((<error>) 'error)))"
  "#t
error
")

 ;; (make <singleton> object: x) as a specializer is the same as
 ;; (singleton x); a singleton method is more specific than a class
 ;; method.  From the specification.
 (ch11-make-singleton-specializer
  "(define-method kind ((x <symbol>)) 'symbol)
   (define-method kind ((x (make <singleton> object: 'tea))) 'tea)
   (print (list (kind 'tea) (kind 'coffee)))"
  "(tea symbol)
")

 ;; add-slot and remove-slot change a class at run time, which a static
 ;; program cannot do (README, CompilerPlan.md); the compiler reports them.
 (ch11-add-slot
  "(define-class <p> (<object>))
   (add-slot <p> getter: foo)"
  ""
  :compile-error "add-slot cannot be used")
 (ch11-remove-slot
  "(define-class <p> (<object>) (foo init-value: 1))
   (remove-slot <p> (slot-descriptor (make <p>) foo))"
  ""
  :compile-error "remove-slot cannot be used")
)
