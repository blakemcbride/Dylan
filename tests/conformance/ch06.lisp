;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conformance tests for chapter 6 of the 1992 Dylan manual,
;;;; "Introduction to Functions and Classes".  Expected results are the
;;;; manual's own, or, where it shows none, what its text specifies (said
;;;; so in the comment).  Errors the manual says are signaled when a
;;;; definition is made are compile errors here, since a program's
;;;; classes and generic functions are fixed when it is compiled.

(
 ;; 6 Generic Functions and Methods: define-method (the double example)
 (ch06-double
  "(define-method double ((thing <number>))
     (+ thing thing))
   (define-method double ((thing <sequence>))
     (concatenate thing thing))
   (print (instance? double <generic-function>))
   (print (double 10))
   (print (double \"the rain in Spain.\"))
   (print (double '(a b c)))"
  "#t
20
\"the rain in Spain.the rain in Spain.\"
(a b c a b c)
")

 ;; 6 Generic Functions and Methods: with a method only for <number>,
 ;; double cannot be called on a string
 (ch06-double-no-method
  "(define-method double ((thing <number>))
     (+ thing thing))
   (print (double 10))
   (print (handler-case (double \"the rain in Spain.\") ((<error>) 'error)))"
  "20
error
")

 ;; 6 define-method: a method with the exact same specializers replaces
 ;; the old one (specification)
 (ch06-define-method-replaces
  "(define-method which ((x <integer>)) 'first)
   (define-method which ((x <string>)) 'string)
   (define-method which ((x <integer>)) 'second)
   (print (which 1))
   (print (which \"a\"))"
  "second
string
")

 ;; 6 define-method: the variable already holds something besides a
 ;; generic function (specification)
 (ch06-define-method-not-generic
  "(define double 3)
   (define-method double ((thing <number>)) (+ thing thing))"
  ""
  :compile-error "")

 ;; 6 define-generic-function: generic functions declared first, then
 ;; given methods; the body's parameters are lexical variables
 ;; (specification)
 (ch06-define-generic-function
  "(define-generic-function area (shape))
   (define-method area ((s <integer>)) (* s s))
   (define-method area ((s <list>)) (* (first s) (second s)))
   (print (instance? area <generic-function>))
   (print (area 3))
   (print (area '(2 5)))"
  "#t
9
10
")

 ;; 6 define-generic-function: #key means the arguments after the
 ;; required ones must be keyword/value pairs, whether or not the
 ;; methods specify #key; #rest does not require that (specification)
 (ch06-define-generic-function-key-rest
  "(define-generic-function gk (a #key))
   (define-method gk (a #rest r) r)
   (define-generic-function gr (a #rest r))
   (define-method gr (a #rest r) r)
   (print (gk 1 b: 2))
   (print (handler-case (gk 1 2) ((<error>) 'error)))
   (print (gr 1 2 3))
   (print (gr 1))"
  "(b: 2)
error
(2 3)
()
")

 ;; 6 Parameter List Syntax: the sample parameter lists (specification)
 (ch06-parameter-lists
  "(define-method p1 (a b c) (list a b c))
   (define-method p2 ((a <list>) b #rest c) (list a b c))
   (define-method p3 (#rest a) a)
   (define-method p4 (a b #key foo bar) (list a b foo bar))
   (define-method p5 (#key bar baz bim) (list bar baz bim))
   (define-method p6 (#rest all #key fee fi) (list all fee fi))
   (print (p1 1 2 3))
   (print (p2 '(1) 2 3 4))
   (print (p3 1 2))
   (print (p4 1 2 bar: 4))
   (print (p5 bim: 3 bar: 1))
   (print (p6 fi: 2 fee: 1))
   (print (handler-case (p1 1 2) ((<error>) 'error)))
   (print (handler-case (p2 #(1) 2) ((<error>) 'error)))"
  "(1 2 3)
((1) 2 (3 4))
(1 2)
(1 2 #f 4)
(1 #f 3)
((fi: 2 fee: 1) 1 2)
error
error
")

 ;; 6 Parameter List Syntax: an explicit #next parameter, and the
 ;; next-method parameter define-method inserts (specification)
 (ch06-next-method-parameter
  "(define-method nm ((x <number>)) 'number)
   (define-method nm ((x <integer>) #next more) (list 'integer (more)))
   (define-method nn ((x <number>)) 'number)
   (define-method nn ((x <integer>)) (list 'integer (next-method)))
   (print (nm 1))
   (print (nn 1))
   (print (nn 1.5))"
  "(integer number)
(integer number)
number
")

 ;; 6.x Rest Parameters
 (ch06-rest-parameters
  "(define-method show-rest (a #rest b)
     (print a)
     (print b)
     #t)
   (print (show-rest 10 20 30 40))
   (print (show-rest 10))"
  "10
(20 30 40)
#t
10
()
#t
")

 ;; 6.x Keyword Parameters: percolate (the manual does not define
 ;; make-coffee; here it lists its arguments) and the separate-keyword
 ;; form of layout; results from the specification
 (ch06-keyword-parameters
  "(define-method make-coffee (brand cups strength)
     (list brand cups strength))
   (define-method percolate (#key (brand 'maxwell-house)
                                  (cups 4)
                                  (strength 'strong))
     (make-coffee brand cups strength))
   (define-method layout (widget #key (position: the-pos)
                                      (size: the-size))
     (list widget the-pos the-size))
   (print (percolate brand: 'folgers cups: 10))
   (print (percolate strength: 'weak
                     brand: 'tasters-choice
                     cups: 1))
   (print (percolate))
   (print (layout 'my-widget position: '(10 10) size: '(30 50)))
   (print (layout 'my-widget size: '(1 2)))"
  "(folgers 10 strong)
(tasters-choice 1 weak)
(maxwell-house 4 strong)
(my-widget (10 10) (30 50))
(my-widget #f (1 2))
")

 ;; 6.x Keyword Parameters: defaults are evaluated each time, in a scope
 ;; holding the preceding parameters; (keyword parameter default); an
 ;; unsupplied keyword with no default is #f; the leftmost occurrence of
 ;; a repeated keyword is used (specification)
 (ch06-keyword-defaults
  "(define counter 0)
   (define-method next-count () (set! counter (+ counter 1)) counter)
   (define-method kd (a #rest r #key (b (+ a 1)) (c (* b 2)) (d: dd (list a r)) e)
     (list a b c dd e))
   (define-method ke (#key (n (next-count))) n)
   (print (kd 1))
   (print (kd 1 b: 5))
   (print (kd 1 c: 0 d: 'x e: 'y))
   (print (kd 1 b: 5 b: 7))
   (print (list (ke) (ke) (ke n: 10) (ke)))"
  "(1 2 4 (1 ()) #f)
(1 5 10 (1 (b: 5)) #f)
(1 2 0 x y)
(1 5 10 (1 (b: 5 b: 7)) #f)
(1 2 10 3)
")

 ;; 6.x Keyword Parameters: a separately named parameter may be a setter
 ;; variable (specification)
 (ch06-keyword-setter-parameter
  "(define-method kset (#key (value: (setter thing) 3))
     (setter thing))
   (print (kset value: 'v))
   (print (kset))"
  "v
3
")

 ;; 6.x Keyword Parameters: required arguments are filled first (show-keys)
 (ch06-show-keys
  "(define-method show-keys (req1 req2 #key foo)
     (format #t \"requireds: ~a ~a~%\" req1 req2)
     (format #t \"key: ~a\" foo)
     #t)
   (print (show-keys 'one 'two foo: 'three))
   (print (show-keys foo: 'three))"
  "requireds: one two
key: three#t
requireds: foo: three
key: #f#t
")

 ;; 6.x Keyword Argument Checking: the label example.  The manual's
 ;; library has no length function (chapter 12 calls it size), so the
 ;; example's (length x) is written (size x) here.
 (ch06-keyword-checking-label
  "(define-method label ((x <object>) #key price)
     (list price x))
   (define-method label ((x <sequence>) #key unit-price)
     (add x (* unit-price (size x))))
   (define-method label ((x <list>) #rest info #key calories)
     (add x calories))
   (print (handler-case (label 'grape price: 189 unit-price: 2) ((<error>) 'error)))
   (print (label 'grape price: 189))
   (print (label (vector 3 4 5) price: 189 unit-price: 2))
   (print (handler-case (label (vector 3 4 5) protein: 7 fat: 8 calories: 9)
            ((<error>) 'error)))
   (print (label (list 3 4 5) protein: 7 fat: 8 calories: 9))"
  "error
(189 grape)
#(6 3 4 5)
error
(9 3 4 5)
")

 ;; 6.x Keyword Argument Checking: a method called directly accepts only
 ;; the keywords it mentions, or any with #rest; with #key the arguments
 ;; after the required ones must be keyword/value pairs (specification)
 (ch06-keyword-checking-direct
  "(define m1 (method (a #key b) (list a b)))
   (define m2 (method (a #rest r) (list a r)))
   (define-method gm (a #key b) (list a b))
   (print (m1 1 b: 2))
   (print (handler-case (m1 1 c: 2) ((<error>) 'error)))
   (print (handler-case (m1 1 b:) ((<error>) 'error)))
   (print (handler-case (m1 1 2 3) ((<error>) 'error)))
   (print (m2 1 c: 2))
   (print (handler-case (gm 1 b:) ((<error>) 'error)))
   (print (handler-case (gm 1 2 3) ((<error>) 'error)))
   (print (handler-case (gm 1 c: 3) ((<error>) 'error)))"
  "(1 2)
error
error
error
(1 (c: 2))
error
error
error
")

 ;; 6.x Combining #rest and #key
 (ch06-rest-and-key
  "(define-method test (the-req #rest the-rest
                                #key a b)
     (print the-req)
     (print the-rest)
     (print a)
     (print b))
   (test 1 a: 2 b: 3 c: 4)
   (print (handler-case (test 1 2) ((<error>) 'error)))"
  "1
(a: 2 b: 3 c: 4)
2
3
error
")

 ;; 6.x Parameter List Congruency: congruent groups (specification)
 (ch06-congruent
  "(define-generic-function g1 (a b #key))
   (define-method g1 ((x <integer>) y #key foo) (list 'i x y foo))
   (define-method g1 ((a <symbol>) b #key bar) (list 's a b bar))
   (define-method g1 ((a <list>) b #rest l) (list 'l a b l))
   (define-method g1 ((a <vector>) b #rest l #key quux) (list 'v a b l quux))
   (define-generic-function g2 (a b))
   (define-method g2 ((x <integer>) y) (list x y))
   (define-generic-function g3 (a b c #key))
   (define-method g3 ((x <integer>) y c #key foo) (list x y c foo))
   (define-method g3 ((a <string>) b c #rest l #key quux) (list a b c l quux))
   (print (g1 1 2 foo: 3))
   (print (g1 'a 2 bar: 3))
   (print (g1 '(1) 2 z: 3))
   (print (g1 #(1) 2 quux: 3))
   (print (g2 1 2))
   (print (g3 1 2 3 foo: 4))
   (print (g3 \"a\" 2 3 quux: 4))"
  "(i 1 2 3)
(s a 2 3)
(l (1) 2 (z: 3))
(v #(1) 2 (quux: 3) 3)
(1 2)
(1 2 3 4)
(\"a\" 2 3 (quux: 4) 4)
")

 ;; 6.x Parameter List Congruency: different numbers of required
 ;; parameters are not congruent (specification)
 (ch06-not-congruent-required
  "(define-generic-function g2 (a b))
   (define-method g2 (a b c) a)"
  ""
  :compile-error "")

 ;; 6.x Parameter List Congruency: #key against none is not congruent
 ;; (specification)
 (ch06-not-congruent-key
  "(define-method g4 (a b #key) a)
   (define-method g4 ((a <integer>) b) a)"
  ""
  :compile-error "")

 ;; 6.x Specializing Required Parameters: classes and singletons; the
 ;; closest match is chosen (specification)
 (ch06-specializers
  "(define-method sp ((a <number>) (b <character>) c) 'number-char)
   (define-method sp ((a <integer>) (b <character>) c) 'integer-char)
   (define-method sp ((a <integer>) (b <character>) (c (singleton 0))) 'integer-char-zero)
   (define-method sr (a (b <string>) #rest c) (list 'string c))
   (define-method sv ((a <vector>) b #key foo bar) (list a b foo bar))
   (print (sp 1.5 #\\a 'x))
   (print (sp 1 #\\a 'x))
   (print (sp 1 #\\a 0))
   (print (handler-case (sp 'x #\\a 1) ((<error>) 'error)))
   (print (handler-case (sp 1 'y 1) ((<error>) 'error)))
   (print (sr 'x \"s\" 1 2))
   (print (handler-case (sr 'x 'y) ((<error>) 'error)))
   (print (sv #(1) 2 bar: 3))
   (print (handler-case (sv '(1) 2) ((<error>) 'error)))"
  "number-char
integer-char
integer-char-zero
error
error
(string (1 2))
error
(#(1) 2 #f 3)
error
")

 ;; 6.x Specializing Required Parameters: a specializer may be any
 ;; expression that evaluates to a class or a singleton, evaluated once
 ;; (specification)
 (ch06-specializer-expressions
  "(define my-class <string>)
   (define-method se ((a my-class)) 'string)
   (define-method se ((a (singleton 'x))) 'x)
   (define-method se ((a <object>)) 'other)
   (print (se \"s\"))
   (print (se 'x))
   (print (se 'y))"
  "string
x
other
")

 ;; 6.x Generic Functions with No Required Parameters: a single method;
 ;; adding one replaces the old (specification)
 (ch06-no-required-parameters
  "(define-method nr (#rest r) (list 'first r))
   (define-method nr (#rest r) (list 'second r))
   (print (nr 1 2))"
  "(second (1 2))
")

 ;; 6.x Slots: the <point> example; getters, setters, set!, and an
 ;; uninitialized slot (specification)
 (ch06-point
  "(define-class <point> (<object>)
     horizontal
     vertical)
   (define my-point (make <point>))
   (print (instance? my-point <point>))
   (print (id? (object-class my-point) <point>))
   (print (instance? horizontal <generic-function>))
   (print (instance? (setter vertical) <generic-function>))
   (print (handler-case (horizontal my-point) ((<error>) 'error)))
   ((setter horizontal) my-point 10)
   (print (horizontal my-point))
   (set! (horizontal my-point) 20)
   (set! (vertical my-point) 30)
   (print (list (horizontal my-point) (vertical my-point)))"
  "#t
#t
#t
#t
error
10
(20 30)
")

 ;; 6.x Defining New Classes: <menu>; each instance has its own slots
 ;; (specification)
 (ch06-menu
  "(define-class <menu> (<object>)
     title
     action)
   (define m1 (make <menu>))
   (define m2 (make <menu>))
   (set! (title m1) \"File\")
   (set! (title m2) \"Edit\")
   (set! (action m1) 'open)
   (print (list (title m1) (title m2) (action m1)))
   (print (instance? <menu> <class>))"
  "(\"File\" \"Edit\" open)
#t
")

 ;; 6.x Defining New Classes: at least one superclass, no duplicates
 ;; (specification)
 (ch06-no-superclass
  "(define-class <none> ())"
  ""
  :compile-error "")
 (ch06-duplicate-superclass
  "(define-class <dup> (<object> <object>))"
  ""
  :compile-error "")

 ;; 6.x Defining New Classes: the class variable is read-only
 ;; (specification)
 (ch06-class-variable-read-only
  "(define-class <ro> (<object>))
   (set! <ro> 3)"
  ""
  :compile-error "")

 ;; 6.x Slot Options: the <rectangle> example
 (ch06-rectangle
  "(define-class <rectangle> (<object>)
      (top type: <integer>
           init-value: 0
           init-keyword: top:)
      (left type: <integer>
            init-value: 0
            init-keyword: left:)
      (bottom type: <integer>
              init-value: 100
              init-keyword: bottom:)
      (right type: <integer>
             init-value: 100
             init-keyword: right:))
   (print (instance? <rectangle> <class>))
   (define my-rectangle (make <rectangle> top: 50 left: 50))
   (print (top my-rectangle))
   (print (bottom my-rectangle))
   (print (set! (bottom my-rectangle) 55))
   (print (bottom my-rectangle))
   (print (handler-case (set! (bottom my-rectangle) 'foo) ((<error>) 'error)))
   (print (bottom my-rectangle))
   (print (list (left my-rectangle) (right my-rectangle)))"
  "#t
50
100
55
55
error
55
(50 100)
")

 ;; 6.x Slot Options: type: is enforced when a slot is initialized
 ;; (specification)
 (ch06-slot-type-at-make
  "(define-class <typed> (<object>)
     (n type: <integer> init-keyword: n:))
   (print (n (make <typed> n: 3)))
   (print (handler-case (make <typed> n: 'foo) ((<error>) 'error)))"
  "3
error
")

 ;; 6.x Slot Options and Slot Initialization: init-value is one value
 ;; used over and over; init-function is called for each instance but not
 ;; when the init-keyword is supplied; with neither the slot is
 ;; uninitialized (specification)
 (ch06-slot-initialization
  "(define calls 0)
   (define-class <auto> (<object>)
     (color init-value: 'black init-keyword: color:)
     (parts init-value: (list 'wheel))
     (cargo init-function: (method () (set! calls (+ calls 1)) (list 'box))
            init-keyword: cargo:)
     (owner init-keyword: owner:))
   (define a (make <auto>))
   (define b (make <auto> color: 'red cargo: 'none owner: 'me))
   (define c (make <auto>))
   (print (list (color a) (color b)))
   (print (id? (parts a) (parts c)))
   (print (list (cargo a) (cargo b) (cargo c)))
   (print (id? (cargo a) (cargo c)))
   (print calls)
   (print (owner b))
   (print (handler-case (owner a) ((<error>) 'error)))"
  "(black red)
#t
((box) none (box))
#f
2
me
error
")

 ;; 6.x Slot Options: getter: and setter: name the generic functions,
 ;; and a slot-spec may omit the slot name (specification)
 (ch06-getter-setter-options
  "(define-class <acct> (<object>)
     (balance getter: acct-balance setter: set-acct-balance init-value: 0)
     (getter: acct-owner init-keyword: owner:))
   (define x (make <acct> owner: 'ann))
   (print (acct-balance x))
   (set-acct-balance x 40)
   (print (acct-balance x))
   (print (acct-owner x))"
  "0
40
ann
")

 ;; 6.x Slot Options: required-init-keyword: (specification)
 (ch06-required-init-keyword
  "(define-class <person> (<object>)
     (name required-init-keyword: name:))
   (print (name (make <person> name: \"Ann\")))
   (print (handler-case (make <person>) ((<error>) 'error)))"
  "\"Ann\"
error
")

 ;; 6.x Slot Options: allocation: class and each-subclass (specification)
 (ch06-shared-allocation
  "(define-class <base> (<object>)
     (shared allocation: class init-value: 1)
     (per-class allocation: each-subclass init-value: 10))
   (define-class <derived> (<base>))
   (define b1 (make <base>))
   (define b2 (make <base>))
   (define d1 (make <derived>))
   (define d2 (make <derived>))
   (set! (shared b1) 2)
   (print (list (shared b1) (shared b2) (shared d1)))
   (set! (shared d2) 3)
   (print (list (shared b1) (shared b2) (shared d1)))
   (set! (per-class b1) 20)
   (print (list (per-class b1) (per-class b2) (per-class d1) (per-class d2)))
   (set! (per-class d1) 30)
   (print (list (per-class b1) (per-class b2) (per-class d1) (per-class d2)))"
  "(2 2 2)
(3 3 3)
(20 20 10 10)
(20 20 30 30)
")

 ;; 6.x Slot Options: allocation: constant (specification)
 (ch06-constant-allocation
  "(define-class <unit> (<object>)
     (size allocation: constant init-value: 8))
   (print (size (make <unit>)))
   (print (size (make <unit>)))"
  "8
8
")

 ;; 6.x Slot Options and Supporting Virtual Slots: Dylan defines the
 ;; generic functions but adds no methods; the programmer does
 ;; (specification)
 (ch06-virtual-slot
  "(define-class <rect> (<object>)
     (width init-keyword: width:)
     (height init-keyword: height:)
     (area allocation: virtual))
   (print (instance? area <generic-function>))
   (print (instance? (setter area) <generic-function>))
   (define r (make <rect> width: 2 height: 3))
   (print (handler-case (area r) ((<error>) 'error)))"
  "#t
#t
error
")
 (ch06-virtual-slot-methods
  "(define-class <rect> (<object>)
     (width init-keyword: width:)
     (height init-keyword: height:)
     (area allocation: virtual))
   (define-method area ((r <rect>)) (* (width r) (height r)))
   (define-method (setter area) ((r <rect>) new)
     (set! (height r) (/ new (width r)))
     new)
   (define r (make <rect> width: 2 height: 3))
   (print (area r))
   (set! (area r) 10)
   (print (list (width r) (height r) (area r)))
   (print (slot-initialized? r area))"
  "6
(2 5 10)
#t
")

 ;; 6.x make and initialize: make calls initialize with the instance and
 ;; the key-value pairs; make accepts initialize's keywords
 ;; (specification)
 (ch06-make-initialize
  "(define-class <counter> (<object>)
     (start init-keyword: start: init-value: 0)
     (doubled init-value: #f))
   (define-method initialize ((c <counter>) #key twice)
     (next-method)
     (set! (doubled c) twice))
   (define c1 (make <counter> start: 5 twice: 'yes))
   (define c2 (make <counter>))
   (print (instance? c1 <counter>))
   (print (list (start c1) (doubled c1)))
   (print (list (start c2) (doubled c2)))"
  "#t
(5 yes)
(0 #f)
")

 ;; 6.x Slot Initialization: the initial value is stored directly; the
 ;; setter generic function is not called (specification)
 (ch06-init-not-via-setter
  "(define setter-called #f)
   (define-class <thing> (<object>)
     (v init-keyword: v: init-value: 1))
   (define-class <sub-thing> (<thing>))
   (define-method (setter v) ((x <sub-thing>) new)
     (set! setter-called #t)
     (next-method))
   (define t1 (make <sub-thing> v: 5))
   (define t2 (make <sub-thing>))
   (print (list (v t1) (v t2) setter-called))
   (set! (v t1) 6)
   (print (list (v t1) setter-called))"
  "(5 1 #f)
(6 #t)
")

 ;; 6.x Testing the Initialization of a Slot: slot-initialized?
 ;; (specification)
 (ch06-slot-initialized
  "(define-class <point> (<object>)
     horizontal
     (vertical init-value: 0))
   (define-method unrelated (x) x)
   (define p (make <point>))
   (print (slot-initialized? p horizontal))
   (print (slot-initialized? p vertical))
   (set! (horizontal p) 1)
   (print (slot-initialized? p horizontal))
   (print (handler-case (slot-initialized? p unrelated) ((<error>) 'error)))"
  "#f
#t
#t
error
")

 ;; 6.x Filtered Slots: the <view> and <displaced-view> example, with
 ;; displace-transform and undisplace-transform supplied here
 ;; (specification)
 (ch06-filtered-slots
  "(define-class <view> (<object>)
     (position allocation: instance))
   (define-class <displaced-view> (<view>)
     (position allocation: virtual))
   (define-method displace-transform (p) (* p 10))
   (define-method undisplace-transform (p) (+ p 1))
   (define-method position ((v <displaced-view>))
     (displace-transform (next-method v)))
   (define-method (setter position) ((v <displaced-view>)
                                     new-position)
     (next-method v (undisplace-transform new-position)))
   (define plain (make <view>))
   (define shifted (make <displaced-view>))
   (set! (position plain) 5)
   (set! (position shifted) 5)
   (print (position plain))
   (print (position shifted))"
  "5
60
")

 ;; 6.x Filtered Slots: an instance slot caching a virtual one (the
 ;; <shape> example, with compute-image supplied here) (specification)
 (ch06-cached-slot
  "(define computed 0)
   (define-class <view> (<object>))
   (define-class <shape> (<view>)
     (image allocation: virtual)
     (cached-image allocation: instance init-value: #f))
   (define-method compute-image ((shape <shape>))
     (set! computed (+ computed 1))
     'picture)
   (define-method image ((shape <shape>))
     (or (cached-image shape)
         (set! (cached-image shape) (compute-image shape))))
   (define-method (setter image) ((shape <shape>) new-image)
     (set! (cached-image shape) new-image))
   (define s (make <shape>))
   (print (list (image s) (image s) computed))
   (set! (image s) 'other)
   (print (list (image s) computed))"
  "(picture picture 1)
(other 1)
")

 ;; 6.x Redefining Slots: a subclass respecifies an inherited slot; the
 ;; same slot inherited identically from two superclasses (specification)
 (ch06-redefining-slots
  "(define-class <a> (<object>)
     (s type: <number> init-value: 1 init-keyword: s:))
   (define-class <b> (<a>)
     (s type: <integer> init-value: 2 init-keyword: s:))
   (define-class <m1> (<object>) (k init-value: 'same))
   (define-class <m2> (<object>) (k init-value: 'same))
   (define-class <m3> (<m1> <m2>))
   (print (list (s (make <a>)) (s (make <b>)) (s (make <b> s: 3))))
   (print (handler-case (make <b> s: 1.5) ((<error>) 'error)))
   (print (k (make <m3>)))"
  "(1 2 3)
error
same
")

 ;; 6.x Defining individual slots: define-slot on a class gives every
 ;; instance the slot, initialized from its init-value or init-function;
 ;; the slot-name may be omitted when getter: is given (specification)
 (ch06-define-slot-class
  "(define-class <pt> (<object>) x)
   (define-slot <pt> z init-value: 7)
   (define-slot <pt> getter: pt-w init-function: (method () 'w))
   (define p (make <pt>))
   (print (z p))
   (print (pt-w p))
   (set! (z p) 8)
   (print (z p))"
  "7
w
8
")

 ;; 6.x Adding Slots After Instance Creation: instances made before the
 ;; slot is added get it, initialized from its init-value or
 ;; init-function (specification)
 (ch06-define-slot-existing-instance
  "(define-class <pt> (<object>) x)
   (define early (make <pt>))
   (define-slot <pt> z init-value: 7)
   (define-slot <pt> getter: pt-w init-function: (method () 'w))
   (print (z early))
   (print (pt-w early))"
  "7
w
")

 ;; 6.x Defining individual slots: define-slot on a singleton gives only
 ;; that object the slot (specification).  A static program's slots are
 ;; fixed when it is compiled, on classes (README.md, "Differences from
 ;; the 1992 manual"), so the compiler reports it.
 (ch06-define-slot-singleton
  "(define-class <pt> (<object>) x)
   (define special (make <pt>))
   (define ordinary (make <pt>))
   (define-slot (singleton special) tag init-value: 'special)
   (print (tag special))
   (print (handler-case (tag ordinary) ((<error>) 'error)))"
  ""
  :compile-error "define-slot's owner must be a class")
)
