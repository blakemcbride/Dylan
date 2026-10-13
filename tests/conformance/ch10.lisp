;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Manual conformance tests: chapter 10, "Functions", of the 1992 Dylan
;;;; manual (manual/Dylan-1992/book.annotated/ch10.html).  The expected
;;;; results are the manual's own, or, where it shows none, what its text
;;;; specifies.  Functions, methods and classes have no readable printed
;;;; form, so their properties are printed instead.  print writes like
;;;; write and ends with a newline.

(
 ;; 10, Methods, method: the manual's examples ({an anonymous method} is
 ;; tested as an instance of <method>; the sort example's person-list
 ;; made concrete, and its test passed as test:, as chapter 12 defines
 ;; sort).  From the specification: a method is called directly, and runs
 ;; its body forms in order.
 (ch10-method
  "(define m (method (num1 num2)
               (+ num1 num2)))
   (print (list (instance? m <method>) (instance? m <function>)))
   (print (m 3 4))
   (print ((method (x) (print 'first) (* x 2)) 21))
   (define-class <person> (<object>) (name init-keyword: name:) (age init-keyword: age:))
   (define person-list (list (make <person> name: \"Ann\" age: 40)
                             (make <person> name: \"Bob\" age: 25)
                             (make <person> name: \"Cy\" age: 31)))
   (print (map name
               (sort person-list
                     test: (method (person1 person2)
                             (< (age person1)
                                (age person2))))))
   (print (bind ((double (method (number)
                           (+ number number))))
            (double (double 10))))
   (print ((method ((x <integer>) #rest r #key k) (list x (size r) k)) 1 k: 2))"
  "(#t #t)
7
first
42
(\"Bob\" \"Cy\" \"Ann\")
40
(1 2 2)
")

 ;; 10, Methods: the manual's double on <function> and print-twice (the
 ;; manual's transcript shows print without the newline and the quotes).
 (ch10-double-function
  "(define-method double ((thing <number>))
     (+ thing thing))
   (define-method double ((my-method <function>))
     (method (#rest args)
       (apply my-method args)
       (apply my-method args)
       #f))
   (define print-twice (double print))
   (print (instance? print-twice <method>))
   (print (print-twice \"The rain in Spain. . .\"))
   (print (print-twice 55))
   (print (double 21))"
  "#t
\"The rain in Spain. . .\"
\"The rain in Spain. . .\"
#f
55
55
#f
42
")

 ;; 10, bind-methods: the manual's root-mean-square (with size for the
 ;; manual's length, which the manual's library does not define).  From the
 ;; specification: the methods can refer to themselves and to each other,
 ;; and the values of the last body form are returned.
 (ch10-bind-methods
  "(define-method root-mean-square ((s <sequence>))
     (bind-methods ((average (numbers)
                      (/ (reduce1 + numbers)
                         (size numbers)))
                    (square (n) (* n n)))
       (sqrt (average (map square s)))))
   (print (root-mean-square '(5 6 6 7 4)))
   (print (bind-methods ((my-even? (n) (if (= n 0) #t (my-odd? (- n 1))))
                         (my-odd? (n) (if (= n 0) #f (my-even? (- n 1)))))
            (print 'body)
            (list (my-even? 10) (my-odd? 7) (my-even? 7))))
   (bind ((a b (bind-methods ((f () (values 1 2))) (f)))) (print (list a b)))"
  "5.692099788303083
body
(#t #t #f)
(1 2)
")

 ;; 10, Method Dispatch: the manual's double on <number> alone.
 (ch10-dispatch
  "(define-method double ((thing <number>))
     (+ thing thing))
   (print (double 10))
   (print (double 4.5))
   (print (double 1/3))"
  "20
9.0
2/3
")

 ;; 10, Most Specific Method and Calling More General Methods: the
 ;; manual's double on <number>, <integer>, (singleton 'cup) and <float>.
 ;; From the specification: a singleton is more specific than any class
 ;; (here (singleton 7) against <integer>).
 (ch10-most-specific
  "(define-method double ((thing <number>))
     (+ thing thing))
   (define-method double ((thing <integer>))
     (* thing 2))
   (define-method double ((thing (singleton 'cup)))
     'pint)
   (define-method double ((num <float>))
     (print \"doubling a floating-point number\")
     (next-method))
   (define-method double ((thing (singleton 7)))
     'singleton-seven)
   (print (double 5))
   (print (double 'cup))
   (print (double 10.5))
   (print (double 1/2))
   (print (double 7))
   (print (handler-case (double 'bowl) ((<error>) 'error)))"
  "10
pint
\"doubling a floating-point number\"
21.0
1
singleton-seven
error
")

 ;; 10, Calling More General Methods, Passing Different Arguments to
 ;; Next-Method, The Next-Method Parameter (from the specification):
 ;; next-method with no arguments passes the same arguments; with
 ;; arguments, those; #f when there is no next method; the parameter may
 ;; be named with #next; a rest parameter and apply pass the keyword
 ;; arguments on.
 (ch10-next-method
  "(define-method describe ((x <number>))
     (list 'number x (if next-method 'has-next 'no-next)))
   (define-method describe ((x <integer>))
     (cons 'integer (next-method)))
   (print (describe 3))
   (print (describe 2.5))
   (define-method bump ((x <number>)) (* x 10))
   (define-method bump ((x <integer>)) (next-method (+ x 1)))
   (print (bump 1))
   (define-method named ((x <number>)) 'number)
   (define-method named ((x <integer>) #next more) (list 'integer (more)))
   (print (named 4))
   (define-method scaled ((x <number>) #rest args #key scale)
     (* x (or scale 1)))
   (define-method scaled ((x <integer>) #rest args #key scale)
     (list 'integer (apply next-method x args)))
   (print (scaled 3 scale: 2))
   (print (scaled 3))"
  "(integer number 3 no-next)
(number 2.5 no-next)
20
(integer number)
(integer 6)
(integer 3)
")

 ;; 10, Multiple Argument Dispatch: the manual's show methods (their
 ;; bodies, elided in the manual, print which method ran; <rectangle>
 ;; left out).  From the specification: every argument must match its
 ;; specializer; a singleton matches with id?; the applicable methods are
 ;; sorted lexicographically by specializer lists, so the first argument
 ;; decides first.
 (ch10-multimethods
  "(define-class <device> (<object>))
   (define-class <window> (<device>))
   (define-class <file> (<device>))
   (define-method show ((device <window>) (thing <character>))
     'window-character)
   (define-method show ((device <window>) (thing <string>))
     'window-string)
   (define-method show ((device <file>) (thing <character>))
     'file-character)
   (define-method show ((device <file>) (thing <string>))
     'file-string)
   (define-method show ((device <device>) (thing (singleton 'bell)))
     'device-bell)
   (define w (make <window>))
   (define f (make <file>))
   (print (list (show w #\\a) (show w \"s\") (show f #\\a) (show f \"s\") (show f 'bell)))
   (print (handler-case (show w 3) ((<error>) 'error)))
   (define-method pick ((a <integer>) (b <object>)) (list 'integer-object (next-method)))
   (define-method pick ((a <number>) (b <integer>)) 'number-integer)
   (print (pick 1 2))"
  "(window-character window-string file-character file-string device-bell)
error
(integer-object number-integer)
")

 ;; 10, Additional Function Details: <function>, <generic-function> and
 ;; <method> (from the specification): <function> inherits from <object>
 ;; and is the superclass of generic functions and methods.
 (ch10-function-classes
  "(define-method gf ((x <integer>)) x)
   (print (list (subclass? <function> <object>) (subclass? <generic-function> <function>)
                (subclass? <method> <function>)))
   (print (list (instance? gf <generic-function>) (instance? gf <function>)
                (instance? gf <method>)))
   (print (list (instance? (method (x) x) <method>) (instance? (method (x) x) <generic-function>)))
   (print (list (instance? + <function>) (instance? 3 <function>)))"
  "(#t #t #t)
(#t #t #f)
(#t #f)
(#t #f)
")

 ;; 10, <generic-function>: the manual's make examples; {the generic
 ;; function expand} has no readable form, so it is tested as an instance
 ;; of <generic-function>.  From the specification: required: must be
 ;; supplied, or an error is signaled; calling a generic function with no
 ;; methods signals an error (the manual: \"no applicable method\").
 (ch10-make-generic-function
  "(print (instance? (make <generic-function> required: 3) <generic-function>))
   (print (instance? (make <generic-function> required: 3
                                               debug-name: 'foo)
                     <generic-function>))
   (define expand
     (make <generic-function> required: 1 debug-name: 'expand))
   (print (instance? expand <generic-function>))
   (print (handler-case (expand 55) ((<error>) 'error)))
   (print (handler-case (make <generic-function>) ((<error>) 'error)))"
  "#t
#t
#t
error
error
")

 ;; 10, make-read-only (from the specification): answers the generic
 ;; function, which still runs its methods.
 (ch10-make-read-only
  "(define-method double ((thing <number>)) (+ thing thing))
   (print (id? (make-read-only double) double))
   (print (double 4))"
  "#t
8
")

 ;; 10, Reflective Operations on Functions and Methods (from the
 ;; specification): generic-function-methods answers a sequence of all the
 ;; methods; method-specializers a sequence of the specializers, one per
 ;; required argument; find-method the method with exactly the given
 ;; specializers, or #f; applicable-method? whether a method, or a
 ;; generic function's method, applies to the sample arguments;
 ;; sorted-applicable-methods the applicable methods, most specific first.
 (ch10-reflection
  "(define-method double ((thing <number>)) (+ thing thing))
   (define-method double ((thing <integer>)) (* thing 2))
   (define-method double ((thing (singleton 'cup))) 'pint)
   (define-method combine ((a <integer>) (b <string>)) 'is)
   (define ms (generic-function-methods double))
   (print (size ms))
   (print (every? (method (m) (instance? m <method>)) ms))
   (define mi (find-method double (list <integer>)))
   (define mn (find-method double (list <number>)))
   (print (list (instance? mi <method>) (instance? mn <method>)
                (find-method double (list <string>)) (find-method double (list <float>))))
   (print (list (mi 5) (mn 5)))
   (print (list (size (method-specializers mi)) (id? (first (method-specializers mi)) <integer>)))
   (define mc (find-method combine (list <integer> <string>)))
   (print (list (size (method-specializers mc)) (id? (first (method-specializers mc)) <integer>)
                (id? (second (method-specializers mc)) <string>)))
   (print (list (applicable-method? double 5) (applicable-method? double 'cup)
                (applicable-method? double \"x\") (applicable-method? mi 5) (applicable-method? mi 2.5)
                (applicable-method? (method ((x <integer>)) x) 'a)
                (applicable-method? combine 1 \"s\") (applicable-method? combine \"s\" 1)))
   (define sorted (sorted-applicable-methods double 5))
   (print (list (size sorted) (id? (first sorted) mi) (id? (second sorted) mn)))
   (print (size (sorted-applicable-methods double 2.5)))
   (print (size (sorted-applicable-methods double \"x\")))
   (print (size (sorted-applicable-methods double 'cup)))"
  "3
#t
(#t #t #f #f)
(10 10)
(1 #t)
(2 #t #t)
(#t #t #f #t #f #f #t #f)
(2 #t #t)
1
0
1
")

 ;; 10, function-arguments (from the specification): the number of
 ;; required arguments, whether there is a #rest argument, and the
 ;; keywords accepted, or #f without #key (possibly empty with #key and no
 ;; keywords).
 (ch10-function-arguments
  "(bind ((r rest keys (function-arguments (method (a b #rest c #key d e) a))))
     (print (list r rest (size keys) (if (member? d: keys) #t #f) (if (member? e: keys) #t #f))))
   (bind ((r rest keys (function-arguments (method (a) a))))
     (print (list r rest keys)))
   (bind ((r rest keys (function-arguments (method (#rest r) r))))
     (print (list r rest keys)))
   (bind ((r rest keys (function-arguments (method (#key) 1))))
     (print (list r rest (empty? keys))))"
  "(2 #t 2 #t #t)
(1 #f #f)
(0 #t #f)
(0 #f #t)
")
)
