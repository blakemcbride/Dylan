;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conformance tests for chapter 2 of the 1992 manual, Language Overview.
;;;; The chapter shows no examples; every expected value here is what its
;;;; text specifies.

(
 ;; 2 Language Overview: <object> is the root of the heterarchy; every
 ;; object is a direct instance of exactly one class and an indirect
 ;; instance of that class's superclasses, so of <object>.  The
 ;; superclasses of a class are the class itself, its direct
 ;; superclasses, theirs, and so on back to <object>.  (Specification.)
 (ch02-class-heterarchy
  "(define-class <animal> (<object>))
   (define-class <dog> (<animal>))
   (define d (make <dog>))
   (print (id? (object-class d) <dog>))
   (print (list (instance? d <dog>) (instance? d <animal>) (instance? d <object>)))
   (print (instance? (make <animal>) <dog>))
   (print (list (instance? 3 <object>) (instance? 'a <object>) (instance? \"s\" <object>)
                (instance? #\\a <object>) (instance? '() <object>) (instance? <dog> <object>)))
   (print (empty? (direct-superclasses <object>)))
   (print (id? (first (direct-superclasses <dog>)) <animal>))
   (print (list (not (not (member? <dog> (all-superclasses <dog>))))
                (not (not (member? <animal> (all-superclasses <dog>))))
                (not (not (member? <object> (all-superclasses <dog>))))))
   (print (list (subclass? <dog> <animal>) (subclass? <dog> <object>) (subclass? <animal> <dog>)))"
  "#t
(#t #t #t)
#f
(#t #t #t #t #t #t)
#t
#t
(#t #t #t)
(#t #t #f)
")

 ;; 2 Language Overview: a sealed class cannot be subclassed; chapter 12
 ;; says <list> is sealed.  (Specification.)
 (ch02-sealed-class
  "(define-class <my-list> (<list>))
   (print 'not-reached)"
  ""
  :compile-error "<list>")

 ;; 2 Language Overview: methods.  A method can be called directly; it is
 ;; applicable when the arguments match its specializers, and calling it
 ;; with inappropriate arguments signals an error.  A generic function
 ;; runs the method with the most appropriate specializers.  Classes and
 ;; functions are first-class objects.  (Specification.)
 (ch02-functions
  "(define double (method ((x <integer>)) (* x 2)))
   (print (double 4))
   (print ((method ((s <string>)) (size s)) \"abcd\"))
   (print (handler-case (double \"x\") ((<error>) 'error)))
   (define-method kind ((x <object>)) 'object)
   (define-method kind ((x <number>)) 'number)
   (define-method kind ((x <integer>)) 'integer)
   (define-method kind ((x (singleton 'zero))) 'zero)
   (print (list (kind 3) (kind 1.5) (kind \"s\") (kind 'zero) (kind 'one)))
   (define-generic-function no-methods (x))
   (print (handler-case (no-methods 1) ((<error>) 'error)))
   (print (list (instance? kind <generic-function>) (instance? double <method>)
                (instance? kind <function>) (instance? double <function>)
                (instance? <integer> <class>)))
   (define classes (list <integer> <string>))
   (print (instance? 3 (first classes)))
   (print ((if #f + kind) 7))"
  "8
4
error
(integer number object zero object)
error
(#t #t #t #t #t)
#t
integer
")

 ;; 2 Structure of Dylan: the named entities of the core language, and
 ;; some of the Extended Control Constructs the chapter names (when, or,
 ;; select).  Each is used here with the result its own chapter
 ;; specifies.
 (ch02-core-language
  "(define-class <point> (<object>) (x init-keyword: x:) (y init-value: 0))
   (define-generic-function area (shape))
   (define-method area ((p <point>)) 0)
   (define-method initialize ((p <point>) #key)
     (next-method)
     (set! (y p) 5))
   (define p (make <point> x: 3))
   (print (list (x p) (y p) (area p)))
   (print (apply + 1 2 '(3 4)))
   (print (begin 1 2 3))
   (print (bind ((a 1) (b 2)) (+ a b)))
   (print (bind-exit (exit) (exit 'out) 'not-reached))
   (print (bind-methods ((sq (n) (* n n))) (sq 6)))
   (print (if #f 'yes 'no))
   (print (quote q))
   (print ((setter x) p 9))
   (print (x p))
   (define-class <box> (<object>) (contents))
   (print (slot-initialized? (make <box>) contents))
   (print (slot-initialized? p x))
   (define trail '())
   (print (unwind-protect 'body (set! trail (cons 'cleanup trail))))
   (print trail)
   (bind ((a b (values 1 2))) (print (list a b)))
   (print ((method (n) (+ n 1)) 1))
   (print (list (when #t 'w) (or #f 'o) (select 2 = ((1) 'one) ((2) 'two) (else: 'other))))"
  "(3 5 0)
10
3
3
out
36
no
q
9
9
#f
#t
body
(cleanup)
(1 2)
2
(w o two)
")
)
