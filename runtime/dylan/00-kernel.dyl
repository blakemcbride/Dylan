module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; The library (CompilerPlan.md, Phase 8): the kernel.
;;;
;;; The library is written in Dylan, in the dylan module, on the C
;;; primitives of runtime/c (their names begin with %).  Every program is
;;; compiled with it.  Its files are read in the order of their names.
;;;
;;; This one: equality and magnitude comparison (chapter 9), the
;;; functional operations (chapter 17), and the generic functions make and
;;; as, whose methods the other files add.

;;; Equality and comparison (chapter 9).  =, /=, <, >, <= and >= are
;;; functions (methods, the manual says); to extend them, programs add
;;; methods to binary= and binary<.

(define-generic-function binary= (object1 object2))

(define-method binary= ((x <object>) (y <object>)) (id? x y))
(define-method binary= ((x <number>) (y <number>)) (%number= x y))

(define-generic-function binary< (object1 object2))

(define-method binary< ((x <real>) (y <real>)) (%number< x y))
(define-method binary< ((x <character>) (y <character>))
  (%number< (%character->integer x) (%character->integer y)))

(define =
  (method (x y #rest more)
    (if (binary= x y)
        (if (id? more '()) #t (apply = y more))
        #f)))

(define /= (method (x y) (not (binary= x y))))

(define <
  (method (x y #rest more)
    (if (binary< x y)
        (if (id? more '()) #t (apply < y more))
        #f)))

(define >
  (method (x y #rest more)
    (if (binary< y x)
        (if (id? more '()) #t (apply > y more))
        #f)))

(define <=
  (method (x y #rest more)
    (if (binary< y x)
        #f
        (if (id? more '()) #t (apply <= y more)))))

(define >=
  (method (x y #rest more)
    (if (binary< x y)
        #f
        (if (id? more '()) #t (apply >= y more)))))

;;; =hash: = objects hash alike (chapter 9).  More methods with the
;;; collections.

(define-generic-function =hash (object))

(define-method =hash ((x <object>)) (%object-hash x))
(define-method =hash ((x <number>)) (%number-hash x))

;;; Functional operations (chapter 17)

(define compose
  (method (function #rest more)
    (if (id? more '())
        function
        (bind ((rest (apply compose more)))
          (method (#rest arguments) (function (apply rest arguments)))))))

(define complement
  (method (predicate)
    (method (#rest arguments) (not (apply predicate arguments)))))

(define disjoin
  (method (predicate #rest more)
    (bind ((predicates (cons predicate more)))
      (method (#rest arguments)
        (bind-methods ((try (ps)
                         (if (id? ps '())
                             #f
                             (or (apply (car ps) arguments) (try (cdr ps))))))
          (try predicates))))))

(define conjoin
  (method (predicate #rest more)
    (bind ((predicates (cons predicate more)))
      (method (#rest arguments)
        (bind-methods ((try (ps value)
                         (if (id? ps '())
                             value
                             (bind ((v (apply (car ps) arguments)))
                               (if v (try (cdr ps) v) #f)))))
          (try predicates #t))))))

(define curry
  (method (function #rest curried)
    (method (#rest arguments) (apply function (append curried arguments)))))

(define rcurry
  (method (function #rest curried)
    (method (#rest arguments) (apply function (append arguments curried)))))

(define always
  (method (object)
    (method (#rest arguments) object)))

;;; make (chapter 6).  The method on <class> makes an instance of a class
;;; the program defines; the built-in collection classes have methods on
;;; their singletons.

(define-generic-function make (class #rest init-arguments))

(define-method make ((class <class>) #rest init-arguments)
  (apply %make class init-arguments))

;;; initialize (chapter 6).  Its method on <object>, which does nothing,
;;; is the compiler's own.

(define-generic-function initialize (instance #rest init-arguments))

;;; shallow-copy: a new object with the same contents.

(define-generic-function shallow-copy (object))
(define-method shallow-copy ((x <object>)) (%shallow-copy x))

;;; as: conversions (chapters 12, 15 and 16).  An object is already as
;;; anything it is an instance of.

(define-generic-function as (class object))

(define-method as ((class <class>) object)
  (if (instance? object class)
      object
      (error "as: ~s cannot be converted to ~s" object class)))

;;; A marker no program can see, for "no argument given".

(define %no-default (cons #f #f))

;;; Slot descriptors (chapter 11): the rest are the runtime's.
(define (setter slot-value)
  (method (instance descriptor new-value)
    (%slot-value-setter instance descriptor new-value)))
