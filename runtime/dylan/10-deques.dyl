module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; <deque> (chapter 12, "Operations on Deques"): a double-ended queue,
;;; held in a circular buffer.  Element i is at (first + i) modulo the
;;; buffer's size.  The iteration state is the index.

(define-class <deque> (<mutable-sequence>)
  (%data type: <simple-object-vector> init-function: (method () (%make-vector 8 #f)))
  (%first type: <integer> init-value: 0)
  (%count type: <integer> init-value: 0))

(define-method make ((class (singleton <deque>)) #key (size 0) (fill #f))
  (unless (and (%instance? size <integer>) (not (negative? size)))
    (error "make: size: must be a non-negative integer: ~s" size))
  (bind ((d (%make <deque>)))
    (set! (%data d) (%make-vector (max size 8) fill))
    (set! (%count d) size)
    d))

;;; A program's subclass is made by make's method on <class>, which calls
;;; initialize: it takes size: and fill: too (chapter 12).
(define-method initialize ((d <deque>) #key (size 0) (fill #f))
  (unless (and (%instance? size <integer>) (not (negative? size)))
    (error "make: size: must be a non-negative integer: ~s" size))
  (set! (%data d) (%make-vector (max size 8) fill))
  (set! (%first d) 0)
  (set! (%count d) size)
  (next-method))

(define %deque-slot
  (method (d i)
    (bind ((n (%vector-size (%data d)))
           (k (binary+ (%first d) i)))
      (if (binary< k n) k (binary- k n)))))

(define %deque-grow
  (method (d)
    (bind ((old (%data d))
           (n (%vector-size old)))
      (when (= (%count d) n)
        (bind ((new (%make-vector (binary* 2 n) #f)))
          (dotimes (i (%count d))
            (%vector-element-setter new i (%vector-element old (%deque-slot d i))))
          (set! (%data d) new)
          (set! (%first d) 0))))))

;;; The index KEY denotes in D, or an error
(define %deque-check
  (method (d key)
    (bind ((i (%index-key key)))
      (unless (and i (not (negative? i)) (binary< i (%count d)))
        (error "element: there is no element at the index ~s" key))
      i)))

(define-method size ((d <deque>)) (%count d))

(define %deque-element
  (method (d key default)
    (bind ((index (%index-key key)))
      (if (and index (not (negative? index)) (binary< index (%count d)))
          (%vector-element (%data d) (%deque-slot d index))
          (if (id? default %no-default)
              (error "element: there is no element at the index ~s" key)
              default)))))

(define %deque-element-setter
  (method (d key new-value)
    (%vector-element-setter (%data d) (%deque-slot d (%deque-check d key)) new-value)))

(define-method element ((d <deque>) key #key (default %no-default))
  (%deque-element d key default))

(define-method (setter element) ((d <deque>) key new-value)
  (%deque-element-setter d key new-value))

;;; The library's own (04-collections.dyl), for a direct instance
(define-method %library-size ((d <deque>))
  (if (id? (%object-class d) <deque>) (%count d) (size d)))
(define-method %library-element ((d <deque>) key default)
  (if (id? (%object-class d) <deque>) (%deque-element d key default) (next-method)))
(define-method %library-element-setter ((d <deque>) key new-value)
  (if (id? (%object-class d) <deque>) (%deque-element-setter d key new-value) (next-method)))

(define-method initial-state ((d <deque>)) (if (zero? (%count d)) #f 0))
(define-method next-state ((d <deque>) (i <integer>))
  (bind ((j (binary+ i 1))) (if (binary< j (%count d)) j #f)))
(define-method current-element ((d <deque>) (i <integer>)) (%deque-element d i %no-default))
(define-method (setter current-element) ((d <deque>) (i <integer>) new-value)
  (%deque-element-setter d i new-value))
(define-method final-state ((d <deque>)) (if (zero? (%count d)) #f (binary- (%count d) 1)))
(define-method previous-state ((d <deque>) (i <integer>)) (if (zero? i) #f (binary- i 1)))

(define-generic-function push (deque new-value))
(define-method push ((d <deque>) new-value)
  (%deque-grow d)
  (bind ((n (%vector-size (%data d))))
    (set! (%first d) (if (zero? (%first d)) (binary- n 1) (binary- (%first d) 1)))
    (%vector-element-setter (%data d) (%first d) new-value)
    (set! (%count d) (binary+ (%count d) 1))
    d))

(define-generic-function push-last (deque new-value))
(define-method push-last ((d <deque>) new-value)
  (%deque-grow d)
  (%vector-element-setter (%data d) (%deque-slot d (%count d)) new-value)
  (set! (%count d) (binary+ (%count d) 1))
  d)

;;; Size-setter for stretchy sequences (design note 12): elements are
;;; dropped from the end, or #f added there, as make adds them without
;;; fill:.
(define-method (setter size) ((d <deque>) n)
  (%check-new-size n)
  (for () ((not (binary< n (%count d)))) (pop-last d))
  (for () ((not (binary< (%count d) n))) (push-last d #f))
  n)

(define-generic-function pop (deque))
(define-method pop ((d <deque>))
  (when (zero? (%count d)) (error "pop: the deque is empty"))
  (bind ((value (%vector-element (%data d) (%first d)))
         (n (%vector-size (%data d))))
    (%vector-element-setter (%data d) (%first d) #f)
    (set! (%first d) (if (= (binary+ (%first d) 1) n) 0 (binary+ (%first d) 1)))
    (set! (%count d) (binary- (%count d) 1))
    value))

(define-generic-function pop-last (deque))
(define-method pop-last ((d <deque>))
  (when (zero? (%count d)) (error "pop-last: the deque is empty"))
  (bind ((k (%deque-slot d (binary- (%count d) 1))))
    (bind ((value (%vector-element (%data d) k)))
      (%vector-element-setter (%data d) k #f)
      (set! (%count d) (binary- (%count d) 1))
      value)))

;;; add! on a deque is push; remove! changes the deque itself.
(define-method add! ((d <deque>) new-value) (push d new-value))

;;; A new instance of the class-for-copy (chapter 12): a deque is pushed
;;; on, as add! does; another class adds as it does.
(define-method add ((d <deque>) new-value)
  (bind ((copy (copy-sequence d)))
    (if (%instance? copy <deque>) (push copy new-value) (add copy new-value))))

(define-method remove! ((d <deque>) value #key (test id?) count)
  (bind ((kept (%remove-elements (%elements d) value test count)))
    (set! (%count d) 0)
    (set! (%first d) 0)
    (for-each ((e kept)) () (push-last d e))
    d))

;;; In place too, as in the Common Lisp implementation
(define-method remove-duplicates! ((d <deque>) #key (test id?))
  (bind ((kept (remove-duplicates (%elements d) test: test)))
    (set! (%count d) 0)
    (set! (%first d) 0)
    (for-each ((e kept)) () (push-last d e))
    d))
