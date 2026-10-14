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
  (bind ((d (%make <deque>)))
    (set! (%data d) (%make-vector (max size 8) fill))
    (set! (%count d) size)
    d))

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

(define %deque-check
  (method (d i)
    (unless (and (instance? i <integer>) (not (negative? i)) (binary< i (%count d)))
      (error "element: there is no element at the index ~s" i))))

(define-method size ((d <deque>)) (%count d))

(define-method element ((d <deque>) index #key (default %no-default))
  (if (and (instance? index <integer>) (not (negative? index)) (binary< index (%count d)))
      (%vector-element (%data d) (%deque-slot d index))
      (if (id? default %no-default)
          (error "element: there is no element at the index ~s" index)
          default)))

(define-method (setter element) ((d <deque>) index new-value)
  (%deque-check d index)
  (%vector-element-setter (%data d) (%deque-slot d index) new-value))

(define-method initial-state ((d <deque>)) (if (zero? (%count d)) #f 0))
(define-method next-state ((d <deque>) (i <integer>))
  (bind ((j (binary+ i 1))) (if (binary< j (%count d)) j #f)))
(define-method current-element ((d <deque>) (i <integer>)) (element d i))
(define-method (setter current-element) ((d <deque>) (i <integer>) new-value)
  (set! (element d i) new-value))
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

(define-method add ((d <deque>) new-value) (push (copy-sequence d) new-value))

(define-method remove! ((d <deque>) value #key (test id?) count)
  (bind ((kept (%remove-elements (%elements d) value test count)))
    (set! (%count d) 0)
    (set! (%first d) 0)
    (for-each ((e kept)) () (push-last d e))
    d))
