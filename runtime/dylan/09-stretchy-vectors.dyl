module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; <stretchy-vector> (chapter 12): a vector that grows and shrinks.  Its
;;; elements are the first SIZE of a <simple-object-vector> that is
;;; replaced by a larger one as needed.

(define-class <stretchy-vector> (<vector>)
  (%data type: <simple-object-vector> init-function: (method () (%make-vector 8 #f)))
  (%count type: <integer> init-value: 0))

(define-method make ((class (singleton <stretchy-vector>)) #key (size 0) (fill #f))
  (unless (and (%instance? size <integer>) (not (negative? size)))
    (error "make: size: must be a non-negative integer: ~s" size))
  (bind ((v (%make <stretchy-vector>)))
    (set! (%data v) (%make-vector (max size 8) fill))
    (set! (%count v) size)
    v))

(define %stretchy-ensure
  (method (v n)
    (bind ((data (%data v)))
      (when (binary< (%vector-size data) n)
        (bind ((new (%make-vector (max n (binary* 2 (%vector-size data))) #f)))
          (%replace-vector! new 0 data 0 (%count v))
          (set! (%data v) new))))))

(define-method size ((v <stretchy-vector>)) (%count v))

;;; Size-setter for stretchy sequences (design note 12): grows with #f, or
;;; shrinks.
(define %check-new-size
  (method (n)
    (unless (and (%instance? n <integer>) (not (negative? n)))
      (error "(setter size): the size must be a non-negative integer: ~s" n))))

(define-method (setter size) ((v <stretchy-vector>) n)
  (%check-new-size n)
  (%stretchy-ensure v n)
  (bind ((data (%data v)))
    (for ((i (%count v) (binary+ i 1))) ((not (binary< i n))) (%vector-element-setter data i #f)))
  (set! (%count v) n)
  n)

;;; The index KEY denotes in V, or an error
(define %stretchy-check
  (method (v key)
    (bind ((i (%index-key key)))
      (unless (and i (not (negative? i)) (binary< i (%count v)))
        (error "element: there is no element at the index ~s" key))
      i)))

(define %stretchy-element
  (method (v key default)
    (bind ((index (%index-key key)))
      (if (and index (not (negative? index)) (binary< index (%count v)))
          (%vector-element (%data v) index)
          (if (id? default %no-default)
              (error "element: there is no element at the index ~s" key)
              default)))))

(define %stretchy-element-setter
  (method (v key new-value)
    (%vector-element-setter (%data v) (%stretchy-check v key) new-value)))

(define-method element ((v <stretchy-vector>) key #key (default %no-default))
  (%stretchy-element v key default))

(define-method (setter element) ((v <stretchy-vector>) key new-value)
  (%stretchy-element-setter v key new-value))

;;; The library's own (04-collections.dyl), for a direct instance
(define-method %library-size ((v <stretchy-vector>))
  (if (id? (%object-class v) <stretchy-vector>) (%count v) (size v)))
(define-method %library-element ((v <stretchy-vector>) key default)
  (if (id? (%object-class v) <stretchy-vector>)
      (%stretchy-element v key default)
      (next-method)))
(define-method %library-element-setter ((v <stretchy-vector>) key new-value)
  (if (id? (%object-class v) <stretchy-vector>)
      (%stretchy-element-setter v key new-value)
      (next-method)))

(define-method initial-state ((v <stretchy-vector>)) (if (zero? (%count v)) #f 0))
(define-method next-state ((v <stretchy-vector>) (i <integer>))
  (bind ((j (binary+ i 1))) (if (binary< j (%count v)) j #f)))
(define-method current-element ((v <stretchy-vector>) (i <integer>))
  (%stretchy-check v i)
  (%vector-element (%data v) i))
(define-method (setter current-element) ((v <stretchy-vector>) (i <integer>) new-value)
  (%stretchy-check v i)
  (%vector-element-setter (%data v) i new-value))
(define-method final-state ((v <stretchy-vector>))
  (if (zero? (%count v)) #f (binary- (%count v) 1)))
(define-method previous-state ((v <stretchy-vector>) (i <integer>))
  (if (zero? i) #f (binary- i 1)))

;;; add! adds at the end (chapter 12).
(define-method add! ((v <stretchy-vector>) new-element)
  (%stretchy-ensure v (binary+ (%count v) 1))
  (%vector-element-setter (%data v) (%count v) new-element)
  (set! (%count v) (binary+ (%count v) 1))
  v)

(define-method add ((v <stretchy-vector>) new-element)
  (add! (copy-sequence v) new-element))

(define-method remove! ((v <stretchy-vector>) value #key (test id?) count)
  (bind ((kept (%remove-elements (%elements v) value test count)))
    (set! (%count v) 0)
    (for-each ((e kept)) () (add! v e))
    v))
