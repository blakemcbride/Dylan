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

;;; Size-setter for stretchy sequences (a design note): grows with #f, or
;;; shrinks.
(define-method (setter size) ((v <stretchy-vector>) (n <integer>))
  (%stretchy-ensure v n)
  (bind ((data (%data v)))
    (for ((i (%count v) (binary+ i 1))) ((not (binary< i n))) (%vector-element-setter data i #f)))
  (set! (%count v) n)
  n)

(define %stretchy-check
  (method (v i)
    (unless (and (instance? i <integer>) (not (negative? i)) (binary< i (%count v)))
      (error "element: there is no element at the index ~s" i))))

(define-method element ((v <stretchy-vector>) index #key (default %no-default))
  (if (and (instance? index <integer>) (not (negative? index)) (binary< index (%count v)))
      (%vector-element (%data v) index)
      (if (id? default %no-default)
          (error "element: there is no element at the index ~s" index)
          default)))

(define-method (setter element) ((v <stretchy-vector>) index new-value)
  (%stretchy-check v index)
  (%vector-element-setter (%data v) index new-value))

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
