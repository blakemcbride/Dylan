module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; Arrays (chapter 12, "Operations on Arrays").  A vector is an array of
;;; one dimension; an array of any other number is a <%general-array>,
;;; its elements in row-major order in a <simple-object-vector>.  The keys
;;; of an array are sequences of indices.

(define-class <%general-array> (<array>)
  (%dimensions init-keyword: dimensions:)
  (%data type: <simple-object-vector> init-keyword: data:))

(define-method make ((class (singleton <array>)) #key dimensions (fill #f))
  (unless dimensions (error "make: <array> requires dimensions:"))
  (bind ((dims (as <list> dimensions)))
    (if (and (%pair? dims) (id? (cdr dims) '()))
        (%make-vector (car dims) fill)
        (%make <%general-array> dimensions: dims
               data: (%make-vector (reduce binary* 1 dims) fill)))))

(define dimensions
  (method (array)
    (if (%instance? array <%general-array>)
        (%dimensions array)
        (list (size array)))))

(define %row-major
  (method (array indices)
    (bind-methods ((index (dims indices k)
                     (cond ((and (id? dims '()) (id? indices '())) k)
                           ((or (id? dims '()) (id? indices '()))
                            (error "aref: the wrong number of indices for ~s" array))
                           ((not (and (%instance? (car indices) <integer>)
                                      (not (negative? (car indices)))
                                      (binary< (car indices) (car dims))))
                            (error "aref: the index ~s is out of bounds" (car indices)))
                           (else: (index (cdr dims) (cdr indices)
                                         (binary+ (binary* k (car dims)) (car indices)))))))
      (index (%dimensions array) indices 0))))

(define aref
  (method (array #rest indices)
    (if (%instance? array <%general-array>)
        (%vector-element (%data array) (%row-major array indices))
        (if (and (%pair? indices) (id? (cdr indices) '()))
            (element array (car indices))
            (error "aref: the wrong number of indices for ~s" array)))))

(define (setter aref)
  (method (array #rest indices-and-new-value)
    (bind ((new-value (last indices-and-new-value))
           (indices (copy-sequence indices-and-new-value
                                   end: (binary- (%list-length indices-and-new-value) 1))))
      (if (%instance? array <%general-array>)
          (%vector-element-setter (%data array) (%row-major array indices) new-value)
          (if (and (%pair? indices) (id? (cdr indices) '()))
              (set! (element array (car indices)) new-value)
              (error "aref: the wrong number of indices for ~s" array))))))

(define-method size ((a <%general-array>)) (%vector-size (%data a)))

(define-method element ((a <%general-array>) (indices <sequence>) #key (default %no-default))
  (bind ((l (as <list> indices)))
    (if (and (= (size l) (size (%dimensions a)))
             (every? (method (i d) (and (%instance? i <integer>) (not (negative? i)) (binary< i d)))
                     l (%dimensions a)))
        (apply aref a l)
        (if (id? default %no-default)
            (error "element: there is no element at ~s" indices)
            default))))

(define-method (setter element) ((a <%general-array>) (indices <sequence>) new-value)
  (apply (setter aref) a (append (as <list> indices) (list new-value))))

;;; Row-major iteration; the state is the row-major index.
(define-method initial-state ((a <%general-array>)) (if (zero? (size a)) #f 0))
(define-method next-state ((a <%general-array>) (i <integer>))
  (bind ((j (binary+ i 1))) (if (binary< j (size a)) j #f)))
(define-method current-element ((a <%general-array>) (i <integer>)) (%vector-element (%data a) i))
(define-method (setter current-element) ((a <%general-array>) (i <integer>) new-value)
  (%vector-element-setter (%data a) i new-value))
(define-method current-key ((a <%general-array>) (i <integer>))
  (bind-methods ((split (dims k)
                   (if (id? dims '())
                       '()
                       (bind ((inner (reduce binary* 1 (cdr dims))))
                         (bind ((q r (truncate/ k inner)))
                           (cons q (split (cdr dims) r)))))))
    (split (%dimensions a) i)))
