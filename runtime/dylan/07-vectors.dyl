module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; The runtime's vectors -- <simple-object-vector>, <byte-string> and
;;; <unicode-string> -- (chapter 12).  Their iteration state is the index.
;;; The runtime's %vector- primitives work on all three; the methods are on
;;; each, so that dispatch need not ask which.

(define %vector-initial-state (method (v) (if (zero? (%vector-size v)) #f 0)))
(define %vector-next-state
  (method (v i) (bind ((j (binary+ i 1))) (if (binary< j (%vector-size v)) j #f))))
(define %vector-final-state
  (method (v) (bind ((n (%vector-size v))) (if (zero? n) #f (binary- n 1)))))
(define %vector-previous-state (method (v i) (if (zero? i) #f (binary- i 1))))

(define %vector-ref
  (method (v index default)
    (if (and (instance? index <integer>) (not (negative? index))
             (binary< index (%vector-size v)))
        (%vector-element v index)
        (if (id? default %no-default)
            (error "element: there is no element at the index ~s" index)
            default))))

(define-method initial-state ((v <simple-object-vector>)) (%vector-initial-state v))
(define-method next-state ((v <simple-object-vector>) (i <integer>)) (%vector-next-state v i))
(define-method current-element ((v <simple-object-vector>) (i <integer>)) (%vector-element v i))
(define-method (setter current-element) ((v <simple-object-vector>) (i <integer>) new-value)
  (%vector-element-setter v i new-value))
(define-method final-state ((v <simple-object-vector>)) (%vector-final-state v))
(define-method previous-state ((v <simple-object-vector>) (i <integer>)) (%vector-previous-state v i))
(define-method size ((v <simple-object-vector>)) (%vector-size v))
(define-method empty? ((v <simple-object-vector>)) (zero? (%vector-size v)))
(define-method element ((v <simple-object-vector>) index #key (default %no-default))
  (%vector-ref v index default))
(define-method (setter element) ((v <simple-object-vector>) index new-value)
  (%vector-element-setter v index new-value))

(define-method make ((class (singleton <simple-object-vector>)) #key (size 0) (fill #f))
  (%make-vector size fill))

;;; <vector> and <array> have no direct instances: make of them makes a
;;; <simple-object-vector> (for <array>, of one dimension; see arrays).
(define-method make ((class (singleton <vector>)) #key (size 0) (fill #f))
  (%make-vector size fill))

;;; A copy of part of a built-in vector, of its own class
(define %copy-vector
  (method (v start end)
    (bind ((n (binary- end start))
           (copy (if (instance? v <simple-object-vector>)
                     (%make-vector n #f)
                     (make (object-class v) size: n))))
      (%replace-vector! copy 0 v start n))))

(define-method copy-sequence ((v <simple-object-vector>) #key (start 0) end)
  (%copy-vector v start (or end (%vector-size v))))

(define-method reverse ((v <simple-object-vector>))
  (bind ((n (%vector-size v))
         (r (%make-vector n #f)))
    (dotimes (i n r)
      (%vector-element-setter r i (%vector-element v (binary- (binary- n 1) i))))))

(define-method fill! ((v <simple-object-vector>) value #key (start 0) end)
  (for ((i start (binary+ i 1)))
       ((not (binary< i (or end (%vector-size v)))) v)
    (%vector-element-setter v i value)))

;;; A vector is an array, so an explicit-key collection too: its keys are
;;; its indices.
(define-method current-key ((v <vector>) (i <integer>)) i)
(define-method key-sequence ((v <vector>)) (range up-to: (size v)))
