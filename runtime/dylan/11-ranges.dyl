module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; <range> (chapter 12, "Operations on Ranges"): arithmetic sequences,
;;; possibly unbounded.  Element i is from + i * by; the size is #f if the
;;; range has no bound.  The iteration state is the index.

(define-class <range> (<sequence>)
  (%from init-keyword: from: init-value: 0)
  (%by init-keyword: by: init-value: 1)
  (%size init-keyword: size: init-value: #f))

(define-method class-for-copy ((r <range>)) <list>)

;;; The largest range from FROM by BY such that no value is at or beyond
;;; UP-TO, none beyond THROUGH, and there are no more than SIZE values.
;;; range is a generic function (chapter 12).
(define-generic-function range (#key from up-to through by size))

(define-method range (#key (from 0) up-to through (by 1) size)
  (bind ((count size))
    (bind-methods ((limit (n) (when (or (not count) (binary< n count)) (set! count n))))
      (when (or up-to through)
        (when (zero? by) (error "range: by: must not be 0 with up-to: or through:")))
      (when up-to
        (limit (max 0 (ceiling/ (binary- up-to from) by))))
      (when through
        (limit (max 0 (binary+ (floor/ (binary- through from) by) 1))))
      (make <range> from: from by: by size: count))))

(define-method make ((class (singleton <range>)) #rest arguments)
  (if (or (member? up-to: arguments) (member? through: arguments))
      (apply range arguments)
      (apply %make <range> arguments)))

(define %range-element (method (r i) (binary+ (%from r) (binary* i (%by r)))))

(define %range-index?
  (method (r i)
    (and (%instance? i <integer>) (not (negative? i))
         (or (not (%size r)) (binary< i (%size r))))))

(define-method size ((r <range>)) (%size r))
(define-method empty? ((r <range>)) (and (%size r) (zero? (%size r))))

(define-method element ((r <range>) index #key (default %no-default))
  (if (%range-index? r index)
      (%range-element r index)
      (if (id? default %no-default)
          (error "element: there is no element at the index ~s" index)
          default)))

(define-method initial-state ((r <range>)) (if (%range-index? r 0) 0 #f))
(define-method next-state ((r <range>) (i <integer>))
  (bind ((j (binary+ i 1)) (n (%size r)))
    (if (or (not n) (binary< j n)) j #f)))
(define-method current-element ((r <range>) (i <integer>)) (%range-element r i))
(define-method final-state ((r <range>))
  (unless (%size r) (error "final-state: the range is unbounded"))
  (if (zero? (%size r)) #f (binary- (%size r) 1)))
(define-method previous-state ((r <range>) (i <integer>)) (if (zero? i) #f (binary- i 1)))

;;; member? always terminates.
(define-method member? (value (r <range>) #key test)
  (if test
      (if (%size r)
          (any? (method (e) (test value e)) (%elements r))
          (error "member?: a test: on an unbounded range might not terminate"))
      (and (%instance? value <real>)
           (if (zero? (%by r))
               (and (%range-index? r 0) (= value (%from r)))
               (bind ((k (binary/ (binary- value (%from r)) (%by r))))
                 (and (integral? k)
                      (%range-index? r (if (%instance? k <integer>) k (truncate k)))
                      #t))))))

(define-method copy-sequence ((r <range>) #key (start 0) end)
  (bind ((stop (or end (%size r))))
    (make <range> from: (%range-element r start) by: (%by r)
                  size: (and stop (max 0 (binary- stop start))))))

(define-method reverse ((r <range>))
  (unless (%size r) (error "reverse: the range is unbounded"))
  (if (zero? (%size r))
      (make <range> from: (%from r) by: (%by r) size: 0)
      (make <range> from: (%range-element r (binary- (%size r) 1))
                    by: (unary- (%by r)) size: (%size r))))

(define-method reverse! ((r <range>))
  (bind ((reversed (reverse r)))
    (set! (%from r) (%from reversed))
    (set! (%by r) (%by reversed))
    r))

(define-method binary= ((x <range>) (y <range>))
  (and (= (%size x) (%size y))
       (or (and (%size x) (zero? (%size x)))
           (and (= (%from x) (%from y))
                (or (and (%size x) (= (%size x) 1))
                    (= (%by x) (%by y)))))))

;;; The intersection of two ranges is a range, and is found without
;;; walking an unbounded one.  If either is bounded, its elements are
;;; tested.  Two unbounded ranges running the same way meet in an
;;; arithmetic progression found as the Chinese remainder theorem finds
;;; it (their values must then be rationals); running opposite ways, they
;;; meet only between their starts.
(define-method intersection ((x <range>) (y <range>) #key test)
  (cond ((or (%size x) (%size y)) (%intersect-bounded x y))
        ((zero? (%by x)) (%intersect-bounded (make <range> from: (%from x) size: 1) y))
        ((zero? (%by y)) (%intersect-bounded (make <range> from: (%from y) size: 1) x))
        ((and (negative? (%by x)) (negative? (%by y)))
         (%negate-range (%intersect-increasing (%negate-range x) (%negate-range y))))
        ((and (positive? (%by x)) (positive? (%by y))) (%intersect-increasing x y))
        (else:
         ;; Opposite ways: between the rising one's start and the falling one's
         (bind ((up (if (positive? (%by x)) x y))
                (down (if (positive? (%by x)) y x)))
           (if (binary< (%from down) (%from up))
               (make <range> from: 0 size: 0)
               (%intersect-bounded (range from: (%from up) through: (%from down) by: (%by up))
                                   down))))))

;;; The elements of the bounded range X that are in Y, as a range
(define %intersect-bounded
  (method (x y)
    (bind ((bounded (if (%size x) x y))
           (other (if (%size x) y x)))
      (bind ((common (choose (method (e) (member? e other)) (%elements bounded))))
        (cond ((id? common '()) (make <range> from: 0 size: 0))
              ((id? (cdr common) '()) (make <range> from: (car common) size: 1))
              (else: (make <range> from: (car common)
                                   by: (binary- (car (cdr common)) (car common))
                                   size: (%list-length common))))))))

(define %negate-range
  (method (r)
    (make <range> from: (unary- (%from r)) by: (unary- (%by r)) size: (%size r))))

;;; g and s such that a * s is g modulo b (the extended Euclidean
;;; algorithm)
(define %extended-gcd
  (method (a b)
    (bind-methods ((loop (r0 r1 s0 s1)
                     (if (zero? r1)
                         (values r0 s0)
                         (bind ((q r (floor/ r0 r1)))
                           (loop r1 r s1 (binary- s0 (binary* q s1)))))))
      (loop a b 1 0))))

(define %intersect-increasing
  (method (x y)
    (unless (every? (method (n) (%instance? n <rational>)) (list (%from x) (%by x) (%from y) (%by y)))
      (error "intersection: two unbounded ranges must be of rationals"))
    ;; Scaled to integers by the denominators' least common multiple
    (bind ((d (lcm (denominator (%from x)) (denominator (%by x))
                   (denominator (%from y)) (denominator (%by y)))))
      (bind ((f1 (binary* (%from x) d)) (b1 (binary* (%by x) d))
             (f2 (binary* (%from y) d)) (b2 (binary* (%by y) d)))
        (bind ((g (gcd b1 b2))
               (diff (binary- f2 f1)))
          (if (not (zero? (modulo diff g)))
              (make <range> from: 0 size: 0)
              (bind ((b1g (truncate/ b1 g)) (b2g (truncate/ b2 g)))
                (bind ((h s (%extended-gcd b1g b2g)))
                  (bind ((i0 (modulo (binary* (truncate/ diff g) s) b2g))
                         (step (binary* b1 b2g)))
                    (bind ((x0 (binary+ f1 (binary* i0 b1))))
                      (when (binary< x0 f2)
                        (set! x0 (binary+ x0 (binary* step (ceiling/ (binary- f2 x0) step)))))
                      (make <range> from: (binary/ x0 d) by: (binary/ step d))))))))))))
