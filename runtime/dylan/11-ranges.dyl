module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; <range> (chapter 12, "Operations on Ranges"): arithmetic sequences,
;;; possibly unbounded.  Element i is from + i * by; the size is #f if the
;;; range has no bound.  The iteration state is the index.

;;; The slots take no init-keywords: make of a program's subclass of
;;; <range> takes only the subclass's own, as for the other collections.
(define-class <range> (<sequence>)
  (%from init-value: 0)
  (%by init-value: 1)
  (%size init-value: #f))

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
      (bind ((r (%make <range>)))
        (set! (%from r) from)
        (set! (%by r) by)
        (set! (%size r) count)
        r))))

(define-method make ((class (singleton <range>)) #rest arguments)
  (apply range arguments))

(define %range-element (method (r i) (binary+ (%from r) (binary* i (%by r)))))

(define %range-index?
  (method (r i)
    (and (%instance? i <integer>) (not (negative? i))
         (or (not (%size r)) (binary< i (%size r))))))

(define-method size ((r <range>)) (%size r))

;;; A collection as a range: its numbers must go up or down by one step.
(define-method as ((class (singleton <range>)) (c <collection>))
  (if (%instance? c <range>)
      c
      (bind ((l (as <list> c)))
        (cond ((id? l '()) (%empty-range))
              ((id? (cdr l) '()) (range from: (car l) size: 1))
              ((not (every? (method (x) (%instance? x <number>)) l))
               (error "as: the elements of ~s are not numbers" c))
              (else:
               (bind ((step (binary- (car (cdr l)) (car l))))
                 (for ((prev (car (cdr l)) (car rest))
                       (rest (cdr (cdr l)) (cdr rest)))
                      ((id? rest '())
                       (make <range> from: (car l) by: step size: (%list-length l)))
                   (unless (= (binary- (car rest) prev) step)
                     (error "as: the elements of ~s are not in steps" c)))))))))
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

;;; member?'s test is id? unless given (chapter 12, design note 18).  With
;;; id? or = the range's arithmetic answers, so that it terminates on an
;;; unbounded range too: VALUE is a member if some index K has an element
;;; the same as VALUE -- id? to it, or = -- K found exactly, by rationals.
;;; Any other test is tried on each element, which only a bounded range
;;; has an end to.
(define-method member? (value (r <range>) #key (test id?))
  (cond ((not (or (id? test id?) (id? test =)))
         (if (%size r)
             (any? (method (e) (test value e)) (%elements r))
             (error "member?: a test: on an unbounded range might not terminate")))
        ((not (%instance? value <real>)) #f)
        ((zero? (%by r))
         (and (%range-index? r 0) (test value (%from r)) #t))
        (else:
         (bind ((k (binary/ (binary- (%exact value) (%exact (%from r))) (%exact (%by r)))))
           (and (%instance? k <integer>)
                (%range-index? r k)
                (test value (%range-element r k))
                #t)))))

;;; A real, exactly: a float as the rational it holds
(define %exact
  (method (x) (if (%instance? x <float>) (as <rational> x) x)))

(define-method copy-sequence ((r <range>) #key (start 0) end)
  ;; START and END are indexes of R, END exclusive, as for any sequence
  (bind ((n (%size r)))
    (when (or (not (%instance? start <integer>)) (negative? start)
              (and n (binary< n start))
              (and end (or (not (%instance? end <integer>)) (binary< end start)
                           (and n (binary< n end)))))
      (error "copy-sequence: bad start: or end: for ~s: ~s ~s" r start end)))
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

;;; An unbounded range is = only to a range with the same start and step
;;; (a sequence's elements would never end), and its hash is of those.
(define-method =hash ((r <range>))
  (if (%size r)
      (next-method)
      (%entry-hash (=hash (%from r)) (=hash (%by r)))))

(define-method binary= ((x <range>) (y <range>))
  (and (= (%size x) (%size y))
       (or (and (%size x) (zero? (%size x)))
           (and (= (%from x) (%from y))
                (or (and (%size x) (= (%size x) 1))
                    (= (%by x) (%by y)))))))

;;; The intersection of two ranges is a range, as the interactive
;;; implementation makes it (src/runtime-collections-range.lisp), and is
;;; found without walking an unbounded one.  The test is id? unless given;
;;; with any other than id? or =, the ranges are intersected as sequences.
;;; A range of rationals is a grid -- the numbers its start is congruent
;;; to modulo its step -- clipped to an interval, unbounded on one side if
;;; the range is; two meet in the grid of their common points, whose step
;;; is the least common multiple of theirs, clipped to both intervals, and
;;; run the way the first runs.  Ranges of floats, both bounded, are
;;; intersected as sequences.
(define-method intersection ((x <range>) (y <range>) #key (test id?))
  (cond ((not (or (id? test id?) (id? test =))) (next-method))
        ((zero? (%by x))
         (if (member? (%from x) y) (copy-sequence x) (%empty-range)))
        ((zero? (%by y))
         (bind ((v (%from y)))
           (if (and (member? v y) (member? v x)) (range from: v size: 1) (%empty-range))))
        ((every? (method (n) (%instance? n <rational>)) (list (%from x) (%by x) (%from y) (%by y)))
         (%grid-intersection x y))
        ((and (%size x) (%size y)) (as <range> (next-method)))
        (else: (error "intersection: unbounded ranges of floats: ~s ~s" x y))))

(define %empty-range (method () (range from: 0 size: 0)))

;;; The lowest and highest elements of R, #f where unbounded
(define %range-interval
  (method (r)
    (bind ((end (and (%size r) (%range-element r (binary- (%size r) 1)))))
      (if (positive? (%by r)) (values (%from r) end) (values end (%from r))))))

;;; The least common multiple of positive rationals
(define %rational-lcm
  (method (a b)
    (bind ((d (lcm (denominator a) (denominator b))))
      (binary/ (lcm (binary* a d) (binary* b d)) d))))

(define %grid-intersection
  (method (x y)
    (bind ((d1 (abs (%by x)))
           (d2 (abs (%by y)))
           (period (%rational-lcm d1 d2))
           ;; A point common to both grids, among one period of the first's
           (common (bind-exit (return)
                     (for ((k 0 (binary+ k 1)))
                          ((not (binary< k (binary/ period d1))) #f)
                       (bind ((p (binary+ (%from x) (binary* k d1))))
                         (when (%instance? (binary/ (binary- p (%from y)) d2) <integer>)
                           (return p)))))))
      (bind ((lo1 hi1 (%range-interval x))
             (lo2 hi2 (%range-interval y)))
        (bind ((lo (if (and lo1 lo2) (max lo1 lo2) (or lo1 lo2)))
               (hi (if (and hi1 hi2) (min hi1 hi2) (or hi1 hi2))))
          (bind ((first (and common lo
                             (binary+ common (binary* period (ceiling/ (binary- lo common) period)))))
                 (last (and common hi
                            (binary- common (binary* period (ceiling/ (binary- common hi) period))))))
            (cond ((not common) (%empty-range))
                  ((and first last)
                   (cond ((binary< last first) (%empty-range))
                         ((positive? (%by x)) (range from: first through: last by: period))
                         (else: (range from: last through: first by: (unary- period)))))
                  (first (range from: first by: period))
                  (else: (range from: last by: (unary- period))))))))))
