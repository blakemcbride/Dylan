module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; <range> (chapter 12, "Operations on Ranges"): arithmetic sequences,
;;; possibly unbounded.  Element i is from + i * by; the size is #f if the
;;; range has no bound.  The iteration state is the index.
;;;
;;; copy-sequence, reverse and intersection make a range of a range, with
;;; the same elements (chapter 12).  Of a range of floats, whose elements
;;; are rounded, the new range keeps the grid of the one it is made from:
;;; its element i is the grid's element base + stride * i, origin + (base +
;;; stride * i) * origin-by, computed as that range computes it.  Its from
;;; and by are still its first element and its step.  %origin is #f for any
;;; other range.

;;; The slots take no init-keywords: make of a program's subclass of
;;; <range> takes only the subclass's own, as for the other collections.
(define-class <range> (<sequence>)
  (%from init-value: 0)
  (%by init-value: 1)
  (%size init-value: #f)
  (%origin init-value: #f)
  (%origin-by init-value: 1)
  (%base init-value: 0)
  (%stride init-value: 1))

(define-method class-for-copy ((r <range>)) <list>)

;;; The largest range from FROM by BY such that no value is at or beyond
;;; UP-TO, none beyond THROUGH, and there are no more than SIZE values.
;;; range is a generic function (chapter 12).
(define-generic-function range (#key from up-to through by size))

(define-method range (#key (from 0) up-to through (by 1) size)
  ;; Each bound and the step a real number, and size, if given, a count.
  (bind-methods ((check (name value optional?)
                   (unless (or (and optional? (not value)) (%instance? value <real>))
                     (error "range: ~a must be a real number: ~s" name value))))
    (check "from:" from #f)
    (check "up-to:" up-to #t)
    (check "through:" through #t)
    (check "by:" by #f))
  (unless (or (not size) (and (%instance? size <integer>) (not (negative? size))))
    (error "range: size: must be a non-negative integer: ~s" size))
  (bind ((count size))
    (bind-methods ((limit (n) (when (or (not count) (binary< n count)) (set! count n))))
      ;; With by: 0 every value is FROM: the range is unbounded if FROM is
      ;; within the bound, and empty if not.
      (when up-to
        (if (zero? by)
            (unless (binary< from up-to) (limit 0))
            (limit (max 0 (ceiling/ (binary- up-to from) by)))))
      (when through
        (if (zero? by)
            (when (binary< through from) (limit 0))
            (limit (max 0 (binary+ (floor/ (binary- through from) by) 1)))))
      (bind ((r (%make <range>)))
        (set! (%from r) from)
        (set! (%by r) by)
        (set! (%size r) count)
        r))))

(define-method make ((class (singleton <range>)) #rest arguments)
  (apply range arguments))

(define %range-element
  (method (r i)
    (if (%origin r)
        (binary+ (%origin r) (binary* (binary+ (%base r) (binary* (%stride r) i)) (%origin-by r)))
        (binary+ (%from r) (binary* i (%by r))))))

;;; Whether R's elements are floats, rounded
(define %float-range?
  (method (r)
    (or (%origin r) (%instance? (%from r) <float>) (%instance? (%by r) <float>))))

;;; R's grid: its element K is origin + K * by
(define %grid-origin (method (r) (or (%origin r) (%from r))))
(define %grid-by (method (r) (if (%origin r) (%origin-by r) (%by r))))

;;; A range of N elements (#f, unbounded) of R, those at the indexes K0,
;;; K0 + D, K0 + 2D, ...: on R's grid if R is a range of floats.
(define %sub-range
  (method (r k0 d n)
    (if (%float-range? r)
        (bind ((s (%make <range>))
               (origin (%grid-origin r))
               (by (%grid-by r))
               (base (binary+ (%base r) (binary* (%stride r) k0)))
               (stride (binary* (%stride r) d)))
          (set! (%origin s) origin)
          (set! (%origin-by s) by)
          (set! (%base s) base)
          (set! (%stride s) stride)
          (set! (%from s) (binary+ origin (binary* base by)))
          (set! (%by s) (binary* stride by))
          (set! (%size s) n)
          s)
        (make <range> from: (%range-element r k0) by: (binary* d (%by r)) size: n))))

;;; The index of R whose element is the grid's element K, if any
(define %grid-index
  (method (r k)
    (bind ((i (binary/ (binary- k (%base r)) (%stride r))))
      (and (%range-index? r i) i))))

(define %range-index?
  (method (r i)
    (and (%instance? i <integer>) (not (negative? i))
         (or (not (%size r)) (binary< i (%size r))))))

(define-method size ((r <range>)) (%size r))

;;; add and concatenate answer a list, the class-for-copy, as for any
;;; sequence (chapter 12); an unbounded range has no end to stop at.
(define-method add ((r <range>) new-element)
  (unless (%size r) (error "add: the range is unbounded"))
  (next-method))

(define-method concatenate ((r <range>) #rest more)
  (unless (%size r) (error "concatenate: the range is unbounded"))
  (next-method))

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

(define %range-ref
  (method (r key default)
    (bind ((index (%index-key key)))
      (if (and index (%range-index? r index))
          (%range-element r index)
          (if (id? default %no-default)
              (error "element: there is no element at the index ~s" key)
              default)))))

(define-method element ((r <range>) key #key (default %no-default))
  (%range-ref r key default))

;;; The library's own (04-collections.dyl), for a direct instance
(define-method %library-size ((r <range>))
  (if (id? (%object-class r) <range>) (%size r) (size r)))
(define-method %library-element ((r <range>) key default)
  (if (id? (%object-class r) <range>) (%range-ref r key default) (next-method)))

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
        ((%float-range? r) (%float-range-member? value r test))
        (else:
         (bind ((from (%from r)) (by (%by r))
                (k (if (and (%exact? value) (%exact? from) (%exact? by))
                       (binary/ (binary- (%exact value) (%exact from)) (%exact by))
                       (%float-range-key value r test))))
           (and (%instance? k <integer>)
                (%range-index? r k)
                (test value (%range-element r k))
                #t)))))

;;; Whether VALUE is an element of the range of floats R: one of the grid
;;; elements by the index (VALUE - origin) / by rounds to, an element
;;; being rounded, that is an element of R.  Its elements are floats: id?
;;; to no rational.
(define %float-range-member?
  (method (value r test)
    (and (or (not (id? test id?)) (%instance? value <float>))
         (bind ((q (binary/ (binary- value (%grid-origin r)) (%grid-by r))))
           (and (binary< (abs q) 4.0e18)
                (bind ((k (round q)))
                  (any? (method (d)
                          (bind ((i (%grid-index r (binary+ k d))))
                            (and i (test value (%range-element r i)))))
                        '(0 -1 1)))
                #t)))))

;;; A real, exactly: a float as the rational it holds
(define %exact
  (method (x) (if (%instance? x <float>) (as <rational> x) x)))

;;; Whether X's exact value is a rational of 63 bits: a float M * 2^E is
;;; if its denominator 2^-E, or M * 2^E, fits.
(define %exact?
  (method (x)
    (or (not (%instance? x <float>))
        (bind ((m e (%float-parts x)))
          (if (%number< e 0)
              (not (%number< e -61))
              (and (%number< e 62)
                   (bind ((limit (%ash 4611686018427387903 (%number-sub 0 e))))
                     (and (not (%number< limit m)) (not (%number< m (%number-sub 0 limit)))))))))))

;;; The index of the range R at which VALUE might be, when a float among
;;; VALUE and R's from and by is not a 63-bit rational: (VALUE - FROM) / BY
;;; in floats, if integral and an index of R; the element there is then
;;; tested.  An index beyond 63 bits, of an unbounded range, is an integer
;;; overflow.  A float is never id? to the rational elements of a range of
;;; rationals, nor a rational to a float range's.
(define %float-range-key
  (method (value r test)
    (bind ((from (%from r)) (by (%by r)))
      (if (and (id? test id?)
               (not (id? (%instance? value <float>)
                         (or (%instance? from <float>) (%instance? by <float>)))))
          #f
          (bind ((q (binary/ (binary- value from) by)))
            (and (integral? q)
                 (not (%number< q 0))
                 (or (not (%size r)) (%number< q (%size r)))
                 (as <integer> q)))))))

(define-method copy-sequence ((r <range>) #key (start 0) end)
  ;; START and END are indexes of R, END exclusive, as for any sequence
  (bind ((n (%size r)))
    (when (or (not (%instance? start <integer>)) (negative? start)
              (and n (binary< n start))
              (and end (or (not (%instance? end <integer>)) (binary< end start)
                           (and n (binary< n end)))))
      (error "copy-sequence: bad start: or end: for ~s: ~s ~s" r start end)))
  (bind ((stop (or end (%size r))))
    (%sub-range r start 1 (and stop (max 0 (binary- stop start))))))

(define-method reverse ((r <range>))
  (unless (%size r) (error "reverse: the range is unbounded"))
  (if (zero? (%size r))
      (make <range> from: (%from r) by: (%by r) size: 0)
      (%sub-range r (binary- (%size r) 1) -1 (%size r))))

(define-method reverse! ((r <range>))
  (bind ((reversed (reverse r)))
    (set! (%from r) (%from reversed))
    (set! (%by r) (%by reversed))
    (set! (%origin r) (%origin reversed))
    (set! (%origin-by r) (%origin-by reversed))
    (set! (%base r) (%base reversed))
    (set! (%stride r) (%stride reversed))
    r))

;;; An unbounded range is = only to a range with the same start and step
;;; (a sequence's elements would never end), on the same grid, and its
;;; hash is of those.  A bounded range made on another's grid is compared
;;; by its elements.
(define-method =hash ((r <range>))
  (if (%size r)
      (next-method)
      (%entry-hash (=hash (%from r)) (=hash (%by r)))))

(define-method binary= ((x <range>) (y <range>))
  (cond ((not (= (%size x) (%size y))) #f)
        ((and (%size x) (or (%origin x) (%origin y))) (next-method))
        ((not (%size x))
         (and (= (%from x) (%from y)) (= (%by x) (%by y))
              (= (%grid-origin x) (%grid-origin y)) (= (%grid-by x) (%grid-by y))
              (= (%base x) (%base y)) (= (%stride x) (%stride y))))
        (else:
         (or (zero? (%size x))
             (and (= (%from x) (%from y))
                  (or (= (%size x) 1)
                      (= (%by x) (%by y))))))))

;;; The intersection of two ranges is a range, as the interactive
;;; implementation makes it (src/runtime-collections-range.lisp), and is
;;; found without walking an unbounded one.  The test is id? unless given;
;;; with any other than id? or =, the ranges are intersected as sequences.
;;; A range of rationals is a grid -- the numbers its start is congruent
;;; to modulo its step -- clipped to an interval, unbounded on one side if
;;; the range is; two meet in the grid of their common points, whose step
;;; is the least common multiple of theirs, clipped to both intervals, and
;;; run the way the first runs.  Ranges of floats, both bounded, are
;;; intersected as sequences: the elements of the first that are in the
;;; second, which must be at indexes in steps.
(define-method intersection ((x <range>) (y <range>) #key (test id?))
  (cond ((not (or (id? test id?) (id? test =))) (next-method))
        ((zero? (%by x))
         (if (member? (%from x) y) (copy-sequence x) (%empty-range)))
        ((zero? (%by y))
         (bind ((v (%from y)))
           (if (and (member? v y) (member? v x)) (range from: v size: 1) (%empty-range))))
        ((every? (method (n) (%instance? n <rational>)) (list (%from x) (%by x) (%from y) (%by y)))
         (%grid-intersection x y))
        ((and (%size x) (%size y)) (%float-intersection x y test))
        (else: (error "intersection: unbounded ranges of floats: ~s ~s" x y))))

(define %empty-range (method () (range from: 0 size: 0)))

(define %float-intersection
  (method (x y test)
    (bind ((ks (for ((i (binary- (%size x) 1) (binary- i 1))
                     (ks '() (if (member? (%range-element x i) y test: test) (cons i ks) ks)))
                    ((negative? i) ks))))
      (cond ((id? ks '()) (%empty-range))
            ((id? (cdr ks) '()) (%sub-range x (car ks) 1 1))
            (else:
             (bind ((d (binary- (car (cdr ks)) (car ks))))
               (for ((l (cdr ks) (cdr l)))
                    ((id? (cdr l) '()))
                 (unless (= (binary- (car (cdr l)) (car l)) d)
                   (error "intersection: the elements of ~s in ~s are not in steps" x y)))
               (%sub-range x (car ks) d (%list-length ks))))))))

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
