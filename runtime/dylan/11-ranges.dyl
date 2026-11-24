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
      ;; Of integers, the counts are exact whatever the size of the bound
      ;; less FROM (%range-steps).
      (when up-to
        (cond ((zero? by) (unless (binary< from up-to) (limit 0)))
              ((%integers? up-to from by) (limit (%range-steps 1 up-to from by)))
              (else: (limit (%max 0 (ceiling/ (binary- up-to from) by))))))
      (when through
        (cond ((zero? by) (when (binary< through from) (limit 0)))
              ((%integers? through from by) (limit (%range-steps 2 through from by)))
              (else: (limit (%max 0 (binary+ (floor/ (binary- through from) by) 1))))))
      (bind ((r (%make <range>)))
        (set! (%from r) from)
        (set! (%by r) by)
        (set! (%size r) count)
        r))))

;;; Whether A, B and C are all integers
(define %integers?
  (method (a b c)
    (and (%instance? a <integer>) (%instance? b <integer>) (%instance? c <integer>))))

(define-method make ((class (singleton <range>)) #rest arguments)
  (apply range arguments))

;;; Of integers, from + i * by exactly: an integer overflow only if the
;;; element is beyond 63 bits, not i * by (%range-steps).
(define %range-element
  (method (r i)
    (cond ((%origin r)
           (binary+ (%origin r) (binary* (binary+ (%base r) (binary* (%stride r) i)) (%origin-by r))))
          ((%integers? (%from r) i (%by r)) (%range-steps 4 (%from r) i (%by r)))
          (else: (binary+ (%from r) (binary* i (%by r)))))))

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
        ;; An empty range starts where it is cut from R, if that fits.
        (make <range>
              from: (if (and (id? n 0) (%integers? (%from r) k0 (%by r)))
                        (or (%range-steps 5 (%from r) k0 (%by r)) (%from r))
                        (%range-element r k0))
              by: (binary* d (%by r)) size: n))))

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

(define-method add! ((r <range>) new-element)
  (unless (%size r) (error "add!: the range is unbounded"))
  (next-method))

(define-method concatenate ((r <range>) #rest more)
  (unless (%size r) (error "concatenate: the range is unbounded"))
  (next-method))

;;; add-new and add-new! of an unbounded range: the range itself if the
;;; element is a member, which member? can tell with id? or =; else add's
;;; error, as add would give (chapter 12), not a search with no end.
(define %add-new-unbounded
  (method (r new-element test)
    (if (and (or (id? test id?) (id? test =)) (member? new-element r test: test))
        r
        (error "add: the range is unbounded"))))

(define-method add-new ((r <range>) new-element #key (test id?))
  (if (%size r) (next-method) (%add-new-unbounded r new-element test)))

(define-method add-new! ((r <range>) new-element #key (test id?))
  (if (%size r) (next-method) (%add-new-unbounded r new-element test)))

;;; sort and sort! of an unbounded range: as the range as a sequence
(define-method sort ((r <range>) #key (test <) stable)
  (unless (%size r) (error "as: the range is unbounded"))
  (next-method))

(define-method sort! ((r <range>) #key (test <) stable)
  (unless (%size r) (error "as: the range is unbounded"))
  (next-method))

;;; An unbounded range as another collection would have no end.  As a
;;; class that is no collection, it cannot be converted, bounded or not.
(define-method as ((class <class>) (r <range>))
  (if (or (%size r) (%instance? r class) (not (%subclass? class <collection>)))
      (next-method)
      (error "as: the range is unbounded")))

;;; A collection as a range: its numbers must go up or down by one step.
(define-method as ((class (singleton <range>)) (c <collection>))
  (if (%instance? c <range>)
      c
      (bind ((l (as <list> c)))
        (cond ((id? l '()) (%empty-range))
              ((id? (cdr l) '()) (range from: (car l) size: 1))
              ((not (%list-every? (method (x) (%instance? x <number>)) l))
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

(define-method last ((r <range>) #key (default %no-default))
  (cond ((not (%size r)) (error "last: the sequence is unbounded"))
        ((zero? (%size r))
         (if (id? default %no-default) (error "last: the sequence is empty") default))
        (else: (%range-element r (binary- (%size r) 1)))))

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
             ;; #t, whatever true value the test answers
             (if (%list-any? (method (e) (test value e)) (%elements r)) #t #f)
             (error "member?: a test: on an unbounded range might not terminate")))
        ((not (%instance? value <real>)) #f)
        ((zero? (%by r))
         (and (%range-index? r 0) (test value (%from r)) #t))
        ((%float-range? r) (%float-range-member? value r test))
        ;; A range of integers: VALUE's index exactly, whatever the size of
        ;; VALUE less from.  The element there is VALUE's exact value; past
        ;; 63 bits, the index is of an unbounded range alone.
        ((and (%instance? (%from r) <integer>) (%instance? (%by r) <integer>) (%exact? value))
         (bind ((v (%exact value)))
           (and (%instance? v <integer>)
                (bind ((k (%range-steps 0 v (%from r) (%by r))))
                  (and k
                       (if (id? k #t) (not (%size r)) (%range-index? r k))
                       (test value v)
                       #t)))))
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
                  (%list-any? (method (d)
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
    (%sub-range r start 1 (and stop (%max 0 (binary- stop start))))))

(define-method reverse ((r <range>))
  (unless (%size r) (error "reverse: the range is unbounded"))
  (if (zero? (%size r))
      (make <range> from: (%from r) by: (%by r) size: 0)
      (%sub-range r (binary- (%size r) 1) -1 (%size r))))

(define-method reverse! ((r <range>))
  (unless (%size r) (error "reverse!: the range is unbounded"))
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
         (if (member? (%from x) y test: test) (copy-sequence x) (%empty-range)))
        ((zero? (%by y))
         (bind ((v (%from y)))
           (if (and (member? v y) (member? v x test: test))
               (range from: (%range-match x v test) size: 1)
               (%empty-range))))
        ((%list-every? (method (n) (%instance? n <rational>)) (list (%from x) (%by x) (%from y) (%by y)))
         (%grid-intersection x y))
        ((%size x) (%float-intersection x y test))
        ((%size y) (%float-intersection (%range-window x y) y test))
        (else: (error "intersection: unbounded ranges of floats: ~s ~s" x y))))

(define %empty-range (method () (range from: 0 size: 0)))

;;; The element of the range X, of a step not zero, that TEST, id? or =,
;;; finds the same as V, a member of X: V itself if the test is id? or all
;;; are integers; else the element at V's index, which = may find of
;;; another class (1.0 for 1).
(define %range-match
  (method (x v test)
    (if (or (id? test id?)
            (and (%instance? v <integer>) (%instance? (%from x) <integer>)
                 (%instance? (%by x) <integer>)))
        v
        (bind ((k (%divide 2 (binary/ (binary- v (%from x)) (%by x)) 1))
               (at? (method (i) (and (%range-index? x i) (test v (%range-element x i))))))
          (cond ((at? k) (%range-element x k))
                ((at? (binary- k 1)) (%range-element x (binary- k 1)))
                ((at? (binary+ k 1)) (%range-element x (binary+ k 1)))
                (else: v))))))

;;; The elements of the unbounded range X that may be in the bounded range
;;; Y: those between Y's lowest and highest, with one more at each end.
;;; Indexes from 4.0e18 on are of no element, as for member?.
(define %range-window
  (method (x y)
    (bind ((lo hi (%range-interval y))
           (q1 (binary/ (binary- lo (%from x)) (%by x)))
           (q2 (binary/ (binary- hi (%from x)) (%by x)))
           (qa (if (binary< q1 q2) q1 q2))
           (qb (if (binary< q1 q2) q2 q1)))
      (if (or (binary< qb -1) (not (binary< qa 4.0e18)))
          (%empty-range)
          (bind ((a (if (binary< qa 1) 0 (binary- (floor qa) 1)))
                 (b (binary+ (ceiling (if (binary< qb 4.0e18) qb 4.0e18)) 1)))
            (%sub-range x a 1 (binary+ (binary- b a) 1)))))))

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

;;; The least common multiple of the positive integers A and B, #f if it
;;; is beyond 63 bits
(define %lcm-or-false
  (method (a b)
    (bind ((q (truncate/ a (%gcd a b))))
      (if (binary< (truncate/ 4611686018427387903 b) q) #f (binary* q b)))))

;;; C's integer overflow, for a result beyond 63 bits
(define %integer-overflow (method () (%range-steps 4 4611686018427387903 1 1)))

(define %grid-intersection
  (method (x y)
    ;; Of rationals, the grids scaled to integers by their common
    ;; denominator D.  The common points nearest the ends of the interval
    ;; both ranges cover are found exactly, in 128 bits (%grid-point).
    (bind ((d (if (%list-every? (method (n) (%instance? n <integer>))
                                (list (%from x) (%by x) (%from y) (%by y)))
                  1
                  (lcm (denominator (%from x)) (denominator (%by x))
                       (denominator (%from y)) (denominator (%by y)))))
           (scale (method (n) (if (id? d 1) n (binary* n d))))
           (unscale (method (n) (if (id? d 1) n (binary/ n d))))
           (m1 (abs (scale (%by x))))
           (m2 (abs (scale (%by y))))
           (point (method (direction v)
                    (%grid-point direction (scale (%from x)) m1 (scale (%from y)) m2 (scale v)))))
      (bind ((lo1 hi1 (%range-interval x))
             (lo2 hi2 (%range-interval y)))
        (bind ((lo (if (and lo1 lo2) (%max lo1 lo2) (or lo1 lo2)))
               (hi (if (and hi1 hi2) (%min hi1 hi2) (or hi1 hi2)))
               (first (and lo (point 0 lo)))
               (last (and hi (point 1 hi))))
          (cond ((or (and lo (not first)) (and hi (not last))) (%empty-range))
                ((and lo hi)
                 ;; #t: beyond 63 bits, and so past the other end
                 (if (or (id? first #t) (id? last #t) (binary< last first))
                     (%empty-range)
                     ;; The step of a single point is the period if it fits.
                     (bind ((period (if (= first last)
                                        (or (%lcm-or-false m1 m2) m1)
                                        (lcm m1 m2))))
                       (if (positive? (%by x))
                           (range from: (unscale first) through: (unscale last)
                                  by: (unscale period))
                           (range from: (unscale last) through: (unscale first)
                                  by: (unscale (unary- period)))))))
                ((id? first #t) (%integer-overflow))
                (first (range from: (unscale first) by: (unscale (lcm m1 m2))))
                ((id? last #t) (%integer-overflow))
                (else: (range from: (unscale last) by: (unscale (unary- (lcm m1 m2)))))))))))
