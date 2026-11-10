;;;; -*- Mode: Lisp -*-
;;;;
;;;;              Copyright 1992 Digital Equipment Corporation
;;;;                         All Rights Reserved
;;;;
;;;; Permission to use, copy, and modify this software and its documentation is
;;;; hereby granted only under the following terms and conditions.  Both the
;;;; above copyright notice and this permission notice must appear in all copies
;;;; of the software, derivative works or modified versions, and any portions
;;;; thereof, and both notices must appear in supporting documentation.
;;;;
;;;; Users of this software agree to the terms and conditions set forth herein,
;;;; and hereby grant back to Digital a non-exclusive, unrestricted, royalty-free
;;;; right and license under any changes, enhancements or extensions made to the
;;;; core functions of the software, including but not limited to those affording
;;;; compatibility with other hardware or software environments, but excluding
;;;; applications which incorporate this software.  Users further agree to use
;;;; their best efforts to return to Digital any such changes, enhancements or
;;;; extensions that they make and inform Digital of noteworthy uses of this
;;;; software.  Correspondence should be provided to Digital at:
;;;;
;;;;			Director, Cambridge Research Lab
;;;;			Digital Equipment Corp
;;;;			One Kendall Square, Bldg 700
;;;;			Cambridge MA 02139
;;;;
;;;; This software may be distributed (but not offered for sale or transferred
;;;; for compensation) to third parties, provided such third parties agree to
;;;; abide by the terms and conditions of this notice.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
;;;; WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
;;;; CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;;;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
;;;; ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;;;; SOFTWARE.
;;;;
;;;; The Common Lisp translation is Copyright 2026 Blake McBride.  It is a
;;;; derivative work of Thomas and is governed by the notice above; see
;;;; ../LICENSE.
;;;;
;;;; Specializations for the Range Type
;;;;                        (was Thomas's src/runtime-collections-range.scm)

(in-package #:dylan)

(declaim (special dylan/get-range-start dylan/get-range-step dylan/get-range-end
                  dylan/set-range-start! dylan/set-range-step!
                  dylan/set-range-end! dylan/get-range-count
                  dylan/set-range-count! dylan/get-range-grid
                  dylan/set-range-grid!))

(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <range>))
                     (collection ,<collection>)) nil nil nil)
  ;; A collection as a range: its numbers must go up or down by one step,
  ;; as the compiled library has it.
  (lambda (class collection)
    (declare (ignore class))
    (if (true? (dylan-call dylan/instance? collection <range>))
        collection
        (let ((l (sequence->list collection)))
          (cond ((null l) (empty-range))
                ((null (cdr l)) (dylan-call dylan/range :from (car l) :size 1))
                ((notevery (lambda (x) (true? (dylan-call dylan/instance? x <number>))) l)
                 (dylan-call dylan/error "as: the elements of ~s are not numbers"
                             collection))
                (t
                 (let ((step (dylan-call dylan/- (second l) (first l))))
                   (loop for (prev x) on (cdr l)
                         while x
                         unless (true? (dylan-call dylan/= (dylan-call dylan/- x prev) step))
                           do (dylan-call dylan/error "as: the elements of ~s are not in steps"
                                          collection))
                   (dylan-call dylan/range :from (first l) :by step
                                           :size (length l))))))))))

;;;
;;; RANGE SPECIALIZED MAKE
;;; <range> has four slots: start, step, end and count
;;;   NIL for end means an unbounded range.  END is the last element.
;;;   A step of zero (by: 0) repeats START, so END cannot say how many
;;;   elements there are; COUNT does, NIL if unbounded.  COUNT is used
;;;   only when the step is zero.
;;; MAKE takes the keywords RANGE does (design note 30).
;;;

(defun empty-range ()
  (dylan-call dylan/range :from 0 :size 0))

(defparameter dylan/get-range-start nil)
(defparameter dylan/get-range-step nil)
(defparameter dylan/get-range-end nil)
(defparameter dylan/set-range-start! nil)
(defparameter dylan/set-range-step! nil)
(defparameter dylan/set-range-end! nil)
(defparameter dylan/get-range-count nil)
(defparameter dylan/set-range-count! nil)

(create-private-slot <range> <object> "internal-range-start"
                     (lambda (set get)
                       (setf dylan/set-range-start! set)
                       (setf dylan/get-range-start get))
                     :init (lambda () 0))
(create-private-slot <range> <object> "internal-range-step"
                     (lambda (set get)
                       (setf dylan/set-range-step! set)
                       (setf dylan/get-range-step get))
                     :init (lambda () 1))
(create-private-slot <range> <object> "internal-range-end"
                     (lambda (set get)
                       (setf dylan/set-range-end! set)
                       (setf dylan/get-range-end get))
                     :init (lambda () nil))
(create-private-slot <range> <object> "internal-range-count"
                     (lambda (set get)
                       (setf dylan/set-range-count! set)
                       (setf dylan/get-range-count get))
                     :init (lambda () nil))

;;; copy-sequence, reverse and intersection make a range of a range, with
;;; the same elements (chapter 12).  Of a range of floats, whose elements
;;; are rounded, the new range keeps the grid of the one it is made from:
;;; its element i is the grid's element base + stride * i, origin + (base +
;;; stride * i) * origin-step, computed as that range computes it, as in
;;; the compiled library (11-ranges.dyl).  Its start, step and end are
;;; still its first element, its step and its last element, and COUNT its
;;; size.  The grid is NIL for any other range.

(defstruct (range-grid (:constructor make-range-grid (origin step base stride)))
  origin step base stride)

(defparameter dylan/get-range-grid nil)
(defparameter dylan/set-range-grid! nil)

(create-private-slot <range> <object> "internal-range-grid"
                     (lambda (set get)
                       (setf dylan/set-range-grid! set)
                       (setf dylan/get-range-grid get))
                     :init (lambda () nil))

;;; A range's bounds and step are the reals given, an <extended-float>
;;; among them: a structure (compat.lisp), not a CL number.  The range's
;;; arithmetic is done on the double-float it holds, and an element or
;;; bound computed from it is an <extended-float> again, as the generic
;;; arithmetic of a compiled program's library makes it (chapter 14).

(defun range-number (x)
  "X, a range's bound or step, as a CL real (NIL stays NIL)."
  (if (extended-float? x) (extended-float-value x) x))

(defun range-result (value &rest operands)
  "VALUE, computed from the CL reals of OPERANDS: an <extended-float> if
any of them is one."
  (if (some #'extended-float? operands)
      (make-extended-float (coerce value 'double-float))
      value))

(defun range-element-at (start step k)
  "START + K*STEP, the element at the index K."
  (range-result (+ (range-number start) (* k (range-number step))) start step))

(defun zero-step? (range)
  (zerop (range-number (dylan-call dylan/get-range-step range))))

(add-method
 dylan/make
 (dylan%function->method
  (make-param-list `((range ,(dylan%make-singleton <range>))) nil nil :any)
  (lambda (range &rest rest)
    (declare (ignore range))
    (dylan-apply dylan/range rest))))

;;;
;;; FUNCTIONS FOR COLLECTIONS (page 99)
;;;
(define-direct-slot range-start set-range-start! <range>
  dylan/get-range-start dylan/set-range-start!)
(define-direct-slot range-step set-range-step! <range>
  dylan/get-range-step dylan/set-range-step!)
(define-direct-slot range-end set-range-end! <range>
  dylan/get-range-end dylan/set-range-end!)
(define-direct-slot range-count set-range-count! <range>
  dylan/get-range-count dylan/set-range-count!)
(define-direct-slot range-grid set-range-grid! <range>
  dylan/get-range-grid dylan/set-range-grid!)

(defun range-ref (range i)
  "RANGE's element at the index I."
  (let ((grid (range-grid range)))
    (if grid
        (range-element-at (range-grid-origin grid) (range-grid-step grid)
                          (+ (range-grid-base grid) (* (range-grid-stride grid) i)))
        (range-element-at (range-start range) (range-step range) i))))

(defun float-range-p (range)
  "Whether RANGE's elements are floats, rounded."
  (or (range-grid range)
      (let ((start (range-start range)) (step (range-step range)))
        (or (floatp start) (extended-float? start)
            (floatp step) (extended-float? step)))))

(defun range-grid-of (range)
  "RANGE's grid, as a RANGE-GRID: its own, or its start and step's."
  (or (range-grid range)
      (make-range-grid (range-start range) (range-step range) 0 1)))

(defun sub-range (range k0 d n)
  "A range of N elements (NIL, unbounded) of RANGE, those at the indexes
K0, K0 + D, K0 + 2D, ...: on RANGE's grid if RANGE is a range of floats."
  (if (float-range-p range)
      (let* ((grid (range-grid-of range))
             (origin (range-grid-origin grid))
             (step (range-grid-step grid))
             (base (+ (range-grid-base grid) (* (range-grid-stride grid) k0)))
             (stride (* (range-grid-stride grid) d))
             (new (dylan%make-<object> <range>)))
        (set-range-grid! new (make-range-grid origin step base stride))
        (set-range-start! new (range-element-at origin step base))
        (set-range-step! new (range-result (* stride (range-number step)) step))
        (set-range-count! new n)
        (set-range-end! new (and n (if (plusp n) (range-ref new (- n 1)) (range-start new))))
        new)
      (let ((from (range-ref range k0))
            (by (* d (range-step range))))
        (if n
            (dylan-call dylan/range :from from :by by :size n)
            (dylan-call dylan/range :from from :by by)))))

(defun grid-index (range k)
  "The index of RANGE whose element is its grid's element K, or NIL."
  (let* ((grid (range-grid-of range))
         (i (/ (- k (range-grid-base grid)) (range-grid-stride grid)))
         (size (range-size range)))
    (and (integerp i) (>= i 0) (or (null size) (< i size)) i)))

(defun range-size (range)
  "The size of RANGE, or NIL if it is unbounded."
  (let ((start (range-number (range-start range)))
        (end (range-number (range-end range)))
        (step (range-number (range-step range))))
    ;; END is on the range's grid (see RANGE), so (END - START)/STEP is an
    ;; integer -- up to float rounding, which ROUND undoes.
    (cond
      ((or (zerop step) (range-grid range)) (range-count range))
      (end
       (if (funcall (if (plusp step) #'< #'>) end start)
           0
           (+ (round (- end start) step) 1)))
      (t nil))))

(add-method
 dylan/size
 (one-arg 'range <range>
          (lambda (range) (or-false (range-size range)))))

;;; A range's element at an index is FROM + index*BY, as its iteration
;;; gives it (runtime-collections-iterate.lisp), found without the walk.
(add-method
 dylan/element
 (dylan%dylan-callable->method
  (make-param-list `((range ,<range>) (key ,<integer>)) nil nil '(:default))
  (lambda (multiple-values next-method range key &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:default))
    (let ((size (collection-size range)))
      (if (and (>= key 0) (or (null size) (< key size)))
          (range-ref range key)
          (let* ((default-marker (cons 1 2))
                 (default (dylan%find-keyword rest :default
                                              (lambda () default-marker))))
            (if (eq default default-marker)
                (dylan-call dylan/error
                            "element: there is no element at the index ~s"
                            key)
                default)))))))

(add-method dylan/class-for-copy
            (dylan%function->method one-range
                                    (lambda (x) (declare (ignore x)) <list>)))

(add-method
 dylan/member?
 (dylan%dylan-callable->method
  (make-param-list `((value ,<object>) (range ,<range>)) nil nil '(:test))
  (lambda (multiple-values next-method val range &rest keys)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method keys '(:test))
    ;; Chapter 12 and design note 18: the test defaults to id?.  With id?
    ;; or = the range's arithmetic answers; any other test is tried on each
    ;; element, which a finite range has to have.
    (let ((test (dylan%find-keyword keys :test (lambda () dylan/id?))))
      (cond
        ((not (or (eq test dylan/id?) (eq test dylan/=)))
         (if (collection-size range)
             (bool (iterate-until (lambda (element)
                                    (true? (dylan-call test val element)))
                                  range))
             (dylan-call
              dylan/error
              "(member? <object> <range>) -- test: on an unbounded range"
              test)))
        ((not (or (realp val) (extended-float? val))) dylan%false)
        (t
         (let* ((start (dylan-call dylan/get-range-start range))
                (step (dylan-call dylan/get-range-step range))
                ;; The elements are <extended-float>s if START or STEP is
                ;; one, and only such a VAL is id? to one of them.
                (class-matches (eq (extended-float? val)
                                   (or (extended-float? start)
                                       (extended-float? step))))
                (val (range-number val))
                (start (range-number start))
                (step (range-number step))
                (end (range-number (dylan-call dylan/get-range-end range)))
                (same? (if (eq test dylan/=)
                           #'=
                           (lambda (a b) (and class-matches (eql a b))))))
           (bool
            (cond
              ((zerop step)
               (and (not (eql 0 (dylan-call dylan/get-range-count range)))
                    (funcall same? val start)))
              ;; A range of floats: one of the grid elements by the index
              ;; (VAL - origin) / step rounds to, an element being rounded,
              ;; that is an element of RANGE.  Its elements are floats: id?
              ;; to no rational.
              ((float-range-p range)
               (and (or (not (eq test dylan/id?)) (floatp val))
                    (let* ((grid (range-grid-of range))
                           (q (/ (- val (range-number (range-grid-origin grid)))
                                 (range-number (range-grid-step grid)))))
                      (and (< (abs q) 4d18)
                           (let ((k (round q)))
                             (some (lambda (d)
                                     (let ((i (grid-index range (+ k d))))
                                       (and i (funcall same? val
                                                       (range-number (range-ref range i))))))
                                   '(0 -1 1)))))))
              (t
                (and (funcall (if (minusp step) #'<= #'>=) val start)
                     (or (not end)
                         (funcall (if (minusp step) #'>= #'<=) val end))
                     (let ((k (/ (- (rational val) (rational start))
                                 (rational step))))
                       (and (integerp k)
                            (funcall same? val (+ start (* k step))))))))))))))))

;;;;
;;;; Functions for Sequences (page 104)
;;;;

(add-method
 dylan/add
 (dylan%function->method
  one-range-and-an-object
  ;; A list, the class-for-copy of a range, as for any sequence (chapter
  ;; 12); add! makes one too.
  (lambda (the-range new-element)
    (unless (collection-size the-range)
      (dylan-call dylan/error "add: the range is unbounded"))
    (cons new-element (sequence->list the-range)))))

(add-method
 dylan/remove!
 (dylan%dylan-callable->method
  (make-param-list `((range ,<range>) (value ,<object>))
                   nil nil '(:count :test))
  (lambda (multiple-values next-method range value &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:count :test))
    (let ((test (dylan%find-keyword rest :test (lambda () dylan/id?)))
          (count (dylan%find-keyword rest :count
                                     (lambda () (collection-size range)))))
      (if count
          (dylan-call dylan/remove range value :test test :count count)
          (dylan-call dylan/remove range value :test test))))))

(add-method
 dylan/remove-duplicates!
 (dylan%dylan-callable->method
  (make-param-list `((range ,<range>)) nil nil '(:test))
  (lambda (multiple-values next-method range &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test))
    (let ((test (dylan%find-keyword rest :test (lambda () dylan/id?))))
      (dylan-call dylan/remove-duplicates range :test test)))))

(add-method
 dylan/copy-sequence
 (dylan%dylan-callable->method
  (make-param-list `((range ,<range>)) nil nil '(:start :end))
  (lambda (multiple-values next-method range &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:start :end))
    ;; START and END are indices, END exclusive (chapter 12), as for any
    ;; sequence.  (The Scheme took them as element values, END inclusive.)
    (let* ((start (dylan%find-keyword rest :start (lambda () 0)))
           (end (check-start-end "copy-sequence: bad start: or end: for ~s: ~s ~s"
                                 range start
                                 (dylan%find-keyword rest :end
                                                     (lambda () dylan%false)))))
      (sub-range range start 1 (and end (- end start)))))))

(add-method
 dylan/concatenate
 (dylan%function->method
  (make-param-list `((range ,<range>)) nil 'rest nil)
  ;; A list, the class-for-copy of a range, as for any sequence (chapter
  ;; 12).
  (lambda (range-1 &rest rest)
    (unless (collection-size range-1)
      (dylan-call dylan/error "concatenate: the range is unbounded"))
    (check-bounded "concatenate" rest)
    (mapcan #'sequence->list (cons range-1 rest)))))

(add-method
 dylan/reverse
 (dylan%function->method
  one-range
  (lambda (range)
    (let ((start (dylan-call dylan/get-range-start range))
          (end (dylan-call dylan/get-range-end range))
          (step (dylan-call dylan/get-range-step range)))
      ;; From the last element by -BY, as many as there are
      (cond ((not end)
             (dylan-call dylan/error "reverse: the range is unbounded"))
            ((zerop (collection-size range))
             (dylan-call dylan/range :from start :by step :size 0))
            (t (let ((n (collection-size range)))
                 (sub-range range (- n 1) -1 n))))))))

(add-method
 dylan/reverse!
 (dylan%function->method
  one-range
  (lambda (range)
    (let ((reversed (dylan-call dylan/reverse range)))
      (set-range-start! range (range-start reversed))
      (set-range-step! range (range-step reversed))
      (set-range-end! range (range-end reversed))
      (set-range-count! range (range-count reversed))
      (set-range-grid! range (range-grid reversed))
      range))))

(add-method
 dylan/last
 (dylan%dylan-callable->method
  (make-param-list `((range ,<range>)) nil nil '(:default))
  (lambda (multiple-values next-method range &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:default))
    (let ((end (dylan-call dylan/get-range-end range)))
      (cond ((not end)
             (dylan-call dylan/error "last: the sequence is unbounded"))
            ((zerop (collection-size range))
             (dylan%find-keyword
              rest :default
              (lambda ()
                (dylan-call dylan/error "last: the sequence is empty"))))
            (t end))))))

;;;;
;;;; Operations on Ranges (page 118)
;;;;

;;; Chapter 12: range #key from up-to through by size
(defparameter dylan/range
  (dylan%generic-fn 'range
                    (make-param-list '() nil nil '(:from :up-to :through :by :size))
                    nil))

;;; by: 0 repeats FROM.  Chapter 12's bounds then admit either every
;;; element or none: up-to admits them if FROM is below it, through if FROM
;;; is not above it (through is an inclusive bound, as for any step).  The
;;; range is unbounded unless size is given or a bound admits nothing.

;;; Each bound and the step a real number, and size, if given, a count.
(defun check-range-arguments (rest)
  (loop for (keyword name optional) in '((:from "from:" nil) (:up-to "up-to:" t)
                                         (:through "through:" t) (:by "by:" nil))
        do (let ((value (dylan%find-keyword rest keyword (lambda () 0))))
             (unless (or (and optional (false? value))
                         (true? (dylan-call dylan/instance? value <real>)))
               (dylan-call dylan/error "range: ~a must be a real number: ~s"
                           name value))))
  (let ((size (false->nil (dylan%find-keyword rest :size (lambda () nil)))))
    (when (and size (not (and (integerp size) (>= size 0))))
      (dylan-call dylan/error "range: size: must be a non-negative integer: ~s"
                  size))))

(defun make-zero-step-range (rest)
  (let* ((given-from (dylan%find-keyword rest :from (lambda () 0)))
         (given-by (dylan%find-keyword rest :by (lambda () 0)))
         (from (range-number given-from))
         (up-to (range-number
                 (false->nil (dylan%find-keyword rest :up-to (lambda () nil)))))
         (through (range-number
                   (false->nil (dylan%find-keyword rest :through (lambda () nil)))))
         (size (false->nil (dylan%find-keyword rest :size (lambda () nil))))
         (by (range-number given-by))
         (count (if (or (and up-to (not (< from up-to)))
                        (and through (not (<= from through))))
                    0
                    size))
         (range (dylan%make-<object> <range>)))
    (dylan-call dylan/set-range-start! range given-from)
    (dylan-call dylan/set-range-step! range given-by)
    (dylan-call dylan/set-range-end! range
                (and count (range-result (+ from by) given-from given-by)))
    (dylan-call dylan/set-range-count! range count)
    range))

(add-method
 dylan/range
 (dylan%dylan-callable->method
  (make-param-list '() nil nil '(:by :through :from :up-to :size))
  (lambda (multiple-values next-method &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:by :through :from :up-to :size))
    (check-range-arguments rest)
   (block range-zero-step
    (when (zerop (range-number (dylan%find-keyword rest :by (lambda () 1))))
      (return-from range-zero-step (make-zero-step-range rest)))
    (let* ((given-from (dylan%find-keyword rest :from (lambda () 0)))
           (given-by (dylan%find-keyword rest :by (lambda () 1)))
           (from (range-number given-from))
           (up-to (range-number
                   (false->nil (dylan%find-keyword rest :up-to (lambda () nil)))))
           (through (range-number
                     (false->nil
                      (dylan%find-keyword rest :through (lambda () nil)))))
           (by (range-number given-by))
           (size (false->nil (dylan%find-keyword rest :size (lambda () nil))))
           (by-negative? (minusp by))
           ;; The last element each given bound allows.  Chapter 12: the
           ;; range is the largest satisfying every bound given, so the
           ;; tightest of them applies.  (The Scheme took the loosest.)
           (bounds (append
                    (when up-to
                      (list (+ from (* (ceiling (- (/ (- up-to from) by) 1))
                                       by))))
                    (when through
                      (list (+ from (* (floor (/ (- through from) by)) by))))
                    (when size
                      (list (+ from (* (- size 1) by))))))
           (max-through (and bounds
                             (apply (if by-negative? #'max #'min) bounds))))
      (let ((range (dylan%make-<object> <range>)))
        (dylan-call dylan/set-range-start! range given-from)
        (dylan-call dylan/set-range-step! range given-by)
        (dylan-call dylan/set-range-count! range nil)
        (if (not (or up-to through size))
            (dylan-call dylan/set-range-end! range nil)
            (dylan-call dylan/set-range-end! range
                        (range-result max-through given-from given-by)))
        range))))))

(add-method
 dylan/binary=
 (dylan%function->method
  two-ranges
  (lambda (range-1 range-2)
    (let ((start-1 (range-number (dylan-call dylan/get-range-start range-1)))
          (end-1 (range-number (dylan-call dylan/get-range-end range-1)))
          (step-1 (range-number (dylan-call dylan/get-range-step range-1)))
          (start-2 (range-number (dylan-call dylan/get-range-start range-2)))
          (end-2 (range-number (dylan-call dylan/get-range-end range-2)))
          (step-2 (range-number (dylan-call dylan/get-range-step range-2)))
          (size-1 (collection-size range-1))
          (size-2 (collection-size range-2)))
      ;; Equal when their elements are: a range of one element has no
      ;; step to compare.
      ;; A range on another's grid is compared by its elements if bounded,
      ;; by its grid if not, as in the compiled library.
      (bool
       (cond ((or (and size-1 (not size-2))
                  (and (not size-1) size-2))
              nil)                      ; One is unbound
             ((not (and size-1 size-2)) ; Both unbound
              (let ((grid-1 (range-grid-of range-1))
                    (grid-2 (range-grid-of range-2)))
                (and (= start-1 start-2) (= step-1 step-2)
                     (= (range-number (range-grid-origin grid-1))
                        (range-number (range-grid-origin grid-2)))
                     (= (range-number (range-grid-step grid-1))
                        (range-number (range-grid-step grid-2)))
                     (= (range-grid-base grid-1) (range-grid-base grid-2))
                     (= (range-grid-stride grid-1) (range-grid-stride grid-2)))))
             ((/= size-1 size-2) nil)
             ((or (range-grid range-1) (range-grid range-2))
              (loop for i below size-1
                    always (true? (dylan-call dylan/= (range-ref range-1 i)
                                              (range-ref range-2 i)))))
             ((= 0 size-1) t)           ; Empty ranges are binary=
             ((= 1 size-1) (= start-1 start-2))
             (t (and (= start-1 start-2)
                     (= step-1 step-2)
                     (= end-1 end-2)))))))))

;;
;; INTERSECTION of two ranges is a range (chapter 12), and always
;; terminates.  Each range is a grid -- the numbers congruent to its start
;; modulo its step -- clipped to an interval (unbounded below for a
;; descending range with no end, above for an ascending one).  The
;; intersection is the grid of common points, whose step is the least
;; common multiple of the two steps, clipped to both intervals.
;;

(defun range-interval (range)
  "The range's lowest and highest elements, NIL where unbounded."
  (let ((start (dylan-call dylan/get-range-start range))
        (end (dylan-call dylan/get-range-end range))
        (step (dylan-call dylan/get-range-step range)))
    (if (plusp step) (values start end) (values end start))))

(defun rational-lcm (a b)
  "The least common multiple of the positive rationals A and B."
  (let ((d (lcm (denominator a) (denominator b))))
    (/ (lcm (* a d) (* b d)) d)))

(defun ranges-intersection (range-1 range-2)
  (let* ((start-1 (dylan-call dylan/get-range-start range-1))
         (start-2 (dylan-call dylan/get-range-start range-2))
         (step-1 (dylan-call dylan/get-range-step range-1))
         (d1 (abs step-1))
         (d2 (abs (dylan-call dylan/get-range-step range-2)))
         (period (rational-lcm d1 d2))
         ;; A point common to both grids, found among one period of the
         ;; first grid; there is none if the grids never meet.
         (common (loop for k from 0 below (/ period d1)
                       for x = (+ start-1 (* k d1))
                       when (integerp (/ (- x start-2) d2)) return x)))
    (multiple-value-bind (lo-1 hi-1) (range-interval range-1)
      (multiple-value-bind (lo-2 hi-2) (range-interval range-2)
        (let* ((lo (if (and lo-1 lo-2) (max lo-1 lo-2) (or lo-1 lo-2)))
               (hi (if (and hi-1 hi-2) (min hi-1 hi-2) (or hi-1 hi-2)))
               (first (and common lo
                           (+ common (* period (ceiling (- lo common) period)))))
               (last (and common hi
                          (- common (* period (ceiling (- common hi) period))))))
          (cond ((null common) (empty-range))
                ((and first last)
                 (cond ((> first last) (empty-range))
                       ((plusp step-1)
                        (dylan-call dylan/range :from first :through last
                                    :by period))
                       (t (dylan-call dylan/range :from last :through first
                                      :by (- period)))))
                (first (dylan-call dylan/range :from first :by period))
                (t (dylan-call dylan/range :from last :by (- period)))))))))

(defun float-intersection (range-1 range-2 test)
  (let ((ks (loop for i below (collection-size range-1)
                  when (true? (dylan-call dylan/member? (range-ref range-1 i) range-2
                                          :test test))
                    collect i)))
    (cond ((null ks) (empty-range))
          ((null (cdr ks)) (sub-range range-1 (car ks) 1 1))
          (t (let ((d (- (second ks) (first ks))))
               (loop for (a b) on ks
                     while b
                     unless (= (- b a) d)
                       do (dylan-call dylan/error
                                      "intersection: the elements of ~s in ~s are not in steps"
                                      range-1 range-2))
               (sub-range range-1 (car ks) d (length ks)))))))

(defun range-window (range-1 range-2)
  "The elements of the unbounded RANGE-1 that may be in the bounded
RANGE-2: those between RANGE-2's lowest and highest, with one more at each
end.  Indexes from 4.0e18 on are of no element, as for member?."
  (multiple-value-bind (lo hi) (range-interval range-2)
    (let* ((from (range-number (dylan-call dylan/get-range-start range-1)))
           (by (range-number (dylan-call dylan/get-range-step range-1)))
           (q1 (/ (- (range-number lo) from) by))
           (q2 (/ (- (range-number hi) from) by))
           (qa (min q1 q2))
           (qb (max q1 q2)))
      (if (or (< qb -1) (not (< qa 4.0d18)))
          (empty-range)
          (let ((a (if (< qa 1) 0 (- (floor qa) 1)))
                (b (+ (ceiling (if (< qb 4.0d18) qb 4.0d18)) 1)))
            (sub-range range-1 a 1 (+ (- b a) 1)))))))

(defun general-intersection (next-method range-1 range-2 rest)
  "The sequence method's intersection of the two ranges."
  (if (functionp next-method)
      (apply next-method nil nil range-1 range-2 rest)
      (dylan-call dylan/error
                  "(intersection <range> <range>) -- no general method"
                  range-1 range-2)))

(add-method
 dylan/intersection
 (dylan%dylan-callable->method
  (make-param-list `((range-1 ,<range>) (range-2 ,<range>)) nil nil '(:test))
  (lambda (multiple-values next-method range-1 range-2 &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test))
    (let ((test (dylan%find-keyword rest :test (lambda () nil))))
      (cond ((and test (not (eq test dylan/id?)) (not (eq test dylan/=)))
             ;; Any other test: intersect as general sequences.
             (general-intersection next-method range-1 range-2 rest))
            ((zero-step? range-1)
             ;; Range-1's elements are kept if its value is in range-2.
             (if (true? (dylan-call dylan/member?
                                    (dylan-call dylan/get-range-start range-1)
                                    range-2))
                 (dylan-call dylan/copy-sequence range-1)
                 (empty-range)))
            ((zero-step? range-2)
             (let ((value (dylan-call dylan/get-range-start range-2)))
               (if (and (true? (dylan-call dylan/member? value range-2))
                        (true? (dylan-call dylan/member? value range-1)))
                   (dylan-call dylan/range :from value :size 1)
                   (empty-range))))
            ;; Exact ranges intersect as grids; any float among the starts
            ;; or steps means intersecting the elements (design note 18).
            ((every (lambda (r)
                      (and (rationalp (dylan-call dylan/get-range-step r))
                           (rationalp (dylan-call dylan/get-range-start r))))
                    (list range-1 range-2))
             (ranges-intersection range-1 range-2))
            ((collection-size range-1)
             ;; Floats: the elements of the first in the second, which
             ;; must be at indexes in steps.
             (float-intersection range-1 range-2 (or test dylan/id?)))
            ((collection-size range-2)
             ;; The first unbounded: its elements near the second's.
             (float-intersection (range-window range-1 range-2) range-2
                                 (or test dylan/id?)))
            (t (dylan-call dylan/error
                           "intersection: unbounded ranges of floats: ~s ~s"
                           range-1 range-2)))))))

;;; A range is not mutable: (setter current-element) has no method for it.


