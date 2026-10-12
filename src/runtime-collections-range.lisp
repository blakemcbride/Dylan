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
                  dylan/set-range-count!))

(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <range>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (if (true? (dylan-call dylan/instance? collection <range>))
        collection
        (let ((size (collection-size collection)))
          (cond ((= size 0) (empty-range))
                ((= size 1)
                 (dylan-call dylan/range
                             :from (current-element collection
                                               (initial-state
                                                           collection))
                             :size 1))
                ((minusp size)
                 (dylan-call dylan/error
                             "(as (singleton <range>) <collection>) -- internal error.size retured negative number"
                             size class collection))
                (t
                 (let* ((state (initial-state collection))
                        (first (current-element
                                           collection state))
                        (state-2 (next-state collection state))
                        (second (current-element
                                            collection state-2)))
                   (if (and (true? (dylan-call dylan/subclass? (get-type first) <number>))
                            (true? (dylan-call dylan/subclass? (get-type second)
                                        <number>)))
                       (let ((step (dylan-call dylan/- second first)))
                         (loop with state = (next-state
                                                        collection state-2)
                               with prev = second
                               do (if state
                                      (let ((cur (current-element
                                                  collection state)))
                                        (if (true? (dylan-call dylan/= step
                                                        (dylan-call dylan/-
                                                                    cur prev)))
                                            (progn
                                              (setf prev cur)
                                              (setf state
                                                    (next-state
                                                                collection
                                                                state)))
                                            (return
                                              (dylan-call
                                               dylan/error
                                               "(as (singleton <range>) <collection>) -- numbers not in sync"
                                               class collection))))
                                      (return (dylan-call dylan/range
                                                          :from first :by step
                                                          :size size)))))
                       (dylan-call
                        dylan/error
                        "(as (singleton <range>) <collection>) -- elements are not numbers"
                        class collection))))))))))

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
                       (setf dylan/get-range-start get)))
(create-private-slot <range> <object> "internal-range-step"
                     (lambda (set get)
                       (setf dylan/set-range-step! set)
                       (setf dylan/get-range-step get)))
(create-private-slot <range> <object> "internal-range-end"
                     (lambda (set get)
                       (setf dylan/set-range-end! set)
                       (setf dylan/get-range-end get)))
(create-private-slot <range> <object> "internal-range-count"
                     (lambda (set get)
                       (setf dylan/set-range-count! set)
                       (setf dylan/get-range-count get)))

(defun zero-step? (range)
  (zerop (dylan-call dylan/get-range-step range)))

(defun zero-step-range (value count)
  "A range of COUNT copies of VALUE; unbounded if COUNT is NIL."
  (if count
      (dylan-call dylan/range :from value :by 0 :size count)
      (dylan-call dylan/range :from value :by 0)))

(add-method
 dylan/make
 (dylan%function->method
  (make-param-list `((range ,(dylan%make-singleton <range>))) nil nil t)
  (lambda (range &rest rest)
    (declare (ignore range))
    (dylan-apply dylan/range rest))))

;;;
;;; FUNCTIONS FOR COLLECTIONS (page 99)
;;;
(add-method
 dylan/size
 (one-arg 'range <range>
          (lambda (range)
            (let ((start (dylan-call dylan/get-range-start range))
                  (end (dylan-call dylan/get-range-end range))
                  (step (dylan-call dylan/get-range-step range)))
              ;; END is on the range's grid (see RANGE), so (END - START)/STEP
              ;; is an integer -- up to float rounding, which ROUND undoes.
              (cond
                ((zerop step)
                 (or-false (dylan-call dylan/get-range-count range)))
                (end
                  (if (funcall (if (plusp step) #'< #'>) end start)
                      0
                      (+ (round (- end start) step) 1)))
                (t dylan%false))))))

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
        ((not (realp val)) dylan%false)
        (t
         (let ((start (dylan-call dylan/get-range-start range))
               (end (dylan-call dylan/get-range-end range))
               (step (dylan-call dylan/get-range-step range))
               (same? (if (eq test dylan/=) #'= #'eql)))
           (bool
            (if (zerop step)
                (and (not (eql 0 (dylan-call dylan/get-range-count range)))
                     (funcall same? val start))
                (and (funcall (if (minusp step) #'<= #'>=) val start)
                     (or (not end)
                         (funcall (if (minusp step) #'>= #'<=) val end))
                     (let ((k (/ (- (rational val) (rational start))
                                 (rational step))))
                       (and (integerp k)
                            (funcall same? val (+ start (* k step)))))))))))))))

;;;;
;;;; Functions for Sequences (page 104)
;;;;

(add-method
 dylan/add
 (dylan%function->method
  one-range-and-an-object
  (lambda (the-range new-element)
    (unless (true? (dylan-call dylan/subclass?
                        (dylan-call dylan/object-class new-element) <number>))
      (dylan-call dylan/error
                  "(add <range> <object>) -- new element not a number"
                  the-range new-element))
    (let ((size (collection-size the-range))
          (start (dylan-call dylan/get-range-start the-range))
          (end (dylan-call dylan/get-range-end the-range))
          (step (dylan-call dylan/get-range-step the-range)))
      (cond ((and (zerop step) (or (not size) (> size 1)))
             (if (= new-element start)
                 (zero-step-range start (and size (+ size 1)))
                 (dylan-call dylan/error
                             "(add <range> <object>) -- cannot add this number to range"
                             the-range new-element)))
            ((and (eql size 1) (= new-element start))
             (zero-step-range start 2))
            ((not size)
             (if (= start (+ new-element step))
                 (dylan-call dylan/range :from new-element :by step)
                 (dylan-call dylan/error
                             "(add <range> <object>) -- cannot add this number to unbound range"
                             the-range new-element)))
            ((= size 0)
             (dylan-call dylan/range :from new-element :through new-element
                         :by 1))
            ((= size 1)
             (dylan-call dylan/range :from new-element :through start
                         :by (- start new-element)))
            (t
             (if (= start (+ new-element step))
                 (dylan-call dylan/range :from new-element :through end :by step)
                 (if (= end (- new-element step))
                     (dylan-call dylan/range :from start :through new-element
                                 :by step)
                     (dylan-call dylan/error
                                 "(add <range> <object>) -- cannot add this number to range"
                                 the-range new-element)))))))))

(add-method
 dylan/add!
 (dylan%function->method
  one-range-and-an-object
  (lambda (the-range new-element)
    (unless (true? (dylan-call dylan/subclass?
                        (dylan-call dylan/object-class new-element) <number>))
      (dylan-call dylan/error
                  "(add! <range> <object>) -- new element not a number"
                  the-range new-element))
    (let ((size (collection-size the-range))
          (start (dylan-call dylan/get-range-start the-range))
          (end (dylan-call dylan/get-range-end the-range))
          (step (dylan-call dylan/get-range-step the-range)))
      (cond ((and (zerop step) (or (not size) (> size 1)))
             (if (= new-element start)
                 (when size
                   (dylan-call dylan/set-range-count! the-range (+ size 1)))
                 (dylan-call dylan/error
                             "(add! <range> <object>) -- cannot add this number to range"
                             the-range new-element)))
            ((and (eql size 1) (= new-element start))
             (dylan-call dylan/set-range-end! the-range start)
             (dylan-call dylan/set-range-step! the-range 0)
             (dylan-call dylan/set-range-count! the-range 2))
            ((not size)
             (if (= start (+ new-element step))
                 (dylan-call dylan/set-range-start! the-range new-element)
                 (dylan-call dylan/error
                             "(add <range> <object>) -- cannot add this number to unbound range"
                             the-range new-element)))
            ((= size 0)
             (dylan-call dylan/set-range-start! the-range new-element)
             (dylan-call dylan/set-range-end! the-range new-element)
             (dylan-call dylan/set-range-step! the-range 1)
             (dylan-call dylan/set-range-count! the-range nil))
            ((= size 1)
             (dylan-call dylan/set-range-start! the-range new-element)
             (dylan-call dylan/set-range-end! the-range start)
             (dylan-call dylan/set-range-step! the-range (- start new-element)))
            (t
             (if (= start (+ new-element step))
                 (dylan-call dylan/set-range-start! the-range new-element)
                 (if (= end (- new-element step))
                     (dylan-call dylan/set-range-end! the-range new-element)
                     (dylan-call dylan/error
                                 "(add <range> <object>) -- cannot add this number to range"
                                 the-range new-element)))))
      the-range))))                     ; Return the modified range

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
    (let* ((size (collection-size range))
           (start (dylan%find-keyword rest :start (lambda () 0)))
           (end (dylan%find-keyword rest :end (lambda () size)))
           (step (dylan-call dylan/get-range-step range))
           (from (+ (dylan-call dylan/get-range-start range) (* start step))))
      (when (or (not (integerp start)) (minusp start)
                (and size (> start size))
                (and end (or (not (integerp end)) (< end start)
                             (and size (> end size)))))
        (dylan-call
         dylan/error
         "(copy-sequence <range>) -- bad start: or end: index"
         range start end))
      (if end
          (dylan-call dylan/range :from from :size (- end start) :by step)
          (dylan-call dylan/range :from from :by step))))))

(add-method
 dylan/concatenate
 (dylan%function->method
  (make-param-list `((range ,<range>)) nil 'rest nil)
  (lambda (range-1 &rest rest)
    (let ((end-1 (dylan-call dylan/get-range-end range-1))
          (step-1 (dylan-call dylan/get-range-step range-1))
          (result (dylan-call dylan/copy-sequence range-1)))
      (unless end-1
        (dylan-call dylan/error
                    "(concatenate <range> !rest) -- range is unbound"
                    range-1 rest))
      (loop with rest-ranges = (mapcar (lambda (collection)
                                         (dylan-call dylan/as <range>
                                                     collection))
                                       rest)
            do (if (null rest-ranges)
                   (return result)
                   (let* ((next-range (car rest-ranges))
                          (start-2 (dylan-call dylan/get-range-start next-range))
                          (end-2 (dylan-call dylan/get-range-end next-range))
                          (step-2 (dylan-call dylan/get-range-step next-range))
                          (size (collection-size next-range)))
                     (unless end-2
                       (dylan-call
                        dylan/error
                        "(concatenate <range> !rest) -- range is unbound"
                        next-range range-1 rest))
                     (if (plusp size)
                         (if (and (zerop step-1) (zerop step-2)
                                  (= start-2 end-1))
                             ;; Runs of the same value join up.
                             (progn
                               (dylan-call dylan/set-range-count!
                                           result
                                           (+ (collection-size result)
                                              size))
                               (pop rest-ranges))
                         (if (and (not (zerop step-1))
                                  (= start-2 (+ end-1 step-1))
                                  (or (= step-1 step-2) (= size 1)))
                             (progn
                               (dylan-call dylan/set-range-end! result end-2)
                               (setf end-1 end-2)
                               (pop rest-ranges))
                             (dylan-call
                              dylan/error
                              "(concatenate <range> !rest) -- incompatible ranges"
                              next-range result range-1 rest)))
                         (pop rest-ranges)))))))))

(add-method
 dylan/reverse
 (dylan%function->method
  one-range
  (lambda (range)
    (let ((start (dylan-call dylan/get-range-start range))
          (end (dylan-call dylan/get-range-end range))
          (step (dylan-call dylan/get-range-step range)))
      (cond ((not end)
             (dylan-call dylan/error
                         "(reverse <range>) -- can't reverse unbounded range"
                         range))
            ((zerop step)
             (zero-step-range start (collection-size range)))
            (t (dylan-call dylan/range :from end :through start
                           :by (- step))))))))

(add-method
 dylan/reverse!
 (dylan%function->method
  one-range
  (lambda (range)
    (let ((start (dylan-call dylan/get-range-start range))
          (end (dylan-call dylan/get-range-end range))
          (step (dylan-call dylan/get-range-step range)))
      (if end
          (progn
            (dylan-call dylan/set-range-start! range end)
            (dylan-call dylan/set-range-end! range start)
            (dylan-call dylan/set-range-step! range (- step))
            range)
          (dylan-call dylan/error
                      "(reverse! <range>) -- can't reverse unbounded range"
                      range))))))

(add-method
 dylan/last
 (dylan%function->method
  one-range
  (lambda (range)
    (let ((end (dylan-call dylan/get-range-end range)))
      (cond ((not end)
             (dylan-call dylan/error
                         "(last <range>) -- no end in sight for this range"
                         range))
            ((zerop (collection-size range))
             (dylan-call dylan/error "(last <range>) -- range is empty" range))
            (t end))))))

;;;;
;;;; Operations on Ranges (page 118)
;;;;

(defparameter dylan/range (dylan%generic-fn 'range only-rest-args nil))

;;; by: 0 repeats FROM.  Chapter 12's bounds then admit either every
;;; element or none: up-to admits them if FROM is below it, through if FROM
;;; is not above it (through is an inclusive bound, as for any step).  The
;;; range is unbounded unless size is given or a bound admits nothing.

(defun make-zero-step-range (rest)
  (let* ((from (dylan%find-keyword rest :from (lambda () 0)))
         (up-to (false->nil (dylan%find-keyword rest :up-to (lambda () nil))))
         (through (false->nil (dylan%find-keyword rest :through (lambda () nil))))
         (size (false->nil (dylan%find-keyword rest :size (lambda () nil))))
         (count (if (or (and up-to (not (< from up-to)))
                        (and through (not (<= from through))))
                    0
                    size))
         (range (dylan%make-<object> <range>)))
    (dylan-call dylan/set-range-start! range from)
    (dylan-call dylan/set-range-step! range 0)
    (dylan-call dylan/set-range-end! range (and count from))
    (dylan-call dylan/set-range-count! range count)
    range))

(add-method
 dylan/range
 (dylan%dylan-callable->method
  (make-param-list '() nil nil '(:by :through :from :up-to :size))
  (lambda (multiple-values next-method &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:by :through :from :up-to :size))
   (block range-zero-step
    (when (zerop (dylan%find-keyword rest :by (lambda () 1)))
      (return-from range-zero-step (make-zero-step-range rest)))
    (let* ((from (dylan%find-keyword rest :from (lambda () 0)))
           (up-to (false->nil (dylan%find-keyword rest :up-to (lambda () nil))))
           (through (false->nil
                     (dylan%find-keyword rest :through (lambda () nil))))
           (by (dylan%find-keyword rest :by (lambda () 1)))
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
        (dylan-call dylan/set-range-start! range from)
        (dylan-call dylan/set-range-step! range by)
        (dylan-call dylan/set-range-count! range nil)
        (if (not (or up-to through size))
            (dylan-call dylan/set-range-end! range nil)
            (dylan-call dylan/set-range-end! range max-through))
        range))))))

(add-method
 dylan/binary=
 (dylan%function->method
  two-ranges
  (lambda (range-1 range-2)
    (let ((start-1 (dylan-call dylan/get-range-start range-1))
          (end-1 (dylan-call dylan/get-range-end range-1))
          (step-1 (dylan-call dylan/get-range-step range-1))
          (start-2 (dylan-call dylan/get-range-start range-2))
          (end-2 (dylan-call dylan/get-range-end range-2))
          (step-2 (dylan-call dylan/get-range-step range-2))
          (size-1 (collection-size range-1))
          (size-2 (collection-size range-2)))
      ;; Equal when their elements are: a range of one element has no
      ;; step to compare.
      (bool
       (cond ((or (and size-1 (not size-2))
                  (and (not size-1) size-2))
              nil)                      ; One is unbound
             ((not (and size-1 size-2)) ; Both unbound
              (and (= start-1 start-2) (= step-1 step-2)))
             ((/= size-1 size-2) nil)
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
            ((every (lambda (r)
                      (rationalp (dylan-call dylan/get-range-step r)))
                    (list range-1 range-2))
             (ranges-intersection range-1 range-2))
            ((and (collection-size range-1)
                  (collection-size range-2))
             ;; Float steps: intersect the elements.
             (dylan-call dylan/as <range>
                         (general-intersection next-method range-1 range-2
                                               rest)))
            (t (dylan-call dylan/error
                           "(intersection <range> <range>) -- unbounded ranges with float steps"
                           range-1 range-2)))))))

;;;
;;; Mutable Collection
;;;

(add-method
 dylan/setter/current-element/
 (dylan%function->method
  (make-param-list `((range ,<range>) (state ,<object>) (new-value ,<object>))
                   nil nil nil)
  (lambda (range state new-value)
    (dylan-call dylan/error
                "((setter current-element) <range>) -- range not mutable"
                range state new-value))))
