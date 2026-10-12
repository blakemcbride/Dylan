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
;;;; This file is a continuation of runtime-collections-generic1, which had
;;;; to be split because of a limitation in the Gambit compiler.
;;;;                      (was Thomas's src/runtime-collections-generic2.scm)

(in-package #:dylan)

;;;;
;;;; FUNCTIONS FOR SEQUENCES (page 104)
;;;;
;;;; Chapter 12: the run-time system provides default implementations of
;;;; all of these, which work on any sequence through its elements and
;;;; answer an instance of the class-for-copy of the (first) sequence.

(defun sequence->list (sequence)
  (iterate->list #'identity sequence))

(defun list->sequence-like (sequence list)
  "LIST as an instance of SEQUENCE's class-for-copy."
  (dylan-call dylan/as (dylan-call dylan/class-for-copy sequence) list))

(defparameter dylan/add
  (dylan%generic-fn
   'add one-sequence-and-an-object
   (lambda (sequence new-element)
     (list->sequence-like sequence
                          (cons new-element (sequence->list sequence))))))

(defparameter dylan/add!
  (dylan%generic-fn 'add! one-sequence-and-an-object
                    (lambda (seq obj)
                      (dylan-call dylan/add seq obj)))) ; Defaults to ADD

(defparameter dylan/add-new
  (dylan%generic-fn
   'add-new
   (make-param-list `((sequence ,<sequence>) (object ,<object>))
                    nil nil '(:test))
   nil))

(add-method
 dylan/add-new
 (dylan%dylan-callable->method
  (make-param-list `((sequence ,<sequence>) (object ,<object>))
                   nil nil '(:test))
  (lambda (multiple-values next-method seq object &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test))
    (let ((test-fn (dylan%find-keyword rest :test (lambda () dylan/id?))))
      (if (iterate-until (lambda (x) (true? (dylan-call test-fn x object))) seq)
          seq
          (dylan-call dylan/add seq object))))))

(defparameter dylan/add-new!
  (dylan%generic-fn
   'add-new!
   (make-param-list `((sequence ,<sequence>) (object ,<object>))
                    nil nil '(:test))
   nil))

(add-method
 dylan/add-new!
 (dylan%dylan-callable->method
  (make-param-list `((sequence ,<sequence>) (object ,<object>))
                   nil nil '(:test))
  (lambda (multiple-values next-method seq object &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test))
    (let ((test-fn (dylan%find-keyword rest :test (lambda () dylan/id?))))
      (if (iterate-until (lambda (x) (true? (dylan-call test-fn x object))) seq)
          seq
          (dylan-call dylan/add! seq object))))))

(defparameter dylan/remove
  (dylan%generic-fn
   'remove
   (make-param-list `((sequence ,<sequence>) (value ,<object>))
                    nil nil '(:test :count))
   nil))

(add-method
 dylan/remove
 (dylan%dylan-callable->method
  (make-param-list `((sequence ,<sequence>) (value ,<object>))
                   nil nil '(:test :count))
  (lambda (multiple-values next-method seq value &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test :count))
    (let ((test? (dylan%find-keyword rest :test (lambda () dylan/id?)))
          (count (dylan%find-keyword rest :count (lambda () -1))))
      (loop with state = (initial-state seq)
            with result = (dylan-call dylan/make
                                      (dylan-call dylan/class-for-copy seq))
            with changed = 0
            do (if state
                   (let ((cur-element (current-element
                                                  seq state)))
                     (if (and (or (minusp count) (< changed count))
                              (true? (dylan-call test? cur-element value)))
                         (progn
                           (setf state (next-state seq state))
                           (incf changed))
                         (progn
                           (setf state (next-state seq state))
                           (setf result (dylan-call dylan/add result
                                                    cur-element)))))
                   (return (dylan-call dylan/reverse result))))))))

(defparameter dylan/remove!
  (dylan%generic-fn
   'remove!
   (make-param-list `((sequence ,<sequence>) (value ,<object>))
                    nil nil '(:test :count))
   nil))

(add-method
 dylan/remove!
 (dylan%dylan-callable->method
  (make-param-list `((sequence ,<sequence>) (value ,<object>))
                   nil nil '(:test :count))
  (lambda (multiple-values next-method seq value &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test :count))
    (let ((test? (dylan%find-keyword rest :test (lambda () dylan/id?)))
          (count (dylan%find-keyword rest :count
                                     (lambda () (collection-size seq)))))
      (dylan-call dylan/remove seq value :test test? :count count)))))

(defparameter dylan/choose
  (dylan%generic-fn
   'choose
   (make-param-list `((predicate ,<function>) (sequence ,<sequence>))
                    nil nil nil)
   (lambda (test? seq)
     (loop with state = (initial-state seq)
           with result = (dylan-call dylan/make
                                     (dylan-call dylan/class-for-copy seq))
           do (if state
                  (let ((cur-element (current-element
                                                 seq state)))
                    (setf state (next-state seq state))
                    (when (true? (dylan-call test? cur-element))
                      (setf result (dylan-call dylan/add result cur-element))))
                  (return (dylan-call dylan/reverse result)))))))

(defparameter dylan/choose-by
  (dylan%generic-fn
   'choose-by
   (make-param-list `((predicate ,<function>) (test-sequence ,<sequence>)
                      (value-sequence ,<sequence>))
                    nil nil nil)
   (lambda (test? test-sequence value-sequence)
     (loop with test-state = (initial-state test-sequence)
           with value-state = (initial-state value-sequence)
           with result = (dylan-call dylan/make
                                     (dylan-call dylan/class-for-copy
                                                 value-sequence))
           do (if (and test-state value-state)
                  (let ((test-element (current-element
                                                  test-sequence test-state))
                        (value-element (current-element
                                                   value-sequence value-state)))
                    (setf test-state (next-state
                                                 test-sequence test-state))
                    (setf value-state (next-state
                                                  value-sequence value-state))
                    (when (true? (dylan-call test? test-element))
                      (setf result (dylan-call dylan/add result value-element))))
                  (return (dylan-call dylan/reverse result)))))))

(defparameter dylan/intersection
  ;; Does intersection result in a set whose elements are unique?
  ;; This implementation may result in a multi-set...
  (dylan%generic-fn
   'intersection
   (make-param-list `((sequence-1 ,<sequence>) (sequence-2 ,<sequence>))
                    nil nil '(:test))
   nil))

(add-method
 dylan/intersection
 (dylan%dylan-callable->method
  (make-param-list `((sequence-1 ,<sequence>) (sequence-2 ,<sequence>))
                   nil nil '(:test))
  (lambda (multiple-values next-method seq-1 seq-2 &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test))
    (let ((test? (dylan%find-keyword rest :test (lambda () dylan/id?))))
      (loop with state = (initial-state seq-1)
            with result = (dylan-call dylan/make
                                      (dylan-call dylan/class-for-copy seq-1))
            do (if state
                   (let ((a (current-element seq-1 state)))
                     (setf state (next-state seq-1 state))
                     ;; MEMBER?, so that a range answers by arithmetic,
                     ;; even an unbounded one.
                     (when (true? (dylan-call dylan/member? a seq-2
                                              :test test?))
                       (setf result (dylan-call dylan/add result a))))
                   (return (dylan-call dylan/reverse result))))))))

(defparameter dylan/union
  (dylan%generic-fn
   'union
   (make-param-list `((sequence-1 ,<sequence>) (sequence-2 ,<sequence>))
                    nil nil '(:test))
   nil))

(add-method
 dylan/union
 (dylan%dylan-callable->method
  (make-param-list `((sequence-1 ,<sequence>) (sequence-2 ,<sequence>))
                   nil nil '(:test))
  (lambda (multiple-values next-method seq-1 seq-2 &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test))
    ;; Chapter 12: the test is always called with an element of SEQUENCE-1
    ;; first and one of SEQUENCE-2 second.  The result is SEQUENCE-1's
    ;; elements and those of SEQUENCE-2 that match none of them; duplicates
    ;; within one argument may remain (design note 14).
    (let* ((test? (dylan%find-keyword rest :test (lambda () dylan/id?)))
           (elements-1 (iterate->list #'identity seq-1))
           (extra (remove-if (lambda (b)
                               (some (lambda (a)
                                       (true? (dylan-call test? a b)))
                                     elements-1))
                             (iterate->list #'identity seq-2))))
      (dylan-call dylan/as (dylan-call dylan/class-for-copy seq-1)
                  (append elements-1 extra))))))

(defparameter dylan/remove-duplicates
  (dylan%generic-fn
   'remove-duplicates
   (make-param-list `((sequence ,<sequence>)) nil nil '(:test))
   nil))

(add-method
 dylan/remove-duplicates
 (dylan%dylan-callable->method
  (make-param-list `((sequence ,<sequence>)) nil nil '(:test))
  (lambda (multiple-values next-method seq &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test))
    ;; Each element is compared with those already kept (the Scheme
    ;; compared it with the first elements of the input instead), the
    ;; earlier element first, as chapter 12 requires.
    (let ((test? (dylan%find-keyword rest :test (lambda () dylan/id?)))
          (kept '()))
      (iterate-until
       (lambda (element)
         (unless (some (lambda (k) (true? (dylan-call test? k element))) kept)
           (push element kept))
         nil)
       seq)
      (dylan-call dylan/as (dylan-call dylan/class-for-copy seq)
                  (reverse kept))))))

(defparameter dylan/remove-duplicates!
  (dylan%generic-fn
   'remove-duplicates!
   (make-param-list `((sequence ,<sequence>)) nil nil '(:test))
   nil))

(add-method
 dylan/remove-duplicates!
 (dylan%dylan-callable->method
  (make-param-list `((sequence ,<sequence>)) nil nil '(:test))
  (lambda (multiple-values next-method seq &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test))
    (let ((test? (dylan%find-keyword rest :test (lambda () dylan/id?))))
      (dylan-call dylan/remove-duplicates seq :test test?)))))

(defparameter dylan/copy-sequence
  (dylan%generic-fn
   'copy-sequence
   (make-param-list `((source ,<sequence>)) nil nil '(:start :end))
   nil))

(add-method
 dylan/copy-sequence
 (dylan%dylan-callable->method
  (make-param-list `((source ,<sequence>)) nil nil '(:start :end))
  (lambda (multiple-values next-method source &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:start :end))
    (let ((start (dylan%find-keyword rest :start (lambda () 0)))
          (end (dylan%find-keyword
                rest :end (lambda () (collection-size source)))))
      (loop with state = (initial-state source)
            with result = (dylan-call dylan/make
                                      (dylan-call dylan/class-for-copy source))
            with index = 0
            do (if (and state (<= index (- end 1)))
                   (let ((element (current-element
                                              source state)))
                     (setf state (next-state source state))
                     (when (>= index start)
                       (setf result (dylan-call dylan/add result element)))
                     (incf index))
                   (return (dylan-call dylan/reverse result))))))))

(defparameter dylan/concatenate-as
  (dylan%generic-fn
   'concatenate-as
   (make-param-list `((class ,<class>) (sequence ,<sequence>)) nil 'rest nil)
   (lambda (class seq-1 &rest rest)
     (unless (subclass? class <mutable-sequence>)
       (dylan-call dylan/error
                   "concatenate-as -- target class not a mutable sequence"
                   class seq-1 rest))
     (dylan-call dylan/as class
                 (mapcan #'sequence->list (cons seq-1 rest))))))

(defparameter dylan/concatenate
  (dylan%generic-fn
   'concatenate at-least-one-sequence
   (lambda (seq-1 &rest rest)
     (list->sequence-like seq-1
                          (mapcan #'sequence->list (cons seq-1 rest))))))

(defparameter dylan/replace-subsequence!
  (dylan%generic-fn
   'replace-subsequence!
   (make-param-list `((mutable-sequence ,<mutable-sequence>)
                      (insert-sequence ,<sequence>))
                    nil nil '(:start))
   nil))

(add-method
 dylan/replace-subsequence!
 (dylan%dylan-callable->method
  (make-param-list `((mutable-sequence ,<mutable-sequence>)
                     (insert-sequence ,<sequence>))
                   nil nil '(:start))
  (lambda (multiple-values next-method mutable insert &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:start))
    (let ((start (dylan%find-keyword rest :start (lambda () 0)))
          (m-state (initial-state mutable)))
      (when (< (- (collection-size mutable) start)
               (collection-size insert))
        (dylan-call dylan/error
                    "replace-subsequence! -- not enough elements in target"
                    mutable insert start))
      (when (minusp start)
        (dylan-call dylan/error
                    "replace-subsequence! -- index cannot be negative"
                    mutable insert start))
      (do ((count 0 (+ count 1)))
          ((= count start) 'done)
        (setf m-state (next-state mutable m-state)))
      (loop with i-state = (initial-state insert)
            do (if i-state
                   (progn
                     (set-current-element
                                 mutable m-state
                                 (current-element
                                             insert i-state))
                     (setf i-state (next-state insert i-state))
                     (setf m-state (next-state
                                               mutable m-state)))
                   (return mutable)))))))

(defparameter dylan/reverse
  (dylan%generic-fn
   'reverse one-sequence
   (lambda (seq-1)
     (list->sequence-like seq-1 (reverse (sequence->list seq-1))))))

(defparameter dylan/reverse!
  (dylan%generic-fn 'reverse! one-sequence
                    (lambda (seq-1) (dylan-call dylan/reverse seq-1))))

(defparameter dylan/sort
  (dylan%generic-fn
   'sort
   (make-param-list `((sequence ,<sequence>)) nil nil '(:test :stable))
   nil))

(add-method
 dylan/sort
 (dylan%dylan-callable->method
  (make-param-list `((sequence ,<sequence>)) nil nil '(:test :stable))
  (lambda (multiple-values next-method seq &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test :stable))
    (let ((test? (dylan%find-keyword rest :test (lambda () dylan/<))))
      (dylan%find-keyword rest :stable (lambda () dylan%false)) ; Ignored
      (dylan-call dylan/as
                  (dylan-call dylan/class-for-copy seq)
                  (scheme-sort (dylan-call dylan/as <list> seq)
                               (lambda (x y) (true? (dylan-call test? x y)))))))))

(defparameter dylan/sort!
  (dylan%generic-fn
   'sort!
   (make-param-list `((sequence ,<sequence>)) nil nil '(:test :stable))
   nil))

(add-method
 dylan/sort!
 (dylan%dylan-callable->method
  (make-param-list `((sequence ,<sequence>)) nil nil '(:test :stable))
  (lambda (multiple-values next-method seq &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test :stable))
    (let ((test? (dylan%find-keyword rest :test (lambda () dylan/<)))
          (stable (dylan%find-keyword rest :stable (lambda () dylan%false))))
      (dylan-call dylan/sort seq :test test? :stable stable)))))

(defparameter dylan/first
  (dylan%generic-fn
   'first one-sequence
   (lambda (sequence-1)
     (let ((state (initial-state sequence-1)))
       (if state
           (current-element sequence-1 state)
           (dylan-call dylan/error "first -- no element in sequence"
                       sequence-1))))))

(defparameter dylan/second
  (dylan%generic-fn
   'second one-sequence
   (lambda (sequence-1)
     (let ((state (dylan-call dylan/get-state sequence-1 1)))
       (if state
           (current-element sequence-1 state)
           (dylan-call dylan/error "second -- sequence size < 2" sequence-1))))))

(defparameter dylan/third
  (dylan%generic-fn
   'third one-sequence
   (lambda (sequence-1)
     (let ((state (dylan-call dylan/get-state sequence-1 2)))
       (if state
           (current-element sequence-1 state)
           (dylan-call dylan/error "third -- sequence size < 3" sequence-1))))))

(defparameter dylan/setter/first/
  (dylan%generic-fn
   'setter/first/ one-mutable-sequence-and-an-object
   (lambda (sequence-1 new-value)
     (let ((state (initial-state sequence-1)))
       (if state
           (progn
             (set-current-element
                         sequence-1 state new-value)
             new-value)
           (dylan-call dylan/error "(setter first) -- sequence is empty"
                       sequence-1 new-value))))))

(defparameter dylan/setter/second/
  (dylan%generic-fn
   'setter/second/ one-mutable-sequence-and-an-object
   (lambda (sequence-1 new-value)
     (let ((size (collection-size sequence-1)))
       (if (or (not size) (>= size 2))
           (progn
             (set-current-element
                         sequence-1
                         (dylan-call dylan/get-state sequence-1 1)
                         new-value)
             new-value)
           (dylan-call dylan/error "(setter second) -- sequence size < 2"
                       sequence-1 new-value))))))

(defparameter dylan/setter/third/
  (dylan%generic-fn
   'setter/third/ one-mutable-sequence-and-an-object
   (lambda (sequence-1 new-value)
     (let ((size (collection-size sequence-1)))
       (if (or (not size) (>= size 3))
           (progn
             (set-current-element
                         sequence-1
                         (dylan-call dylan/get-state sequence-1 2)
                         new-value)
             new-value)
           (dylan-call dylan/error "(setter third) -- sequence size < 3"
                       sequence-1 new-value))))))

(defparameter dylan/last
  (dylan%generic-fn
   'last one-sequence
   (lambda (sequence-1)
     (let ((prev-state nil))
       (do ((state (initial-state sequence-1)
                   (next-state sequence-1 state)))
           ((not state)
            (if prev-state
                (current-element sequence-1 prev-state)
                (dylan-call dylan/error "last -- sequence is empty"
                            sequence-1)))
         (setf prev-state state))))))

(defun check-subsequence (test? big big-state pattern pattern-state)
  (labels ((check-loop (big-state pattern-state)
             (if (not pattern-state)
                 t
                 (if (and big-state
                          (true? (dylan-call test?
                                             (current-element
                                                         big big-state)
                                             (current-element
                                                         pattern pattern-state))))
                     (check-loop (next-state big big-state)
                                 (next-state
                                             pattern pattern-state))
                     nil))))
    (check-loop (copy-state big big-state)
                (copy-state pattern pattern-state))))

(defparameter dylan/subsequence-position
  (dylan%generic-fn
   'subsequence-position
   (make-param-list `((big ,<sequence>) (pattern ,<sequence>))
                    nil nil '(:test :count))
   nil))

(add-method
 dylan/subsequence-position
 (dylan%dylan-callable->method
  (make-param-list `((big ,<sequence>) (pattern ,<sequence>))
                   nil nil '(:test :count))
  (lambda (multiple-values next-method big pattern &rest rest)
    (declare (ignore multiple-values))
    (block subsequence-position
      (dylan%keyword-validate next-method rest '(:test :count))
      (let ((test? (dylan%find-keyword rest :test (lambda () dylan/id?)))
            (count (dylan%find-keyword rest :count (lambda () 1)))
            (init-state-pattern (initial-state pattern)))
        (when (null init-state-pattern)
          ;; An empty pattern occurs at every index, up to the size of BIG.
          (return-from subsequence-position
            (if (<= count (+ 1 (collection-size big)))
                (- count 1)
                dylan%false)))
        (loop with first-of-pattern = (dylan-call dylan/first pattern)
              with state = (initial-state big)
              with num-found = 0
              with index = 0
              do (if state
                     (if (and (true? (dylan-call test?
                                                 (current-element
                                                             big state)
                                                 first-of-pattern))
                              (check-subsequence test? big state
                                                 pattern init-state-pattern))
                         (if (>= num-found (- count 1))
                             (return index)
                             (progn
                               (setf state (next-state big state))
                               (incf num-found)
                               (incf index)))
                         (progn
                           (setf state (next-state big state))
                           (incf index)))
                     (return dylan%false))))))))   ; not found

;;;;
;;;; MUTABLE COLLECTIONS (p. 127)
;;;;

(defparameter dylan/setter/current-element/
  (dylan%generic-fn
   'setter/current-element/
   (make-param-list `((mutable-collection ,<mutable-collection>)
                      (state ,<object>) (new-value ,<object>))
                    nil nil nil)
   (lambda (mutable-collection state new-value)
     (dylan-call dylan/error
                 "(setter current-element) -- cannot set! this collection type"
                 mutable-collection state new-value))))

(defparameter dylan/setter/element/
  (dylan%generic-fn
   'setter/element/
   (make-param-list `((mutable-collection ,<mutable-collection>)
                      (key ,<object>) (new-val ,<object>))
                    nil nil nil)
   (lambda (collection key new-value)
     (dylan-call dylan/error
                 "(setter element) -- not defined for this collection type"
                 collection key new-value))))

(add-method
 dylan/setter/element/
 (dylan%function->method
  (make-param-list `((mutable-sequence ,<mutable-sequence>)
                     (key ,<integer>) (new-value ,<object>))
                   nil nil nil)
  (lambda (mut-seq key new-value)
    (do ((state (initial-state mut-seq)
                (next-state mut-seq state))
         (k 0 (+ k 1)))
        ((or (not state) (= k key))
         (if state
             (progn
               (set-current-element mut-seq state new-value)
               new-value)
             (dylan-call dylan/error "(setter element) -- key not found"
                         mut-seq key new-value)))))))

(add-method
 dylan/setter/element/
 (dylan%function->method
  (make-param-list
   `((mutable-explicit-key-collection ,<mutable-explicit-key-collection>)
     (key ,<object>) (new-value ,<object>))
   nil nil nil)
  (lambda (mut-seq key new-value)
    (do ((state (initial-state mut-seq)
                (next-state mut-seq state)))
        ((or (not state)
             (true? (dylan-call dylan/=
                                (current-key mut-seq state)
                                key)))
         (if state
             (progn
               (set-current-element mut-seq state new-value)
               new-value)
             (dylan-call dylan/error "(setter element) -- key not found"
                         mut-seq key new-value)))))))
