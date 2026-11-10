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

(defun copy-like (sequence elements)
  "A new instance of SEQUENCE's class-for-copy holding the list ELEMENTS."
  (sequence-of-class (dylan-call dylan/class-for-copy sequence) elements))

(defun sequence-of-class (class elements)
  "A new instance of the sequence CLASS holding the list ELEMENTS, in order:
made with its size and filled through the iteration protocol, as the
compiled library's %from-list does."
  (if (subclass? class <list>)
      (dylan-call dylan/as class elements)
      (let ((result (dylan-call dylan/make (string-class-for class elements)
                                :size (length elements))))
        (loop for state = (initial-state result) then (next-state result state)
              for rest on elements
              while state
              do (set-current-element result state (car rest)))
        result)))

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
          ;; No count (or #f) removes every match; a count below 1, none.
          (count (false->nil (dylan%find-keyword rest :count (lambda () nil)))))
      (loop with state = (initial-state seq)
            with kept = '()
            with changed = 0
            do (if state
                   (let ((cur-element (current-element
                                                  seq state)))
                     (if (and (or (null count) (< changed count))
                              (true? (dylan-call test? cur-element value)))
                         (progn
                           (setf state (next-state seq state))
                           (incf changed))
                         (progn
                           (setf state (next-state seq state))
                           (push cur-element kept))))
                   (return (copy-like seq (nreverse kept)))))))))

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
          (count (dylan%find-keyword rest :count (lambda () dylan%false))))
      (dylan-call dylan/remove seq value :test test? :count count)))))

(defparameter dylan/choose
  (dylan%generic-fn
   'choose
   (make-param-list `((predicate ,<function>) (sequence ,<sequence>))
                    nil nil nil)
   (lambda (test? seq)
     (loop with state = (initial-state seq)
           with kept = '()
           do (if state
                  (let ((cur-element (current-element
                                                 seq state)))
                    (setf state (next-state seq state))
                    (when (true? (dylan-call test? cur-element))
                      (push cur-element kept)))
                  (return (copy-like seq (nreverse kept))))))))

(defparameter dylan/choose-by
  (dylan%generic-fn
   'choose-by
   (make-param-list `((predicate ,<function>) (test-sequence ,<sequence>)
                      (value-sequence ,<sequence>))
                    nil nil nil)
   (lambda (test? test-sequence value-sequence)
     (loop with test-state = (initial-state test-sequence)
           with value-state = (initial-state value-sequence)
           with kept = '()
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
                      (push value-element kept)))
                  (return (copy-like value-sequence (nreverse kept))))))))

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
            with kept = '()
            do (if state
                   (let ((a (current-element seq-1 state)))
                     (setf state (next-state seq-1 state))
                     ;; MEMBER?, so that a range answers by arithmetic,
                     ;; even an unbounded one.
                     (when (true? (dylan-call dylan/member? a seq-2
                                              :test test?))
                       (push a kept)))
                   (return (copy-like seq-1 (nreverse kept)))))))))

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
    (let* ((start (dylan%find-keyword rest :start (lambda () 0)))
           (end (check-start-end "copy-sequence: bad start: or end: for ~s: ~s ~s"
                                 source start
                                 (dylan%find-keyword rest :end
                                                     (lambda () dylan%false)))))
      (loop with state = (initial-state source)
            with kept = '()
            with index = 0
            do (if (and state (<= index (- end 1)))
                   (let ((element (current-element
                                              source state)))
                     (setf state (next-state source state))
                     (when (>= index start)
                       (push element kept))
                     (incf index))
                   (return (copy-like source (nreverse kept)))))))))

(defparameter dylan/concatenate-as
  (dylan%generic-fn
   'concatenate-as
   (make-param-list `((class ,<class>) (sequence ,<sequence>)) nil 'rest nil)
   (lambda (class seq-1 &rest rest)
     (unless (subclass? class <mutable-sequence>)
       (dylan-call dylan/error
                   "concatenate-as: the class is not a mutable sequence: ~s"
                   class))
     (dylan-call dylan/as class
                 (mapcan #'sequence->list (cons seq-1 rest))))))

(defparameter dylan/concatenate
  (dylan%generic-fn
   'concatenate at-least-one-sequence
   (lambda (seq-1 &rest rest)
     (list->sequence-like seq-1
                          (mapcan #'sequence->list (cons seq-1 rest))))))

;;; Chapter 12: the elements of INSERT-SEQUENCE overwrite those of the
;;; sequence from START (default 0) on, in place; an error is signaled if
;;; the sequence has not enough elements from START to hold them all.

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
    (let* ((size (collection-size mutable))
           (inserted (sequence->list insert))
           (start (let ((start (dylan%find-keyword rest :start (lambda () 0))))
                    ;; A float = to an index is that index, as = finds it
                    (or (index-key start) start))))
      (when (and inserted
                 (not (and (integerp start) size (<= 0 start)
                           (<= (+ start (length inserted)) size))))
        (dylan-call dylan/error
                    "replace-subsequence!: the sequence is too short"))
      (when inserted
        (let ((state (dylan-call dylan/get-state mutable start)))
          (dolist (element inserted)
            (set-current-element mutable state element)
            (setf state (next-state mutable state)))))
      mutable))))

(defparameter dylan/reverse
  (dylan%generic-fn
   'reverse one-sequence
   (lambda (seq-1)
     (list->sequence-like seq-1 (reverse (sequence->list seq-1))))))

;;; Sequences that can reuse storage for reverse! and sort! should
;;; (chapter 12): a mutable sequence gets its elements back in the new
;;; order, and is the result, as in a compiled program.
(defun mutable-sequence-p (sequence)
  (true? (dylan-call dylan/instance? sequence <mutable-sequence>)))

(defun replace-elements! (sequence elements)
  (loop for state = (initial-state sequence) then (next-state sequence state)
        while (and state elements)
        do (set-current-element sequence state (pop elements)))
  sequence)

(defparameter dylan/reverse!
  (dylan%generic-fn 'reverse! one-sequence
                    (lambda (seq-1)
                      (if (mutable-sequence-p seq-1)
                          (replace-elements! seq-1 (reverse (sequence->list seq-1)))
                          (dylan-call dylan/reverse seq-1)))))

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
    (let* ((test? (dylan%find-keyword rest :test (lambda () dylan/<)))
           (stable (dylan%find-keyword rest :stable (lambda () dylan%false)))
           (sorted (dylan-call dylan/sort seq :test test? :stable stable)))
      (if (mutable-sequence-p seq)
          (replace-elements! seq (sequence->list sorted))
          sorted)))))

;;; First, second, third and last take default: (design note 2): the value
;;; when the sequence has no such element, which is otherwise an error.
;;; First, second and third, and their setters, call element or (setter
;;; element) with the index (design note 28), so their errors are
;;; element's, as in a compiled program.

(defun nth-accessor (name index)
  (let ((param-list (make-param-list `((sequence ,<sequence>))
                                     nil nil '(:default))))
    (let ((generic-function (dylan%generic-fn name param-list nil)))
      (add-method
       generic-function
       (dylan%dylan-callable->method
        param-list
        (lambda (multiple-values next-method sequence &rest rest)
          (declare (ignore multiple-values))
          (dylan%keyword-validate next-method rest '(:default))
          (dylan-apply dylan/element sequence index rest))))
      generic-function)))

(defparameter dylan/first (nth-accessor 'first 0))
(defparameter dylan/second (nth-accessor 'second 1))
(defparameter dylan/third (nth-accessor 'third 2))

(defun nth-setter (name index)
  (dylan%generic-fn
   name one-mutable-sequence-and-an-object
   (lambda (sequence new-value)
     (dylan-call dylan/setter/element/ sequence index new-value)
     new-value)))

(defparameter dylan/setter/first/ (nth-setter 'setter/first/ 0))
(defparameter dylan/setter/second/ (nth-setter 'setter/second/ 1))
(defparameter dylan/setter/third/ (nth-setter 'setter/third/ 2))

(defparameter dylan/last
  (let ((param-list (make-param-list `((sequence ,<sequence>))
                                     nil nil '(:default))))
    (let ((generic-function (dylan%generic-fn 'last param-list nil)))
      (add-method
       generic-function
       (dylan%dylan-callable->method
        param-list
        (lambda (multiple-values next-method sequence &rest rest)
          (declare (ignore multiple-values))
          (dylan%keyword-validate next-method rest '(:default))
          ;; The last element the iteration reaches (as the compiler to C
          ;; finds it)
          (let ((state (initial-state sequence)))
            (cond ((null (collection-size sequence))
                   (dylan-call dylan/error "last: the sequence is unbounded"))
                  ((not state)
                   (dylan%find-keyword
                    rest :default
                    (lambda ()
                      (dylan-call dylan/error "last: the sequence is empty"))))
                  (t (do ((previous state state)
                          (state (next-state sequence state)
                                 (next-state sequence state)))
                         ((not state) (current-element sequence previous)))))))))
      generic-function)))

;;; Design note 12: the methods are on <stretchy-vector> and <deque>.
(defparameter dylan/setter/size/
  (dylan%generic-fn 'setter/size/ one-sequence-and-an-object nil))

(defun check-new-size (sequence n)
  (declare (ignore sequence))
  (unless (and (integerp n) (not (minusp n)))
    (dylan-call dylan/error
                "(setter size): the size must be a non-negative integer: ~s"
                n))
  (check-room n 8))

;;; Design note 11.
(defparameter dylan/setter/last/
  (dylan%generic-fn
   'setter/last/ one-mutable-sequence-and-an-object
   (lambda (sequence new-value)
     (let ((size (collection-size sequence)))
       (cond ((null size)
              (dylan-call dylan/error "last: the sequence is unbounded"))
             ((zerop size)
              (dylan-call dylan/error "last: the sequence is empty")))
       (library-element-setter sequence (- size 1) new-value)
       new-value))))

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
        ;; The countth place, counting from 1: there is none before the first.
        (unless (integerp count)
          (dylan-call dylan/error "subsequence-position: count: must be an integer: ~s"
                      count))
        (when (< count 1)
          (return-from subsequence-position dylan%false))
        (when (null init-state-pattern)
          ;; An empty pattern occurs at every index, up to the size of BIG.
          (return-from subsequence-position
            (let ((size (collection-size big)))
              (if (or (null size) (<= count (+ 1 size)))
                  (- count 1)
                  dylan%false))))
        (loop with first-of-pattern = (library-element pattern 0)
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
  ;; No method of its own, as in the compiled library: a range, say, is
  ;; not mutable, and no method takes it
  (dylan%generic-fn
   'setter/current-element/
   (make-param-list `((mutable-collection ,<mutable-collection>)
                      (state ,<object>) (new-value ,<object>))
                    nil nil nil)
   nil))

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
             (dylan-call dylan/error
                         "element: there is no element at the index ~s"
                         key)))))))

;;; Any other key: the index it denotes, if any, as for element.
(add-method
 dylan/setter/element/
 (dylan%function->method
  (make-param-list `((mutable-sequence ,<mutable-sequence>)
                     (key ,<object>) (new-value ,<object>))
                   nil nil nil)
  (lambda (mut-seq key new-value)
    (let ((index (index-key key)))
      (if index
          (library-element-setter mut-seq index new-value)
          (dylan-call dylan/error
                      "element: there is no element at the index ~s"
                      key))))))

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
             (dylan-call dylan/error
                         "element: there is no element for the key ~s"
                         key)))))))
