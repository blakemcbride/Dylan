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
;;;; Specializations for the Deque Type
;;;;                        (was Thomas's src/runtime-collections-deque.scm)

(in-package #:dylan)

;;;; A <deque> is held, as in the compiled library (10-deques.dyl), in a
;;;; circular buffer: element i is at (first + i) modulo the buffer's
;;;; length, and the iteration state is the index.  SIZE and ELEMENT take
;;;; constant time.

(declaim (special dylan/get-deque-data dylan/get-deque-first
                  dylan/get-deque-count dylan/set-deque-data!
                  dylan/set-deque-first! dylan/set-deque-count!))

(defparameter dylan/get-deque-data nil)
(defparameter dylan/get-deque-first nil)
(defparameter dylan/get-deque-count nil)
(defparameter dylan/set-deque-data! nil)
(defparameter dylan/set-deque-first! nil)
(defparameter dylan/set-deque-count! nil)

(create-private-slot <deque> <object> "internal-deque-data"
                     (lambda (set get)
                       (setf dylan/set-deque-data! set)
                       (setf dylan/get-deque-data get))
                     :init (lambda () (make-array 8 :initial-element dylan%false)))
(create-private-slot <deque> <object> "internal-deque-first"
                     (lambda (set get)
                       (setf dylan/set-deque-first! set)
                       (setf dylan/get-deque-first get))
                     :init (lambda () 0))
(create-private-slot <deque> <object> "internal-deque-count"
                     (lambda (set get)
                       (setf dylan/set-deque-count! set)
                       (setf dylan/get-deque-count get))
                     :init (lambda () 0))

(define-direct-slot deque-data set-deque-data! <deque>
  dylan/get-deque-data dylan/set-deque-data!)
(define-direct-slot deque-first set-deque-first! <deque>
  dylan/get-deque-first dylan/set-deque-first!)
(define-direct-slot deque-count set-deque-count! <deque>
  dylan/get-deque-count dylan/set-deque-count!)

(defun deque-slot (deque i)
  "The buffer index of DEQUE's element I."
  (let ((n (length (deque-data deque)))
        (k (+ (deque-first deque) i)))
    (if (< k n) k (- k n))))

(defun deque-grow (deque)
  "Room for one more element: a buffer twice as long when DEQUE's is full."
  (let* ((old (deque-data deque))
         (n (length old))
         (count (deque-count deque)))
    (when (= count n)
      (let ((new (make-array (* 2 n) :initial-element dylan%false)))
        (dotimes (i count)
          (setf (svref new i) (svref old (deque-slot deque i))))
        (set-deque-data! deque new)
        (set-deque-first! deque 0)))))

(defun deque-index (deque key)
  "The index KEY denotes among DEQUE's elements, or NIL."
  (let ((i (index-key key)))
    (and i (< -1 i (deque-count deque)) i)))

(defun deque-check (deque key)
  (or (deque-index deque key)
      (dylan-call dylan/error "element: there is no element at the index ~s" key)))

(defun deque-ref (deque i)
  (svref (deque-data deque) (deque-slot deque i)))

(defun deque-set (deque i value)
  (setf (svref (deque-data deque) (deque-slot deque i)) value))

(defun deque-refill (deque elements)
  "Make ELEMENTS, a list, DEQUE's elements."
  (let ((data (make-array (max 8 (length elements)) :initial-element dylan%false)))
    (loop for e in elements for i from 0 do (setf (svref data i) e))
    (set-deque-data! deque data)
    (set-deque-first! deque 0)
    (set-deque-count! deque (length elements))
    deque))

(defun deque-elements (deque)
  (loop for i from 0 below (deque-count deque) collect (deque-ref deque i)))

;;;;
;;;; OPERATIONS ON DEQUES (page 114)
;;;;

(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <deque>))
                     (object ,<collection>)) nil nil nil)
  (lambda (class object)
    (unless (subclass? (dylan-call dylan/object-class object) <collection>)
      (dylan-call dylan/error
                  "as -- cannot convert a non-collection to a collection"
                  class object (dylan-call dylan/object-class object)))
    ;; Chapter 11: an object already a deque is returned unchanged.
    (if (subclass? (dylan-call dylan/object-class object) <deque>)
        object
        (let ((new-deque (dylan-call dylan/make <deque>)))
          (do ((state (initial-state object)
                      (next-state object state)))
              ((not state) new-deque)
            (dylan-call dylan/push-last new-deque
                        (current-element object state))))))))

(add-method
 dylan/make
 (dylan%dylan-callable->method
  (make-param-list `((deque ,(dylan%make-singleton <deque>)))
                   nil nil '(:size :fill))
  (lambda (multiple-values next-method class &rest rest)
    (declare (ignore multiple-values class))
    (dylan%keyword-validate next-method rest '(:size :fill))
    (let ((size (dylan%find-keyword rest :size (lambda () 0)))
          (fill (dylan%find-keyword rest :fill (lambda () dylan%false))))
      (when (or (not (integerp size)) (minusp size))
        (dylan-call dylan/error "make: size: must be a non-negative integer: ~s" size))
      (check-room size 8)
      (let ((instance (dylan%make-<object> <deque>)))
        (set-deque-data! instance (make-array (max size 8) :initial-element fill))
        (set-deque-first! instance 0)
        (set-deque-count! instance size)
        instance)))))

;;; A program's subclass is made by make's method on <class>, which calls
;;; initialize: it takes size: and fill: too (chapter 12).
(add-method
 dylan/initialize
 (dylan%dylan-callable->method
  (make-param-list `((deque ,<deque>)) nil nil '(:size :fill))
  (lambda (multiple-values next-method deque &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:size :fill))
    (let ((size (dylan%find-keyword rest :size (lambda () 0)))
          (fill (dylan%find-keyword rest :fill (lambda () dylan%false))))
      (when (or (not (integerp size)) (minusp size))
        (dylan-call dylan/error "make: size: must be a non-negative integer: ~s" size))
      (check-room size 8)
      (set-deque-data! deque (make-array (max size 8) :initial-element fill))
      (set-deque-first! deque 0)
      (set-deque-count! deque size)
      (if (functionp next-method)
          (apply next-method nil nil deque rest)
          deque)))))

;;;;
;;;; Functions for Collections
;;;;

(add-method
 dylan/size
 (one-arg 'deque <deque> #'deque-count))

(add-method
 dylan/element
 (dylan%dylan-callable->method
  (make-param-list `((deque ,<deque>) (key ,<object>)) nil nil '(:default))
  (lambda (multiple-values next-method deque key &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:default))
    (let ((i (deque-index deque key)))
      (if i
          (deque-ref deque i)
          (dylan%find-keyword rest :default
                              (lambda ()
                                (dylan-call dylan/error
                                            "element: there is no element at the index ~s"
                                            key))))))))

(add-method
 dylan/setter/element/
 (dylan%function->method
  (make-param-list `((deque ,<deque>) (key ,<object>) (new-value ,<object>))
                   nil nil nil)
  (lambda (deque key new-value)
    (deque-set deque (deque-check deque key) new-value)
    new-value)))

;;;;
;;;; Functions for Sequences (page 104)
;;;;

(add-method
 dylan/add
 (dylan%function->method one-deque-and-an-object
                         ;; A new deque: ADD! is the one that may change
                         ;; its argument (chapter 12).  A copy of another
                         ;; class-for-copy is added to as that class adds.
                         (lambda (deque new-element)
                           (let ((copy (dylan-call dylan/copy-sequence deque)))
                             (if (true? (dylan-call dylan/instance? copy <deque>))
                                 (dylan-call dylan/push copy new-element)
                                 (dylan-call dylan/add copy new-element))))))

(defun dylan%push (deque new-value)
  (deque-grow deque)
  (let ((n (length (deque-data deque)))
        (first (deque-first deque)))
    (set-deque-first! deque (if (zerop first) (- n 1) (- first 1)))
    (setf (svref (deque-data deque) (deque-first deque)) new-value)
    (set-deque-count! deque (+ (deque-count deque) 1))
    deque))

(add-method dylan/add!
            (dylan%function->method one-deque-and-a-value #'dylan%push))

;;; Up to COUNT elements that TEST finds the same as VALUE are removed,
;;; the rest kept in order.
(add-method
 dylan/remove!
 (dylan%dylan-callable->method
  (make-param-list `((deque ,<deque>) (value ,<object>))
                   nil nil '(:test :count))
  (lambda (multiple-values next-method deque value &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test :count))
    (let ((test? (dylan%find-keyword rest :test (lambda () dylan/id?)))
          (left (false->nil (dylan%find-keyword rest :count (lambda () nil))))
          (kept '()))
      (dotimes (i (deque-count deque))
        (let ((element (deque-ref deque i)))
          (if (and (or (null left) (> left 0))
                   (true? (dylan-call test? element value)))
              (when left (decf left))
              (push element kept))))
      (deque-refill deque (nreverse kept))))))

(add-method
 dylan/remove-duplicates!
 (dylan%dylan-callable->method
  (make-param-list `((deque ,<deque>)) nil nil '(:test))
  (lambda (multiple-values next-method old-deque &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test))
    (let* ((test? (dylan%find-keyword rest :test (lambda () dylan/id?)))
           (new-deque (dylan-call dylan/remove-duplicates old-deque
                                  :test test?)))
      (deque-refill old-deque (iterate->list #'identity new-deque))))))

(add-method
 dylan/concatenate
 (dylan%function->method
  (make-param-list `((deque ,<deque>)) nil t nil)
  (lambda (deque-1 &rest rest)
    (check-bounded "concatenate" rest)
    ;; An instance of the class-for-copy (chapter 12)
    (if (eq (dylan-call dylan/class-for-copy deque-1) <deque>)
        (let ((result-deque (dylan-call dylan/make <deque>)))
          (dolist (collection (cons deque-1 rest) result-deque)
            (iterate! (lambda (element)
                        (dylan-call dylan/push-last result-deque element))
                      collection)))
        (copy-like deque-1 (mapcan #'sequence->list (cons deque-1 rest)))))))

(add-method
 dylan/reverse
 (dylan%function->method
  one-deque
  (lambda (deque-1)
    ;; An instance of the class-for-copy (chapter 12)
    (if (eq (dylan-call dylan/class-for-copy deque-1) <deque>)
        (let ((result (dylan-call dylan/make <deque>)))
          (do ((state (initial-state deque-1)
                      (next-state deque-1 state)))
              ((not state) result)
            (dylan-call dylan/push result
                        (current-element deque-1 state))))
        (copy-like deque-1 (reverse (sequence->list deque-1)))))))

(add-method
 dylan/reverse!
 (dylan%function->method
  one-deque
  (lambda (deque)
    (deque-refill deque (nreverse (deque-elements deque))))))

(add-method
 dylan/sort!
 (dylan%dylan-callable->method
  (make-param-list `((deque ,<deque>)) nil nil '(:test :stable))
  (lambda (multiple-values next-method deque &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test :stable))
    (let* ((test? (dylan%find-keyword rest :test (lambda () dylan/<)))
           (deque-list (scheme-sort (iterate->list #'identity deque)
                                    (lambda (x y) (true? (dylan-call test? x y))))))
      (dylan%find-keyword rest :stable (lambda () nil)) ; Ignored
      (deque-refill deque deque-list)))))

(defparameter dylan/push
  (dylan%generic-fn 'push one-deque-and-a-value #'dylan%push))

(defparameter dylan/pop
  (dylan%generic-fn
   'pop one-deque
   (lambda (deque)
     (when (zerop (deque-count deque))
       (dylan-call dylan/error "pop: the deque is empty"))
     (let* ((data (deque-data deque))
            (first (deque-first deque))
            (value (svref data first)))
       (setf (svref data first) dylan%false)
       (set-deque-first! deque (if (= (+ first 1) (length data)) 0 (+ first 1)))
       (set-deque-count! deque (- (deque-count deque) 1))
       value))))

(defparameter dylan/push-last
  (dylan%generic-fn
   'push-last one-deque-and-a-value
   (lambda (deque new-value)
     (deque-grow deque)
     (deque-set deque (deque-count deque) new-value)
     (set-deque-count! deque (+ (deque-count deque) 1))
     deque)))

(defparameter dylan/pop-last
  (dylan%generic-fn
   'pop-last one-deque
   (lambda (deque)
     (when (zerop (deque-count deque))
       (dylan-call dylan/error "pop-last: the deque is empty"))
     (let* ((k (deque-slot deque (- (deque-count deque) 1)))
            (value (svref (deque-data deque) k)))
       (setf (svref (deque-data deque) k) dylan%false)
       (set-deque-count! deque (- (deque-count deque) 1))
       value))))

;;;
;;; Mutable Collection
;;;

(add-method
 dylan/setter/current-element/
 (dylan%function->method
  (make-param-list `((deque ,<deque>) (state ,<integer>) (new-value ,<object>))
                   nil nil nil)
  (lambda (deque state new-value)
    (deque-set deque (deque-check deque state) new-value)
    new-value)))

;;; Design note 12.
(add-method
 dylan/setter/size/
 (dylan%function->method
  (make-param-list `((deque ,<deque>) (n ,<object>)) nil nil nil)
  (lambda (deque n)
    (check-new-size deque n)
    (loop while (> (deque-count deque) n)
          do (dylan-call dylan/pop-last deque))
    (loop while (< (deque-count deque) n)
          do (dylan-call dylan/push-last deque dylan%false))
    n)))
