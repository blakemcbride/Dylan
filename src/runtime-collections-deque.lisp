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

(declaim (special dylan/get-deque-front dylan/get-deque-last
                  dylan/set-deque-front! dylan/set-deque-last!))

;;;;
;;;; OPERATIONS ON DEQUES (page 114)
;;;;

(defstruct (deque-entry
            (:conc-name deque-entry.)
            (:predicate deque-entry?)
            (:print-object print-opaque-object)
            (:constructor make-deque-entry (previous next value)))
  previous                              ; Pointer to previous entry
  next                                  ; Pointer to next entry
  value)                                ; Value of this entry

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
    (let ((new-deque (dylan-call dylan/make <deque>)))
      (do ((state (dylan-call dylan/initial-state object)
                  (dylan-call dylan/next-state object state)))
          ((not state) new-deque)
        (dylan-call dylan/push-last new-deque
                    (dylan-call dylan/current-element object state)))))))

;;;
;;; DEQUE SPECIALIZED MAKE
;;; Two slots: front and end of deque
;;;

(defparameter dylan/get-deque-front nil)
(defparameter dylan/get-deque-last nil)
(defparameter dylan/set-deque-front! nil)
(defparameter dylan/set-deque-last! nil)

(create-private-slot <deque> <object> "internal-deque-front"
                     (lambda (set get)
                       (setf dylan/set-deque-front! set)
                       (setf dylan/get-deque-front get)))
(create-private-slot <deque> <object> "internal-deque-last"
                     (lambda (set get)
                       (setf dylan/set-deque-last! set)
                       (setf dylan/get-deque-last get)))

(add-method
 dylan/make
 (dylan%dylan-callable->method
  (make-param-list `((deque ,(dylan%make-singleton <deque>)))
                   nil nil '(:size :fill))
  (lambda (multiple-values next-method class &rest rest)
    (declare (ignore multiple-values class))
    (dylan%keyword-validate next-method rest '(:size :fill))
    (let ((size (dylan%find-keyword rest :size (lambda () 0)))
          (fill (dylan%find-keyword rest :fill (lambda () nil))))
      (when (or (not (integerp size)) (minusp size))
        (dylan-call dylan/error "make -- deque size invalid" size))
      (let ((instance (dylan%make-<object> <deque>)))
        (dylan-call dylan/set-deque-front! instance '())
        (dylan-call dylan/set-deque-last! instance '())
        (do ((n 0 (+ n 1)))
            ((= n size) instance)
          (dylan-call dylan/push instance fill)))))))

;;;;
;;;; Functions for Sequences (page 104)
;;;;

(add-method
 dylan/add
 (dylan%function->method one-deque-and-an-object
                         (lambda (deque new-element)
                           (dylan-call dylan/push deque new-element))))

(defun dylan%push (deque new-value)
  (let* ((old-deque-front (dylan-call dylan/get-deque-front deque))
         (new-entry (make-deque-entry nil old-deque-front new-value)))
    (if old-deque-front
        (setf (deque-entry.previous old-deque-front) new-entry)
        (dylan-call dylan/set-deque-last! deque new-entry))
    (dylan-call dylan/set-deque-front! deque new-entry)
    deque))

(add-method dylan/add!
            (dylan%function->method one-deque-and-a-value #'dylan%push))

(add-method
 dylan/remove!
 (dylan%dylan-callable->method
  (make-param-list `((deque ,<deque>) (value ,<object>))
                   nil nil '(:test :count))
  (lambda (multiple-values next-method deque value &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test :count))
    (let ((test? (dylan%find-keyword rest :test (lambda () dylan/id?)))
          (count (dylan%find-keyword rest :count
                                     (lambda () (dylan-call dylan/size deque))))
          (num-changed 0)
          (front-entry (dylan-call dylan/get-deque-front deque)))
      (cond ((null front-entry) front-entry)
            ((null (deque-entry.next front-entry))
             (when (and (> count 0)
                        (dylan-call test? (deque-entry.value front-entry) value))
               (dylan-call dylan/set-deque-front! deque nil)
               (dylan-call dylan/set-deque-last! deque nil))
             deque)
            (t
             (loop
               (if (and (> count num-changed)
                        (dylan-call test? (deque-entry.value front-entry) value))
                   (let ((next-entry (deque-entry.next front-entry)))
                     (incf num-changed)
                     (setf (deque-entry.previous next-entry) nil)
                     (dylan-call dylan/set-deque-front! deque next-entry)
                     (if next-entry
                         (setf front-entry next-entry)
                         (return deque)))
                   (return nil)))
             (loop with current-entry = (deque-entry.next
                                         (dylan-call dylan/get-deque-front
                                                     deque))
                   do (if (or (null current-entry) (>= num-changed count))
                          (return deque)
                          (if (dylan-call test?
                                          (deque-entry.value current-entry)
                                          value)
                              (let ((prev (deque-entry.previous current-entry))
                                    (next (deque-entry.next current-entry)))
                                (if prev
                                    (setf (deque-entry.next prev) next)
                                    (dylan-call dylan/set-deque-front!
                                                deque next))
                                (if next
                                    (progn
                                      (setf (deque-entry.previous next) prev)
                                      (setf current-entry next)
                                      (incf num-changed))
                                    (progn
                                      (dylan-call dylan/set-deque-last!
                                                  deque prev)
                                      (return deque))))
                              (setf current-entry
                                    (deque-entry.next current-entry)))))))))))

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
      (do ((count (dylan-call dylan/size old-deque) (- count 1)))
          ((<= count 0) 'done)
        (dylan-call dylan/pop old-deque))
      (do ((count (dylan-call dylan/size new-deque) (- count 1)))
          ((<= count 0) old-deque)
        (dylan-call dylan/push-last old-deque
                    (dylan-call dylan/pop new-deque)))))))

(add-method
 dylan/concatenate
 (dylan%function->method
  (make-param-list `((deque ,<deque>)) nil t nil)
  (lambda (deque-1 &rest rest)
    (let ((result-deque (dylan-call dylan/make <deque>)))
      (dolist (collection (cons deque-1 rest) result-deque)
        (iterate! (lambda (element)
                    (dylan-call dylan/push-last result-deque element))
                  collection))))))

(add-method
 dylan/reverse
 (dylan%function->method
  one-deque
  (lambda (deque-1)
    (let ((result (dylan-call dylan/make <deque>)))
      (do ((state (dylan-call dylan/initial-state deque-1)
                  (dylan-call dylan/next-state deque-1 state)))
          ((not state) result)
        (dylan-call dylan/push result
                    (dylan-call dylan/current-element deque-1 state)))))))

(add-method
 dylan/reverse!
 (dylan%function->method
  one-deque
  (lambda (deque-1)
    (let ((tmp-deque (dylan-call dylan/make <deque>))
          (size (dylan-call dylan/size deque-1)))
      (if (<= size 0)
          deque-1
          (progn
            (do ((i 1 (+ i 1)))
                ((= i size) 'continue)
              (dylan-call dylan/push tmp-deque (dylan-call dylan/pop deque-1)))
            (do ((i 1 (+ i 1)))
                ((= i size) deque-1)
              (dylan-call dylan/push-last deque-1
                          (dylan-call dylan/pop tmp-deque)))))))))

(add-method
 dylan/sort!
 (dylan%dylan-callable->method
  (make-param-list `((deque ,<deque>)) nil nil '(:test :stable))
  (lambda (multiple-values next-method deque &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test :stable))
    (let* ((test? (dylan%find-keyword rest :test (lambda () dylan/<)))
           (deque-list (scheme-sort (iterate->list #'identity deque)
                                    (lambda (x y) (dylan-call test? x y)))))
      (dylan%find-keyword rest :stable (lambda () nil)) ; Ignored
      (dylan%empty-deque! deque)
      (dolist (x deque-list) (dylan-call dylan/push-last deque x))
      deque))))

(defun dylan%empty-deque! (deque)
  (let ((size (dylan-call dylan/size deque)))
    (do ((i 0 (+ i 1)))
        ((= i size) deque)
      (dylan-call dylan/pop deque))))

(defparameter dylan/push
  (dylan%generic-fn 'push one-deque-and-a-value #'dylan%push))

(defparameter dylan/pop
  (dylan%generic-fn
   'pop one-deque
   (lambda (deque)
     (let ((old-front-entry (dylan-call dylan/get-deque-front deque)))
       (if old-front-entry
           (let ((old-front-value (deque-entry.value old-front-entry))
                 (second-entry (deque-entry.next old-front-entry)))
             (if second-entry
                 (progn
                   (dylan-call dylan/set-deque-front! deque second-entry)
                   (setf (deque-entry.previous second-entry) nil))
                 (progn
                   (dylan-call dylan/set-deque-front! deque nil)
                   (dylan-call dylan/set-deque-last! deque nil)))
             old-front-value)
           (dylan-call dylan/error "pop -- deque is empty" deque))))))

(defparameter dylan/push-last
  (dylan%generic-fn
   'push-last one-deque-and-a-value
   (lambda (deque new-value)
     (let* ((old-deque-last (dylan-call dylan/get-deque-last deque))
            (new-entry (make-deque-entry old-deque-last nil new-value)))
       (if old-deque-last
           (setf (deque-entry.next old-deque-last) new-entry)
           (dylan-call dylan/set-deque-front! deque new-entry))
       (dylan-call dylan/set-deque-last! deque new-entry)
       deque))))

(defparameter dylan/pop-last
  (dylan%generic-fn
   'pop-last one-deque
   (lambda (deque)
     (let ((old-last-entry (dylan-call dylan/get-deque-last deque)))
       (if old-last-entry
           (let ((old-last-value (deque-entry.value old-last-entry))
                 (previous-entry (deque-entry.previous old-last-entry)))
             (if previous-entry
                 (progn
                   (dylan-call dylan/set-deque-last! deque previous-entry)
                   (setf (deque-entry.next previous-entry) nil))
                 (progn
                   (dylan-call dylan/set-deque-last! deque nil)
                   (dylan-call dylan/set-deque-front! deque nil)))
             old-last-value)
           (dylan-call dylan/error "pop-last -- deque is empty" deque))))))

;;;
;;; Mutable Collection
;;;

(add-method
 dylan/setter/current-element/
 (dylan%function->method
  (make-param-list `((deque ,<deque>) (state ,<object>) (new-value ,<object>))
                   nil nil nil)
  (lambda (deque state new-value)
    (declare (ignore deque))
    (setf (deque-entry.value state) new-value)
    new-value)))
