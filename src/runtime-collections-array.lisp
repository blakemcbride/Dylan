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
;;;; This file contains all the specializations for the array type.
;;;;                        (was Thomas's src/runtime-collections-array.scm)

(in-package #:dylan)

(declaim (special dylan/get-array-value dylan/get-array-dimensions
                  dylan/set-array-value! dylan/set-array-dimensions!))

(add-method
 dylan/shallow-copy
 (dylan%function->method
  (make-param-list `((array ,<array>)) nil nil nil)
  (lambda (array)
    (let* ((dimensions (dylan-call dylan/dimensions array))
           (new-array (dylan-call dylan/make <array> :dimensions dimensions))
           (key-sequence (dylan-call dylan/key-sequence array)))
      (do ((state (initial-state key-sequence)
                  (next-state key-sequence state)))
          ((not state)
           (dylan-call dylan/as (dylan-call dylan/class-for-copy array)
                       new-array))
        (let ((key (current-element key-sequence state)))
          (library-element-setter new-array key
                      (library-element array key))))))))

(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <array>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (declare (ignore class))
    (if (true? (dylan-call dylan/instance? collection <array>))
        collection
        (let* ((size (collection-size collection))
               (new-array (dylan-call dylan/make <array> :dimensions (list size)))
               (vector-value (dylan-call dylan/get-array-value new-array)))
          (do ((state (initial-state collection)
                      (next-state collection state))
               (index 0 (+ index 1)))
              ((not state) new-array)
            (setf (aref vector-value index)
                  (current-element collection state))))))))

;;;
;;; ARRAY SPECIALIZED MAKE
;;;

;; All subclasses of ARRAY have slots for the data ("value") and a list of
;; dimensions ("dimensions").

(defparameter dylan/get-array-value nil)
(defparameter dylan/get-array-dimensions nil)
(defparameter dylan/set-array-value! nil)
(defparameter dylan/set-array-dimensions! nil)

(create-private-slot <array> <simple-object-vector> "internal-array-value"
                     (lambda (set get)
                       (setf dylan/set-array-value! set)
                       (setf dylan/get-array-value get))
                     :init (lambda () (vector)))
(create-private-slot <array> <list> "internal-array-dimensions"
                     (lambda (set get)
                       (setf dylan/set-array-dimensions! set)
                       (setf dylan/get-array-dimensions get))
                     :init (lambda () (list 0)))

(define-direct-slot stretchy-value set-stretchy-value! <stretchy-vector>
  dylan/get-array-value dylan/set-array-value!)
(define-direct-slot stretchy-dimensions set-stretchy-dimensions!
  <stretchy-vector> dylan/get-array-dimensions dylan/set-array-dimensions!)

(defun vector-value (vector)
  "What GET-ARRAY-VALUE answers for VECTOR, a <vector>: itself for a
simple object vector."
  (if (simple-vector-p vector) vector (stretchy-value vector)))

;; These four generic operations must be specialized for the special cases
;; of <byte-string> and <simple-object-vector>

(dolist (class (list <byte-string> <unicode-string>))
  (add-method dylan/get-array-value
              (one-arg 'string class #'identity))
  (add-method
   dylan/set-array-value!
   (dylan%function->method
    (make-param-list `((string ,class) (value ,<object>)) nil nil nil)
    (lambda (string value)
      (dylan-call dylan/error "set-array-value! -- internal error on string"
                  string value))))
  (add-method dylan/get-array-dimensions
              (one-arg 'string class
                       (lambda (string) (list (length string)))))
  (add-method
   dylan/set-array-dimensions!
   (dylan%function->method
    (make-param-list `((string ,class) (value ,<object>)) nil nil nil)
    (lambda (string value)
      (dylan-call dylan/error "set-array-dimensions! -- internal error on string"
                  string value)))))

(add-method dylan/get-array-value
            (one-arg 'vector <simple-object-vector> #'identity))
(add-method
 dylan/set-array-value!
 (dylan%function->method
  (make-param-list `((vector ,<simple-object-vector>) (value ,<object>))
                   nil nil nil)
  (lambda (vector value)
    (dylan-call dylan/error
                "set-array-value! -- internal error on simple-object-vector"
                vector value))))
(add-method dylan/get-array-dimensions
            (one-arg 'vector <simple-object-vector>
                     (lambda (vector) (list (length vector)))))
(add-method
 dylan/set-array-dimensions!
 (dylan%function->method
  (make-param-list `((vector ,<simple-object-vector>) (value ,<object>))
                   nil nil nil)
  (lambda (vector value)
    (dylan-call dylan/error
                "set-array-dimensions! -- internal error on simple-object-vector"
                vector value))))

(add-method
 dylan/make
 (dylan%dylan-callable->method
  (make-param-list `((array ,(dylan%make-singleton <array>)))
                   nil nil '(:dimensions :fill))
  (lambda (multiple-values next-method class &rest rest)
    (declare (ignore multiple-values))
   (block make-array-method
    (labels ((make-multi-dimensional-array (dimensions fill)
               ;; An array of no dimensions has one element, kept in a
               ;; vector of one (see LOCATE-ARRAY-ELEMENT).
               (when (null dimensions)
                 (return-from make-multi-dimensional-array
                   (make-array 1 :initial-element fill)))
               (let ((result (make-array (car dimensions)
                                         :initial-element fill)))
                 (if (null (cdr dimensions))
                     result
                     (do ((n 0 (+ n 1)))
                         ((= n (car dimensions)) result)
                       (setf (aref result n)
                             (make-multi-dimensional-array (cdr dimensions)
                                                           fill)))))))
      (dylan%keyword-validate next-method rest '(:dimensions :fill))
      (let ((dimensions
              (dylan%find-keyword
               rest :dimensions
               (lambda ()
                 (dylan-call dylan/error "make: <array> requires dimensions:"))))
            (fill-value (dylan%find-keyword rest :fill (lambda () dylan%false))))
        (declare (ignore class))
        (flet ((bad-dimensions ()
                 (dylan-call dylan/error
                             "make: the dimensions of an <array> are not non-negative integers: ~s"
                             dimensions)))
        (unless (subclass? (get-type dimensions) <sequence>)
          (bad-dimensions))
        (let ((instance (dylan%make-<object> <%general-array>))
              (dim-list
                (iterate->list
                 (lambda (elem)
                   ;; Chapter 12: non-negative integers.
                   (if (not (and (integerp elem) (not (minusp elem))))
                       (bad-dimensions)
                       elem))
                 dimensions)))
          (check-room (cl:reduce #'* dim-list) 8)
          ;; One dimension: a <simple-object-vector>, as in a compiled
          ;; program (a vector is an array of one dimension).
          (when (and (consp dim-list) (null (cdr dim-list)))
            (return-from make-array-method
              (make-array (car dim-list) :initial-element fill-value)))
          (dylan-call dylan/set-array-value! instance
                      (make-multi-dimensional-array dim-list fill-value))
          (dylan-call dylan/set-array-dimensions! instance dim-list)
          instance))))))))

;;;;
;;;; Operations on Arrays (page 113)
;;;;

;;; An array is stored as nested simple vectors, one level per dimension.
;;; Element locations are found from the array's dimensions, which say how
;;; deep to go -- not from whether what is found is a vector, since an
;;; element may itself be a vector.  (The Scheme went by the latter, so it
;;; answered an internal row for too few subscripts and would not
;;; overwrite an element that was a vector.)

(defun locate-array-element (array-instance indices fail)
  "Answer the row vector holding element INDICES of ARRAY-INSTANCE and the
index within it, or call FAIL with :COUNT, or :BOUNDS and the first index
not within its dimension, if INDICES are the wrong number or out of bounds
(chapter 12).  The indices are checked in turn against the dimensions, as
in a compiled program."
  (let ((dimensions (dylan-call dylan/get-array-dimensions array-instance)))
    (loop for ds = dimensions then (cdr ds)
          for is = indices then (cdr is)
          until (and (null ds) (null is))
          do (cond ((or (null ds) (null is))
                    (return (funcall fail :count nil)))
                   ((not (and (integerp (car is)) (< -1 (car is) (car ds))))
                    (return (funcall fail :bounds (car is))))))
    (if (null indices)                  ; No dimensions: the one element
        (values (dylan-call dylan/get-array-value array-instance) 0)
        (let ((row (dylan-call dylan/get-array-value array-instance)))
          (dolist (index (butlast indices))
            (setf row (aref row index)))
          (values row (car (cl:last indices)))))))

(defun general-array-p (object)
  (and (instance? object) (eq (instance.class object) <%general-array>)))

;;; The number of elements, as the compiled library answers it, without
;;; walking them
(add-method
 dylan/size
 (one-arg 'array <%general-array>
          (lambda (array)
            (cl:reduce #'* (dylan-call dylan/get-array-dimensions array)))))

(defun aref-error (array kind index)
  (if (eq kind :count)
      (dylan-call dylan/error "aref: the wrong number of indices for ~s" array)
      (dylan-call dylan/error "aref: the index ~s is out of bounds" index)))

;;; Chapter 12: aref and dimensions are functions, not generic functions.
(defparameter dylan/aref
  (dylan%function->method
   (make-param-list `((array ,<array>)) nil t nil)
   ;; An array not of one dimension is found by its indices; any other,
   ;; a vector, by its one index, as element finds it -- as in a compiled
   ;; program.
   (lambda (array-instance &rest indices)
     (dylan-call dylan/check-type array-instance <array>)
     (cond ((general-array-p array-instance)
            (multiple-value-bind (row index)
                (locate-array-element
                 array-instance indices
                 (lambda (kind index) (aref-error array-instance kind index)))
              (aref row index)))
           ((and (consp indices) (null (cdr indices)))
            (library-element array-instance (car indices)))
           (t (aref-error array-instance :count nil))))))

(defparameter dylan/setter/aref/
  (dylan%function->method
   (make-param-list `((array ,<array>)) nil t nil)
   (lambda (array-instance &rest indices-and-new-value)
     (dylan-call dylan/check-type array-instance <array>)
     (when (null indices-and-new-value)
       (dylan-call dylan/error
                   "(setter aref) -- no indices and new-value given"
                   array-instance))
     (let ((new-value (car (cl:last indices-and-new-value)))
           (indices (butlast indices-and-new-value)))
       (cond ((general-array-p array-instance)
              (multiple-value-bind (row index)
                  (locate-array-element
                   array-instance indices
                   (lambda (kind index) (aref-error array-instance kind index)))
                (setf (aref row index) new-value)))
             ((and (consp indices) (null (cdr indices)))
              (library-element-setter array-instance (car indices)
                          new-value))
             (t (aref-error array-instance :count nil)))))))

(defparameter dylan/dimensions
  (dylan%function->method
   (make-param-list `((array ,<array>)) nil nil nil)
   ;; A copy: the array's own list is what it checks subscripts against.
   (lambda (array)
     (dylan-call dylan/check-type array <array>)
     (copy-list (dylan-call dylan/get-array-dimensions array)))))

(add-method
 dylan/element
 (dylan%dylan-callable->method
  (make-param-list `((array ,<array>) (index ,<sequence>))
                   nil nil '(:default))
  (lambda (multiple-values next-method array-instance index-sequence &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:default))
    ;; Chapter 12: the default is answered for an invalid index sequence if
    ;; one was given at all -- #f included.
    (let* ((no-default (list 'no-default))
           (default (dylan%find-keyword rest :default (lambda () no-default))))
      (block element
        (multiple-value-bind (row index)
            (locate-array-element
             array-instance (iterate->list #'identity index-sequence)
             (lambda (kind index)
               (declare (ignore kind index))
               (return-from element
                 (if (eq default no-default)
                     (dylan-call dylan/error "element: there is no element at ~s"
                                 index-sequence)
                     default))))
          (aref row index)))))))

(add-method
 dylan/setter/element/
 (dylan%function->method
  (make-param-list `((array ,<%general-array>) (index ,<sequence>)
                     (new-value ,<object>))
                   nil nil nil)
  (lambda (array-instance index-sequence new-value)
    (multiple-value-bind (row index)
        (locate-array-element
         array-instance (iterate->list #'identity index-sequence)
         (lambda (kind index)
           (declare (ignore kind index))
           (dylan-call dylan/error "element: there is no element at ~s"
                       index-sequence)))
      (setf (aref row index) new-value)))))

(add-method
 dylan/current-key
 (dylan%function->method
  (make-param-list `((array ,<array>) (state ,<object>)) nil nil nil)
  (lambda (array state) (declare (ignore array)) state)))

;;;
;;; Mutable Collections
;;;

(add-method
 dylan/setter/current-element/
 (dylan%function->method
  (make-param-list `((array ,<array>) (state ,<object>) (new-value ,<object>))
                   nil nil nil)
  ;; The state is the element's subscripts, which may be none.
  (lambda (array state new-value)
    (multiple-value-bind (row index)
        (locate-array-element
         array (coerce state 'list)
         (lambda (message)
           (dylan-call dylan/error
                       (concatenate 'string "(setter current-element) -- " message)
                       array state)))
      (setf (aref row index) new-value)))))

;;; A copy has its own elements, as in a compiled program
(add-method
 dylan/shallow-copy
 (dylan%function->method
  (make-param-list `((array ,<%general-array>)) nil nil nil)
  (lambda (array)
    (dylan-call dylan/map-as <array> dylan/identity array))))
