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
;;;; This file contains all the specializations for the vector,
;;;; simple-object-vector, and stretchy-vector types.
;;;;                        (was Thomas's src/runtime-collections-vector.scm)

(in-package #:dylan)

(add-method                             ; To override <array> handling
 dylan/shallow-copy
 (dylan%function->method
  (make-param-list `((vector ,<vector>)) nil nil nil)
  (lambda (seq) (dylan-call dylan/copy-sequence seq))))

(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <vector>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (declare (ignore class))
    (if (true? (dylan-call dylan/instance? collection <vector>))
        collection
        (dylan-call dylan/as <simple-object-vector> collection)))))

(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <simple-object-vector>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (declare (ignore class))
    (if (true? (dylan-call dylan/instance? collection <simple-object-vector>))
        collection
        (let* ((size (collection-size collection))
               (new-vector (make-array size)))
          (do ((state (initial-state collection)
                      (next-state collection state))
               (index 0 (+ index 1)))
              ((not state) new-vector)
            (setf (aref new-vector index)
                  (current-element collection state))))))))

(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <stretchy-vector>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (declare (ignore class))
    (if (true? (dylan-call dylan/instance? collection <stretchy-vector>))
        collection
        (let* ((size (collection-size collection))
               (new-s-vector (dylan-call dylan/make <stretchy-vector>
                                         :size size))
               (vector-value (dylan-call dylan/get-array-value new-s-vector)))
          (do ((state (initial-state collection)
                      (next-state collection state))
               (index 0 (+ index 1)))
              ((not state) new-s-vector)
            (setf (aref vector-value index)
                  (current-element collection state))))))))

;;;
;;; VECTOR SPECIALIZED MAKE
;;; <vector> ... like the book says, this yields a <simple-object-vector>
;;;
(add-method
 dylan/make
 (dylan%function->method
  (make-param-list `((vector ,(dylan%make-singleton <vector>))) nil nil t)
  (lambda (class &rest rest)
    (declare (ignore class))
    (dylan-apply dylan/make <simple-object-vector> rest))))

;;;
;;; SIMPLE-OBJECT-VECTOR SPECIALIZED MAKE
;;; <simple-object-vector> generates a Lisp vector
;;;
(add-method
 dylan/make
 (dylan%dylan-callable->method
  (make-param-list `((sov ,(dylan%make-singleton <simple-object-vector>)))
                   nil nil '(:size :fill))
  (lambda (multiple-values next-method class &rest rest)
    (declare (ignore multiple-values class))
    (dylan%keyword-validate next-method rest '(:size :fill))
    (let ((size (dylan%find-keyword rest :size (lambda () 0)))
          (fill (dylan%find-keyword rest :fill (lambda () dylan%false))))
      (when (or (not (integerp size)) (minusp size))
        (dylan-call
         dylan/error
         "(make (singleton <simple-object-vector>)) -- size: invalid" size))
      (make-array size :initial-element fill)))))

;;;
;;; STRETCHY-VECTOR SPECIALIZED MAKE
;;; <stretchy-vector> has one slot, for the vector itself.  I'm using the
;;; slot that is inherited from <array> for this purpose.  Dimensions here
;;; is a list of one number.
;;;
(add-method
 dylan/make
 (dylan%dylan-callable->method
  (make-param-list `((sv ,(dylan%make-singleton <stretchy-vector>)))
                   nil nil '(:size :fill))
  (lambda (multiple-values next-method class &rest rest)
    (declare (ignore multiple-values class))
    (dylan%keyword-validate next-method rest '(:size :fill))
    (let ((size (dylan%find-keyword rest :size (lambda () 0)))
          (fill (dylan%find-keyword rest :fill (lambda () dylan%false))))
      (when (or (not (integerp size)) (minusp size))
        (dylan-call dylan/error
                    "(make (singleton <stretchy-vector>)) size: invalid" size))
      (let ((instance (dylan%make-<object> <stretchy-vector>)))
        (dylan-call dylan/set-array-value! instance
                    (make-array size :initial-element fill))
        (dylan-call dylan/set-array-dimensions! instance (list size))
        instance)))))

;;;
;;; Functions for collections
;;;
(add-method
 dylan/size
 (one-arg 'sov <vector>
          (lambda (vect) (length (vector-value vect)))))

;;;
;;; Functions for sequences
;;;
(add-method
 dylan/add
 (dylan%function->method
  one-vector-and-an-object
  (lambda (vector new-element)
    (let ((new-vector (dylan-call dylan/make <vector>))
          (size (car (dylan-call dylan/get-array-dimensions vector))))
      (dylan-call dylan/set-array-value! new-vector
                  (coerce (cons new-element (coerce vector 'list))
                          'simple-vector))
      (dylan-call dylan/set-array-dimensions! new-vector (list (+ size 1)))
      new-vector))))

(add-method
 dylan/add
 (dylan%function->method
  one-simple-object-vector-and-an-object
  (lambda (sov new-element)
    (coerce (cons new-element (coerce sov 'list)) 'simple-vector))))

(add-method
 dylan/add
 (dylan%function->method
  one-stretchy-vector-and-an-object
  (lambda (s-vector new-element)
    (let ((new-vector (dylan-call dylan/make <stretchy-vector>))
          (size (car (dylan-call dylan/get-array-dimensions s-vector))))
      (dylan-call dylan/set-array-value! new-vector
                  (coerce (cons new-element
                                (coerce (dylan-call dylan/get-array-value
                                                    s-vector)
                                        'list))
                          'simple-vector))
      (dylan-call dylan/set-array-dimensions! new-vector (list (+ size 1)))
      new-vector))))

(add-method
 dylan/add!
 (dylan%function->method
  one-stretchy-vector-and-an-object
  ;; The Scheme copied the whole vector to add each element.  Here the
  ;; vector is made adjustable, with a fill pointer, the first time, and
  ;; then grows by doubling; LENGTH and the rest see only the elements.
  (lambda (s-vector new-element)
    (let ((vector (stretchy-value s-vector))
          (size (car (stretchy-dimensions s-vector))))
      (unless (and (adjustable-array-p vector)
                   (array-has-fill-pointer-p vector)
                   (= (fill-pointer vector) size))
        (setf vector (make-array size :adjustable t :fill-pointer size
                                      :initial-contents (subseq vector 0 size)))
        (set-stretchy-value! s-vector vector))
      (vector-push-extend new-element vector (max 8 size))
      (set-stretchy-dimensions! s-vector (list (+ size 1)))
      s-vector))))

(add-method
 dylan/concatenate
 (dylan%function->method
  (make-param-list `((sov ,<simple-object-vector>)) nil 'rest nil)
  (lambda (vector-1 &rest rest)
    (let ((result (coerce vector-1 'list)))
      (dolist (v (mapcar (lambda (seq)
                           (dylan-call dylan/as <simple-object-vector> seq))
                         rest)
                 (coerce result 'simple-vector))
        (setf result (append result (coerce v 'list))))))))

(add-method
 dylan/concatenate
 (dylan%function->method
  (make-param-list `((vector ,<vector>)) nil 'rest nil)
  ;; Concatenate as simple vectors, then convert to the first argument's
  ;; class-for-copy.  (The Scheme re-applied CONCATENATE to the same
  ;; arguments, which dispatched straight back here.)
  (lambda (vector-1 &rest rest)
    (dylan-call dylan/as (dylan-call dylan/class-for-copy vector-1)
                (dylan-call dylan/apply dylan/concatenate
                            (dylan-call dylan/as <simple-object-vector>
                                        vector-1)
                            rest)))))

(add-method
 dylan/remove!
 (dylan%dylan-callable->method
  (make-param-list `((stretchy-vector ,<stretchy-vector>) (value ,<object>))
                   nil nil '(:test :count))
  (lambda (multiple-values next-method s-vector value &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test :count))
    (let* ((test? (dylan%find-keyword rest :test (lambda () dylan/id?)))
           (count (dylan%find-keyword
                   rest :count
                   (lambda ()
                     (car (dylan-call dylan/get-array-dimensions s-vector)))))
           (old-vector (dylan-call dylan/get-array-value s-vector))
           (new-vector (dylan-call dylan/remove old-vector value
                                   :test test? :count count)))
      (dylan-call dylan/set-array-value! s-vector new-vector)
      (dylan-call dylan/set-array-dimensions! s-vector
                  (list (length new-vector)))
      s-vector))))

(add-method
 dylan/remove-duplicates!
 (dylan%dylan-callable->method
  (make-param-list `((stretchy-vector ,<stretchy-vector>)) nil nil '(:test))
  (lambda (multiple-values next-method s-vector &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test))
    (let ((test? (dylan%find-keyword rest :test (lambda () dylan/id?))))
      (let ((new-vector (dylan-call dylan/remove-duplicates
                                    (dylan-call dylan/get-array-value s-vector)
                                    :test test?)))
        (dylan-call dylan/set-array-value! s-vector new-vector)
        (dylan-call dylan/set-array-dimensions! s-vector
                    (list (length new-vector)))
        s-vector)))))

(add-method
 dylan/reverse
 (dylan%function->method
  one-simple-object-vector
  (lambda (vector-1)
    (let ((result (make-array (length vector-1))))
      (do ((from (- (length vector-1) 1) (- from 1))
           (to 0 (+ to 1)))
          ((< from 0) result)
        (setf (aref result to) (aref vector-1 from)))))))

(add-method
 dylan/reverse
 (dylan%function->method
  one-stretchy-vector
  (lambda (s-vector)
    (let* ((vector-1 (dylan-call dylan/get-array-value s-vector))
           (result (make-array (length vector-1)))
           (result-s-vector (dylan-call dylan/make <stretchy-vector>)))
      (do ((from (- (length vector-1) 1) (- from 1))
           (to 0 (+ to 1)))
          ((< from 0) result)
        (setf (aref result to) (aref vector-1 from)))
      (dylan-call dylan/set-array-value! result-s-vector result)
      (dylan-call dylan/set-array-dimensions! result-s-vector
                  (list (length result)))
      result-s-vector))))

(add-method
 dylan/reverse!
 (dylan%function->method
  one-simple-object-vector
  (lambda (vector-1)
    (do ((from (- (length vector-1) 1) (- from 1))
         (to 0 (+ to 1)))
        ((<= from to) vector-1)
      (rotatef (aref vector-1 to) (aref vector-1 from))))))

(add-method
 dylan/reverse!
 (dylan%function->method
  one-stretchy-vector
  (lambda (s-vector)
    (let ((vector-1 (dylan-call dylan/get-array-value s-vector)))
      (do ((from (- (length vector-1) 1) (- from 1))
           (to 0 (+ to 1)))
          ((<= from to) s-vector)
        (rotatef (aref vector-1 to) (aref vector-1 from)))))))

(add-method
 dylan/sort!
 (dylan%dylan-callable->method
  (make-param-list `((stretchy-vector ,<stretchy-vector>))
                   nil nil '(:test :stable))
  (lambda (multiple-values next-method s-vector &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test :stable))
    (let ((test? (dylan%find-keyword rest :test (lambda () dylan/<))))
      (dylan%find-keyword rest :stable (lambda () nil)) ; Ignored
      (dylan-call dylan/set-array-value!
                  s-vector
                  (dylan-call dylan/as <simple-object-vector>
                              (scheme-sort (dylan-call dylan/as <pair> s-vector)
                                           (lambda (x y)
                                             (true? (dylan-call test? x y))))))
      s-vector))))                      ; Not the internal simple vector

(defparameter dylan/vector
  (dylan%function->method
   (make-param-list '() nil 'rest-args nil)
   (lambda (&rest args) (coerce args 'simple-vector))))

(add-method
 dylan/current-key
 (dylan%function->method
  (make-param-list `((vector ,<vector>) (state ,<object>)) nil nil nil)
  (lambda (vector state) (declare (ignore vector)) (aref state 0))))

;;;
;;; Collection Keys
;;;

(add-method
 dylan/element
 (dylan%dylan-callable->method
  (make-param-list `((vector ,<vector>) (index ,<integer>))
                   nil nil '(:default))
  (lambda (multiple-values next-method vector index &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:default))
    (let* ((vector-value (vector-value vector))
           (size (length vector-value)))
      (if (and (>= index 0) (< index size))
          (aref vector-value index)
          ;; N.B. the Scheme looked for the LIST (default:) rather than the
          ;; keyword itself, so the default: value was never found; fixed.
          (dylan%find-keyword
           rest :default
           (lambda ()
             (dylan-call
              dylan/error
              "(element <vector> <integer>) -- invalid index with no default value"
              vector-value index))))))))

;;;
;;; Mutable Collections
;;;

(add-method
 dylan/setter/current-element/
 (dylan%function->method
  (make-param-list `((sov ,<simple-object-vector>) (state ,<object>)
                     (new-value ,<object>))
                   nil nil nil)
  (lambda (sov state new-value)
    (setf (aref sov (aref state 0)) new-value)
    new-value)))

(add-method
 dylan/setter/current-element/
 (dylan%function->method
  (make-param-list `((stretchy-vector ,<stretchy-vector>) (state ,<object>)
                     (new-value ,<object>))
                   nil nil nil)
  (lambda (st-vector state new-value)
    (setf (aref (dylan-call dylan/get-array-value st-vector) (aref state 0))
          new-value)
    new-value)))

(add-method
 dylan/setter/element/
 (dylan%function->method
  (make-param-list `((vector ,<vector>) (index ,<object>)
                     (new-value ,<object>))
                   nil nil nil)
  ;; Chapter 12: it is an error to set an element whose key is not
  ;; already in the vector.
  (lambda (vector-instance index new-value)
    (let ((vector (vector-value vector-instance)))
      (unless (and (integerp index) (< -1 index (length vector)))
        (dylan-call dylan/error
                    "((setter element) <vector>) -- invalid index"
                    vector-instance index))
      (setf (aref vector index) new-value)
      new-value))))

;;; Design note 12: elements past the old size are what MAKE gives without
;;; fill:, #f.
(add-method
 dylan/setter/size/
 (dylan%function->method
  one-stretchy-vector-and-an-object
  (lambda (s-vector n)
    (check-new-size s-vector n)
    (let ((old (dylan-call dylan/get-array-value s-vector))
          (new (make-array n :initial-element dylan%false)))
      (replace new old)
      (dylan-call dylan/set-array-value! s-vector new)
      (dylan-call dylan/set-array-dimensions! s-vector (list n))
      n))))
