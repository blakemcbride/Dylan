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
;;;; This file contains all the specializations for the list, pair, and
;;;; empty-list types.       (was Thomas's src/runtime-collections-list.scm)

(in-package #:dylan)

(add-method
 dylan/binary=
 (dylan%function->method
  two-lists
  (lambda (list-1 list-2)
    (let ((size-1 (dylan-call dylan/size list-1))
          (size-2 (dylan-call dylan/size list-2)))
      (if (not (= size-1 size-2))
          nil
          (do ((state-1 (dylan-call dylan/initial-state list-1)
                        (dylan-call dylan/next-state list-1 state-1))
               (state-2 (dylan-call dylan/initial-state list-2)
                        (dylan-call dylan/next-state list-2 state-2)))
              ((or (or (not state-1) (not state-2))
                   (not (dylan-call dylan/id?
                                    (dylan-call dylan/current-element
                                                list-1 state-1)
                                    (dylan-call dylan/current-element
                                                list-2 state-2))))
               (if (or state-1 state-2) nil t))))))))

(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <list>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (declare (ignore class))
    (if (dylan-call dylan/instance? collection <list>)
        collection
        (iterate->list #'identity collection)))))

(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <pair>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (declare (ignore class))
    (if (dylan-call dylan/instance? collection <pair>)
        collection
        (iterate->list #'identity collection)))))

;;;
;;; LIST SPECIALIZED MAKE
;;; supports size: and fill:
;;;
(add-method
 dylan/make
 (dylan%dylan-callable->method
  (make-param-list `((list ,(dylan%make-singleton <list>)))
                   nil nil '(:size :fill))
  (lambda (multiple-values next-method class &rest rest)
    (declare (ignore multiple-values class))
    (dylan%keyword-validate next-method rest '(:size :fill))
    (let ((size (dylan%find-keyword rest :size (lambda () 0)))
          (fill (dylan%find-keyword rest :fill (lambda () nil))))
      (when (or (not (integerp size)) (minusp size))
        (dylan-call dylan/error "make -- list size: invalid" size))
      (make-list size :initial-element fill)))))

;;;
;;; PAIR SPECIALIZED MAKE
;;;
(add-method
 dylan/make
 (dylan%dylan-callable->method
  (make-param-list `((pair ,(dylan%make-singleton <pair>)))
                   nil nil '(:size :fill))
  (lambda (multiple-values next-method class &rest rest)
    (declare (ignore multiple-values class))
    (dylan%keyword-validate next-method rest '(:size :fill))
    (let ((size (dylan%find-keyword rest :size (lambda () 0)))
          (fill (dylan%find-keyword rest :fill (lambda () nil))))
      (when (or (not (integerp size)) (minusp size))
        (dylan-call dylan/error "make -- list size invalid" size))
      (make-list size :initial-element fill)))))

;;;
;;; EMPTY-LIST SPECIALIZED MAKE
;;;
(add-method
 dylan/make
 (dylan%dylan-callable->method
  (make-param-list `((empty-list ,(dylan%make-singleton <empty-list>)))
                   nil nil '(:size :fill))
  (lambda (multiple-values next-method class &rest rest)
    (declare (ignore multiple-values class))
    (dylan%keyword-validate next-method rest '(:size :fill))
    '())))

;;;
;;; FUNCTIONS FOR COLLECTIONS (page 99)
;;;
(add-method
 dylan/size
 (one-arg 'list <list>
          (lambda (list)
            (let ((length (dylan-list-length list)))
              (if (= length -1) nil length)))))

(add-method dylan/size
            (one-arg 'pair <empty-list>
                     (lambda (pair) (declare (ignore pair)) 0)))

(add-method
 dylan/member?
 (dylan%dylan-callable->method
  (make-param-list `((value ,<object>) (collection ,<list>))
                   nil nil '(:test))
  (lambda (multiple-values next-method value list &rest keys)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method keys '(:test))
    (let ((test (dylan%find-keyword keys :test (lambda () dylan/id?))))
      (loop for tail = list then (cdr tail)
            do (cond ((null tail) (return nil))
                     ((dylan-call test value (car tail)) (return tail))))))))

;;;;
;;;; Functions for Sequences (page 104)
;;;;

(add-method
 dylan/add
 (dylan%function->method one-list-and-an-object
                         (lambda (the-list new-element)
                           (cons new-element the-list))))

(add-method
 dylan/add!
 (dylan%function->method one-list-and-an-object
                         (lambda (list-1 val)
                           ;; Like the book (p. 117)
                           (dylan-call dylan/cons val list-1))))

(add-method
 dylan/concatenate
 (dylan%function->method
  at-least-one-list
  (lambda (list-1 &rest rest)
    (let ((rest-lists (mapcar (lambda (seq) (dylan-call dylan/as <pair> seq))
                              rest)))
      (apply #'append list-1 rest-lists)))))

(add-method dylan/reverse (dylan%function->method one-list #'reverse))

(add-method
 dylan/first
 (dylan%function->method
  one-list
  (lambda (list)
    (if (null list)
        (dylan-call dylan/error "first -- list is empty" list)
        (car list)))))

(add-method
 dylan/second
 (dylan%function->method
  one-list
  (lambda (list)
    (if (or (null list) (null (cdr list)))
        (dylan-call dylan/error "second -- list doesn't have 2 elements" list)
        (cadr list)))))

(add-method
 dylan/third
 (dylan%function->method
  one-list
  (lambda (list)
    (if (or (null list) (null (cdr list)) (null (cddr list)))
        (dylan-call dylan/error "third -- list doesn't have 3 elements" list)
        (caddr list)))))

(add-method
 dylan/last
 (dylan%function->method
  one-list
  (lambda (list)
    (if (null list)
        (dylan-call dylan/error "last -- list is empty")
        (last list)))))

;;;;
;;;; Operations on LISTS (page 115)
;;;;

(defparameter dylan/cons (dylan%function->method two-objects #'cons))

(defparameter dylan/list (dylan%function->method only-rest-args #'list))

(defparameter dylan/car
  (dylan%function->method one-list
                          (lambda (list) (if (null list) '() (car list)))))

(defparameter dylan/cdr
  (dylan%function->method one-list
                          (lambda (list) (if (null list) '() (cdr list)))))

;;; The c[ad]+r family and their setters are all defined in terms of
;;; DYLAN/CAR and DYLAN/CDR, exactly as in the Scheme.

(macrolet
    ((define-cxr (name &rest operations)
       "OPERATIONS are applied right to left, as in the name."
       `(defparameter ,name
          (dylan%function->method
           one-list
           (lambda (list)
             ,(cl:reduce (lambda (form op)
                           `(dylan-call ,(if (eq op 'a) 'dylan/car 'dylan/cdr)
                                        ,form))
                         (reverse operations)
                         :initial-value 'list)))))
     (define-cxr-setter (name accessor &rest operations)
       `(defparameter ,name
          (dylan%function->method
           one-list-and-an-object
           (lambda (list object)
             (setf (,accessor ,(cl:reduce
                                (lambda (form op)
                                  `(dylan-call ,(if (eq op 'a)
                                                    'dylan/car 'dylan/cdr)
                                               ,form))
                                (reverse operations)
                                :initial-value 'list))
                   object)
             object)))))

  (define-cxr dylan/caar a a)
  (define-cxr dylan/cadr a d)
  (define-cxr dylan/cdar d a)
  (define-cxr dylan/cddr d d)
  (define-cxr dylan/caaar a a a)
  (define-cxr dylan/caadr a a d)
  (define-cxr dylan/cadar a d a)
  (define-cxr dylan/caddr a d d)
  (define-cxr dylan/cdaar d a a)
  (define-cxr dylan/cdadr d a d)
  (define-cxr dylan/cddar d d a)
  (define-cxr dylan/cdddr d d d)

  (define-cxr-setter dylan/setter/car/ car)
  (define-cxr-setter dylan/setter/cdr/ cdr)
  (define-cxr-setter dylan/setter/caar/ car a)
  (define-cxr-setter dylan/setter/cadr/ car d)
  (define-cxr-setter dylan/setter/cdar/ cdr a)
  (define-cxr-setter dylan/setter/cddr/ cdr d)
  (define-cxr-setter dylan/setter/caaar/ car a a)
  (define-cxr-setter dylan/setter/caadr/ car a d)
  (define-cxr-setter dylan/setter/cadar/ car d a)
  (define-cxr-setter dylan/setter/caddr/ car d d)
  (define-cxr-setter dylan/setter/cdaar/ cdr a a)
  (define-cxr-setter dylan/setter/cdadr/ cdr a d)
  (define-cxr-setter dylan/setter/cddar/ cdr d a)
  (define-cxr-setter dylan/setter/cdddr/ cdr d d))

(defparameter dylan/list*
  (dylan%function->method
   only-rest-args
   (labels ((list-star (arg &rest args)
              (if (null args)
                  arg
                  (cons arg (apply #'list-star args)))))
     (lambda (&rest args)
       (if (null args)
           '()                          ; Very peculiar ?
           (apply #'list-star args))))))

(defparameter dylan/append
  (dylan%generic-fn 'append at-least-one-list #'append))

(defparameter dylan/find-pair
  (dylan%generic-fn
   'find-pair
   (make-param-list `((item ,<object>) (list ,<list>)) nil nil '(:test))
   nil))

(add-method
 dylan/find-pair
 (dylan%dylan-callable->method
  (make-param-list `((item ,<object>) (list ,<list>)) nil nil '(:test))
  (lambda (multiple-values next-method item list &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:test))
    (let ((test-fn (dylan%find-keyword rest :test (lambda () dylan/id?))))
      (loop for rest-list = list then (cdr rest-list)
            do (cond ((null rest-list) (return nil))
                     ;; N.B. the Scheme tested (car LIST) rather than
                     ;; (car REST-LIST) here.  Preserved as it was.
                     ((not (consp (car list)))
                      (return (dylan-call dylan/error
                                          "find-pair -- list element not a pair"
                                          (car rest-list))))
                     ((dylan-call test-fn item (caar rest-list))
                      (return (car rest-list)))))))))

;;;
;;; Collection Keys
;;;

(add-method
 dylan/element
 (dylan%dylan-callable->method
  (make-param-list `((list ,<list>) (index ,<integer>)) nil nil '(:default))
  (lambda (multiple-values next-method list-1 index &rest rest)
    (declare (ignore multiple-values))
    (dylan%keyword-validate next-method rest '(:default))
    (let ((size (length list-1)))
      (if (and (>= index 0) (< index size))
          (nth index list-1)
          (dylan%find-keyword
           rest :default
           (lambda ()
             (dylan-call dylan/error
                         "element -- invalid index with no default value"
                         list-1 index))))))))

;;;
;;; Mutable Collection
;;;

(add-method
 dylan/setter/current-element/
 (dylan%function->method
  (make-param-list `((list ,<list>) (state ,<object>) (new-value ,<object>))
                   nil nil nil)
  (lambda (list state new-value)
    (declare (ignore list))
    (setf (car state) new-value)
    new-value)))

(add-method
 dylan/setter/current-element/
 (dylan%function->method
  (make-param-list `((empty-list ,<empty-list>) (state ,<object>)
                     (new-value ,<object>))
                   nil nil nil)
  (lambda (empty-list state new-value)
    (dylan-call dylan/error
                "setter/current-element/ for <empty-list>: not possible"
                empty-list state new-value))))
