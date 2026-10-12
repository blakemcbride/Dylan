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
;;;; Utility procedures for the runtime system only.
;;;;                                     (was Thomas's src/runtime-top.scm)

(in-package #:dylan)

(defun get-type (obj)
  (cond ((instance? obj) (instance.class obj))
        ((numberp obj)
         (cond ((integerp obj) <integer>)
               ((rationalp obj) <ratio>)
               ((typep obj 'single-float) <single-float>)
               ((typep obj 'double-float) <double-float>)
               ((floatp obj) <extended-float>)
               (t <rectangular-complex>)))
        ((class? obj) <class>)
        ((singleton? obj) <singleton>)
        ((null obj) <empty-list>)
        ;; In Scheme #T is not a symbol; keep it an <object> rather than
        ;; letting the SYMBOLP clause below claim it.
        ((eq obj t) <object>)
        ((slot? obj) <slot-descriptor>)
        ((consp obj) <pair>)
        ;; Unlike Scheme, Common Lisp strings *are* vectors, so the string
        ;; test has to come first.
        ((stringp obj) <byte-string>)
        ((vectorp obj) <simple-object-vector>)
        ((characterp obj) <character>)
        ((functionp obj)
         (cond ((dylan%generic-function? obj) <generic-function>)
               ((dylan%method? obj) <method>)
               (t <function>)))
        ((keyword? obj) <keyword>)
        ((symbolp obj) <symbol>)
        (t <object>)))

(defun dylan-list-length (l)
  "Returns >= 0  for finite proper lists
            = -1 for infinite lists
            = (size + 1) for improper lists, where size is the \"proper
              list\" portion of the list"
  (labels ((phase-1 (l1 l2 n)
             (cond ((consp l1) (phase-2 (cdr l1) l2 (+ n 1)))
                   ((null l1) n)
                   (t (+ n 1))))
           (phase-2 (l1 l2 n)
             (cond ((eq l1 l2) -1)      ; Circular list.
                   ((consp l1) (phase-1 (cdr l1) (cdr l2) (+ n 1)))
                   ((null l1) n)
                   (t (+ n 1)))))
    (phase-1 l l 0)))

(defun dylan%keyword-validate (next-method arglist allowed)
  (cond ((functionp next-method)
         ;; Assume that the generic function has checked the content of ARGS.
         t)
        ;; The last method of a generic dispatch: the generic function has
        ;; checked ARGS against every applicable method's keywords, which
        ;; may include keywords this method does not take.  (The Scheme
        ;; checked again here against this method's alone, so a keyword
        ;; meant for a more specific method was rejected once that method
        ;; reached this one through NEXT-METHOD.)
        ((and (false? next-method)
              *prevalidated-keywords*
              (equal arglist (cdr *prevalidated-keywords*)))
         (setf *prevalidated-keywords* nil)
         t)
        (t
         (validate-keywords arglist allowed
                            (lambda (&rest args)
                              (dylan-apply dylan/error args))))))

;;;
;;; Common parameter lists for MAKE-PARAM-LIST
;;;

(defparameter only-rest-args (make-param-list '() nil 'rest-args nil))
(defparameter function-and-arguments
  (make-param-list `((function ,<function>)) nil 'rest-fns nil))
(defparameter procedure-and-at-least-one-collection
  (make-param-list `((procedure ,<function>) (collection ,<collection>))
                   nil 'rest nil))

;; one-<xxx>
(defparameter one-number (make-param-list `((number ,<number>)) nil nil nil))
(defparameter one-object (make-param-list `((object ,<object>)) nil nil nil))
(defparameter one-list (make-param-list `((list ,<list>)) nil nil nil))
(defparameter one-function (make-param-list `((function ,<function>)) nil nil nil))
(defparameter one-real (make-param-list `((real ,<real>)) nil nil nil))
(defparameter one-integer (make-param-list `((integer ,<integer>)) nil nil nil))
(defparameter one-class (make-param-list `((class ,<class>)) nil nil nil))
(defparameter one-slot (make-param-list `((slot ,<slot-descriptor>)) nil nil nil))
(defparameter one-char (make-param-list `((character ,<character>)) nil nil nil))
(defparameter one-string (make-param-list `((string ,<string>)) nil nil nil))
(defparameter one-byte-string
  (make-param-list `((byte-string ,<byte-string>)) nil nil nil))
(defparameter one-collection
  (make-param-list `((collection ,<collection>)) nil nil nil))
(defparameter one-vector (make-param-list `((vector ,<vector>)) nil nil nil))
(defparameter one-stretchy-vector
  (make-param-list `((stretchy-vector ,<stretchy-vector>)) nil nil nil))
(defparameter one-simple-object-vector
  (make-param-list `((simple-object-vector ,<simple-object-vector>)) nil nil nil))
(defparameter one-sequence (make-param-list `((sequence ,<sequence>)) nil nil nil))
(defparameter one-deque (make-param-list `((deque ,<deque>)) nil nil nil))
(defparameter one-range (make-param-list `((range ,<range>)) nil nil nil))
(defparameter one-table (make-param-list `((table ,<table>)) nil nil nil))

;; at-least-one-<xxx>
(defparameter at-least-one-number
  (make-param-list `((number ,<number>)) nil 'rest-args nil))
(defparameter at-least-one-function
  (make-param-list `((function ,<function>)) nil 'rest-fns nil))
(defparameter at-least-two-objects
  (make-param-list `((object-1 ,<object>) (object-2 ,<object>)) nil 'rest nil))
(defparameter at-least-one-real
  (make-param-list `((real ,<real>)) nil 'rest-real nil))
(defparameter at-least-one-list
  (make-param-list `((first-list ,<list>)) nil 'rest nil))
(defparameter at-least-one-sequence
  (make-param-list `((sequence ,<sequence>)) nil 'rest nil))

;; two-<xxx>
(defparameter two-objects
  (make-param-list `((object-1 ,<object>) (object-2 ,<object>)) nil nil nil))
(defparameter two-collections
  (make-param-list `((collection-1 ,<collection>) (collection-2 ,<collection>))
                   nil nil nil))
(defparameter two-sequences
  (make-param-list `((sequence-1 ,<sequence>) (sequence-2 ,<sequence>))
                   nil nil nil))
(defparameter two-numbers
  (make-param-list `((number-1 ,<number>) (number-2 ,<number>)) nil nil nil))
(defparameter two-reals
  (make-param-list `((real-1 ,<real>) (real-2 ,<real>)) nil nil nil))
(defparameter two-integers
  (make-param-list `((integer-1 ,<integer>) (integer-2 ,<integer>)) nil nil nil))
(defparameter two-ranges
  (make-param-list `((range-1 ,<range>) (range-2 ,<range>)) nil nil nil))
(defparameter two-lists
  (make-param-list `((list-1 ,<list>) (list-2 ,<list>)) nil nil nil))
(defparameter two-strings
  (make-param-list `((string-1 ,<string>) (string-2 ,<string>)) nil nil nil))
(defparameter two-tables
  (make-param-list `((table-1 ,<table>) (table-2 ,<table>)) nil nil nil))

;; one-<xxx>-and-one-<zzz>
(defparameter one-sequence-and-an-object
  (make-param-list `((sequence ,<sequence>) (object ,<object>)) nil nil nil))
(defparameter one-mutable-sequence-and-an-object
  (make-param-list `((mutable-sequence ,<mutable-sequence>) (object ,<object>))
                   nil nil nil))
(defparameter one-vector-and-an-object
  (make-param-list `((vector ,<vector>) (object ,<object>)) nil nil nil))
(defparameter one-simple-object-vector-and-an-object
  (make-param-list `((simple-object-vector ,<simple-object-vector>)
                     (object ,<object>))
                   nil nil nil))
(defparameter one-string-and-an-object
  (make-param-list `((string ,<string>) (object ,<object>)) nil nil nil))
(defparameter one-byte-string-and-an-object
  (make-param-list `((byte-string ,<byte-string>) (object ,<object>))
                   nil nil nil))
(defparameter one-list-and-an-object
  (make-param-list `((list ,<list>) (object ,<object>)) nil nil nil))
(defparameter one-stretchy-vector-and-an-object
  (make-param-list `((stretchy-vector ,<stretchy-vector>) (object ,<object>))
                   nil nil nil))
(defparameter one-collection-and-a-state
  (make-param-list `((collection ,<collection>) (state ,<object>)) nil nil nil))
(defparameter one-deque-and-a-value
  (make-param-list `((deque ,<deque>) (object ,<object>)) nil nil nil))
(defparameter one-range-and-an-object
  (make-param-list `((range ,<range>) (object ,<object>)) nil nil nil))
(defparameter one-deque-and-an-object
  (make-param-list `((deque ,<deque>) (object ,<object>)) nil nil nil))

;;; The file compiler.lisp contains the definition of
;;; DYLAN%LISP-NAMES-OF-PREDEFINED-NAMES, which must be kept up to date
;;; with the actual methods/classes/functions defined in the runtime system.
