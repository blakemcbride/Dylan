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
;;;; General Supporting Procedures            (was Thomas's src/support.scm)
;;;;
;;;; This file contains general support routines used at runtime by Dylan
;;;; procedures.  See also the files generic.lisp (generic operator
;;;; dispatch), class.lisp (class heterarchy), and runtime-*.lisp
;;;; (Dylan-callable operations written in Lisp).

(in-package #:dylan)

(defvar the-unassigned-value '<<the-unassigned-value>>)
(defvar *the-uninitialized-slot-value* (list '<<uninitialized>>))

(defvar next-method/not-generic dylan%false
  "Passed as the next-method parameter when not going through the generic
dispatch mechanism: chapter 10 says #next is then #f.")

(defvar next-method/last-applicable (make-symbol "LAST-APPLICABLE-METHOD")
  "Passed by generic dispatch as the next-method parameter of the last
applicable method.  A compiled method's #next is then #f, as for a direct
call, but the method knows its arguments were checked by the dispatch.")

(defvar *prevalidated-keywords* nil
  "Bound by generic dispatch around the call of the last applicable method,
when that method takes keywords, to the keyword arguments it has already
validated against all the applicable methods.  DYLAN%KEYWORD-VALIDATE
consumes it.")

;;; Create a new symbol from two strings (prefix and postfix) and an
;;; existing symbol or string.

(defun new-name (prefix name postfix)
  ;; Interned in DYLAN explicitly: NEW-NAME runs while Dylan code is being
  ;; compiled, and *PACKAGE* is then whatever the caller of the compiler
  ;; happened to be in.
  (intern (canonicalize-string-for-symbol
           (concatenate 'string
                        prefix
                        (if (stringp name) name (symbol-name name))
                        postfix))
          '#:dylan))

;;;; Slot descriptors

(defstruct (dylan-slot
            (:conc-name slot.)
            (:predicate slot?)
            (:print-object print-opaque-object)
            (:constructor make-slot (debug-name getter setter type
                                     init-value has-init-value? init-function
                                     init-keyword required-init-keyword
                                     allocation inherited? data-location)))
  debug-name                            ; Name, for debugging
  getter
  setter
  type
  init-value
  has-init-value?
  init-function
  init-keyword
  required-init-keyword
  allocation
  inherited?
  data-location                         ; Depends on allocation type:
                                        ;  Instance = offset in instance data
                                        ;  Class = (class-ptr . offset)
                                        ;  Each-Subclass = offset in class-data
                                        ;  Virtual = NIL
                                        ;  Constant = the value of the slot
  (filtered nil))                       ; Virtual only: the inherited slot
                                        ; it overrides, whose storage it
                                        ; keeps (a "filtered slot")

;;; The Scheme spelled this accessor SLOT.HAS-INITIAL-VALUE? but the record
;;; field HAS-INIT-VALUE?; keep the accessor name the rest of the system
;;; uses.
(declaim (inline slot.has-initial-value?))
(defun slot.has-initial-value? (slot) (slot.has-init-value? slot))
(defun (setf slot.has-initial-value?) (value slot)
  (setf (slot.has-init-value? slot) value))

;;;; Keywords and names
;;;
;;; Dylan's keywords (foo:) are read as Common Lisp keywords (:foo); see
;;; DYLAN-READ in compat.lisp.

(declaim (inline keyword?))
(defun keyword? (object) (keywordp object))

(defun dylan%find-keyword (keyword-list keyword default-fn)
  (loop for rest = keyword-list then (cddr rest)
        do (cond ((null rest) (return (funcall default-fn)))
                 ((eq keyword (car rest)) (return (cadr rest))))))

(defun validate-keywords (arglist allowed error)
  (loop for args = arglist then (cddr args)
        do (cond ((null args) (return t))
                 ((or (not (consp args))
                      (not (consp (cdr args)))
                      (not (keyword? (car args))))
                  (return (funcall error "incorrect keyword format"
                                   arglist allowed)))
                 ((not (or (eq allowed t) (member (car args) allowed)))
                  (return (funcall error "illegal keyword argument" (car args)
                                   (if (eq allowed t) 'any allowed)))))))

;;;; General utilities

(defun last (l)
  "The last ELEMENT of L, or () when L is empty."
  (if (null l)
      '()
      (loop for tail on l
            when (null (cdr tail)) return (car tail))))

(defun split-last (l continue)
  "Call CONTINUE with all but the last element of L, and with the final
one-element tail."
  (if (null l)
      (funcall continue '() '())
      (loop for previous = '() then (cons (car left) previous)
            for left on l
            when (null (cdr left))
              return (funcall continue (nreverse previous) left))))

(defun subset? (smaller larger)
  (every (lambda (x) (member x larger)) smaller))

(defun unique? (objects predicate)
  (loop for tail on objects
        never (funcall predicate (car tail) (cdr tail))))

(defun set-difference (main subtract predicate)
  (remove-if (lambda (x) (funcall predicate x subtract)) main))

(defun union (set1 set2 predicate)
  (let ((result set1))
    (dolist (elt set2 result)
      (unless (funcall predicate elt result)
        (push elt result)))))

(defun adjoin (elem set predicate)
  (if (funcall predicate elem set)
      set
      (cons elem set)))

(defun any? (fn l)
  (loop for x in l thereis (funcall fn x)))

(defun all? (fn l)
  (loop for x in l always (funcall fn x)))

(defun but-first (count list)
  (nthcdr count list))

(defun population->list (population)
  (map-over-population population #'identity))
