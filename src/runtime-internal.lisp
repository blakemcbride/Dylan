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
;;;; This file contains the functions that are referenced only directly by
;;;; the output of the Dylan to Lisp compiler, rather than by user code
;;;; written in Dylan.
;;;;                                (was Thomas's src/runtime-internal.scm)
;;;;
;;;; Many of the functions here are just renamings of ordinary Lisp
;;;; functions.  The renaming keeps the compiler's output recognisable and
;;;; insulates it from redefinition of the corresponding Dylan names.

(in-package #:dylan)

(defun dylan%free-variable-ref (value name)
  (if (eq value the-unassigned-value)
      (dylan-call dylan/error "unbound variable" name)
      value))

(defun dylan%call/cc (receiver)
  "Used for BIND-EXIT and HANDLER-CASE.  Dylan only ever escapes with its
continuations, so a CATCH/THROW pair suffices."
  (call-with-escape-continuation receiver))

(defun dylan%dotimes (count result-fn fn)
  ;; Used for the DOTIMES special form
  (loop for n from 0
        do (if (>= n count)
               (return (funcall result-fn))
               (funcall fn n))))

(defun dylan%while (test thunk)
  ;; Used for the UNTIL and WHILE special forms
  (loop while (funcall test) do (funcall thunk) finally (return nil)))

(defun dylan%apply (multiple-values? operator-thunk &rest operand-thunks)
  "Used for combinations.  Forces left-to-right evaluation, and adds an
initial NIL next-method argument."
  (let ((rands (mapcar #'funcall operand-thunks)))
    (apply (funcall operator-thunk)
           multiple-values?
           next-method/not-generic
           rands)))

(defun dylan%lisp-apply (fn &rest args)
  (apply #'apply fn args))

(defun dylan%dynamic-wind (before thunk after) ; Used for UNWIND-PROTECT
  (funcall before)
  (unwind-protect (funcall thunk) (funcall after)))

(defun dylan%type-check (value class)   ; Used for BIND
  ;; Should signal a <type-error>!
  (dylan-call dylan/check-type value class))

(defun dylan%list (&rest args) args)                    ; BIND
(defun dylan%cons (a b) (cons a b))                     ; BIND
(defun dylan%car (pair) (car pair))                     ; BIND
(defun dylan%vector (&rest args) (coerce args 'simple-vector)) ; BIND
(defun dylan%vector-ref (v i) (aref v i))               ; BIND
(defun dylan%not (x) (not x))           ; DEFINE-GENERIC-FUNCTION, UNLESS
(defun dylan%eq? (a b) (eq a b))                        ; DEFINE-GENERIC-FUNCTION
(defun dylan%class? (x) (class? x))                     ; DEFINE-CLASS
(defun dylan%make-param-list (required next? rest? keys) ; METHOD
  (make-param-list required next? rest? keys))
(defun dylan%add-method (generic-function method)       ; DEFINE-METHOD
  (add-method generic-function method))
(defun dylan%null? (x) (null x))                        ; COND

(defun dylan%for-each (fn &rest collections)
  (dolist (collection collections)
    (unless (subclass? (get-type collection) <collection>)
      (dylan-call dylan/error "for-each -- not a collection" collection)))
  (collections-iterate fn
                       (lambda (result)
                         (and result (lambda () (car result))))
                       nil
                       collections))
