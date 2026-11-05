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
;;;; Language extensions.
;;;;
;;;; The Scheme kits each added a few Dylan-visible procedures of their
;;;; own.  These are the Common Lisp counterparts of the three the MIT kit
;;;; provided (see kits/MIT/README):
;;;;
;;;;   (PP <object>)               calls the Lisp pretty printer
;;;;     (defined in runtime-printing.lisp with the other printing functions).
;;;;   (LISP-VARIABLE <symbol>)    returns the value of a Lisp variable.
;;;;     Results are unpredictable unless the object has a type that
;;;;     corresponds to one of the non-procedural Dylan types.
;;;;   (LISP-PROCEDURE <symbol>)   returns the value of a Lisp variable and
;;;;     assumes (without checking) that it is applicable, converting it to
;;;;     a Dylan method.  From within Dylan it appears to take an
;;;;     arbitrary number of arguments, but issues an error if the number
;;;;     supplied doesn't match the number expected.
;;;;
;;;; The symbol naming the variable is read by the Dylan reader, so it is
;;;; interned in the DYLAN package; use LISP-VARIABLE for values this
;;;; system itself defines, since a fully qualified name is not available.

(in-package #:dylan)

(defparameter dylan/lisp-variable
  (make-dylan-callable
   (lambda (variable-name) (symbol-value variable-name))
   1))

(defparameter dylan/lisp-procedure
  (make-dylan-callable
   (lambda (variable-name)
     (make-dylan-callable (if (fboundp variable-name)
                              (symbol-function variable-name)
                              (symbol-value variable-name))))
   1))

;;;; Threads
;;;
;;; Threads are an extension of the compiler to C (user manual, chapter
;;; 16).  The dylan module here exports the same names, so that the two
;;; implementations' dylan modules agree, but there are no threads:
;;; calling a thread function, or making a thread object, is an error.
;;; The classes exist, so that instance? and the like answer.

(defun no-threads (name)
  (dylan%error (format nil "~a: threads exist only in compiled programs" name)))

(defparameter <thread> (make-dylan-class '<thread> (list <object>) '() nil))
(defparameter <lock> (make-dylan-class '<lock> (list <object>) '() nil))
(defparameter <recursive-lock> (make-dylan-class '<recursive-lock> (list <lock>) '() nil))
(defparameter <condition-variable>
  (make-dylan-class '<condition-variable> (list <object>) '() nil))
(defparameter <atomic-integer>
  (make-dylan-class '<atomic-integer> (list <object>) '() nil))
(defparameter <thread-error> (make-dylan-class '<thread-error> (list <error>) '() nil))

(dolist (class (list <thread> <recursive-lock> <condition-variable> <atomic-integer>))
  (setf (class.sealed? class) t))

(dolist (class (list <thread> <lock> <recursive-lock> <condition-variable>
                     <atomic-integer>))
  (add-method
   dylan/make
   (dylan%function->method
    (make-param-list `((class ,(dylan%make-singleton class))) nil nil :any)
    (lambda (class &rest rest)
      (declare (ignore class rest))
      (no-threads "make")))))

(defparameter dylan/thread-error-thread
  (dylan%generic-fn 'thread-error-thread
                    (make-param-list `((condition ,<object>)) nil nil nil)
                    nil))
(defparameter dylan/thread-error-condition
  (dylan%generic-fn 'thread-error-condition
                    (make-param-list `((condition ,<object>)) nil nil nil)
                    nil))
(dylan%add-slot <thread-error> nil 'instance nil dylan/thread-error-thread
                'thread-error-thread nil nil nil :thread nil)
(dylan%add-slot <thread-error> nil 'instance nil dylan/thread-error-condition
                'thread-error-condition nil nil nil :condition nil)

(defmacro define-no-thread-function (name &rest parameters)
  `(defparameter ,(intern (format nil "DYLAN/~a" name) '#:dylan)
     (dylan%function->method
      (make-param-list (list ,@(mapcar (lambda (p) `(list ',p <object>)) parameters))
                       nil nil nil)
      (lambda ,parameters
        (declare (ignore ,@parameters))
        (no-threads ,(string-downcase (symbol-name name)))))))

(define-no-thread-function make-thread function)
(define-no-thread-function join-thread thread)
(define-no-thread-function current-thread)
(define-no-thread-function thread-yield)
(define-no-thread-function acquire-lock lock)
(define-no-thread-function release-lock lock)
(define-no-thread-function wait-condition-variable variable lock)
(define-no-thread-function notify-condition-variable variable)
(define-no-thread-function notify-all-condition-variable variable)
(define-no-thread-function atomic-value atomic)
(define-no-thread-function atomic-add! atomic n)
(define-no-thread-function atomic-compare-and-swap! atomic old new)
