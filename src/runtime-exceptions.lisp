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
;;;; The Dylan exception system.      (was Thomas's src/runtime-exceptions.scm)
;;;;
;;;; Note: functions starting with "implementation-specific/" abstract over
;;;; the host Lisp's condition system.  They live in compat.lisp, which is
;;;; where the Scheme's "mit-specific.scm" and friends ended up.

(in-package #:dylan)

;;; Classes

(defparameter dylan/condition-format-string
  (dylan%generic-fn
   'condition-format-string
   (make-param-list `((condition ,<condition>)) nil nil nil)
   (lambda (condition)
     (dylan-call dylan/error
                 "(condition-format-string <condition>) -- no specialization"
                 condition))))

(defparameter dylan/condition-format-arguments
  (dylan%generic-fn
   'condition-format-arguments
   (make-param-list `((condition ,<condition>)) nil nil nil)
   (lambda (condition)
     (dylan-call dylan/error
                 "(condition-format-arguments <condition>) -- no specialization"
                 condition))))

(dylan%add-slot <simple-error>
                nil 'instance nil dylan/condition-format-string
                'condition-format-string nil nil nil :format-string nil)
(dylan%add-slot <simple-error>
                nil 'instance nil dylan/condition-format-arguments
                'condition-format-arguments nil nil nil :format-arguments nil)

(defparameter dylan/type-error-value
  (dylan%generic-fn
   'type-error-value
   (make-param-list `((condition ,<condition>)) nil nil nil)
   (lambda (condition)
     (dylan-call dylan/error
                 "(type-error-value <condition>) -- no specialization"
                 condition))))

(defparameter dylan/type-error-expected-type
  (dylan%generic-fn
   'type-error-expected-type
   (make-param-list `((condition ,<condition>)) nil nil nil)
   (lambda (condition)
     (dylan-call dylan/error
                 "(type-error-expected-type <condition>) -- no specialization"
                 condition))))

(dylan%add-slot <type-error>
                nil 'instance nil dylan/type-error-value 'type-error-value
                nil nil nil :value nil)
(dylan%add-slot <type-error>
                nil 'instance nil dylan/type-error-expected-type
                'type-error-expected-type nil nil nil :type nil)

(dylan%add-slot <simple-warning>
                nil 'instance nil dylan/condition-format-string
                'condition-format-string nil nil nil :format-string nil)
(dylan%add-slot <simple-warning>
                nil 'instance nil dylan/condition-format-arguments
                'condition-format-arguments nil nil nil :format-arguments nil)

;;; Basic Operators (pages 138 and 139)

(defun dylan%handler-bind (type function test description thunk)
  ;; Assumes FUNCTION is a method of two args, TEST is a method of one arg,
  ;; and DESCRIPTION is a string or method of one argument.  Can't check???
  (when (and (not (and (class? type) (subclass? type <condition>)))
             (not (and (singleton? type)
                       (subclass? (get-type (singleton.object type))
                                  <condition>))))
    (dylan-call dylan/error "handler-bind -- not a <condition>" type))
  (implementation-specific/push-handler type function test description thunk))

(defun make-default-condition (args type operator)
  "Handles one arg (a condition) or many args (format string and format
args).  Assumes TYPE is a <simple-error> or <simple-warning>."
  (cond ((and (consp args)
              (subclass? (get-type (car args)) <condition>))
         (if (null (cdr args))
             (car args)
             (dylan-call dylan/error "extraneous args" operator (cdr args))))
        ((and (consp args) (stringp (car args)))
         (dylan-call dylan/make type
                     :format-string (car args)
                     :format-arguments (cdr args)))
        (t (dylan-call dylan/error "bad first argument" operator args))))

(defparameter dylan/signal
  (lambda (multiple-values next-method &rest args)
    (declare (ignore next-method))
    (let* ((condition (make-default-condition args <simple-warning> 'signal))
           (condition-type (get-type condition)))
      (labels ((frame-loop (frames)
                 (if (consp frames)
                     (let ((handler-type (caar frames))
                           (handler-test (caddar frames)))
                       (if (and (or (and (singleton? handler-type)
                                         (eq condition
                                             (singleton.object handler-type)))
                                    (subclass? condition-type handler-type))
                                (dylan-call handler-test condition))
                           (let ((handler (cadar frames)))
                             (dylan-mv-call
                              handler multiple-values condition
                              (lambda (multiple-values next-method)
                                (declare (ignore multiple-values next-method))
                                (frame-loop (cdr frames)))))
                           (frame-loop (cdr frames))))
                     (dylan-mv-call dylan/default-handler multiple-values
                                    condition))))
        (frame-loop (implementation-specific/get-dylan-handler-frames))))))

;;; Full set of Operators for Signaling

(defparameter dylan/error            ; NOT continuable
  (make-dylan-callable
   (lambda (&rest args)
     (dylan-call dylan/signal
                 (make-default-condition args <simple-error> 'error))
     (dylan-call dylan/error "error -- attempt to return from error"))))

(defparameter dylan/cerror           ; OK to continue
  (make-dylan-callable
   (lambda (restart-description &rest others)
     (call-with-escape-continuation
      (lambda (continue)
        (dylan%handler-bind
         <simple-restart>
         (make-dylan-callable       ; Called if restart attempted
          (lambda (condition next-handler)
            (declare (ignore condition next-handler))
            (funcall continue nil)))
         (make-dylan-callable       ; Test: always ready to handle
          (lambda (condition) (declare (ignore condition)) t))
         restart-description
         (lambda () (dylan-apply dylan/error others))))))))

(defparameter dylan/break
  (make-dylan-callable
   (lambda (&rest args)
     (call-with-escape-continuation
      (lambda (continue)
        (dylan%handler-bind
         <simple-restart>
         (make-dylan-callable
          (lambda (condition next-handler)
            (declare (ignore condition next-handler))
            (funcall continue nil)))
         (make-dylan-callable
          (lambda (condition) (declare (ignore condition)) t))
         "Continue from breakpoint."
         (lambda ()
           (implementation-specific/enter-debugger
            (make-default-condition args <simple-error> 'break))
           nil)))))))

(defparameter dylan/check-type
  (make-dylan-callable
   (lambda (value type)
     (if (not (dylan-call dylan/instance? value type))
         (let ((condition (dylan-call dylan/make <type-error>
                                      :value value :type type)))
           (dylan-call dylan/signal condition))
         value))
   2))

(defparameter dylan/abort
  (lambda (multiple-values next-method)
    (dylan-full-call dylan/error multiple-values next-method
                     (dylan-call dylan/make <abort>))))

;;; Additional Operators for Handling

(defparameter dylan/default-handler
  (dylan%generic-fn
   'default-handler
   (make-param-list `((condition ,<condition>)) nil nil nil)
   (lambda (condition) (declare (ignore condition)) nil)))

(add-method
 dylan/default-handler
 (dylan%function->method
  (make-param-list `((condition ,<serious-condition>)) nil nil nil)
  (lambda (serious)
    ;; Turn an unhandled Dylan condition into a Lisp condition unless it is
    ;; a Dylan condition reflecting a Lisp condition, in which case just
    ;; return so that the handler in DYLAN%CATCH-ALL-CONDITIONS returns and
    ;; the remaining Lisp condition handlers may run.
    (let ((error-type (get-type serious)))
      (cond
        ((and (eq error-type <simple-error>)
              (implementation-specific/is-reflected-error?
               (dylan-call dylan/condition-format-string serious)
               (dylan-call dylan/condition-format-arguments serious)))
         (implementation-specific/let-lisp-handle-it serious))
        ((eq error-type <simple-error>)
         (implementation-specific/induce-error
          (dylan-call dylan/condition-format-string serious)
          (dylan-call dylan/condition-format-arguments serious)))
        ((eq error-type <type-error>)
         (let ((value (dylan-call dylan/type-error-value serious))
               (expected-type (dylan-call dylan/type-error-expected-type
                                          serious)))
           (implementation-specific/induce-type-error
            value (class.debug-name expected-type))))
        (t (implementation-specific/signal-unhandled-dylan-condition
            serious)))))))

(add-method
 dylan/default-handler
 (dylan%function->method
  (make-param-list `((condition ,<simple-warning>)) nil nil nil)
  (lambda (warning)
    (implementation-specific/warning
     (dylan-call dylan/condition-format-string warning)
     (dylan-call dylan/condition-format-arguments warning))
    nil)))

(add-method
 dylan/default-handler
 (dylan%function->method
  (make-param-list `((condition ,<restart>)) nil nil nil)
  (lambda (restart)
    (dylan-call dylan/error
                "(default-handler <restart>) -- no handler established"
                restart))))

;;; Operators for Interactive Handling

(defparameter dylan/restart-query
  (dylan%generic-fn 'restart-query
                    (make-param-list `((restart ,<restart>)) nil nil nil)
                    nil))

(add-method
 dylan/restart-query
 (dylan%dylan-callable->method
  (make-param-list `((restart ,<restart>)) nil nil nil)
  (lambda (multiple-values next-method restart)
    (declare (ignore restart))
    (dylan-full-call dylan/values multiple-values next-method))))

(defparameter dylan/return-query
  (dylan%generic-fn 'return-query
                    (make-param-list `((condition ,<condition>)) nil nil nil)
                    nil))

(add-method
 dylan/return-query
 (dylan%dylan-callable->method
  (make-param-list `((condition ,<condition>)) nil nil nil)
  (lambda (multiple-values next-method condition)
    (declare (ignore condition))
    (dylan-display "RETURN-QUERY: please type in a list of values")
    (dylan-newline)
    (dylan-full-apply dylan/values multiple-values next-method (dylan-read)))))

;;; Operators for Introspection

(defparameter dylan/do-handlers
  (make-dylan-callable
   (lambda (funarg)
     (dolist (frame (implementation-specific/get-dylan-handler-frames))
       (dylan-call funarg (car frame) (cadr frame)
                   (caddr frame) (cadddr frame)))
     ;; No value???
     nil)
   1))

(defparameter dylan/return-allowed?
  (dylan%generic-fn 'return-allowed?
                    (make-param-list `((condition ,<condition>)) nil nil nil)
                    (lambda (condition) (declare (ignore condition)) nil)))

(defparameter dylan/return-description
  (dylan%generic-fn
   'return-description
   (make-param-list `((condition ,<condition>)) nil nil nil)
   (lambda (condition)
     (dylan-call dylan/error
                 "(return-description <condition>) -- not specialized"
                 condition))))

;;; Top-level wrapper for DYLAN code

(defvar catch-errors? t)

(defun dylan%catch-all-conditions (dylan-compiled-output)
  (if catch-errors?
      (implementation-specific/catch-all-errors #'dylan%lisp-condition-handler
                                                dylan-compiled-output)
      (funcall dylan-compiled-output)))

;; A name for the condition handler...
(defun dylan%lisp-condition-handler (condition)
  (dylan-call
   dylan/signal
   (dylan-call dylan/make <simple-error>
               :format-string (implementation-specific/get-error-message
                               condition)
               :format-arguments (implementation-specific/get-error-arguments
                                  condition))))
