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
   nil))

(defparameter dylan/condition-format-arguments
  (dylan%generic-fn
   'condition-format-arguments
   (make-param-list `((condition ,<condition>)) nil nil nil)
   nil))

;;; A format string not given is empty, as in the compiled library, so
;;; that such a condition still has a message.
(dylan%add-slot <simple-error>
                nil 'instance nil dylan/condition-format-string
                'condition-format-string "" t nil :format-string nil)
(dylan%add-slot <simple-error>
                nil 'instance nil dylan/condition-format-arguments
                'condition-format-arguments '() t nil :format-arguments nil)

(defparameter dylan/type-error-value
  (dylan%generic-fn
   'type-error-value
   (make-param-list `((condition ,<condition>)) nil nil nil)
   nil))

(defparameter dylan/type-error-expected-type
  (dylan%generic-fn
   'type-error-expected-type
   (make-param-list `((condition ,<condition>)) nil nil nil)
   nil))

(dylan%add-slot <type-error>
                nil 'instance nil dylan/type-error-value 'type-error-value
                nil nil nil :value nil)
(dylan%add-slot <type-error>
                nil 'instance nil dylan/type-error-expected-type
                'type-error-expected-type nil nil nil :type nil)

;;; Chapter 13: the initialization argument condition: is accepted, and
;;; ignored, by <restart>.
(defparameter dylan/restart-condition
  (dylan%generic-fn 'restart-condition
                    (make-param-list `((restart ,<restart>)) nil nil nil)
                    nil))

(setf (gethash dylan/restart-condition *private-slot-getters*) t)
(dylan%add-slot <restart>
                nil 'instance nil dylan/restart-condition
                'restart-condition dylan%false t nil :condition nil)

(dylan%add-slot <simple-warning>
                nil 'instance nil dylan/condition-format-string
                'condition-format-string "" t nil :format-string nil)
(dylan%add-slot <simple-warning>
                nil 'instance nil dylan/condition-format-arguments
                'condition-format-arguments '() t nil :format-arguments nil)

;;; Basic Operators (pages 138 and 139)

(defun dylan%handler-bind (type function test description thunk)
  ;; Chapter 13: TYPE is a class or a singleton, FUNCTION a function, and
  ;; TEST a function (#f, as a compiled program takes it, for none); all
  ;; are evaluated before the forms are, and checked then, as a compiled
  ;; program checks them.
  (flet ((function-p (x) (true? (dylan-call dylan/instance? x <function>))))
    (unless (or (class? type) (singleton? type))
      (dylan-call dylan/error "handler-bind: not a class or singleton: ~s" type))
    (unless (function-p function)
      (dylan-call dylan/error "handler-bind: not a function: ~s" function))
    (cond ((eq test dylan%false)
           (setf test (make-dylan-callable
                       (lambda (condition) (declare (ignore condition)) t))))
          ((not (function-p test))
           (dylan-call dylan/error "handler-bind: the test: is not a function: ~s" test))))
  (implementation-specific/push-handler type function test description thunk))

(defun make-default-condition (args type operator)
  "Handles one arg (a condition) or many args (format string and format
args).  Assumes TYPE is a <simple-error> or <simple-warning>."
  (let ((name (string-downcase (symbol-name operator))))
    (cond ((null args)
           (dylan-call dylan/error "wrong number of arguments (0)"))
          ((subclass? (get-type (car args)) <condition>)
           (if (null (cdr args))
               (car args)
               (dylan-call dylan/error
                           (concatenate 'string name
                                        ": arguments after a condition: ~s")
                           (cdr args))))
          ((stringp (car args))
           (dylan-call dylan/make type
                       :format-string (car args)
                       :format-arguments (cdr args)))
          (t (dylan-call dylan/error
                         (concatenate 'string name
                                      ": not a condition or a string: ~s")
                         (car args))))))

(defvar *running-handlers* '()
  "The frames from the innermost handler running (DYLAN/SIGNAL).")

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
                                (true? (dylan-call handler-test condition)))
                           (let ((handler (cadar frames)))
                             ;; A handler running holds a binding (its
                             ;; frames), so one that signals without end
                             ;; exhausts the binding stack, as it exhausts
                             ;; a compiled program's stack.
                             (dylan%check-binding-stack)
                             (let ((*running-handlers* frames))
                               (dylan-mv-call
                                handler multiple-values condition
                                (lambda (multiple-values next-method)
                                  (declare (ignore multiple-values next-method))
                                  (frame-loop (cdr frames))))))
                           (frame-loop (cdr frames))))
                     (dylan-mv-call dylan/default-handler multiple-values
                                    condition))))
        (frame-loop (implementation-specific/get-dylan-handler-frames))))))

;;; Full set of Operators for Signaling

(defparameter dylan/error            ; NOT continuable
  (make-dylan-callable
   (lambda (&rest args)
     (let ((condition (make-default-condition args <simple-error> 'error)))
       (dylan-call dylan/signal condition)
       (implementation-specific/return-from-error condition)))))

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
            (funcall continue dylan%false)))
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
            (funcall continue dylan%false)))
         (make-dylan-callable
          (lambda (condition) (declare (ignore condition)) t))
         "Continue from breakpoint."
         (lambda ()
           ;; Chapter 13: the condition is made as SIGNAL makes one, and
           ;; with no arguments a default message is used.
           (implementation-specific/enter-debugger
            (make-default-condition (or args (list "break"))
                                    <simple-warning> 'break))
           dylan%false)))))))

(defparameter dylan/check-type
  (make-dylan-callable
   (lambda (value type)
     (if (false? (dylan-call dylan/instance? value type))
         ;; Chapter 13: check-type signals an error, which a handler may
         ;; not return from.
         (let ((condition (dylan-call dylan/make <type-error>
                                      :value value :type type)))
           (dylan-call dylan/error condition))
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
   (lambda (condition) (declare (ignore condition)) dylan%false)))

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
           (implementation-specific/induce-type-error value expected-type)))
        (t (implementation-specific/signal-unhandled-dylan-condition
            serious)))))))

(add-method
 dylan/default-handler
 (dylan%function->method
  ;; Chapter 13: the default handler for <warning> displays the warning
  ;; and returns #f.
  (make-param-list `((condition ,<warning>)) nil nil nil)
  (lambda (warning)
    ;; As display prints it, through its class's print-object
    (implementation-specific/warning "~a" (list warning))
    dylan%false)))

(add-method
 dylan/default-handler
 (dylan%function->method
  (make-param-list `((condition ,<restart>)) nil nil nil)
  (lambda (restart)
    (dylan-call dylan/error
                "no handler was established for the restart"
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
    ;; Nothing to ask, as in a compiled program
    (dylan-full-call dylan/values multiple-values next-method dylan%false))))

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
    (if *batch*
        ;; No one to ask: the program's next forms are not the answer.
        ;; As in a compiled program, #f.
        (dylan-full-call dylan/values multiple-values next-method dylan%false)
        (progn
          (dylan-display "RETURN-QUERY: please type in a list of values")
          (dylan-newline)
          (dylan-full-apply dylan/values multiple-values next-method (dylan-read)))))))

;;; Operators for Introspection

(defparameter dylan/do-handlers
  (make-dylan-callable
   (lambda (funarg)
     ;; Chapter 13: the arguments are type, test, function and description.
     ;; A frame holds them as (type function test description).
     (dolist (frame (implementation-specific/get-dylan-handler-frames))
       (destructuring-bind (type function test description) frame
         (dylan-call funarg type test function description)))
     dylan%false)
   1))

(defparameter dylan/return-allowed?
  (dylan%generic-fn 'return-allowed?
                    (make-param-list `((condition ,<condition>)) nil nil nil)
                    (lambda (condition) (declare (ignore condition)) dylan%false)))

(defparameter dylan/return-description
  (dylan%generic-fn
   'return-description
   (make-param-list `((condition ,<condition>)) nil nil nil)
   (lambda (condition) (declare (ignore condition)) dylan%false)))

;;; Chapter 13: a warning's recovery protocol is that any value can be
;;; returned and will be ignored.
(add-method dylan/return-allowed?
            (dylan%function->method
             (make-param-list `((condition ,<warning>)) nil nil nil)
             (lambda (condition) (declare (ignore condition)) t)))

(add-method dylan/return-description
            (dylan%function->method
             (make-param-list `((condition ,<warning>)) nil nil nil)
             (lambda (condition)
               (declare (ignore condition))
               "Return; the value is ignored.")))

;;; Top-level wrapper for DYLAN code

(defvar catch-errors? t)

(defun dylan%catch-all-conditions (dylan-compiled-output)
  (if catch-errors?
      (implementation-specific/catch-all-errors #'dylan%lisp-condition-handler
                                                dylan-compiled-output)
      (funcall dylan-compiled-output)))

;; A name for the condition handler...
(defun dylan%lisp-condition-handler (condition)
  (multiple-value-bind (format-string format-arguments)
      (implementation-specific/reflect-condition condition)
    (dylan-call dylan/signal
                (dylan-call dylan/make <simple-error>
                            :format-string format-string
                            :format-arguments format-arguments))))

;;; Chapter 2: every Dylan function is a method or a generic function.
;;; These predefined functions are Lisp closures, made methods here with
;;; the parameter lists the manual gives them.

(flet ((methodize (function required rest?)
         (dylan%make-method
          (make-param-list (mapcar (lambda (name) `(,name ,<object>)) required)
                           nil rest? nil)
          function)))
  (methodize dylan/values '() t)
  (methodize dylan/not '(thing) nil)
  ;; Chapter 13: signal and error take a condition or a format string
  ;; first, and cerror a restart description and then one of those.
  (methodize dylan/signal '(condition) t)
  (methodize dylan/error '(condition) t)
  (methodize dylan/cerror '(restart-description condition) t)
  (methodize dylan/break '() t)
  (methodize dylan/check-type '(value type) nil)
  (methodize dylan/abort '() nil)
  (methodize dylan/do-handlers '(function) nil)
  (methodize dylan/lisp-variable '(name) nil)
  (methodize dylan/lisp-procedure '(name) nil))

;;; A recursion too deep is an error a handler can catch, as in a compiled
;;; program (runtime/c/control.c): each method checks, on entry, that the
;;; stack is above *STACK-GUARD*, the soft limit.  While the error is
;;; handled the guard is the hard limit, below which the stack is reported
;;; exhausted, not signalled (FIND-STACK, in compat.lisp, sets them for the
;;; loop, or the first form evaluated).  Handlers are dynamic bindings,
;;; whose stack is apart: one is established only with 128 KB of it left,
;;; or 32 KB while the error is handled.

(defvar *binding-guard* (* 128 1024) "The room a handler needs.")
(defconstant +binding-hard+ (* 32 1024))

(define-condition stack-exhausted (storage-condition) ()
  (:report "the stack is exhausted")
  (:documentation "The stack exhausted below the hard limit: not an
error, which a Dylan handler would see, but the end of the run."))

(defun dylan%stack-exhausted ()
  (if (or (< (implementation-specific/stack-pointer) *stack-hard*)
          (< (implementation-specific/binding-stack-room) +binding-hard+))
      (error 'stack-exhausted)
      (let ((*stack-guard* *stack-hard*)
            (*binding-guard* +binding-hard+))
        (dylan-call dylan/error "the stack is exhausted"))))

(defun dylan%check-binding-stack ()
  (when (< (implementation-specific/binding-stack-room) *binding-guard*)
    (dylan%stack-exhausted)))

(defmacro dylan%check-stack ()
  `(when (< (implementation-specific/stack-pointer) *stack-guard*)
     (dylan%stack-exhausted)))

(defun dylan%check-print-stack ()
  "DYLAN%CHECK-STACK, for DYLAN-PRINT (compat.lisp), which recurses through
the elements of what it prints."
  (dylan%check-stack))
