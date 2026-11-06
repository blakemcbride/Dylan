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
;;;; An interactive read-eval-print loop for Dylan.
;;;;                                      (was Thomas's src/portable-rep.scm)
;;;;
;;;; Just use the current package and keep a list of known module variables
;;;; in REP-MODULE-VARIABLES.

(in-package #:dylan)

(defvar rep-module-variables '())

(defvar rep-eof (list 'eof)
  "What DYLAN-READ returns at the end of the input.")

(defun empty-environment! ()
  "Forget the module variables created by previous DYLAN:REP interactions,
and their values: they are read-only no longer, as define-class and
define-generic-function made them, and the macros defined are forgotten
too, so that each name can be defined afresh.  (Every macro is a
program's; the system defines none.)"
  (dolist (name rep-module-variables)
    (unless (member name dylan%predefined-variables)
      (let ((variable (name->module-variable name)))
        (makunbound variable)
        (remhash variable *read-only-variables*))))
  (clrhash *dylan-macros*)
  (setf rep-module-variables '()))

(defun batch-error-message (condition)
  "The message of CONDITION, an error no handler took, as a compiled
program reports it."
  (typecase condition
    (dylan-condition
     (dylan-format-message (dylan-condition-message condition)
                           (dylan-condition-irritants condition)))
    (unhandled-dylan-condition
     ;; As display prints it, through its class's print-object
     (with-output-to-string (s)
       (dylan-display (unhandled-dylan-condition-object condition) s)))
    ;; The Lisp errors that are Dylan's too, as a handler would see them
    ((or program-error arithmetic-error type-error)
     (multiple-value-bind (message irritants)
         (implementation-specific/reflect-condition condition)
       (if (eq message *reflected-error-tag*)
           (princ-to-string condition)
           (dylan-format-message message irritants))))
    (t (princ-to-string condition))))

(defun end-batch-run (condition hook)
  "The debugger when the loop reads a file (*BATCH*): report CONDITION as
a compiled program reports an unhandled error, and exit with status 1."
  (declare (ignore hook))
  (finish-output *standard-output*)
  (format *error-output* "~&Dylan error: ~a~%" (batch-error-message condition))
  (finish-output *error-output*)
  #+sbcl (sb-ext:exit :code 1 :abort t)
  #-sbcl (abort))

(defun print-rep-result (result multiple-values)
  (if (eq result multiple-values)
      (progn
        (loop for value in (aref multiple-values 0)
              for count from 1
              do (fresh-line)
                 (format t ";Value[~a]: " count)
                 (dylan-write value))
        (terpri))
      (progn
        (fresh-line)
        (write-string ";Value: ")
        (dylan-write result)
        (terpri))))

(defun make-result-form (compiled-output)
  `(let* ((!multiple-values (vector '()))
          (!result ,compiled-output))
     (print-rep-result !result !multiple-values)))

(defun rep-command-p (input name)
  "True if INPUT is the command NAME, typed bare or as (NAME)."
  (flet ((named-p (x)
           (and (symbolp x) x (string-equal (symbol-name x) name))))
    (or (named-p input)
        (and (consp input) (null (cdr input)) (named-p (car input))))))

;;; dylan:done leaves the loop; exit leaves Lisp as well -- unless the
;;; program has given the variable exit a value of its own, which (exit)
;;; then calls.

(defun rep-done-p (input)
  (rep-command-p input "DYLAN:DONE"))

(defun rep-exit-p (input)
  (and (consp input) (rep-command-p input "EXIT")
       (let ((variable (name->module-variable (variable->name (car input)))))
         (not (and (boundp variable)
                   (not (eq (symbol-value variable) the-unassigned-value)))))))

;;; (dylan:in-module name) switches the module later input is compiled in.
(defun rep-in-module-p (input)
  (and (consp input)
       (symbolp (car input))
       (car input)
       (string-equal (symbol-name (car input)) "DYLAN:IN-MODULE")))

(defun with-abort-handler (thunk)
  "Call THUNK with a handler for <abort>.  Chapter 13: signalling an
<abort> returns control to the command loop -- here, the next prompt."
  (call-with-escape-continuation
   (lambda (return)
     (dylan%handler-bind
      <abort>
      (make-dylan-callable
       (lambda (condition next-handler)
         (declare (ignore condition next-handler))
         (format t "~&;Aborted~%")
         (funcall return nil)))
      (make-dylan-callable (lambda (condition) (declare (ignore condition)) t))
      "Return to the read-eval-print loop."
      thunk))))

(defun rep ()
  "Start a Dylan read-eval-print loop."
  (terpri)
  (write-line "Entering Dylan read-eval-print-loop.")
  (write-line "Leave by typing \"(exit)\"")
  (find-stack)
  (dylan%catch-all-conditions
   (lambda ()
     (loop
       (terpri)
       (write-string "? ")
       (finish-output)
       (let ((input (dylan-read *standard-input* nil rep-eof)))
         (terpri)
         (cond
           ((or (eq input rep-eof) (rep-done-p input)) (return '|DYLAN:DONE|))
           ((rep-exit-p input)
            (finish-output)
            #+sbcl (sb-ext:exit :code 0)
            #-sbcl (return '|DYLAN:DONE|))
           ((rep-in-module-p input)
            (unless (list-of-length? input 2)
              (dylan%error "dylan:in-module -- one module name expected" input))
            (in-module (cadr input))
            (format t ";Module: ~(~a~)~%" *current-module*))
           (t
             (compile-expression
              input '!multiple-values rep-module-variables
              (lambda (new-vars preamble compiled-output)
                ;; Any error, Dylan's or one only Lisp knows of, can
                ;; return to the loop from the debugger.
                (with-simple-restart (dylan-rep "Return to the Dylan read-eval-print loop.")
                  ;; A program run from a file has no loop to return to:
                  ;; an <abort> no handler takes is an error, as in a
                  ;; compiled program.
                  (funcall (if *batch* #'funcall #'with-abort-handler)
                           (lambda ()
                             (implementation-specific/eval
                              (append preamble
                                      (list (make-result-form compiled-output)))))))
                (setf rep-module-variables
                      (append new-vars rep-module-variables)))))))))))

(format t "~&~%Call DYLAN:REP to start a Dylan read-eval-print loop.~%")
