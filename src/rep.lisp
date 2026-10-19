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

(defun empty-environment! ()
  "Forget the module variables created by previous DYLAN:REP interactions,
and their values."
  (dolist (name rep-module-variables)
    (unless (member name dylan%predefined-variables)
      (makunbound (name->module-variable name))))
  (setf rep-module-variables '()))

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

(defun rep-done-p (input)
  (and (symbolp input)
       (not (null input))
       (string-equal (symbol-name input) "DYLAN:DONE")))

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
  (write-line "Exit by typing \"dylan:done\"")
  (dylan%catch-all-conditions
   (lambda ()
     (loop
       (terpri)
       (write-string "? ")
       (finish-output)
       (let ((input (dylan-read)))
         (terpri)
         (cond
           ((rep-done-p input) (return '|DYLAN:DONE|))
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
                  (with-abort-handler
                    (lambda ()
                      (implementation-specific/eval
                       (append preamble
                               (list (make-result-form compiled-output)))))))
                (setf rep-module-variables
                      (append new-vars rep-module-variables)))))))))))

(format t "~&~%Call DYLAN:REP to start a Dylan read-eval-print loop.~%")
