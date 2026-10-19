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
;;;; A regression test driver: reads and executes the examples
;;;; in ../dylan-examples.dyl.                     (was Thomas's full-test.scm)

(in-package #:dylan)

(defun display-simple-condition (condition)
  (dylan-display (dylan-format-message
                  (dylan-call dylan/condition-format-string condition)
                  (dylan-call dylan/condition-format-arguments condition))))

(defun display-condition (condition)
  (fresh-line)
  (let ((condition-type (get-type condition)))
    (cond
      ((eq condition-type <simple-error>)
       (dylan-display ";Error: ")
       (display-simple-condition condition))
      ((eq condition-type <simple-warning>)
       (dylan-display ";Warning: ")
       (display-simple-condition condition))
      ((eq condition-type <type-error>)
       (dylan-display ";Error: ")
       (dylan-display (condition-message condition)))
      (t
       (dylan-display ";Unhandled dylan condition: ")
       (dylan-write condition)))))

(defun test (file)
  (with-open-file (in file)
    (let ((eof (list 'eof))
          (module-variables '()))
      (loop
        (let ((sexpr (dylan-read in nil eof)))
          (when (eq sexpr eof)
            (terpri) (terpri)
            (return))
          (pp sexpr)
          (terpri)
          (setf module-variables
                (call-with-escape-continuation
                 (lambda (error-exit)
                   (dylan%catch-all-conditions
                    (lambda ()
                      (dylan%handler-bind
                       <condition>              ; type
                       (make-dylan-callable     ; function
                        (lambda (condition next-handler)
                          (declare (ignore next-handler))
                          (display-condition condition)
                          (terpri)
                          (funcall error-exit module-variables)))
                       (make-dylan-callable     ; test
                        (lambda (condition) (declare (ignore condition)) t))
                       (make-dylan-callable     ; description
                        (lambda (stream)
                          (dylan-display "error handler from full-test.lisp"
                                         stream)))
                       (lambda ()
                         (compile-expression
                          sexpr '!multiple-values module-variables
                          (lambda (new-vars preamble compiled-output)
                            (implementation-specific/eval
                             (append preamble
                                     (list (make-result-form
                                            compiled-output))))
                            (append new-vars module-variables)))))))))))))))

(defun test-examples (&optional (file "../dylan-examples.dyl"))
  (test file))
