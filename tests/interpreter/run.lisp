;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Run test programs in the interactive Common Lisp implementation:
;;;;
;;;;     sbcl --script tests/interpreter/run.lisp [file.lisp ...] [test ...]
;;;;
;;;; The files are in the form of tests/codegen/runner.lisp -- each case
;;;; (name program expected &key error compile-error) -- so that programs
;;;; the compiler to C is tested on test the interactive implementation
;;;; too, which must agree with it.  By default the macro cases,
;;;; tests/codegen/cases-macros.lisp: macros are the one extension both
;;;; implementations have.
;;;;
;;;; Each program runs in a module of its own, made for it, with its output
;;;; captured and compared with the expected output.  A program the
;;;; compiler would reject, or that would fail when run, must signal an
;;;; error here, whose message contains the expected text; whether it
;;;; comes from compiling or running makes no difference in this
;;;; implementation, which does both a form at a time.

(load (merge-pathnames "../../src/load-dylan.lisp"
                       (directory-namestring *load-truename*)))

(in-package #:dylan)

(defun read-program (text)
  "The forms of the Dylan program TEXT."
  (with-input-from-string (in text)
    (let ((eof (list 'eof)))
      (loop for form = (dylan-read in nil eof)
            until (eq form eof)
            collect form))))

(defun evaluate-forms (forms)
  "Compile and evaluate FORMS one by one, as the read-eval-print loop does."
  (let ((module-variables '()))
    (dolist (form forms)
      (compile-expression
       form nil module-variables
       (lambda (new-variables preamble compiled)
         (implementation-specific/eval (append preamble (list compiled)))
         (setf module-variables (append new-variables module-variables)))))))

(defun condition-message (condition)
  "The message of CONDITION on one line, without the Error: prefix."
  (let ((text (string-trim '(#\Space #\Newline #\Tab) (format nil "~a" condition))))
    (if (and (> (length text) 7) (string= "Error: " text :end2 7))
        (subseq text 7)
        text)))

(defun run-program (name text)
  "Run the program TEXT in a module of its own; answer its output and the
message of the error that ended it, or NIL."
  (let ((out (make-string-output-stream))
        (module (intern (format nil "test-~(~a~)" name) '#:dylan))
        (message nil))
    (handler-case
        (let ((*standard-output* out))
          (in-module 'dylan-user)
          (evaluate-forms (list `(define-module ,module (use dylan))))
          (in-module module)
          (evaluate-forms (read-program text)))
      (error (e) (setf message (condition-message e))))
    (in-module 'dylan-user)
    (values (get-output-stream-string out) message)))

(defun run-case (case)
  "The failures of CASE, a list of strings."
  (destructuring-bind (name text expected &key error compile-error) case
    (let ((wanted (or error compile-error)))
      (multiple-value-bind (output message) (run-program name text)
        (append
         (unless (string= output expected)
           (list (format nil "~a: output~%~a~%expected~%~a" name output expected)))
         (cond ((and wanted (null message))
                (list (format nil "~a: expected the error ~s, got none" name wanted)))
               ((and wanted (not (search wanted message)))
                (list (format nil "~a: expected the error ~s, got ~s" name wanted message)))
               ((and message (not wanted))
                (list (format nil "~a: error: ~a" name message)))))))))

(defun read-cases (file)
  (with-open-file (in file)
    (let ((*package* (find-package '#:dylan)))
      (read in))))

(defun run-test-files (default-files arguments)
  (let* ((files (or (remove-if-not (lambda (a) (search ".lisp" a)) arguments)
                    default-files))
         (selected (remove-if (lambda (a) (search ".lisp" a)) arguments))
         (cases (loop for file in files
                      append (remove-if-not
                              (lambda (c)
                                (or (null selected)
                                    (member (string-downcase (symbol-name (first c)))
                                            selected :test #'string=)))
                              (read-cases file))))
         (failures (loop for case in cases append (run-case case))))
    (dolist (f failures) (format t "FAIL ~a~%" f))
    (format t "~d program~:p, ~d failure~:p~%" (length cases) (length failures))
    (finish-output)
    (sb-ext:exit :code (if failures 1 0) :abort t)))

(run-test-files (list (merge-pathnames "../codegen/cases-macros.lisp"
                                       (directory-namestring *load-truename*)))
                (rest sb-ext:*posix-argv*))
