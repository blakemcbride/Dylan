;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Run test programs in the interactive Common Lisp implementation:
;;;;
;;;;     sbcl --script tests/interpreter/run.lisp [file.lisp ...] [test ...]
;;;;
;;;; The files are in the form of tests/codegen/runner.lisp -- each case
;;;; (name program expected &key error compile-error compiler-only) -- so
;;;; that the programs the compiler to C is tested on test the interactive
;;;; implementation too, which must agree with it.  By default all of the
;;;; compiler's case files, tests/codegen/cases*.lisp.
;;;;
;;;; Each program runs in the module dylan-user, as a compiled program
;;;; does, and what it defined there is forgotten before the next; its
;;;; output is captured and compared with the expected output.  A program
;;;; the compiler would reject, or that would fail when run, must signal an
;;;; error here, whose message contains the expected text; whether it comes
;;;; from compiling or running makes no difference in this implementation,
;;;; which does both a form at a time, and the "Dylan error: " with which a
;;;; compiled program reports an unhandled error is not expected of it.  A
;;;; case marked :compiler-only t tests something the interactive
;;;; implementation does not do by design -- 63-bit integers, a static
;;;; program's restrictions, the compiler's own diagnostics, threads -- and
;;;; is skipped.

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

(defvar *program-variables* '()
  "The module variables the program being run has made so far.")

(defun evaluate-forms (forms)
  "Compile and evaluate FORMS one by one, as the read-eval-print loop does."
  (let ((module-variables '()))
    (dolist (form forms)
      (compile-expression
       form nil module-variables
       (lambda (new-variables preamble compiled)
         (implementation-specific/eval (append preamble (list compiled)))
         (setf module-variables (append new-variables module-variables))
         (setf *program-variables* (append new-variables *program-variables*)))))))

(defun forget-program ()
  "Undo the program's definitions in dylan-user, for the next program:
its module variables lose their values, and are macros and read-only no
longer."
  (dolist (name *program-variables*)
    (let ((variable (name->module-variable name)))
      (makunbound variable)
      (remhash variable *read-only-variables*)))
  (clrhash *dylan-macros*)              ; a macro's own name is not among them
  (clrhash *symbol-spellings*)          ; chapter 15: how a symbol was spelled
  (setf *program-variables* '()))

(defun condition-message (condition)
  "The message of CONDITION on one line, without the Error: prefix."
  (let ((text (string-trim '(#\Space #\Newline #\Tab) (format nil "~a" condition))))
    (if (and (> (length text) 7) (string= "Error: " text :end2 7))
        (subseq text 7)
        text)))

(defun run-program (text)
  "Run the program TEXT in dylan-user; answer its output and the message
of the error that ended it, or NIL.  Lisp's own errors are offered to the
program's handlers, as at the read-eval-print loop."
  (let ((out (make-string-output-stream))
        (message nil))
    (in-module 'dylan-user)
    (handler-case
        (let ((*standard-output* out))
          (dylan%catch-all-conditions
           (lambda () (evaluate-forms (read-program text)))))
      (error (e) (setf message (condition-message e))))
    (forget-program)
    (values (get-output-stream-string out) message)))

(defun expected-message (text)
  "TEXT less the prefix a compiled program's report of an unhandled error
has, which this implementation's has not."
  (let ((prefix "Dylan error: "))
    (if (and (>= (length text) (length prefix))
             (string= prefix text :end2 (length prefix)))
        (subseq text (length prefix))
        text)))

(defun run-case (case)
  "The failures of CASE, a list of strings, or :SKIPPED."
  (destructuring-bind (name text expected &key error compile-error compiler-only) case
    (if compiler-only
        :skipped
        (let ((wanted (and (or error compile-error)
                           (expected-message (or error compile-error)))))
          (multiple-value-bind (output message) (run-program text)
            (append
             (unless (string= output expected)
               (list (format nil "~(~a~): output~%~a~%expected~%~a" name output expected)))
             (cond ((and wanted (null message))
                    (list (format nil "~(~a~): expected the error ~s, got none" name wanted)))
                   ((and wanted (not (search wanted message)))
                    (list (format nil "~(~a~): expected the error ~s, got ~s" name wanted message)))
                   ((and message (not wanted))
                    (list (format nil "~(~a~): error: ~a" name message))))))))))

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
         (skipped 0)
         (failures (loop for case in cases
                         for result = (run-case case)
                         if (eq result :skipped) do (incf skipped)
                           else append result)))
    (dolist (f failures) (format t "FAIL ~a~%" f))
    (format t "~d program~:p, ~d failure~:p~[~:;, ~:*~d compiler-only skipped~]~%"
            (- (length cases) skipped) (length failures) skipped)
    (finish-output)
    (sb-ext:exit :code (if failures 1 0) :abort t)))

(run-test-files (directory (merge-pathnames "../codegen/cases*.lisp"
                                            (directory-namestring *load-truename*)))
                (rest sb-ext:*posix-argv*))
