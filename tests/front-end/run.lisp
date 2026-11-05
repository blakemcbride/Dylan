;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Run the front-end tests:
;;;;
;;;;     sbcl --script tests/front-end/run.lisp [print]
;;;;
;;;; Each case in cases.lisp is converted on its own as a whole program, and
;;;; its core forms (or diagnostics) compared with what the case expects.
;;;; Then dylan-examples.dyl, the book's examples, is converted, and must
;;;; produce no diagnostic but the one error its page 68 example is meant
;;;; to cause.

(let ((here (directory-namestring (or *load-truename* *default-pathname-defaults*))))
  (let ((*standard-output* (make-broadcast-stream)))
    (load (merge-pathnames "../../compiler/load.lisp" here)))
  (defparameter cl-user::*front-end-test-directory* here))

(in-package #:dylan-compiler)

;;; The line of the page 68 example, the if with too many arguments: the
;;; last "(if #t" before the comment that records its error.
(defun page-68-if-line (file)
  (with-open-file (in file)
    (loop with if-line = nil
          for line = (read-line in nil)
          for n from 1
          while line
          when (string= (string-trim " " line) "(if #t")
            do (setf if-line n)
          when (search "error: too many arguments to if" line)
            return if-line)))

(defvar *print-actual* (member "print" sb-ext:*posix-argv* :test #'string=)
  "Given the argument print, show what each case converts to.")

(defun program-text (program)
  (format nil "~{~a~^~%~}"
          (loop for unit in (program-units program)
                append (mapcar #'unparse-string (unit-forms unit)))))

(defun run-case (case)
  "Answer NIL if CASE passes, else a description of the failure."
  (destructuring-bind (name source expected) case
    (let* ((program (front-end-string source))
           (diagnostics (mapcar #'diagnostic-string (program-diagnostics program))))
      (if (and (consp expected) (eq (car expected) :diagnostics))
          (let ((wanted (cdr expected)))
            (unless (and (= (length wanted) (length diagnostics))
                         (every #'search wanted diagnostics))
              (format nil "~(~a~): expected diagnostics~%~{  ~a~%~}got~%~{  ~a~%~}"
                      name wanted diagnostics)))
          ;; Only errors matter here: the cases call functions they do not
          ;; define.
          (let ((text (program-text program))
                (diagnostics (mapcar #'diagnostic-string (program-errors program))))
            (when *print-actual*
              (format t "~&;;; ~(~a~)~%~a~%~{  ~a~%~}" name text diagnostics))
            (unless (and (null diagnostics) (string= text expected))
              (format nil "~(~a~): expected~%~a~%got~%~a~%~{  ~a~%~}"
                      name expected text diagnostics)))))))

(defun run-tests ()
  (let* ((cases (with-open-file (in (merge-pathnames "cases.lisp"
                                                     cl-user::*front-end-test-directory*))
                  (let ((*package* (find-package '#:dylan-compiler)))
                    (read in))))
         (failures (remove nil (mapcar #'run-case cases))))
    (dolist (f failures) (format t "FAIL ~a~%" f))
    ;; The book's examples.
    (let* ((file (merge-pathnames "../../dylan-examples.dyl"
                                  cl-user::*front-end-test-directory*))
           (program (front-end (list file)))
           (diagnostics (mapcar #'diagnostic-string (program-diagnostics program)))
           (expected (format nil "dylan-examples.dyl:~d:1: error: if: needs a test"
                             (page-68-if-line file))))
      (unless (and (= (length diagnostics) 1)
                   (search expected (first diagnostics)))
        (push "dylan-examples" failures)
        (format t "FAIL dylan-examples.dyl:~%~{  ~a~%~}" diagnostics)))
    (format t "~d case~:p, ~d failure~:p~%" (1+ (length cases)) (length failures))
    (null failures)))

(sb-ext:exit :code (if (run-tests) 0 1))
