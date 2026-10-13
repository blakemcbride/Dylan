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
    (let* ((program (front-end (list (merge-pathnames
                                      "../../dylan-examples.dyl"
                                      cl-user::*front-end-test-directory*))))
           (diagnostics (mapcar #'diagnostic-string (program-diagnostics program))))
      (unless (and (= (length diagnostics) 1)
                   (search "dylan-examples.dyl:889:1: error: if: needs a test"
                           (first diagnostics)))
        (push "dylan-examples" failures)
        (format t "FAIL dylan-examples.dyl:~%~{  ~a~%~}" diagnostics)))
    (format t "~d case~:p, ~d failure~:p~%" (1+ (length cases)) (length failures))
    (null failures)))

(sb-ext:exit :code (if (run-tests) 0 1))
