;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Run the IR tests (Phase 2):
;;;;
;;;;     sbcl --script tests/ir/run.lisp [print]
;;;;
;;;; Each case in cases.lisp is converted to the IR, checked with CHECK-IR,
;;;; and compared with the IR it expects; then its property checks are
;;;; made.  Last, the IR of dylan-examples.dyl must pass CHECK-IR.

(let ((here (directory-namestring (or *load-truename* *default-pathname-defaults*))))
  (let ((*standard-output* (make-broadcast-stream)))
    (load (merge-pathnames "../../compiler/load.lisp" here)))
  (defparameter cl-user::*ir-test-directory* here))

(in-package #:dylan-compiler)

(defvar *print-actual* (member "print" sb-ext:*posix-argv* :test #'string=)
  "Given the argument print, show what each case converts to.")

(defun ir-text (ir)
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (top) (ir-string (ir-fn-body top)))
                  (ir-program-toplevels ir))))

(defun find-var (ir name)
  (or (find name (ir-bound-vars ir)
            :key (lambda (v) (name-string (ivar-name v))) :test #'string=)
      (error "no variable ~a" name)))

(defun find-fn (ir which)
  "The fn named WHICH, or the WHICH'th fn (counting from 0)."
  (let ((fns (ir-fns ir)))
    (or (if (integerp which)
            (nth which fns)
            (find which fns :key (lambda (f) (and (ir-fn-name f)
                                                  (name-string (ir-fn-name f))))
                            :test #'equal))
        (error "no fn ~a" which))))

(defun property (object key)
  (etypecase object
    (ivar (ecase key
            (:captured (ivar-captured-p object))
            (:assigned (ivar-assigned-p object))
            (:refs (ivar-ref-count object))
            (:calls (ivar-call-count object))
            (:escapes (ivar-escapes-p object))
            (:fn (and (ivar-value-fn object) t))))
    (ir-fn (ecase key
             (:escapes (ir-fn-escapes-p object))
             (:call-sites (length (ir-fn-call-sites object)))
             (:tail-call-sites (count-if #'ir-call-tail-p (ir-fn-call-sites object)))
             (:free (sort (mapcar (lambda (v) (name-string (ivar-name v)))
                                  (ir-fn-free-vars object))
                          #'string<))))))

(defun run-check (ir check)
  "NIL if CHECK holds, else a description of how it fails."
  (destructuring-bind (kind which &rest expected) check
    (let ((object (ecase kind
                    (:var (find-var ir which))
                    (:fn (find-fn ir which)))))
      (loop for (key value) on expected by #'cddr
            for actual = (property object key)
            unless (equal actual value)
              collect (format nil "~(~s ~s ~s~): expected ~s, got ~s"
                              kind which key value actual)))))

(defun run-case (case)
  (destructuring-bind (name source expected &rest checks) case
    (handler-case
        (let ((program (front-end-string source)))
          (when (program-errors program)
            (return-from run-case
              (format nil "~(~a~): ~{~a~%~}" name
                      (mapcar #'diagnostic-string (program-errors program)))))
          (let* ((ir (to-ir program))
                 (text (ir-text ir)))
            (check-ir ir)
            (when *print-actual*
              (format t "~&;;; ~(~a~)~%~a~%" name text))
            (let ((problems (append (unless (string= text expected)
                                      (list (format nil "expected~%~a~%got~%~a"
                                                    expected text)))
                                    (loop for c in checks append (run-check ir c)))))
              (and problems
                   (format nil "~(~a~):~{ ~a~%~}" name problems)))))
      (error (e) (format nil "~(~a~): ~a" name e)))))

(defun run-tests ()
  (let* ((cases (with-open-file (in (merge-pathnames "cases.lisp"
                                                     cl-user::*ir-test-directory*))
                  (let ((*package* (find-package '#:dylan-compiler)))
                    (read in))))
         (failures (remove nil (mapcar #'run-case cases))))
    (dolist (f failures) (format t "FAIL ~a~%" f))
    (handler-case
        (check-ir (to-ir (front-end (list (merge-pathnames
                                           "../../dylan-examples.dyl"
                                           cl-user::*ir-test-directory*)))))
      (error (e)
        (push "dylan-examples" failures)
        (format t "FAIL dylan-examples.dyl: ~a~%" e)))
    (format t "~d case~:p, ~d failure~:p~%" (1+ (length cases)) (length failures))
    (null failures)))

(sb-ext:exit :code (if (run-tests) 0 1))
