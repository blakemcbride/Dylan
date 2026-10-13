;;;; Copyright 2026 Blake McBride.  See ../LICENSE.
;;;;
;;;; Load the Dylan-to-C compiler.
;;;;
;;;;     (load "compiler/load.lisp")
;;;;     (dylanc:front-end '("program.dyl"))
;;;;
;;;; The compiler reads Dylan with the reader of the Common Lisp
;;;; implementation, so that is loaded first (src/load-dylan.lisp).  Files
;;;; are compiled into compiler/fasl/<lisp>-<version>/ when their fasl is
;;;; missing or older than the source, and every file after a recompiled
;;;; one is recompiled too.

(in-package #:cl-user)

(defparameter *dylanc-directory*
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *default-pathname-defaults*)))

(let ((*standard-output* (make-broadcast-stream)))
  (load (merge-pathnames "../src/load-dylan.lisp" *dylanc-directory*)))

(defparameter *dylanc-files*
  '("package"
    "front/source"
    "front/dylan-names"
    "front/core"
    "front/modules"
    "front/syntax"
    "front/macros"
    "front/convert"
    "front/front-end"
    "ir/ir"
    "ir/anf"
    "ir/analyze"
    "ir/check"
    "ir/control"
    "backend/classes"
    "backend/cdata"
    "backend/primitives"
    "backend/objects"
    "backend/codegen"
    "driver"))

(defparameter *dylanc-fasl-directory*
  (merge-pathnames
   (make-pathname :directory
                  (list :relative "fasl"
                        (substitute-if #\- (lambda (c) (not (alphanumericp c)))
                                       (format nil "~a-~a"
                                               (lisp-implementation-type)
                                               (lisp-implementation-version)))))
   *dylanc-directory*))

(let ((stale nil))
  (with-compilation-unit ()
    (handler-bind (#+sbcl (sb-ext:compiler-note #'muffle-warning))
      (dolist (name *dylanc-files*)
        (let* ((source (merge-pathnames (concatenate 'string name ".lisp")
                                        *dylanc-directory*))
               (fasl (merge-pathnames (concatenate 'string name ".fasl")
                                      *dylanc-fasl-directory*)))
          (when (or stale
                    (not (probe-file fasl))
                    (>= (file-write-date source) (file-write-date fasl)))
            (setf stale t)
            (ensure-directories-exist fasl)
            (let ((*compile-verbose* nil) (*compile-print* nil))
              (compile-file source :output-file fasl)))
          (load fasl))))))
