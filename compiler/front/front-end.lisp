;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; The front end: Dylan source files to a PROGRAM of core forms.
;;;;
;;;;   1. Read every file, with the position of every list recorded.
;;;;   2. Declare every module a top-level define-module form names, then
;;;;      define them all, each after the modules it uses.
;;;;   3. Convert each file's top-level forms, in order, in the module its
;;;;      header names (or dylan-user).
;;;;   4. Warn of module variables that are referred to but never defined.
;;;;
;;;; An error abandons the top-level form it occurs in; the rest are still
;;;; converted, so one run reports every error it can.

(in-package #:dylan-compiler)

(defstruct (unit (:constructor make-unit (file module raw-forms)))
  file                                  ; SOURCE-FILE, or NIL
  module                                ; DYLAN-MODULE
  raw-forms                             ; (form . location), as read
  (forms '()))                          ; core nodes, in order

(defstruct (program (:constructor make-program (units modules diagnostics)))
  units
  modules                               ; name -> DYLAN-MODULE
  diagnostics                           ; in the order found
  (spellings nil))                      ; symbol -> its first spelling in
                                        ; this program (chapter 15)

(defun program-errors (program)
  (remove :warning (program-diagnostics program) :key #'diagnostic-kind))

(defun program-warnings (program)
  (remove :error (program-diagnostics program) :key #'diagnostic-kind))

(defmacro collecting-errors (&body body)
  "Evaluate BODY; a compile error in it is recorded and answers NIL."
  `(handler-case (progn ,@body)
     (compile-error (e) (note-error e) nil)))

(defun library-files ()
  "The library's source files, which every program is compiled with
(CompilerPlan.md, Phase 8)."
  (sort (directory (merge-pathnames "../runtime/dylan/*.dyl" cl-user::*dylanc-directory*))
        #'string< :key #'namestring))

(defun front-end (pathnames &key library)
  "Read and convert the Dylan source files PATHNAMES, a whole program --
after the library's, if LIBRARY.  Answer a PROGRAM; its diagnostics say
whether it succeeded."
  (front-end-sources
   (mapcar (lambda (p)
             (cons (namestring p)
                   (handler-case (read-file-text p)
                     (file-error (e)
                       (make-diagnostic :error nil (format nil "~a" e))))))
           (append (and library (library-files)) pathnames))))

(defun front-end-forms (forms &key (module "dylan-user"))
  "Convert FORMS, Dylan forms as DYLAN::DYLAN-READ reads them, as a whole
program in MODULE.  For tests."
  (front-end-units (list (list nil module (mapcar (lambda (f) (cons f nil)) forms)))))

(defun front-end-string (text &key (name "<string>") library)
  "Convert the Dylan source TEXT as a whole program (after the library's
sources, if LIBRARY).  For tests."
  (front-end-sources
   (append (and library
                (mapcar (lambda (p) (cons (namestring p) (read-file-text p)))
                        (library-files)))
           (list (cons name text)))))

(defun front-end-sources (sources)
  "SOURCES is a list of (name . text); a text may instead be a diagnostic
saying why the file could not be read."
  (let ((*diagnostics* '())
        (specs '())
        ;; The case a symbol is first written in is the program's own.
        (dylan::*symbol-spellings* (make-hash-table :test #'eq)))
    (dolist (source sources)
      (if (diagnostic-p (cdr source))
          (push (cdr source) *diagnostics*)
          (collecting-errors
            (multiple-value-bind (file module forms)
                (read-source (car source) (cdr source))
              (push (list file (or module "dylan-user") forms) specs)))))
    (let ((program (front-end-units (nreverse specs) :diagnostics *diagnostics*)))
      (setf (program-spellings program) dylan::*symbol-spellings*)
      program)))

(defun front-end-units (specs &key diagnostics)
  "SPECS is a list of (source-file module-name-string forms)."
  (let ((*diagnostics* diagnostics)
        (*compile-time-variables* :new)
        (*variable-counter* 0)
        (*dylan-modules* nil)
        (*location* nil))
    (initialize-modules)
    ;; Modules first.
    (dolist (spec specs)
      (let ((*source-file* (first spec)))
        (dolist (entry (third spec))
          (let ((*location* (cdr entry)))
            (declare-modules-in (car entry))))))
    (define-all-modules)
    ;; Then the forms of each file, in its module.
    (let ((units
            (loop for (file module-name forms) in specs
                  for unit = (let ((*source-file* file)
                                   (*location* (and file (make-location file 0))))
                               (collecting-errors
                                 (make-unit file
                                            (find-module (dylan-name module-name))
                                            forms)))
                  when unit
                    collect unit)))
      (dolist (unit units)
        (let ((*source-file* (unit-file unit))
              (*module* (unit-module unit)))
          (setf (unit-forms unit)
                (loop for (form . location) in (unit-raw-forms unit)
                      append (let ((*location* location))
                               (collecting-errors (convert-top-level form)))))))
      (check-undefined-variables)
      (make-program units *dylan-modules* (reverse *diagnostics*)))))

(defun declare-modules-in (form)
  "Declare the modules the top-level FORM defines, looking into top-level
begin forms."
  (when (consp form)
    (case (car form)
      ((dylan::define-module)
       (collecting-errors (declare-module form)))
      ((dylan::begin)
       (when (proper-list-p form)
         (mapc #'declare-modules-in (cdr form)))))))

(defun check-undefined-variables ()
  "Warn of every module variable referred to but defined nowhere in the
program.  (Referring to one is an error only if the reference runs, so this
is a warning.)"
  (loop for module being the hash-values of *dylan-modules*
        do (loop for var being the hash-values of (dylan-module-variables module)
                 when (and (module-var-first-reference var)
                           (null (module-var-definitions var))
                           (not (module-var-predefined-p var)))
                   do (let ((*location* (let ((l (module-var-first-reference var)))
                                          (and (location-p l) l))))
                        (compile-warning "~a is not defined~a"
                                         (name-string (module-var-name var))
                                         (if (eq module (dylan-module))
                                             " in the dylan module"
                                             (format nil " (module ~a)"
                                                     (dylan-string
                                                      (dylan-module-name module)))))))))

;;; Printing a program

(defun print-program (program &optional (stream *standard-output*))
  "Print PROGRAM's core forms and diagnostics, for people."
  (dolist (unit (program-units program))
    (format stream "~&;;; ~a  (module ~a)~%"
            (if (unit-file unit) (source-file-name (unit-file unit)) "<forms>")
            (dylan-string (dylan-module-name (unit-module unit))))
    (dolist (node (unit-forms unit))
      (format stream "~a~%~%" (unparse-string node))))
  (dolist (d (program-diagnostics program))
    (format stream "~a~%" (diagnostic-string d))))
