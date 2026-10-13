;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Modules (chapter 5 and design note 32), resolved at compile time.
;;;;
;;;; This follows src/comp-module.lisp, but where that maps a Dylan name to
;;;; a Lisp name, this maps it to a MODULE-VAR: one object per variable,
;;;; held by the module that owns it.  A name either refers to a variable
;;;; imported from a module the module uses, or to one the module owns.  A
;;;; variable named (setter foo) goes with foo: it is imported, renamed
;;;; and excluded with it.
;;;;
;;;; The program is compiled whole, so every define-module form is known
;;;; before any other form is converted.  The front end defines all the
;;;; modules first, each after the modules it uses, which makes the order
;;;; of definition in the source irrelevant.
;;;;
;;;; Two modules are predefined.  DYLAN owns the predefined variables and
;;;; exports them all; until the library is written (CompilerPlan.md,
;;;; Phase 8) its contents are the names in dylan-names.lisp.  DYLAN-USER
;;;; uses DYLAN, and is where a file's forms go unless its module: header
;;;; names another.

(in-package #:dylan-compiler)

(defstruct (dylan-module (:constructor make-dylan-module (name)))
  name                                  ; a symbol
  (variables (make-hash-table :test #'equal)) ; name -> MODULE-VAR it owns
  (imports (make-hash-table :test #'eq)) ; symbol here -> (module . symbol)
  (exports '())                         ; symbols
  (created '())                         ; symbols named in create clauses
  (clauses '())
  (location nil)
  (state :defined))                     ; or :pending, :defining

(defmethod print-object ((m dylan-module) stream)
  (print-unreadable-object (m stream :type t)
    (write-string (dylan-string (dylan-module-name m)) stream)))

(defstruct (module-var (:constructor %make-module-var (module name)))
  module                                ; the DYLAN-MODULE that owns it
  name                                  ; symbol or (setter symbol)
  (definitions '())                     ; definition nodes, in order
  (first-reference nil)                 ; location of the first reference
  (predefined-p nil)                    ; one of the DYLAN module's own?
  (assigned-p nil)                      ; the target of a set!?
  (thread-local-p nil))                 ; defined by define-thread-variable?

(defmethod print-object ((v module-var) stream)
  (print-unreadable-object (v stream :type t)
    (write-string (module-var-string v) stream)))

(defun module-var-string (var)
  (format nil "~a@~a" (name-string (module-var-name var))
          (dylan-string (dylan-module-name (module-var-module var)))))

(defvar *dylan-modules* nil
  "The modules of the program being compiled: name -> DYLAN-MODULE.")

(defun find-module (name &optional (errorp t))
  (or (gethash name *dylan-modules*)
      (and errorp (compile-error "unknown module ~a" name))))

(defun dylan-module () (find-module 'dylan::dylan))

;;; Variables

(defun name-symbol (name) (if (consp name) (cadr name) name))
(defun setter-name-p (name) (consp name))

(defun same-kind-name (symbol name)
  "SYMBOL, or (setter SYMBOL) if NAME is a setter name."
  (if (setter-name-p name) (list 'dylan::setter symbol) symbol))

(defun owned-variable (module name)
  "The variable MODULE owns under NAME, made if need be."
  (let ((table (dylan-module-variables module)))
    (or (gethash name table)
        (setf (gethash name table) (%make-module-var module name)))))

(defun resolve-symbol (module symbol)
  "The module that owns the variable SYMBOL names in MODULE, and the name
it has there."
  (let ((import (gethash symbol (dylan-module-imports module))))
    (if import
        (values (car import) (cdr import))
        (values module symbol))))

(defun lookup-module-var (module name)
  "The variable NAME (a symbol or (setter symbol)) refers to in MODULE."
  (multiple-value-bind (owner symbol) (resolve-symbol module (name-symbol name))
    (owned-variable owner (same-kind-name symbol name))))

(defun defining-module-var (module name form-name)
  "The variable a definition of NAME in MODULE defines.  Chapter 5: a
module may not define a variable it imports -- unless the module it comes
from names it in a create clause, which asks for exactly that (design note
32)."
  (let ((var (lookup-module-var module name)))
    (let ((owner (module-var-module var)))
      (unless (or (eq owner module)
                  (member (name-symbol (module-var-name var))
                          (dylan-module-created owner)))
        (compile-error "~a of ~a: the variable is imported from module ~a"
                       form-name (name-string name) (dylan-module-name owner))))
    var))

;;; Exports

(defun module-exports (module) (dylan-module-exports module))

;;; DEFINE-MODULE, in prefix form:
;;;
;;;   (define-module name clause ...)
;;;     clause: (use module import: all | (name (old => new) ...)
;;;                         exclude: (name ...)  prefix: "string"
;;;                         rename: ((old => new) ...)  export: all | (name ...))
;;;             (export name ...)
;;;             (create name ...)

(defun module-name-p (x)
  (and (symbolp x) x (not (keywordp x)) (not (eq x t))))

(defun keyword-options (options allowed what)
  "OPTIONS as an alist, checked: keyword/value pairs, each keyword one of
ALLOWED and given at most once."
  (let ((alist '()))
    (loop
      (cond ((null options) (return (nreverse alist)))
            ((not (and (consp options) (keywordp (car options))
                       (consp (cdr options))))
             (compile-error "~a: bad keyword options ~a" what options))
            ((not (member (car options) allowed))
             (compile-error "~a: unknown option ~a" what (car options)))
            ((assoc (car options) alist)
             (compile-error "~a: option ~a given twice" what (car options)))
            (t (push (cons (car options) (cadr options)) alist)
               (setf options (cddr options)))))))

(defun rename-pair (spec)
  (if (and (consp spec) (consp (cdr spec)) (consp (cddr spec)) (null (cdddr spec))
           (eq (cadr spec) 'dylan::=>)
           (module-name-p (car spec)) (module-name-p (caddr spec)))
      (cons (car spec) (caddr spec))
      (compile-error "define-module: bad rename ~a" spec)))

(defun all-p (x) (and (symbolp x) (string-equal (symbol-name x) "ALL")))

(defun process-use-clause (module clause)
  "Import into MODULE what the use CLAUSE says; answer the names it
exports again."
  (with-form (clause)
    (destructuring-bind (used-name &rest options) (cdr clause)
      (let* ((alist (keyword-options options
                                     '(:import :exclude :prefix :rename :export)
                                     "define-module use clause"))
             (used (define-module-now used-name))
             (exports (module-exports used))
             (import (if (assoc :import alist) (cdr (assoc :import alist)) 'all))
             (import-all (all-p import))
             (exclude (cdr (assoc :exclude alist)))
             (prefix (if (assoc :prefix alist) (cdr (assoc :prefix alist)) ""))
             (renames (mapcar #'rename-pair (cdr (assoc :rename alist))))
             (export (cdr (assoc :export alist)))
             (pairs '()))               ; (old . new)
        (when (eq used module)
          (compile-error "define-module: a module cannot use itself"))
        (unless (stringp prefix)
          (compile-error "define-module: prefix: must be a string, not ~a" prefix))
        (when (and exclude (not import-all))
          (compile-error "define-module: exclude: needs import: all"))
        (unless (or import-all (listp import))
          (compile-error "define-module: bad import: ~a" import))
        (flet ((add (old new)
                 (unless (member old exports)
                   (compile-error "define-module: ~a is not exported from ~a"
                                  old used-name))
                 (pushnew (cons old new) pairs :test #'equal))
               (prefixed (old)
                 (if (string= prefix "")
                     old
                     (dylan::dylan-user-symbol
                      (concatenate 'string (string-upcase prefix)
                                   (symbol-name old))))))
          (if import-all
              (dolist (old exports)
                (unless (or (member old exclude) (assoc old renames))
                  (add old (prefixed old))))
              (dolist (spec import)
                (cond ((module-name-p spec)
                       (unless (assoc spec renames)
                         (add spec (prefixed spec))))
                      (t (let ((pair (rename-pair spec)))
                           (add (car pair) (cdr pair)))))))
          (dolist (pair renames)
            (add (car pair) (cdr pair))))
        (dolist (pair pairs)
          (multiple-value-bind (owner name) (resolve-symbol used (car pair))
            (let ((previous (gethash (cdr pair) (dylan-module-imports module))))
              (when (and previous
                         (not (and (eq (car previous) owner)
                                   (eq (cdr previous) name))))
                (compile-error "define-module: two variables named ~a in ~a"
                               (cdr pair) (dylan-module-name module)))
              (setf (gethash (cdr pair) (dylan-module-imports module))
                    (cons owner name)))))
        (let ((imported (mapcar #'cdr pairs)))
          (cond ((null export) '())
                ((all-p export) imported)
                ((listp export)
                 (dolist (name export)
                   (unless (member name imported)
                     (compile-error "define-module: export: of ~a, which is not imported"
                                    name)))
                 export)
                (t (compile-error "define-module: bad export: ~a" export))))))))

(defun define-module-now (name)
  "Define the module NAME, if it is not defined yet, and answer it."
  (let ((module (find-module name)))
    (ecase (dylan-module-state module)
      (:defined module)
      (:defining
       (compile-error "define-module: modules ~a use each other" name))
      (:pending
       (setf (dylan-module-state module) :defining)
       (let ((*location* (or (dylan-module-location module) *location*)))
         (let ((clauses (dylan-module-clauses module))
               (exports '()))
           (dolist (clause clauses)
             (with-form (clause)
               (unless (and (consp clause) (listp (cdr clause))
                            (null (cdr (last clause))))
                 (compile-error "define-module: bad clause ~a" clause))
               (case (car clause)
                 ((dylan::use)
                  (unless (and (consp (cdr clause)) (module-name-p (cadr clause)))
                    (compile-error "define-module: use needs a module name"))
                  (setf exports (append (process-use-clause module clause)
                                        exports)))
                 ((dylan::export dylan::create)
                  (dolist (variable (cdr clause))
                    (unless (module-name-p variable)
                      (compile-error "define-module: bad variable name ~a" variable))
                    (when (gethash variable (dylan-module-imports module))
                      (compile-error "define-module: cannot ~a ~a, which is imported"
                                     (string-downcase (symbol-name (car clause)))
                                     variable))
                    (when (eq (car clause) 'dylan::create)
                      (pushnew variable (dylan-module-created module)))
                    (push variable exports)))
                 (t (compile-error "define-module: unknown clause ~a" clause)))))
           (setf (dylan-module-exports module) (remove-duplicates exports))))
       (setf (dylan-module-state module) :defined)
       module))))

(defun declare-module (form)
  "Record the module the define-module FORM defines, to be defined by
DEFINE-ALL-MODULES."
  (with-form (form)
    (unless (and (consp (cdr form)) (listp (cddr form))
                 (null (cdr (last form))))
      (compile-error "define-module: bad syntax"))
    (let ((name (cadr form)))
      (unless (module-name-p name)
        (compile-error "define-module: bad module name ~a" name))
      (when (eq name 'dylan::dylan)
        (compile-error "define-module: the dylan module cannot be redefined"))
      (let ((old (gethash name *dylan-modules*)))
        (when (and old (dylan-module-location old))
          (compile-error "define-module: module ~a is already defined at ~a"
                         name (location-string (dylan-module-location old)))))
      (let ((module (make-dylan-module name)))
        (setf (dylan-module-clauses module) (cddr form)
              (dylan-module-location module) *location*
              (dylan-module-state module) :pending)
        (setf (gethash name *dylan-modules*) module)))))

(defun define-all-modules ()
  (loop for module being the hash-values of *dylan-modules*
        when (eq (dylan-module-state module) :pending)
          do (handler-case (define-module-now (dylan-module-name module))
               (compile-error (e)
                 (note-error e)
                 ;; Go on as though it had no clauses.
                 (setf (dylan-module-state module) :defined)))))

;;; The predefined modules

(defun initialize-modules ()
  (setf *dylan-modules* (make-hash-table :test #'eq))
  (let ((dylan (make-dylan-module 'dylan::dylan)))
    (setf (gethash 'dylan::dylan *dylan-modules*) dylan)
    (dolist (name (append *dylan-module-names* *dylan-module-extension-names*))
      (let ((symbol (dylan-name name)))
        (push symbol (dylan-module-exports dylan))
        (setf (module-var-predefined-p (owned-variable dylan symbol)) t)))
    (dolist (name *dylan-module-internal-names*)
      (setf (module-var-predefined-p (owned-variable dylan (dylan-name name))) t))
    (dolist (name *dylan-module-setter-names*)
      (setf (module-var-predefined-p
             (owned-variable dylan (list 'dylan::setter (dylan-name name))))
            t)))
  (let ((user (make-dylan-module 'dylan::dylan-user)))
    (setf (dylan-module-clauses user) '((dylan::use dylan::dylan))
          (dylan-module-state user) :pending
          (gethash 'dylan::dylan-user *dylan-modules*) user)))

(defun dylan-name (string)
  "The symbol the Dylan reader reads for STRING."
  (dylan::dylan-user-symbol (dylan::canonicalize-string-for-symbol string)))

(defun dylan-var (string)
  "The DYLAN module's variable named STRING, or (setter STRING) if STRING
is a list of the word setter and a name.  The front end's expansions
refer to the library this way, so that what they mean cannot be changed by
a program's own variables of the same names."
  (owned-variable (dylan-module)
                  (if (consp string)
                      (list 'dylan::setter (dylan-name (cadr string)))
                      (dylan-name string))))
