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
;;;; Modules (chapter 5, and the module defining forms of design note 32).
;;;; Thomas had none: every expression shared one namespace.  This file is
;;;; new in the Common Lisp translation.
;;;;
;;;; A module maps variable names to variables.  A name either refers to a
;;;; variable imported from a module the module uses, or else to a variable
;;;; the module owns.  The mapping is made at compile time: VARIABLE->NAME
;;;; asks RESOLVE-VARIABLE-NAME for the Lisp name of the variable a Dylan
;;;; name refers to in the current module, and from then on the compiler
;;;; works with Lisp names exactly as before.
;;;;
;;;; Two modules are predefined.  DYLAN owns the predefined variables and
;;;; exports them all.  DYLAN-USER uses DYLAN and is the module expressions
;;;; are compiled in unless a file's module: header (design note 33) or
;;;; DYLAN:IN-MODULE says otherwise.  Variables DYLAN-USER owns keep the
;;;; Lisp names they had before there were modules.

(in-package #:dylan)

(defstruct (dylan-module (:conc-name module.))
  name
  (imports (make-hash-table :test #'eq)) ; name here -> (module . name there)
  (exports '()))                         ; names exported

(defvar *dylan-modules* (make-hash-table :test #'eq))

(defvar *current-module* 'dylan-user
  "The name of the module expressions are compiled in.")

(defun find-module (name &optional (errorp t))
  (or (gethash name *dylan-modules*)
      (and errorp (dylan%error "unknown module" name))))

;;; Lisp names made for module variables, so that a name already resolved
;;; is never resolved again.
(defvar *resolved-names* (make-hash-table :test #'eq))

(defun module-variable-name (module symbol setter?)
  "The Lisp name of the variable MODULE owns under SYMBOL (or, if SETTER?,
under (SETTER SYMBOL))."
  (let ((name
          (case (module.name module)
            ((dylan)
             (cond (setter? (name->setter symbol))
                   ((cadr (assoc symbol dylan%lisp-names-of-predefined-names)))
                   ((constantp symbol) (name->lisp-safe-name symbol))
                   (t (new-name "" symbol "@dylan"))))
            ((dylan-user)
             (cond (setter? (name->setter symbol))
                   ((constantp symbol) (name->lisp-safe-name symbol))
                   (t symbol)))
            (t
             (let ((base (new-name "" symbol
                                   (concatenate 'string "@"
                                                (string-downcase
                                                 (symbol-name
                                                  (module.name module)))))))
               (if setter? (name->setter base) base))))))
    ;; A DYLAN-USER variable's Lisp name is its own symbol, which other
    ;; modules must still resolve for themselves.
    (unless (eq name symbol)
      (setf (gethash name *resolved-names*) t))
    name))

(defun resolve-symbol (module symbol)
  "The module that owns the variable SYMBOL names in MODULE, and the name
it has there."
  (let ((import (gethash symbol (module.imports module))))
    (if import
        (values (car import) (cdr import))
        (values module symbol))))

(defun check-not-imported (dylan-variable form-name)
  "Chapter 5: a module may not create a variable under a name it imports;
the conflict is an error, which cancels the definition."
  (let ((symbol (if (consp dylan-variable) (cadr dylan-variable) dylan-variable)))
    (unless (or (gethash symbol *resolved-names*) (dylan-special-name? symbol))
      (let ((owner (resolve-symbol (find-module *current-module*) symbol)))
        (unless (eq (module.name owner) *current-module*)
          (dylan%error (concatenate 'string form-name
                                    " -- the variable is imported from module")
                       dylan-variable (module.name owner)))))))

(defun resolve-variable-name (dylan-variable)
  "The Lisp name of the variable DYLAN-VARIABLE -- a symbol or (SETTER
symbol) -- refers to in the current module."
  (let ((setter? (consp dylan-variable))
        (symbol (if (consp dylan-variable) (cadr dylan-variable) dylan-variable)))
    (if (or (gethash symbol *resolved-names*) (dylan-special-name? symbol))
        (if setter? (name->setter symbol) symbol)
        (multiple-value-bind (owner name)
            (resolve-symbol (find-module *current-module*) symbol)
          (module-variable-name owner name setter?)))))

;;; Exports.  Renaming, like importing, is by symbol: a variable named FOO
;;; and one named (SETTER FOO) go together.

(defun module-exports (module)
  (if (eq (module.name module) 'dylan)
      (dylan-module-exports)
      (module.exports module)))

(defun dylan-module-exports ()
  (remove-duplicates
   (loop for (name) in dylan%lisp-names-of-predefined-names
         collect (if (symbolp name)
                     (let ((setter (setter-name-root name)))
                       (or setter name))
                     name))))

(defun setter-name-root (lisp-name)
  "FOO, if LISP-NAME is the Lisp name of (SETTER FOO)."
  (let ((string (symbol-name lisp-name)))
    (when (and (> (length string) 14)
               (string-equal "dylan/setter/" string :end2 13)
               (char= #\/ (char string (- (length string) 1))))
      (intern (subseq string 13 (- (length string) 1)) '#:dylan))))

;;; DEFINE-MODULE (design note 32), in prefix form:
;;;
;;;   (define-module name clause ...)
;;;     clause: (use module import: all | (name (old => new) ...)
;;;                         exclude: (name ...)  prefix: "string"
;;;                         rename: ((old => new) ...)  export: all | (name ...))
;;;             (export name ...)
;;;             (create name ...)

(defun rename-pair (spec)
  (if (and (list-of-length? spec 3) (eq (cadr spec) '=>)
           (symbolp (car spec)) (symbolp (caddr spec)))
      (cons (car spec) (caddr spec))
      (dylan%error "define-module -- bad rename" spec)))

(defun process-use-clause (module clause)
  "Import into MODULE what CLAUSE says; answer the names it imports."
  (destructuring-bind (used-name &rest options) clause
    (validate-keywords options '(:import :exclude :prefix :rename :export)
                       #'dylan%error)
    (let* ((used (find-module used-name))
           (exports (module-exports used))
           (import (dylan%find-keyword options :import (lambda () 'all)))
           (exclude (dylan%find-keyword options :exclude (lambda () '())))
           (prefix (dylan%find-keyword options :prefix (lambda () "")))
           (renames (mapcar #'rename-pair
                            (dylan%find-keyword options :rename (lambda () '()))))
           (export (dylan%find-keyword options :export (lambda () '())))
           (pairs '()))                 ; (old . new)
      (when (eq used module)
        (dylan%error "define-module -- a module cannot use itself" used-name))
      (unless (stringp prefix)
        (dylan%error "define-module -- prefix: must be a string" prefix))
      (when (and exclude (not (eq import 'all)))
        (dylan%error "define-module -- exclude: needs import: all" clause))
      (flet ((add (old new)
               (unless (member old exports)
                 (dylan%error "define-module -- not exported" old used-name))
               (pushnew (cons old new) pairs :test #'equal))
             (prefixed (old)
               (if (string= prefix "")
                   old
                   (intern (concatenate 'string (string-upcase prefix)
                                        (symbol-name old))
                           '#:dylan))))
        (if (eq import 'all)
            (dolist (old exports)
              (unless (or (member old exclude) (assoc old renames))
                (add old (prefixed old))))
            (dolist (spec import)
              (cond ((symbolp spec)
                     (unless (assoc spec renames)
                       (add spec (prefixed spec))))
                    (t (let ((pair (rename-pair spec)))
                         (add (car pair) (cdr pair)))))))
        (dolist (pair renames)
          (add (car pair) (cdr pair))))
      ;; Each name now refers to the variable the old name refers to in the
      ;; used module.
      (dolist (pair pairs)
        (multiple-value-bind (owner name) (resolve-symbol used (car pair))
          (let ((previous (gethash (cdr pair) (module.imports module))))
            (when (and previous
                       (not (and (eq (car previous) owner)
                                 (eq (cdr previous) name))))
              (dylan%error "define-module -- two variables with one name"
                           (cdr pair) (module.name module)))
            (setf (gethash (cdr pair) (module.imports module))
                  (cons owner name)))))
      (let ((imported (mapcar #'cdr pairs)))
        (cond ((eq export 'all) imported)
              ((listp export)
               (dolist (name export)
                 (unless (member name imported)
                   (dylan%error "define-module -- export: of a name not imported"
                                name used-name)))
               export)
              (t (dylan%error "define-module -- bad export:" export)))))))

(defun dylan%define-module (name clauses)
  "Define (or redefine) the module NAME; answer NAME."
  (when (eq name 'dylan)
    (dylan%error "define-module -- the dylan module cannot be redefined"))
  (unless (and (symbolp name) name (not (keyword? name)))
    (dylan%error "define-module -- bad module name" name))
  (let ((module (make-dylan-module :name name))
        (exports '()))
    (dolist (clause clauses)
      (unless (and (consp clause) (null (cdr (cl:last clause))))
        (dylan%error "define-module -- bad clause" clause))
      (case (car clause)
        ((use)
         (unless (cdr clause)
           (dylan%error "define-module -- use needs a module name" clause))
         (setf exports
               (append (process-use-clause module (cdr clause)) exports)))
        ((export create)
         (dolist (variable (cdr clause))
           (unless (and (symbolp variable) (variable-name? variable))
             (dylan%error "define-module -- bad variable name" variable))
           (push variable exports)))
        (t (dylan%error "define-module -- unknown clause" clause))))
    ;; Owned variables may not also be imported.
    (dolist (clause clauses)
      (when (member (car clause) '(export create))
        (dolist (variable (cdr clause))
          (when (gethash variable (module.imports module))
            (dylan%error "define-module -- cannot export or create an imported variable"
                         variable name)))))
    (setf (module.exports module) (remove-duplicates exports))
    (setf (gethash name *dylan-modules*) module)
    name))

(defun compile-define-module-form (forms module-vars bound-vars really-compile
                                   multiple-values? continue)
  (declare (ignore bound-vars really-compile multiple-values?))
  (must-be-list-of-at-least-length forms 1 "DEFINE-MODULE: invalid syntax")
  ;; The module exists from here on in this compilation, and again
  ;; whenever the compiled code is loaded.
  (dylan%define-module (car forms) (cdr forms))
  (funcall continue
           `(dylan%define-module ',(car forms) ',(cdr forms))
           module-vars))

(defun in-module (name)
  "Compile subsequent expressions in the module NAME."
  (let ((name (if (stringp name) (intern (string-upcase name) '#:dylan) name)))
    (find-module name)
    (setf *current-module* name)))

;;; A Dylan source file may begin with a header (design note 33): lines of
;;; the form "keyword: value", ended by a blank line.  The module: keyword
;;; names the module the file's expressions are compiled in; the others are
;;; ignored.

(defun header-keyword-line-p (line)
  (let ((colon (position #\: line)))
    (and colon (> colon 0)
         (alpha-char-p (char line 0))
         (every (lambda (c) (or (alphanumericp c) (char= c #\-)))
                (subseq line 0 colon)))))

(defun read-source-header (stream)
  "Read the header of a Dylan source file, if it has one, and answer the
module it names (or NIL).  Without a header the stream is left untouched."
  (let ((start (file-position stream))
        (first (peek-char nil stream nil nil)))
    (if (not (and first (alpha-char-p first)))
        nil
        (let ((first-line (read-line stream nil "")))
          (if (not (header-keyword-line-p first-line))
              (progn (file-position stream start) nil)
              (let ((module nil))
                (loop for line = first-line then (read-line stream nil "")
                      until (string= (string-trim " 	" line) "")
                      do (when (header-keyword-line-p line)
                           (let ((colon (position #\: line)))
                             (when (string-equal (subseq line 0 colon) "module")
                               (setf module (string-trim " 	"
                                                         (subseq line
                                                                 (1+ colon))))))))
                module))))))

(defun initialize-modules ()
  (clrhash *dylan-modules*)
  (setf (gethash 'dylan *dylan-modules*) (make-dylan-module :name 'dylan))
  (dylan%define-module 'dylan-user '((use dylan)))
  (setf *current-module* 'dylan-user))

;;; INITIALIZE-MODULES is called at the end of compiler.lisp, once the
;;; predefined variables the DYLAN module exports are known.
