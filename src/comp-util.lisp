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
;;;; Utility procedures for use at compile time only
;;;;                                       (was Thomas's src/comp-util.scm)
;;;;
;;;; Name operations: new-name, name->module-getter, name->module-setter,
;;;;                  name->setter, variable->name, variable-name?,
;;;;                  name->keyword, dylan-special-name?
;;;; General: cant-parse, list-of-length?, list-of-at-least-length?,
;;;;          must-be-list-of-length, must-be-list-of-at-least-length,
;;;;          set-difference
;;;; Compiler specific: module-refs, add-variable, add-module-variable

(in-package #:dylan)

;;; Name operations.  These convert from Dylan names (including (SETTER x)
;;; and Keyword:) to Lisp forms.
;;;
;;; NEW-NAME is in support.lisp, since it is used at runtime as well as at
;;; compile time.

(defun name->module-getter (name)
  (new-name "dylan/module-get/" (variable->name name) "/"))

(defun name->module-setter (name)
  (new-name "dylan/module-set/" (variable->name name) "/!"))

(defun name->setter (name)
  (new-name "dylan/setter/" name "/"))

(defun name->lisp-safe-name (name)
  (new-name "dylan/lisp-safe/" name "/"))

(defun variable->name (dylan-variable)
  "The Scheme renamed Dylan variables that clashed with Scheme's syntactic
keywords.  Compiled Dylan variables never appear in operator position here
-- every Dylan call goes through DYLAN%APPLY -- so the only names that need
renaming are the ones Common Lisp will not let us bind at all: T, PI and
any other constant."
  (if (symbolp dylan-variable)
      (let ((entry (assoc dylan-variable dylan%lisp-names-of-predefined-names)))
        (cond (entry (cadr entry))
              ((constantp dylan-variable) (name->lisp-safe-name dylan-variable))
              (t dylan-variable)))
      (name->setter (cadr dylan-variable))))

;;; Module (that is, global) variables get a name of their own, so that a
;;; Dylan module variable can never be captured by -- or capture -- a Dylan
;;; lexical variable of the same name.  In the Scheme both were simply the
;;; Dylan name; in Common Lisp the module variable has to be proclaimed
;;; special, which would have made every like-named lexical binding dynamic.
;;; Predefined runtime variables keep their own names, since the runtime
;;; already defines them.

(defun name->module-variable (name)
  (if (member name dylan%predefined-variables)
      name
      (new-name "!module/" name "/")))

(defun variable-name? (x)
  (flet ((simple-variable-name? (x)
           (and (symbolp x)
                (not (null x))
                (not (keyword? x))
                (not (dylan-special-name? x)))))
    (or (simple-variable-name? x)
        (and (list-of-length? x 2)
             (eq (car x) 'setter)
             (simple-variable-name? (cadr x))))))

(defun name->keyword (symbol)
  (intern (symbol-name symbol) :keyword))

(defun dylan-special-name? (x)
  "Dylan's #rest and friends are simulated with !rest, so variable names
beginning with \"!\" are precluded, as are those beginning with the two
prefixes the runtime reserves for itself."
  (and (symbolp x)
       (not (null x))
       (let* ((string (symbol-name x))
              (length (length string)))
         (or (and (> length 0) (char= #\! (char string 0)))
             (and (> length 5)
                  (or (string-equal "dylan/" string :end2 6)
                      (string-equal "dylan%" string :end2 6)))))))

;;; KEYWORD? is used at runtime: see support.lisp

;;; General support operations needed only at compile time

(defun cant-parse (reason orig-l l)
  (dylan%error (concatenate 'string "illegal parameter list in " reason)
               orig-l l))

(defun list-of-length? (l n)
  (loop (cond ((and (= n 0) (null l)) (return t))
              ((or (= n 0) (not (consp l))) (return nil))
              (t (pop l) (decf n)))))

(defun list-of-at-least-length? (l n)
  (and (listp l)
       (null (cdr (cl:last l)))         ; a proper list
       (loop (cond ((= n 0) (return t))
                   ((not (consp l)) (return nil))
                   (t (pop l) (decf n))))))

(defun must-be-list-of-length (l n error-message)
  (or (list-of-length? l n)
      (dylan%error error-message l)))

(defun must-be-list-of-at-least-length (l n error-message)
  (or (list-of-at-least-length? l n)
      (dylan%error error-message l)))

(defun module-refs (variable bound-vars module-vars continue core)
  "Called to generate code to deal with direct references to module
variables.  VARIABLE is the variable name, CORE is a procedure to generate
the main body."
  (let* ((name (variable->name variable))
         (hidden? (member name bound-vars)))
    (funcall
     continue
     (if hidden?
         `(let ((!old-value (,(name->module-getter name))))
            ,(funcall core '!old-value
                      (lambda (val) `(,(name->module-setter name) ,val))))
         (let ((global (name->module-variable name)))
           (funcall core global (lambda (val) `(setq ,global ,val)))))
     (add-module-variable name hidden? module-vars))))

;;; Compile a list of forms, producing a list of corresponding Lisp forms
;;; and the (updated) module variables.

(defun add-variable (name bound-vars module-vars)
  (if (or (member name bound-vars)
          (member name dylan%predefined-variables))
      module-vars
      (adjoin name module-vars #'member)))

(defun add-module-variable (name require-accessor-fns module-vars)
  (if (or require-accessor-fns (not (member name dylan%predefined-names)))
      (adjoin name module-vars #'member)
      module-vars))

(defun must-be-unique (objects predicate error-string)
  (unless (unique? objects predicate)
    (dylan%error error-string (car objects) objects)))
