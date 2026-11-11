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
any other constant.  Which variable a name refers to depends on the
current module: see comp-module.lisp."
  (resolve-variable-name dylan-variable))

;;; Dylan lexical variables are bound under the Lisp name VARIABLE->NAME
;;; gives them, unless that name is a constant or a special variable --
;;; most often one of the runtime's own DYLAN/X globals, since a Dylan
;;; variable named LIST is DYLAN/LIST.  Binding a special would make the
;;; Dylan binding dynamic and replace the runtime's value for its extent,
;;; so such variables get a name of their own instead.  Every binding site
;;; and every reference goes through VARIABLE->LEXICAL, and BOUND-VARS
;;; holds these lexical names.

(defun name->lexical (name)
  ;; Also Lisp's lambda-list keywords, which Dylan names &rest and &key
  ;; read as, and which cannot be bound by LAMBDA.
  (if (or (member name dylan%predefined-variables)
          (constantp name)
          (globally-special-p name)
          (member name lambda-list-keywords))
      (new-name "!local/" name "/")
      name))

(defun variable->lexical (dylan-variable)
  (name->lexical (variable->name dylan-variable)))

(defvar *unfilled-lexicals* '()
  "The lexical names of a bind-methods form whose methods are being
compiled: a reference to one checks that its method has been made
(DYLAN%FILLED), since a specializer may name a method made after it.")

;;; Module (that is, global) variables get a name of their own, so that a
;;; Dylan module variable can never be captured by -- or capture -- a Dylan
;;; lexical variable of the same name.  In the Scheme both were simply the
;;; Dylan name; in Common Lisp the module variable has to be proclaimed
;;; special, which would have made every like-named lexical binding dynamic.
;;; Predefined runtime variables keep their own names, since the runtime
;;; already defines them -- but for the program's copy of a library
;;; variable it may assign (PROGRAM-VARIABLE, runtime-internal.lisp).

(defun name->module-variable (name)
  (if (member name dylan%predefined-variables)
      (or (get name 'program-variable) name)
      (new-name "!module/" name "/")))

(defun simple-variable-name? (x)
  (and (symbolp x)
       (not (null x))
       (not (keyword? x))
       (not (eq x t))                   ; #t, which a Dylan name T is not
       (not (eq x 'setter))             ; Chapter 3: never a variable name
       (not (dylan-special-name? x))))

(defun variable-name? (x)
  (or (simple-variable-name? x)
      (and (list-of-length? x 2)
           (eq (car x) 'setter)
           (simple-variable-name? (cadr x)))))

(defun name->keyword (symbol)
  "The keyword a call writes for the parameter SYMBOL, NAME: for the
variable NAME.  The reader makes that keyword of the name as written,
where it makes the variable's name t or nil for T and NIL, and puts %%
before one that looks like the compiler's own (DYLAN-USER-SYMBOL)."
  (let ((name (symbol-name symbol)))
    (intern (cond ((member name '("t" "nil") :test #'string=)
                   (string-upcase name))
                  ((and (>= (length name) 2) (string= "%%" name :end2 2))
                   (subseq name 2))
                  (t name))
            :keyword)))

(defun dylan-special-name? (x)
  "Is X one of the compiler's own names?  Dylan's #rest and friends are
read as !rest and so on, and the runtime's names begin DYLAN/ or DYLAN%.
A Dylan program cannot write such a name: the reader gives a symbol that
looks like one a name of its own (see DYLAN-USER-SYMBOL)."
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

;;; A special form's shape, checked as the compiler to C checks it, with
;;; its words: "WHAT: bad syntax".  N counts the elements after the
;;; form's name: at least N, or, if EXACT, N.
(defun must-have-syntax (l n what &optional exact)
  (or (if exact (list-of-length? l n) (list-of-at-least-length? l n))
      (dylan%error (concatenate 'string what ": bad syntax"))))

(defun proper-list? (x)
  (list-of-at-least-length? x 0))

(defun syntax-error (what control &rest arguments)
  "Signal a special form's syntax error as the compiler to C words it:
\"WHAT: \" and CONTROL, a format control of ~a directives."
  (apply #'dylan%error (concatenate 'string what ": " control) arguments))

(defun check-keyword-options (options allowed what)
  "Check OPTIONS, keyword/value pairs, each keyword one of ALLOWED and
given at most once, as the compiler to C checks them, with its words."
  (let ((seen '()))
    (loop (cond ((null options) (return))
                ((not (and (consp options) (keyword? (car options))
                           (consp (cdr options))))
                 (syntax-error what "bad keyword options ~a" options))
                ((not (member (car options) allowed))
                 (syntax-error what "unknown option ~a" (car options)))
                ((member (car options) seen)
                 (syntax-error what "option ~a given twice" (car options)))
                (t (push (car options) seen)
                   (setf options (cddr options)))))))

(defun require-variable-name (x what)
  (unless (variable-name? x)
    (syntax-error what "~a is not a variable name" x)))

;;; A method's parameter list (chapter 6), checked as the compiler to C
;;; checks it (compiler/front/syntax.lisp, PARSE-PARAMETERS), with its
;;; words; WHAT names the form: method, define-method or bind-methods.
;;;
;;;   (required ... [#next var] [#rest var] [#key key-spec ...])
(defun check-parameter-list (params what)
  (unless (proper-list? params)
    (syntax-error what "bad parameter list ~a" params))
  (let ((l params) (names '()) (keywords '()))
    (loop while (and l (not (member (car l) '(!next !rest !key))))
          do (let ((p (pop l)))
               (cond ((variable-name? p) (push p names))
                     ((and (consp p) (variable-name? (car p))
                           (list-of-length? p 2))
                      (push (car p) names))
                     (t (syntax-error what "bad parameter ~a" p)))))
    (dolist (marker '(!next !rest))
      (when (eq (car l) marker)
        (pop l)
        (unless (and l (variable-name? (car l)))
          (syntax-error what "~a needs a variable name" marker))
        (push (pop l) names)))
    (when (eq (car l) '!key)
      (pop l)
      (loop while l
            do (let ((spec (pop l)))
                 (cond ((simple-variable-name? spec)
                        (push (name->keyword spec) keywords)
                        (push spec names))
                       ;; (keyword: var [default]): any variable name
                       ((and (consp spec) (keyword? (car spec)))
                        (unless (and (proper-list? spec) (<= 2 (length spec) 3)
                                     (variable-name? (cadr spec)))
                          (syntax-error what "bad #key parameter ~a" spec))
                        (push (car spec) keywords)
                        (push (cadr spec) names))
                       ((and (consp spec) (simple-variable-name? (car spec)))
                        (unless (and (proper-list? spec) (<= 1 (length spec) 2))
                          (syntax-error what "bad #key parameter ~a" spec))
                        (push (name->keyword (car spec)) keywords)
                        (push (car spec) names))
                       (t (syntax-error what "bad #key parameter ~a" spec))))))
    (when l
      (syntax-error what "unexpected ~a in parameter list" (car l)))
    (check-names-once what (reverse names))
    (check-names-once what (reverse keywords))))

(defun module-refs (variable bound-vars module-vars continue core)
  "Called to generate code to deal with direct references to module
variables.  VARIABLE is the variable name, CORE is a procedure to generate
the main body."
  (let* ((name (variable->name variable))
         (hidden? (member (name->lexical name) bound-vars)))
    (funcall
     continue
     (if hidden?
         ;; The getter is called at each reference, not once beforehand:
         ;; CORE may read the variable again after setting it.
         (funcall core `(,(name->module-getter name))
                  (lambda (val) `(,(name->module-setter name) ,val)))
         (let ((global (name->module-variable name)))
           (funcall core global (lambda (val) `(setq ,global ,val)))))
     (add-module-variable name hidden? module-vars))))

;;; Compile a list of forms, producing a list of corresponding Lisp forms
;;; and the (updated) module variables.

(defun add-variable (name bound-vars module-vars)
  (if (or (member (name->lexical name) bound-vars)
          (member name dylan%predefined-variables))
      module-vars
      (adjoin name module-vars #'member)))

(defun add-module-variable (name require-accessor-fns module-vars)
  (if (or require-accessor-fns (not (member name dylan%predefined-names)))
      (adjoin name module-vars #'member)
      module-vars))

(defun check-names-once (form-name names)
  "Signal an error, naming FORM-NAME, if one of the variable NAMES a form
binds appears twice."
  (loop for tail on names
        when (member (car tail) (cdr tail) :test #'equal)
          do (dylan%error (concatenate 'string form-name ": ~a appears twice")
                          (car tail))))

(defun must-be-unique (objects predicate error-string)
  (unless (unique? objects predicate)
    (dylan%error error-string (car objects) objects)))
