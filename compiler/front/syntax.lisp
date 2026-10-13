;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Syntax helpers shared by the converter and the macros: names, lists,
;;;; literals, parameter lists, and the objects macros use to refer to
;;;; library variables and to make temporaries without capturing a
;;;; program's own names.

(in-package #:dylan-compiler)

(define-symbol-macro +false+ dylan::dylan%false)

(defun false-p (x) (eq x +false+))

;;; Lists

(defun proper-list-p (x)
  (loop (cond ((null x) (return t))
              ((atom x) (return nil))
              (t (setf x (cdr x))))))

(defun list-of-length-p (x n)
  (and (proper-list-p x) (= (length x) n)))

(defun list-of-at-least-p (x n)
  (and (proper-list-p x) (>= (length x) n)))

(defun require-list (x n what &optional (at-least t))
  "Signal an error naming WHAT unless X is a proper list of N (or, if
AT-LEAST, at least N) elements."
  (unless (if at-least (list-of-at-least-p x n) (list-of-length-p x n))
    (compile-error "~a: bad syntax" what)))

;;; Names

(defun parameter-marker-p (x)
  (member x '(dylan::!rest dylan::!key dylan::!next)))

(defun simple-variable-name-p (x)
  (and (symbolp x) x (not (eq x t)) (not (keywordp x))
       (not (eq x 'dylan::setter))      ; chapter 3: never a variable name
       (not (parameter-marker-p x))))

(defun variable-name-p (x)
  "Is X a variable name: a symbol, or (setter symbol)?"
  (or (simple-variable-name-p x)
      (and (consp x) (eq (car x) 'dylan::setter)
           (consp (cdr x)) (null (cddr x))
           (simple-variable-name-p (cadr x)))))

(defun require-variable-name (x what)
  (unless (variable-name-p x)
    (compile-error "~a: ~a is not a variable name" what x)))

(defun name-keyword (symbol)
  "The keyword that names a parameter or slot SYMBOL: foo: for foo."
  (dylan::remember-spelling (intern (symbol-name symbol) :keyword)
                            (dylan::dylan-symbol->string symbol)))

(defun check-unique (names what)
  (loop for (name . rest) on names
        when (member name rest :test #'equal)
          do (compile-error "~a: ~a appears twice" what
                            (if (consp name) (name-string name) name))))

;;; Literals.  Version 1 has no bignums and no complex numbers
;;; (CompilerPlan.md): an integer is 63 bits, and so are the numerator and
;;; denominator of a ratio.

(defconstant +most-positive-integer+ (1- (expt 2 62)))
(defconstant +most-negative-integer+ (- (expt 2 62)))

(defun check-integer-literal (n)
  (unless (<= +most-negative-integer+ n +most-positive-integer+)
    (compile-error "the integer ~a does not fit in 63 bits (there are no bignums)"
                   n)))

(defun check-literal (x)
  "Signal an error if X, a literal or quoted datum, holds a number this
version cannot represent."
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((walk (x)
               (cond ((integerp x) (check-integer-literal x))
                     ((typep x 'ratio)
                      (check-integer-literal (numerator x))
                      (check-integer-literal (denominator x)))
                     ((complexp x)
                      (compile-error "complex numbers are not supported: ~a" x))
                     ((gethash x seen))
                     ((consp x)
                      (setf (gethash x seen) t)
                      (walk (car x)) (walk (cdr x)))
                     ((and (vectorp x) (not (stringp x)))
                      (setf (gethash x seen) t)
                      (map nil #'walk x)))))
      (walk x))))

(defun self-evaluating-p (x)
  (or (null x) (eq x t) (false-p x) (keywordp x) (numberp x) (stringp x)
      (characterp x) (and (vectorp x) (not (stringp x)))))

;;; What macros put in the forms they make.
;;;
;;; A GLOBAL-NAME stands for a module variable directly, whatever the names
;;; in scope where the form appears: the expansion of (and a b) calls the
;;; library's id?, not a variable named id? the program may have bound.
;;; Temporaries are uninterned symbols, which no program can write.

(defstruct (global-name (:constructor make-global-name (var)))
  var)

(defun g (string)
  "A form referring to the DYLAN module's variable STRING."
  (make-global-name (dylan-var string)))

(defun temp (name)
  "A fresh variable name for a macro's own use."
  (make-symbol name))

;;; Parameter lists (chapter 6)
;;;
;;;   (required ... [#next var] [#rest var] [#key key-spec ...])
;;;
;;; A required parameter is var or (var specializer).  A key-spec is var,
;;; (var default), (keyword: var) or (keyword: var default).  #key alone
;;; accepts any keywords.

(defstruct (parsed-params (:constructor make-parsed-params
                              (required next rest keys)))
  required                              ; list of (name . specializer form)
                                        ; -- specializer NIL for none
  next                                  ; name or NIL
  rest                                  ; name or NIL
  keys)                                 ; NIL, :ANY, or list of
                                        ; (keyword name default-form)

(defun parse-parameters (params what)
  (unless (proper-list-p params)
    (compile-error "~a: bad parameter list ~a" what params))
  (let ((l params) (required '()) next rest keys)
    ;; Required parameters
    (loop while (and l (not (parameter-marker-p (car l))))
          do (let ((p (pop l)))
               (cond ((variable-name-p p) (push (cons p nil) required))
                     ((and (consp p) (variable-name-p (car p))
                           (list-of-length-p p 2))
                      (push (cons (car p) (cadr p)) required))
                     (t (compile-error "~a: bad parameter ~a" what p)))))
    (flet ((marker-var (marker)
             (when (eq (car l) marker)
               (pop l)
               (unless (and l (variable-name-p (car l)))
                 (compile-error "~a: ~a needs a variable name" what
                                (dylan-marker-string marker)))
               (pop l))))
      (setf next (marker-var 'dylan::!next)
            rest (marker-var 'dylan::!rest)))
    (when (eq (car l) 'dylan::!key)
      (pop l)
      (setf keys
            (if (null l)
                :any
                (loop while l
                      collect (parse-key-spec (pop l) what)))))
    (when l
      (compile-error "~a: unexpected ~a in parameter list" what (car l)))
    (let ((required (nreverse required)))
      (check-unique (append (mapcar #'car required)
                            (and next (list next))
                            (and rest (list rest))
                            (and (consp keys) (mapcar #'second keys)))
                    what)
      (when (consp keys)
        (check-unique (mapcar #'first keys) what))
      (make-parsed-params required next rest keys))))

(defun dylan-marker-string (marker)
  (case marker (dylan::!next "#next") (dylan::!rest "#rest") (t "#key")))

(defun parse-key-spec (spec what)
  (cond ((simple-variable-name-p spec)
         (list (name-keyword spec) spec +false+))
        ((and (consp spec) (keywordp (car spec)))
         ;; (keyword: var [default]) -- the variable may be any variable
         ;; name, a setter name included (chapter 6).
         (unless (and (proper-list-p spec) (<= 2 (length spec) 3)
                      (variable-name-p (cadr spec)))
           (compile-error "~a: bad #key parameter ~a" what spec))
         (list (car spec) (cadr spec)
               (if (cddr spec) (caddr spec) +false+)))
        ((and (consp spec) (simple-variable-name-p (car spec)))
         (unless (and (proper-list-p spec) (<= 1 (length spec) 2))
           (compile-error "~a: bad #key parameter ~a" what spec))
         (list (name-keyword (car spec)) (car spec)
               (if (cdr spec) (cadr spec) +false+)))
        (t (compile-error "~a: bad #key parameter ~a" what spec))))
