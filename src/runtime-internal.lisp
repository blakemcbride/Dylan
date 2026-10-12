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
;;;; This file contains the functions that are referenced only directly by
;;;; the output of the Dylan to Lisp compiler, rather than by user code
;;;; written in Dylan.
;;;;                                (was Thomas's src/runtime-internal.scm)
;;;;
;;;; Many of the functions here are just renamings of ordinary Lisp
;;;; functions.  The renaming keeps the compiler's output recognisable and
;;;; insulates it from redefinition of the corresponding Dylan names.

(in-package #:dylan)

(declaim (inline dylan%free-variable-ref))
(defun dylan%free-variable-ref (value name)
  (if (eq value the-unassigned-value)
      (dylan-call dylan/error "unbound variable" name)
      value))

;;; Read-only module variables (chapter 5): DEFINE-CLASS and
;;; DEFINE-GENERIC-FUNCTION make them, and SET! and DEFINE may not assign
;;; them.  The defining forms themselves assign without the check, so a
;;; class or generic function can still be redefined.  The predefined
;;; classes are read-only too.  The predefined generic functions are not:
;;; chapter 7's own example assigns (SETTER ELEMENT).

(defvar *read-only-variables* (make-hash-table :test #'eq))

(do-symbols (symbol '#:dylan)
  (when (and (eq (symbol-package symbol) (find-package '#:dylan))
             (boundp symbol)
             (class? (symbol-value symbol))
             (eq symbol (class.debug-name (symbol-value symbol))))
    (setf (gethash symbol *read-only-variables*) t)))

(defun dylan%make-variable-read-only (variable)
  (setf (gethash variable *read-only-variables*) t))

(defun dylan%check-assignable (variable name)
  (when (gethash variable *read-only-variables*)
    (dylan-call dylan/error "variable is read-only" name)))

(defun dylan%note-redefinition (variable name)
  "Chapter 4: DEFINE of a variable that already has a value warns."
  (when (and (boundp variable)
             (not (eq (symbol-value variable) the-unassigned-value)))
    (implementation-specific/warning "redefining variable ~a" (list name))))

(defun dylan%check-generic-function-definition (generic-function name
                                                nrequired keys rest?)
  "Signal an error unless a DEFINE-GENERIC-FUNCTION of NAME with this
parameter list is consistent with the existing GENERIC-FUNCTION."
  (let ((data (generic-function-data generic-function))
        (keys (guarantee-keys keys)))
    (flet ((same-keys? (a b)
             (cond ((or (null a) (null b)) (and (null a) (null b)))
                   ((or (vectorp a) (vectorp b)) (and (vectorp a) (vectorp b)))
                   (t (and (subsetp a b) (subsetp b a))))))
      (unless (and (= nrequired (generic-function-data.nrequired data))
                   (eq rest? (generic-function-data.rest? data))
                   (same-keys? keys (generic-function-data.keys data)))
        (dylan-call dylan/error
                    "define-generic-function -- inconsistent with the existing generic function"
                    name)))))

(defun dylan%call/cc (receiver)
  "Used for BIND-EXIT and HANDLER-CASE.  Dylan only ever escapes with its
continuations, so a CATCH/THROW pair suffices."
  (call-with-escape-continuation receiver))

(defun dylan%dotimes (count result-fn fn)
  ;; Used for the DOTIMES special form
  (loop for n from 0
        do (if (>= n count)
               (return (funcall result-fn n))
               (funcall fn n))))

(defun dylan%while (test thunk)
  ;; Used for the UNTIL and WHILE special forms
  (loop while (funcall test) do (funcall thunk) finally (return dylan%false)))

;;; A combination.  The operands are evaluated left to right, then the
;;; operator, as DYLAN%APPLY below does with thunks (it remains for code
;;; compiled before DYLAN%CALL existed).  A call of one of the functions in
;;; *FAST-PATHS* with the right number of arguments is compiled inline for
;;; the arguments its fast paths handle, while their guards hold.

(defvar *fast-paths* '()
  "Entries (OPERATOR NARGS GUARD TEST-FN EXPANSION-FN): when TEST-FN's form
of the argument variables is true and GUARD is, the call is EXPANSION-FN's
form of them.  NARGS :N-ARY means any number of arguments from two up, and
the functions then take the list of argument variables.  An operator may
have several entries for the same NARGS, tried in the order defined.")

(defmacro define-fast-path (operator guard args test expansion)
  "ARGS is a list of variables, or a single variable for an :N-ARY fast
path."
  (let ((nargs (if (listp args) (length args) :n-ary))
        (lambda-list (if (listp args) args (list args))))
    `(progn
       (defvar ,guard nil)
       (setf *fast-paths*
             (append (remove-if (lambda (entry)
                                  (and (eq (first entry) ',operator)
                                       (eql (second entry) ',nargs)
                                       (eq (third entry) ',guard)))
                                *fast-paths*)
                     (list (list ',operator ',nargs ',guard
                                 (lambda ,lambda-list ,test)
                                 (lambda ,lambda-list ,expansion)))))
       ',operator)))

(defmacro dylan%call (multiple-values? operator &rest operands)
  (let* ((vars (loop repeat (length operands) collect (gensym "ARG")))
         (nargs (length operands))
         (fast (and (symbolp operator)
                    (or (remove-if-not (lambda (entry)
                                         (and (eq (first entry) operator)
                                              (eql (second entry) nargs)))
                                       *fast-paths*)
                        (and (>= nargs 2)
                             (remove-if-not (lambda (entry)
                                              (and (eq (first entry) operator)
                                                   (eq (second entry) :n-ary)))
                                            *fast-paths*)))))
         (general `(funcall ,operator ,multiple-values?
                            next-method/not-generic ,@vars)))
    (flet ((call (entry fn)
             (if (eq (second entry) :n-ary)
                 (funcall fn vars)
                 (apply fn vars))))
      `(let* ,(mapcar #'list vars operands)
         (cond ,@(mapcar (lambda (entry)
                           `((and ,(third entry) ,(call entry (fourth entry)))
                             ,(call entry (fifth entry))))
                         fast)
               (t ,general))))))

(defmacro define-real-fast-paths (guard &rest operators-and-lisp-functions)
  "Fast paths for two reals, answering what the Lisp function does."
  `(progn
     ,@(loop for (operator lisp-function boolean?) in operators-and-lisp-functions
             collect `(define-fast-path ,operator ,guard (a b)
                        `(and (realp ,a) (realp ,b))
                        ,(if boolean?
                             ``(bool (,',lisp-function ,a ,b))
                             ``(,',lisp-function ,a ,b))))))

(define-real-fast-paths *fast-binary+* (dylan/+ +))
(define-real-fast-paths *fast-binary-* (dylan/- -))
(define-real-fast-paths *fast-binary** (dylan/* *))
;;; More than two: + and * fold their arguments left to right, as the
;;; Dylan functions do.
(define-fast-path dylan/+ *fast-binary+* args
  `(and ,@(mapcar (lambda (a) `(realp ,a)) args))
  (cl:reduce (lambda (x y) `(+ ,x ,y)) args))
(define-fast-path dylan/* *fast-binary** args
  `(and ,@(mapcar (lambda (a) `(realp ,a)) args))
  (cl:reduce (lambda (x y) `(* ,x ,y)) args))
(define-real-fast-paths *fast-binary<* (dylan/< < t) (dylan/>= >= t))
(define-real-fast-paths *fast-binary=* (dylan/= = t))
(define-real-fast-paths *fast-binary<=* (dylan/> > t) (dylan/<= <= t))

;;; Characters compare by code (binary< and binary= on characters).
(defmacro define-character-fast-paths (&rest operators-and-lisp-functions)
  `(progn
     ,@(loop for (operator lisp-function) in operators-and-lisp-functions
             collect `(define-fast-path ,operator *fast-character-compare* (a b)
                        `(and (characterp ,a) (characterp ,b))
                        `(bool (,',lisp-function ,a ,b))))))

(define-character-fast-paths (dylan/= char=) (dylan/< char<) (dylan/> char>)
  (dylan/<= char<=) (dylan/>= char>=))

;;; Integers only: for them MODULO (by way of FLOOR/) and REMAINDER answer
;;; what MOD and REM do.
(define-fast-path dylan/modulo *fast-modulo* (a b)
  `(and (integerp ,a) (integerp ,b) (not (zerop ,b)))
  `(mod ,a ,b))
(define-fast-path dylan/remainder *fast-remainder* (a b)
  `(and (integerp ,a) (integerp ,b) (not (zerop ,b)))
  `(rem ,a ,b))

(define-fast-path dylan/element *fast-element* (v i)
  `(and (simple-vector-p ,v) (typep ,i 'fixnum)
        (< -1 ,i (length (the simple-vector ,v))))
  `(svref ,v ,i))
(define-fast-path dylan/element *fast-string-element* (s i)
  `(and (stringp ,s) (typep ,i 'fixnum) (< -1 ,i (length (the string ,s))))
  `(char ,s ,i))
(define-fast-path dylan/setter/element/ *fast-setter-element* (v i x)
  `(and (simple-vector-p ,v) (typep ,i 'fixnum)
        (< -1 ,i (length (the simple-vector ,v))))
  `(setf (svref ,v ,i) ,x))
(define-fast-path dylan/setter/element/ *fast-string-setter-element* (s i c)
  `(and (stringp ,s) (typep ,i 'fixnum) (< -1 ,i (length (the string ,s)))
        (characterp ,c))
  `(setf (char ,s ,i) ,c))

(defun dylan%apply (multiple-values? operator-thunk &rest operand-thunks)
  "Used for combinations.  Forces left-to-right evaluation, and adds an
initial NIL next-method argument."
  (let ((rands (mapcar #'funcall operand-thunks)))
    (apply (funcall operator-thunk)
           multiple-values?
           next-method/not-generic
           rands)))

(defun dylan%lisp-apply (fn &rest args)
  (apply #'apply fn args))

(defun dylan%dynamic-wind (before thunk after) ; Used for UNWIND-PROTECT
  (funcall before)
  (unwind-protect (funcall thunk) (funcall after)))

(defun dylan%type-check (value class)   ; Used for BIND
  ;; Should signal a <type-error>!
  (dylan-call dylan/check-type value class))

(defun dylan%list (&rest args) args)                    ; BIND
(defun dylan%cons (a b) (cons a b))                     ; BIND
(defun dylan%car (pair) (car pair))                     ; BIND
(defun dylan%vector (&rest args) (coerce args 'simple-vector)) ; BIND
(defun dylan%vector-ref (v i) (aref v i))               ; BIND
(defun dylan%not (x) (not x))           ; DEFINE-GENERIC-FUNCTION, UNLESS
(defun dylan%eq? (a b) (eq a b))                        ; DEFINE-GENERIC-FUNCTION
(defun dylan%class? (x) (class? x))                     ; DEFINE-CLASS
(defun dylan%make-param-list (required next? rest? keys) ; METHOD
  (make-param-list required next? rest? keys))
(defun dylan%add-method (generic-function method)       ; DEFINE-METHOD
  (add-method generic-function method))
(defun dylan%null? (x) (null x))                        ; COND

(defvar *fast-iterate-vector* nil)
(defvar *fast-iterate-list* nil)
(defvar *fast-iterate-range* nil)
(defvar *fast-key=* nil)                ; See runtime-collections-table.lisp

(defun dylan%for-each (fn &rest collections)
  ;; A single simple vector or proper list is walked directly while no
  ;; method of the iteration protocol but the predefined ones could apply
  ;; to it (see runtime-fast-paths.lisp).
  (let ((collection (car collections)))
    (when (null (cdr collections))
      (flet ((visit (element)
               (let ((result (funcall fn nil next-method/not-generic element)))
                 (when (and result (car result))
                   (return-from dylan%for-each (funcall (car result)))))))
        (cond ((and (simple-vector-p collection) *fast-iterate-vector*)
               (dotimes (i (length collection))
                 (visit (svref collection i)))
               (return-from dylan%for-each dylan%false))
              ((and (listp collection) *fast-iterate-list*
                    (>= (dylan-list-length collection) 0))
               (loop for tail = collection then (cdr tail)
                     while (consp tail)
                     do (visit (car tail)))
               (return-from dylan%for-each dylan%false))
              ;; Any other single collection, by the iteration protocol
              ;; (as COLLECTIONS-ITERATE does), without its lists.
              ((subclass? (get-type collection) <collection>)
               (loop for state = (initial-state collection)
                       then (next-state collection state)
                     while state
                     do (visit (current-element collection state)))
               (return-from dylan%for-each dylan%false))))))
  (dolist (collection collections)
    (unless (subclass? (get-type collection) <collection>)
      (dylan-call dylan/error "for-each -- not a collection" collection)))
  (collections-iterate fn
                       (lambda (result) (and result (car result)))
                       dylan%false
                       collections))

(defun dylan%quasi (skeleton values)
  "Build the value of a backquote from its SKELETON (see QUASI-SKELETON in
compiler.lisp) and the VALUES of its unquoted expressions."
  (labels ((marker? (x name)
             (and (consp x) (eq (car x) name)
                  (consp (cdr x)) (integerp (cadr x)) (null (cddr x))))
           (value (x) (aref values (cadr x)))
           (contains-marker? (x)
             (cond ((or (marker? x 'dylan%qq-value) (marker? x 'dylan%qq-splice)
                        (marker? x 'dylan%qq-tail))
                    t)
                   ((consp x) (or (contains-marker? (car x))
                                  (contains-marker? (cdr x))))
                   ((and (vectorp x) (not (stringp x)))
                    (some #'contains-marker? x))
                   (t nil)))
           (fill-in (x)
             (cond ((not (contains-marker? x)) x)
                   ((marker? x 'dylan%qq-value) (value x))
                   ((consp x)
                    (let ((result '()) (tail nil))
                      (loop for rest = x then (cdr rest)
                            while (consp rest)
                            do (let ((e (car rest)))
                                 (cond ((marker? e 'dylan%qq-splice)
                                        (setf result
                                              (revappend
                                               (iterate->list #'identity
                                                              (value e))
                                               result)))
                                       ((marker? e 'dylan%qq-tail)
                                        (setf tail (value e)))
                                       (t (push (fill-in e) result))))
                            finally (unless (null rest) (setf tail rest)))
                      (nreconc result tail)))
                   (t (coerce (fill-in (coerce x 'list)) 'simple-vector)))))
    (fill-in skeleton)))
