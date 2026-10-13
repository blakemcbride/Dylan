;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Macros, expanded at compile time.
;;;;
;;;; The manual calls and, or, cond, case, select, when, unless, while,
;;;; until, for, for-each, dotimes, handler-bind and handler-case macros
;;;; (chapters 8 and 13); they are defined here as rewrites of one form
;;;; into another, the way a program's own macros will be when the language
;;;; has them.  A macro is a function from a form to its expansion, kept in
;;;; *MACROS* under its name.  Expansions refer to library variables
;;;; through G and make their temporaries with TEMP (syntax.lisp), so a
;;;; program's own variables can neither capture nor be captured by them.
;;;;
;;;; Loops expand into local methods that call themselves in tail
;;;; position, which the back end turns into jumps.

(in-package #:dylan-compiler)

(defvar *macros* (make-hash-table :test #'eq)
  "Macro name -> function of the whole form answering its expansion.")

(defun find-macro (name) (gethash name *macros*))

(defun expand-macro (form)
  (funcall (gethash (car form) *macros*) form))

(defmacro define-dylan-macro (name (form) &body body)
  `(setf (gethash ',name *macros*)
         (lambda (,form) ,@body)))

(defun body-form (forms)
  "FORMS as one form: a begin, or #f if there are none."
  (cond ((null forms) +false+)
        ((null (cdr forms)) (car forms))
        (t `(dylan::begin ,@forms))))

;;; Conditionals (chapter 8)

(define-dylan-macro and (form)
  (require-list form 1 "and")
  (labels ((walk (forms)
             (cond ((null forms) t)
                   ((null (cdr forms)) (car forms))
                   (t `(if ,(car forms) ,(walk (cdr forms)) ,+false+)))))
    (walk (cdr form))))

(define-dylan-macro or (form)
  (require-list form 1 "or")
  (labels ((walk (forms)
             (cond ((null forms) +false+)
                   ((null (cdr forms)) (car forms))
                   (t (let ((v (temp "or")))
                        `(dylan::bind ((,v ,(car forms)))
                           (if ,v ,v ,(walk (cdr forms)))))))))
    (walk (cdr form))))

(define-dylan-macro when (form)
  (require-list form 2 "when")
  `(if ,(second form) ,(body-form (cddr form)) ,+false+))

(define-dylan-macro unless (form)
  (require-list form 2 "unless")
  `(if ,(second form) ,+false+ ,(body-form (cddr form))))

;;; (cond (test consequent ...) ...)  A clause of a test alone answers the
;;; test's value.  else: and #t are ordinary true tests.

(define-dylan-macro cond (form)
  (require-list form 1 "cond")
  (labels ((walk (clauses)
             (if (null clauses)
                 +false+
                 (let ((clause (car clauses)))
                   (with-form (clause)
                     (require-list clause 1 "cond clause"))
                   (if (null (cdr clause))
                       (let ((v (temp "test")))
                         `(dylan::bind ((,v ,(car clause)))
                            (if ,v ,v ,(walk (cdr clauses)))))
                       `(if ,(car clause)
                            ,(body-form (cdr clause))
                            ,(walk (cdr clauses))))))))
    (walk (cdr form))))

;;; (case target (match-list consequent ...) ... [(else: consequent ...)])
;;; Matches are compared with id?; else: or #t matches when nothing else
;;; does, and must come last.  No match is an error.

(defun else-clause-p (clause)
  (or (eq (car clause) :else) (eq (car clause) t)))

(defun check-clauses (clauses what)
  "Check the clauses of a case or select; answer them and the else
clause's body (or :NONE)."
  (let ((else :none) (normal '()))
    (dolist (clause clauses)
      (with-form (clause)
        (unless (and (consp clause) (proper-list-p clause))
          (compile-error "~a: bad clause ~a" what clause))
        (cond ((not (eq else :none))
               (compile-error "~a: the else: clause must be the last" what))
              ((else-clause-p clause) (setf else (cdr clause)))
              ((proper-list-p (car clause)) (push clause normal))
              (t (compile-error "~a: bad match list ~a" what (car clause))))))
    (values (nreverse normal) else)))

(define-dylan-macro case (form)
  (require-list form 2 "case")
  (multiple-value-bind (clauses else) (check-clauses (cddr form) "case")
    (let ((seen '()))
      (dolist (clause clauses)
        (with-form (clause)
          (dolist (match (car clause))
            (when (member match seen)
              (compile-error "case: ~a appears in more than one match list" match))
            (push match seen)))))
    (let ((key (temp "key")))
      `(dylan::bind ((,key ,(second form)))
         ,(reduce (lambda (clause rest)
                    `(if (or ,@(mapcar (lambda (match)
                                         `(,(g "id?") ,key (quote ,match)))
                                       (car clause)))
                         ,(body-form (cdr clause))
                         ,rest))
                  clauses
                  :from-end t
                  :initial-value
                  (if (eq else :none)
                      `(,(g "error") "case: no clause matches ~s" ,key)
                      (body-form else)))))))

;;; (select target test (match-list consequent ...) ... [(else: ...)])
;;; Like case, but the matches are evaluated, and compared by calling test
;;; on the target and each in turn.

(define-dylan-macro dylan::select (form)
  (require-list form 3 "select")
  (multiple-value-bind (clauses else) (check-clauses (cdddr form) "select")
    (let ((target (temp "target"))
          (test (temp "test")))
      `(dylan::bind ((,target ,(second form))
                     (,test ,(third form)))
         ,(reduce (lambda (clause rest)
                    `(if (or ,@(mapcar (lambda (match) `(,test ,target ,match))
                                       (car clause)))
                         ,(body-form (cdr clause))
                         ,rest))
                  clauses
                  :from-end t
                  :initial-value
                  (if (eq else :none)
                      `(,(g "error") "select: no clause matches ~s" ,target)
                      (body-form else)))))))

;;; Iteration (chapter 8)

(define-dylan-macro dylan::while (form)
  (require-list form 2 "while")
  (let ((loop (temp "while")))
    `(dylan::bind-methods ((,loop ()
                             (if ,(second form)
                                 (dylan::begin ,@(cddr form) (,loop))
                                 ,+false+)))
       (,loop))))

(define-dylan-macro dylan::until (form)
  (require-list form 2 "until")
  (let ((loop (temp "until")))
    `(dylan::bind-methods ((,loop ()
                             (if ,(second form)
                                 ,+false+
                                 (dylan::begin ,@(cddr form) (,loop)))))
       (,loop))))

;;; (dotimes (var count [result]) body ...)  The body is evaluated with var
;;; bound to each integer from 0 below count; then result, with var bound
;;; to count.

(define-dylan-macro dotimes (form)
  (require-list form 2 "dotimes")
  (let ((spec (second form)))
    (unless (and (proper-list-p spec) (<= 2 (length spec) 3)
                 (variable-name-p (first spec)))
      (compile-error "dotimes: bad (var count [result]) ~a" spec))
    (destructuring-bind (var count &optional (result +false+)) spec
      (let ((n (temp "count"))
            (i (temp "i"))
            (loop (temp "dotimes")))
        `(dylan::bind ((,n ,count))
           (dylan::bind-methods
               ((,loop (,i)
                  (if (,(g "<") ,i ,n)
                      (dylan::begin
                        (dylan::bind ((,var ,i)) ,@(cddr form))
                        (,loop (,(g "+") ,i 1)))
                      (dylan::bind ((,var ,i)) ,result))))
             (,loop 0)))))))

;;; (for ((var init step) ...) (end-test result ...) body ...)  Each pass
;;; binds the variables afresh: the inits are evaluated outside their
;;; scope, the steps inside it.

(define-dylan-macro dylan::for (form)
  (require-list form 3 "for")
  (let ((clauses (second form))
        (end (third form)))
    (unless (and (proper-list-p clauses)
                 (every (lambda (c) (and (list-of-length-p c 3)
                                         (variable-name-p (car c))))
                        clauses))
      (compile-error "for: bad variable clauses ~a" clauses))
    (unless (proper-list-p end)
      (compile-error "for: bad end test ~a" end))
    (let* ((loop (temp "for"))
           (again `(,loop ,@(mapcar #'third clauses))))
      `(dylan::bind-methods
           ((,loop ,(mapcar #'first clauses)
              ,(if (null end)
                   `(dylan::begin ,@(cdddr form) ,again)
                   `(if ,(car end)
                        ,(body-form (cdr end))
                        (dylan::begin ,@(cdddr form) ,again)))))
         (,loop ,@(mapcar #'second clauses))))))

;;; (for-each ((var collection) ...) (end-test result ...) body ...)
;;; The collections are walked together with the iteration protocol
;;; (chapter 12); the loop ends, answering #f, when any is exhausted.

(define-dylan-macro dylan::for-each (form)
  (require-list form 3 "for-each")
  (let ((clauses (second form))
        (end (third form)))
    (unless (and (proper-list-p clauses) clauses
                 (every (lambda (c) (and (list-of-length-p c 2)
                                         (variable-name-p (car c))))
                        clauses))
      (compile-error "for-each: bad bindings ~a" clauses))
    (unless (proper-list-p end)
      (compile-error "for-each: bad end test ~a" end))
    (let* ((collections (mapcar (lambda (c) (declare (ignore c)) (temp "collection"))
                                clauses))
           (states (mapcar (lambda (c) (declare (ignore c)) (temp "state"))
                           clauses))
           (loop (temp "for-each"))
           (again `(,loop ,@(mapcar (lambda (c s) `(,(g "next-state") ,c ,s))
                                    collections states)))
           (pass `(dylan::bind
                      ,(mapcar (lambda (clause c s)
                                 `(,(first clause) (,(g "current-element") ,c ,s)))
                               clauses collections states)
                    ,(if (null end)
                         `(dylan::begin ,@(cdddr form) ,again)
                         `(if ,(car end)
                              ,(body-form (cdr end))
                              (dylan::begin ,@(cdddr form) ,again))))))
      `(dylan::bind ,(mapcar (lambda (c clause) `(,c ,(second clause)))
                             collections clauses)
         (dylan::bind-methods
             ((,loop ,states
                ,(reduce (lambda (s rest) `(if ,s ,rest ,+false+))
                         states :from-end t :initial-value pass)))
           (,loop ,@(mapcar (lambda (c) `(,(g "initial-state") ,c))
                            collections)))))))

;;; Conditions (chapter 13)
;;;
;;; (handler-bind (type function [test: test] [description: string]) body ...)
;;; runs the body with a handler established; the runtime primitive does
;;; the work.  No test: means every condition of the type is handled.

(defun handler-options (options what)
  (let ((alist (keyword-options options '(:test :description) what)))
    (values (if (assoc :test alist) (cdr (assoc :test alist)) +false+)
            (if (assoc :description alist) (cdr (assoc :description alist)) +false+))))

(define-dylan-macro handler-bind (form)
  (require-list form 2 "handler-bind")
  (let ((spec (second form)))
    (with-form (spec)
      (require-list spec 2 "handler-bind handler"))
    (multiple-value-bind (test description) (handler-options (cddr spec) "handler-bind")
      `(%primitive :handler-bind ,(first spec) ,(second spec) ,test ,description
                   (method () ,@(cddr form))))))

;;; (handler-case protected-form
;;;   ((type [condition: var] [test: test] [description: string]) body ...)
;;;   ...)
;;;
;;; A handler that applies exits from the handler-case and then evaluates
;;; its body, with var bound to the condition.  The first clause is tried
;;; first: its handler is established innermost.  The handlers pass the
;;; body back out as a method, which is called once the exit is complete.

(define-dylan-macro handler-case (form)
  (require-list form 2 "handler-case")
  (let ((protected (second form))
        (clauses (cddr form)))
    (if (null clauses)
        protected
        (let ((done (temp "done"))
              (handle (temp "handle"))
              (thunk (temp "thunk"))
              (values-var (temp "values")))
          (let ((inner `(dylan::bind ((dylan::!rest ,values-var ,protected))
                          (,(g "apply") ,done ,values-var))))
            (dolist (clause clauses)
              (with-form (clause)
                (unless (and (list-of-at-least-p clause 1)
                             (list-of-at-least-p (car clause) 1))
                  (compile-error "handler-case: bad clause ~a" clause))
                (let* ((spec (car clause))
                       (alist (keyword-options (cdr spec)
                                               '(:condition :test :description)
                                               "handler-case"))
                       (var (cdr (assoc :condition alist)))
                       (condition (temp "condition"))
                       (next (temp "next-handler")))
                  (when var (require-variable-name var "handler-case condition:"))
                  (setf inner
                        `(handler-bind (,(car spec)
                                        (method (,condition ,next)
                                          (,handle
                                           (method ()
                                             ,(if var
                                                  `(dylan::bind ((,var ,condition))
                                                     ,@(cdr clause))
                                                  (body-form (cdr clause))))))
                                        ,@(and (assoc :test alist)
                                               (list :test (cdr (assoc :test alist))))
                                        ,@(and (assoc :description alist)
                                               (list :description
                                                     (cdr (assoc :description alist)))))
                           ,inner)))))
            `(dylan::bind-exit (,done)
               (dylan::bind ((,thunk (dylan::bind-exit (,handle) ,inner)))
                 (,thunk))))))))

;;; Backquote.  `x, ,x and ,@x are read as (quasiquote x), (unquote x) and
;;; (unquote-splicing x).  The expansion builds the new structure with
;;; runtime primitives, copying only what contains unquotes; nested
;;; backquotes are kept, their unquotes evaluated only at the matching
;;; depth.

(defun unquote-form-p (x name)
  (and (consp x) (eq (car x) name) (consp (cdr x)) (null (cddr x))))

(defun quasi (x depth)
  "The expansion of the template X at nesting DEPTH, and whether it holds
anything to evaluate (if not, the expansion is X quoted)."
  (cond ((unquote-form-p x 'dylan::unquote)
         (if (= depth 1)
             (values (cadr x) t)
             (quasi-list-of 'dylan::unquote (cadr x) (1- depth))))
        ((unquote-form-p x 'dylan::unquote-splicing)
         (if (= depth 1)
             (compile-error "unquote-splicing (,@) not in a list")
             (quasi-list-of 'dylan::unquote-splicing (cadr x) (1- depth))))
        ((unquote-form-p x 'dylan::quasiquote)
         (quasi-list-of 'dylan::quasiquote (cadr x) (1+ depth)))
        ((consp x)
         (if (and (= depth 1) (unquote-form-p (car x) 'dylan::unquote-splicing))
             (multiple-value-bind (rest live) (quasi (cdr x) depth)
               (declare (ignore live))
               (values `(%primitive :append2 ,(cadr (car x)) ,rest) t))
             (multiple-value-bind (head head-live) (quasi (car x) depth)
               (multiple-value-bind (rest rest-live) (quasi (cdr x) depth)
                 (if (or head-live rest-live)
                     (values `(%primitive :pair ,head ,rest) t)
                     (values `(quote ,x) nil))))))
        ((and (vectorp x) (not (stringp x)))
         (multiple-value-bind (list live) (quasi (coerce x 'list) depth)
           (if live
               (values `(%primitive :list->vector ,list) t)
               (values `(quote ,x) nil))))
        (t (values `(quote ,x) nil))))

(defun quasi-list-of (name x depth)
  "The expansion of (NAME X), where X is at DEPTH."
  (multiple-value-bind (inner live) (quasi x depth)
    (if live
        (values `(%primitive :pair (quote ,name)
                             (%primitive :pair ,inner (quote ())))
                t)
        (values `(quote (,name ,x)) nil))))

(define-dylan-macro dylan::quasiquote (form)
  (require-list form 2 "quasiquote" nil)
  (values (quasi (second form) 1)))
