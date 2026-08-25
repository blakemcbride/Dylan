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
;;;; More of the compiler: simple special forms
;;;;
;;;; SET!, COND, CASE, SELECT, FOR       (was Thomas's src/comp-sf.scm)

(in-package #:dylan)

;;; SET!

(defun compile-set!-form (e mod-vars bound-vars compiler
                          multiple-values? continue)
  (must-be-list-of-length e 2 "SET! -- invalid syntax")
  (let ((place (car e))
        (value (cadr e)))
    (funcall
     compiler value mod-vars bound-vars nil
     (lambda (value-code module-vars)
       (cond
         ((variable-name? place)
          (let ((name (variable->name place)))
            (funcall
             continue
             `(let ((!saved-value ,value-code))
                (setq ,(if (member name bound-vars)
                           name
                           (name->module-variable name))
                      !saved-value)
                !saved-value)
             (add-variable name bound-vars module-vars))))
         ((and (listp place) (null (cdr (cl:last place))))
          (compile-forms
           `((setter ,(car place)) ,@(cdr place))
           module-vars bound-vars compiler nil
           (lambda (subform-codes mod-vars)
             (funcall continue
                      `(let ((!saved-value ,value-code))
                         (dylan%apply
                          ,multiple-values?
                          ,@(mapcar (lambda (x) `(lambda () ,x))
                                    subform-codes)
                          (lambda () !saved-value))
                         !saved-value)
                      mod-vars))))
         (t (dylan%error "SET! -- bad place specification")))))))

;;; COND

(defun compile-cond-form (clauses module-vars bound-vars compiler
                          multiple-values? continue)
  (labels
      ((compile-clause (clause mod-vars continue)
         (must-be-list-of-at-least-length clause 1 "COND -- bad clause syntax")
         (let ((predicate (car clause))
               (consequents (cdr clause)))
           (funcall
            compiler predicate mod-vars bound-vars nil
            (lambda (pred-code mod-vars)
              (if (null consequents)
                  (funcall continue (list pred-code) mod-vars)
                  (compile-forms consequents mod-vars bound-vars
                                 compiler multiple-values?
                                 (lambda (consequent-code mod-vars)
                                   (funcall continue
                                            `(,pred-code ,@consequent-code)
                                            mod-vars))))))))
       (compile-remaining-clauses (clauses mod-vars continue)
         (if (null clauses)
             (funcall continue `((t ,compiled-sharp-f)) mod-vars)
             (compile-clause
              (car clauses) mod-vars
              (lambda (this-clause-code mod-vars)
                (compile-remaining-clauses
                 (cdr clauses) mod-vars
                 (lambda (rest-clauses-code mod-vars)
                   (funcall continue
                            (cons this-clause-code rest-clauses-code)
                            mod-vars))))))))
    (unless (and (listp clauses) (null (cdr (cl:last clauses))))
      (dylan%error "COND -- bad syntax" clauses))
    (compile-remaining-clauses clauses module-vars
                               (lambda (clause-code mod-vars)
                                 (funcall continue `(cond ,@clause-code)
                                          mod-vars)))))

;;; CASE
;;;
;;; Scheme's CASE compares with EQV? and spells its default clause ELSE.
;;; Common Lisp's CASE would take a clause whose key list is (t) or
;;; (otherwise) to be the default, so the expansion here is a COND over
;;; MEMBER instead, which compares with EQL -- Common Lisp's EQV?.

(defun compile-case-form (e module-vars bound-vars compiler
                          multiple-values? continue)
  (must-be-list-of-at-least-length e 1 "CASE -- bad syntax")
  (let ((have-default? nil))
    (labels
        ((compile-clause (clause mod-vars continue)
           (unless (consp clause)
             (dylan%error "CASE -- bad clause syntax" clause))
           (let ((match-list (car clause))
                 (consequents (cdr clause)))
             (let ((new-match
                     (cond ((or (eq match-list :else) (eq match-list t))
                            (setf have-default? t)
                            t)
                           ((and (listp match-list)
                                 (null (cdr (cl:last match-list))))
                            `(member !case-key ',match-list))
                           (t (dylan%error "CASE -- bad match list" clause)))))
               (if (null consequents)
                   (funcall continue `(,new-match ,compiled-sharp-f) mod-vars)
                   (compile-forms consequents mod-vars bound-vars compiler
                                  multiple-values?
                                  (lambda (consequent-code mod-vars)
                                    (funcall continue
                                             `(,new-match ,@consequent-code)
                                             mod-vars))))))))
      (let ((expr (car e)))
        (funcall
         compiler expr module-vars bound-vars nil
         (lambda (expr-code mod-vars)
           (labels
               ((walk (clauses compiled mod-vars)
                  (if (null clauses)
                      (funcall
                       continue
                       `(let ((!case-key ,expr-code))
                          (cond ,@(reverse compiled)
                                ,@(if have-default?
                                      '()
                                      `((t (dylan-call dylan/error
                                                       "CASE -- no matches"
                                                       ',e))))))
                       mod-vars)
                      (if have-default?
                          (dylan%error "CASE -- else isn't last clause" e)
                          (compile-clause (car clauses) mod-vars
                                          (lambda (code mod-vars)
                                            (walk (cdr clauses)
                                                  (cons code compiled)
                                                  mod-vars)))))))
             (walk (cdr e) '() mod-vars))))))))

;;; SELECT

(defun compile-select-form (e module-vars bound-vars compiler
                            multiple-values? continue)
  (must-be-list-of-at-least-length e 2 "SELECT -- bad syntax")
  (let ((have-default? nil))
    (labels
        ((compile-clause (clause mod-vars continue)
           (unless (consp clause)
             (dylan%error "SELECT -- bad clause syntax" clause))
           (let ((match-list (car clause))
                 (consequents (cdr clause)))
             (let ((new-match
                     (cond ((or (eq match-list :else) (eq match-list t))
                            (setf have-default? t)
                            t)
                           ((and (listp match-list)
                                 (null (cdr (cl:last match-list))))
                            (compile-forms
                             match-list mod-vars bound-vars compiler nil
                             (lambda (matches new-mod-vars)
                               (setf mod-vars new-mod-vars)
                               `(or ,@(mapcar
                                       (lambda (code)
                                         `(dylan%apply
                                           nil
                                           (lambda () !select-test)
                                           (lambda () !select-target)
                                           (lambda () ,code)))
                                       matches)))))
                           (t (dylan%error "SELECT -- bad match list"
                                           clause)))))
               (if (null consequents)
                   (funcall continue `(,new-match ,compiled-sharp-f) mod-vars)
                   (compile-forms consequents mod-vars bound-vars compiler
                                  multiple-values?
                                  (lambda (consequent-code mod-vars)
                                    (funcall continue
                                             `(,new-match ,@consequent-code)
                                             mod-vars))))))))
      (let ((target (car e))
            (test (cadr e)))
        (compile-forms
         (list target test) module-vars bound-vars compiler nil
         (lambda (expr-codes mod-vars)
           (let ((target (car expr-codes))
                 (test (cadr expr-codes)))
             (labels
                 ((walk (clauses compiled mod-vars)
                    (if (null clauses)
                        (funcall
                         continue
                         `(let ((!select-target ,target)
                                (!select-test ,test))
                            (cond
                              ,@(reverse compiled)
                              ,@(if have-default?
                                    '()
                                    `((t (dylan-call dylan/error
                                                     "SELECT -- no match"
                                                     ',e))))))
                         mod-vars)
                        (if have-default?
                            (dylan%error "SELECT -- else isn't last clause" e)
                            (compile-clause (car clauses) mod-vars
                                            (lambda (code mod-vars)
                                              (walk (cdr clauses)
                                                    (cons code compiled)
                                                    mod-vars)))))))
               (walk (cddr e) '() mod-vars)))))))))

;;; FOR

(defun compile-for-form (e module-vars bound-vars compiler
                         multiple-values? continue)
  (must-be-list-of-at-least-length e 2 "FOR -- bad syntax")
  (let ((bindings (car e))
        (end-test (cadr e))
        (body (cddr e)))
    (when (or (not (and (listp bindings) (null (cdr (cl:last bindings)))))
              (not (all? (lambda (x) (list-of-length? x 3)) bindings)))
      (dylan%error "FOR -- illegal binding list" e))
    (let ((vars (mapcar (lambda (x)
                          (unless (variable-name? (car x))
                            (dylan%error "FOR -- illegal variable name" x e))
                          (variable->name (car x)))
                        bindings))
          (inits (mapcar #'cadr bindings))
          (steps (mapcar #'caddr bindings)))
      (let ((inner-bound (append vars bound-vars)))
        (compile-forms
         body module-vars inner-bound compiler nil
         (lambda (body-code module-vars)
           (compile-forms
            inits module-vars bound-vars compiler nil
            (lambda (init-code module-vars)
              (compile-forms
               steps module-vars inner-bound compiler nil
               (lambda (step-code module-vars)
                 (compile-forms
                  end-test module-vars inner-bound compiler multiple-values?
                  (lambda (end-code module-vars)
                    (funcall continue
                             `(do ,(mapcar #'list vars init-code step-code)
                                  ,end-code
                                ,@body-code)
                             module-vars)))))))))))))
