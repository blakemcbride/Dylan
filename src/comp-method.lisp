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
;;;; More of the compiler: METHOD and DEFINE-METHOD
;;;;                                        (was Thomas's src/comp-method.scm)

(in-package #:dylan)

;;; Parsing and compiling of the METHOD form

(defun validate-names (names)
  (dolist (name names)
    (unless (variable-name? name)
      (dylan%error "invalid parameter name" name names)))
  (must-be-unique names #'member "parameter names not unique"))

(defun parse-requireds (orig-l continuation)
  (flet ((oops (l) (cant-parse "requireds" orig-l l)))
    (loop with l = orig-l
          with requireds = '()
          do (cond
               ((null l) (return (funcall continuation (reverse requireds) l)))
               ((consp l)
                (let ((this-param (car l)))
                  (cond ((or (eq this-param '!next)
                             (eq this-param '!rest)
                             (eq this-param '!key))
                         (return (funcall continuation (reverse requireds) l)))
                        ((variable-name? this-param)
                         (push (list this-param '<object>) requireds)
                         (pop l))
                        ((and (consp this-param)
                              (variable-name? (car this-param))
                              (consp (cdr this-param))
                              (null (cdr (cdr this-param))))
                         (push (list (car this-param) (cadr this-param))
                               requireds)
                         (pop l))
                        (t (return (oops l))))))
               (t (return (oops l)))))))

(defun make-parse-next/rest (flag name)
  (lambda (orig-l continuation)
    (cond ((null orig-l) (funcall continuation nil orig-l))
          ((consp orig-l)
           (let ((this-param (car orig-l))
                 (next-params (cdr orig-l)))
             (if (and (eq this-param flag)
                      (consp next-params)
                      (variable-name? (car next-params)))
                 (funcall continuation (car next-params) (cdr next-params))
                 (funcall continuation nil orig-l))))
          (t (cant-parse name orig-l orig-l)))))

(defparameter parse-next (make-parse-next/rest '!next "next"))
(defparameter parse-rest (make-parse-next/rest '!rest "rest"))

(defun parse-keys (orig-l continuation)
  (labels ((oops (l) (cant-parse "keys" orig-l l))
           (walk (l keys)
             (cond
               ((null l)
                (funcall continuation (if (null keys) t (reverse keys))))
               ((consp l)
                (let ((this-param (car l)))
                  (cond
                    ((and (symbolp this-param) (not (null this-param)))
                     (walk (cdr l)
                           (cons (list (name->keyword this-param)
                                       this-param dylan%false)
                                 keys)))
                    ((consp this-param)
                     (cond
                       ((keyword? (car this-param))
                        (let ((the-key (car this-param))
                              (the-rest (cdr this-param)))
                          ;; Chapter 6: the parameter may be any variable,
                          ;; a setter variable included.
                          (if (and (consp the-rest)
                                   (variable-name? (car the-rest)))
                              (let ((variable (car the-rest))
                                    (the-rest (cdr the-rest)))
                                (cond
                                  ((null the-rest)
                                   (walk (cdr l)
                                         (cons (list the-key variable dylan%false)
                                               keys)))
                                  ((and (consp the-rest)
                                        (null (cdr the-rest)))
                                   (walk (cdr l)
                                         (cons (list the-key variable
                                                     (car the-rest))
                                               keys)))
                                  (t (oops l))))
                              (oops l))))
                       ((and (symbolp (car this-param))
                             (not (null (car this-param))))
                        (let ((variable (car this-param))
                              (the-rest (cdr this-param)))
                          (cond
                            ((null the-rest)
                             (walk (cdr l)
                                   (cons (list (name->keyword variable)
                                               variable dylan%false)
                                         keys)))
                            ((consp the-rest)
                             (let ((expr (car the-rest))
                                   (the-rest (cdr the-rest)))
                               (if (null the-rest)
                                   (walk (cdr l)
                                         (cons (list (name->keyword variable)
                                                     variable expr)
                                               keys))
                                   (oops l))))
                            (t (oops l)))))
                       (t (oops l))))
                    (t (oops l)))))
               (t (oops l)))))
    (cond ((null orig-l) (funcall continuation nil))
          ((and (consp orig-l) (eq (car orig-l) '!key))
           (walk (cdr orig-l) '()))
          (t (oops orig-l)))))

(defun parse-method-parameter-list (orig-list default-next-method continue)
  (funcall
   #'parse-requireds orig-list
   (lambda (required-params l)
     (funcall
      parse-next l
      (lambda (next l)
        (funcall
         parse-rest l
         (lambda (rest-param l)
           (parse-keys
            l
            (lambda (keys)
              (validate-names
               (append (mapcar #'car required-params)
                       (if next (list next) '())
                       (if rest-param (list rest-param) '())
                       (if (consp keys) (mapcar #'cadr keys) '())))
              (when (consp keys)
                (must-be-unique (mapcar #'car keys) #'member
                                "keywords not unique"))
              (funcall continue required-params
                       (or next default-next-method)
                       rest-param
                       keys))))))))))

(defun compile-method-form (e mod-vars bound-vars compiler
                            multiple-values? continue)
  (declare (ignore multiple-values?))   ; No reductions
  (must-be-list-of-at-least-length e 1 "METHOD -- invalid syntax")
  (let ((params (car e))
        (forms (or (cdr e) (list dylan%false))))   ; An empty body answers #f
    (parse-method-parameter-list
     params '!next-method
     (lambda (reqs next rest keys)
       (let ((next (variable->lexical next))
             (rest (and rest (variable->lexical rest)))
             (keys (if (consp keys)
                       (mapcar (lambda (key)
                                 (list* (car key)
                                        (variable->lexical (cadr key))
                                        (cddr key)))
                               keys)
                       keys)))
        (flet ((lambda-list (req-names)
                 `(!multi-values !raw-next ,@req-names
                   ,@(cond (rest (list '&rest rest))
                           (keys (list '&rest '!keys))
                           (t '())))))
          (let* ((req-names (mapcar (lambda (req) (variable->lexical (car req)))
                                    reqs))
                 (req-restrictions (mapcar #'cadr reqs))
                 (key-name (if rest rest '!keys)))
            (compile-forms
             req-restrictions mod-vars bound-vars compiler nil
             (lambda (restrictions mod-vars)
               (flet ((generate (mod-vars validation-code)
                        (funcall
                         continue
                         `(dylan%make-compiled-method
                           (dylan%make-param-list
                            (dylan%list
                             ,@(mapcar (lambda (name code)
                                         `(dylan%list ',name ,code))
                                       req-names restrictions))
                            ',next ',rest
                            ',(if (consp keys) (mapcar #'car keys) keys))
                           (lambda ,(lambda-list req-names)
                             (declare (ignorable !multi-values
                                                 ,@req-names
                                                 ,@(if rest (list rest) '())
                                                 ,@(if (and (not rest) keys)
                                                       (list '!keys)
                                                       '())))
                             ;; #next is #f for the last method of a
                             ;; dispatch, as for a direct call.
                             (let ((,next (dylan%next-method-value !raw-next)))
                               (declare (ignorable ,next))
                               ,@validation-code)))
                         mod-vars)))
                 (compile-forms
                  forms mod-vars
                  (append (list next) req-names (if rest (list rest) '())
                          (if (consp keys) (mapcar #'cadr keys) '())
                          bound-vars)
                  compiler '!multi-values
                  (lambda (body-forms mod-vars)
                    (cond
                      ((not keys) (generate mod-vars body-forms))
                      ((eq keys t)
                       (generate mod-vars
                                 `((dylan%keyword-validate !raw-next ,key-name t)
                                   ,@body-forms)))
                      (t
                       (compile-let*-forms
                        (mapcar #'cadr keys) (mapcar #'caddr keys) mod-vars
                        (append (list next) req-names
                                (if rest (list rest) '()) bound-vars)
                        compiler nil
                        (lambda (defaults mod-vars)
                          (generate
                           mod-vars
                           `((dylan%keyword-validate
                              !raw-next ,key-name
                              ,(if rest t `',(mapcar #'car keys)))
                             (let* (,@(mapcar
                                       (lambda (var key default)
                                         `(,var
                                           (dylan%find-keyword
                                            ,key-name ',key
                                            (lambda () ,default))))
                                       (mapcar #'cadr keys)
                                       (mapcar #'car keys)
                                       defaults))
                               ,@body-forms)))))))))))))))))))

(defun compile-let*-forms (names forms module-vars bound-vars
                           compiler multiple-values? continue)
  (labels ((walk (result forms names bound-vars mod-vars)
             (if (null forms)
                 (funcall continue (reverse result) mod-vars)
                 (funcall compiler (car forms) mod-vars bound-vars
                          multiple-values?
                          (lambda (compiled mod-vars)
                            (walk (cons compiled result) (cdr forms)
                                  (cdr names)
                                  (cons (car names) bound-vars)
                                  mod-vars))))))
    (walk '() forms names bound-vars module-vars)))

(defun compile-define-method-form (e mod-vars bound-vars compiler
                                   multiple-values? continue)
  (declare (ignore multiple-values?))   ; Doesn't reduce
  (labels ((rebuild-param-list (params)
             (loop with result = '()
                   with so-far = params
                   do (if (null so-far)
                          (return (reverse `(next-method !next ,@result)))
                          (case (car so-far)
                            ((!next) (return params))
                            ((!rest !key)
                             (return `(,@(reverse result)
                                       !next next-method ,@so-far)))
                            (t (push (car so-far) result)
                               (pop so-far)))))))
    (must-be-list-of-at-least-length e 2 "DEFINE-METHOD -- invalid syntax")
    (let ((name (car e))
          (params (cadr e))
          (forms (cddr e)))
      (unless (variable-name? name)
        (dylan%error "DEFINE-METHOD -- illegal name" name params forms))
      (check-not-macro name "DEFINE-METHOD")
      (funcall
       compiler `(method ,(rebuild-param-list params) ,@forms)
       mod-vars bound-vars nil
       (lambda (method-code mod-vars)
         (module-refs
          name bound-vars mod-vars
          continue
          (lambda (ref set)
            (parse-method-parameter-list
             params nil
             (lambda (reqs next rest keys)
               `(progn
                  (cond ((dylan%eq? ,ref ',the-unassigned-value)
                         ,(funcall set
                                   `(dylan%create-generic-function
                                     ',(variable->name name)
                                     ,(length reqs)
                                     nil ; No required keywords
                                     ,(if (or keys rest) t nil))))
                        ((dylan%not (dylan%generic-function? ,ref))
                         (dylan-call dylan/error
                                     "DEFINE-METHOD -- already has a value"
                                     ',name ,ref ',reqs ',next ',rest ',keys)))
                  (dylan%add-method ,ref ,method-code)
                  ',name))))))))))

(defun compile-bind-methods-form (e mod-vars bound-vars compiler
                                  multiple-values? continue)
  (must-be-list-of-at-least-length e 2 "BIND-METHODS -- bad syntax")
  (let ((bindings (car e))
        (forms (cdr e)))
    (dolist (binding bindings)
      (must-be-list-of-at-least-length binding 3
                                       "BIND-METHODS -- illegal binding syntax"))
    (let ((names (mapcar (lambda (binding) (variable->lexical (car binding)))
                         bindings))
          (methods (mapcar (lambda (binding)
                             `(method ,(cadr binding) ,@(cddr binding)))
                           bindings)))
      (let ((new-bvs (append names bound-vars)))
        (compile-forms
         methods mod-vars new-bvs compiler nil
         (lambda (method-bodies mod-vars)
           (compile-forms
            forms mod-vars new-bvs compiler multiple-values?
            (lambda (body-codes mod-vars)
              ;; Scheme's LETREC; in Common Lisp these are variables, not
              ;; functions, so the bindings are made first and filled in
              ;; afterwards.
              (funcall continue
                       `(let ,(mapcar (lambda (name) (list name nil)) names)
                          ,@(mapcar (lambda (name body) `(setq ,name ,body))
                                    names method-bodies)
                          ,@body-codes)
                       mod-vars)))))))))
