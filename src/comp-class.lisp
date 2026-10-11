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
;;;; More of the compiler: class and binding related operations
;;;;
;;;; BIND, DEFINE-GENERIC-FUNCTION, DEFINE-CLASS, DEFINE-SLOT
;;;;                                        (was Thomas's src/comp-class.scm)

(in-package #:dylan)

;;; Parsing and compiling BIND special forms

;;; Access functions for BINDings

(defun binding->names (binding)
  (loop with names = '()
        with rest = binding
        do (progn
             (unless (and (listp binding) (null (cdr (cl:last binding))))
               (dylan%error "bind -- bad binding" binding))
             (if (or (null rest) (null (cdr rest)) (eq '!rest (car rest)))
                 (return (reverse names))
                 (let ((this-binding (car rest)))
                   (cond ((variable-name? this-binding)
                          (push this-binding names)
                          (pop rest))
                         ((and (list-of-length? this-binding 2)
                               (variable-name? (car this-binding)))
                          (push (car this-binding) names)
                          (pop rest))
                         (t (return (dylan%error "bind -- bad binding list"
                                                 binding)))))))))

(defun binding->restrictions (binding)
  (loop with restrictions = '()
        with rest = binding
        do (if (or (null rest) (null (cdr rest)) (eq '!rest (car rest)))
               (return (reverse restrictions))
               (let ((this-binding (car rest)))
                 (push (if (variable-name? this-binding)
                           nil
                           (cadr this-binding))
                       restrictions)
                 (pop rest)))))

(defun binding->rest (binding)
  (let ((found (member '!rest binding)))
    (if found
        (progn
          (must-be-list-of-length found 3 "bind -- error in bindings")
          (cadr found))
        nil)))

(defun binding->init (binding)
  (last binding))

(defun build-bind-form (bound-names rest-name compiled-restrictions
                        compiled-init compiled-body)
  (let* ((all
           ;; Build a list with entries (offset name restriction) where
           ;; `offset' is the offset in the values vector.
           (loop for offset from 0
                 for name in bound-names
                 for restriction in compiled-restrictions
                 collect (list offset name restriction)))
         (->offset #'car)
         (->name #'cadr)
         (->restriction #'caddr)
         (restricted (remove-if-not ->restriction all)))
    `(let ((!bind-body
             (lambda (,@bound-names ,@(if rest-name (list rest-name) '()))
               ,compiled-body))
           (!bind-init-form (lambda (!multi-value)
                              (declare (ignorable !multi-value))
                              ,compiled-init))
           ,@(mapcar (lambda (restriction)
                       `(,(funcall ->name restriction)
                         ,(funcall ->restriction restriction)))
                     restricted))
       (let* ((!multi-value
                (dylan%vector ,@(mapcar (lambda (name)
                                          (declare (ignore name))
                                          nil)
                                        bound-names)
                              nil))
              (!first-value (funcall !bind-init-form !multi-value)))
         (if (dylan%eq? !first-value !multi-value)
             ;; We went through values ...
             ,(let ((call
                      `(funcall !bind-body
                                ,@(mapcar (lambda (offset)
                                            `(dylan%vector-ref !multi-value
                                                               ,offset))
                                          (mapcar ->offset all))
                                ,@(if rest-name
                                      `((dylan%vector-ref
                                         !multi-value ,(length bound-names)))
                                      '()))))
                (if (null restricted)
                    call
                    `(progn
                       ,@(mapcar (lambda (offset name)
                                   `(dylan%type-check
                                     (dylan%vector-ref !multi-value ,offset)
                                     ,name))
                                 (mapcar ->offset restricted)
                                 (mapcar ->name restricted))
                       ,call)))
             ;; Didn't go through values ...
             ,(if (null bound-names)
                  `(funcall !bind-body (dylan%list !first-value))
                  ;; Only one value: the other variables are bound to #f,
                  ;; and are type-checked like the first.
                  (let ((call `(funcall !bind-body !first-value
                                        ,@(mapcar (lambda (name)
                                                    (declare (ignore name))
                                                    nil)
                                                  (cdr bound-names))
                                        ,@(if rest-name '('()) '()))))
                    (if (null restricted)
                        call
                        `(progn
                           ,@(mapcar (lambda (entry)
                                       `(dylan%type-check
                                         ,(if (= (funcall ->offset entry) 0)
                                              '!first-value
                                              nil)
                                         ,(funcall ->name entry)))
                                     restricted)
                           ,call)))))))))

;;; Compiling the BIND form

(defun compile-bind-form (e module-vars bound-vars really-compile
                          multiple-values? continue)
  (must-be-list-of-at-least-length e 1 "BIND form invalid")
  (let ((bindings (car e))
        (forms (cdr e)))
    (if (null bindings)
        (if (null forms)
            (funcall continue compiled-sharp-f module-vars)
            (funcall really-compile `(begin ,@forms)
                     module-vars bound-vars multiple-values? continue))
        (progn
          (unless (and (listp bindings) (null (cdr (cl:last bindings))))
            (dylan%error "bind -- bad bindings" bindings))
          (let* ((binding (car bindings))
                 (bound-names (binding->names binding))
                 (rest-name (binding->rest binding))
                 (init-form (binding->init binding)))
            (when (and (null bound-names) (not rest-name))
              (dylan%error "bind -- no variables bound" e))
            (validate-names (if rest-name
                                (cons rest-name bound-names)
                                bound-names))
            (compile-forms
             (binding->restrictions binding)
             module-vars bound-vars really-compile nil
             (lambda (compiled-restrictions mod-vars)
               (funcall
                really-compile
                init-form mod-vars bound-vars
                (if (or rest-name
                        (and (not (null bound-names))
                             (not (null (cdr bound-names)))))
                    '!multi-value
                    nil)
                (lambda (compiled-init new-mods)
                  (let ((bound-names (mapcar #'variable->lexical bound-names))
                        (rest-name (and rest-name
                                        (variable->lexical rest-name))))
                    (funcall
                     really-compile
                     `(bind ,(cdr bindings) ,@forms)
                     new-mods (append (if rest-name (list rest-name) '())
                                      bound-names bound-vars)
                     multiple-values?
                     (lambda (compiled-body new-mods)
                       (funcall continue
                                (build-bind-form bound-names rest-name
                                                 compiled-restrictions
                                                 compiled-init compiled-body)
                                new-mods)))))))))))))

;;; Parsing and compiling the DEFINE-GENERIC-FUNCTION special form

(defun gen-fn-param-error (params where)
  (dylan%error "define-generic-function -- parameter syntax error"
               params where))

(defun parse-gen-fn-params (orig-params allowed-fn continue)
  (loop with names = '()
        with params = orig-params
        do (cond ((null params)
                  (return (funcall continue (reverse names) params)))
                 ((not (consp params))
                  (return (gen-fn-param-error orig-params
                                              (list 'parsing params))))
                 (t (let ((value (funcall allowed-fn (car params))))
                      (if value
                          (progn (push value names) (pop params))
                          (return (funcall continue (reverse names)
                                           params))))))))

(defun parse-define-generic-function (name params keywords compiler)
  (declare (ignore keywords))           ; Not used
  (unless (variable-name? name)
    (dylan%error "define-generic-function -- illegal name" name))
  (parse-gen-fn-params
   params (lambda (x) (and (variable-name? x) x))
   (lambda (reqs rest)
     (labels ((symbol-or-keyword (x)
                (cond ((member x '(!key !rest)) x)
                      ((dylan-special-name? x)
                       (gen-fn-param-error params (list 'reserved-name rest)))
                      ((keyword? x) x)
                      ((and (symbolp x) (not (null x))) (name->keyword x))
                      (t nil)))
              (compile-keys (rest)
                (lambda (keys after-keys)
                  (unless (null after-keys)
                    (gen-fn-param-error params
                                        (list 'compile-keys after-keys)))
                  (funcall compiler name reqs rest keys))))
       (cond ((null rest) (funcall compiler name reqs nil nil))
             ((not (consp rest))
              (gen-fn-param-error params (list 'strange-rest rest)))
             (t
              (case (car rest)
                ((!rest)
                 (let ((after-rest (cdr rest)))
                   (when (or (not (consp after-rest))
                             (not (variable-name? (car after-rest))))
                     (gen-fn-param-error params
                                         (list 'post-!rest after-rest)))
                   (let ((rest (car after-rest)))
                     (cond ((null (cdr after-rest))
                            (funcall compiler name reqs rest nil))
                           ((not (consp (cdr after-rest)))
                            (gen-fn-param-error
                             params (list 'after-!rest (cdr after-rest))))
                           ((eq '!key (cadr after-rest))
                            (parse-gen-fn-params
                             (cddr after-rest) #'symbol-or-keyword
                             (compile-keys rest)))
                           (t (gen-fn-param-error
                               params
                               (list 'before-!key (cddr after-rest))))))))
                ((!key)
                 (if (null (cdr rest))
                     (funcall compiler name reqs nil t)
                     (parse-gen-fn-params (cdr rest) #'symbol-or-keyword
                                          (compile-keys nil))))
                (t (gen-fn-param-error params
                                       (list 'unknown-stopper rest))))))))))

(defun compile-define-generic-function-form (e mod-vars bound-vars compiler
                                             multiple-values? continue)
  (declare (ignore compiler multiple-values?)) ; No sub-compilations or
                                        ; reductions
  (must-be-list-of-at-least-length e 2
                                   "DEFINE-GENERIC-FUNCTION: invalid syntax")
  ;; Keyword options after the parameter list are accepted and ignored.
  (validate-keywords (cddr e) t #'dylan%error)
  (parse-define-generic-function
   (car e) (cadr e) (cddr e)
   (lambda (name reqs rest keys)
     (module-refs
      name bound-vars mod-vars
      continue
      (lambda (ref set)
        `(if (dylan%not (or (dylan%eq? ,ref ',the-unassigned-value)
                            (dylan%generic-function? ,ref)))
             (dylan-call dylan/error
                         "define-generic-function -- already has a value"
                         ',name ,ref ',reqs ',rest ',keys)
             (progn
               ,(funcall set `(dylan%create-generic-function
                               ',name ,(length reqs) ',keys ,(if rest t nil)))
               ',name)))))))

;;; Parsing and compiling the DEFINE-CLASS form

(defun expand-slot (slot)
  (labels
      ((build-slot (pairs default-getter)
         (validate-keywords
          pairs
          '(:getter :setter :type :init-value :init-function :init-keyword
            :required-init-keyword :allocation)
          #'dylan%error)
         (let* ((getter
                 (dylan%find-keyword
                  pairs :getter
                  (lambda ()
                    (or default-getter
                        (dylan%error "slot expander -- no getter name"
                                     pairs)))))
               (setter
                 (dylan%find-keyword
                  pairs :setter (lambda ()
                                  (if (variable-name? getter)
                                      `(setter ,getter)
                                      nil))))
               (type (dylan%find-keyword pairs :type (lambda () '<object>)))
               (has-initial-value? t))
           (let ((init-value
                   (dylan%find-keyword pairs :init-value
                                       (lambda ()
                                         (setf has-initial-value? nil)
                                         nil)))
                 (init-function
                   (dylan%find-keyword pairs :init-function (lambda () nil)))
                 (init-keyword
                   (dylan%find-keyword pairs :init-keyword (lambda () nil)))
                 (required-init-keyword
                   (dylan%find-keyword pairs :required-init-keyword
                                       (lambda () nil)))
                 (allocation
                   (dylan%find-keyword pairs :allocation
                                       (lambda () 'instance))))
             (if (and (variable-name? getter)
                      (or (not setter) (variable-name? setter))
                      (or (not has-initial-value?) (not init-function))
                      (or (not required-init-keyword)
                          (and (not init-keyword) (not has-initial-value?)
                               (not init-function)))
                      (member allocation '(instance class each-subclass
                                           constant virtual)))
                 (make-slot getter      ; debug-name
                            getter setter type init-value has-initial-value?
                            init-function init-keyword required-init-keyword
                            allocation nil nil)
                 (dylan%error "slot expander -- bad slot"
                              getter setter has-initial-value? init-value
                              init-function init-keyword
                              required-init-keyword allocation))))))
    (cond ((and (symbolp slot) (not (null slot)))
           (make-slot slot slot `(setter ,slot) '<object> nil nil nil nil nil
                      'instance nil nil))
          ((and (consp slot) (keyword? (car slot)))
           (build-slot slot nil))
          ((and (consp slot) (symbolp (car slot)) (not (null (car slot)))
                (variable-name? (car slot)))
           (build-slot (cdr slot) (car slot)))
          (t (dylan%error "slot expander -- bad slot specification" slot)))))

(defun expand-slots (slots)
  (mapcar #'expand-slot slots))

(defun make-add-slot-call (slot owner bound-vars mod-vars compiler continue)
  (labels ((get-mod-var (name mod-vars setter? continue)
             (if name
                 (module-refs
                  name bound-vars mod-vars continue
                  (lambda (ref set)
                    `(progn
                       (cond ((dylan%eq? ,ref ',the-unassigned-value)
                              ,(funcall set
                                        `(dylan%create-generic-function
                                          ',(variable->name name)
                                          ,(if setter? 2 1) ; # of arguments
                                          nil ; No required keywords
                                          nil))) ; No rest argument
                             ((dylan%not (dylan%generic-function? ,ref))
                              (dylan-call dylan/error
                                          "Slot function -- already has a value"
                                          ',(slot.debug-name slot) ,owner)))
                       ,ref)))
                 (funcall continue nil mod-vars))))
    (get-mod-var
     (slot.getter slot) mod-vars nil
     (lambda (getter mv)
       (get-mod-var
        (slot.setter slot) mv t
        (lambda (setter mv)
          (compile-forms
           (list (slot.type slot) (slot.init-value slot)
                 (slot.init-function slot))
           mv bound-vars compiler nil
           (lambda (compiled-forms mv)
             (funcall continue
                      `(dylan%add-slot ,owner
                                       ,(car compiled-forms)   ; Type
                                       ',(slot.allocation slot)
                                       ,setter
                                       ,getter
                                       ',(slot.debug-name slot)
                                       ,(cadr compiled-forms)  ; Init-Value
                                       ,(slot.has-initial-value? slot)
                                       ,(caddr compiled-forms) ; Init-Function
                                       ',(slot.init-keyword slot)
                                       ',(slot.required-init-keyword slot))
                      mv)))))))))

(defun generate-define-class (name superclasses getter-code slots bound-vars
                              mod-vars compiler continue)
  (labels ((walk (slots-left mod-vars add-slot-calls)
             (if (null slots-left)
                 (module-refs
                  name bound-vars mod-vars continue
                  (lambda (ref set)
                    `(progn
                       (if (dylan%not (or (dylan%eq? ,ref
                                                     ',the-unassigned-value)
                                          (dylan%class? ,ref)))
                           (dylan-call dylan/error
                                       "define-class -- already has a value"
                                       ',name ,ref
                                       ',(mapcar #'slot.getter slots))
                           (let ((!class
                                   (dylan%make-a-class
                                    ',name
                                    (dylan%list ,@superclasses)
                                    (dylan%list ,@getter-code))))
                             ,@(reverse add-slot-calls)
                             ,(funcall set '!class)
                             ',name)))))
                 (make-add-slot-call
                  (car slots-left) '!class bound-vars mod-vars compiler
                  (lambda (code mod-vars)
                    (walk (cdr slots-left) mod-vars
                          (cons code add-slot-calls)))))))
    (walk slots mod-vars '())))

(defun compile-define-class-form (e mod-vars bound-vars compiler
                                  multiple-values? continue)
  (declare (ignore multiple-values?))   ; No reductions
  (must-be-list-of-at-least-length e 2 "DEFINE-CLASS: invalid syntax")
  (let ((superclasses (cadr e))
        (slots (expand-slots (cddr e))))
    (compile-forms
     superclasses mod-vars bound-vars compiler nil
     (lambda (supers mod-vars)
       (labels ((walk (getter-code slots-left mod-vars)
                  (if (null slots-left)
                      (generate-define-class (car e) supers
                                             (reverse getter-code) slots
                                             bound-vars mod-vars compiler
                                             continue)
                      (module-refs (slot.getter (car slots-left))
                                   bound-vars mod-vars
                                   (lambda (code mod-vars)
                                     (walk (cons code getter-code)
                                           (cdr slots-left)
                                           mod-vars))
                                   (lambda (ref set)
                                     (declare (ignore set)) ; Ignored
                                     ref)))))
         (walk '() slots mod-vars))))))

;;; DEFINE-SLOT

(defun compile-define-slot-form (e mod-vars bound-vars compiler
                                 multiple-values? continue)
  (declare (ignore multiple-values?))   ; Ignored
  (must-be-list-of-at-least-length e 2 "DEFINE-SLOT: invalid syntax")
  (let ((owner (car e))
        (slot (cdr e)))
    ;; The Scheme spliced OWNER into the output uncompiled, which happened
    ;; to work because a Dylan module variable compiled to a Scheme
    ;; variable of the same name.  Module variables have names of their own
    ;; here, so the owner expression is compiled like any other.
    (funcall compiler owner mod-vars bound-vars nil
             (lambda (compiled-owner mod-vars)
               (make-add-slot-call
                (expand-slot (if (keyword? (car slot)) slot `(:getter ,@slot)))
                compiled-owner bound-vars mod-vars compiler continue)))))
