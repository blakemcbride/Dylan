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
  ;; The init form, last, is an expression, never the #rest marker
  (cadr (member '!rest (butlast binding))))

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
                ;; Variables the values don't reach are bound to #f.
                (dylan%vector ,@(mapcar (lambda (name)
                                          (declare (ignore name))
                                          'dylan%false)
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
                                                    'dylan%false)
                                                  (cdr bound-names))
                                        ,@(if rest-name '('()) '()))))
                    (if (null restricted)
                        call
                        `(progn
                           ,@(mapcar (lambda (entry)
                                       `(dylan%type-check
                                         ,(if (= (funcall ->offset entry) 0)
                                              '!first-value
                                              'dylan%false)
                                         ,(funcall ->name entry)))
                                     restricted)
                           ,call)))))))))

;;; Compiling the BIND form

(defun check-binding (binding)
  "Check a bind form's BINDING as the compiler to C checks it, with its
words (compiler/front/convert.lisp, PARSE-BINDING)."
  (unless (list-of-at-least-length? binding 2)
    (syntax-error "bind" "bad binding ~a" binding))
  (let* ((specs (butlast binding))
         (rest-tail (member '!rest specs))
         (rest (and rest-tail
                    (if (and (= (length rest-tail) 2)
                             (variable-name? (second rest-tail)))
                        (second rest-tail)
                        (syntax-error "bind" "bad #rest in ~a" binding))))
         (names (mapcar (lambda (spec)
                          (cond ((variable-name? spec) spec)
                                ((and (list-of-length? spec 2)
                                      (variable-name? (car spec)))
                                 (car spec))
                                (t (syntax-error "bind" "bad variable ~a" spec))))
                        (ldiff specs rest-tail))))
    (check-names-once "bind" (append names (and rest (list rest))))))

(defun compile-bind-form (e module-vars bound-vars really-compile
                          multiple-values? continue)
  (must-have-syntax e 1 "bind")
  (let ((bindings (car e))
        (forms (cdr e)))
    (if (null bindings)
        (if (null forms)
            (funcall continue compiled-sharp-f module-vars)
            (funcall really-compile `(begin ,@forms)
                     module-vars bound-vars multiple-values? continue))
        (progn
          (unless (proper-list? bindings)
            (syntax-error "bind" "bad bindings ~a" bindings))
          (check-binding (car bindings))
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
                            ;; #key alone accepts no keywords but those
                            ;; its methods name (chapter 6)
                            (if (null (cddr after-rest))
                                (funcall compiler name reqs rest t)
                                (parse-gen-fn-params
                                 (cddr after-rest) #'symbol-or-keyword
                                 (compile-keys rest))))
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

(defun check-generic-function-form (e)
  "Check the define-generic-function form E, less its first element, as
the compiler to C checks it, with its words."
  (let ((name (car e))
        (params (cadr e))
        (options (cddr e))
        (what "define-generic-function"))
    (require-variable-name name what)
    (unless (proper-list? params)
      (syntax-error what "bad parameter list ~a" params))
    (unless (and (evenp (length options))
                 (loop for k in options by #'cddr always (keyword? k)))
      (syntax-error what "bad options ~a" options))
    (let ((l params) (keys '()))
      (loop while (and l (not (member (car l) '(!next !rest !key))))
            do (unless (variable-name? (car l))
                 (syntax-error what "bad parameter ~a" (car l)))
               (pop l))
      (when (eq (car l) '!rest)
        (pop l)
        (unless (and l (variable-name? (car l)))
          (syntax-error what "#rest needs a variable name"))
        (pop l))
      (when (eq (car l) '!key)
        (pop l)
        (loop while l
              do (let ((k (pop l)))
                   (cond ((keyword? k) (push k keys))
                         ((simple-variable-name? k) (push (name->keyword k) keys))
                         (t (syntax-error what "bad keyword ~a" k)))))
        (check-names-once what (reverse keys)))
      (when l
        (syntax-error what "unexpected ~a in parameter list" (car l))))))

(defun compile-define-generic-function-form (e mod-vars bound-vars compiler
                                             multiple-values? continue)
  (declare (ignore compiler multiple-values?)) ; No sub-compilations or
                                        ; reductions
  (must-have-syntax e 2 "define-generic-function")
  ;; Keyword options after the parameter list are accepted and ignored.
  (check-generic-function-form e)
  (parse-define-generic-function
   (car e) (cadr e) (cddr e)
   (lambda (name reqs rest keys)
     (module-refs
      name bound-vars mod-vars
      continue
      (lambda (ref set)
        `(progn
          (dylan%check-not-class
           ',(name->module-variable (variable->name name)) ',name)
          (if (dylan%not (or (dylan%eq? ,ref ',the-unassigned-value)
                            (dylan%generic-function? ,ref)))
             ;; As the compiler to C words it
             (dylan-call dylan/error
                         ,(format nil "~a is defined both as a variable and as a generic function"
                                  (definition-name-string name)))
             (progn
               ;; Chapter 6: an existing generic function is kept, with
               ;; its methods, if the definition is consistent with it.
               (if (dylan%generic-function? ,ref)
                   (dylan%check-generic-function-definition
                    ,ref ',name ,(length reqs) ',keys ,(if rest t nil))
                   ,(funcall set `(dylan%create-generic-function
                                   ',name ,(length reqs) ',keys
                                   ,(if rest t nil))))
               ;; Chapter 6: it makes read-only the variable it creates;
               ;; one the program imports from the library it only checks.
               ,@(unless (member (variable->name name) dylan%predefined-variables)
                   `((dylan%make-variable-read-only
                      ',(name->module-variable (variable->name name)))))
               ',name))))))))

;;; Parsing and compiling the DEFINE-CLASS form

(defun expand-slot (slot)
  "SLOT, a slot specification of define-class or define-slot, as a slot.
Its faults are worded as the compiler to C words them."
  (labels
      ((check-options (pairs)
         ;; Keyword/value pairs, each keyword one a slot takes, and each at
         ;; most once (chapter 6)
         (let ((seen '()))
           (loop for rest = pairs then (cddr rest)
                 until (null rest)
                 do (cond ((not (and (consp rest) (keyword? (car rest))
                                     (consp (cdr rest))))
                           (dylan%error "slot: bad keyword options ~a" rest))
                          ((not (member (car rest)
                                        '(:getter :setter :type :init-value
                                          :init-function :init-keyword
                                          :required-init-keyword :allocation)))
                           (dylan%error "slot: unknown option ~a" (car rest)))
                          ((member (car rest) seen)
                           (dylan%error "slot: option ~a given twice" (car rest)))
                          (t (push (car rest) seen))))))
       (simple-name? (x) (and (symbolp x) (variable-name? x)))
       (build-slot (pairs default-getter)
         (check-options pairs)
         (let* ((getter
                  (dylan%find-keyword
                   pairs :getter
                   (lambda ()
                     (or default-getter
                         (dylan%error "slot: no getter name in ~a" slot)))))
                ;; Chapter 6: the default setter is (setter slot-name)
                ;; if the slot name is given; otherwise there is none.
                (setter
                  (let ((setter (dylan%find-keyword
                                 pairs :setter
                                 (lambda ()
                                   (if default-getter
                                       `(setter ,default-getter)
                                       dylan%false)))))
                    (if (false? setter) nil setter)))   ; setter: #f
                (type-given? t)
                (type (dylan%find-keyword pairs :type
                                          (lambda () (setf type-given? nil) '<object>)))
                (has-initial-value? t)
                (init-value
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
                  (dylan%find-keyword pairs :allocation (lambda () 'instance))))
           (unless (simple-name? getter)
             (dylan%error "slot: bad getter name ~a" getter))
           (when (and setter (not (variable-name? setter)))
             (dylan%error "slot setter: ~a is not a variable name" setter))
           (when (and has-initial-value? init-function)
             (dylan%error "slot ~a: both init-value: and init-function:" getter))
           (when (and init-keyword (not (keyword? init-keyword)))
             (dylan%error "slot ~a: init-keyword: must be a keyword" getter))
           (when (and required-init-keyword (not (keyword? required-init-keyword)))
             (dylan%error "slot ~a: required-init-keyword: must be a keyword" getter))
           (when (and required-init-keyword
                      (or init-keyword has-initial-value? init-function))
             (dylan%error "slot ~a: required-init-keyword: with an init-keyword: or initial value"
                          getter))
           (unless (member allocation '(instance class each-subclass constant virtual))
             (dylan%error "slot ~a: allocation: must be one of the symbols instance, class, each-subclass, constant and virtual, not ~a"
                          getter allocation))
           ;; Chapter 6: a constant slot has no setter method, and so
           ;; defines no setter variable
           (when (eq allocation 'constant)
             (setf setter nil))
           (let ((slot (make-slot getter            ; debug-name
                                  getter setter type init-value has-initial-value?
                                  init-function init-keyword required-init-keyword
                                  allocation nil nil)))
             (setf (slot.type-given? slot) type-given?)
             slot))))
    (cond ((and (symbolp slot) (not (null slot)))
           (unless (simple-name? slot)
             (dylan%error "slot: bad getter name ~a" slot))
           (let ((spec (make-slot slot slot `(setter ,slot) '<object> nil nil nil nil nil
                                  'instance nil nil)))
             (setf (slot.type-given? spec) nil)
             spec))
          ((and (consp slot) (keyword? (car slot)))
           (build-slot slot nil))
          ((and (consp slot) (simple-name? (car slot)))
           (build-slot (cdr slot) (car slot)))
          (t (dylan%error "bad slot specification ~a" slot)))))

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
                              ;; As the compiler to C words it
                              (dylan-call dylan/error
                                          ,(format nil "~a is defined both as a variable and as a generic function"
                                                   (definition-name-string name)))))
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
                                       ',(slot.required-init-keyword slot)
                                       ',(slot.type-given? slot))
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
                           ;; As the compiler to C words it
                           (dylan-call dylan/error
                                       ,(format nil "~a is defined both as a class and otherwise"
                                                (definition-name-string name)))
                           ;; Chapter 6: the slots' types and initial
                           ;; values are evaluated after the variable holds
                           ;; the class, so they may name it.  A class
                           ;; redefined keeps its identity, the variable's
                           ;; value, and takes the new one's slots after.
                           (let ((!class
                                   (dylan%make-a-class
                                    ',name
                                    (dylan%list ,@superclasses)
                                    (dylan%list ,@getter-code)))
                                 (!redefined (dylan%class? ,ref))
                                 (!done nil))
                             ;; An error in a slot cancels the definition
                             ;; (chapter 5): the new class is discarded,
                             ;; and the variable is as it was.
                             (unwind-protect
                                  (progn
                                    (unless !redefined ,(funcall set '!class))
                                    ,@(reverse add-slot-calls)
                                    (setf !done t))
                               (unless !done
                                 (dylan%discard-class !class)
                                 (unless !redefined
                                   ,(funcall set `',the-unassigned-value))))
                             (when !redefined
                               (dylan%redefine-class ,ref !class))
                             (dylan%make-variable-read-only
                              ',(name->module-variable (variable->name name)))
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
  (must-have-syntax e 2 "define-class")
  (require-variable-name (car e) "define-class")
  (unless (proper-list? (cadr e))
    (syntax-error "define-class" "bad superclass list ~a" (cadr e)))
  (let ((superclasses (cadr e))
        (slots (expand-slots (cddr e))))
    ;; Two slots with one getter would be one slot (chapter 6).
    (check-names-once "define-class slots" (mapcar #'slot.getter slots))
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
  (must-have-syntax e 2 "define-slot")
  (let ((owner (car e))
        (slot (cdr e)))
    ;; The Scheme spliced OWNER into the output uncompiled, which happened
    ;; to work because a Dylan module variable compiled to a Scheme
    ;; variable of the same name.  Module variables have names of their own
    ;; here, so the owner expression is compiled like any other.
    ;; Chapter 6: the slot name and options are a define-class slot's;
    ;; define-slot answers the slot's name, its getter.
    (let ((expanded (expand-slot slot)))
      (funcall compiler owner mod-vars bound-vars nil
               (lambda (compiled-owner mod-vars)
                 (make-add-slot-call
                  expanded compiled-owner bound-vars mod-vars compiler
                  (lambda (code mod-vars)
                    (funcall continue
                             `(progn ,code
                                     ,(if (slot.getter expanded)
                                          `',(slot.getter expanded)
                                          'dylan%false))
                             mod-vars))))))))
