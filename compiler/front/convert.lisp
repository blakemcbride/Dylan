;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conversion of Dylan forms to core forms (core.lisp).
;;;;
;;;; CONVERT handles the special forms itself.  Every other form whose
;;;; operator names a macro is expanded (macros.lisp) and the expansion
;;;; converted in its place; anything else is a function call.  As in the
;;;; Common Lisp implementation, special form and macro names are
;;;; recognized by name: a lexical variable cannot shadow them.
;;;;
;;;; The lexical environment is an alist from names -- symbols, or
;;;; (setter symbol) -- to LEXICAL-VARs.  A name it does not hold refers to
;;;; a variable of the current module.

(in-package #:dylan-compiler)

(defvar *special-forms* (make-hash-table :test #'eq)
  "Special form name -> function of (form env) answering a node.")

(defmacro define-special-form (name (form env) &body body)
  `(setf (gethash ',name *special-forms*)
         (lambda (,form ,env)
           (declare (ignorable ,form ,env))
           ,@body)))

;;; Variables

(defun lookup-lexical (name env)
  (cdr (assoc name env :test #'equal)))

(defun bind-lexical (name env)
  "ENV extended with a fresh variable for NAME; answer the new environment
and the variable."
  (let ((var (make-lexical-var name)))
    (values (acons name var env) var)))

(defun reference-module-var (name)
  (let ((var (lookup-module-var *module* name)))
    (when (module-var-macro var)
      (compile-error "~a is a macro: it cannot be used as a variable" (name-string name)))
    (unless (module-var-first-reference var)
      (setf (module-var-first-reference var) (or *location* t)))
    var))

(defun convert-variable (name env)
  (make-ref (or (lookup-lexical name env) (reference-module-var name))))

;;; The converter

(defun convert (form env)
  (with-form (form)
    (cond ((global-name-p form)
           (let ((var (global-name-var form)))
             (unless (module-var-first-reference var)
               (setf (module-var-first-reference var) (or *location* t)))
             (make-ref var)))
          ((self-evaluating-p form)
           (check-literal form)
           (make-constant form))
          ((variable-name-p form) (convert-variable form env))
          ((symbolp form)
           (compile-error "~a cannot be used as a variable" form))
          ((not (consp form))
           (compile-error "~a is not a Dylan expression" form))
          ((and (symbolp (car form)) (gethash (car form) *special-forms*))
           (funcall (gethash (car form) *special-forms*) form env))
          ((and (symbolp (car form)) (find-macro (car form)))
           (convert (expand-macro form) env))
          ((user-macro (car form) env)
           (with-expansion (form) (convert (expand-user-macro form) env)))
          ((proper-list-p form)
           (make-call (convert (car form) env)
                      (mapcar (lambda (f) (convert f env)) (cdr form))))
          (t (compile-error "~a is not a Dylan expression" form)))))

(defun convert-body (forms env)
  "FORMS evaluated in order: the value of the last, or #f if none."
  (make-seq (mapcar (lambda (f) (convert f env)) forms)))

;;; Top level.  The forms of a top-level begin, and of the expansion of a
;;; top-level macro, are themselves top level, so a define-module may
;;; appear in them.

(defun convert-top-level (form)
  "The nodes for the top-level FORM, in order."
  (with-form (form)
    (cond ((and (consp form) (eq (car form) 'dylan::define-module))
           '())                         ; done before conversion began
          ((and (consp form) (eq (car form) 'dylan::define-macro))
           (define-user-macro form)
           '())
          ((and (consp form) (eq (car form) 'dylan::define-for-macros))
           (eval-for-macros form)
           '())
          ((and (consp form) (eq (car form) 'dylan::begin)
                (proper-list-p form))
           (mapcan #'convert-top-level (cdr form)))
          ((and (consp form) (symbolp (car form))
                (not (gethash (car form) *special-forms*))
                (find-macro (car form)))
           (convert-top-level (expand-macro form)))
          ((and (consp form) (user-macro (car form) '()))
           (with-expansion (form) (convert-top-level (expand-user-macro form))))
          (t (list (convert form '()))))))

(define-special-form dylan::define-module (form env)
  (compile-error "define-module must be at top level"))

(define-special-form dylan::define-macro (form env)
  (compile-error "define-macro must be at top level"))

(define-special-form dylan::define-for-macros (form env)
  (compile-error "define-for-macros must be at top level"))

;;; Simple special forms

(define-special-form quote (form env)
  (require-list form 2 "quote" nil)
  (check-literal (cadr form))
  (make-constant (cadr form)))

(define-special-form dylan::begin (form env)
  (require-list form 1 "begin")
  (convert-body (cdr form) env))

(define-special-form if (form env)
  (unless (list-of-length-p form 4)
    (compile-error "if: needs a test, a consequent and an alternative"))
  (make-if-node (convert (second form) env)
                (convert (third form) env)
                (convert (fourth form) env)))

(define-special-form dylan::setter (form env)
  (compile-error "bad setter variable ~a" form))

(define-special-form %primitive (form env)
  ;; Not available to programs: the Dylan reader interns nothing in this
  ;; package.
  (make-primitive (second form)
                  (mapcar (lambda (f) (convert f env)) (cddr form))))

(define-special-form dylan::unquote (form env)
  (compile-error "unquote (,) outside a backquote"))

(define-special-form dylan::unquote-splicing (form env)
  (compile-error "unquote-splicing (,@) outside a backquote"))

(define-special-form unwind-protect (form env)
  (require-list form 2 "unwind-protect")
  (make-unwind-protect-node (convert (second form) env)
                            (convert-body (cddr form) env)))

;;; SET! (chapter 7)

(define-special-form dylan::set! (form env)
  (unless (list-of-length-p form 3)
    (compile-error "set!: needs a place and a new value"))
  (let ((place (second form))
        (value (third form)))
    (cond
      ((variable-name-p place)
       (let ((var (or (lookup-lexical place env) (reference-module-var place))))
         (if (lexical-var-p var)
             (setf (lexical-var-assigned-p var) t)
             (setf (module-var-assigned-p var) t))
         (make-assign var (convert value env))))
      ((and (consp place) (simple-variable-name-p (car place))
            (proper-list-p place))
       ;; The extended form: (set! (f arg ...) value) calls
       ;; ((setter f) arg ... value) and answers the value.  The arguments
       ;; are evaluated before the value, as in the call it stands for.
       (let* ((temps (mapcar (lambda (arg)
                               (if (self-evaluating-p arg) arg (temp "arg")))
                             (cdr place)))
              (value-temp (temp "value")))
         (convert
          `(dylan::bind (,@(loop for temp in temps
                                 for arg in (cdr place)
                                 unless (eq temp arg)
                                   collect (list temp arg))
                         (,value-temp ,value))
             ((dylan::setter ,(car place)) ,@temps ,value-temp)
             ,value-temp)
          env)))
      (t (compile-error "set!: bad place ~a" place)))))

;;; BIND (chapter 4): bindings made one after another, each binding
;;; variables to the values of one expression.
;;;
;;;   (bind ((var ... [#rest var] init) ...) body ...)
;;;
;;; A variable may be written (var type).  Each type is evaluated before
;;; the init, and the value bound is checked against it.

(define-special-form dylan::bind (form env)
  (require-list form 2 "bind")
  (let ((bindings (second form)))
    (unless (proper-list-p bindings)
      (compile-error "bind: bad bindings ~a" bindings))
    (convert-bindings bindings (cddr form) env)))

(defun convert-bindings (bindings body env)
  (if (null bindings)
      (convert-body body env)
      (with-form ((car bindings))
        (multiple-value-bind (vars rest init) (parse-binding (car bindings))
          (convert-binding vars rest init
                           (lambda (env) (convert-bindings (cdr bindings) body env))
                           env)))))

(defun parse-binding (binding)
  "Answer the variables of BINDING as a list of (name . type form), the
#rest variable's name or NIL, and the init form."
  (unless (and (proper-list-p binding) (>= (length binding) 2))
    (compile-error "bind: bad binding ~a" binding))
  (let* ((init (car (last binding)))
         (specs (butlast binding))
         (rest-tail (member 'dylan::!rest specs))
         (rest (and rest-tail
                    (if (and (= (length rest-tail) 2)
                             (variable-name-p (second rest-tail)))
                        (second rest-tail)
                        (compile-error "bind: bad #rest in ~a" binding))))
         (vars (mapcar (lambda (spec)
                         (cond ((variable-name-p spec) (cons spec nil))
                               ((and (list-of-length-p spec 2)
                                     (variable-name-p (car spec)))
                                (cons (car spec) (cadr spec)))
                               (t (compile-error "bind: bad variable ~a" spec))))
                       (ldiff specs rest-tail))))
    (check-unique (append (mapcar #'car vars) (and rest (list rest))) "bind")
    (values vars rest init)))

(defun convert-binding (vars rest init body-fn env)
  "Bind VARS (and REST) to the values of INIT, then the node BODY-FN
answers for the extended environment."
  (let ((typed (remove nil vars :key #'cdr))
        (type-vars '())
        (outer env))
    ;; The types first, each in a variable of its own.
    (let ((type-bindings
            (mapcar (lambda (spec)
                      (multiple-value-bind (new-env var)
                          (bind-lexical (make-symbol (format nil "~a-type"
                                                             (name-string (car spec))))
                                        outer)
                        (declare (ignore new-env))
                        (push (cons (car spec) var) type-vars)
                        (cons var (convert (cdr spec) outer))))
                    typed)))
      (let* ((value (convert init outer))
             (env outer)
             (lex-vars (mapcar (lambda (spec)
                                 (multiple-value-bind (new-env var)
                                     (bind-lexical (car spec) env)
                                   (setf env new-env)
                                   (let ((type (cdr (assoc (car spec) type-vars
                                                           :test #'equal))))
                                     (setf (lexical-var-type var) type))
                                   var))
                               vars))
             (rest-var (and rest
                            (multiple-value-bind (new-env var)
                                (bind-lexical rest env)
                              (setf env new-env)
                              var)))
             (checks (loop for var in lex-vars
                           when (lexical-var-type var)
                             collect (make-primitive
                                      :check-type
                                      (list (make-ref var)
                                            (make-ref (lexical-var-type var))))))
             (node (make-bind-node lex-vars rest-var value
                                   (make-seq (append checks
                                                     (list (funcall body-fn env)))))))
        ;; Wrap the type bindings around it, first type outermost.
        (dolist (tb (reverse type-bindings) node)
          (setf node (make-bind-node (list (car tb)) nil (cdr tb) node)))))))

;;; BIND-EXIT (chapter 8)

(define-special-form dylan::bind-exit (form env)
  (require-list form 2 "bind-exit")
  (let ((spec (second form)))
    (unless (and (list-of-length-p spec 1) (variable-name-p (car spec)))
      (compile-error "bind-exit: bad exit variable ~a" spec))
    (multiple-value-bind (env var) (bind-lexical (car spec) env)
      (make-bind-exit-node var (convert-body (cddr form) env)))))

;;; METHOD and BIND-METHODS (chapters 6 and 10)

(defun convert-method (params body env &key name implicit-next (what "method"))
  "A METHOD-NODE.  If IMPLICIT-NEXT and the parameters name no #next
variable, the next method is bound to next-method, as define-method
does."
  (let* ((parsed (parse-parameters params what))
         (required
           (mapcar (lambda (r)
                     (cons (car r)
                           (and (cdr r) (convert (cdr r) env))))
                   (parsed-params-required parsed)))
         (next-name (or (parsed-params-next parsed)
                        (and implicit-next
                             (not (member (dylan-name "next-method")
                                          (append (mapcar #'car required)
                                                  (list (parsed-params-rest parsed))
                                                  (let ((k (parsed-params-keys parsed)))
                                                    (and (consp k) (mapcar #'second k))))
                                          :test #'equal))
                             (dylan-name "next-method"))))
         (required-params
           (mapcar (lambda (r)
                     (multiple-value-bind (new-env var) (bind-lexical (car r) env)
                       (setf env new-env)
                       (make-required-param var (cdr r))))
                   required))
         (next (and next-name
                    (multiple-value-bind (new-env var) (bind-lexical next-name env)
                      (setf env new-env)
                      var)))
         (rest (and (parsed-params-rest parsed)
                    (multiple-value-bind (new-env var)
                        (bind-lexical (parsed-params-rest parsed) env)
                      (setf env new-env)
                      var)))
         (keys (let ((k (parsed-params-keys parsed)))
                 (if (listp k)
                     ;; Each default sees the parameters before it.
                     (mapcar (lambda (spec)
                               (destructuring-bind (keyword name default) spec
                                 (let ((default (convert default env)))
                                   (multiple-value-bind (new-env var)
                                       (bind-lexical name env)
                                     (setf env new-env)
                                     (make-key-param keyword var default)))))
                             k)
                     k))))
    (%make-method-node :name name
                       :required required-params
                       :next next
                       :rest rest
                       :keys keys
                       :body (convert-body body env))))

(define-special-form method (form env)
  (require-list form 2 "method")
  (convert-method (second form) (cddr form) env))

(define-special-form dylan::bind-methods (form env)
  (require-list form 2 "bind-methods")
  (let ((bindings (second form)))
    (unless (proper-list-p bindings)
      (compile-error "bind-methods: bad bindings ~a" bindings))
    (dolist (b bindings)
      (with-form (b)
        (unless (and (list-of-at-least-p b 2) (variable-name-p (car b)))
          (compile-error "bind-methods: bad binding ~a" b))))
    (check-unique (mapcar #'car bindings) "bind-methods")
    (let ((vars '()))
      (dolist (b bindings)
        (multiple-value-bind (new-env var) (bind-lexical (car b) env)
          (setf env new-env)
          (push var vars)))
      (make-letrec-node (nreverse vars)
                        (mapcar (lambda (b)
                                  (with-form (b)
                                    (convert-method (second b) (cddr b) env
                                                    :name (car b)
                                                    :what "bind-methods")))
                                bindings)
                        (convert-body (cddr form) env)))))

;;; Definitions

(defun note-definition (var node)
  (setf (module-var-definitions var)
        (append (module-var-definitions var) (list node)))
  node)

(define-special-form dylan::define (form env)
  (unless (list-of-length-p form 3)
    (compile-error "define: needs a variable name and a value"))
  (require-variable-name (second form) "define")
  ;; The definition is noted before its parts are converted, so that an
  ;; error in them does not also make the variable seem undefined.
  (let* ((var (defining-module-var *module* (second form) "define"))
         (node (note-definition var (make-define-variable-node var nil))))
    (setf (define-variable-node-value node) (convert (third form) env))
    node))

;;; (define-thread-variable name value): an extension (CompilerPlan.md,
;;; Phase 7).  Each thread has its own value of the variable; a thread
;;; made after the definition has run starts with VALUE.

(define-special-form dylan::define-thread-variable (form env)
  (unless (list-of-length-p form 3)
    (compile-error "define-thread-variable: needs a variable name and a value"))
  (require-variable-name (second form) "define-thread-variable")
  (let* ((var (defining-module-var *module* (second form) "define-thread-variable"))
         (node (note-definition var (make-define-variable-node var nil))))
    (setf (module-var-thread-local-p var) t)
    (setf (define-variable-node-value node) (convert (third form) env))
    node))

(define-special-form dylan::define-method (form env)
  (require-list form 3 "define-method")
  (require-variable-name (second form) "define-method")
  ;; A method may be added to an imported generic function.
  (let* ((var (reference-module-var (second form)))
         (node (note-definition var (make-define-method-node var nil))))
    (setf (define-method-node-method node)
          (convert-method (third form) (cdddr form) env
                          :name (second form)
                          :implicit-next t
                          :what "define-method"))
    node))

;;; (define-generic-function name (required ... [#rest var] [#key key ...])
;;;                          option ...)
;;; Options are keyword/value pairs, accepted and ignored.

(define-special-form dylan::define-generic-function (form env)
  (require-list form 3 "define-generic-function")
  (let ((name (second form))
        (params (third form))
        (options (cdddr form)))
    (require-variable-name name "define-generic-function")
    (unless (proper-list-p params)
      (compile-error "define-generic-function: bad parameter list ~a" params))
    (unless (and (evenp (length options))
                 (loop for k in options by #'cddr always (keywordp k)))
      (compile-error "define-generic-function: bad options ~a" options))
    (let ((required 0) rest keys (l params))
      (loop while (and l (not (parameter-marker-p (car l))))
            do (unless (variable-name-p (car l))
                 (compile-error "define-generic-function: bad parameter ~a" (car l)))
               (incf required)
               (pop l))
      (when (eq (car l) 'dylan::!rest)
        (pop l)
        (unless (and l (variable-name-p (car l)))
          (compile-error "define-generic-function: #rest needs a variable name"))
        (pop l)
        (setf rest t))
      (when (eq (car l) 'dylan::!key)
        (pop l)
        (setf keys
              (if (null l)
                  :any
                  (loop while l
                        collect (let ((k (pop l)))
                                  (cond ((keywordp k) k)
                                        ((simple-variable-name-p k) (name-keyword k))
                                        (t (compile-error
                                            "define-generic-function: bad keyword ~a"
                                            k)))))))
        (when (consp keys) (check-unique keys "define-generic-function")))
      (when l
        (compile-error "define-generic-function: unexpected ~a in parameter list"
                       (car l)))
      (let ((var (defining-module-var *module* name "define-generic-function")))
        (note-definition var (make-define-generic-node var required rest keys))))))

;;; DEFINE-CLASS and DEFINE-SLOT (chapter 11)
;;;
;;;   slot: getter | (getter option ...) | (option ...)
;;;   options: getter: setter: type: init-value: init-function:
;;;            init-keyword: required-init-keyword: allocation:

(defun parse-slot (spec env)
  (with-form (spec)
    (multiple-value-bind (default-getter options)
        (cond ((simple-variable-name-p spec) (values spec '()))
              ((and (consp spec) (keywordp (car spec))) (values nil spec))
              ((and (consp spec) (simple-variable-name-p (car spec)))
               (values (car spec) (cdr spec)))
              (t (compile-error "bad slot specification ~a" spec)))
      (let* ((alist (keyword-options options
                                     '(:getter :setter :type :init-value
                                       :init-function :init-keyword
                                       :required-init-keyword :allocation)
                                     "slot"))
             (getter (or (cdr (assoc :getter alist)) default-getter
                         (compile-error "slot: no getter name in ~a" spec)))
             (setter-entry (assoc :setter alist))
             (setter (cond ((null setter-entry) (list 'dylan::setter getter))
                           ((false-p (cdr setter-entry)) nil)
                           (t (cdr setter-entry))))
             (init-value (assoc :init-value alist))
             (init-function (assoc :init-function alist))
             (init-keyword (cdr (assoc :init-keyword alist)))
             (required (cdr (assoc :required-init-keyword alist)))
             (allocation (let ((a (assoc :allocation alist)))
                           (if a (cdr a) (dylan-name "instance")))))
        (unless (simple-variable-name-p getter)
          (compile-error "slot: bad getter name ~a" getter))
        (when setter (require-variable-name setter "slot setter"))
        (when (and init-value init-function)
          (compile-error "slot ~a: both init-value: and init-function:" getter))
        (when (and init-keyword (not (keywordp init-keyword)))
          (compile-error "slot ~a: init-keyword: must be a keyword" getter))
        (when (and required (not (keywordp required)))
          (compile-error "slot ~a: required-init-keyword: must be a keyword" getter))
        (when (and required (or init-keyword init-value init-function))
          (compile-error "slot ~a: required-init-keyword: with an init-keyword: or initial value"
                         getter))
        (let ((allocation-keyword
                ;; Chapter 6: one of the symbols, not a keyword
                (and (symbolp allocation)
                     (not (keywordp allocation))
                     (find (symbol-name allocation)
                           '(:instance :class :each-subclass :constant :virtual)
                           :test #'string-equal))))
          (unless allocation-keyword
            (compile-error "slot ~a: allocation: must be one of the symbols instance, class, each-subclass, constant and virtual, not ~a"
                           getter (dylan-form-string allocation)))
          (%make-slot-spec
           :getter (reference-module-var getter)
           :setter (and setter (reference-module-var setter))
           :type (and (assoc :type alist) (convert (cdr (assoc :type alist)) env))
           :init-value (and init-value (convert (cdr init-value) env))
           :init-function (and init-function (convert (cdr init-function) env))
           :init-keyword init-keyword
           :required-init-keyword required
           :allocation allocation-keyword
           :location *location*))))))

(define-special-form dylan::define-class (form env)
  (require-list form 3 "define-class")
  (let ((name (second form))
        (supers (third form)))
    (require-variable-name name "define-class")
    (unless (proper-list-p supers)
      (compile-error "define-class: bad superclass list ~a" supers))
    (let* ((var (defining-module-var *module* name "define-class"))
           (slots (mapcar (lambda (s) (parse-slot s env)) (cdddr form))))
      (check-unique (mapcar (lambda (s) (module-var-name (slot-spec-getter s))) slots)
                    "define-class slots")
      (note-slot-definitions
       slots
       (note-definition var (make-define-class-node
                             var
                             (mapcar (lambda (s) (convert s env)) supers)
                             slots))))))

(defun note-slot-definitions (slots node)
  "A slot's getter and setter are generic functions its definition
defines (unless they are defined elsewhere too)."
  (dolist (slot slots node)
    (note-definition (slot-spec-getter slot) node)
    (when (slot-spec-setter slot)
      (note-definition (slot-spec-setter slot) node))))

(define-special-form dylan::define-slot (form env)
  (require-list form 3 "define-slot")
  (let ((owner (second form))
        (spec (cddr form)))
    (let ((slot (parse-slot spec env)))
      (note-slot-definitions (list slot)
                             (make-define-slot-node (convert owner env) slot)))))
