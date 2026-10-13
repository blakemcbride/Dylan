;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; The object system (CompilerPlan.md, Phase 5): classes, slots, generic
;;;; functions and their dispatch, and make.
;;;;
;;;; The program is static, so the compiler knows every class and every
;;;; method.  Superclasses and specializers must therefore be known at
;;;; compile time: classes, or singletons of constants or classes.  A slot's
;;;; type, init-value and init-function are evaluated when its define-class
;;;; runs, as chapter 6 says, into static variables the accessors and
;;;; makers read.
;;;;
;;;; Each generic function gets a C dispatch function: a decision tree on
;;;; its arguments -- tests for singletons, then a switch on the class id,
;;;; whose cases are ranges of ids -- that ends at a leaf for each distinct
;;;; sorted list of applicable methods.  The lists are computed here, by
;;;; chapter 10's rule: specializers compared left to right, a singleton
;;;; before any class, and a class before its superclasses -- among classes
;;;; neither of which is a subclass of the other, as in CLOS, the one
;;;; earlier in the precedence list of the argument's class.  A leaf checks
;;;; keyword arguments against its methods (chapter 6), sets the thread's
;;;; next method and tail-calls the first.  The next methods are static
;;;; objects, one for each tail of each list.
;;;;
;;;; Two slots are the same slot if they have the same getter (chapter 6).
;;;; Each instance slot of the program has a number; each class's wrapper
;;;; maps it to the slot's index in that class's instances.

(in-package #:dylan-compiler)

(defvar *let-values* nil "IVAR -> the expression a let binds it to.")
(defvar *gclasses* nil "MODULE-VAR -> GCLASS, for the program's classes.")
(defvar *gfuns* nil "MODULE-VAR -> GFUN.")
(defvar *method-of-fn* nil "IR-FN -> GMETHOD, for define-method fns.")
(defvar *constant-globals* nil "MODULE-VAR -> C expression of its value.")
(defvar *sdef-of-node* nil "definition node -> its SDEFs.")
(defvar *object-text* nil "C text for the object system, newest first.")
(defvar *object-decls* nil "C declarations it needs early, newest first.")
(defvar *instance-slot-numbers* nil "getter MODULE-VAR -> number.")
(defvar *definition-order* 0)

(defstruct gclass var info sdefs location ready-c)

(defstruct sdef
  class                                 ; CLASS-INFO
  getter setter                         ; MODULE-VARs (setter may be NIL)
  allocation
  spec                                  ; the IR's SLOT-SPEC
  id
  location
  order)

(defstruct gfun var c-name required variable-p methods declared-keys
  (object-c nil))

(defstruct gmethod
  specs                                 ; SPECs, one per required parameter
  kind                                  ; :ir :getter :setter :default
  fn sdef
  rest-p
  keys                                  ; NIL (no #key), :ANY, or keywords
  object-c                              ; C name of its <method> object
  entry-c                               ; C function dispatch calls
  location
  order)

(defstruct (spec (:constructor make-spec (kind class &optional value c)))
  kind                                  ; :class or :singleton
  class                                 ; :class: the class; :singleton:
                                        ; the class of the object
  value                                 ; :singleton: the object (Lisp)
  c)                                    ; :singleton: C expression for it

(defun object-spec () (make-spec :class (find-class-info "<object>" *classes*)))
(defun object-spec-p (s) (and (eq (spec-kind s) :class)
                              (string= (class-info-name (spec-class s)) "<object>")))

(defun emit-object (control &rest arguments)
  (push (apply #'format nil control arguments) *object-text*))

(defun declare-object (control &rest arguments)
  (push (apply #'format nil control arguments) *object-decls*))

(defun c-name-string (string)
  (c-string-literal (map 'list #'char-code string)))

;;; ------------------------------------------------------- Static values

(defun collect-let-values (ir)
  (dolist (fn (ir-fns ir))
    (labels ((walk (e)
               (typecase e
                 (ir-let (setf (gethash (ir-let-var e) *let-values*) (ir-let-value e))
                         (walk (ir-let-value e)) (walk (ir-let-body e)))
                 (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (walk (ir-letrec-body e)))
                 (ir-bind-exit (walk (ir-bind-exit-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e))))))
      (when (listp (ir-fn-keys fn))
        (dolist (k (ir-fn-keys fn)) (walk (ir-key-default k))))
      (walk (ir-fn-body fn)))))

(defun static-value (atom)
  "What ATOM is known at compile time to hold: (:CONSTANT value),
(:GLOBAL module-var), (:SINGLETON static-value), or NIL."
  (cond ((const-p atom) (list :constant (const-value atom)))
        ((and (ivar-p atom) (not (ivar-assigned-p atom)))
         (let ((value (gethash atom *let-values*)))
           (typecase value
             (ir-global-ref (list :global (ir-global-ref-var value)))
             ((or const ivar) (static-value value))
             (ir-call
              (let ((f (static-value (ir-call-function value)))
                    (args (ir-call-arguments value)))
                (and (eq (car f) :global)
                     (eq (second f) (dylan-var "singleton"))
                     (= (length args) 1)
                     (let ((arg (static-value (first args))))
                       (and arg (list :singleton arg)))))))))))

(defun global-class (var)
  "The CLASS-INFO of the class the module variable VAR holds, or NIL."
  (let ((g (gethash var *gclasses*)))
    (cond (g (gclass-info g))
          ((module-var-predefined-p var)
           (let ((c (find (dylan::dylan-symbol->string (name-symbol (module-var-name var)))
                          *classes* :key #'class-info-name :test #'string-equal)))
             (and c (class-info-exported-p c) (not (consp (module-var-name var))) c))))))

(defun resolve-class (atom what)
  (let ((v (static-value atom)))
    (or (and (eq (car v) :global) (global-class (second v)))
        (compile-error "~a must be a class known at compile time" what))))

(defun constant-class (value)
  "The class of the Dylan constant VALUE."
  (find-class-info
   (cond ((class-info-p value) "<class>")
         ((integerp value) "<integer>")
         ((typep value 'ratio) "<ratio>")
         ((typep value 'single-float) "<single-float>")
         ((floatp value) "<double-float>")
         ((characterp value) "<character>")
         ((or (eq value t) (false-p value)) "<boolean>")
         ((null value) "<empty-list>")
         ((consp value) "<pair>")
         ((keywordp value) "<keyword>")
         ((symbolp value) "<symbol>")
         ((stringp value) (if (every (lambda (c) (< (char-code c) 256)) value)
                              "<byte-string>" "<unicode-string>"))
         ((vectorp value) "<simple-object-vector>")
         (t (error "No class for ~s." value)))
   *classes*))

(defun resolve-specializer (atom)
  (if (null atom)
      (object-spec)
      (let ((v (static-value atom)))
        (case (car v)
          (:global
           (make-spec :class
                      (or (global-class (second v))
                          (compile-error "a specializer must be a class or a singleton known at compile time"))))
          (:singleton
           (let ((x (second v)))
             (cond ((eq (car x) :constant)
                    (let ((value (second x)))
                      (make-spec :singleton (constant-class value) value
                                 (c-constant *cdata* value))))
                   ((and (eq (car x) :global) (global-class (second x)))
                    (let ((class (global-class (second x))))
                      (make-spec :singleton (constant-class class) class
                                 (address (class-c-name class)))))
                   (t (compile-error "a singleton specializer must be of a constant or a class")))))
          (t (compile-error "a specializer must be a class or a singleton known at compile time"))))))

(defun same-spec-p (a b)
  (and (eq (spec-kind a) (spec-kind b))
       (if (eq (spec-kind a) :class)
           (eq (spec-class a) (spec-class b))
           (eql (spec-value a) (spec-value b)))))

(defun spec-object-c (spec)
  "A C expression for SPEC as a Dylan object, for reflection."
  (if (eq (spec-kind spec) :class)
      (address (class-c-name (spec-class spec)))
      (let ((name (cdata-name *cdata* "dy_singleton_")))
        (define-c *cdata* "static DySingleton ~a = { &dy_wrapper_singleton, ~a };"
                  name (spec-c spec))
        (address name))))

(defun spec-vector-c (specs)
  (address (c-vector *cdata* (mapcar #'spec-object-c specs))))

;;; --------------------------------------------------------- Definitions

(defun collect-definitions (ir)
  "The definition nodes of IR, in program order."
  (let ((found '()))
    (dolist (fn (ir-fns ir))
      (labels ((walk (e)
                 (typecase e
                   ((or ir-define-class ir-define-slot ir-define-method ir-define-generic)
                    (push e found))
                   (ir-let (walk (ir-let-value e)) (walk (ir-let-body e)))
                   (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                   (ir-seq (mapc #'walk (ir-seq-forms e)))
                   (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                   (ir-letrec (walk (ir-letrec-body e)))
                   (ir-bind-exit (walk (ir-bind-exit-body e)))
                   (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e))))))
        (walk (ir-fn-body fn))))
    (nreverse found)))

(defmacro at-node ((node) &body body)
  `(let ((*location* (or (ir-node-location ,node) *location*))) ,@body))

(defun build-classes (definitions)
  "Make the program's classes and the class table."
  (let ((user '()))
    (dolist (d definitions)
      (when (ir-define-class-p d)
        (at-node (d)
          (let ((var (ir-define-class-var d)))
            (when (gethash var *gclasses*)
              (compile-error "~a is defined twice (classes cannot be redefined)"
                             (name-string (module-var-name var))))
            (let* ((info (%make-class-info
                          :name (dylan::dylan-symbol->string (name-symbol (module-var-name var)))
                          :kind :instance))
                   (g (make-gclass :var var :info info :location *location*
                                   :ready-c (format nil "dy_ready_~d" (hash-table-count *gclasses*)))))
              (setf (gethash var *gclasses*) g)
              (push (cons d g) user))))))
    (setf user (nreverse user))
    ;; Superclasses, now that every class exists.
    (loop for (d . g) in user
          do (at-node (d)
               (let ((supers (mapcar (lambda (a) (resolve-class a "a superclass"))
                                     (ir-define-class-superclasses d))))
                 (when (null supers)
                   (compile-error "define-class: at least one superclass is needed"))
                 (loop for (s . more) on supers
                       when (member s more)
                         do (compile-error "define-class: ~a is given twice" (class-info-name s)))
                 (dolist (s supers)
                   (unless (member (class-info-kind s) '(:none :instance))
                     (compile-error "~a cannot be a superclass of a class defined in the program"
                                    (class-info-name s))))
                 (setf (class-info-supers (gclass-info g)) supers))))
    (let ((all (append (built-in-class-infos-of *classes*)
                       (mapcar (lambda (u) (gclass-info (cdr u))) user))))
      (handler-case (setf *classes* (prepare-classes all))
        (error (e) (compile-error "~a" e))))))

(defun built-in-class-infos-of (classes)
  (remove :instance classes :key #'class-info-kind))

;;; Slots

(defvar *sdef-counter* 0)

(defun make-slot-definition (class spec location)
  (let* ((allocation (slot-spec-allocation spec))
         (sd (make-sdef :class class :getter (slot-spec-getter spec)
                        :setter (and (not (eq allocation :constant)) (slot-spec-setter spec))
                        :allocation allocation :spec spec :id (incf *sdef-counter*)
                        :location location :order (incf *definition-order*)))
         (name (name-string (module-var-name (slot-spec-getter spec)))))
    (let ((*location* (or location *location*)))
      (case allocation
        ((:class :each-subclass)
         (when (or (slot-spec-init-function spec) (slot-spec-init-keyword spec)
                   (slot-spec-required-init-keyword spec))
           (compile-error "slot ~a: a ~(~a~) slot takes only an init-value:" name allocation)))
        (:constant
         (unless (slot-spec-init-value spec)
           (compile-error "slot ~a: a constant slot needs an init-value:" name)))
        (:virtual
         (when (or (slot-spec-init-value spec) (slot-spec-init-function spec)
                   (slot-spec-init-keyword spec) (slot-spec-required-init-keyword spec))
           (compile-error "slot ~a: a virtual slot cannot be initialized" name)))))
    sd))

(defun build-slots (definitions)
  (dolist (d definitions)
    (typecase d
      (ir-define-class
       (at-node (d)
         (let ((g (gethash (ir-define-class-var d) *gclasses*)))
           (setf (gethash d *sdef-of-node*)
                 (mapcar (lambda (spec)
                           (make-slot-definition (gclass-info g) spec
                                                 (or (slot-spec-location spec) *location*)))
                         (ir-define-class-slots d)))
           (setf (gclass-sdefs g) (gethash d *sdef-of-node*)))))
      (ir-define-slot
       (at-node (d)
         (let* ((class (resolve-class (ir-define-slot-owner d) "define-slot's owner"))
                (g (find class (loop for g being the hash-values of *gclasses* collect g)
                         :key #'gclass-info)))
           (unless g
             (compile-error "define-slot: slots can be added only to classes the program defines"))
           (let ((sd (make-slot-definition class (ir-define-slot-slot d) *location*)))
             (setf (gethash d *sdef-of-node*) (list sd))
             (setf (gclass-sdefs g) (append (gclass-sdefs g) (list sd)))))))))
  ;; Number the instance slots, and lay out each class's instances.
  (let ((count 0))
    (loop for g being the hash-values of *gclasses*
          do (dolist (sd (gclass-sdefs g))
               (when (and (eq (sdef-allocation sd) :instance)
                          (not (gethash (sdef-getter sd) *instance-slot-numbers*)))
                 (setf (gethash (sdef-getter sd) *instance-slot-numbers*) count)
                 (incf count))))
    (loop for g being the hash-values of *gclasses*
          do (let ((layout '()))
               (dolist (c (reverse (class-info-cpl (gclass-info g))))
                 (let ((cg (class-gclass c)))
                   (when cg
                     (dolist (sd (gclass-sdefs cg))
                       (when (and (eq (sdef-allocation sd) :instance)
                                  (not (member (sdef-getter sd) layout)))
                         (setf layout (append layout (list (sdef-getter sd)))))))))
               (let ((offsets (make-array count :initial-element -1)))
                 (loop for getter in layout
                       for i from 0
                       do (setf (aref offsets (gethash getter *instance-slot-numbers*)) i))
                 (setf (class-info-size (gclass-info g)) (length layout)
                       (class-info-slot-offsets (gclass-info g))
                       (and (plusp count) offsets)))))))

(defun class-gclass (info)
  (loop for g being the hash-values of *gclasses*
        when (eq (gclass-info g) info) return g))

(defun effective-slots (info)
  "For the instances of the program's class INFO: each storage slot's
definition, the most specific in its precedence list that is not virtual,
in layout order."
  (let ((result '()))
    (dolist (c (reverse (class-info-cpl info)))
      (let ((g (class-gclass c)))
        (when g
          (dolist (sd (gclass-sdefs g))
            (unless (eq (sdef-allocation sd) :virtual)
              (let ((old (find (sdef-getter sd) result :key #'sdef-getter)))
                (if old
                    (setf result (substitute sd old result))
                    (setf result (append result (list sd))))))))))
    result))

;;; Static storage of a slot definition: its type, init-value and
;;; init-function as evaluated, and the storage of shared slots.

(defun sdef-c (sd what) (format nil "dy_slot~d_~a" (sdef-id sd) what))

(defun declare-slot-storage (sd)
  (let ((spec (sdef-spec sd)))
    (when (slot-spec-type spec)
      (declare-object "static D ~a = DY_UNBOUND;" (sdef-c sd "type")))
    (when (slot-spec-init-value spec)
      (declare-object "static D ~a = DY_UNBOUND;" (sdef-c sd "init")))
    (when (slot-spec-init-function spec)
      (declare-object "static D ~a = DY_UNBOUND;" (sdef-c sd "initfn")))
    (case (sdef-allocation sd)
      ((:class :constant) (declare-object "static D ~a = DY_UNBOUND;" (sdef-c sd "cell")))
      (:each-subclass
       (declare-object "static D ~a[~d];" (sdef-c sd "cells") (length *classes*))))))

(defun gen-slot-initialization (sd)
  "Code, for the define-class form, storing the slot definition's values."
  (let ((spec (sdef-spec sd)))
    (when (slot-spec-type spec)
      (emit "~a = ~a;" (sdef-c sd "type") (atom-c (slot-spec-type spec))))
    (when (slot-spec-init-value spec)
      (let ((v (atom-c (slot-spec-init-value spec))))
        (emit "~a = ~a;" (sdef-c sd "init") v)
        (when (and (slot-spec-type spec) (member (sdef-allocation sd) '(:class :each-subclass :constant)))
          (emit "dy_check_type(~a, ~a);" v (sdef-c sd "type")))
        (case (sdef-allocation sd)
          ((:class :constant) (emit "~a = ~a;" (sdef-c sd "cell") v))
          (:each-subclass
           (dolist (c *classes*)
             (when (subclass-info-p c (sdef-class sd))
               (emit "~a[~d] = ~a;" (sdef-c sd "cells") (class-info-id c) v)))))))
    (when (slot-spec-init-function spec)
      (emit "~a = ~a;" (sdef-c sd "initfn") (atom-c (slot-spec-init-function spec))))))

(defun gen-class-definition (node)
  "Code for a define-class or define-slot form, run where it appears."
  (dolist (sd (gethash node *sdef-of-node*))
    (gen-slot-initialization sd))
  (when (ir-define-class-p node)
    (emit "~a = 1;" (gclass-ready-c (gethash (ir-define-class-var node) *gclasses*)))))

;;; Accessors

(defun slot-name-c (sd) (c-name-string (name-string (module-var-name (sdef-getter sd)))))

(defun emit-instance-check (sd var)
  (emit-object "    if (!dy_instance_p(~a, &~a)) dy_not_a_slot(~a, ~a);"
               var (wrapper-c-name (sdef-class sd)) (slot-name-c sd) var))

(defun emit-storage-ref (sd var)
  "Emit code setting the C variable `place' to point at the storage of the
slot SD in the object VAR."
  (ecase (sdef-allocation sd)
    (:instance
     (emit-object "    const int32_t *o = dy_wrapper_of(~a)->slot_offsets;" var)
     (emit-object "    int32_t i = DY_IS_POINTER(~a) && o ? o[~d] : -1;" var
                  (gethash (sdef-getter sd) *instance-slot-numbers*))
     (emit-object "    if (i < 0) dy_not_a_slot(~a, ~a);" (slot-name-c sd) var)
     (emit-object "    D *place = &((DyInstance *)DY_PTR(~a))->slots[i];" var))
    ((:class :constant)
     (emit-instance-check sd var)
     (emit-object "    D *place = &~a;" (sdef-c sd "cell")))
    (:each-subclass
     (emit-instance-check sd var)
     (emit-object "    D *place = &~a[dy_wrapper_of(~a)->id];" (sdef-c sd "cells") var))))

(defun gen-accessors (sd)
  "The getter and setter methods of the slot definition SD."
  (let ((methods '())
        (getter (format nil "dy_slot~d_get" (sdef-id sd)))
        (setter (format nil "dy_slot~d_set" (sdef-id sd))))
    (declare-object "static D ~a(D, D, D, D, D, intptr_t);" getter)
    (emit-object "static D ~a(D self, D a0, D a1, D a2, D a3, intptr_t argc) {" getter)
    (emit-object "    (void)a1; (void)a2; (void)a3;")
    (emit-object "    if (argc != 1) dy_wrong_argument_count(self, argc);")
    (emit-storage-ref sd "a0")
    (emit-object "    if (*place == DY_UNBOUND) dy_unbound_slot(~a);" (slot-name-c sd))
    (emit-object "    return *place;~%}")
    (push (make-gmethod :specs (list (make-spec :class (sdef-class sd)))
                        :kind :getter :sdef sd :entry-c getter
                        :location (sdef-location sd) :order (sdef-order sd))
          methods)
    (when (sdef-setter sd)
      (declare-object "static D ~a(D, D, D, D, D, intptr_t);" setter)
      (emit-object "static D ~a(D self, D a0, D a1, D a2, D a3, intptr_t argc) {" setter)
      (emit-object "    (void)a2; (void)a3;")
      (emit-object "    if (argc != 2) dy_wrong_argument_count(self, argc);")
      (emit-storage-ref sd "a0")
      (when (slot-spec-type (sdef-spec sd))
        (emit-object "    if (~a != DY_UNBOUND) dy_check_type(a1, ~a);"
                     (sdef-c sd "type") (sdef-c sd "type")))
      (emit-object "    *place = a1;~%    return a1;~%}")
      (push (make-gmethod :specs (list (make-spec :class (sdef-class sd)) (object-spec))
                          :kind :setter :sdef sd :entry-c setter
                          :location (sdef-location sd) :order (sdef-order sd))
            methods))
    methods))

;;; --------------------------------------------------------- Generics

(defun gfun-for (var)
  (or (gethash var *gfuns*)
      (setf (gethash var *gfuns*)
            (make-gfun :var var :c-name (format nil "dy_gf~d" (hash-table-count *gfuns*))))))

(defun method-shape (m)
  "Number of required parameters, and whether more arguments may follow."
  (ecase (gmethod-kind m)
    (:ir (let ((fn (gmethod-fn m)))
           (values (length (ir-fn-required fn))
                   (or (ir-fn-rest fn) (ir-fn-keys fn)))))
    (:getter (values 1 nil))
    (:setter (values 2 nil))
    (:default (values 1 t))))

(defun build-generics (definitions)
  (dolist (d definitions)
    (at-node (d)
      (typecase d
        (ir-define-generic
         (let ((g (gfun-for (ir-define-generic-var d))))
           (when (gfun-declared-keys g)
             (compile-error "define-generic-function: ~a is defined twice"
                            (name-string (module-var-name (ir-define-generic-var d)))))
           (setf (gfun-required g) (ir-define-generic-required d)
                 (gfun-variable-p g) (or (ir-define-generic-rest d)
                                         (and (ir-define-generic-keys d) t))
                 (gfun-declared-keys g) (or (ir-define-generic-keys d) :none))))
        (ir-define-method
         (let* ((fn (ir-define-method-fn d))
                (var (ir-define-method-var d)))
           (when (and (module-var-predefined-p var)
                      (predefined-c-value var *classes*))
             (compile-error "methods cannot be added to ~a (it is not yet a generic function in the runtime)"
                            (name-string (module-var-name var))))
           (when (ir-fn-free-vars fn)
             (compile-error "define-method: the method refers to local variables (~{~a~^, ~})"
                            (mapcar (lambda (v) (name-string (ivar-name v))) (ir-fn-free-vars fn))))
           (let ((m (make-gmethod
                     :specs (mapcar #'resolve-specializer (ir-fn-specializers fn))
                     :kind :ir :fn fn
                     :rest-p (and (ir-fn-rest fn) t)
                     :keys (let ((k (ir-fn-keys fn)))
                             (if (listp k) (mapcar #'ir-key-keyword k) k))
                     :location *location* :order (incf *definition-order*))))
             (setf (gethash fn *method-of-fn*) m)
             (push m (gfun-methods (gfun-for var)))))))))
  ;; Slot accessors
  (loop for g being the hash-values of *gclasses*
        do (dolist (sd (gclass-sdefs g))
             (declare-slot-storage sd)
             (unless (eq (sdef-allocation sd) :virtual)
               (dolist (m (gen-accessors sd))
                 (push m (gfun-methods (gfun-for (if (eq (gmethod-kind m) :getter)
                                                     (sdef-getter sd)
                                                     (sdef-setter sd)))))))
             ;; Chapter 6: the getter and setter are generic functions even
             ;; if no method is added.
             (gfun-for (sdef-getter sd))
             (when (sdef-setter sd) (gfun-for (sdef-setter sd)))))
  ;; initialize has its method on <object>.
  (let ((init (gethash (dylan-var "initialize") *gfuns*)))
    (when init
      (push (make-gmethod :specs (list (object-spec)) :kind :default :rest-p t
                          :entry-c "dy_default_initialize.entry"
                          :object-c "dy_default_initialize" :order 0)
            (gfun-methods init))))
  (loop for g being the hash-values of *gfuns*
        do (finish-generic g)))

(defun finish-generic (g)
  (let ((var (gfun-var g)))
    (when (some (lambda (d) (define-variable-node-p d)) (module-var-definitions var))
      (let ((*location* (node-location (find-if #'define-variable-node-p
                                                (module-var-definitions var)))))
        (compile-error "~a is defined both as a variable and as a generic function"
                       (name-string (module-var-name var)))))
    ;; In program order; a method with the same specializers as an
    ;; earlier one replaces it (chapter 10).
    (let ((methods '()))
      (dolist (m (sort (copy-list (gfun-methods g)) #'< :key #'gmethod-order))
        (setf methods (remove-if (lambda (old)
                                   (and (= (length (gmethod-specs old)) (length (gmethod-specs m)))
                                        (every #'same-spec-p (gmethod-specs old) (gmethod-specs m))))
                                 methods))
        (setf methods (append methods (list m))))
      (setf (gfun-methods g) methods))
    ;; The parameter lists must be congruent (chapter 6).
    (dolist (m (gfun-methods g))
      (multiple-value-bind (required variable) (method-shape m)
        (cond ((null (gfun-required g))
               (setf (gfun-required g) required
                     (gfun-variable-p g) (and variable t)))
              ((or (/= required (gfun-required g))
                   (not (eq (and variable t) (gfun-variable-p g))))
               (let ((*location* (or (gmethod-location m) *location*)))
                 (compile-error "the parameter list of a method of ~a is not congruent with its generic function"
                                (name-string (module-var-name var))))))))
    (unless (gfun-required g) (setf (gfun-required g) 0))
    (setf (gethash var *constant-globals*) (address (gfun-c-name g)))))

;;; Method objects and direct entries

(defun method-objects ()
  (loop for g being the hash-values of *gfuns*
        do (dolist (m (gfun-methods g))
             (unless (gmethod-object-c m)
               (let ((name (cdata-name *cdata* "dy_method_")))
                 (setf (gmethod-object-c m) name)
                 (declare-object "DyFunction ~a;" name))))))

(defun emit-method-objects ()
  (loop for g being the hash-values of *gfuns*
        do (dolist (m (gfun-methods g))
             (unless (eq (gmethod-kind m) :default)
               (emit-object "DyFunction ~a = { &dy_wrapper_method, ~a, ~a };"
                            (gmethod-object-c m)
                            (if (eq (gmethod-kind m) :ir)
                                (format nil "~a_direct" (gmethod-entry-c m))
                                (gmethod-entry-c m))
                            (spec-vector-c (gmethod-specs m)))))))

(defun arg-c (i) (if (< i 4) (format nil "a~d" i) (format nil "dy_thread->args[~d]" (- i 4))))

(defun keyword-array (keywords)
  "The C name of a static array of KEYWORDS (Lisp keywords)."
  (let ((elements (or (mapcar (lambda (k) (c-constant *cdata* k)) keywords) '("0")))
        (name (cdata-name *cdata* "dy_keywords_")))
    (define-c *cdata* "static D ~a[] = { ~{~a~^, ~} };" name elements)
    name))

(defun emit-direct-entry (m)
  "The entry of a define-method method called directly, as a value: it
checks what the generic function would have, then enters the method with
no next method."
  (let* ((fn (gmethod-fn m))
         (r (length (ir-fn-required fn)))
         (name (format nil "~a_direct" (gmethod-entry-c m))))
    (declare-object "static D ~a(D, D, D, D, D, intptr_t);" name)
    (emit-object "static D ~a(D self, D a0, D a1, D a2, D a3, intptr_t argc) {" name)
    (if (or (ir-fn-rest fn) (ir-fn-keys fn))
        (emit-object "    if (argc < ~d) dy_wrong_argument_count(self, argc);" r)
        (emit-object "    if (argc != ~d) dy_wrong_argument_count(self, argc);" r))
    (loop for spec in (gmethod-specs m)
          for i from 0
          unless (object-spec-p spec)
            do (emit-object "    dy_check_type(~a, ~a);" (arg-c i) (spec-object-c spec)))
    (let ((keys (gmethod-keys m)))
      (cond ((and keys (gmethod-rest-p m))
             (emit-object "    dy_check_keywords(self, argc, ~d, a0, a1, a2, a3, NULL, 0);" r))
            (keys
             (let ((list (if (eq keys :any) '() keys)))
               (emit-object "    dy_check_keywords(self, argc, ~d, a0, a1, a2, a3, ~a, ~d);"
                            r (keyword-array list) (length list))))))
    (emit-object "    dy_thread->next_method = DY_FALSE;")
    (emit-object "    DY_TAIL_CALL_DIRECT(~a, self, a0, a1, a2, a3, argc);~%}"
                 (gmethod-entry-c m))))

;;; Dispatch

(defvar *leaves* nil "list of methods -> leaf label, for one generic.")
(defvar *nexts* nil "list of methods -> next-method object C name.")

(defun next-object (methods)
  "The C expression for the next method that runs METHODS in order."
  (if (null methods)
      "DY_FALSE"
      (address
       (or (gethash methods *nexts*)
           (setf (gethash methods *nexts*)
                 (let* ((name (cdata-name *cdata* "dy_next_"))
                        (m (first methods))
                        (after (next-object (rest methods))))
                   (declare-object "static D ~a_entry(D, D, D, D, D, intptr_t);" name)
                   (declare-object "static DyFunction ~a;" name)
                   (emit-object "static DyFunction ~a = { &dy_wrapper_method, ~a_entry, DY_FALSE };"
                                name name)
                   (emit-object "static D ~a_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {"
                                name)
                   (emit-object "    (void)self;~%    dy_thread->next_method = ~a;" after)
                   (emit-object "    DY_TAIL_CALL_DIRECT(~a, ~a, a0, a1, a2, a3, argc);~%}"
                                (gmethod-entry-c m) (address (gmethod-object-c m)))
                   name))))))

(defun spec-rank (spec class)
  "How specific SPEC is for an argument of CLASS: lower is more; NIL if
it does not apply."
  (ecase (spec-kind spec)
    (:singleton -1)
    (:class (position (spec-class spec) (class-info-cpl class)))))

(defun rank< (a b)
  (loop for x in a for y in b
        do (cond ((< x y) (return t))
                 ((> x y) (return nil)))
        finally (return nil)))

(defun singleton-test-c (spec arg)
  (let ((value (spec-value spec)))
    (if (or (typep value '(or ratio float)))
        (format nil "dy_identical(~a, ~a)" arg (spec-c spec))
        (format nil "~a == ~a" arg (spec-c spec)))))

(defun id-ranges (ids)
  (let ((ids (sort (copy-list ids) #'<)) (ranges '()))
    (dolist (id ids)
      (if (and ranges (= id (1+ (cdar ranges))))
          (setf (cdar ranges) id)
          (push (cons id id) ranges)))
    (nreverse ranges)))

(defun gen-dispatch-tree (g pos candidates indent)
  "Emit the decision tree for argument POS on, CANDIDATES being (method
. ranks so far)."
  (let ((pad (make-string (* 4 indent) :initial-element #\Space)))
    (cond
      ((null candidates)
       (emit-object "~agoto no_method;" pad))
      ((= pos (gfun-required g))
       (let ((sorted (mapcar #'car (stable-sort (copy-list candidates) #'rank<
                                                :key (lambda (c) (reverse (cdr c)))))))
         (emit-object "~agoto ~a;" pad
                      (or (gethash sorted *leaves*)
                          (setf (gethash sorted *leaves*)
                                (format nil "leaf_~d" (hash-table-count *leaves*)))))))
      ((every (lambda (c) (object-spec-p (nth pos (gmethod-specs (car c))))) candidates)
       (gen-dispatch-tree g (1+ pos) (mapcar (lambda (c) (cons (car c) (cons 0 (cdr c))))
                                             candidates)
                          indent))
      (t
       (let* ((arg (format nil "x~d" pos))
              (singletons (remove-duplicates
                           (loop for c in candidates
                                 for s = (nth pos (gmethod-specs (car c)))
                                 when (eq (spec-kind s) :singleton) collect s)
                           :test #'same-spec-p))
              (first t))
         (flet ((narrow (class singleton)
                  ;; The candidates applicable to an argument of CLASS (that
                  ;; is SINGLETON's object, if it is not NIL), with their rank.
                  (loop for c in candidates
                        for s = (nth pos (gmethod-specs (car c)))
                        for rank = (if (eq (spec-kind s) :singleton)
                                       (and singleton (same-spec-p s singleton) -1)
                                       (spec-rank s class))
                        when rank collect (cons (car c) (cons rank (cdr c))))))
           (dolist (s singletons)
             (emit-object "~a~:[} else ~;~]if (~a) {" pad first (singleton-test-c s arg))
             (setf first nil)
             (gen-dispatch-tree g (1+ pos) (narrow (spec-class s) s) (1+ indent)))
           (unless first (emit-object "~a} else {" pad))
           (let ((groups '()))           ; (narrowed . ids)
             (dolist (class *classes*)
               (let* ((narrowed (narrow class nil))
                      (key (mapcar (lambda (c) (cons (car c) (cadr c))) narrowed))
                      (group (assoc key groups :test #'equal)))
                 (if group
                     (push (class-info-id class) (cddr group))
                     (push (list* key narrowed (list (class-info-id class))) groups))))
             (let ((pad2 (if first pad (concatenate 'string pad "    ")))
                   (cases '()))         ; (code . ids): groups whose code is
                                        ; the same share their case
               (dolist (group (reverse groups))
                 (destructuring-bind (key narrowed &rest ids) group
                   (declare (ignore key))
                   (let* ((code (let ((*object-text* '()))
                                  (gen-dispatch-tree g (1+ pos) narrowed
                                                     (+ indent (if first 1 2)))
                                  (reverse *object-text*)))
                          (same (assoc code cases :test #'equal)))
                     (if same
                         (setf (cdr same) (append (cdr same) ids))
                         (push (cons code ids) cases)))))
               (emit-object "~aswitch (dy_wrapper_of(~a)->id) {" pad2 arg)
               (dolist (c (reverse (remove-if (lambda (c)
                                                ;; The default does that.
                                                (and (= (length (car c)) 1)
                                                     (search "goto no_method;" (first (car c)))))
                                              cases)))
                 (emit-object "~a~{~a~^ ~}" pad2
                              (mapcar (lambda (r)
                                        (if (= (car r) (cdr r))
                                            (format nil "case ~d:" (car r))
                                            (format nil "case ~d ... ~d:" (car r) (cdr r))))
                                      (id-ranges (cdr c))))
                 (dolist (line (car c)) (emit-object "~a" line)))
               (emit-object "~adefault: goto no_method;" pad2)
               (emit-object "~a}" pad2)))
           (unless first (emit-object "~a}" pad))))))))

(defun leaf-keyword-check (g methods)
  "C code checking the keyword arguments for a call whose applicable
methods are METHODS, or NIL."
  (when (gfun-variable-p g)
    (let* ((r (gfun-required g))
           (any (some #'gmethod-rest-p methods))
           (declared (gfun-declared-keys g))
           (keyed (or (some #'gmethod-keys methods)
                      (and declared (not (eq declared :none))))))
      (cond ((not keyed) nil)
            (any (format nil "dy_check_keywords(self, argc, ~d, a0, a1, a2, a3, NULL, 0);" r))
            (t (let ((keys (remove-duplicates
                            (append (loop for m in methods
                                          for k = (gmethod-keys m)
                                          when (consp k) append k)
                                    (and (consp declared) declared)))))
                 (format nil "dy_check_keywords(self, argc, ~d, a0, a1, a2, a3, ~a, ~d);"
                         r (keyword-array keys) (length keys))))))))

(defun emit-dispatch (g)
  (let* ((*leaves* (make-hash-table :test #'equal))
         (r (gfun-required g))
         (name (format nil "~a_dispatch" (gfun-c-name g)))
         ;; The tree first, apart: making the next methods emits functions.
         (tree (let ((*object-text* '()))
                 (gen-dispatch-tree g 0 (mapcar (lambda (m) (list m)) (gfun-methods g)) 1)
                 (reverse *object-text*)))
         (leaves (loop for methods being the hash-keys of *leaves* using (hash-value label)
                       collect (list label methods (next-object (rest methods))
                                     (leaf-keyword-check g methods)))))
    (declare-object "static D ~a(D, D, D, D, D, intptr_t);" name)
    (emit-object "static D ~a(D self, D a0, D a1, D a2, D a3, intptr_t argc) {" name)
    (emit-object "    (void)a0; (void)a1; (void)a2; (void)a3;")
    (emit-object "    if (argc ~:[!=~;<~] ~d) dy_wrong_argument_count(self, argc);"
                 (gfun-variable-p g) r)
    (dotimes (i r)
      (emit-object "    D x~d = ~a; (void)x~d;" i (arg-c i) i))
    (dolist (line tree) (emit-object "~a" line))
    (loop for (label methods next check) in (sort leaves #'string< :key #'first)
          do (emit-object "~a:" label)
             (when check (emit-object "    ~a" check))
             (emit-object "    dy_thread->next_method = ~a;" next)
             (emit-object "    DY_TAIL_CALL_DIRECT(~a, ~a, a0, a1, a2, a3, argc);"
                          (gmethod-entry-c (first methods))
                          (address (gmethod-object-c (first methods)))))
    (emit-object "no_method:~%    dy_no_applicable_method(self);~%}")))

(defun emit-generic-object (g)
  (declare-object "DyGeneric ~a;" (gfun-c-name g))
  (emit-object "DyGeneric ~a = { &dy_wrapper_generic_function, ~a_dispatch, ~a, ~a, ~d };"
               (gfun-c-name g) (gfun-c-name g)
               (c-constant *cdata* (name-symbol (module-var-name (gfun-var g))))
               (address (c-vector *cdata* (mapcar (lambda (m) (address (gmethod-object-c m)))
                                                  (gfun-methods g))))
               (gfun-required g)))

;;; make

(defun emit-maker (g)
  (let* ((info (gclass-info g))
         (name (format nil "dy_make_~a" (class-info-c-name info)))
         (slots (effective-slots info))
         (init (gethash (dylan-var "initialize") *gfuns*))
         (init-methods (and init
                            (remove-if-not
                             (lambda (m)
                               (and (not (eq (gmethod-kind m) :default))
                                    (let ((s (first (gmethod-specs m))))
                                      (and (eq (spec-kind s) :class)
                                           (subclass-info-p info (spec-class s))))))
                             (gfun-methods init))))
         (any (some #'gmethod-rest-p init-methods))
         (keywords (remove-duplicates
                    (append (loop for sd in slots
                                  for spec = (sdef-spec sd)
                                  for k = (or (slot-spec-init-keyword spec)
                                              (slot-spec-required-init-keyword spec))
                                  when k collect k)
                            (loop for m in init-methods
                                  for k = (gmethod-keys m)
                                  when (consp k) append k)))))
    (setf (class-info-maker info) name)
    (emit-object "D ~a(D self, D a0, D a1, D a2, D a3, intptr_t argc) {" name)
    (emit-object "    (void)self;")
    (emit-object "    dy_check_keywords(self, argc, 1, a0, a1, a2, a3, ~:[~a, ~d~;NULL, 0~*~]);"
                 any (keyword-array keywords) (length keywords))
    (emit-object "    D keys = dy_collect_arguments(argc, 1, a0, a1, a2, a3), v;")
    (let ((readies (loop for c in (class-info-cpl info)
                         for cg = (class-gclass c)
                         when cg collect (gclass-ready-c cg))))
      (emit-object "    if (!(~{~a~^ && ~})) dy_error(\"make: the class's define-class has not run yet\", a0);"
                   readies))
    (emit-object "    D x = dy_make_instance(&~a);" (wrapper-c-name info))
    (emit-object "    DyInstance *object = DY_PTR(x);")
    (dolist (sd slots)
      (when (eq (sdef-allocation sd) :instance)
        (let* ((spec (sdef-spec sd))
               (keyword (or (slot-spec-init-keyword spec) (slot-spec-required-init-keyword spec)))
               (offset (aref (class-info-slot-offsets info)
                             (gethash (sdef-getter sd) *instance-slot-numbers*))))
          (if keyword
              (emit-object "    v = dy_find_keyword(keys, ~a);" (c-constant *cdata* keyword))
              (emit-object "    v = DY_UNSUPPLIED;"))
          (when (slot-spec-required-init-keyword spec)
            (emit-object "    if (v == DY_UNSUPPLIED) dy_error(\"make: the required init-keyword is missing\", ~a);"
                         (c-constant *cdata* keyword)))
          (cond ((slot-spec-init-value spec)
                 (emit-object "    if (v == DY_UNSUPPLIED) v = ~a;" (sdef-c sd "init")))
                ((slot-spec-init-function spec)
                 (emit-object "    if (v == DY_UNSUPPLIED) v = DY_CALL(~a, 0, 0, 0, 0, 0);"
                              (sdef-c sd "initfn"))))
          (emit-object "    if (v != DY_UNSUPPLIED) {")
          (when (slot-spec-type spec)
            (emit-object "        if (~a != DY_UNBOUND) dy_check_type(v, ~a);"
                         (sdef-c sd "type") (sdef-c sd "type")))
          (emit-object "        object->slots[~d] = v;~%    }" offset))))
    (when (and init init-methods)
      (emit-object "    dy_call_list(~a, x, keys);" (address (gfun-c-name init))))
    (emit-object "    dy_thread->mv_count = 1;~%    return x;~%}")))

;;; The pass

(defun prepare-object-system (ir)
  "Analyze IR's classes and generic functions; answer the C text they
need, which GENERATE-C places."
  (let ((definitions (collect-definitions ir)))
    (collect-let-values ir)
    (build-classes definitions)
    (build-slots definitions)
    (loop for g being the hash-values of *gclasses*
          do (setf (gethash (gclass-var g) *constant-globals*)
                   (address (class-c-name (gclass-info g))))
             (declare-object "static int ~a;" (gclass-ready-c g))
             (dolist (d (module-var-definitions (gclass-var g)))
               (unless (define-class-node-p d)
                 (let ((*location* (node-location d)))
                   (compile-error "~a is defined both as a class and otherwise"
                                  (name-string (module-var-name (gclass-var g))))))))
    (build-generics definitions)
    (method-objects)))

(defun finish-object-system ()
  "Emit what depends on the code of the methods: dispatch, method
objects, makers."
  (let ((*nexts* (make-hash-table :test #'equal)))
    (loop for g being the hash-values of *gfuns*
          do (dolist (m (gfun-methods g))
               (when (eq (gmethod-kind m) :ir) (emit-direct-entry m)))
             (emit-dispatch g))
    (emit-method-objects)
    (loop for g being the hash-values of *gfuns* do (emit-generic-object g))
    (loop for g being the hash-values of *gclasses* do (emit-maker g))))
