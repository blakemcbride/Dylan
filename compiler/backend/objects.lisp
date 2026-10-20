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
(defvar *global-definitions* nil
  "MODULE-VAR -> the atoms define gives it, for the variables define defines.")
(defvar *assigned-globals* nil "MODULE-VAR -> T, for those set! assigns.")
(defvar *gclasses* nil "MODULE-VAR -> GCLASS, for the program's classes.")
(defvar *gfuns* nil "MODULE-VAR -> GFUN.")
(defvar *method-of-fn* nil "IR-FN -> GMETHOD, for define-method fns.")
(defvar *constant-globals* nil "MODULE-VAR -> C expression of its value.")
(defvar *sdef-of-node* nil "definition node -> its SDEFs.")
(defvar *singleton-sdefs* '()
  "The slots define-slot gives single objects: SDEFs with no class.")
(defvar *object-text* nil "C text for the object system, newest first.")
(defvar *object-decls* nil "C declarations it needs early, newest first.")
(defvar *instance-slot-numbers* nil "getter MODULE-VAR -> number.")
(defvar *definition-order* 0)

(defvar *reflective-methods* t
  "Might the program get hold of a method of a generic function, by
reflection, and call it?  If not, methods need no direct entries.")

(defstruct gclass var info sdefs location ready-c)

(defstruct sdef
  class                                 ; CLASS-INFO, or NIL for a slot of
                                        ; one object (define-slot on a
                                        ; singleton)
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

(defun var-c-name (prefix var)
  "A C name for the module variable VAR, from its name and module, not the
order variables are met (CONTENT-NAME)."
  (let ((name (module-var-name var)))
    (format nil "~a~a~:[~;_setter~]_~a" prefix
            (c-identifier (dylan-string (if (consp name) (cadr name) name)))
            (consp name)
            (subseq (content-hash (module-var-string var)) 0 8))))

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
                 (ir-define-variable (push (ir-define-variable-value e)
                                           (gethash (ir-define-variable-var e) *global-definitions*)))
                 (ir-global-set (setf (gethash (ir-global-set-var e) *assigned-globals*) t))
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
             (ir-global-ref (let ((var (ir-global-ref-var value)))
                              (or (global-alias-value var) (list :global var))))
             ((or const ivar) (static-value value))
             (ir-call
              (let ((f (static-value (ir-call-function value)))
                    (args (ir-call-arguments value)))
                (and (eq (car f) :global)
                     (let ((object
                             (cond ((and (eq (second f) (dylan-var "singleton"))
                                         (= (length args) 1))
                                    (first args))
                                   ;; (make <singleton> object: x), the same
                                   ;; (chapter 11)
                                   ((and (eq (second f) (dylan-var "make"))
                                         (= (length args) 3)
                                         (equal (static-value (first args))
                                                (list :global (dylan-var "<singleton>")))
                                         (const-p (second args))
                                         (keywordp (const-value (second args)))
                                         (string-equal (dylan::dylan-symbol->string
                                                        (const-value (second args)))
                                                       "object"))
                                    (third args)))))
                       (let ((arg (and object (static-value object))))
                         (and arg (list :singleton arg))))))))))))

(defvar *chasing* '() "The module variables global-alias-value is in.")

(defun global-alias-value (var)
  "If the module variable VAR is defined once by define, never assigned,
to something known at compile time (a class, a generic function, a
singleton, a constant), that; else NIL.  (A specializer may be any
expression that evaluates to a class or singleton, chapter 6.)"
  (let ((definitions (and *global-definitions* (gethash var *global-definitions*))))
    (and (= (length definitions) 1)
         (not (gethash var *assigned-globals*))
         (not (module-var-thread-local-p var))
         (not (member var *chasing*))
         (let ((*chasing* (cons var *chasing*)))
           (static-value (first definitions))))))

(defun global-class (var)
  "The CLASS-INFO of the class the module variable VAR holds, or NIL."
  (let ((g (gethash var *gclasses*)))
    (cond (g (gclass-info g))
          ((module-var-predefined-p var)
           (let ((c (find (dylan::dylan-symbol->string (name-symbol (module-var-name var)))
                          *classes* :key #'class-info-name :test #'string-equal)))
             (and c (not (consp (module-var-name var)))
                  ;; The library may name the runtime's internal classes.
                  (or (class-info-exported-p c)
                      (eq (module-var-module var) (dylan-module)))
                  c))))))

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

(defun runtime-spec-p (spec)
  "Is SPEC the singleton of an object known only when the program runs:
the owner of a define-slot on a singleton?  Its value is that slot's SDEF,
its C the object, which the define-slot stores."
  (sdef-p (spec-value spec)))

(defun owner-c (sd) (format nil "dy_owner~d" (sdef-id sd)))

(defun owner-spec (sd)
  (make-spec :singleton nil sd (format nil "~a.object" (owner-c sd))))

(defun same-spec-p (a b)
  (and (eq (spec-kind a) (spec-kind b))
       (if (eq (spec-kind a) :class)
           (eq (spec-class a) (spec-class b))
           (eql (spec-value a) (spec-value b)))))

(defun spec-object-c (spec)
  "A C expression for SPEC as a Dylan object, for reflection."
  (cond
    ((eq (spec-kind spec) :class)
     (address (class-c-name (spec-class spec))))
    ((runtime-spec-p spec) (address (owner-c (spec-value spec))))
    (t
      (let ((name (cdata-name *cdata* "dy_singleton_")))
        (define-c *cdata* "static DySingleton ~a = { &dy_wrapper_singleton, ~a };"
                  name (spec-c spec))
        (address name)))))

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
                   ;; A sealed class "cannot be subclassed" (chapter 2).
                   (when (class-info-sealed-p s)
                     (compile-error "~a is sealed: it cannot be subclassed" (class-info-name s)))
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

(defun inherited-slot-definition (class getter)
  "The definition of the slot GETTER reads in a proper superclass of CLASS
that the program defines, or NIL."
  (dolist (c (class-info-cpl class) nil)
    (unless (eq c class)
      (let ((cg (class-gclass c)))
        (when cg
          (let ((sd (find getter (gclass-sdefs cg) :key #'sdef-getter)))
            (when sd (return sd))))))))

(defun check-slot-redefinition (sd old)
  "Chapter 6: if you redefine an inherited slot, you must respecify all the
slot features the superior definition specified."
  (let ((new (sdef-spec sd))
        (was (sdef-spec old))
        (name (name-string (module-var-name (sdef-getter sd))))
        (*location* (or (sdef-location sd) *location*)))
    (flet ((missing (what)
             (compile-error "slot ~a: redefining an inherited slot must respecify its ~a"
                            name what)))
      (when (and (slot-spec-type was) (not (slot-spec-type new)))
        (missing "type:"))
      (when (and (or (slot-spec-init-value was) (slot-spec-init-function was))
                 (not (or (slot-spec-init-value new) (slot-spec-init-function new))))
        (missing "init-value: or init-function:"))
      (when (and (or (slot-spec-init-keyword was) (slot-spec-required-init-keyword was))
                 (not (or (slot-spec-init-keyword new) (slot-spec-required-init-keyword new))))
        (missing "init-keyword:"))
      (when (and (not (eq (sdef-allocation old) :instance))
                 (eq (sdef-allocation sd) :instance))
        (missing "allocation:"))
      (when (and (sdef-setter old) (not (sdef-setter sd)))
        (missing "setter:")))))

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
         (let* ((v (static-value (ir-define-slot-owner d)))
                (class (and (eq (car v) :global) (global-class (second v)))))
           (if class
               (let ((g (find class (loop for g being the hash-values of *gclasses* collect g)
                              :key #'gclass-info)))
                 (unless g
                   (compile-error "define-slot: slots can be added only to classes the program defines"))
                 (let ((sd (make-slot-definition class (ir-define-slot-slot d) *location*)))
                   (setf (gethash d *sdef-of-node*) (list sd))
                   (setf (gclass-sdefs g) (append (gclass-sdefs g) (list sd)))))
               ;; Any other owner must be a singleton when the define-slot
               ;; runs: the slot is the one object's (chapter 6).
               (let* ((spec (ir-define-slot-slot d))
                      (name (name-string (module-var-name (slot-spec-getter spec)))))
                 (unless (member (slot-spec-allocation spec) '(:instance :constant :virtual))
                   (compile-error "slot ~a: the slot of a singleton must be instance, constant or virtual"
                                  name))
                 (when (or (slot-spec-init-keyword spec) (slot-spec-required-init-keyword spec))
                   (compile-error "slot ~a: the slot of a singleton takes no init-keyword" name))
                 (let ((sd (make-slot-definition nil spec *location*)))
                   (setf (gethash d *sdef-of-node*) (list sd))
                   (push sd *singleton-sdefs*)))))))))
  ;; Chapter 6: a slot redefined in a subclass must respecify what the
  ;; inherited definition specified.
  (loop for g being the hash-values of *gclasses*
        do (dolist (sd (gclass-sdefs g))
             ;; Except a virtual redeclaration, a filtered slot (the
             ;; manual's <displaced-view>): the inherited slot stays as it
             ;; was, storage, initialization and all.
             (unless (eq (sdef-allocation sd) :virtual)
               (let ((old (inherited-slot-definition (gclass-info g) (sdef-getter sd))))
                 (when old (check-slot-redefinition sd old))))))
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
    (when (null (sdef-class sd))
      (declare-object "static DySingleton ~a = { &dy_wrapper_singleton, DY_UNBOUND };"
                      (owner-c sd)))
    (case (sdef-allocation sd)
      ((:class :constant) (declare-object "static D ~a = DY_UNBOUND;" (sdef-c sd "cell")))
      (:instance (when (null (sdef-class sd))
                   (declare-object "static D ~a = DY_UNBOUND;" (sdef-c sd "cell"))))
      (:each-subclass
       (declare-object "static D ~a[~d] = { ~{~a~^, ~} };" (sdef-c sd "cells") (length *classes*)
                       (make-list (length *classes*) :initial-element "DY_UNBOUND"))))))

(defun gen-slot-initialization (sd)
  "Code, for the define-class form, storing the slot definition's values."
  (let ((spec (sdef-spec sd)))
    (when (slot-spec-type spec)
      (emit "~a = ~a;" (sdef-c sd "type") (atom-c (slot-spec-type spec))))
    (when (slot-spec-init-value spec)
      (let ((v (atom-c (slot-spec-init-value spec))))
        (emit "~a = ~a;" (sdef-c sd "init") v)
        (when (and (slot-spec-type spec)
                   (or (member (sdef-allocation sd) '(:class :each-subclass :constant))
                       (null (sdef-class sd))))
          (emit "dy_check_type(~a, ~a);" v (sdef-c sd "type")))
        (case (sdef-allocation sd)
          ((:class :constant) (emit "~a = ~a;" (sdef-c sd "cell") v))
          (:instance (when (null (sdef-class sd)) (emit "~a = ~a;" (sdef-c sd "cell") v)))
          (:each-subclass
           (dolist (c *classes*)
             (when (subclass-info-p c (sdef-class sd))
               (emit "~a[~d] = ~a;" (sdef-c sd "cells") (class-info-id c) v)))))))
    (when (slot-spec-init-function spec)
      (emit "~a = ~a;" (sdef-c sd "initfn") (atom-c (slot-spec-init-function spec)))
      (when (null (sdef-class sd))
        ;; The one object's slot, initialized now
        (emit "~a = DY_CALL(~a, 0, 0, 0, 0, 0);" (sdef-c sd "cell") (sdef-c sd "initfn"))
        (when (slot-spec-type spec)
          (emit "dy_check_type(~a, ~a);" (sdef-c sd "cell") (sdef-c sd "type")))))
    ;; The slot descriptor's copies (a singleton's slot has none)
    (when (and (slot-spec-type spec) (sdef-class sd))
      (emit "~a.type = ~a;" (sdef-descriptor-c sd) (sdef-c sd "type")))
    (when (and (slot-spec-init-value spec) (sdef-class sd))
      (emit "~a.init_value = ~a;" (sdef-descriptor-c sd) (sdef-c sd "init")))
    (when (and (slot-spec-init-function spec) (sdef-class sd))
      (emit "~a.init_function = ~a;" (sdef-descriptor-c sd) (sdef-c sd "initfn")))))

(defun sdef-descriptor-c (sd) (format nil "dy_sd~d" (sdef-id sd)))

(defun gen-class-definition (node)
  "Code for a define-class or define-slot form, run where it appears."
  (dolist (sd (gethash node *sdef-of-node*))
    (when (null (sdef-class sd))
      ;; The owner: its object is the one with the slot from now on.
      (let ((owner (atom-c (ir-define-slot-owner node))))
        (emit "if (!dy_kind_p(~a, DY_KIND_SINGLETON)) dy_error(\"define-slot: the owner must be a singleton, or a class known when compiling\", ~a);"
              owner owner)
        (emit "~a.object = ((DySingleton *)DY_PTR(~a))->object;" (owner-c sd) owner)))
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
  (ecase (if (sdef-class sd) (sdef-allocation sd) :one-object)
    (:one-object
     ;; Dispatch has found the object.
     (emit-object "    D *place = &~a;" (sdef-c sd "cell")))
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

(defun owner-or-class-spec (sd)
  (if (sdef-class sd) (make-spec :class (sdef-class sd)) (owner-spec sd)))

(defun gen-accessors (sd)
  "The getter and setter methods of the slot definition SD."
  (let ((methods '())
        (getter (format nil "dy_slot~d_get" (sdef-id sd)))
        (setter (format nil "dy_slot~d_set" (sdef-id sd))))
    (declare-object "static D ~a(D, D, D, D, D, intptr_t);" getter)
    (emit-object "static D ~a(D self, D a0, D a1, D a2, D a3, intptr_t argc) {" getter)
    (emit-object "    (void)a1; (void)a2; (void)a3;")
    (emit-object "    if (DY_UNLIKELY(argc != 1)) dy_wrong_argument_count(self, argc);")
    (emit-storage-ref sd "a0")
    (let ((spec (sdef-spec sd)))
      (when (and (eq (sdef-allocation sd) :instance) (sdef-class sd)
                 (or (slot-spec-init-value spec) (slot-spec-init-function spec)))
        ;; An instance made before define-slot added the slot takes its
        ;; initial value now (chapter 6).
        (emit-object "    if (*place == DY_UNBOUND && ~a != DY_UNBOUND)"
                     (sdef-c sd (if (slot-spec-init-value spec) "init" "initfn")))
        (emit-object "        *place = ~a;"
                     (if (slot-spec-init-value spec)
                         (sdef-c sd "init")
                         (format nil "DY_CALL(~a, 0, 0, 0, 0, 0)" (sdef-c sd "initfn"))))))
    (emit-object "    if (DY_UNLIKELY(*place == DY_UNBOUND)) dy_unbound_slot(~a);" (slot-name-c sd))
    (emit-object "    return *place;~%}")
    (push (make-gmethod :specs (list (owner-or-class-spec sd))
                        :kind :getter :sdef sd :entry-c getter
                        :location (sdef-location sd) :order (sdef-order sd))
          methods)
    (when (sdef-setter sd)
      (declare-object "static D ~a(D, D, D, D, D, intptr_t);" setter)
      (emit-object "static D ~a(D self, D a0, D a1, D a2, D a3, intptr_t argc) {" setter)
      (emit-object "    (void)a2; (void)a3;")
      (emit-object "    if (DY_UNLIKELY(argc != 2)) dy_wrong_argument_count(self, argc);")
      (emit-storage-ref sd "a0")
      (when (slot-spec-type (sdef-spec sd))
        (emit-object "    if (~a != DY_UNBOUND) dy_check_type(a1, ~a);"
                     (sdef-c sd "type") (sdef-c sd "type")))
      (emit-object "    *place = a1;~%    return a1;~%}")
      (push (make-gmethod :specs (list (owner-or-class-spec sd) (object-spec))
                          :kind :setter :sdef sd :entry-c setter
                          :location (sdef-location sd) :order (sdef-order sd))
            methods))
    methods))

;;; --------------------------------------------------------- Generics

(defun gfun-for (var)
  (or (gethash var *gfuns*)
      (setf (gethash var *gfuns*)
            (make-gfun :var var :c-name (var-c-name "dy_gf_" var)))))

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
             (compile-error "define-method: the method refers to local variables (~a)"
                            (format nil "~{~a~^, ~}"
                                    (mapcar (lambda (v) (name-string (ivar-name v)))
                                            (ir-fn-free-vars fn)))))
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
  (loop for sds in (cons *singleton-sdefs*
                         (loop for g being the hash-values of *gclasses* collect (gclass-sdefs g)))
        do (dolist (sd sds)
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
  "Emit each method's object.  Its specializers and its arguments are for
the reflective functions, which can get at a method of a generic function
only when *REFLECTIVE-METHODS*; otherwise they are dropped.  Its owner, the
generic function, is how it prints (chapter 11's {a method on double})."
  (loop for g being the hash-values of *gfuns*
        do (dolist (m (gfun-methods g))
             (unless (eq (gmethod-kind m) :default)
               (emit-object "DyFunction ~a = { &dy_wrapper_method, ~a, ~a, ~a, ~a, NULL };"
                            (gmethod-object-c m)
                            (if (and (eq (gmethod-kind m) :ir) *reflective-methods*)
                                (format nil "~a_direct" (gmethod-entry-c m))
                                (gmethod-entry-c m))
                            (if *reflective-methods* (spec-vector-c (gmethod-specs m)) "DY_FALSE")
                            (if *reflective-methods* (method-info-c m) "DY_FALSE")
                            (address (gfun-c-name g)))))))

(defparameter *method-reflection-functions*
  '("generic-function-methods" "find-method" "sorted-applicable-methods"))

(defun uses-method-reflection-p (ir)
  "Does the program -- not the library, whose methods of these generic
functions call their primitives -- use them?"
  (let ((functions (mapcar #'dylan-var *method-reflection-functions*)))
    (some (lambda (top)
            (and (not (library-form-p top))
                 (some (lambda (v) (member v functions)) (referenced-variables top))))
          (ir-program-toplevels ir))))

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
        (emit-object "    if (DY_UNLIKELY(argc < ~d)) dy_wrong_argument_count(self, argc);" r)
        (emit-object "    if (DY_UNLIKELY(argc != ~d)) dy_wrong_argument_count(self, argc);" r))
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
                   (emit-object "static DyFunction ~a = { &dy_wrapper_method, ~a_entry, DY_FALSE, DY_FALSE, DY_FALSE, NULL };"
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
    (if (or (typep value '(or ratio float)) (runtime-spec-p spec))
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
         (labels ((narrow (class singleton)
                    ;; The candidates applicable to an argument of CLASS (that
                    ;; is SINGLETON's object, if it is not NIL), with their rank.
                    (loop for c in candidates
                          for s = (nth pos (gmethod-specs (car c)))
                          for rank = (if (eq (spec-kind s) :singleton)
                                         (and singleton (same-spec-p s singleton) -1)
                                         (spec-rank s class))
                          when rank collect (cons (car c) (cons rank (cdr c)))))
                  (class-switch (singleton indent)
                    ;; A switch on the argument's class (that is SINGLETON's
                    ;; object, if it is not NIL)
                    (let ((pad (make-string (* 4 indent) :initial-element #\Space))
                          (groups '())    ; (narrowed . ids)
                          (cases '()))    ; (code . ids): groups whose code is
                                          ; the same share their case
                      (dolist (class *classes*)
                        (let* ((narrowed (narrow class singleton))
                               (key (mapcar (lambda (c) (cons (car c) (cadr c))) narrowed))
                               (group (assoc key groups :test #'equal)))
                          (if group
                              (push (class-info-id class) (cddr group))
                              (push (list* key narrowed (list (class-info-id class))) groups))))
                      (dolist (group (reverse groups))
                        (destructuring-bind (key narrowed &rest ids) group
                          (declare (ignore key))
                          (let* ((code (let ((*object-text* '()))
                                         (gen-dispatch-tree g (1+ pos) narrowed (1+ indent))
                                         (reverse *object-text*)))
                                 (same (assoc code cases :test #'equal)))
                            (if same
                                (setf (cdr same) (append (cdr same) ids))
                                (push (cons code ids) cases)))))
                      (emit-object "~aswitch (dy_wrapper_of(~a)->id) {" pad arg)
                      (dolist (c (reverse (remove-if (lambda (c)
                                                       ;; The default does that.
                                                       (and (= (length (car c)) 1)
                                                            (search "goto no_method;" (first (car c)))))
                                                     cases)))
                        (emit-object "~a~{~a~^ ~}" pad
                                     (mapcar (lambda (r)
                                               ;; Standard C has no case ranges
                                               (format nil "~{case ~d:~^ ~}"
                                                       (loop for id from (car r) to (cdr r)
                                                             collect id)))
                                             (id-ranges (cdr c))))
                        (dolist (line (car c)) (emit-object "~a" line)))
                      (emit-object "~adefault: goto no_method;" pad)
                      (emit-object "~a}" pad))))
           (dolist (s singletons)
             (emit-object "~a~:[} else ~;~]if (~a) {" pad first (singleton-test-c s arg))
             (setf first nil)
             (if (spec-class s)
                 (gen-dispatch-tree g (1+ pos) (narrow (spec-class s) s) (1+ indent))
                 ;; An object known only when running: its class is not.
                 (class-switch s (1+ indent))))
           (if first
               (class-switch nil indent)
               (progn (emit-object "~a} else {" pad)
                      (class-switch nil (1+ indent))
                      (emit-object "~a}" pad)))))))))

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
    (emit-object "    if (DY_UNLIKELY(argc ~:[!=~;<~] ~d)) dy_wrong_argument_count(self, argc);"
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

(defun function-info-c (required rest keys)
  "The C constant for function-arguments: #(required rest? keys), KEYS
NIL (no #key), :ANY (#key alone) or a list of keywords."
  (c-constant *cdata* (vector required (bool rest)
                              (cond ((null keys) +false+) ((eq keys :any) '()) (t keys)))))

(defun method-info-c (m)
  (ecase (gmethod-kind m)
    (:ir (let ((fn (gmethod-fn m)))
           (function-info-c (length (ir-fn-required fn)) (ir-fn-rest fn)
                            (let ((k (ir-fn-keys fn))) (if (listp k) (mapcar #'ir-key-keyword k) k)))))
    (:getter (function-info-c 1 nil nil))
    (:setter (function-info-c 2 nil nil))))

(defun emit-generic-object (g)
  (declare-object "DyGeneric ~a;" (gfun-c-name g))
  (emit-object "DyGeneric ~a = { &dy_wrapper_generic_function, ~a_dispatch, ~a, ~a, ~d, ~a };"
               (gfun-c-name g) (gfun-c-name g)
               ;; Its name: a symbol, spelled (setter x) for a setter
               (let ((name (module-var-name (gfun-var g))))
                 (if (consp name)
                     (address (c-symbol *cdata* (name-string name) :symbol))
                     (c-constant *cdata* name)))
               (address (c-vector *cdata* (mapcar (lambda (m) (address (gmethod-object-c m)))
                                                  (gfun-methods g))))
               (gfun-required g)
               (let ((declared (gfun-declared-keys g)))
                 (function-info-c (gfun-required g)
                                  (and (gfun-variable-p g)
                                       (or (not declared) (eq declared :none)
                                           (some #'gmethod-rest-p (gfun-methods g))))
                                  (if (and declared (not (eq declared :none))) declared nil)))))

;;; make

(defun initialize-methods (info)
  "The program's methods of initialize for instances of the class INFO."
  (let ((init (gethash (dylan-var "initialize") *gfuns*)))
    (and init
         (remove-if-not
          (lambda (m)
            (and (not (eq (gmethod-kind m) :default))
                 (let ((s (first (gmethod-specs m))))
                   (and (eq (spec-kind s) :class)
                        (subclass-info-p info (spec-class s))))))
          (gfun-methods init)))))

(defun emit-maker (g)
  "The class's maker, which make calls, and its filler, which initializes
an instance already allocated -- on the heap by the maker, or on the stack
by code that the compiler has found does not let it escape."
  (let* ((info (gclass-info g))
         (name (format nil "dy_maker_~a" (class-info-c-name info)))
         (filler (format nil "dy_fill_~a" (class-info-c-name info)))
         (slots (effective-slots info))
         (init (gethash (dylan-var "initialize") *gfuns*))
         (init-methods (initialize-methods info))
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
    (emit-object "    return ~a(dy_make_instance(&~a), self, a0, a1, a2, a3, argc);~%}"
                 filler (wrapper-c-name info))
    (emit-object "D ~a(D x, D self, D a0, D a1, D a2, D a3, intptr_t argc) {" filler)
    (emit-object "    (void)self;")
    (emit-object "    dy_check_keywords(self, argc, 1, a0, a1, a2, a3, ~:[~a, ~d~;NULL, 0~*~]);"
                 any (keyword-array keywords) (length keywords))
    (emit-object "    D keys = dy_collect_arguments(argc, 1, a0, a1, a2, a3), v;")
    (let ((readies (loop for c in (class-info-cpl info)
                         for cg = (class-gclass c)
                         when cg collect (gclass-ready-c cg))))
      (emit-object "    if (!(~{~a~^ && ~})) dy_error(\"make: the class's define-class has not run yet\", a0);"
                   readies))
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
                 ;; (Not yet if its define-slot has not been done.)
                 (emit-object "    if (v == DY_UNSUPPLIED && ~a != DY_UNBOUND) v = DY_CALL(~a, 0, 0, 0, 0, 0);"
                              (sdef-c sd "initfn") (sdef-c sd "initfn"))))
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
               (when (and (eq (gmethod-kind m) :ir) *reflective-methods*)
                 (emit-direct-entry m)))
             (emit-dispatch g))
    (emit-method-objects)
    (loop for g being the hash-values of *gfuns* do (emit-generic-object g))
    (emit-slot-initialized-p)
    (emit-slot-descriptors)
    (loop for g being the hash-values of *gclasses* do (emit-maker g))))

;;; slot-initialized? (chapter 6): whether the slot that GETTER reads in
;;; INSTANCE has a value.  Written into each program, from its slots; true
;;; for a virtual slot, as the manual says the default is.

(defun emit-slot-initialized-p ()
  (emit-object "static D dy_slot_initialized_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {")
  (emit-object "    (void)a2; (void)a3;")
  (emit-object "    if (DY_UNLIKELY(argc != 2)) dy_wrong_argument_count(self, argc);")
  (emit-object "    DyWrapper *w = dy_wrapper_of(a0); (void)w;")
  ;; The slots of single objects, which are more specific
  (dolist (sd *singleton-sdefs*)
    (let ((getter (gethash (sdef-getter sd) *gfuns*)))
      (when getter
        (emit-object "    if (a1 == ~a && dy_identical(a0, ~a.object)) return ~a;"
                     (address (gfun-c-name getter)) (owner-c sd)
                     (if (eq (sdef-allocation sd) :virtual)
                         "DY_TRUE"
                         (format nil "DY_BOOL(~a != DY_UNBOUND)" (sdef-c sd "cell")))))))
  (loop for g being the hash-values of *gclasses*
        do (dolist (sd (gclass-sdefs g))
             (let ((getter (gethash (sdef-getter sd) *gfuns*)))
               (when getter
                 (emit-object "    if (a1 == ~a && dy_wrapper_subtype(w, &~a)) {"
                              (address (gfun-c-name getter)) (wrapper-c-name (sdef-class sd)))
                 (ecase (sdef-allocation sd)
                   (:instance
                    (emit-object "        int32_t i = w->slot_offsets ? w->slot_offsets[~d] : -1;"
                                 (gethash (sdef-getter sd) *instance-slot-numbers*))
                    (emit-object "        if (i >= 0) return DY_BOOL(((DyInstance *)DY_PTR(a0))->slots[i] != DY_UNBOUND);"))
                   ((:class :constant)
                    (emit-object "        return DY_BOOL(~a != DY_UNBOUND);" (sdef-c sd "cell")))
                   (:each-subclass
                    (emit-object "        return DY_BOOL(~a[w->id] != DY_UNBOUND);" (sdef-c sd "cells")))
                   (:virtual (emit-object "        return DY_TRUE;")))
                 (emit-object "    }")))))
  (emit-object "    dy_error(\"slot-initialized?: the getter does not read a slot of the object\", a0);~%}")
  (declare-object "DyFunction dy_prim_slot_initialized_p;")
  (emit-object "DyFunction dy_prim_slot_initialized_p = { &dy_wrapper_method, dy_slot_initialized_entry, DY_FALSE, DY_FALSE, DY_FALSE, NULL };"))

;;; Slot descriptors (chapter 11): one static DySlotDescriptor for each slot
;;; definition, and the functions slot-descriptor and slot-descriptors,
;;; written into each program from its slots.

(defun emit-slot-descriptors ()
  (let ((sdefs (loop for g being the hash-values of *gclasses* append (gclass-sdefs g))))
    (dolist (sd sdefs)
      (let ((spec (sdef-spec sd))
            (setter (and (sdef-setter sd) (gethash (sdef-setter sd) *gfuns*))))
        (declare-object "DySlotDescriptor ~a;" (sdef-descriptor-c sd))
        (emit-object "DySlotDescriptor ~a = { &dy_wrapper_slot_descriptor, ~a, ~a, DY_UNBOUND, ~a, DY_UNBOUND, DY_UNBOUND, ~a, ~a, ~d, ~d, ~a };"
                     (sdef-descriptor-c sd)
                     (address (gfun-c-name (gethash (sdef-getter sd) *gfuns*)))
                     (if setter (address (gfun-c-name setter)) "DY_FALSE")
                     (c-constant *cdata* (dylan-name (string-downcase (sdef-allocation sd))))
                     (c-constant *cdata* (or (slot-spec-init-keyword spec)
                                             (slot-spec-required-init-keyword spec)
                                             +false+))
                     (address (class-c-name (sdef-class sd)))
                     (position (sdef-allocation sd) '(:instance :class :each-subclass :constant :virtual))
                     (or (gethash (sdef-getter sd) *instance-slot-numbers*) 0)
                     (case (sdef-allocation sd)
                       ((:class :constant) (format nil "&~a" (sdef-c sd "cell")))
                       (:each-subclass (sdef-c sd "cells"))
                       (t "NULL")))))
    ;; (slot-descriptor instance getter): the most specific definition first
    (emit-object "static D dy_slot_descriptor_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {")
    (emit-object "    (void)a2; (void)a3;")
    (emit-object "    if (DY_UNLIKELY(argc != 2)) dy_wrong_argument_count(self, argc);")
    (emit-object "    DyWrapper *w = dy_wrapper_of(a0); (void)w;")
    (dolist (sd (stable-sort (copy-list sdefs) #'>
                             :key (lambda (sd) (length (class-info-cpl (sdef-class sd))))))
      (emit-object "    if (a1 == ~a && dy_wrapper_subtype(w, &~a)) return ~a;"
                   (address (gfun-c-name (gethash (sdef-getter sd) *gfuns*)))
                   (wrapper-c-name (sdef-class sd)) (address (sdef-descriptor-c sd))))
    (emit-object "    return DY_FALSE;~%}")
    (declare-object "DyFunction dy_prim_slot_descriptor;")
    (emit-object "DyFunction dy_prim_slot_descriptor = { &dy_wrapper_method, dy_slot_descriptor_entry, DY_FALSE, DY_FALSE, DY_FALSE, NULL };")
    ;; (slot-descriptors class): of each slot of its instances, the most
    ;; specific definition
    (emit-object "static D dy_slot_descriptors_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {")
    (emit-object "    (void)a1; (void)a2; (void)a3;")
    (emit-object "    if (DY_UNLIKELY(argc != 1)) dy_wrong_argument_count(self, argc);")
    (emit-object "    if (dy_kind_p(a0, DY_KIND_CLASS) && ((DyClass *)DY_PTR(a0))->sealed) return ~a;"
                 (address (c-vector *cdata* '())))
    (loop for g being the hash-values of *gclasses*
          do (let ((effective '()))
               (dolist (c (reverse (class-info-cpl (gclass-info g))))
                 (let ((cg (class-gclass c)))
                   (when cg
                     (dolist (sd (gclass-sdefs cg))
                       (let ((old (find (sdef-getter sd) effective :key #'sdef-getter)))
                         (if old
                             (setf effective (substitute sd old effective))
                             (setf effective (append effective (list sd)))))))))
               (emit-object "    if (a0 == ~a) return ~a;"
                            (address (class-c-name (gclass-info g)))
                            (address (c-vector *cdata* (mapcar (lambda (sd) (address (sdef-descriptor-c sd)))
                                                               effective))))))
    (emit-object "    return ~a;~%}" (address (c-vector *cdata* '())))
    (declare-object "DyFunction dy_prim_slot_descriptors;")
    (emit-object "DyFunction dy_prim_slot_descriptors = { &dy_wrapper_method, dy_slot_descriptors_entry, DY_FALSE, DY_FALSE, DY_FALSE, NULL };")))
