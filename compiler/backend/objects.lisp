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
(defvar *program-assigned-library-vars* nil
  "MODULE-VAR -> T, for the library's variables the program's own set!
assigns: the program's code keeps them in cells of its own (PROGRAM-CELL-
C-NAME, codegen.lisp), and the library's in its.")
(defvar *global-set-values* nil "MODULE-VAR -> the atoms its set!s store.")
(defvar *made-generic-dispatch* nil
  "Can a define-method add its method to a generic function made when the
program runs, which enters it by its object (dy_add_method_when_run)?")
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
  order
  (node nil)                            ; for a define-slot on a class, its
                                        ; node: the slot is the class's only
                                        ; once that has run
  (once nil)                            ; that node surely runs, once
  (form-end nil)                        ; else the top-level IR-FN whose
                                        ; own code it is in, if any: it
                                        ; can run no more after that
  (definer nil))                        ; its define-class or define-slot
                                        ; node

(defstruct gfun var c-name required variable-p methods declared-keys
  (declared-rest nil)                   ; a define-generic-function's #rest
  (object-c nil)
  (defined-c nil)                       ; C name of the flag set once a
                                        ; definition of it has run, for a
                                        ; program's generic function
  (incongruent '())                     ; slot accessor methods not
                                        ; congruent with it, never added
  (declared-c nil))                     ; C name of the flag set once its
                                        ; define-generic-function has run,
                                        ; for one whose variable a set! or
                                        ; define assigns: read-only from then

(defvar *own-class-refs* nil
  "IVAR -> T for a variable bound to a class's value only for a slot
option of its own define-class, where the class is already its value.")

(defvar *toplevel-order* nil
  "Top-level IR-FN -> its place in the program, and module variable -> the
place of the top-level form that defines it (inline.lisp, TOPLEVEL-ORDER).")

(defvar *generic-declarations* nil
  "MODULE-VAR -> its define-generic-functions and the methods of its
define-methods, newest first.")
(defvar *inconsistent-declarations* nil
  "The later define-generic-functions not consistent with the first, an
error when they run (chapter 6).")
(defvar *incongruent* nil
  "A define-method's IR-FN, or an IR-DEFINE-GENERIC, -> the error, as
(before-name . after-name), that the definition signals when it runs: the
method is not congruent with its generic function (chapter 6).")

(defvar *definition-fns* nil
  "Definition node -> the IR-FN whose code it is in.")

(defstruct gmethod
  specs                                 ; SPECs, one per required parameter
  kind                                  ; :ir :getter :setter :default
  fn sdef
  rest-p
  keys                                  ; NIL (no #key), :ANY, or keywords
  object-c                              ; C name of its <method> object
  entry-c                               ; C function dispatch calls
  location
  order
  var                                   ; the generic function's MODULE-VAR
  (flag-c nil)                          ; C name of the flag set while it is
                                        ; attached: a program's method
                                        ; (chapter 6: when its define-method
                                        ; runs); NIL if always attached
  (toplevel nil)                        ; for a program's method, the index
                                        ; of the top-level form whose own
                                        ; code defines it, or NIL
  (siblings '())                        ; the other methods with its
                                        ; specializers, which replace it or
                                        ; it replaces
  (closure-c nil)                       ; for a method that refers to local
                                        ; variables, the C name of the
                                        ; variable holding the closure its
                                        ; define-method made last
  (original nil)                        ; for a copy, the method copied
  (copies '())                          ; its copies in the other generic
                                        ; functions its variable may hold
  (foreign-errors '())                  ; (GFUN . error) for those it is not
                                        ; congruent with
  (made nil))                           ; its variable may hold a generic
                                        ; function made when the program runs

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
  (c-utf8-literal string))

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
                 (ir-global-set (setf (gethash (ir-global-set-var e) *assigned-globals*) t)
                                (when (and (library-var-p (ir-global-set-var e))
                                           (not (ir-fn-library-p fn)))
                                  (setf (gethash (ir-global-set-var e)
                                                 *program-assigned-library-vars*)
                                        t))
                                (push (ir-global-set-value e)
                                      (gethash (ir-global-set-var e) *global-set-values*)))
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
        ;; A constant is never a class
        (and (eq (car v) :constant)
             (compile-error "not a class: ~a" (dylan-form-string (second v))))
        (compile-error "~a must be a class known at compile time" what))))

(defun constant-class (value)
  "The class of the Dylan constant VALUE."
  (find-class-info
   (cond ((class-info-p value) "<class>")
         ((integerp value) "<integer>")
         ((typep value 'ratio) "<ratio>")
         ((typep value 'single-float) "<single-float>")
         ((floatp value) "<double-float>")
         ((dylan::extended-float? value) "<extended-float>")
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
           ;; ID?: an extended float is the same object as another
           ;; of its value
           (dylan::id? (spec-value a) (spec-value b)))))

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
  "The definition nodes of IR, in program order.  Each is noted in
*DEFINITION-FNS* with the fn whose code it is in."
  (let ((found '()))
    (dolist (fn (ir-fns ir))
      (labels ((walk (e)
                 (typecase e
                   ((or ir-define-class ir-define-slot ir-define-method ir-define-generic)
                    (setf (gethash e *definition-fns*) fn)
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

(defun note-own-class-refs (definitions)
  "Note in *OWN-CLASS-REFS* the variables bound to a class for an option of
its own define-class's slots: as the slots are made the class is the
variable's value (chapter 6's slot types and initial values may name it)."
  (dolist (d definitions)
    (when (ir-define-class-p d)
      (dolist (slot (ir-define-class-slots d))
        (dolist (a (list (slot-spec-type slot) (slot-spec-init-value slot)))
          (let ((v (and (ivar-p a) (gethash a *let-values*))))
            (when (and (ir-global-ref-p v)
                       (eq (ir-global-ref-var v) (ir-define-class-var d)))
              (setf (gethash a *own-class-refs*) t))))))))

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

;;; Slots inherited along two paths (chapter 6, "Redefining Slots"): the
;;; definitions must be exactly the same, the options' values id?, or the
;;; class must define the slot itself.

(defun slot-definition-in (class getter)
  "The definition of the slot GETTER reads in CLASS: its own, or the one
it inherits; NIL if none the program defines."
  (dolist (c (class-info-cpl class) nil)
    (let* ((cg (class-gclass c))
           (sd (and cg (find getter (gclass-sdefs cg) :key #'sdef-getter))))
      (when sd (return sd)))))

(defun inherited-slot-origins (class getter)
  "The definitions of the slot GETTER that CLASS inherits through its
direct superclasses, but for any that another of them overrides."
  (let ((sds '()))
    (dolist (super (class-info-supers class))
      (let ((sd (slot-definition-in super getter)))
        (when sd (pushnew sd sds))))
    (remove-if (lambda (sd)
                 (some (lambda (other)
                         (and (not (eq other sd)) (sdef-class other) (sdef-class sd)
                              (subclass-info-p (sdef-class other) (sdef-class sd))))
                       sds))
               (nreverse sds))))

(defun slot-option-comparison (sd1 sd2 reader what)
  "Whether the option of SD1 and SD2 that READER reads from their specs
(WHAT names its storage) is the same: :SAME, :DIFFER, or a list of the
two C expressions to compare with id? when the program runs."
  (flet ((known (sd)
           (let ((atom (funcall reader (sdef-spec sd))))
             (cond (atom (static-value atom))
                   ;; No type: is <object> (chapter 6)
                   ((eq reader #'slot-spec-type)
                    (list :global (dylan-var "<object>")))
                   (t (list :none))))))
    (let ((k1 (known sd1)) (k2 (known sd2)))
      (cond ((and k1 k2)
             (cond ((and (eq (car k1) :constant) (eq (car k2) :constant))
                    (if (dylan::id? (second k1) (second k2)) :same :differ))
                   ((equal k1 k2) :same)
                   ((or (eq (car k1) :none) (eq (car k2) :none)) :differ)
                   ((and (eq (car k1) :global) (eq (car k2) :global)
                         (global-class (second k1)) (global-class (second k2)))
                    (if (eq (global-class (second k1)) (global-class (second k2)))
                        :same
                        :differ))
                   ((and (funcall reader (sdef-spec sd1)) (funcall reader (sdef-spec sd2)))
                    (list (sdef-c sd1 what) (sdef-c sd2 what)))
                   (t :differ)))
            ((and (funcall reader (sdef-spec sd1)) (funcall reader (sdef-spec sd2)))
             (list (sdef-c sd1 what) (sdef-c sd2 what)))
            (t :differ)))))

(defun slot-clash-message (g getter sd1 sd2)
  (format nil "~a inherits two different definitions of the slot ~a, from ~a and ~a"
          (class-info-name (gclass-info g))
          (name-string (module-var-name getter))
          (class-info-name (sdef-class sd1)) (class-info-name (sdef-class sd2))))

(defun slot-clash-checks (g)
  "For the program's class G: signal a compile error if it inherits a slot
whose definitions differ; else answer the checks, (MESSAGE C1 C2) each, of
options whose values are compared when the class is defined."
  (let* ((class (gclass-info g))
         (own (mapcar #'sdef-getter (gclass-sdefs g)))
         (getters '())
         (checks '()))
    (dolist (c (rest (class-info-cpl class)))
      (let ((cg (class-gclass c)))
        (when cg
          (dolist (sd (gclass-sdefs cg))
            (unless (member (sdef-getter sd) own)
              (pushnew (sdef-getter sd) getters))))))
    (dolist (getter (nreverse getters) (nreverse checks))
      (let ((origins (inherited-slot-origins class getter)))
        (dolist (other (rest origins))
          (let* ((first (first origins))
                 (message (slot-clash-message g getter first other))
                 (comparisons
                   (list (if (and (eq (sdef-allocation first) (sdef-allocation other))
                                  (eq (sdef-setter first) (sdef-setter other))
                                  (eq (slot-spec-init-keyword (sdef-spec first))
                                      (slot-spec-init-keyword (sdef-spec other)))
                                  (eq (slot-spec-required-init-keyword (sdef-spec first))
                                      (slot-spec-required-init-keyword (sdef-spec other))))
                             :same
                             :differ)
                         (slot-option-comparison first other #'slot-spec-type "type")
                         (slot-option-comparison first other #'slot-spec-init-value "init")
                         (slot-option-comparison first other #'slot-spec-init-function
                                                 "initfn"))))
            (if (member :differ comparisons)
                (let ((*location* (or (gclass-location g) *location*)))
                  (compile-error "~a" message))
                (dolist (c comparisons)
                  (when (consp c) (push (cons message c) checks))))))))))

(defun inherited-slot-definitions (class getter)
  "Every definition of the slot GETTER reads in a proper superclass of
CLASS that the program defines: a define-slot's as well as its
define-class's, since either may be the one in force when CLASS's runs."
  (loop for c in (class-info-cpl class)
        for cg = (and (not (eq c class)) (class-gclass c))
        when cg
          append (remove getter (gclass-sdefs cg) :key #'sdef-getter :test-not #'eq)))

(defun slot-type-class (sd)
  "The class SD's type: names, if a class known at compile time, or NIL."
  (let* ((type (slot-spec-type (sdef-spec sd)))
         (v (and type (static-value type))))
    (and (eq (car v) :global) (global-class (second v)))))

(defun check-slot-type-redefinition (sd)
  "Chapter 6: the type of a redefined slot must be within the
intersection of the types of all the slots it overrides."
  (let ((new (slot-type-class sd)))
    (when new
      (dolist (old (inherited-slot-definitions (sdef-class sd) (sdef-getter sd)))
        (let ((was (slot-type-class old)))
          (when (and was (not (subclass-info-p new was)))
            (let ((*location* (or (sdef-location sd) *location*)))
              (compile-error "slot ~a: the new slot type ~a is not a subclass of the inherited type ~a"
                             (name-string (module-var-name (sdef-getter sd)))
                             (class-info-name new) (class-info-name was)))))))))

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
                           (let ((sd (make-slot-definition (gclass-info g) spec
                                                           (or (slot-spec-location spec) *location*))))
                             (setf (sdef-definer sd) d)
                             sd))
                         (ir-define-class-slots d)))
           (setf (gclass-sdefs g) (gethash d *sdef-of-node*)))))
      (ir-define-slot
       (at-node (d)
         (let* ((v (static-value (ir-define-slot-owner d)))
                (class (and (eq (car v) :global) (global-class (second v)))))
           (if class
               (let ((g (find class (loop for g being the hash-values of *gclasses* collect g)
                              :key #'gclass-info)))
                 (unless (and g (program-gclass-p g))
                   (compile-error "define-slot: slots can be added only to classes the program defines"))
                 (let ((sd (make-slot-definition class (ir-define-slot-slot d) *location*))
                       (in (gethash d *definition-fns*)))
                   (setf (sdef-node sd) d
                         (sdef-definer sd) d
                         ;; Surely run once, where it is (TOPLEVEL-ORDER),
                         ;; or at most once, by the end of its top-level
                         ;; form; or, in a method or a loop, any number of
                         ;; times, any time.
                         (sdef-once sd) (and (gethash d *toplevel-order*) t)
                         (sdef-form-end sd) (and (not (gethash d *toplevel-order*))
                                                 in (eq (ir-fn-kind in) :toplevel)
                                                 in))
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
                   (setf (sdef-definer sd) d)
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
               (let ((olds (inherited-slot-definitions (gclass-info g) (sdef-getter sd))))
                 (when olds
                   (dolist (old olds)
                     (check-slot-redefinition sd old))
                   (check-slot-type-redefinition sd))))))
  ;; Chapter 6: a slot inherited from two superclasses with different
  ;; definitions must be defined in the class.  What differs when compiling
  ;; is an error now; what may differ is checked where the class is
  ;; defined (SLOT-CLASH-CHECKS).
  (loop for g being the hash-values of *gclasses*
        do (slot-clash-checks g))
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

(defun library-private-slot-p (sd)
  "Is SD a slot of the library's own, which no program can name: one whose
getter is the DYLAN module's and not exported?  slot-descriptors leaves
such slots out, as the interpreter does."
  (let ((getter (sdef-getter sd)))
    (and (eq (module-var-module getter) (dylan-module))
         (not (member (module-var-name getter) (module-exports (dylan-module))
                      :test #'equal)))))

(defun reported-slot-definitions (info &optional (memo (make-hash-table :test 'eq)))
  "The definitions slot-descriptors answers for the class INFO, in the
interpreter's order: the slots of each direct superclass in turn, each slot
where it first appears -- a more specific definition reached later taking
its place -- and then the class's own, a redefinition in the place of
the slot it redefines."
  (or (gethash info memo)
      (setf (gethash info memo)
            (let ((effective '()))
              (flet ((put (sd own)
                       (let ((old (find (sdef-getter sd) effective :key #'sdef-getter)))
                         (cond ((null old) (setf effective (append effective (list sd))))
                               ((or own (and (not (eq (sdef-class sd) (sdef-class old)))
                                             (subclass-info-p (sdef-class sd) (sdef-class old))))
                                (setf effective (substitute sd old effective)))))))
                (dolist (super (class-info-supers info))
                  (dolist (sd (reported-slot-definitions super memo))
                    (put sd nil)))
                (let ((g (class-gclass info)))
                  (when g
                    (dolist (sd (gclass-sdefs g))
                      (put sd t)))))
              effective))))

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

;;; Classes whose slots define-slot changes when it runs (chapter 6), as
;;; the interpreter changes them: each of the program's classes has its
;;; slot descriptors in a vector made when its define-class runs, which a
;;; define-slot on it or a superclass may change (DyProgramClass, dylan.h).

(defun program-gclass-p (g)
  "Is G a class the program defines, not the library?"
  (not (library-location-p (gclass-location g))))

(defun class-slots-c (info) (format nil "dy_slots_~a" (class-info-c-name info)))
(defun program-class-c (info) (format nil "dy_pc_~a" (class-info-c-name info)))
(defun early-c (info) (format nil "dy_early_~a" (class-info-c-name info)))
(defun pending-c (info) (format nil "dy_pending_~a" (class-info-c-name info)))

(defun class-define-slots ()
  "The slot definitions of the define-slots on classes."
  (loop for g being the hash-values of *gclasses*
        append (remove-if-not #'sdef-node (gclass-sdefs g))))

(defun define-slots-within (info)
  "The define-slots on classes whose slot instances of the class INFO may
gain: those on it or a superclass."
  (remove-if-not (lambda (sd) (subclass-info-p info (sdef-class sd)))
                 (class-define-slots)))

(defun slot-changing-class-p (info)
  "Must the instances of the program's class INFO be made by its slot
descriptors when make runs, a define-slot on it or a superclass having
changed them, or being yet to?"
  (let ((g (class-gclass info)))
    (and g (program-gclass-p g) (define-slots-within info) t)))

(defun base-storage-sdefs (info)
  "Of each instance slot of INFO, the definition its define-class or a
superclass's gives it: what a virtual slot keeps."
  (let ((result '()))
    (dolist (c (class-info-cpl info))
      (let ((g (class-gclass c)))
        (when g
          (dolist (sd (gclass-sdefs g))
            (when (and (null (sdef-node sd)) (eq (sdef-allocation sd) :instance)
                       (not (find (sdef-getter sd) result :key #'sdef-getter)))
              (push sd result))))))
    (nreverse result)))

(defun declare-program-classes ()
  (declare-object "static int dy_class_serial;")
  (loop for g being the hash-values of *gclasses*
        when (program-gclass-p g)
          do (let ((info (gclass-info g)))
               (declare-object "static D ~a = DY_UNBOUND;" (class-slots-c info))
               (when (slot-changing-class-p info)
                 (declare-object "static D ~a = DY_EMPTY;" (early-c info))
                 (declare-object "static int ~a = ~d;" (pending-c info)
                                 (count :instance (define-slots-within info)
                                        :key #'sdef-allocation))
                 (declare-object "static DyProgramClass ~a;" (program-class-c info))))))

(defun emit-program-class (info init-keywords any any-flags)
  "The DyProgramClass of INFO, a class SLOT-CHANGING-CLASS-P.
INIT-KEYWORDS are the keywords initialize's methods take, each as
(keyword . flag), the flag the C name of the one that says its method is
attached, or NIL if it always is; with ANY, or while one of the ANY-FLAGS
is set, a method takes any."
  (let* ((g (class-gclass info))
         (subclasses (loop for c in *classes*
                           when (and (member info (class-info-supers c))
                                     (slot-changing-class-p c))
                             collect c))
         (subs (and subclasses (cdata-name *cdata* "dy_subclasses_")))
         (keys (and init-keywords (not any) (keyword-array (mapcar #'car init-keywords))))
         (key-flags (and keys (some #'cdr init-keywords) (cdata-name *cdata* "dy_key_flags_")))
         (anys (and any-flags (not any) (cdata-name *cdata* "dy_any_flags_")))
         (storage (base-storage-sdefs info))
         (stores (and storage (cdata-name *cdata* "dy_storage_"))))
    (when subs
      (emit-object "static const DyProgramClass *const ~a[] = { ~{&~a~^, ~} };"
                   subs (mapcar #'program-class-c subclasses)))
    (when stores
      (emit-object "static const D ~a[] = { ~{~a~^, ~} };"
                   stores (mapcar (lambda (sd) (address (sdef-descriptor-c sd))) storage)))
    (when key-flags
      (emit-object "static const unsigned char *const ~a[] = { ~{~a~^, ~} };"
                   key-flags (mapcar (lambda (k) (if (cdr k) (format nil "&~a" (cdr k)) "NULL")) init-keywords)))
    (when anys
      (emit-object "static const unsigned char *const ~a[] = { ~{&~a~^, ~} };" anys any-flags))
    (emit-object "static DyProgramClass ~a = { ~a, &~a, &~a, &~a, &~a, ~a, ~d, ~a, ~d, ~:[false~;true~], ~a, ~d, ~a, ~a, ~d };"
                 (program-class-c info) (address (class-c-name info)) (gclass-ready-c g)
                 (class-slots-c info) (early-c info) (pending-c info)
                 (or subs "NULL") (length subclasses)
                 (or keys "NULL") (if keys (length init-keywords) 0) any
                 (or stores "NULL") (length storage)
                 (or key-flags "NULL") (or anys "NULL") (if anys (length any-flags) 0))))

;;; Static storage of a slot definition: its type, init-value and
;;; init-function as evaluated, and the storage of shared slots.

(defun sdef-c (sd what) (format nil "dy_slot~d_~a" (sdef-id sd) what))

(defun declare-slot-storage (sd)
  (let ((spec (sdef-spec sd)))
    (when (sdef-node sd)
      (declare-object "static int ~a;" (sdef-c sd "ran")))
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
    ;; The slot descriptor's copies
    (when (slot-spec-type spec)
      (emit "~a.type = ~a;" (sdef-descriptor-c sd) (sdef-c sd "type")))
    (when (slot-spec-init-value spec)
      (emit "~a.init_value = ~a;" (sdef-descriptor-c sd) (sdef-c sd "init")))
    (when (slot-spec-init-function spec)
      (emit "~a.init_function = ~a;" (sdef-descriptor-c sd) (sdef-c sd "initfn")))))

(defun sdef-descriptor-c (sd) (format nil "dy_sd~d" (sdef-id sd)))

(defun gen-class-definition (node &optional (part :all))
  "Code for a define-class or define-slot form, run where it appears: PART
:ALL of it; or :START, all but storing the slots' options, which :OPTIONS
then does, once they are computed (IR-DEFINE-CLASS-START)."
  (unless (eq part :start)
    (gen-slot-options node))
  (unless (eq part :options)
    (gen-class-definition-rest node)))

(defun gen-slot-options (node)
  (dolist (sd (gethash node *sdef-of-node*))
    (when (null (sdef-class sd))
      ;; The owner: its object is the one with the slot from now on.
      (let ((owner (atom-c (ir-define-slot-owner node))))
        (emit "if (!dy_kind_p(~a, DY_KIND_SINGLETON)) dy_error(\"define-slot: the owner must be a singleton, or a class known when compiling\", ~a);"
              owner owner)
        ;; One object owns the slot: run again, the define-slot must be
        ;; for the same one.
        (emit "if (~a.object != DY_UNBOUND && !dy_identical(~a.object, ((DySingleton *)DY_PTR(~a))->object)) dy_error(~a, ((DySingleton *)DY_PTR(~a))->object);"
              (owner-c sd) (owner-c sd) owner
              (c-utf8-literal
               (format nil "define-slot: a compiled program gives the slot ~a to one object only, not also to"
                       (name-string (module-var-name (sdef-getter sd)))))
              owner)
        (emit "~a.object = ((DySingleton *)DY_PTR(~a))->object;" (owner-c sd) owner)
        (emit "~a.owner = ~a.object;" (sdef-descriptor-c sd) (owner-c sd))))
    (gen-slot-initialization sd)))

(defun gen-class-definition-rest (node)
  (when (ir-define-class-p node)
    (let ((g (gethash (ir-define-class-var node) *gclasses*)))
      ;; A class seal has sealed since compiling cannot be subclassed
      ;; (chapter 11).
      (dolist (s (class-info-supers (gclass-info g)))
        (emit "if (~a.sealed) dy_error(~a, 0);" (class-c-name s)
              (c-utf8-literal (format nil "~a is sealed: it cannot be subclassed"
                                      (class-info-name s)))))
      (loop for (message c1 c2) in (slot-clash-checks g)
            do (emit "if (!dy_identical(~a, ~a)) dy_error(~a, 0);"
                     c1 c2 (c-utf8-literal message)))))
  ;; A program's slots' getters and setters are generic functions from
  ;; now on, with these methods (chapter 6).
  (unless (library-location-p (ir-node-location node))
    (dolist (sd (gethash node *sdef-of-node*))
      (dolist (var (list (sdef-getter sd) (sdef-setter sd)))
        (let ((g (and var (gethash var *gfuns*))))
          (when g
            (let ((methods (remove-if-not (lambda (m)
                                            (and (eq (gmethod-sdef m) sd) (added-when-run-p m)))
                                          (append (gfun-methods g) (gfun-incongruent g)))))
              (flet ((own ()
                       (dolist (line (generic-definition-c g)) (emit "~a" line))
                       (dolist (m methods)
                         (if (gethash m *incongruent*)
                             ;; Not congruent with the generic function (chapter 6)
                             (emit-incongruent (gethash m *incongruent*) var)
                             (dolist (line (method-attachment-c m)) (emit "~a" line))))))
                ;; Or to another generic function the variable holds
                (let ((foreign (find-if (lambda (m)
                                          (or (gmethod-copies m) (gmethod-foreign-errors m)
                                              (gmethod-made m)))
                                        methods)))
                  (if foreign
                      (emit-foreign-attachment foreign var #'own)
                      (own)))))))))
    ;; The setter of a slot this define-slot replaces with one that has no
    ;; setter method in its generic function is detached
    ;; (DETACHES-SETTER-P).
    (dolist (new (gethash node *sdef-of-node*))
      (let ((g (and (sdef-class new) (ir-define-slot-p node)
                    (class-gclass (sdef-class new)))))
        (dolist (old (and g (gclass-sdefs g)))
          (when (detaches-setter-p new old)
            (let ((gf (gethash (sdef-setter old) *gfuns*)))
              (dolist (m (and gf (gfun-methods gf)))
                (when (and (eq (gmethod-kind m) :setter) (eq (gmethod-sdef m) old)
                           (gmethod-flag-c m))
                  (emit "dy_lock_methods();")
                  (emit "~a = 0;" (gmethod-flag-c m))
                  (emit "dy_remove_methods(~a, (const D[]){ ~a }, 1);"
                        (address (gfun-c-name gf)) (method-object-c-expr m))
                  (emit "dy_unlock_methods();")))))))))
  (when (ir-define-class-p node)
    (let ((g (gethash (ir-define-class-var node) *gclasses*)))
      (if (program-gclass-p g)
          (progn
            (emit-class-slots g node)
            (emit "~a = ++dy_class_serial;" (gclass-ready-c g)))
          (emit "~a = 1;" (gclass-ready-c g)))))
  ;; A define-slot on a class: the class's slot from now on, and its
  ;; subclasses' that lack one with the getter; the instances made so far
  ;; are given its initial value (chapter 6).
  (dolist (sd (gethash node *sdef-of-node*))
    (when (sdef-node sd)
      (emit "~a = 1;" (sdef-c sd "ran"))
      (emit "dy_define_class_slot(&~a, ~a);" (program-class-c (sdef-class sd))
            (address (sdef-descriptor-c sd)))
      (when (sdef-once sd)
        (dolist (line (define-slot-done-c sd)) (emit "~a" line))))))

(defun define-slot-done-c (sd)
  "The C statements that tell each class whose instances the define-slot
of SD, an instance slot, may give a slot that it can run no more: they
need not be kept for it (dy_define_slot_done)."
  (and (eq (sdef-allocation sd) :instance)
       (loop for c in *classes*
             when (and (slot-changing-class-p c) (subclass-info-p c (sdef-class sd)))
               collect (format nil "dy_define_slot_done(&~a);" (program-class-c c)))))

(defun toplevel-end-c (top)
  "The C statements to run after the top-level form TOP, whose own code
has a define-slot that may not run (in an arm of an if, say): it can run
no more."
  (loop for sd in (class-define-slots)
        when (eq (sdef-form-end sd) top)
          append (define-slot-done-c sd)))

(defun emit-class-slots (g node)
  "Code giving the program's class G, whose define-class NODE runs, its
slot descriptors: its superclasses' as they are now, then its own."
  (let* ((info (gclass-info g))
         (supers (mapcar (lambda (super)
                           (let ((sg (class-gclass super)))
                             (if (and sg (program-gclass-p sg))
                                 (class-slots-c super)
                                 (address (c-vector *cdata*
                                                    (mapcar (lambda (sd) (address (sdef-descriptor-c sd)))
                                                            (reported-slot-definitions super)))))))
                         (class-info-supers info)))
         (own (mapcar (lambda (sd) (address (sdef-descriptor-c sd)))
                      (gethash node *sdef-of-node*))))
    (emit "{")
    (emit "    const D supers[] = { ~{~a~^, ~} };" (or supers '("DY_UNBOUND")))
    (if own
        (progn
          (emit "    const D own[] = { ~{~a~^, ~} };" own)
          (emit "    ~a = dy_combine_slots(supers, ~d, own, ~d);" (class-slots-c info)
                (length supers) (length own)))
        (emit "    ~a = dy_combine_slots(supers, ~d, NULL, 0);" (class-slots-c info)
              (length supers)))
    (emit "}")))

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

(defun consistent-declarations-p (a b)
  "Do the define-generic-functions A and B declare the same parameters?"
  (let ((ka (ir-define-generic-keys a)) (kb (ir-define-generic-keys b)))
    (and (= (ir-define-generic-required a) (ir-define-generic-required b))
         (eq (and (ir-define-generic-rest a) t) (and (ir-define-generic-rest b) t))
         (cond ((or (null ka) (null kb)) (and (null ka) (null kb)))
               ((or (atom ka) (atom kb)) (and (atom ka) (atom kb)))
               (t (and (subsetp ka kb) (subsetp kb ka)))))))

(defun build-generics (definitions)
  (dolist (d definitions)
    (at-node (d)
      (typecase d
        (ir-define-generic
         (let ((var (ir-define-generic-var d)))
           (gfun-for var)
           (push d (gethash var *generic-declarations*))))
        ;; A slot's getter and setter, given their methods when the
        ;; define-class or define-slot runs
        ((or ir-define-class ir-define-slot)
         (dolist (sd (gethash d *sdef-of-node*))
           (push (list :accessor sd :getter) (gethash (sdef-getter sd) *generic-declarations*))
           (when (sdef-setter sd)
             (push (list :accessor sd :setter) (gethash (sdef-setter sd) *generic-declarations*)))))
        (ir-define-method
         (let* ((fn (ir-define-method-fn d))
                (var (ir-define-method-var d)))
           (when (and (module-var-predefined-p var)
                      (predefined-c-value var *classes*))
             (compile-error "methods cannot be added to ~a (it is not yet a generic function in the runtime)"
                            (name-string (module-var-name var))))
           (let ((m (make-gmethod
                     :specs (mapcar #'resolve-specializer (ir-fn-specializers fn))
                     :kind :ir :fn fn :var var
                     :rest-p (and (ir-fn-rest fn) t)
                     :keys (let ((k (ir-fn-keys fn)))
                             (if (listp k) (mapcar #'ir-key-keyword k) k))
                     :location *location* :order (incf *definition-order*)
                     ;; The place of its top-level form if it surely
                     ;; runs when that form does (TOPLEVEL-ORDER)
                     :toplevel (gethash d *toplevel-order*))))
             (setf (gethash fn *method-of-fn*) m)
             (push m (gethash var *generic-declarations*))
             (push m (gfun-methods (gfun-for var)))))))))
  ;; Slot accessors
  (loop for sds in (cons *singleton-sdefs*
                         (loop for g being the hash-values of *gclasses* collect (gclass-sdefs g)))
        do (dolist (sd sds)
             (declare-slot-storage sd)
             (unless (eq (sdef-allocation sd) :virtual)
               (dolist (m (gen-accessors sd))
                 ;; The place of the top-level form its definition is in,
                 ;; if it surely runs when that does (TOPLEVEL-ORDER)
                 (setf (gmethod-toplevel m) (gethash (sdef-definer sd) *toplevel-order*))
                 (push m (gfun-methods (gfun-for (if (eq (gmethod-kind m) :getter)
                                                     (sdef-getter sd)
                                                     (sdef-setter sd)))))))
             ;; Chapter 6: the getter and setter are generic functions even
             ;; if no method is added, of one argument and of two.
             (let ((getter (gfun-for (sdef-getter sd))))
               (unless (gfun-required getter) (setf (gfun-required getter) 1)))
             (when (sdef-setter sd)
               (let ((setter (gfun-for (sdef-setter sd))))
                 (unless (gfun-required setter) (setf (gfun-required setter) 2))))))
  (loop for g being the hash-values of *gfuns*
        do (settle-generic-in-run-order g (reverse (gethash (gfun-var g) *generic-declarations*))))
  (add-foreign-copies)
  ;; initialize has its method on <object>.
  (let ((init (gethash (dylan-var "initialize") *gfuns*)))
    (when init
      (push (make-gmethod :specs (list (object-spec)) :kind :default :rest-p t
                          :entry-c "dy_default_initialize.entry"
                          :object-c "dy_default_initialize" :order 0)
            (gfun-methods init))))
  (loop for g being the hash-values of *gfuns*
        do (finish-generic g)))

(defparameter +not-congruent+
  (cons "the parameter list of a method of " " is not congruent with its generic function"))

(defun method-congruence-error (m required variable-p declared)
  "The error, as (before-name . after-name), that adding M signals when its
generic function takes REQUIRED arguments, more if VARIABLE-P, and
declares the keys DECLARED (chapter 6); or NIL."
  (multiple-value-bind (r v) (method-shape m)
    (cond ((or (/= r required) (not (eq (and v t) (and variable-p t))))
           +not-congruent+)
          ((and (consp declared) (eq (gmethod-kind m) :ir)
                (not (gmethod-rest-p m)) (listp (gmethod-keys m)))
           (let ((missing (remove-if (lambda (k) (member k (gmethod-keys m))) declared)))
             (and missing
                  (cons "a method of "
                        (format nil " does not accept the keys its generic function names: ~{~a~^ ~}"
                                (mapcar #'dylan-string missing)))))))))

(defun same-declared-keys-p (a b)
  "Do the keys A and B of two declarations (NIL, :ANY or a list) agree?"
  (cond ((or (null a) (null b)) (and (null a) (null b)))
        ((or (atom a) (atom b)) (and (atom a) (atom b)))
        (t (and (subsetp a b) (subsetp b a)))))

(defun settle-generic (g events &optional dry)
  "Settle the parameter list of the generic function G from EVENTS, in
the order they run, as the interpreter does when they run (chapter 6):
its define-generic-functions, the methods of its define-methods, and
(:ACCESSOR sdef kind) for a slot whose getter or setter it is.  The first
gives the parameter list: a define-method's method a provisional one, a
define-generic-function or a slot a fixed one.  A method not congruent
with it is an error when its definition runs, and is never added.  A
define-generic-function then takes over a provisional one, unless the
methods so far are not congruent with it, an error when it runs; with a
fixed one it must be consistent, or it is an error when it runs.  Answer
the events that are errors, and the parameter list as (required
variable-p declared-keys declared-rest).  With DRY, G and the tables are
left as they are."
  (let ((state nil)                     ; NIL, :PROVISIONAL or :FIXED
        required rest keys declared-p (attached '()) (errors '()))
    (labels ((variable-p () (or rest (and keys t)))
             (declared () (and declared-p (or keys :none)))
             (fix (r rs ks dgf)
               (setf state :fixed required r rest rs keys ks declared-p dgf))
             (fail (e table err)
               (push e errors)
               (unless dry (setf (gethash e table) err)))
             (add (m)
               (let ((err (method-congruence-error m required (variable-p) (declared))))
                 (cond ((null err) (push m attached))
                       (dry (push m errors))
                       ((eq (gmethod-kind m) :ir)
                        (push m errors)
                        (setf (gethash (gmethod-fn m) *incongruent*) err
                              (gfun-methods g) (remove m (gfun-methods g))))
                       (t
                        (push m errors)
                        (setf (gethash m *incongruent*) err
                              (gfun-methods g) (remove m (gfun-methods g)))
                        (push m (gfun-incongruent g)))))))
      (dolist (e events)
        (cond
          ((ir-define-generic-p e)
           (let ((r (ir-define-generic-required e))
                 (rs (and (ir-define-generic-rest e) t))
                 (ks (ir-define-generic-keys e)))
             (case state
               ((nil) (fix r rs ks t))
               (:provisional
                (if (or (/= r required)
                        (some (lambda (m)
                                (method-congruence-error m r (or rs (and ks t)) (or ks :none)))
                              attached))
                    (fail e *incongruent* +not-congruent+)
                    (fix r rs ks t)))
               (:fixed
                (if (and (= r required) (eq rs rest) (same-declared-keys-p ks keys))
                    (setf declared-p t)
                    (fail e *inconsistent-declarations* t))))))
          ((consp e)
           (destructuring-bind (sd kind) (cdr e)
             ;; The slot makes the generic function if there is none.
             (unless state (fix (if (eq kind :getter) 1 2) nil nil nil))
             (let ((m (find-if (lambda (m) (and (eq (gmethod-sdef m) sd) (eq (gmethod-kind m) kind)))
                               (gfun-methods g))))
               (when m (add m)))))
          ((null state)
           (multiple-value-bind (r v) (method-shape e)
             (setf state :provisional required r rest (and v t) keys nil declared-p nil)
             (push e attached)))
          (t (add e))))
      (when (and state (not dry))
        (setf (gfun-required g) required
              (gfun-variable-p g) (variable-p)
              (gfun-declared-keys g) (declared)
              (gfun-declared-rest g) (and declared-p rest)))
      (values errors
              (and state (list required (variable-p) (declared) (and declared-p rest)))))))

(defun declaration-place (e)
  "The place in the order the program runs of the generic function event
E (SETTLE-GENERIC), as (place . surely): the place is -1 for the
library's, which it has from the start, and otherwise the index of the
top-level form E is in (TOPLEVEL-ORDER); SURELY is true if E runs when
that form does, not maybe later, or maybe not at all, from code within it
-- a method, a branch -- which cannot run before the form does."
  (flet ((in-form (node)
           (let ((fn (gethash node *definition-fns*)))
             (cons (or (and fn (gethash (fn-toplevel fn) *toplevel-order*)) 0)
                   (and (gethash node *toplevel-order*) t)))))
    (cond ((ir-define-generic-p e)
           (if (library-location-p (ir-node-location e)) (cons -1 t) (in-form e)))
          ((consp e)
           (let ((sd (second e)))
             (if (library-location-p (sdef-location sd))
                 (cons -1 t)
                 (in-form (sdef-definer sd)))))
          ((program-method-p e)
           (if (gmethod-toplevel e)
               (cons (gmethod-toplevel e) t)
               (cons (or (gethash (fn-toplevel (gmethod-fn e)) *toplevel-order*) 0) nil)))
          (t (cons -1 t)))))

(defun declaration-location (e)
  (cond ((ir-define-generic-p e) (ir-node-location e))
        ((consp e) (sdef-location (second e)))
        (t (gmethod-location e))))

(defparameter +run-orders-limit+ 5000
  "The most orders of a generic function's definitions compared.")

(defun possible-run-orders (events places)
  "The orders EVENTS may run in, their PLACES given (DECLARATION-PLACE):
those that surely run, in the order of their top-level forms; each other
at any point after the definitions of the forms before its own.  NIL if
there are more than +RUN-ORDERS-LIMIT+."
  (let* ((pairs (mapcar #'cons events places))
         (sure (stable-sort (remove-if-not #'cddr pairs) #'< :key #'cadr))
         (maybe (remove-if #'cddr pairs))
         (orders (list sure)))
    (dolist (u maybe)
      (let ((next '()))
        (dolist (order orders)
          ;; After every sure definition of an earlier form
          (let ((from (or (position-if (lambda (p) (and (cddr p) (>= (cadr p) (cadr u)))) order)
                          (length order))))
            (loop for k from from to (length order)
                  do (push (append (subseq order 0 k) (list u) (subseq order k)) next))))
        (when (> (length next) +run-orders-limit+) (return-from possible-run-orders nil))
        (setf orders next)))
    (mapcar (lambda (order) (mapcar #'car order)) orders)))

(defun settle-generic-in-run-order (g events)
  "SETTLE-GENERIC, the events of G in the order they run.  Where that
order is known only when the program runs (DECLARATION-PLACE), every
order they may run in must settle G alike, the same definitions errors
and the same parameter list, or the definitions are refused: a compiled
program's generic function has its parameter list when compiled, and the
interpreter settles it as its definitions run.  Definitions with no error
in one order have none in any."
  (let* ((places (mapcar #'declaration-place events))
         (order (mapcar #'car (stable-sort (mapcar #'cons events places) #'<
                                           :key (lambda (p) (if (cddr p) (cadr p) most-positive-fixnum))))))
    (when (and (notevery #'cdr places) (settle-generic g order t))
      (let* ((orders (possible-run-orders events places))
             (outcomes (mapcar (lambda (order)
                                 (multiple-value-bind (errors shape) (settle-generic g order t)
                                   (cons errors shape)))
                               orders)))
        (when (or (null orders)
                  (notevery (lambda (o)
                              (and (null (set-exclusive-or (car o) (car (first outcomes))))
                                   (equal (cdr o) (cdr (first outcomes)))))
                            outcomes))
          (let ((*location* (or (some (lambda (e place) (and (not (cdr place)) (declaration-location e)))
                                      events places)
                                *location*)))
            (compile-error "the definitions of ~a are not congruent, and which of them runs first is known only when the program runs"
                           (name-string (module-var-name (gfun-var g))))))
        (setf order (first orders))))
    (settle-generic g order)))

(defun made-generic-p (atom)
  "Is ATOM bound to what (make <generic-function> ...) makes?"
  (let ((value (and (ivar-p atom) (not (ivar-assigned-p atom)) (gethash atom *let-values*))))
    (and (ir-call-p value)
         (equal (static-value (ir-call-function value)) (list :global (dylan-var "make")))
         (ir-call-arguments value)
         (equal (static-value (first (ir-call-arguments value)))
                (list :global (dylan-var "<generic-function>"))))))

(defvar *held-made* nil "Set by HELD-GENERICS when it finds a made one.")

(defun held-generics (var &optional seen)
  "The generic functions of the program the module variable VAR may hold
from what its set!s and defines store: a list of GFUNs, or :ALL when it
may be one not known when compiling.  A generic function made when the
program runs is not one of them: finding one sets *HELD-MADE*, as :ALL
does."
  (let ((found '())
        (seen (cons var seen)))
    (dolist (atom (append (gethash var *global-definitions*) (gethash var *global-set-values*))
                  found)
      (let ((value (static-value atom)))
        (case (car value)
          (:global
           (let ((w (second value)))
             (unless (member w seen)
               (let ((g (gethash w *gfuns*))
                     (more (held-generics w seen)))
                 (when (eq more :all) (return :all))
                 (setf found (union found (if g (adjoin g more) more)))))))
          ((:constant :singleton))
          (t (cond ((made-generic-p atom) (setf *held-made* t))
                   ;; A method, which is not a generic function
                   ((and (ivar-p atom) (not (ivar-assigned-p atom))
                         (ir-lambda-p (gethash atom *let-values*))))
                   (t (setf *held-made* t) (return :all)))))))))

(defun attached-when-run ()
  "The program's methods its definitions attach when they run: those of
its define-methods and its slots' getters and setters (which are given
their generic function's variable here)."
  (let ((found '()))
    (loop for m being the hash-values of *method-of-fn*
          when (program-method-p m) do (push m found))
    (loop for g being the hash-values of *gfuns*
          do (dolist (m (append (gfun-methods g) (gfun-incongruent g)))
               (when (and (member (gmethod-kind m) '(:getter :setter)) (added-when-run-p m))
                 (setf (gmethod-var m) (gfun-var g))
                 (push m found))))
    found))

(defun add-foreign-copies ()
  "Chapter 6: define-method adds its method to the generic function its
variable holds when it runs.  A program's variable that a set! or define
may give another generic function: each method a define-method of it
makes has a copy in each other generic function it may hold, attached
when the definition runs and the variable holds that one -- or, not
congruent with that one, the error then."
  (let ((all (loop for g being the hash-values of *gfuns* collect g)))
    (loop for m in (attached-when-run)
          for var = (gmethod-var m)
          for own = (gethash var *gfuns*)
          when (or (gethash var *global-set-values*) (gethash var *global-definitions*))
            do (let* ((*held-made* nil)
                      (targets (held-generics var)))
                 (when *held-made*
                   ;; Added when it runs, entered by its object
                   (setf (gmethod-made m) t
                         *made-generic-dispatch* t
                         *reflective-methods* t))
                 (dolist (g (remove own (if (eq targets :all) all targets)))
                   (when (gfun-required g)
                     (let ((err (method-congruence-error m (gfun-required g) (gfun-variable-p g)
                                                         (gfun-declared-keys g))))
                       (if err
                           ;; With :ALL, only those it may be added to
                           (unless (eq targets :all)
                             (push (cons g err) (gmethod-foreign-errors m)))
                           (let ((c (copy-gmethod m)))
                             (setf (gmethod-var c) (gfun-var g)
                                   (gmethod-flag-c c) nil (gmethod-toplevel c) nil
                                   (gmethod-siblings c) '() (gmethod-closure-c c) nil
                                   (gmethod-object-c c) nil (gmethod-original c) m
                                   (gmethod-copies c) '() (gmethod-foreign-errors c) '())
                             (push c (gmethod-copies m))
                             (push c (gfun-methods g)))))))
                 ;; Attached to its own only if the variable holds it then
                 (when (or (gmethod-copies m) (gmethod-foreign-errors m) (gmethod-made m))
                   (setf (gmethod-toplevel m) nil))))))

(defun finish-generic (g)
  (let ((var (gfun-var g)))
    ;; A program's variable may be defined both by define and as a generic
    ;; function: each definition checks, when it runs, what the variable
    ;; holds (chapters 4 and 6), so it holds its value in a cell, as an
    ;; assigned one does.
    (when (and (not (library-var-p var))
               (some #'define-variable-node-p (module-var-definitions var)))
      (setf (gethash var *assigned-globals*) t))
    (when (and (library-var-p var)
               (some (lambda (d) (define-variable-node-p d)) (module-var-definitions var)))
      ;; Placed at the program's definition, not the library's
      (let ((*location*
              (node-location
               (or (find-if (lambda (d)
                              (let ((l (node-location d)))
                                (and l (not (library-source-file-p (location-file l))))))
                            (module-var-definitions var))
                   (find-if #'define-variable-node-p (module-var-definitions var))))))
        (compile-error "~a is defined both as a variable and as a generic function"
                       (name-string (module-var-name var)))))
    ;; In program order.  A method with the same specializers as another
    ;; replaces it when its define-method runs (chapters 6 and 10), so the
    ;; program has both, and each of a program's methods a flag set while
    ;; it is attached, which dispatch tests.
    (flet ((same-specs-p (a b)
             (and (= (length (gmethod-specs a)) (length (gmethod-specs b)))
                  (every #'same-spec-p (gmethod-specs a) (gmethod-specs b)))))
      (let ((methods '()))
        ;; The library's own, which it has from the start: a later one
        ;; replaces an earlier.
        (dolist (m (sort (copy-list (gfun-methods g)) #'< :key #'gmethod-order))
          (unless (flagged-method-p m)
            (setf methods (remove-if (lambda (old)
                                       (and (not (flagged-method-p old)) (same-specs-p old m)))
                                     methods)))
          (setf methods (append methods (list m))))
        (dolist (m methods)
          (setf (gmethod-var m) (gfun-var g)
                (gmethod-siblings m)
                (remove-if-not (lambda (other) (and (not (eq other m)) (same-specs-p other m)))
                               methods)))
        (setf (gfun-methods g) methods)))
    (let ((methods (gfun-methods g)))
      (dolist (m methods)
        (when (or (flagged-method-p m) (some #'flagged-method-p (gmethod-siblings m)))
          ;; Named apart from the library's objects, whose names must not
          ;; depend on the program
          (let ((name (format nil "~a_on~d" (gfun-c-name g) (position m methods))))
            (setf (gmethod-flag-c m) name)
            ;; The library's are attached from the start.
            (declare-object "static unsigned char ~a~:[~; = 1~];" name
                            (not (added-when-run-p m)))))
        ;; A method that refers to local variables is a closure, made
        ;; anew each time its define-method runs (chapter 6), as method
        ;; makes one.
        (when (and (eq (gmethod-kind m) :ir) (ir-fn-free-vars (gmethod-fn m)))
          (let ((name (format nil "~a_mc~d" (gfun-c-name g) (position m methods))))
            (setf (gmethod-closure-c m) name)
            (declare-object "static D ~a = DY_FALSE;" name))))
      (setf (gfun-methods g) methods))
    ;; A program's generic function has no value until one of its
    ;; definitions has run (chapter 6).
    (unless (eq (module-var-module var) (dylan-module))
      (let ((name (format nil "~a_defined" (gfun-c-name g))))
        (setf (gfun-defined-c g) name)
        (declare-object "static int ~a;" name))
      ;; Its variable is read-only once its define-generic-function has
      ;; run (chapter 5), not before; and the keys it names are accepted
      ;; from then (LEAF-KEYWORD-CHECK).
      (when (and (gfun-declared-keys g)
                 (or (gethash var *assigned-globals*)
                     (not (eq (gfun-declared-keys g) :none))))
        (let ((name (format nil "~a_declared" (gfun-c-name g))))
          (setf (gfun-declared-c g) name)
          (declare-object "static int ~a;" name))))
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
                                (name-string (module-var-name var)))))))
      ;; Chapter 6: if define-generic-function names keys, every method
      ;; must accept them, by naming them after #key or by #rest.
      (let ((declared (gfun-declared-keys g)))
        (when (and (consp declared) (eq (gmethod-kind m) :ir)
                   (not (gmethod-rest-p m)) (listp (gmethod-keys m)))
          (let ((missing (remove-if (lambda (k) (member k (gmethod-keys m))) declared)))
            (when missing
              (let ((*location* (or (gmethod-location m) *location*)))
                (compile-error "a method of ~a does not accept the keys its generic function names: ~a"
                               (name-string (module-var-name var))
                               (format nil "~{~a~^ ~}" (mapcar #'dylan-string missing)))))))))
    (unless (gfun-required g) (setf (gfun-required g) 0))
    (setf (gethash var *constant-globals*) (address (gfun-c-name g)))))

;;; When methods are attached (chapter 6)

(defun flagged-method-p (m)
  "Has M a flag set while it is attached: a program's define-method's, the
getter or setter of a program's define-slot, or a copy of a method in
another generic function (ADD-FOREIGN-COPIES)?  A define-slot's may be
called on instances made before it runs, which must not find it until
then (chapter 6); a define-class's need no flag unless another method
has its specializers, since no instance of the class is made before it
runs."
  (or (program-method-p m) (define-slot-accessor-p m) (and (gmethod-original m) t)
      (and (setter-detachers m) t)))

(defun program-method-p (m)
  "Is M a method a program's define-method makes?"
  (and (eq (gmethod-kind m) :ir) (not (ir-fn-library-p (gmethod-fn m)))))

(defun program-accessor-p (m)
  "Is M the getter or setter of a slot a program's define-class or
define-slot defines, attached when that runs?"
  (and (member (gmethod-kind m) '(:getter :setter))
       (added-when-run-p m)))

(defun define-slot-accessor-p (m)
  (and (program-accessor-p m)
       (ir-define-slot-p (sdef-definer (gmethod-sdef m)))))

(defun detaches-setter-p (new old)
  "Does NEW, a define-slot on OLD's class of the slot with OLD's getter,
give the slot no setter method in OLD's setter generic function?  The
slot it defines replaces OLD's (chapter 6), so when it runs OLD's setter
method, which would write the old slot, is detached."
  (and (not (eq new old))
       (sdef-class old)
       (eq (sdef-class new) (sdef-class old))
       (ir-define-slot-p (sdef-definer new))
       (eq (sdef-getter new) (sdef-getter old))
       (not (eq (sdef-allocation new) :virtual))
       (not (eq (sdef-allocation old) :virtual))
       (sdef-setter old)
       (not (eq (sdef-setter new) (sdef-setter old)))))

(defun setter-detachers (m)
  "If M is the setter method of a slot of a program's class, the
define-slots that detach it when they run (DETACHES-SETTER-P)."
  (let ((sd (gmethod-sdef m)))
    (and (eq (gmethod-kind m) :setter)
         sd (sdef-class sd)
         (added-when-run-p m)
         (let ((g (class-gclass (sdef-class sd))))
           (and g (remove-if-not (lambda (new) (detaches-setter-p new sd))
                                 (gclass-sdefs g)))))))

(defun library-location-p (location)
  (and location (location-p location) (library-source-file-p (location-file location))))

(defun added-when-run-p (m)
  "Is M attached to its generic function when the program's definition
of it runs -- a define-method, or the define-class or define-slot of a
slot it reads or writes -- not from the start?"
  (or (program-method-p m)
      (and (member (gmethod-kind m) '(:getter :setter))
           (not (library-location-p (sdef-location (gmethod-sdef m)))))))

(defun method-object-c-expr (m)
  "The C expression of M's <method> object: its static object, or the
closure its define-method made last."
  (or (gmethod-closure-c m) (address (gmethod-object-c m))))

(defun method-attachment-c (m &optional closure (define t))
  "The C statements that attach M, as its definition does when it runs:
the methods with its specializers are detached.  CLOSURE, if not NIL, is
the C of the closure of M the definition made now, which replaces the one
made before (GMETHOD-CLOSURE-C).  With DEFINE, the definition gives the
generic function's variable its value too, first, since that may be an
error.  The change itself is made with the methods' lock held, so that
definitions running at once in several threads attach each method once
(dy_lock_methods)."
  (let ((g (gethash (gmethod-var m) *gfuns*))
        (replaced (gmethod-siblings m)))
    (append
     (and define (generic-definition-c g))
     (list "dy_lock_methods();")
     (loop for s in replaced
           when (gmethod-flag-c s) collect (format nil "~a = 0;" (gmethod-flag-c s)))
     (and (gmethod-flag-c m) (list (format nil "~a = 1;" (gmethod-flag-c m))))
     (let ((object (or closure (method-object-c-expr m)))
           (dropped (append (mapcar #'method-object-c-expr replaced)
                            (and closure (list (gmethod-closure-c m))))))
       (list (if dropped
                 (format nil "dy_add_method(~a, ~a, (const D[]){ ~{~a~^, ~} }, ~d);"
                         (address (gfun-c-name g)) object dropped (length dropped))
                 (format nil "dy_add_method(~a, ~a, NULL, 0);"
                         (address (gfun-c-name g)) object))))
     (and closure (list (format nil "~a = ~a;" (gmethod-closure-c m) closure)))
     (list "dy_unlock_methods();"))))

(defun defined-flag-c (var)
  "The C name of the flag that says whether VAR, the variable of a
program's class or generic function, has its value yet; NIL if it is not
one."
  (unless (eq (module-var-module var) (dylan-module))
    (let ((c (gethash var *gclasses*))
          (g (gethash var *gfuns*)))
      (cond (c (gclass-ready-c c))
            (g (gfun-defined-c g))))))

;;; Assigned module variables (chapters 5 and 7)

(defun library-var-p (var)
  (eq (module-var-module var) (dylan-module)))

(defun read-only-var-p (var)
  "Is the module variable VAR read-only: a class's, or a program's
define-generic-function's (chapter 5)?  The library's functions are
plain variables, as the manual's own (set! (setter element) ...) has
them."
  (or (and (global-class var) t)
      (let ((g (gethash var *gfuns*)))
        (and g (gfun-declared-keys g) (not (library-var-p var)) t))))

(defun variable-global-p (var &optional fn)
  "Must the code of FN read the module variable VAR when it runs?  The
program assigns VAR, and FN is not the library reading a variable of its
own: the library's C is the same for every program."
  (and (gethash var *assigned-globals*)
       (not (and fn (ir-fn-library-p fn) (library-var-p var)))))

(defun library-ref-p (ref)
  "Is REF, an IR-GLOBAL-REF, a macro's reference to a library variable,
which reads the library's own value whatever the program assigns it?"
  (and (ir-global-ref-library ref) (library-var-p (ir-global-ref-var ref))))

(defun library-ref-atom-p (atom)
  "Is ATOM bound to a macro's reference to a library variable?"
  (and (ivar-p atom) (not (ivar-assigned-p atom))
       (let ((value (gethash atom *let-values*)))
         (and (ir-global-ref-p value) (library-ref-p value)))))

(defun generic-definition-c (g &optional other)
  "The C statements a definition of the generic function G does when it
runs, besides attaching a method: G's variable has its value (chapter 6).
If the program assigns the variable, it is given G when it has no value,
and must hold G otherwise -- or, given OTHER, a function of the C of
the value and of the name, its C statement for any other value."
  (let ((var (gfun-var g)))
    (append
     (and (gfun-defined-c g) (list (format nil "~a = 1;" (gfun-defined-c g))))
     (and (variable-global-p var)
          (let ((cell (global-c-name var)) (object (address (gfun-c-name g)))
                (name (c-utf8-literal (name-string (module-var-name var)))))
            (list (if other
                      (format nil "if (~a == DY_UNBOUND) ~a = ~a; else if (~a != ~a) ~a"
                              cell cell object cell object (funcall other cell name))
                      (format nil "if (~a == DY_UNBOUND) ~a = ~a; else dy_check_generic_variable(~a, ~a, ~a);"
                              cell cell object cell object name))))))))

(defun fn-toplevel (fn)
  (loop while (ir-fn-parent fn) do (setf fn (ir-fn-parent fn)))
  fn)

(defun fn-within-p (fn outer)
  (loop for f = fn then (ir-fn-parent f)
        while f
        thereis (eq f outer)))

(defun method-certain-p (m site)
  "Is M surely attached whenever the code of the fn SITE runs, so that a
call there may go straight to it (or have its code inline)?  So it is if
it is always attached; or if its define-method (or, for a slot's getter
or setter, its define-class or define-slot), a top-level form of its own
that surely runs and that nothing replaces, ran before SITE's top-level
form, or SITE's code is M's own.  Never a method that refers to local
variables: it is a
closure its define-method makes when it runs, which only the generic
function's dispatch has (GMETHOD-CLOSURE-C), not the static object a
direct call would pass."
  (and (not (and (eq (gmethod-kind m) :ir) (ir-fn-free-vars (gmethod-fn m))))
       (or (null (gmethod-flag-c m))
           (and site
                (or (program-method-p m) (program-accessor-p m))
                (null (gmethod-siblings m))
                (null (setter-detachers m))
                (gmethod-toplevel m)
                (or (and (program-method-p m) (fn-within-p site (gmethod-fn m)))
                    (let ((here (gethash (fn-toplevel site) *toplevel-order*)))
                      (and here (< (gmethod-toplevel m) here))))))))

;;; Method objects and direct entries

(defun made-only-methods ()
  "The methods that a generic function made when the program runs may be
given, but not their own: not congruent with it."
  (loop for m in (append (loop for m being the hash-values of *method-of-fn* collect m)
                         (loop for g being the hash-values of *gfuns* append (gfun-incongruent g)))
        when (and (gmethod-made m)
                  (not (member m (gfun-methods (gethash (gmethod-var m) *gfuns*)))))
          collect m))

(defun method-objects ()
  (dolist (m (append (loop for g being the hash-values of *gfuns* append (gfun-methods g))
                     (made-only-methods)))
    (unless (gmethod-object-c m)
      (let ((name (cdata-name *cdata* "dy_method_")))
        (setf (gmethod-object-c m) name)
        (declare-object "DyFunction ~a;" name)))))

(defun emit-method-objects ()
  "Emit each method's object.  Its specializers and its arguments are for
the reflective functions, which can get at a method of a generic function
only when *REFLECTIVE-METHODS*; otherwise they are dropped.  Its owner, the
generic function, is how it prints (chapter 11's {a method on double})."
  (dolist (m (append (loop for g being the hash-values of *gfuns* append (gfun-methods g))
                     (made-only-methods)))
    (unless (eq (gmethod-kind m) :default)
      (emit-object "DyFunction ~a = { &dy_wrapper_method, ~a, ~a, ~a, ~a, NULL };"
                   (gmethod-object-c m)
                   (if (and (eq (gmethod-kind m) :ir) *reflective-methods*)
                       (format nil "~a_direct" (gmethod-entry-c m))
                       (gmethod-entry-c m))
                   (if *reflective-methods* (spec-vector-c (gmethod-specs m)) "DY_FALSE")
                   (if *reflective-methods* (method-info-c m) "DY_FALSE")
                   (address (gfun-c-name (gethash (gmethod-var m) *gfuns*)))))))

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

(defconstant +area-arguments+ 1024 "As DY_AREA_ARGUMENTS in dylan.h.")
(defconstant +inline-values+ 1024 "As DY_INLINE_VALUES in dylan.h.")

(defun arg-c (i)
  "The C lvalue of argument I: a0..a3, the thread's argument area, or
past +AREA-ARGUMENTS+ its overflow."
  (cond ((< i 4) (format nil "a~d" i))
        ((< i +area-arguments+) (format nil "dy_thread->args[~d]" (- i 4)))
        (t (format nil "dy_thread->more_args[~d]" (- i +area-arguments+)))))

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
    (when *made-generic-dispatch* (emit-made-dispatch-entry m))
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

(defun emit-made-dispatch-entry (m)
  "With *MADE-GENERIC-DISPATCH*: the start of M's direct entry, which a
generic function made when the program runs enters with its next method
in dispatch_next, having chosen M and checked the arguments."
  (emit-object "    if (DY_UNLIKELY(dy_thread->dispatch_for == self)) {")
  (emit-object "        dy_thread->next_method = dy_thread->dispatch_next;")
  (emit-object "        dy_thread->dispatch_for = 0;")
  (emit-object "        DY_TAIL_CALL_DIRECT(~a, self, a0, a1, a2, a3, argc);" (gmethod-entry-c m))
  (emit-object "    }"))

;;; Dispatch

(defvar *leaves* nil "list of methods -> leaf label, for one generic.")
(defvar *nexts* nil "list of methods -> next-method object C name.")

(defun next-c (methods)
  "The C expression for the next method of a call that has METHODS, in
order, left to run: #f if none of them is attached."
  (let ((flags '()))
    (dolist (m methods)
      (if (gmethod-flag-c m)
          (push (gmethod-flag-c m) flags)
          (return-from next-c (next-object methods))))
    (if flags
        (format nil "((~{~a~^ || ~}) ? ~a : DY_FALSE)" (reverse flags) (next-object methods))
        "DY_FALSE")))

(defun emit-method-runs (methods nexts pad)
  "Emit the code that runs the first attached method of METHODS, NEXTS
being the next-method expression for each: answer whether one surely is."
  (loop for m in methods
        for next in nexts
        for flag = (gmethod-flag-c m)
        do (when flag (emit-object "~aif (~a) {" pad flag))
           (emit-object "~a~:[~;    ~]dy_thread->next_method = ~a;" pad flag next)
           (emit-object "~a~:[~;    ~]DY_TAIL_CALL_DIRECT(~a, ~a, a0, a1, a2, a3, argc);"
                        pad flag (gmethod-entry-c m) (method-object-c-expr m))
           (if flag
               (emit-object "~a}" pad)
               (return t))
        finally (return nil)))

(defun next-object (methods)
  "The C expression for the next method that runs the first attached
method of METHODS, with the rest after it."
  (if (null methods)
      "DY_FALSE"
      (address
       (or (gethash methods *nexts*)
           (setf (gethash methods *nexts*)
                 (let* ((name (cdata-name *cdata* "dy_next_"))
                        ;; Made first, apart: making them emits functions.
                        (nexts (loop for tail on methods collect (next-c (rest tail)))))
                   (declare-object "static D ~a_entry(D, D, D, D, D, intptr_t);" name)
                   (declare-object "static DyFunction ~a;" name)
                   (emit-object "static DyFunction ~a = { &dy_wrapper_method, ~a_entry, DY_FALSE, DY_FALSE, DY_FALSE, NULL };"
                                name name)
                   (emit-object "static D ~a_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {"
                                name)
                   (emit-object "    (void)self;")
                   (unless (emit-method-runs methods nexts "    ")
                     ;; (Not reached: the call checked that one is attached.)
                     (emit-object "    dy_no_applicable_method(~a);"
                                  (address (gfun-c-name (gethash (gmethod-var (first methods))
                                                                 *gfuns*)))))
                   (emit-object "}")
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
    (if (or (typep value '(or ratio float)) (dylan::extended-float? value)
            (runtime-spec-p spec))
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
methods are METHODS, or NIL.  Only the methods attached when the call is
made count (chapter 6), and the keys a define-generic-function names only
once it has run: a program's, flagged (GMETHOD-FLAG-C, GFUN-DECLARED-C),
are known only then."
  (when (gfun-variable-p g)
    (let* ((r (gfun-required g))
           (declared (gfun-declared-keys g))
           ;; Each source of keys, as (flag keyed rest keys): FLAG the C
           ;; of the flag set while it counts, or NIL if it always does
           (sources (append
                     (loop for m in methods
                           for k = (gmethod-keys m)
                           collect (list (gmethod-flag-c m) (and k t) (gmethod-rest-p m)
                                         (and (consp k) k)))
                     (and declared (not (eq declared :none))
                          (list (list (gfun-declared-c g) t nil
                                      (and (consp declared) declared))))))
           (check (format nil "dy_check_keywords(self, argc, ~d, a0, a1, a2, a3, " r)))
      (labels ((condition (test)
                 ;; T if a source passing TEST always counts, else the
                 ;; flags of those that may, NIL if none
                 (let ((those (remove-if-not test sources)))
                   (if (some (lambda (s) (null (first s))) those)
                       t
                       (mapcar #'first those))))
               (c-test (flags) (format nil "~{~a~^ || ~}" flags)))
        (let ((keyed (condition #'second))
              (any (condition #'third))
              (always-keys (remove-duplicates
                            (loop for (flag nil nil keys) in sources
                                  unless flag append keys)))
              (flagged-keys (loop for (flag nil nil keys) in sources
                                  when (and flag keys) collect (cons flag keys))))
          (flet ((check-keys (out)
                   ;; The keys of the sources that count, gathered when run
                   (if (and (null always-keys) (null flagged-keys))
                       (format out "~a~a, 0);" check (keyword-array '()))
                       (progn
                         ;; (Initialized: with every flag clear, none is stored.)
                         (format out "{~%            D accepted[~d] = { 0 };~%            size_t n = 0;"
                                 (+ (length always-keys)
                                    (loop for (nil . keys) in flagged-keys sum (length keys))))
                         (dolist (k always-keys)
                           (format out "~%            accepted[n++] = ~a;" (c-constant *cdata* k)))
                         (loop for (flag . keys) in flagged-keys
                               do (format out "~%            if (~a) {~{ accepted[n++] = ~a;~} }"
                                          flag (mapcar (lambda (k) (c-constant *cdata* k)) keys)))
                         (format out "~%            ~aaccepted, n);~%        }" check)))))
            (cond ((null keyed) nil)
                  ((and (eq keyed t) (eq any t))
                   (format nil "~aNULL, 0);" check))
                  ((and (eq keyed t) (null any) (null flagged-keys))
                   (format nil "~a~a, ~d);" check (keyword-array always-keys) (length always-keys)))
                  (t
                   (with-output-to-string (out)
                     (unless (eq keyed t) (format out "if (~a) " (c-test keyed)))
                     (format out "{~%        ")
                     (cond ((eq any t) (format out "~aNULL, 0);" check))
                           (t (when any
                                (format out "if (~a) ~aNULL, 0); else " (c-test any) check))
                              (check-keys out)))
                     (format out "~%    }"))))))))))

(defun emit-dispatch (g)
  (let* ((*leaves* (make-hash-table :test #'equal))
         (r (gfun-required g))
         (name (format nil "~a_dispatch" (gfun-c-name g)))
         ;; The tree first, apart: making the next methods emits functions.
         (tree (let ((*object-text* '()))
                 (gen-dispatch-tree g 0 (mapcar (lambda (m) (list m)) (gfun-methods g)) 1)
                 (reverse *object-text*)))
         (leaves (loop for methods being the hash-keys of *leaves* using (hash-value label)
                       collect (list label methods
                                     (loop for tail on methods collect (next-c (rest tail)))
                                     (leaf-keyword-check g methods)))))
    (declare-object "static D ~a(D, D, D, D, D, intptr_t);" name)
    (emit-object "static D ~a(D self, D a0, D a1, D a2, D a3, intptr_t argc) {" name)
    (emit-object "    (void)a0; (void)a1; (void)a2; (void)a3;")
    (emit-object "    if (DY_UNLIKELY(argc ~:[!=~;<~] ~d)) dy_wrong_argument_count(self, argc);"
                 (gfun-variable-p g) r)
    (dotimes (i r)
      (emit-object "    D x~d = ~a; (void)x~d;" i (arg-c i) i))
    (dolist (line tree) (emit-object "~a" line))
    (loop for (label methods nexts check) in (sort leaves #'string< :key #'first)
          do (emit-object "~a:" label)
             (when check (emit-object "    ~a" check))
             ;; The first of them attached (a program's method is once its
             ;; define-method has run)
             (unless (emit-method-runs methods nexts "    ")
               (emit-object "    goto no_method;")))
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
               ;; Those attached from the start: a program's are added
               ;; as their definitions run (ADD-METHOD-C).
               (address (c-vector *cdata* (mapcar (lambda (m) (address (gmethod-object-c m)))
                                                  (remove-if #'added-when-run-p
                                                             (gfun-methods g)))))
               (gfun-required g)
               (let ((declared (gfun-declared-keys g)))
                 ;; Chapter 10: whether it accepts #rest is what its
                 ;; define-generic-function says, or else its first method
                 (function-info-c (gfun-required g)
                                  (if declared
                                      (gfun-declared-rest g)
                                      (gfun-variable-p g))
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
         ;; Chapter 6: make takes the keywords of the initialize methods
         ;; the class has when it runs.  A method with a flag has them
         ;; only while the flag says it is attached.
         (always (remove-if #'gmethod-flag-c init-methods))
         (any (some #'gmethod-rest-p always))
         (any-flags (mapcar #'gmethod-flag-c
                            (remove-if-not (lambda (m) (and (gmethod-flag-c m) (gmethod-rest-p m)))
                                           init-methods)))
         (flagged-keys (loop for m in init-methods
                             for k = (gmethod-keys m)
                             when (and (gmethod-flag-c m) (consp k))
                               collect (cons (gmethod-flag-c m) k)))
         (keywords (remove-duplicates
                    (append (loop for sd in slots
                                  for spec = (sdef-spec sd)
                                  for k = (or (slot-spec-init-keyword spec)
                                              (slot-spec-required-init-keyword spec))
                                  when k collect k)
                            (loop for m in always
                                  for k = (gmethod-keys m)
                                  when (consp k) append k)))))
    (setf (class-info-maker info) name)
    (emit-object "D ~a(D self, D a0, D a1, D a2, D a3, intptr_t argc) {" name)
    (emit-object "    return ~a(dy_make_instance(&~a), self, a0, a1, a2, a3, argc);~%}"
                 filler (wrapper-c-name info))
    (when (slot-changing-class-p info)
      ;; Its slots are what define-slots have made them so far
      (emit-program-class info
                          (append (loop for m in always
                                        for k = (gmethod-keys m)
                                        when (consp k) append (mapcar (lambda (key) (cons key nil)) k))
                                  (loop for (flag . keys) in flagged-keys
                                        append (mapcar (lambda (key) (cons key flag)) keys)))
                          any any-flags)
      (emit-object "D ~a(D x, D self, D a0, D a1, D a2, D a3, intptr_t argc) {" filler)
      (let ((readies (loop for c in (class-info-cpl info)
                           for cg = (class-gclass c)
                           when cg collect (gclass-ready-c cg))))
        (emit-object "    if (!(~{~a~^ && ~})) dy_error(\"make: the class's define-class has not run yet\", a0);"
                     readies))
      (emit-object "    D keys = dy_fill_slots(x, &~a, self, argc, a0, a1, a2, a3);"
                   (program-class-c info))
      (when (and init init-methods)
        (emit-object "    dy_call_list(~a, x, keys);" (address (gfun-c-name init))))
      (emit-object "    (void)keys;~%    dy_thread->mv_count = 1;~%    return x;~%}")
      (return-from emit-maker))
    (emit-object "D ~a(D x, D self, D a0, D a1, D a2, D a3, intptr_t argc) {" filler)
    (emit-object "    (void)self;")
    ;; As the interpreter makes an instance: the required init-keywords
    ;; first, then each slot in the order slot-descriptors answers them,
    ;; then the keywords nothing takes.
    (emit-object "    dy_check_keywords(self, argc, 1, a0, a1, a2, a3, NULL, 0);")
    (emit-object "    D keys = dy_collect_arguments(argc, 1, a0, a1, a2, a3), v;")
    (let ((readies (loop for c in (class-info-cpl info)
                         for cg = (class-gclass c)
                         when cg collect (gclass-ready-c cg))))
      (emit-object "    if (!(~{~a~^ && ~})) dy_error(\"make: the class's define-class has not run yet\", a0);"
                   readies))
    (emit-object "    DyInstance *object = DY_PTR(x);")
    (setf slots (loop for reported in (reported-slot-definitions info)
                      for sd = (find (sdef-getter reported) slots :key #'sdef-getter)
                      when sd collect sd))
    (dolist (sd slots)
      (let ((keyword (slot-spec-required-init-keyword (sdef-spec sd))))
        (when (and keyword (eq (sdef-allocation sd) :instance))
          (emit-object "    if (dy_find_keyword(keys, ~a) == DY_UNSUPPLIED) dy_error(\"make: the required init-keyword is missing\", ~a);"
                       (c-constant *cdata* keyword) (c-constant *cdata* keyword)))))
    (dolist (sd slots)
      (when (eq (sdef-allocation sd) :instance)
        (let* ((spec (sdef-spec sd))
               (keyword (or (slot-spec-init-keyword spec) (slot-spec-required-init-keyword spec)))
               (offset (aref (class-info-slot-offsets info)
                             (gethash (sdef-getter sd) *instance-slot-numbers*))))
          (if keyword
              (emit-object "    v = dy_find_keyword(keys, ~a);" (c-constant *cdata* keyword))
              (emit-object "    v = DY_UNSUPPLIED;"))
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
    ;; The keywords nothing takes, from KEYS: an init function may have
    ;; reused the thread's argument area (dylan.h)
    (cond (any)
          ((and (null flagged-keys) (null any-flags))
           (emit-object "    dy_check_keyword_list(self, keys, ~a, ~d);"
                        (keyword-array keywords) (length keywords)))
          (t
           (emit-object "    if (!(0~{ || ~a~})) {" any-flags)
           (emit-object "        D accepted[~d];~%        size_t n = 0;"
                        (max 1 (+ (length keywords) (loop for (nil . keys) in flagged-keys sum (length keys)))))
           (dolist (k keywords)
             (emit-object "        accepted[n++] = ~a;" (c-constant *cdata* k)))
           (loop for (flag . keys) in flagged-keys
                 do (emit-object "        if (~a) {~{ accepted[n++] = ~a;~} }"
                                 flag (mapcar (lambda (k) (c-constant *cdata* k)) keys)))
           (emit-object "        dy_check_keyword_list(self, keys, accepted, n);~%    }")))
    (when (and init init-methods)
      (emit-object "    dy_call_list(~a, x, keys);" (address (gfun-c-name init))))
    (emit-object "    dy_thread->mv_count = 1;~%    return x;~%}")))

;;; The pass

(defun prepare-object-system (ir)
  "Analyze IR's classes and generic functions; answer the C text they
need, which GENERATE-C places."
  (let ((definitions (collect-definitions ir)))
    (collect-let-values ir)
    (note-own-class-refs definitions)
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
    (declare-program-classes)
    ;; A set! of a class's variable stores nothing: it is an error when
    ;; it runs (chapter 5).  A generic function's is read-only only once
    ;; its define-generic-function has run.
    (loop for var being the hash-keys of *assigned-globals*
          when (global-class var)
            do (remhash var *assigned-globals*))
    (method-objects)))

(defun finish-object-system ()
  "Emit what depends on the code of the methods: dispatch, method
objects, makers."
  (let ((*nexts* (make-hash-table :test #'equal))
        (direct (make-hash-table :test #'equal)))
    (dolist (m (append (loop for g being the hash-values of *gfuns* append (gfun-methods g))
                       (made-only-methods)))
      ;; One for a method and its copies (ADD-FOREIGN-COPIES)
      (when (and (eq (gmethod-kind m) :ir) *reflective-methods*
                 (not (gethash (gmethod-entry-c m) direct)))
        (setf (gethash (gmethod-entry-c m) direct) t)
        (emit-direct-entry m)))
    (loop for g being the hash-values of *gfuns* do (emit-dispatch g))
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
                 (emit-object "    if (a1 == ~a && ~@[~a && ~]dy_wrapper_subtype(w, &~a)) {"
                              (address (gfun-c-name getter))
                              (and (sdef-node sd) (sdef-c sd "ran"))
                              (wrapper-c-name (sdef-class sd)))
                 (ecase (sdef-allocation sd)
                   (:instance
                    (emit-object "        int32_t i = w->slot_offsets ? w->slot_offsets[~d] : -1;"
                                 (gethash (sdef-getter sd) *instance-slot-numbers*))
                    (emit-object "        if (i >= 0) {")
                    (emit-object "            return DY_BOOL(((DyInstance *)DY_PTR(a0))->slots[i] != DY_UNBOUND);~%        }"))
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
    ;; A slot of one object (define-slot on a singleton): its OWNER is the
    ;; object, once the define-slot has run, and its storage the cell.
    (dolist (sd *singleton-sdefs*)
      (let ((spec (sdef-spec sd))
            (getter (gethash (sdef-getter sd) *gfuns*))
            (setter (and (sdef-setter sd) (gethash (sdef-setter sd) *gfuns*))))
        (when getter
          (declare-object "DySlotDescriptor ~a;" (sdef-descriptor-c sd))
          (emit-object "DySlotDescriptor ~a = { &dy_wrapper_slot_descriptor, ~a, ~a, DY_UNBOUND, ~a, DY_UNBOUND, DY_UNBOUND, ~a, DY_UNBOUND, ~d, 0, ~a, true };"
                       (sdef-descriptor-c sd)
                       (address (gfun-c-name getter))
                       (if setter (address (gfun-c-name setter)) "DY_FALSE")
                       (c-constant *cdata* (dylan-name (string-downcase (sdef-allocation sd))))
                       (c-constant *cdata* (or (slot-spec-init-keyword spec)
                                               (slot-spec-required-init-keyword spec)
                                               +false+))
                       (position (sdef-allocation sd) '(:instance :class :each-subclass :constant :virtual))
                       (if (member (sdef-allocation sd) '(:instance :constant))
                           (format nil "&~a" (sdef-c sd "cell"))
                           "NULL")))))
    (dolist (sd sdefs)
      (let ((spec (sdef-spec sd))
            (setter (and (sdef-setter sd) (gethash (sdef-setter sd) *gfuns*))))
        (declare-object "DySlotDescriptor ~a;" (sdef-descriptor-c sd))
        (emit-object "DySlotDescriptor ~a = { &dy_wrapper_slot_descriptor, ~a, ~a, DY_UNBOUND, ~a, DY_UNBOUND, DY_UNBOUND, ~a, ~a, ~d, ~d, ~a, false, ~:[false~;true~], ~:[false~;true~] };"
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
                       (t "NULL"))
                     (slot-spec-required-init-keyword spec)
                     (library-private-slot-p sd))))
    ;; (slot-descriptor instance getter): the most specific definition first
    (emit-object "static D dy_slot_descriptor_entry(D self, D a0, D a1, D a2, D a3, intptr_t argc) {")
    (emit-object "    (void)a2; (void)a3;")
    (emit-object "    if (DY_UNLIKELY(argc != 2)) dy_wrong_argument_count(self, argc);")
    (emit-object "    DyWrapper *w = dy_wrapper_of(a0); (void)w;")
    ;; The slots of single objects, which are more specific
    (dolist (sd *singleton-sdefs*)
      (let ((getter (gethash (sdef-getter sd) *gfuns*)))
        (when getter
          (emit-object "    if (a1 == ~a && dy_identical(a0, ~a.object)) return ~a;"
                       (address (gfun-c-name getter)) (owner-c sd)
                       (address (sdef-descriptor-c sd))))))
    ;; A program's class: its slots as they are now
    (loop for g being the hash-values of *gclasses*
          when (program-gclass-p g)
            do (emit-object "    if (w == &~a) return dy_find_slot_descriptor(~a, a1);"
                            (wrapper-c-name (gclass-info g)) (class-slots-c (gclass-info g))))
    (dolist (sd (stable-sort (remove-if (lambda (sd) (program-gclass-p (class-gclass (sdef-class sd))))
                                        sdefs)
                             #'>
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
          do (if (program-gclass-p g)
                 (emit-object "    if (a0 == ~a) return dy_reported_slots(~a);"
                              (address (class-c-name (gclass-info g))) (class-slots-c (gclass-info g)))
                 (let ((effective (remove-if #'library-private-slot-p
                                             (reported-slot-definitions (gclass-info g)))))
                   (emit-object "    if (a0 == ~a) return ~a;"
                                (address (class-c-name (gclass-info g)))
                                (address (c-vector *cdata* (mapcar (lambda (sd) (address (sdef-descriptor-c sd)))
                                                                   effective)))))))
    (emit-object "    return ~a;~%}" (address (c-vector *cdata* '())))
    (declare-object "DyFunction dy_prim_slot_descriptors;")
    (emit-object "DyFunction dy_prim_slot_descriptors = { &dy_wrapper_method, dy_slot_descriptors_entry, DY_FALSE, DY_FALSE, DY_FALSE, NULL };")))
