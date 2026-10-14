;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; The IR: core Dylan in A-normal form.
;;;;
;;;; Every intermediate value is named.  An ATOM is a constant (CONST) or a
;;;; variable (IVAR); every operand of a call, primitive, if test,
;;;; assignment or definition is an atom.  Anything else is an expression
;;;; that a LET (one value) or MV-LET (several) names, or that appears in a
;;;; position whose value is not used again in the same expression.
;;;;
;;;; Expressions:
;;;;
;;;;   atom                    CONST or IVAR
;;;;   ir-let       var value body
;;;;   ir-mv-let    vars rest value body
;;;;   ir-seq       forms   -- all but the last for effect
;;;;   ir-if        test-atom then else
;;;;   ir-letrec    vars fns body
;;;;   ir-lambda    fn      -- make a closure of FN
;;;;   ir-call      function-atom argument-atoms
;;;;   ir-prim      name argument-atoms
;;;;   ir-global-ref  module-var
;;;;   ir-global-set  module-var atom
;;;;   ir-set       ivar atom
;;;;   ir-bind-exit var body
;;;;   ir-unwind    protected cleanup
;;;;
;;;; and the definitions, whose operands are atoms:
;;;;
;;;;   ir-define-variable  module-var atom
;;;;   ir-define-generic   module-var required rest keys
;;;;   ir-define-method    module-var fn
;;;;   ir-define-class     module-var superclass-atoms slots
;;;;   ir-define-slot      owner-atom slot
;;;;
;;;; Each function is an IR-FN.  A program is the list of its top-level
;;;; forms, each the body of an IR-FN of kind :TOPLEVEL, in order.
;;;;
;;;; The conversion (anf.lisp) gives every variable its own IVAR, so the IR
;;;; is alpha-converted: no two bindings share a variable.  The analysis
;;;; (analyze.lisp) then fills in what later phases need: which variables
;;;; are captured and assigned and how often referred to, which calls are
;;;; tail calls, which functions escape and where the others are called,
;;;; and which exits never leave the function that establishes them.

(in-package #:dylan-compiler)

;;; Atoms

(defvar *fn* nil "The IR-FN being converted or analyzed.")

(defstruct (const (:constructor make-const (value)))
  value)

(defvar *ivar-counter* 0)

(defstruct (ivar (:constructor %make-ivar (name id kind binder)))
  name                                  ; symbol or (setter symbol), for people
  id                                    ; unique within a run
  kind                                  ; :param :next :rest :key :let :temp
                                        ; :letrec :exit
  binder                                ; the IR-FN whose code binds it
  (assigned-p nil)                      ; the target of an ir-set?
  (captured-p nil)                      ; referred to or set by a nested fn?
  (ref-count 0)                         ; references, calls included
  (call-count 0)                        ; references as the function called
  (declared-type nil)                   ; IVAR holding its declared type
  (type nil)                            ; inferred type (Phase 9); NIL: <object>
  (value-fn nil)                        ; the IR-FN it is always bound to
  (escapes-p nil))                      ; :exit only: used other than by a
                                        ; direct call in its own function?

(defun make-ivar (name kind &optional (binder *fn*))
  (%make-ivar name (incf *ivar-counter*) kind binder))

(defun atom-p (x) (or (const-p x) (ivar-p x)))

(defun ivar-string (var)
  (format nil "~a_~d" (name-string (ivar-name var)) (ivar-id var)))

(defmethod print-object ((v ivar) stream)
  (print-unreadable-object (v stream :type t)
    (write-string (ivar-string v) stream)))

;;; Functions

(defvar *fn-counter* 0)

(defstruct (ir-fn (:constructor %make-ir-fn (id kind name parent location)))
  id
  kind                                  ; :toplevel :method :lambda :local
  name                                  ; for people: symbol, name, or NIL
  parent                                ; enclosing IR-FN, or NIL
  location
  (required '())                        ; IVARs
  (specializers '())                    ; one per required: atom, or NIL
  (next nil)                            ; IVAR or NIL
  (rest nil)                            ; IVAR or NIL
  (keys nil)                            ; NIL, :ANY, or list of IR-KEYs
  (body nil)
  (free-vars '())                       ; IVARs bound outside it that it,
                                        ; or a fn within it, uses
  (binder nil)                          ; IVAR it is bound to, if any
  (call-sites '())                      ; IR-CALLs that call it directly
  (escapes-p nil))                      ; may it be called from where its
                                        ; call sites are not known?

(defstruct (ir-key (:constructor make-ir-key (keyword var default)))
  keyword
  var
  default)                              ; expression, evaluated in the fn

(defun make-ir-fn (kind name &optional (location *location*))
  (%make-ir-fn (incf *fn-counter*) kind name *fn* location))

(defmethod print-object ((f ir-fn) stream)
  (print-unreadable-object (f stream :type t)
    (format stream "~(~a~) ~a ~d" (ir-fn-kind f)
            (if (ir-fn-name f) (name-string (ir-fn-name f)) "-")
            (ir-fn-id f))))

(defun fn-shape (fn)
  "The number of required parameters, whether FN takes #rest, and its
keywords (NIL, :ANY, or a list)."
  (values (length (ir-fn-required fn))
          (and (ir-fn-rest fn) t)
          (let ((keys (ir-fn-keys fn)))
            (if (listp keys) (mapcar #'ir-key-keyword keys) keys))))

;;; Expressions

(defstruct (ir-node (:constructor nil))
  (location *location*))

(defstruct (ir-let (:include ir-node) (:constructor make-ir-let (var value body)))
  var value body)

(defstruct (ir-mv-let (:include ir-node)
                      (:constructor make-ir-mv-let (vars rest value body)))
  vars rest value body)

(defstruct (ir-seq (:include ir-node) (:constructor %make-ir-seq (forms)))
  forms)

(defstruct (ir-if (:include ir-node) (:constructor make-ir-if (test then else)))
  test then else)

(defstruct (ir-letrec (:include ir-node)
                      (:constructor make-ir-letrec (vars fns body)))
  vars fns body)

(defstruct (ir-lambda (:include ir-node) (:constructor make-ir-lambda (fn)))
  fn)

(defstruct (ir-call (:include ir-node)
                    (:constructor make-ir-call (function arguments)))
  function arguments
  (tail-p nil)
  (kind :unknown))                      ; :unknown, :known (a fn whose call
                                        ; sites are tracked) or :exit

(defstruct (ir-prim (:include ir-node) (:constructor make-ir-prim (name arguments)))
  name arguments)

(defstruct (ir-global-ref (:include ir-node) (:constructor make-ir-global-ref (var)))
  var)

(defstruct (ir-global-set (:include ir-node)
                          (:constructor make-ir-global-set (var value)))
  var value)

(defstruct (ir-set (:include ir-node) (:constructor make-ir-set (var value)))
  var value)

(defstruct (ir-bind-exit (:include ir-node) (:constructor make-ir-bind-exit (var body)))
  var body)

(defstruct (ir-unwind (:include ir-node)
                      (:constructor make-ir-unwind (protected cleanup)))
  protected cleanup)

;;; Definitions

(defstruct (ir-define-variable (:include ir-node)
                               (:constructor make-ir-define-variable (var value)))
  var value)

(defstruct (ir-define-generic (:include ir-node)
                              (:constructor make-ir-define-generic
                                  (var required rest keys)))
  var required rest keys)

(defstruct (ir-define-method (:include ir-node)
                             (:constructor make-ir-define-method (var fn)))
  var fn)

(defstruct (ir-define-class (:include ir-node)
                            (:constructor make-ir-define-class
                                (var superclasses slots)))
  var
  superclasses                          ; atoms
  slots)                                ; SLOT-SPECs whose type, init-value
                                        ; and init-function are atoms

(defstruct (ir-define-slot (:include ir-node)
                           (:constructor make-ir-define-slot (owner slot)))
  owner slot)

(defun make-ir-seq (forms)
  "FORMS in order, flattened; atoms whose values are discarded dropped."
  (let ((flat '()))
    (dolist (form forms)
      (if (ir-seq-p form)
          (dolist (f (ir-seq-forms form)) (push f flat))
          (push form flat)))
    (let ((last (car flat))
          (effects (remove-if #'atom-p (cdr flat))))
      (cond ((null flat) (make-const dylan::dylan%false))
            ((null effects) last)
            (t (%make-ir-seq (append (reverse effects) (list last))))))))

;;; Programs

(defstruct (ir-program (:constructor make-ir-program (toplevels source library-counters)))
  toplevels                             ; IR-FNs of kind :TOPLEVEL, in order
  source                                ; the PROGRAM they came from
  library-counters)                     ; (ivar-counter . fn-counter) after
                                        ; the library's forms, or NIL

(defparameter +library-id-gap+ 100000
  "How many variable and fn ids the program's start after the library's,
left for those lowering the library makes (LOWER-CONTROL): so the library's
are the same whatever the program, and the library's C too (Phase 9,
separate compilation).")

(defun ir-fn-library-p (fn)
  "Is FN in one of the library's top-level forms?"
  (loop while (ir-fn-parent fn)
        do (setf fn (ir-fn-parent fn)))
  (let ((location (ir-fn-location fn)))
    (and location (library-source-file-p (location-file location)))))

;;; Printing, for people and for the tests.  Calls in tail position print
;;; as (tail-call f ...), calls of an exit procedure as (exit-to x ...).

(defun ir-unparse (x)
  (etypecase x
    (const (unparse-constant (const-value x)))
    (ivar (make-symbol (ivar-string x)))
    (ir-fn `(,(word "method")
             ,@(and (ir-fn-name x) (list (word (name-string (ir-fn-name x)))))
             ,(ir-unparse-parameters x)
             ,(ir-unparse (ir-fn-body x))))
    (ir-let `(let ((,(ir-unparse (ir-let-var x)) ,(ir-unparse (ir-let-value x))))
               ,(ir-unparse (ir-let-body x))))
    (ir-mv-let `(dylan::let-values ((,@(mapcar #'ir-unparse (ir-mv-let-vars x))
                                     ,@(and (ir-mv-let-rest x)
                                            (list (word "#rest")
                                                  (ir-unparse (ir-mv-let-rest x))))
                                     ,(ir-unparse (ir-mv-let-value x))))
                  ,(ir-unparse (ir-mv-let-body x))))
    (ir-seq `(dylan::begin ,@(mapcar #'ir-unparse (ir-seq-forms x))))
    (ir-if `(if ,(ir-unparse (ir-if-test x))
                ,(ir-unparse (ir-if-then x))
                ,(ir-unparse (ir-if-else x))))
    (ir-letrec `(dylan::letrec
                 ,(mapcar (lambda (var fn)
                            (list (ir-unparse var) (ir-unparse fn)))
                          (ir-letrec-vars x) (ir-letrec-fns x))
                 ,(ir-unparse (ir-letrec-body x))))
    (ir-lambda (ir-unparse (ir-lambda-fn x)))
    (ir-call `(,@(case (ir-call-kind x)
                   (:exit (list (word "exit-to")))
                   (t (and (ir-call-tail-p x) (list (word "tail-call")))))
               ,(ir-unparse (ir-call-function x))
               ,@(mapcar #'ir-unparse (ir-call-arguments x))))
    (ir-prim `(dylan::%primitive ,(word (string-downcase (ir-prim-name x)))
                                 ,@(mapcar #'ir-unparse (ir-prim-arguments x))))
    (ir-global-ref (word (module-var-string (ir-global-ref-var x))))
    (ir-global-set `(dylan::set! ,(word (module-var-string (ir-global-set-var x)))
                                 ,(ir-unparse (ir-global-set-value x))))
    (ir-set `(dylan::set! ,(ir-unparse (ir-set-var x)) ,(ir-unparse (ir-set-value x))))
    (ir-bind-exit `(dylan::bind-exit (,(ir-unparse (ir-bind-exit-var x)))
                     ,(ir-unparse (ir-bind-exit-body x))))
    (ir-unwind `(unwind-protect ,(ir-unparse (ir-unwind-protected x))
                  ,(ir-unparse (ir-unwind-cleanup x))))
    (ir-define-variable `(dylan::define ,(word (module-var-string (ir-define-variable-var x)))
                           ,(ir-unparse (ir-define-variable-value x))))
    (ir-define-generic `(dylan::define-generic-function
                         ,(word (module-var-string (ir-define-generic-var x)))
                         ,(word "required:") ,(ir-define-generic-required x)
                         ,(word "rest:") ,(unparse-constant (bool (ir-define-generic-rest x)))
                         ,(word "keys:") ,(let ((keys (ir-define-generic-keys x)))
                                            (if (eq keys :any)
                                                (word "any")
                                                (unparse-constant keys)))))
    (ir-define-method `(dylan::define-method
                        ,(word (module-var-string (ir-define-method-var x)))
                        ,(ir-unparse-parameters (ir-define-method-fn x))
                        ,(ir-unparse (ir-fn-body (ir-define-method-fn x)))))
    (ir-define-class `(dylan::define-class
                       ,(word (module-var-string (ir-define-class-var x)))
                       ,(mapcar #'ir-unparse (ir-define-class-superclasses x))
                       ,@(mapcar #'ir-unparse-slot (ir-define-class-slots x))))
    (ir-define-slot `(dylan::define-slot ,(ir-unparse (ir-define-slot-owner x))
                       ,@(ir-unparse-slot (ir-define-slot-slot x))))))

(defun ir-unparse-parameters (fn)
  (append
   (loop for var in (ir-fn-required fn)
         for spec in (ir-fn-specializers fn)
         collect (if spec
                     (list (ir-unparse var) (ir-unparse spec))
                     (ir-unparse var)))
   (and (ir-fn-next fn) (list (word "#next") (ir-unparse (ir-fn-next fn))))
   (and (ir-fn-rest fn) (list (word "#rest") (ir-unparse (ir-fn-rest fn))))
   (let ((keys (ir-fn-keys fn)))
     (cond ((null keys) '())
           ((eq keys :any) (list (word "#key")))
           (t (cons (word "#key")
                    (mapcar (lambda (k)
                              (list (unparse-constant (ir-key-keyword k))
                                    (ir-unparse (ir-key-var k))
                                    (ir-unparse (ir-key-default k))))
                            keys)))))))

(defun ir-unparse-slot (slot)
  `(,(word (module-var-string (slot-spec-getter slot)))
    ,@(and (slot-spec-setter slot)
           (list (word "setter:") (word (module-var-string (slot-spec-setter slot)))))
    ,@(and (slot-spec-type slot) (list (word "type:") (ir-unparse (slot-spec-type slot))))
    ,@(and (slot-spec-init-value slot)
           (list (word "init-value:") (ir-unparse (slot-spec-init-value slot))))
    ,@(and (slot-spec-init-function slot)
           (list (word "init-function:") (ir-unparse (slot-spec-init-function slot))))
    ,@(and (slot-spec-init-keyword slot)
           (list (word "init-keyword:") (unparse-constant (slot-spec-init-keyword slot))))
    ,@(and (slot-spec-required-init-keyword slot)
           (list (word "required-init-keyword:")
                 (unparse-constant (slot-spec-required-init-keyword slot))))
    ,@(and (not (eq (slot-spec-allocation slot) :instance))
           (list (word "allocation:")
                 (word (string-downcase (slot-spec-allocation slot)))))))

(defun ir-string (x)
  "X, an IR expression or fn, as text."
  (let ((*package* (find-package '#:dylan))
        (*print-escape* nil)
        (*print-case* :downcase)
        (*print-pretty* t)
        (*print-right-margin* 100))
    (princ-to-string (ir-unparse x))))

(defun print-ir-program (ir &optional (stream *standard-output*))
  (dolist (top (ir-program-toplevels ir))
    (format stream "~a~%~%" (ir-string (ir-fn-body top)))))
