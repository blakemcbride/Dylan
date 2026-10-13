;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Core Dylan: what the front end produces.
;;;;
;;;; Every special form and macro of the language is reduced to these
;;;; nodes.  Names are gone: a variable reference holds the variable
;;;; itself -- a LEXICAL-VAR, unique to its binding, or a MODULE-VAR,
;;;; unique to the module that owns it (modules.lisp).  Each node records
;;;; where in the source it came from.
;;;;
;;;;   constant          a literal value
;;;;   ref               read a variable
;;;;   assign            set! a variable; answers the new value
;;;;   if-node           test, then, else
;;;;   seq-node           forms in order; answers the last (begin)
;;;;   bind-node         bind variables to the values of one expression
;;;;                     (the multiple-value bind of the bind form)
;;;;   letrec-node       mutually recursive local methods (bind-methods)
;;;;   method-node       a method
;;;;   call              call a function
;;;;   primitive         call an operation of the C runtime directly
;;;;   bind-exit-node    establish an exit procedure
;;;;   unwind-protect-node
;;;;
;;;; and the top-level definitions, which later phases evaluate at compile
;;;; time where they can:
;;;;
;;;;   define-variable-node    (define name value)
;;;;   define-generic-node     (define-generic-function ...)
;;;;   define-method-node      (define-method ...)
;;;;   define-class-node       (define-class ...)
;;;;   define-slot-node        (define-slot ...)
;;;;
;;;; Values of Dylan constants are represented as the reader represents
;;;; them: #t is T, #f is DYLAN::DYLAN%FALSE, () is NIL, a keyword foo: is
;;;; :FOO, a symbol is a symbol, and so on.

(in-package #:dylan-compiler)

;;; Variables

(defvar *variable-counter* 0)

(defstruct (lexical-var (:constructor %make-lexical-var (name id)))
  name                                  ; symbol or (setter symbol)
  id                                    ; unique within a run
  (assigned-p nil)                      ; is it the target of a set!?
  (type nil))                           ; LEXICAL-VAR holding its declared
                                        ; type, if it has one

(defun make-lexical-var (name)
  (%make-lexical-var name (incf *variable-counter*)))

(defmethod print-object ((v lexical-var) stream)
  (print-unreadable-object (v stream :type t)
    (format stream "~a_~d" (name-string (lexical-var-name v))
            (lexical-var-id v))))

(defun name-string (name)
  "A variable name -- a symbol or (setter symbol) -- as Dylan writes it."
  (if (consp name)
      (format nil "(setter ~a)" (dylan-string (cadr name)))
      (dylan-string name)))

;;; Nodes

(defstruct (node (:constructor nil))
  (location *location*))

(defstruct (constant (:include node) (:constructor make-constant (value)))
  value)

(defstruct (ref (:include node) (:constructor make-ref (var)))
  var)                                  ; LEXICAL-VAR or MODULE-VAR

(defstruct (assign (:include node) (:constructor make-assign (var value)))
  var
  value)

(defstruct (if-node (:include node) (:constructor make-if-node (test then else)))
  test then else)

(defstruct (seq-node (:include node) (:constructor %make-seq (forms)))
  forms)                                ; at least two

(defstruct (bind-node (:include node)
                      (:constructor make-bind-node (vars rest value body)))
  vars                                  ; LEXICAL-VARs for the first values
  rest                                  ; LEXICAL-VAR for the others, or NIL
  value                                 ; the expression
  body)

(defstruct (letrec-node (:include node)
                        (:constructor make-letrec-node (vars methods body)))
  vars
  methods                               ; METHOD-NODEs, one for each var
  body)

(defstruct (method-node (:include node) (:constructor %make-method-node))
  name                                  ; for messages and debugging
  required                              ; list of REQUIRED-PARAMs
  next                                  ; LEXICAL-VAR, or NIL
  rest                                  ; LEXICAL-VAR, or NIL
  keys                                  ; NIL (no #key), :ANY (#key alone),
                                        ; or a list of KEY-PARAMs
  body)

(defstruct (required-param (:constructor make-required-param (var specializer)))
  var
  specializer)                          ; node, or NIL for <object>

(defstruct (key-param (:constructor make-key-param (keyword var default)))
  keyword                               ; :FOO
  var
  default)                              ; node; #f when none was given

(defstruct (call (:include node) (:constructor make-call (function arguments)))
  function arguments)

(defstruct (primitive (:include node) (:constructor make-primitive (name arguments)))
  name                                  ; a keyword naming the operation
  arguments)

(defstruct (bind-exit-node (:include node)
                           (:constructor make-bind-exit-node (var body)))
  var body)

(defstruct (unwind-protect-node (:include node)
                                (:constructor make-unwind-protect-node
                                    (protected cleanup)))
  protected cleanup)

;;; Definitions

(defstruct (define-variable-node (:include node)
                                 (:constructor make-define-variable-node
                                     (var value)))
  var value)

(defstruct (define-generic-node (:include node)
                                (:constructor make-define-generic-node
                                    (var required rest keys)))
  var
  required                              ; the number of required parameters
  rest                                  ; does it take #rest?
  keys)                                 ; NIL, :ANY, or a list of keywords

(defstruct (define-method-node (:include node)
                               (:constructor make-define-method-node
                                   (var method)))
  var
  method)

(defstruct (define-class-node (:include node)
                              (:constructor make-define-class-node
                                  (var superclasses slots)))
  var
  superclasses                          ; nodes
  slots)                                ; SLOT-SPECs

(defstruct (define-slot-node (:include node)
                             (:constructor make-define-slot-node (owner slot)))
  owner                                 ; node: the class
  slot)

(defstruct (slot-spec (:constructor %make-slot-spec))
  getter                                ; MODULE-VAR
  setter                                ; MODULE-VAR, or NIL
  type                                  ; node, or NIL for <object>
  init-value                            ; node, or NIL
  init-function                         ; node, or NIL
  init-keyword                          ; keyword, or NIL
  required-init-keyword                 ; keyword, or NIL
  allocation                            ; :instance :class :each-subclass
                                        ; :constant or :virtual
  location)

;;; Constructors that simplify

(defun make-seq (forms)
  "A node evaluating FORMS in order.  Nested sequences are flattened, and
a constant or lexical variable reference whose value is discarded is
dropped."
  (let ((flat '()))
    (dolist (form forms)
      (if (seq-node-p form)
          (dolist (f (seq-node-forms form)) (push f flat))
          (push form flat)))
    (let* ((last (car flat))
           (effects (remove-if (lambda (f)
                                 (or (constant-p f)
                                     (and (ref-p f) (lexical-var-p (ref-var f)))))
                               (cdr flat))))
      (cond ((null flat) (make-constant dylan::dylan%false))
            ((null effects) last)
            (t (%make-seq (append (reverse effects) (list last))))))))

;;; Unparsing: a core form as an s-expression, for reading by people and
;;; for the tests.  Lexical variables print as name_id, module variables as
;;; name@module.

(defun var-string (var)
  (etypecase var
    (lexical-var (format nil "~a_~d" (name-string (lexical-var-name var))
                         (lexical-var-id var)))
    (module-var (module-var-string var))))

(defun var-symbol (var)
  (make-symbol (var-string var)))

(defun word (string)
  "STRING, to be printed as it is in an unparsed form."
  (make-symbol string))

(defun bool (x) (if x t dylan::dylan%false))

(defun unparse-constant (value)
  (let ((written (make-symbol (dylan-string value))))
    (if (or (consp value) (and (symbolp value) value (not (eq value t))
                               (not (keywordp value))))
        (list 'quote written)
        written)))

(defun unparse-string (node)
  "NODE as the text of an s-expression, in Dylan's spelling."
  (let ((*package* (find-package '#:dylan))
        (*print-escape* nil)
        (*print-case* :downcase)
        (*print-pretty* t)
        (*print-right-margin* 100))
    (princ-to-string (unparse node))))

(defun unparse (node)
  "NODE as an s-expression."
  (etypecase node
    (constant (unparse-constant (constant-value node)))
    (ref (var-symbol (ref-var node)))
    (assign `(dylan::set! ,(var-symbol (assign-var node)) ,(unparse (assign-value node))))
    (if-node `(if ,(unparse (if-node-test node))
                  ,(unparse (if-node-then node))
                  ,(unparse (if-node-else node))))
    (seq-node `(dylan::begin ,@(mapcar #'unparse (seq-node-forms node))))
    (bind-node `(dylan::bind ((,@(mapcar #'var-symbol (bind-node-vars node))
                        ,@(and (bind-node-rest node)
                               (list (word "#rest") (var-symbol (bind-node-rest node))))
                        ,(unparse (bind-node-value node))))
                  ,(unparse (bind-node-body node))))
    (letrec-node `(dylan::bind-methods
                   ,(mapcar (lambda (var method)
                              (list* (var-symbol var)
                                     (cdr (unparse method))))
                            (letrec-node-vars node) (letrec-node-methods node))
                   ,(unparse (letrec-node-body node))))
    (method-node `(method ,(unparse-parameters node)
                    ,(unparse (method-node-body node))))
    (call `(,(unparse (call-function node)) ,@(mapcar #'unparse (call-arguments node))))
    (primitive `(dylan::%primitive ,(word (string-downcase (primitive-name node)))
                            ,@(mapcar #'unparse (primitive-arguments node))))
    (bind-exit-node `(dylan::bind-exit (,(var-symbol (bind-exit-node-var node)))
                       ,(unparse (bind-exit-node-body node))))
    (unwind-protect-node `(unwind-protect ,(unparse (unwind-protect-node-protected node))
                            ,(unparse (unwind-protect-node-cleanup node))))
    (define-variable-node `(dylan::define ,(var-symbol (define-variable-node-var node))
                             ,(unparse (define-variable-node-value node))))
    (define-generic-node `(dylan::define-generic-function
                              ,(var-symbol (define-generic-node-var node))
                            ,(word "required:") ,(define-generic-node-required node)
                            ,(word "rest:") ,(unparse-constant (bool (define-generic-node-rest node)))
                            ,(word "keys:") ,(let ((keys (define-generic-node-keys node)))
                                                (if (eq keys :any)
                                                    (word "any")
                                                    (unparse-constant keys)))))
    (define-method-node `(dylan::define-method ,(var-symbol (define-method-node-var node))
                           ,@(cdr (unparse (define-method-node-method node)))))
    (define-class-node `(dylan::define-class ,(var-symbol (define-class-node-var node))
                          ,(mapcar #'unparse (define-class-node-superclasses node))
                          ,@(mapcar #'unparse-slot (define-class-node-slots node))))
    (define-slot-node `(dylan::define-slot ,(unparse (define-slot-node-owner node))
                         ,@(unparse-slot (define-slot-node-slot node))))))

(defun unparse-parameters (method)
  (append
   (mapcar (lambda (p)
             (if (required-param-specializer p)
                 (list (var-symbol (required-param-var p))
                       (unparse (required-param-specializer p)))
                 (var-symbol (required-param-var p))))
           (method-node-required method))
   (and (method-node-next method)
        (list (word "#next") (var-symbol (method-node-next method))))
   (and (method-node-rest method)
        (list (word "#rest") (var-symbol (method-node-rest method))))
   (let ((keys (method-node-keys method)))
     (cond ((null keys) '())
           ((eq keys :any) (list (word "#key")))
           (t (cons (word "#key")
                    (mapcar (lambda (k)
                              (list (unparse-constant (key-param-keyword k))
                                    (var-symbol (key-param-var k))
                                    (unparse (key-param-default k))))
                            keys)))))))

(defun unparse-slot (slot)
  `(,(var-symbol (slot-spec-getter slot))
    ,@(and (slot-spec-setter slot)
           (list (word "setter:") (var-symbol (slot-spec-setter slot))))
    ,@(and (slot-spec-type slot) (list (word "type:") (unparse (slot-spec-type slot))))
    ,@(and (slot-spec-init-value slot)
           (list (word "init-value:") (unparse (slot-spec-init-value slot))))
    ,@(and (slot-spec-init-function slot)
           (list (word "init-function:") (unparse (slot-spec-init-function slot))))
    ,@(and (slot-spec-init-keyword slot)
           (list (word "init-keyword:") (unparse-constant (slot-spec-init-keyword slot))))
    ,@(and (slot-spec-required-init-keyword slot)
           (list (word "required-init-keyword:")
                 (unparse-constant (slot-spec-required-init-keyword slot))))
    ,@(and (not (eq (slot-spec-allocation slot) :instance))
           (list (word "allocation:")
                 (word (string-downcase (slot-spec-allocation slot)))))))
