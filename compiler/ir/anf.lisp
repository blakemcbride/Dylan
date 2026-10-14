;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Conversion of core forms (front/core.lisp) to the IR (ir.lisp).
;;;;
;;;; NORM converts a core node to an IR expression.  NORM-ATOM converts
;;;; one to an atom, naming the expression's value with a LET when it is
;;;; not one already, and passes the atom to a continuation that makes the
;;;; expression using it.  Operands are converted left to right, so the
;;;; IR evaluates them in the order the source does.
;;;;
;;;; A variable that is assigned somewhere is copied into a temporary
;;;; when it is used as an operand: the operands after it might assign it,
;;;; and the operation must see the value it had when it was evaluated.
;;;; Module variables are read by an IR-GLOBAL-REF, also into a temporary,
;;;; for the same reason.

(in-package #:dylan-compiler)

(defvar *var-map* nil "LEXICAL-VAR -> IVAR.")

(defun bind-ivar (lexical-var kind)
  "The IVAR for a binding of LEXICAL-VAR."
  (let ((var (make-ivar (lexical-var-name lexical-var) kind)))
    (setf (ivar-assigned-p var) (lexical-var-assigned-p lexical-var))
    (let ((type (lexical-var-type lexical-var)))
      (when type
        (setf (ivar-declared-type var) (lookup-ivar type))))
    (setf (gethash lexical-var *var-map*) var)))

(defun lookup-ivar (lexical-var)
  (or (gethash lexical-var *var-map*)
      (error "No IR variable for ~a." lexical-var)))

(defun make-temp (&optional (name "t"))
  (make-ivar (make-symbol name) :temp))

;;; Bindings.  A let whose value is itself a let (or a sequence) is
;;; turned inside out, so the IR is a straight chain of bindings:
;;;
;;;   (let ((a (let ((b e1)) e2))) body)  =>  (let ((b e1)) (let ((a e2)) body))
;;;   (let ((a (begin e1 e2))) body)      =>  (begin e1 (let ((a e2)) body))
;;;
;;; Every variable is bound once, so moving a binding out cannot capture
;;; anything.

(defun make-let (var value body)
  (typecase value
    (ir-let (make-ir-let (ir-let-var value) (ir-let-value value)
                         (make-let var (ir-let-body value) body)))
    (ir-mv-let (make-ir-mv-let (ir-mv-let-vars value) (ir-mv-let-rest value)
                               (ir-mv-let-value value)
                               (make-let var (ir-mv-let-body value) body)))
    (ir-seq (let ((forms (ir-seq-forms value)))
              (make-ir-seq (append (butlast forms)
                                   (list (make-let var (car (last forms)) body))))))
    (t (make-ir-let var value body))))

(defun make-mv-let (vars rest value body)
  (typecase value
    (ir-let (make-ir-let (ir-let-var value) (ir-let-value value)
                         (make-mv-let vars rest (ir-let-body value) body)))
    (ir-mv-let (make-ir-mv-let (ir-mv-let-vars value) (ir-mv-let-rest value)
                               (ir-mv-let-value value)
                               (make-mv-let vars rest (ir-mv-let-body value) body)))
    (ir-seq (let ((forms (ir-seq-forms value)))
              (make-ir-seq (append (butlast forms)
                                   (list (make-mv-let vars rest (car (last forms))
                                                      body))))))
    (t (make-ir-mv-let vars rest value body))))

;;; Atoms

(defun norm-atom (node k)
  "Convert NODE to an atom and answer the expression K makes of it."
  (let ((e (norm node)))
    (if (and (atom-p e)
             (not (and (ivar-p e) (ivar-assigned-p e))))
        (funcall k e)
        (let ((temp (make-temp)))
          (make-let temp e (funcall k temp))))))

(defun norm-atoms (nodes k)
  "Convert NODES to atoms, left to right, and answer the expression K
makes of the list of them."
  (if (null nodes)
      (funcall k '())
      (norm-atom (car nodes)
                 (lambda (a)
                   (norm-atoms (cdr nodes)
                               (lambda (as) (funcall k (cons a as))))))))

(defun norm-optional-atoms (nodes k)
  "As NORM-ATOMS, but an element of NODES may be NIL, which stays NIL."
  (norm-atoms (remove nil nodes)
              (lambda (atoms)
                (funcall k (mapcar (lambda (n) (and n (pop atoms))) nodes)))))

;;; Expressions

(defun norm (node)
  (let ((*location* (or (node-location node) *location*)))
    (etypecase node
      (constant (make-const (constant-value node)))
      (ref (let ((var (ref-var node)))
             (if (lexical-var-p var)
                 (lookup-ivar var)
                 (make-ir-global-ref var))))
      (assign
       (norm-atom (assign-value node)
                  (lambda (a)
                    (let ((var (assign-var node)))
                      (if (lexical-var-p var)
                          (make-ir-set (lookup-ivar var) a)
                          (make-ir-global-set var a))))))
      (if-node
       (norm-atom (if-node-test node)
                  (lambda (test)
                    (make-ir-if test
                                (norm (if-node-then node))
                                (norm (if-node-else node))))))
      (seq-node (make-ir-seq (mapcar #'norm (seq-node-forms node))))
      (bind-node
       (let ((value (norm (bind-node-value node))))
         (if (and (null (bind-node-rest node))
                  (= (length (bind-node-vars node)) 1))
             (let ((var (bind-ivar (first (bind-node-vars node)) :let)))
               (make-let var value (norm (bind-node-body node))))
             (let ((vars (mapcar (lambda (v) (bind-ivar v :let))
                                 (bind-node-vars node)))
                   (rest (and (bind-node-rest node)
                              (bind-ivar (bind-node-rest node) :let))))
               (make-mv-let vars rest value (norm (bind-node-body node)))))))
      (letrec-node (norm-letrec node))
      (method-node
       (norm-method node :lambda (method-node-name node)
                    (lambda (fn) (make-ir-lambda fn))))
      (call
       (norm-atom (call-function node)
                  (lambda (f)
                    (norm-atoms (call-arguments node)
                                (lambda (args) (make-ir-call f args))))))
      (primitive
       (norm-atoms (primitive-arguments node)
                   (lambda (args) (make-ir-prim (primitive-name node) args))))
      (bind-exit-node
       (let ((var (bind-ivar (bind-exit-node-var node) :exit)))
         (make-ir-bind-exit var (norm (bind-exit-node-body node)))))
      (unwind-protect-node
       (make-ir-unwind (norm (unwind-protect-node-protected node))
                       (norm (unwind-protect-node-cleanup node))))
      (define-variable-node
       (norm-atom (define-variable-node-value node)
                  (lambda (a)
                    (make-ir-define-variable (define-variable-node-var node) a))))
      (define-generic-node
       (make-ir-define-generic (define-generic-node-var node)
                               (define-generic-node-required node)
                               (define-generic-node-rest node)
                               (define-generic-node-keys node)))
      (define-method-node
       (norm-method (define-method-node-method node) :method
                    (module-var-name (define-method-node-var node))
                    (lambda (fn)
                      (make-ir-define-method (define-method-node-var node) fn))))
      (define-class-node
       (norm-atoms (define-class-node-superclasses node)
                   (lambda (supers)
                     (norm-slots (define-class-node-slots node)
                                 (lambda (slots)
                                   (make-ir-define-class
                                    (define-class-node-var node) supers slots))))))
      (define-slot-node
       (norm-atom (define-slot-node-owner node)
                  (lambda (owner)
                    (norm-slots (list (define-slot-node-slot node))
                                (lambda (slots)
                                  (make-ir-define-slot owner (first slots))))))))))

(defun norm-slots (slots k)
  "SLOTS with their type, init-value and init-function made atoms."
  (if (null slots)
      (funcall k '())
      (let ((slot (car slots)))
        (norm-optional-atoms
         (list (slot-spec-type slot) (slot-spec-init-value slot)
               (slot-spec-init-function slot))
         (lambda (atoms)
           (let ((new (copy-slot-spec slot)))
             (setf (slot-spec-type new) (first atoms)
                   (slot-spec-init-value new) (second atoms)
                   (slot-spec-init-function new) (third atoms))
             (norm-slots (cdr slots)
                         (lambda (rest) (funcall k (cons new rest))))))))))

;;; Methods.  The specializers are evaluated where the method is made, so
;;; they are atoms of the enclosing function; the parameters, the keyword
;;; defaults and the body belong to the new one.

(defun norm-method (method kind name k)
  (let ((*location* (or (node-location method) *location*)))
    (norm-optional-atoms
     (mapcar #'required-param-specializer (method-node-required method))
     (lambda (specializers)
       (funcall k (norm-method-fn method kind name specializers))))))

(defun norm-method-fn (method kind name specializers)
  (let* ((fn (make-ir-fn kind name))
         (*fn* fn))
    (setf (ir-fn-required fn)
          (mapcar (lambda (p) (bind-ivar (required-param-var p) :param))
                  (method-node-required method))
          (ir-fn-specializers fn) specializers
          (ir-fn-next fn) (and (method-node-next method)
                               (bind-ivar (method-node-next method) :next))
          (ir-fn-rest fn) (and (method-node-rest method)
                               (bind-ivar (method-node-rest method) :rest))
          (ir-fn-keys fn)
          (let ((keys (method-node-keys method)))
            (if (listp keys)
                ;; Each default is converted before its variable is bound:
                ;; it sees only the parameters before it.
                (mapcar (lambda (k)
                          (let ((default (norm (key-param-default k))))
                            (make-ir-key (key-param-keyword k)
                                         (bind-ivar (key-param-var k) :key)
                                         default)))
                        keys)
                keys))
          (ir-fn-body fn) (norm (method-node-body method)))
    fn))

;;; Local methods.  Their specializers are evaluated before any of them
;;; is made.

(defun norm-letrec (node)
  (let ((methods (letrec-node-methods node)))
    (norm-optional-atoms
     (loop for m in methods
           append (mapcar #'required-param-specializer (method-node-required m)))
     (lambda (specializers)
       (let ((vars (mapcar (lambda (v) (bind-ivar v :letrec))
                           (letrec-node-vars node))))
         (make-ir-letrec
          vars
          (mapcar (lambda (m var)
                    (let ((n (length (method-node-required m))))
                      (prog1 (norm-method-fn m :local (ivar-name var)
                                             (subseq specializers 0 n))
                        (setf specializers (nthcdr n specializers)))))
                  methods vars)
          (norm (letrec-node-body node))))))))

;;; Programs

(defun to-ir (program)
  "The IR of PROGRAM, a front-end PROGRAM without errors, analyzed."
  (let ((*var-map* (make-hash-table :test #'eq))
        (*ivar-counter* 0)
        (*fn-counter* 0)
        (library-counters nil)
        (in-library nil)
        (toplevels '()))
    (dolist (unit (program-units program))
      (let ((library (library-source-file-p (unit-file unit))))
        (when (and in-library (not library) (not library-counters))
          (setf library-counters (cons *ivar-counter* *fn-counter*))
          (incf *ivar-counter* +library-id-gap+)
          (incf *fn-counter* +library-id-gap+))
        (setf in-library library))
      (let ((*source-file* (unit-file unit)))
        (dolist (node (unit-forms unit))
          (let* ((*location* (node-location node))
                 (fn (let ((*fn* nil)) (make-ir-fn :toplevel nil)))
                 (*fn* fn))
            (setf (ir-fn-body fn) (norm node))
            (push fn toplevels)))))
    (let ((ir (make-ir-program (nreverse toplevels) program library-counters)))
      (analyze-program ir)
      ir)))
