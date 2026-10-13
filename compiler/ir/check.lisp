;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; A check of the IR's invariants, for the tests and for debugging later
;;;; phases that transform the IR:
;;;;
;;;;   - every operand is an atom, and a let's value is not itself a let,
;;;;     mv-let or sequence (the chain of bindings is flat);
;;;;   - every variable is bound exactly once, and used only where bound;
;;;;   - a variable used by a function other than the one binding it is
;;;;     marked captured, and is free in that function and in every one
;;;;     between it and the binder;
;;;;   - every call of an exit procedure is marked as one.
;;;;
;;;; Also: walkers that collect every variable and function of a program.

(in-package #:dylan-compiler)

(defun ir-violation (control &rest arguments)
  (error "IR check: ~?" control arguments))

(defun check-ir (ir)
  "Signal an error if IR breaks an invariant; else answer T."
  (let ((bound (make-hash-table :test #'eq)))
    (labels
        ((bind (var fn scope)
           (when (gethash var bound)
             (ir-violation "~a is bound twice" var))
           (unless (eq (ivar-binder var) fn)
             (ir-violation "~a is bound in ~a but says ~a" var fn (ivar-binder var)))
           (setf (gethash var bound) t)
           (cons var scope))
         (use (var fn scope)
           (unless (member var scope)
             (ir-violation "~a is used where it is not bound" var))
           (unless (eq fn (ivar-binder var))
             (unless (ivar-captured-p var)
               (ir-violation "~a is used by ~a but not marked captured" var fn))
             (loop for f = fn then (ir-fn-parent f)
                   until (eq f (ivar-binder var))
                   do (when (null f)
                        (ir-violation "~a: binder is not an enclosing fn" var))
                      (unless (member var (ir-fn-free-vars f))
                        (ir-violation "~a is not free in ~a" var f)))))
         (operand (a fn scope)
           (unless (atom-p a)
             (ir-violation "operand ~a is not an atom" a))
           (when (ivar-p a) (use a fn scope)))
         (fn (f scope)
           (dolist (v (ir-fn-required f)) (setf scope (bind v f scope)))
           (when (ir-fn-next f) (setf scope (bind (ir-fn-next f) f scope)))
           (when (ir-fn-rest f) (setf scope (bind (ir-fn-rest f) f scope)))
           (when (listp (ir-fn-keys f))
             (dolist (k (ir-fn-keys f))
               (expr (ir-key-default k) f scope)
               (setf scope (bind (ir-key-var k) f scope))))
           (expr (ir-fn-body f) f scope))
         (made-fn (made f scope)
           (unless (eq (ir-fn-parent made) f)
             (ir-violation "~a's parent is not ~a" made f))
           (dolist (s (ir-fn-specializers made))
             (when s (operand s f scope)))
           (fn made scope))
         (slot (s f scope)
           (dolist (a (list (slot-spec-type s) (slot-spec-init-value s)
                            (slot-spec-init-function s)))
             (when a (operand a f scope))))
         (expr (e f scope)
           (etypecase e
             (const)
             (ivar (use e f scope))
             (ir-let
              (when (typep (ir-let-value e) '(or ir-let ir-mv-let ir-seq))
                (ir-violation "a let's value is a ~a" (type-of (ir-let-value e))))
              (expr (ir-let-value e) f scope)
              (expr (ir-let-body e) f (bind (ir-let-var e) f scope)))
             (ir-mv-let
              (when (typep (ir-mv-let-value e) '(or ir-let ir-mv-let ir-seq))
                (ir-violation "an mv-let's value is a ~a" (type-of (ir-mv-let-value e))))
              (expr (ir-mv-let-value e) f scope)
              (dolist (v (ir-mv-let-vars e)) (setf scope (bind v f scope)))
              (when (ir-mv-let-rest e)
                (setf scope (bind (ir-mv-let-rest e) f scope)))
              (expr (ir-mv-let-body e) f scope))
             (ir-seq (dolist (x (ir-seq-forms e)) (expr x f scope)))
             (ir-if
              (operand (ir-if-test e) f scope)
              (expr (ir-if-then e) f scope)
              (expr (ir-if-else e) f scope))
             (ir-letrec
              (dolist (v (ir-letrec-vars e)) (setf scope (bind v f scope)))
              (dolist (m (ir-letrec-fns e)) (made-fn m f scope))
              (expr (ir-letrec-body e) f scope))
             (ir-lambda (made-fn (ir-lambda-fn e) f scope))
             (ir-call
              (operand (ir-call-function e) f scope)
              (dolist (a (ir-call-arguments e)) (operand a f scope))
              (let ((callee (ir-call-function e)))
                (when (and (ivar-p callee) (eq (ivar-kind callee) :exit)
                           (not (eq (ir-call-kind e) :exit)))
                  (ir-violation "a call of exit ~a is not marked" callee))))
             (ir-prim (dolist (a (ir-prim-arguments e)) (operand a f scope)))
             (ir-global-ref)
             (ir-global-set (operand (ir-global-set-value e) f scope))
             (ir-set
              (use (ir-set-var e) f scope)
              (unless (ivar-assigned-p (ir-set-var e))
                (ir-violation "~a is set but not marked assigned" (ir-set-var e)))
              (operand (ir-set-value e) f scope))
             (ir-bind-exit
              (expr (ir-bind-exit-body e) f (bind (ir-bind-exit-var e) f scope)))
             (ir-unwind
              (expr (ir-unwind-protected e) f scope)
              (expr (ir-unwind-cleanup e) f scope))
             (ir-define-variable (operand (ir-define-variable-value e) f scope))
             (ir-define-generic)
             (ir-define-method (made-fn (ir-define-method-fn e) f scope))
             (ir-define-class
              (dolist (a (ir-define-class-superclasses e)) (operand a f scope))
              (dolist (s (ir-define-class-slots e)) (slot s f scope)))
             (ir-define-slot
              (operand (ir-define-slot-owner e) f scope)
              (slot (ir-define-slot-slot e) f scope)))))
      (dolist (top (ir-program-toplevels ir))
        (fn top '())))
    t))

;;; Collecting

(defun map-ir-fns (function ir)
  "Call FUNCTION on every IR-FN of IR, outer before inner."
  (labels ((fn (f)
             (funcall function f)
             (when (listp (ir-fn-keys f))
               (dolist (k (ir-fn-keys f)) (walk (ir-key-default k))))
             (walk (ir-fn-body f)))
           (walk (e)
             (typecase e
               (ir-let (walk (ir-let-value e)) (walk (ir-let-body e)))
               (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
               (ir-seq (mapc #'walk (ir-seq-forms e)))
               (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
               (ir-letrec (mapc #'fn (ir-letrec-fns e)) (walk (ir-letrec-body e)))
               (ir-lambda (fn (ir-lambda-fn e)))
               (ir-bind-exit (walk (ir-bind-exit-body e)))
               (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e)))
               (ir-define-method (fn (ir-define-method-fn e))))))
    (mapc #'fn (ir-program-toplevels ir))))

(defun ir-fns (ir)
  (let ((fns '()))
    (map-ir-fns (lambda (f) (push f fns)) ir)
    (nreverse fns)))

(defun ir-bound-vars (ir)
  "Every variable IR binds, in order of binding."
  (let ((vars '()))
    (labels ((walk (e)
               (typecase e
                 (ir-let (walk (ir-let-value e))
                         (push (ir-let-var e) vars)
                         (walk (ir-let-body e)))
                 (ir-mv-let (walk (ir-mv-let-value e))
                            (dolist (v (ir-mv-let-vars e)) (push v vars))
                            (when (ir-mv-let-rest e) (push (ir-mv-let-rest e) vars))
                            (walk (ir-mv-let-body e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (dolist (v (ir-letrec-vars e)) (push v vars))
                            (walk (ir-letrec-body e)))
                 (ir-bind-exit (push (ir-bind-exit-var e) vars)
                               (walk (ir-bind-exit-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e))))))
      (dolist (f (ir-fns ir))
        (dolist (v (ir-fn-required f)) (push v vars))
        (when (ir-fn-next f) (push (ir-fn-next f) vars))
        (when (ir-fn-rest f) (push (ir-fn-rest f) vars))
        (when (listp (ir-fn-keys f))
          (dolist (k (ir-fn-keys f)) (walk (ir-key-default k)) (push (ir-key-var k) vars)))
        (walk (ir-fn-body f))))
    (nreverse vars)))
