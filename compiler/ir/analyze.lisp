;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Analysis of the IR: what later phases need to know about variables,
;;;; calls, functions and exits.  Two walks over each top-level form.
;;;;
;;;; The first records, for each variable, its references and calls, and
;;;; whether a function other than the one binding it uses it (CAPTURED-P,
;;;; with the variable added to the FREE-VARS of every function between);
;;;; for each function bound to a variable that is never assigned, the
;;;; calls of it, or that it ESCAPES-P because it is used some other way;
;;;; and for each exit procedure whether it ESCAPES-P -- is used other
;;;; than by being called directly in the function that binds it.  A
;;;; method of a generic function, and a function that is not bound to a
;;;; variable, escapes.
;;;;
;;;; The second marks the calls in tail position.  A call is in tail
;;;; position when its value is the value of its function: the last form
;;;; of a body, a branch of an if in tail position, the body of a let.
;;;; The protected form and cleanup of an unwind-protect are not, and
;;;; neither is the body of a bind-exit whose exit escapes, whose frame
;;;; must stay until the body returns (CompilerPlan.md, Phase 4).  An exit
;;;; that does not escape becomes a jump, and leaves no frame.

(in-package #:dylan-compiler)

(defun analyze-program (ir)
  (dolist (top (ir-program-toplevels ir))
    (scan-fn top))
  (dolist (top (ir-program-toplevels ir))
    (mark-tail (ir-fn-body top) t)))

;;; First walk

(defun note-use (var fn how &optional call)
  "Record that FN uses VAR: HOW is :VALUE, :CALL (as the function CALL
calls) or :SET."
  (unless (eq how :set)
    (incf (ivar-ref-count var)))
  (when (eq how :call)
    (incf (ivar-call-count var)))
  (unless (eq fn (ivar-binder var))
    (setf (ivar-captured-p var) t)
    (loop for f = fn then (ir-fn-parent f)
          until (or (null f) (eq f (ivar-binder var)))
          do (pushnew var (ir-fn-free-vars f))))
  (let ((target (ivar-value-fn var)))
    (case how
      (:call
       (when target
         (push call (ir-fn-call-sites target))
         (setf (ir-call-kind call) :known))
       (when (eq (ivar-kind var) :exit)
         (setf (ir-call-kind call) :exit)
         (unless (eq fn (ivar-binder var))
           (setf (ivar-escapes-p var) t))))
      (:value
       (when target (setf (ir-fn-escapes-p target) t))
       (when (eq (ivar-kind var) :exit)
         (setf (ivar-escapes-p var) t))))))

(defun scan-atom (a fn)
  (when (ivar-p a) (note-use a fn :value)))

(defun bind-fn (var fn)
  "VAR is bound to the function FN.  If VAR is never assigned, the calls
of VAR are calls of FN."
  (setf (ir-fn-binder fn) var)
  (unless (ivar-assigned-p var)
    (setf (ivar-value-fn var) fn)))

(defun scan-made-fn (made fn)
  "Scan MADE, a function FN makes.  Its specializers are atoms of FN,
evaluated when it is made."
  (dolist (s (ir-fn-specializers made))
    (when s (scan-atom s fn)))
  (scan-fn made))

(defun scan-fn (fn)
  (let ((*fn* fn))
    (when (eq (ir-fn-kind fn) :method)
      (setf (ir-fn-escapes-p fn) t))
    (dolist (key (and (listp (ir-fn-keys fn)) (ir-fn-keys fn)))
      (scan (ir-key-default key) fn))
    (scan (ir-fn-body fn) fn)))

(defun scan-slot (slot fn)
  (dolist (a (list (slot-spec-type slot) (slot-spec-init-value slot)
                   (slot-spec-init-function slot)))
    (when a (scan-atom a fn))))

(defun scan (e fn)
  (etypecase e
    (const)
    (ivar (note-use e fn :value))
    (ir-let
     (let ((value (ir-let-value e)))
       (if (ir-lambda-p value)
           (progn (bind-fn (ir-let-var e) (ir-lambda-fn value))
                  (scan-made-fn (ir-lambda-fn value) fn))
           (scan value fn)))
     (scan (ir-let-body e) fn))
    (ir-mv-let
     (scan (ir-mv-let-value e) fn)
     (scan (ir-mv-let-body e) fn))
    (ir-seq (dolist (f (ir-seq-forms e)) (scan f fn)))
    (ir-if
     (scan-atom (ir-if-test e) fn)
     (scan (ir-if-then e) fn)
     (scan (ir-if-else e) fn))
    (ir-letrec
     (loop for var in (ir-letrec-vars e)
           for f in (ir-letrec-fns e)
           do (bind-fn var f))
     (dolist (f (ir-letrec-fns e)) (scan-made-fn f fn))
     (scan (ir-letrec-body e) fn))
    (ir-lambda
     ;; Not bound to a variable: called from who knows where.
     (setf (ir-fn-escapes-p (ir-lambda-fn e)) t)
     (scan-made-fn (ir-lambda-fn e) fn))
    (ir-call
     (let ((f (ir-call-function e)))
       (when (ivar-p f) (note-use f fn :call e)))
     (dolist (a (ir-call-arguments e)) (scan-atom a fn)))
    (ir-prim (dolist (a (ir-prim-arguments e)) (scan-atom a fn)))
    (ir-global-ref)
    (ir-global-set (scan-atom (ir-global-set-value e) fn))
    (ir-set
     (note-use (ir-set-var e) fn :set)
     (scan-atom (ir-set-value e) fn))
    (ir-bind-exit (scan (ir-bind-exit-body e) fn))
    (ir-unwind
     (scan (ir-unwind-protected e) fn)
     (scan (ir-unwind-cleanup e) fn))
    (ir-define-variable (scan-atom (ir-define-variable-value e) fn))
    (ir-define-generic)
    (ir-define-method (scan-made-fn (ir-define-method-fn e) fn))
    (ir-define-class
     (dolist (a (ir-define-class-superclasses e)) (scan-atom a fn))
     (dolist (s (ir-define-class-slots e)) (scan-slot s fn)))
    (ir-define-slot
     (scan-atom (ir-define-slot-owner e) fn)
     (scan-slot (ir-define-slot-slot e) fn))))

;;; Second walk

(defun mark-fn (fn)
  (dolist (key (and (listp (ir-fn-keys fn)) (ir-fn-keys fn)))
    (mark-tail (ir-key-default key) nil))
  (mark-tail (ir-fn-body fn) t))

(defun mark-tail (e tail)
  (etypecase e
    ((or const ivar ir-global-ref ir-global-set ir-set ir-prim
         ir-define-variable ir-define-generic))
    (ir-let
     (mark-tail (ir-let-value e) nil)
     (mark-tail (ir-let-body e) tail))
    (ir-mv-let
     (mark-tail (ir-mv-let-value e) nil)
     (mark-tail (ir-mv-let-body e) tail))
    (ir-seq
     (loop for (f . more) on (ir-seq-forms e)
           do (mark-tail f (and tail (null more)))))
    (ir-if
     (mark-tail (ir-if-then e) tail)
     (mark-tail (ir-if-else e) tail))
    (ir-letrec
     (mapc #'mark-fn (ir-letrec-fns e))
     (mark-tail (ir-letrec-body e) tail))
    (ir-lambda (mark-fn (ir-lambda-fn e)))
    (ir-call (setf (ir-call-tail-p e) tail))
    (ir-bind-exit
     (mark-tail (ir-bind-exit-body e)
                (and tail (not (ivar-escapes-p (ir-bind-exit-var e))))))
    (ir-unwind
     (mark-tail (ir-unwind-protected e) nil)
     (mark-tail (ir-unwind-cleanup e) nil))
    (ir-define-method (mark-fn (ir-define-method-fn e)))
    ((or ir-define-class ir-define-slot))))
