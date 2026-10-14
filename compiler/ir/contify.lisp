;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Contification (CompilerPlan.md, Phase 4): local functions of a letrec
;;;; whose every call is a jump -- from the letrec's body, or a tail call
;;;; from another of them -- are compiled as labelled blocks of the
;;;; function their letrec is in, their calls as goto.  The code generator
;;;; uses the groups decided here, and so does the lowering of exits
;;;; (control.lisp): an exit called from such a function is called from the
;;;; function that binds it.

(in-package #:dylan-compiler)

(defstruct (group (:constructor make-group (letrec fns mode entry)))
  letrec
  fns                                   ; the contified IR-FNs
  mode                                  ; :TAIL or :SINGLE
  entry                                 ; :SINGLE: the one entering call
  dest                                  ; where their values go
  (labels (make-hash-table :test #'eq))) ; fn -> entry label

(defvar *groups* nil "IR-FN -> its GROUP, for contified fns.")

(defun direct-calls (e)
  "The calls in E outside any function E contains, each as (call . tail-p),
TAIL-P meaning in tail position with respect to E."
  (let ((calls '()))
    (labels ((walk (e tail)
               (typecase e
                 (ir-call (push (cons e tail) calls))
                 (ir-let (walk (ir-let-value e) nil) (walk (ir-let-body e) tail))
                 (ir-mv-let (walk (ir-mv-let-value e) nil) (walk (ir-mv-let-body e) tail))
                 (ir-seq (loop for (f . more) on (ir-seq-forms e)
                               do (walk f (and tail (null more)))))
                 (ir-if (walk (ir-if-then e) tail) (walk (ir-if-else e) tail))
                 (ir-letrec (walk (ir-letrec-body e) tail))
                 (ir-bind-exit (walk (ir-bind-exit-body e) nil))
                 (ir-unwind (walk (ir-unwind-protected e) nil)
                            (walk (ir-unwind-cleanup e) nil)))))
      (walk e t))
    calls))

(defun letrecs-in (e)
  "The letrec forms in E outside any function E contains."
  (let ((found '()))
    (labels ((walk (e)
               (typecase e
                 (ir-let (walk (ir-let-value e)) (walk (ir-let-body e)))
                 (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (push e found) (walk (ir-letrec-body e)))
                 (ir-bind-exit (walk (ir-bind-exit-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e))))))
      (walk e))
    found))

(defun call-target (call)
  (let ((f (ir-call-function call)))
    (and (ivar-p f) (ivar-value-fn f))))

(defun simple-fn-p (fn)
  "Only required parameters, none specialized?"
  (and (null (ir-fn-next fn)) (null (ir-fn-rest fn)) (null (ir-fn-keys fn))
       (every #'null (ir-fn-specializers fn))))

(defun decide-contification (letrec)
  (let* ((arity-ok (lambda (fn)
                     (every (lambda (c) (= (length (ir-call-arguments c))
                                           (length (ir-fn-required fn))))
                            (ir-fn-call-sites fn))))
         (candidates (remove-if-not
                      (lambda (fn)
                        (and (not (ir-fn-escapes-p fn))
                             (simple-fn-p fn)
                             (funcall arity-ok fn)))
                      (ir-letrec-fns letrec)))
         (body-calls (make-hash-table :test #'eq))
         (fn-calls (make-hash-table :test #'eq)))  ; call -> (fn . tail-p)
    (loop for (c . tail) in (direct-calls (ir-letrec-body letrec))
          do (setf (gethash c body-calls) tail))
    (dolist (fn candidates)
      (loop for (c . tail) in (direct-calls (ir-fn-body fn))
            do (setf (gethash c fn-calls) (cons fn tail))))
    ;; Keep only those every call of which is in the letrec's body or a
    ;; tail call in a member's body.
    (loop
      (let ((before (length candidates)))
        (setf candidates
              (remove-if-not
               (lambda (fn)
                 (every (lambda (c)
                          (or (nth-value 1 (gethash c body-calls))
                              (let ((owner (gethash c fn-calls)))
                                (and owner (member (car owner) candidates)
                                     (cdr owner)))))
                        (ir-fn-call-sites fn)))
               candidates))
        (when (= before (length candidates)) (return))))
    (when candidates
      (let ((entries (loop for c being the hash-keys of body-calls using (hash-value tail)
                           when (member (call-target c) candidates)
                             collect (cons c tail))))
        (let ((group (cond ((every #'cdr entries)
                            (make-group letrec candidates :tail nil))
                           ((= (length entries) 1)
                            (make-group letrec candidates :single (car (first entries)))))))
          (when group
            (dolist (fn candidates)
              (setf (gethash fn *groups*) group))))))))

