;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Tree shaking (CompilerPlan.md, Phase 9, done early): a program
;;;; compiled with the library, not apart from it (*SEPARATE-LIBRARY*),
;;;; keeps only the library's definitions it can reach.
;;;;
;;;; The program's own top-level forms are all kept.  So is every top-level
;;;; form of the library that is not a definition, and every define-class
;;;; (a class is static data, cheap, and its slots' values are evaluated
;;;; where it is defined).  A library form defining a variable -- define,
;;;; define-generic-function, define-method -- is kept when the variable is
;;;; reached: referred to by a form kept, or by a function a kept form
;;;; contains.

(in-package #:dylan-compiler)

(defvar *separate-library* t
  "Compile the library apart from the program (CompilerPlan.md, Phase 9)?
Then nothing of the library is dropped, and nothing about how a library
function is compiled depends on the program: the program may call any
function the library exports, with any arguments.  So the library's C is
the same for every program, and the driver compiles it once.")

(defun library-form-p (top) (ir-fn-library-p top))

(defun open-library-var-p (var)
  "Must the code generator suppose the program calls the function in the
library's module variable VAR from anywhere, as the library is compiled
apart?  A program sees what the dylan module exports, and its expansions
other names the compiler knows (DYLAN-VAR)."
  (and *separate-library*
       (module-var-predefined-p var)))

(defun defined-variable (top)
  "The module variable the top-level form TOP defines, if defining it is all
it does; else NIL."
  (labels ((find-definition (e)
             (typecase e
               (ir-let (find-definition (ir-let-body e)))
               (ir-define-variable (ir-define-variable-var e))
               (ir-define-generic (ir-define-generic-var e))
               (ir-define-method (ir-define-method-var e)))))
    (find-definition (ir-fn-body top))))

(defun referenced-variables (fn)
  "The module variables FN, or a function in it, refers to."
  (let ((vars '()))
    (labels ((walk-fn (f)
               (when (listp (ir-fn-keys f))
                 (dolist (k (ir-fn-keys f)) (walk (ir-key-default k))))
               (walk (ir-fn-body f)))
             (walk (e)
               (typecase e
                 (ir-global-ref (push (ir-global-ref-var e) vars))
                 (ir-global-set (push (ir-global-set-var e) vars))
                 (ir-let (walk (ir-let-value e)) (walk (ir-let-body e)))
                 (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (mapc #'walk-fn (ir-letrec-fns e)) (walk (ir-letrec-body e)))
                 (ir-lambda (walk-fn (ir-lambda-fn e)))
                 (ir-bind-exit (walk (ir-bind-exit-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e)))
                 (ir-define-method (walk-fn (ir-define-method-fn e)))
                 (ir-define-class
                  (dolist (s (ir-define-class-slots e))
                    (push (slot-spec-getter s) vars)
                    (when (slot-spec-setter s) (push (slot-spec-setter s) vars)))))))
      (walk-fn fn))
    vars))

(defun shake-library (ir)
  "Drop from IR the library's definitions the program cannot reach."
  (let ((definers (make-hash-table :test #'eq)) ; var -> library forms defining it
        (kept (make-hash-table :test #'eq))
        (reached (make-hash-table :test #'eq))
        (work '()))
    (dolist (top (ir-program-toplevels ir))
      (let ((var (and (library-form-p top) (defined-variable top))))
        (if var
            (push top (gethash var definers))
            (push top work))))
    ;; What the code generator refers to itself
    (let ((*dylan-modules* (program-modules (ir-program-source ir))))
      (dolist (name '("initialize"))
        (push (dylan-var name) work)))
    (loop while work
          do (let ((item (pop work)))
               (if (ir-fn-p item)
                   (unless (gethash item kept)
                     (setf (gethash item kept) t)
                     (dolist (var (referenced-variables item))
                       (push var work)))
                   (unless (gethash item reached)
                     (setf (gethash item reached) t)
                     (dolist (top (gethash item definers))
                       (push top work))))))
    (setf (ir-program-toplevels ir)
          (remove-if-not (lambda (top) (gethash top kept)) (ir-program-toplevels ir)))
    ir))
