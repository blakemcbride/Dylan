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
;;;; contains.  A method, besides, only when each class its specializers
;;;; name could have instances: a class the library or the program defines
;;;; has none unless it or a subclass is used as a value -- made, say --
;;;; by what is kept.  Naming a class as a specializer or superclass makes
;;;; no instance of it.

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

(defun type-only-ivars (top)
  "The variables of the top-level form TOP bound to a module variable's
value and used only to name a class: as a superclass of the class TOP
defines, as a specializer of the method it defines, or as the argument of
a singleton specializer.  Using a class that way makes no instance of it."
  (let ((uses (make-hash-table :test #'eq))
        (bound (make-hash-table :test #'eq))) ; ivar -> value
    (labels ((use (a) (when (ivar-p a) (incf (gethash a uses 0))))
             (walk (e)
               (typecase e
                 (ir-let (setf (gethash (ir-let-var e) bound) (ir-let-value e))
                         (walk (ir-let-body e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-define-class (mapc #'use (ir-define-class-superclasses e)))
                 (ir-define-method
                  (dolist (s (ir-fn-specializers (ir-define-method-fn e)))
                    (use s)
                    (let ((v (and (ivar-p s) (gethash s bound))))
                      (when (and (ir-call-p v) (= (length (ir-call-arguments v)) 1))
                        (use (first (ir-call-arguments v))))))))))
      (walk (ir-fn-body top))
      (let ((only '()))
        (maphash (lambda (var n)
                   (when (and (ir-global-ref-p (gethash var bound))
                              (= n (ivar-ref-count var)))
                     (push var only)))
                 uses)
        only))))

(defun method-classes (top)
  "The module variables naming the classes a method's specializers require
an instance of, for the library's top-level form TOP defining a method:
(singleton C), where C names a class, needs one too, or the class could
not be had to be the singleton's object, but by naming it."
  (let ((bound (make-hash-table :test #'eq))
        (classes '()))
    (labels ((global (a)
               (let ((v (and (ivar-p a) (gethash a bound))))
                 (and (ir-global-ref-p v) (ir-global-ref-var v))))
             (walk (e)
               (typecase e
                 (ir-let (setf (gethash (ir-let-var e) bound) (ir-let-value e))
                         (walk (ir-let-body e)))
                 (ir-define-method
                  (dolist (s (ir-fn-specializers (ir-define-method-fn e)))
                    (let ((v (and (ivar-p s) (gethash s bound))))
                      (cond ((ir-global-ref-p v) (push (ir-global-ref-var v) classes))
                            ((and (ir-call-p v)
                                  (eq (global (ir-call-function v)) (dylan-var "singleton"))
                                  (= (length (ir-call-arguments v)) 1)
                                  (global (first (ir-call-arguments v))))
                             (push (global (first (ir-call-arguments v))) classes)))))))))
      (walk (ir-fn-body top))
      classes)))

(defun class-definition (top)
  "If the top-level form TOP defines a class: its module variable, and those
of its superclasses."
  (let ((bound (make-hash-table :test #'eq)))
    (labels ((walk (e)
               (typecase e
                 (ir-let (setf (gethash (ir-let-var e) bound) (ir-let-value e))
                         (walk (ir-let-body e)))
                 (ir-define-class
                  (return-from class-definition
                    (values (ir-define-class-var e)
                            (loop for a in (ir-define-class-superclasses e)
                                  for v = (and (ivar-p a) (gethash a bound))
                                  when (ir-global-ref-p v)
                                    collect (ir-global-ref-var v))))))))
      (walk (ir-fn-body top))
      nil)))

(defun referenced-variables (fn &optional type-only)
  "The module variables FN, or a function in it, refers to: in two lists,
those it uses as values and those it uses only to name a class (bound to
one of the TYPE-ONLY variables)."
  (let ((vars '()) (types '()))
    (labels ((walk-fn (f)
               (when (listp (ir-fn-keys f))
                 (dolist (k (ir-fn-keys f)) (walk (ir-key-default k))))
               (walk (ir-fn-body f)))
             (walk (e)
               (typecase e
                 (ir-global-ref (push (ir-global-ref-var e) vars))
                 (ir-global-set (push (ir-global-set-var e) vars))
                 (ir-let (if (and (ir-global-ref-p (ir-let-value e))
                                  (member (ir-let-var e) type-only))
                             (push (ir-global-ref-var (ir-let-value e)) types)
                             (walk (ir-let-value e)))
                         (walk (ir-let-body e)))
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
    (values vars types)))

(defun shake-library (ir)
  "Drop from IR the library's definitions the program cannot reach.  A
method of the library is kept when its generic function is reached and
each class its specializers name could have an instance: the class is not
one the library or program defines, or it or a subclass is used as a
value by what is kept (made, say, or answered), not only named as a
specializer or superclass -- or the program may find classes by
direct-subclasses."
  (let ((definers (make-hash-table :test #'eq)) ; var -> library forms defining it
        (needs (make-hash-table :test #'eq))    ; method form -> class vars
        (superclasses (make-hash-table :test #'eq)) ; class var -> its superclasses
        (possible (make-hash-table :test #'eq)) ; class vars that may have instances
        (kept (make-hash-table :test #'eq))
        (reached (make-hash-table :test #'eq))
        (waiting '())                           ; method forms reached, not kept
        (all-possible nil)                      ; may any class be found?
        (subclasses nil)                        ; direct-subclasses
        (work '()))
    (let ((*dylan-modules* (program-modules (ir-program-source ir))))
      (dolist (top (ir-program-toplevels ir))
        (multiple-value-bind (class supers) (class-definition top)
          (when class (setf (gethash class superclasses) supers)))
        (let ((var (and (library-form-p top) (defined-variable top))))
          (if var
              (progn (push top (gethash var definers))
                     (let ((classes (method-classes top)))
                       (when classes (setf (gethash top needs) classes))))
              (push top work))))
      ;; What the code generator refers to itself
      (dolist (name '("initialize"))
        (push (dylan-var name) work))
      (setf subclasses (dylan-var "direct-subclasses")))
    (labels ((make-possible (class)
               (unless (gethash class possible)
                 (setf (gethash class possible) t)
                 (mapc #'make-possible (gethash class superclasses))))
             (can-have-instances-p (class)
               (or all-possible
                   (gethash class possible)
                   (not (nth-value 1 (gethash class superclasses)))))
             (ready-p (top)
               (every #'can-have-instances-p (gethash top needs))))
      (loop
        (loop while work
              do (let ((item (pop work)))
                   (if (ir-fn-p item)
                       (unless (gethash item kept)
                         (setf (gethash item kept) t)
                         (multiple-value-bind (vars types)
                             (referenced-variables item (type-only-ivars item))
                           (dolist (var vars)
                             (make-possible var)
                             (push var work))
                           (dolist (var types)
                             (push var work))))
                       (unless (gethash item reached)
                         (setf (gethash item reached) t)
                         ;; Any class a class could lead to
                         (when (eq item subclasses) (setf all-possible t))
                         (dolist (top (gethash item definers))
                           (if (ready-p top)
                               (push top work)
                               (push top waiting)))))))
        (let ((ready (remove-if-not #'ready-p waiting)))
          (unless ready (return))
          (setf waiting (set-difference waiting ready))
          (setf work ready))))
    (setf (ir-program-toplevels ir)
          (remove-if-not (lambda (top) (gethash top kept)) (ir-program-toplevels ir)))
    ir))
