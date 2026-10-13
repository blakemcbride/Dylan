;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Type inference and float unboxing (CompilerPlan.md, Phase 9).
;;;;
;;;; A variable's type is a class all its values are instances of, or NIL
;;;; for none known.  The types come from what binds and assigns the
;;;; variable: constants; the inline methods of calls.lisp, whose result
;;;; classes are known once their arguments' are; the specializers of a
;;;; method defined by define-method, which dispatch has checked; and, for
;;;; the parameters of a function whose every call is known (a loop, most
;;;; often), the arguments of those calls.  They are found together, from
;;;; nothing up, until none changes.
;;;;
;;;; The types let calls.lisp drop the tests of its inline cases.  A
;;;; <double-float> variable that no closure holds, and whose every use
;;;; wants a C double, is UNBOXED: a C double, which needs no allocation.

(in-package #:dylan-compiler)

(defun atom-type (a)
  "The class of the atom A's value, if known."
  (etypecase a
    (const (ignore-errors (constant-class (const-value a))))
    (ivar (let ((type (ivar-type a))) (and (class-info-p type) type)))
    (c-atom (c-atom-type a))))

(defun double-class () (find-class-info "<double-float>" *classes*))

(defun unboxed-p (var) (and *unboxed* (gethash var *unboxed*)))

(defun atom-double-c (a)
  "A C double expression for A, a <double-float>."
  (cond ((and (ivar-p a) (constant-atom a)) (atom-double-c (constant-atom a)))
        ((and (ivar-p a) (unboxed-p a)) (v-name a))
        ((and (c-atom-p a) (c-atom-double-p a)) (c-atom-expr a))
        ((and (const-p a) (floatp (const-value a)))
         (double-literal (const-value a)))
        (t (format nil "DY_DOUBLE(~a)" (atom-c a)))))

(defun double-literal (x)
  (let ((d (coerce x 'double-float)))
    (cond ((sb-ext:float-infinity-p d) (if (plusp d) "(1.0/0.0)" "(-1.0/0.0)"))
          ((sb-ext:float-nan-p d) "(0.0/0.0)")
          (t (let ((*read-default-float-format* 'double-float))
               (substitute #\e #\d (format nil "~,,,,,,'eE" d)))))))

;;; Calls known program-wide

(defun analyze-calls (fns)
  "Find the module variables that always hold one function with no free
variables (*global-functions*), and answer a table: IR-FN -> the argument
lists of all its calls, for the functions whose every call is known --
local functions that do not escape, global functions whose variable is
used only to call them, and the sole methods of generic functions used so."
  (let ((escapes (make-hash-table :test #'eq))     ; module var -> T
        (definitions (make-hash-table :test #'eq)) ; module var -> value atoms
        (assigned (make-hash-table :test #'eq))    ; module var -> T
        (binders (make-hash-table :test #'eq))     ; ivar -> module var
        (calls-of (make-hash-table :test #'eq))    ; ivar -> calls of it
        (generic-calls (make-hash-table :test #'eq)) ; gfun -> arg lists, or :unknown
        (callers (make-hash-table :test #'eq)))
    (labels ((walk-fn (fn)
               (when (listp (ir-fn-keys fn))
                 (dolist (k (ir-fn-keys fn)) (walk (ir-key-default k))))
               (walk (ir-fn-body fn)))
             (walk (e)
               (typecase e
                 (ir-global-ref (setf (gethash (ir-global-ref-var e) escapes) t))
                 (ir-let (if (ir-global-ref-p (ir-let-value e))
                             (setf (gethash (ir-let-var e) binders)
                                   (ir-global-ref-var (ir-let-value e)))
                             (walk (ir-let-value e)))
                         (walk (ir-let-body e)))
                 (ir-global-set (setf (gethash (ir-global-set-var e) assigned) t))
                 (ir-define-variable (push (ir-define-variable-value e)
                                           (gethash (ir-define-variable-var e) definitions)))
                 (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (walk (ir-letrec-body e)))
                 (ir-bind-exit (walk (ir-bind-exit-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e)))
                 (ir-call
                  (when (ivar-p (ir-call-function e))
                    (push e (gethash (ir-call-function e) calls-of)))
                  (multiple-value-bind (kind what swap) (call-callee e)
                    (case kind
                      (:generic
                       (unless (eq (gethash what generic-calls) :unknown)
                         (push (if swap (reverse (ir-call-arguments e)) (ir-call-arguments e))
                               (gethash what generic-calls))))
                      (:fold (setf (gethash what generic-calls) :unknown))))))))
      (mapc #'walk-fn fns)
      ;; A variable escapes if a binding of it is used other than to call.
      (loop for ivar being the hash-keys of binders using (hash-value var)
            unless (and (not (ivar-assigned-p ivar))
                        (= (ivar-ref-count ivar) (length (gethash ivar calls-of))))
              do (setf (gethash var escapes) t))
      ;; Global functions
      (loop for var being the hash-keys of definitions using (hash-value values)
            do (let* ((value (and (= (length values) 1) (ivar-p (first values))
                                  (not (ivar-assigned-p (first values)))
                                  (gethash (first values) *let-values*)))
                      (fn (and (ir-lambda-p value) (ir-lambda-fn value)))
                      (cfun (and fn (gethash fn *cfuns*))))
                 (when (and cfun (null (cfun-env cfun)) (not (gethash var assigned))
                            (not (module-var-thread-local-p var))
                            (= (length (module-var-definitions var)) 1))
                   (setf (gethash var *global-functions*) fn))))
      (flet ((arg-lists (calls fn)
               (if (every (lambda (args) (= (length args) (length (ir-fn-required fn)))) calls)
                   calls
                   :unknown)))
        (loop for var being the hash-keys of *global-functions* using (hash-value fn)
              unless (gethash var escapes)
                do (setf (gethash fn callers)
                         (arg-lists (loop for ivar being the hash-keys of binders
                                            using (hash-value v)
                                          when (eq v var)
                                            append (mapcar #'ir-call-arguments
                                                           (gethash ivar calls-of)))
                                    fn)))
        (unless *reflective-methods*
          (loop for g being the hash-values of *gfuns*
                for m = (sole-method g)
                when (and m (gmethod-fn m)
                          (not (gethash (gfun-var g) escapes))
                          (not (eq (gfun-var g) (dylan-var "initialize")))
                          (not (eq (gethash g generic-calls) :unknown)))
                  do (setf (gethash (gmethod-fn m) callers)
                           (arg-lists (gethash g generic-calls) (gmethod-fn m)))))
        (dolist (fn fns)
          (when (and (not (ir-fn-escapes-p fn)) (ir-fn-call-sites fn))
            (setf (gethash fn callers)
                  (arg-lists (mapcar #'ir-call-arguments (ir-fn-call-sites fn)) fn)))))
      callers)))

(defun global-callee (e)
  "The IR-FN of the global function the call E calls, if it calls one."
  (let ((v (static-value (ir-call-function e))))
    (and (eq (car v) :global) *global-functions*
         (gethash (second v) *global-functions*))))

(defun known-callee (e)
  "The IR-FN the call E always calls, if known."
  (or (call-target e)
      (global-callee e)
      (multiple-value-bind (kind what) (call-callee e)
        (and (eq kind :generic)
             (let ((m (sole-method what))) (and m (gmethod-fn m)))))))

;;; Inference

(defun join-types (a b)
  (cond ((eq a :none) b)
        ((eq b :none) a)
        ((eq a b) a)
        (t nil)))

(defun infer-types (fns)
  "Set the type of every variable of FNS."
  (let ((sources (make-hash-table :test #'eq))   ; ivar -> list of sources
        (types (make-hash-table :test #'eq))     ; ivar -> type during the search
        (results (make-hash-table :test #'eq))   ; ir-fn -> type of its value
        (callers (analyze-calls fns)))
    (labels ((source (var s) (push s (gethash var sources)))
             (unknown (var) (source var :unknown))
             (walk-fn (fn)
               (let ((method (gethash fn *method-of-fn*))
                     (sites (let ((c (gethash fn callers)))
                              (and (listp c) c (null (ir-fn-rest fn)) (null (ir-fn-keys fn))
                                   c))))
                 (loop for var in (ir-fn-required fn)
                       for spec in (ir-fn-specializers fn)
                       for i from 0
                       do (cond (sites
                                 (dolist (args sites)
                                   (source var (nth i args))))
                                ((and method spec (known-class spec))
                                 (source var (known-class spec)))
                                (t (unknown var))))
                 (when (ir-fn-next fn) (unknown (ir-fn-next fn)))
                 (when (ir-fn-rest fn) (unknown (ir-fn-rest fn)))
                 (when (listp (ir-fn-keys fn))
                   (dolist (k (ir-fn-keys fn))
                     (unknown (ir-key-var k))
                     (walk (ir-key-default k))))
                 (walk (ir-fn-body fn))))
             (walk (e)
               (typecase e
                 (ir-let (source (ir-let-var e) (ir-let-value e))
                         (walk (ir-let-value e)) (walk (ir-let-body e)))
                 (ir-mv-let (dolist (v (ir-mv-let-vars e)) (unknown v))
                            (when (ir-mv-let-rest e) (unknown (ir-mv-let-rest e)))
                            (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-set (source (ir-set-var e) (ir-set-value e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (mapc #'unknown (ir-letrec-vars e))
                            (walk (ir-letrec-body e)))
                 (ir-bind-exit (unknown (ir-bind-exit-var e))
                               (walk (ir-bind-exit-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e)))))
             (var-type (var)
               (multiple-value-bind (type found) (gethash var types)
                 (if found type nil)))
             (atom-type* (a)
               (if (ivar-p a) (var-type a) (atom-type a)))
             (expr-type (e)
               (typecase e
                 ((or const ivar) (atom-type* e))
                 (ir-let (expr-type (ir-let-body e)))
                 (ir-mv-let (expr-type (ir-mv-let-body e)))
                 (ir-letrec (expr-type (ir-letrec-body e)))
                 (ir-seq (expr-type (car (last (ir-seq-forms e)))))
                 (ir-if (join-types (expr-type (ir-if-then e)) (expr-type (ir-if-else e))))
                 (ir-set (atom-type* (ir-set-value e)))
                 (ir-global-set (atom-type* (ir-global-set-value e)))
                 (ir-prim (and (eq (ir-prim-name e) :pair) (find-class-info "<pair>" *classes*)))
                 (ir-call (call-type e))
                 (t nil)))
             (call-type (e)
               (if (eq (ir-call-kind e) :exit)
                   :none
                   (multiple-value-bind (kind what swap) (call-callee e)
                     (let* ((args (if swap (reverse (ir-call-arguments e)) (ir-call-arguments e)))
                            (type
                              (case kind
                                (:generic (step-type what (mapcar #'atom-type* args)))
                                (:fold (reduce (lambda (acc a)
                                                 (step-type what (list acc (atom-type* a))))
                                               (rest args)
                                               :initial-value (atom-type* (first args))))
                                (:primitive
                                 (and (member what '("id" "not" "instance_p" "pair_p")
                                              :test #'string=)
                                      (find-class-info "<boolean>" *classes*)))
                                (t nil)))
                            (callee (and (null type) (known-callee e))))
                       (if callee
                           (multiple-value-bind (r found) (gethash callee results)
                             (if found r nil))
                           type)))))
             (step-type (g arg-types)
               ;; The class of an inline method's value, given its
               ;; arguments' types
               (cond ((member :none arg-types) :none)
                     (t (let ((args (mapcar (lambda (type) (make-c-atom "" type)) arg-types)))
                          (generic-call-type g args))))))
      (mapc #'walk-fn fns)
      (loop for var being the hash-keys of sources do (setf (gethash var types) :none))
      (dolist (fn fns) (setf (gethash fn results) :none))
      (loop
        (let ((changed nil))
          (loop for var being the hash-keys of sources using (hash-value srcs)
                do (let ((type (reduce #'join-types
                                       (mapcar (lambda (s)
                                                 (cond ((eq s :unknown) nil)
                                                       ((class-info-p s) s)
                                                       (t (expr-type s))))
                                               srcs)
                                       :initial-value :none)))
                     (unless (eq type (gethash var types))
                       (setf (gethash var types) type changed t))))
          (dolist (fn fns)
            (let ((type (expr-type (ir-fn-body fn))))
              (unless (eq type (gethash fn results))
                (setf (gethash fn results) type changed t))))
          (unless changed (return))))
      (loop for var being the hash-keys of types using (hash-value type)
            do (setf (ivar-type var) (and (class-info-p type) type))))))

;;; Unboxing

(defun raw-argument-p (template i)
  "Does TEMPLATE use argument I only as a C double?"
  (and (search (format nil "{d~d}" i) template)
       (not (search (format nil "{~d}" i) template))))

(defun raw-uses (g args)
  "For the call of G on ARGS, a list saying of each argument whether the
code for the call uses it only as a C double."
  (let ((cases (inline-cases g args)))
    (if (and cases (certain-case-p (first cases))
             (not (fold-call (car (first cases)) args)))
        (let ((c (car (first cases))))
          (loop for a in args for i from 0
                collect (raw-argument-p (case-expression c) i)))
        (mapcar (constantly nil) args))))

(defun choose-unboxed (fns)
  "Decide which variables of FNS are C doubles."
  (setf *unboxed* (make-hash-table :test #'eq))
  (let ((double (double-class))
        (candidates (make-hash-table :test #'eq))
        (uses '()))                     ; (var . target): raw if TARGET is,
                                        ; T: always, NIL: never
    (labels ((note (a target) (when (and (ivar-p a) (gethash a candidates))
                                (push (cons a target) uses)))
             (consider (var)
               (when (and (eq (ivar-type var) double)
                          (member (ivar-kind var) '(:let :temp :param))
                          (not (gethash var *closed-over*)))
                 (setf (gethash var candidates) t)))
             (collect-fn (fn)
               (mapc #'consider (ir-fn-required fn))
               (collect (ir-fn-body fn)))
             (collect (e)
               (typecase e
                 (ir-let (consider (ir-let-var e))
                         (collect (ir-let-value e)) (collect (ir-let-body e)))
                 (ir-mv-let (collect (ir-mv-let-value e)) (collect (ir-mv-let-body e)))
                 (ir-seq (mapc #'collect (ir-seq-forms e)))
                 (ir-if (collect (ir-if-then e)) (collect (ir-if-else e)))
                 (ir-letrec (dolist (f (ir-letrec-fns e))
                              (when (gethash f *groups*) (collect-fn f)))
                            (collect (ir-letrec-body e)))
                 (ir-bind-exit (collect (ir-bind-exit-body e)))
                 (ir-unwind (collect (ir-unwind-protected e)) (collect (ir-unwind-cleanup e)))))
             (walk-fn (fn)
               (when (listp (ir-fn-keys fn))
                 (dolist (k (ir-fn-keys fn)) (walk (ir-key-default k))))
               (walk (ir-fn-body fn)))
             (walk (e)
               (typecase e
                 (ivar (note e nil))
                 (ir-let (let ((v (ir-let-value e)))
                           (if (ivar-p v) (note v (ir-let-var e)) (walk v)))
                         (walk (ir-let-body e)))
                 (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-set (note (ir-set-value e) (ir-set-var e)))
                 (ir-global-set (note (ir-global-set-value e) nil))
                 (ir-define-variable (note (ir-define-variable-value e) nil))
                 (ir-prim (dolist (a (ir-prim-arguments e)) (note a nil)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (note (ir-if-test e) nil) (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (walk (ir-letrec-body e)))
                 (ir-bind-exit (walk (ir-bind-exit-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e)))
                 (ir-call (walk-call e))))
             (walk-call (e)
               (let* ((target (call-target e))
                      (group (and target (gethash target *groups*))))
                 (note (ir-call-function e) nil)
                 (if group
                     (loop for a in (ir-call-arguments e)
                           for p in (ir-fn-required target)
                           do (note a p))
                     (multiple-value-bind (kind what swap) (call-callee e)
                       (let ((args (if swap (reverse (ir-call-arguments e))
                                       (ir-call-arguments e))))
                         (case kind
                           (:generic (loop for a in args for raw in (raw-uses what args)
                                           do (note a raw)))
                           (:fold
                            (let ((acc (first args)))
                              (loop for a in (rest args)
                                    for raws = (raw-uses what (list acc a))
                                    do (note acc (first raws))
                                       (note a (second raws))
                                       (setf acc (make-c-atom "" (generic-call-type
                                                                  what (list acc a)))))))
                           (t (dolist (a args) (note a nil))))))))))
      (dolist (fn fns)
        (unless (gethash fn *groups*)
          (when (gethash fn *cfuns*) (collect-fn fn))))
      (dolist (fn fns) (walk-fn fn))
      ;; Drop each candidate with a use that wants a boxed value.
      (loop
        (let ((changed nil))
          (loop for (var . target) in uses
                when (and (gethash var candidates)
                          (not (or (eq target t)
                                   (and (ivar-p target) (gethash target candidates)))))
                  do (remhash var candidates) (setf changed t))
          (unless changed (return))))
      (loop for var being the hash-keys of candidates do (setf (gethash var *unboxed*) t)))))
;;; Escape analysis: #rest lists

(defun stack-rest-p (var) (and *stack-rests* (gethash var *stack-rests*)))

(defun find-stack-rests (fns)
  "Find the #rest lists of FNS that never escape their function, which
may then be on its stack: those used only to be walked -- by car, cdr,
the iteration protocol inline, tests -- through variables no closure
holds."
  (setf *stack-rests* (make-hash-table :test #'eq))
  (let ((uses (make-hash-table :test #'eq))      ; ivar -> (kind . data)
        (results (make-hash-table :test #'eq)))  ; ir-call -> let var of its value
    (labels ((use (a kind &optional data)
               (when (ivar-p a) (push (cons kind data) (gethash a uses))))
             (walk-fn (fn)
               (when (listp (ir-fn-keys fn))
                 (dolist (k (ir-fn-keys fn)) (walk (ir-key-default k))))
               (walk (ir-fn-body fn)))
             (walk (e)
               (typecase e
                 (ivar (use e :other))
                 (ir-let (let ((v (ir-let-value e)))
                           (cond ((ivar-p v) (use v :let (ir-let-var e)))
                                 (t (when (ir-call-p v) (setf (gethash v results) (ir-let-var e)))
                                    (walk v))))
                         (walk (ir-let-body e)))
                 (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-set (use (ir-set-value e) :let (ir-set-var e)))
                 (ir-global-set (use (ir-global-set-value e) :other))
                 (ir-define-variable (use (ir-define-variable-value e) :other))
                 (ir-prim (dolist (a (ir-prim-arguments e)) (use a :other)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (use (ir-if-test e) :test)
                        (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (walk (ir-letrec-body e)))
                 (ir-bind-exit (walk (ir-bind-exit-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e)))
                 (ir-call (use (ir-call-function e) :other)
                          (loop for a in (ir-call-arguments e) for i from 0
                                do (use a :call (cons e i))))
                 (ir-define-class (dolist (a (ir-define-class-superclasses e)) (use a :other)))))
             (call-use (e i derive)
               ;; Is argument I of the call E a safe use?  DERIVE is
               ;; called on what then holds part of the list.
               (let* ((target (call-target e))
                      (group (and target (gethash target *groups*))))
                 (if group
                     (progn (funcall derive (nth i (ir-fn-required target))) t)
                     (multiple-value-bind (kind what swap) (call-callee e)
                       (let ((args (if swap (reverse (ir-call-arguments e)) (ir-call-arguments e)))
                             (result (gethash e results)))
                         (flet ((derived-result ()
                                  (and result (progn (funcall derive result) t))))
                           (case kind
                             (:primitive
                              (cond ((member what '("car" "pair_p" "id" "not") :test #'string=) t)
                                    ((string= what "cdr") (derived-result))))
                             (:generic
                              (let ((name (dylan::dylan-symbol->string
                                           (name-symbol (module-var-name (gfun-var what)))))
                                    (cases (inline-cases what args)))
                                (and (eq (module-var-module (gfun-var what)) (dylan-module))
                                     (some (lambda (c) (member "<pair>" (case-classes (car c))
                                                               :test #'string=))
                                           cases)
                                     (cond ((string= name "current-element") t)
                                           ((member name '("initial-state" "next-state")
                                                    :test #'string=)
                                            (derived-result)))))))))))))
             (escapes-p (rest)
               (let ((derived (list rest)) (work (list rest)))
                 (loop while work
                       do (let ((var (pop work)))
                            (when (or (gethash var *closed-over*)
                                      ;; a use not seen above
                                      (/= (length (gethash var uses)) (ivar-ref-count var)))
                              (return-from escapes-p t))
                            (dolist (u (gethash var uses))
                              (flet ((derive (v)
                                       (unless (member v derived)
                                         (push v derived) (push v work))))
                                (unless (ecase (car u)
                                          (:test t)
                                          (:let (derive (cdr u)) t)
                                          (:other nil)
                                          (:call (call-use (cadr u) (cddr u) #'derive)))
                                  (return-from escapes-p t))))))
                 nil)))
      (mapc #'walk-fn fns)
      (dolist (fn fns)
        (let ((rest (ir-fn-rest fn)))
          (when (and rest (not (escapes-p rest)))
            (setf (gethash rest *stack-rests*) t)))))))
