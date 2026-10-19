;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Type inference and float unboxing (CompilerPlan.md, Phase 9).
;;;;
;;;; A variable's type is a class all its values are instances of, or NIL
;;;; for none known.  The types come from what binds and assigns the
;;;; variable: constants; the inline methods of calls.lisp, whose result
;;;; classes are known once their arguments' are; make of a class it calls
;;;; the maker of; the getters of slots of a known type; the type a bind
;;;; declares; the specializers of a method defined by define-method, which
;;;; dispatch has checked; and, for the parameters of a function whose every
;;;; call is known (a loop, most often), the arguments of those calls.
;;;; They are found together, from nothing up, until none changes.
;;;;
;;;; They are also flow-sensitive where a test proves more: in the branch
;;;; of an if that (instance? x C) chose -- (not (instance? x C)) too -- x
;;;; is known to be a C (*REFINEMENTS*), for the code generated there and
;;;; for the types of what is bound there.  A variable whose values are a
;;;; C or #f, as the iteration protocol's states are, is known to be a C in
;;;; the branch of an if that tested it true (*OR-FALSE-TYPES*).
;;;;
;;;; The types let calls.lisp drop the tests of its inline cases.  A
;;;; <double-float> variable that no closure holds, and whose every use
;;;; wants a C double, is UNBOXED: a C double, which needs no allocation.

(in-package #:dylan-compiler)

(defun narrower (type refined)
  "The more specific of the classes TYPE and REFINED (either may be NIL).
If neither is a subclass of the other, the code is never run; TYPE."
  (cond ((null type) refined)
        ((null refined) type)
        ((subclass-info-p refined type) refined)
        (t type)))

(defun atom-type (a)
  "The class of the atom A's value, if known."
  (etypecase a
    (const (ignore-errors (constant-class (const-value a))))
    (ivar (let ((type (ivar-type a))
                (refined (cdr (assoc a *refinements*))))
            (if (eq refined :true)
                (or (and (class-info-p type) type)
                    (and *or-false-types* (gethash a *or-false-types*)))
                (narrower (and (class-info-p type) type) refined))))
    (c-atom (c-atom-type a))))

(defun type-test (atom &optional negated)
  "If the atom ATOM is the value of (instance? x C), x a variable never
assigned and C a class known when compiling -- or of not of such a value
-- answer x, C, and whether it was negated."
  (when (and (ivar-p atom) (not (ivar-assigned-p atom)) *let-values*)
    (let ((v (gethash atom *let-values*)))
      (when (ir-call-p v)
        (multiple-value-bind (kind what) (call-callee v)
          (when (eq kind :primitive)
            (let ((args (ir-call-arguments v)))
              (cond ((and (string= what "instance_p") (= (length args) 2)
                          (ivar-p (first args)) (not (ivar-assigned-p (first args)))
                          (known-class (second args)))
                     (values (first args) (known-class (second args)) negated))
                    ((and (string= what "not") (= (length args) 1))
                     (type-test (first args) (not negated)))))))))))

(defun if-refinements (e)
  "The refinements the test of the if E gives its then branch and its else
branch: two alists.  A variable the test is, never assigned, is :TRUE in
the then branch: not #f."
  (multiple-value-bind (var class negated) (type-test (ir-if-test e))
    (cond ((and (null var) (ivar-p (ir-if-test e)) (not (ivar-assigned-p (ir-if-test e))))
           (values (list (cons (ir-if-test e) :true)) '()))
          ((null var) (values '() '()))
          (negated (values '() (list (cons var class))))
          (t (values (list (cons var class)) '())))))

(defun double-class () (find-class-info "<double-float>" *classes*))

(defparameter *primitive-result-classes*
  '(("id" "<boolean>") ("not" "<boolean>") ("instance_p" "<boolean>")
    ("pair_p" "<boolean>") ("character_p" "<boolean>")
    ("cons" "<pair>") ("vector" "<simple-object-vector>")
    ("make_vector" "<simple-object-vector>") ("make_byte_string" "<byte-string>")
    ("vector_size" "<integer>") ("character_to_integer" "<integer>")
    ("integer_to_character" "<character>"))
  "The functions written in C whose values are always of a class: the C
name of each and the class.")

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
              unless (or (gethash var escapes) (open-library-var-p var))
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
                          (not (open-library-var-p (gfun-var g)))
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
      (multiple-value-bind (kind what swap) (call-callee e)
        (and (eq kind :generic)
             (let ((m (or (sole-method what)
                          (resolved-method what (if swap
                                                    (reverse (ir-call-arguments e))
                                                    (ir-call-arguments e))))))
               (and m (gmethod-fn m)))))))

(defun made-class (c-name args)
  "The class of what the function written in C named C-NAME makes, if it
makes an instance of a class known when compiling: %make of the class,
or apply of %make to it."
  (flet ((make-p (a)
           (let ((v (static-value a)))
             (and (eq (car v) :global) (eq (second v) (dylan-var "%make"))))))
    (cond ((and (string= c-name "make") args) (known-made-class (first args)))
          ((and (string= c-name "apply") (rest args) (make-p (first args)))
           (known-made-class (second args))))))

(defun known-made-class (a)
  "The class A is, if %make of it makes a direct instance of it."
  (let ((class (known-class a)))
    (and class (class-gclass class) class)))

;;; Inference

;;; During the search a type is also :NONE, for no value yet; :FALSE, for
;;; #f alone; or (:OR-FALSE . C), for an instance of the class C or #f.

(defun or-false-type-p (type) (and (consp type) (eq (car type) :or-false)))

(defun join-types (a b)
  (flet ((join-false (other)
           ;; :FALSE and OTHER
           (cond ((or (eq other :false) (or-false-type-p other)) other)
                 ((and (class-info-p other)
                       (subclass-info-p other (find-class-info "<boolean>" *classes*)))
                  (find-class-info "<boolean>" *classes*))
                 ((class-info-p other) (cons :or-false other)))))
    (cond ((eq a :none) b)
          ((eq b :none) a)
          ((equal a b) a)
          ((eq a :false) (join-false b))
          ((eq b :false) (join-false a))
          ((and (or-false-type-p a) (eq (cdr a) b)) a)
          ((and (or-false-type-p b) (eq (cdr b) a)) b)
          (t nil))))

(defun class-of-type (type)
  "The class a search type stands for: NIL if not one, or :NONE."
  (cond ((eq type :false) (find-class-info "<boolean>" *classes*))
        ((or-false-type-p type) nil)
        (t type)))

(defun infer-types (fns)
  "Set the type of every variable of FNS."
  (let ((sources (make-hash-table :test #'eq))   ; ivar -> list of sources
        (types (make-hash-table :test #'eq))     ; ivar -> type during the search
        (results (make-hash-table :test #'eq))   ; ir-fn -> type of its value
        (declared (make-hash-table :test #'eq))  ; ivar -> the class a bind declares
        (callers (analyze-calls fns)))
    (labels ((source (var s)
               ;; An expression is evaluated with the refinements where it is.
               (push (if (or (eq s :unknown) (class-info-p s)) s (cons s *refinements*))
                     (gethash var sources)))
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
                 (let ((*refinements* '()))
                   (walk (ir-fn-body fn)))))
             (walk (e)
               (typecase e
                 (ir-let (source (ir-let-var e) (ir-let-value e))
                         (walk (ir-let-value e)) (walk (ir-let-body e)))
                 (ir-prim
                  ;; A bind's declared type, checked before its body
                  (let ((args (ir-prim-arguments e)))
                    (when (and (eq (ir-prim-name e) :check-type)
                               (ivar-p (first args)) (not (ivar-assigned-p (first args)))
                               (known-class (second args)))
                      (setf (gethash (first args) declared) (known-class (second args))))))
                 (ir-mv-let (dolist (v (ir-mv-let-vars e)) (unknown v))
                            (when (ir-mv-let-rest e) (unknown (ir-mv-let-rest e)))
                            (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-set (source (ir-set-var e) (ir-set-value e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (multiple-value-bind (then else) (if-refinements e)
                          (with-refinements (then) (walk (ir-if-then e)))
                          (with-refinements (else) (walk (ir-if-else e)))))
                 (ir-letrec (mapc #'unknown (ir-letrec-vars e))
                            (walk (ir-letrec-body e)))
                 (ir-bind-exit (unknown (ir-bind-exit-var e))
                               (walk (ir-bind-exit-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e)))))
             (var-type (var)
               (multiple-value-bind (type found) (gethash var types)
                 (if found type nil)))
             (search-atom-type (a)
               ;; The search type of A's value
               (cond ((ivar-p a)
                      (let ((type (var-type a))
                            (refined (cdr (assoc a *refinements*))))
                        (cond ((eq type :none) :none)
                              ((eq refined :true)
                               (if (or-false-type-p type) (cdr type) type))
                              ((or (null type) (class-info-p type)) (narrower type refined))
                              (t type))))
                     ((and (const-p a) (eq (const-value a) +false+)) :false)
                     (t (atom-type a))))
             (atom-type* (a) (class-of-type (search-atom-type a)))
             (expr-type (e)
               (typecase e
                 ((or const ivar) (search-atom-type e))
                 (ir-let (expr-type (ir-let-body e)))
                 (ir-mv-let (expr-type (ir-mv-let-body e)))
                 (ir-letrec (expr-type (ir-letrec-body e)))
                 (ir-seq (expr-type (car (last (ir-seq-forms e)))))
                 (ir-if (multiple-value-bind (then else) (if-refinements e)
                          (join-types (with-refinements (then) (expr-type (ir-if-then e)))
                                      (with-refinements (else) (expr-type (ir-if-else e))))))
                 (ir-set (search-atom-type (ir-set-value e)))
                 (ir-global-set (search-atom-type (ir-global-set-value e)))
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
                                (:generic (or (step-type what (mapcar #'atom-type* args))
                                              (resolved-make-class what args)
                                              (slot-getter-type what args)))
                                (:fold (reduce (lambda (acc a)
                                                 (step-type what (list acc (atom-type* a))))
                                               (rest args)
                                               :initial-value (atom-type* (first args))))
                                (:primitive
                                 (let ((class (second (assoc what *primitive-result-classes*
                                                             :test #'string=))))
                                   (if class
                                       (find-class-info class *classes*)
                                       (made-class what args))))
                                (t nil)))
                            (callee (and (null type)
                                         (or (known-callee e)
                                             (and (eq kind :generic)
                                                  (let ((types (mapcar #'atom-type* args)))
                                                    (if (member :none types)
                                                        (return-from call-type :none)
                                                        (let ((m (resolved-method what args types)))
                                                          (and m (gmethod-fn m))))))))))
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
      (loop for round from 1 do
        (let ((changed nil))
          (when (> round 1000)
            ;; Narrowing makes the search not plainly monotone; should it
            ;; not settle, nothing is known.
            (clrhash types)
            (return))
          (loop for var being the hash-keys of sources using (hash-value srcs)
                do (let ((type (reduce #'join-types
                                       (mapcar (lambda (s)
                                                 (cond ((eq s :unknown) nil)
                                                       ((class-info-p s) s)
                                                       (t (let ((*refinements* (cdr s)))
                                                            (expr-type (car s))))))
                                               srcs)
                                       :initial-value :none))
                         (declared-type (gethash var declared)))
                     (when (and declared-type (or (null type) (class-info-p type)))
                       (setf type (narrower type declared-type)))
                     (unless (equal type (gethash var types))
                       (setf (gethash var types) type changed t))))
          (dolist (fn fns)
            (let ((type (expr-type (ir-fn-body fn))))
              (unless (equal type (gethash fn results))
                (setf (gethash fn results) type changed t))))
          (unless changed (return))))
      (loop for var being the hash-keys of types using (hash-value type)
            do (setf (ivar-type var) (and (class-info-p (class-of-type type))
                                          (class-of-type type)))
               (when (and (or-false-type-p type) *or-false-types*)
                 (setf (gethash var *or-false-types*) (cdr type)))))))

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
        ;; A double's own cases want it as one; the others, which call
        ;; the generic function, box it there.
        (mapcar (lambda (a)
                  (and cases (double-contagion-p g args)
                       (eq (argument-fit a "<double-float>") :yes)))
                args))))

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
                 (ir-if (note (ir-if-test e) nil)
                        (multiple-value-bind (then else) (if-refinements e)
                          (with-refinements (then) (walk (ir-if-then e)))
                          (with-refinements (else) (walk (ir-if-else e)))))
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
                        (multiple-value-bind (then else) (if-refinements e)
                          (with-refinements (then) (walk (ir-if-then e)))
                          (with-refinements (else) (walk (ir-if-else e)))))
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

;;; Escape analysis: closures and instances on the stack
;;;
;;; A closure, or an instance made by make of a class whose maker the call
;;; resolves to, may be on the stack of the C function that makes it if
;;; nothing can reach it once that function returns -- or makes a tail
;;; call, which ends its frame.  Its variable, and those it is bound to
;;; (never assigned ones only: the object of one pass of a loop must be
;;; gone by the next), may only be tested, called (a closure, not in tail
;;; position, with the number of arguments it takes), given to a slot's
;;; getter or setter that is sure to apply (an instance), or passed, not
;;; in tail position, to a function known when compiling whose parameter
;;; does the same.  A parameter may also be passed or called in tail
;;; position: the frame that holds the object is below.  Nothing that could
;;; signal an error holding the object -- dispatch, a type check, a
;;; primitive -- is allowed, since a handler could keep the condition.

(defun stack-object-p (var) (and *stack-objects* (gethash var *stack-objects*)))

(defun find-stack-objects (fns)
  "Find the closures and instances of FNS that may be on the stack."
  (setf *stack-objects* (make-hash-table :test #'eq))
  (let ((uses (make-hash-table :test #'eq))      ; ivar -> (kind . data)
        (candidates '())                         ; (var :closure fn) or (var :instance class)
        (params (make-hash-table :test #'eq)))   ; fn -> vector of (escapes . arities)
    (labels ((use (a kind &optional data)
               (when (ivar-p a) (push (cons kind data) (gethash a uses))))
             (walk-fn (fn)
               (when (listp (ir-fn-keys fn))
                 (dolist (k (ir-fn-keys fn)) (walk (ir-key-default k))))
               (walk (ir-fn-body fn)))
             (walk (e)
               (typecase e
                 (ivar (use e :other))
                 (ir-let
                  (let ((var (ir-let-var e)) (v (ir-let-value e)))
                    (typecase v
                      (ivar (use v :let var))
                      (ir-lambda (push (list var :closure (ir-lambda-fn v)) candidates))
                      (ir-call (multiple-value-bind (kind what) (call-callee v)
                                 (let ((class (and (eq kind :generic)
                                                   (resolved-make-class what (ir-call-arguments v)))))
                                   (when class (push (list var :instance class) candidates))))
                               (walk v))
                      (t (walk v)))
                    (walk (ir-let-body e))))
                 (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-set (use (ir-set-value e) :other))
                 (ir-global-set (use (ir-global-set-value e) :other))
                 (ir-define-variable (use (ir-define-variable-value e) :other))
                 (ir-prim (dolist (a (ir-prim-arguments e)) (use a :other)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (use (ir-if-test e) :test) (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (loop for var in (ir-letrec-vars e)
                                  for fn in (ir-letrec-fns e)
                                  unless (gethash fn *groups*)
                                    do (push (list var :closure fn) candidates))
                            (walk (ir-letrec-body e)))
                 (ir-bind-exit (walk (ir-bind-exit-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e)))
                 (ir-call (use (ir-call-function e) :callee e)
                          (loop for a in (ir-call-arguments e) for i from 0
                                do (use a :call (cons e i))))
                 (ir-define-class (dolist (a (ir-define-class-superclasses e)) (use a :other)))))
             (parameter (fn i)
               ;; Is the argument for FN's required parameter I safe there:
               ;; (values ok arities)?  (Past them, it goes in FN's #rest
               ;; list or keywords.  Too few or too many arguments are an
               ;; error about FN, not them.)
               (let ((entries (gethash fn params)))
                 (cond ((or (null entries) (>= i (length entries))) nil)
                       ((car (aref entries i)) nil)
                       (t (values t (cdr (aref entries i)))))))
             (slot-accessor-p (g i n type)
               ;; Is argument I of N to G that of a slot's getter or setter
               ;; one of whose methods surely applies to an object of TYPE?
               (let ((methods (gfun-methods g)))
                 (and (= i 0) type methods
                      (every (lambda (m)
                               (and (member (gmethod-kind m) '(:getter :setter))
                                    (= n (if (eq (gmethod-kind m) :getter) 1 2))))
                             methods)
                      (some (lambda (m)
                              (let ((sd (gmethod-sdef m)))
                                (and (sdef-class sd) (subclass-info-p type (sdef-class sd)))))
                            methods))))
             (argument (call i mode type)
               ;; Is the object safe as argument I of CALL: (values ok arities)?
               (let* ((target (call-target call))
                      (n (length (ir-call-arguments call))))
                 (cond
                   ((and (eq mode :host) (ir-call-tail-p call)) nil)
                   ((eq (ir-call-kind call) :exit) nil)
                   ((and target (gethash target *groups*)) nil) ; into a loop's variables
                   ((and target (gethash target *cfuns*)) (parameter target i))
                   ((global-callee call) (parameter (global-callee call) i))
                   (t
                    (multiple-value-bind (kind what swap) (call-callee call)
                      (case kind
                        (:primitive
                         (and (or (member what '("id" "not" "pair_p") :test #'string=)
                                  (and (string= what "instance_p") (= i 0)))
                              (values t nil)))
                        (:generic
                         (let ((i (if swap (- n 1 i) i))
                               (args (if swap (reverse (ir-call-arguments call))
                                         (ir-call-arguments call))))
                           (cond ((resolved-make-class what args) nil)
                                 ((slot-accessor-p what i n type) (values t nil))
                                 ((sole-method what)
                                  (parameter (gmethod-fn (sole-method what)) i)))))))))))
             (check (var mode type)
               ;; Does the object VAR holds escape: (values escapes arities)?
               (let ((work (list var)) (seen (list var)) (arities '()))
                 (loop while work
                       do (let ((v (pop work)))
                            (when (or (gethash v *closed-over*) (ivar-assigned-p v)
                                      (/= (length (gethash v uses)) (ivar-ref-count v)))
                              (return-from check t))
                            (dolist (u (gethash v uses))
                              (unless
                                  (ecase (car u)
                                    (:test t)
                                    (:other nil)
                                    (:let (let ((v2 (cdr u)))
                                            (unless (member v2 seen)
                                              (push v2 seen) (push v2 work))
                                            t))
                                    (:callee (let ((call (cdr u)))
                                               (and (not (and (eq mode :host) (ir-call-tail-p call)))
                                                    (not (eq (ir-call-kind call) :exit))
                                                    (progn (pushnew (length (ir-call-arguments call))
                                                                    arities)
                                                           t))))
                                    (:call (multiple-value-bind (ok more)
                                               (argument (cadr u) (cddr u) mode type)
                                             (when ok
                                               (dolist (a more) (pushnew a arities))
                                               t))))
                                (return-from check t)))))
                 (values nil arities))))
      (mapc #'walk-fn fns)
      ;; The parameters, from all safe down
      (dolist (fn fns)
        (when (gethash fn *cfuns*)
          (setf (gethash fn params)
                (let ((v (make-array (length (ir-fn-required fn)))))
                  (dotimes (i (length v) v) (setf (aref v i) (cons nil nil)))))))
      (loop
        (let ((changed nil))
          (loop for fn being the hash-keys of params using (hash-value entries)
                do (loop for p in (ir-fn-required fn)
                         for spec in (ir-fn-specializers fn)
                         for i from 0
                         do (multiple-value-bind (escapes arities)
                                (if (or (and spec (not (gethash fn *method-of-fn*))) ; checked
                                        (unboxed-p p)
                                        (and (ir-fn-next fn) (gethash fn *method-of-fn*)
                                             (needs-captured-next-p (ir-fn-next fn) fn)))
                                    t
                                    (check p :param (atom-type p)))
                              (let ((old (aref entries i)))
                                (unless (and (eq (car old) escapes)
                                             (null (set-exclusive-or (cdr old) arities)))
                                  (setf (aref entries i) (cons escapes arities) changed t))))))
          (unless changed (return))))
      ;; The objects
      (loop for (var kind what) in candidates
            do (multiple-value-bind (escapes arities)
                   (check var :host (and (eq kind :instance) what))
                 (when (and (not escapes)
                            (ecase kind
                              (:closure
                               (let ((cfun (gethash what *cfuns*)))
                                 (and cfun (cfun-env cfun)
                                      (simple-fn-p what)
                                      (every (lambda (a) (= a (length (ir-fn-required what))))
                                             arities))))
                              (:instance
                               (and (null arities) (null (initialize-methods what))))))
                   (setf (gethash var *stack-objects*) kind)))))))
