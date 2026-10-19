;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Inlining.  A call of a generic function that is known to reach one
;;;; small method -- its only method, or the one the classes of the
;;;; arguments choose (calls.lisp) -- is replaced by a copy of the method's
;;;; body, its parameters bound to the arguments; so is a call of a module
;;;; variable that always holds one small function (types.lisp).  The C compiler then
;;;; sees the whole loop that iterates over a range or a stretchy vector,
;;;; not calls into the library, and the copy's types follow from the
;;;; caller's: a test of an argument already known to be an integer is
;;;; dropped, and so on.
;;;;
;;;; Only the program's functions are inlined into, never the library's,
;;;; whose C must not depend on the program.  A function qualifies if it is
;;;; small (*INLINE-SIZE*), makes no closures, binds no exits and has no
;;;; cleanups, calls no next method, takes no #rest, and uses no variable
;;;; of an enclosing function -- and, if not a method, specializes none of
;;;; its parameters, which it would check.  The calls in a copy may be inlined in their
;;;; turn, after the types are found again, to a depth of *INLINE-DEPTH*,
;;;; but a method is never inlined into a copy of itself.
;;;;
;;;; Nothing the program can see changes: the method run is the one
;;;; dispatch would run, with the same arguments, and its keyword
;;;; arguments' defaults are evaluated as its entry would.

(in-package #:dylan-compiler)

(defparameter *inline-size* 16
  "The most calls, primitives, ifs and assignments a function's code may
have to be inlined.")

(defparameter *inline-depth* 3
  "How many times a copy's calls may be inlined in their turn.")

(defvar *inline-chains* nil
  "IR-CALL -> the methods whose inlining made it, innermost first.")

(defun inline-calls (ir)
  "Inline the calls of the program's functions that qualify, finding the
types again before each round; leave IR analyzed afresh."
  (let ((*ivar-counter* (reduce #'max (ir-bound-vars ir) :key #'ivar-id :initial-value 0))
        (*inline-chains* (make-hash-table :test #'eq))
        (bodies (make-hash-table :test #'eq)))  ; fn -> T if it qualifies
    (loop repeat (1+ *inline-depth*)
          do (let ((fns (ir-fns ir)) (changed nil))
               (infer-types fns)
               (dolist (fn fns)
                 (unless (ir-fn-library-p fn)
                   (when (inline-in-fn fn bodies) (setf changed t))))
               (unless changed (return))
               (reanalyze-program ir)
               (note-let-values ir)))))

(defun note-let-values (ir)
  "Record the value of every let in IR (*LET-VALUES*)."
  (dolist (fn (ir-fns ir))
    (labels ((walk (e)
               (typecase e
                 (ir-let (setf (gethash (ir-let-var e) *let-values*) (ir-let-value e))
                         (walk (ir-let-value e)) (walk (ir-let-body e)))
                 (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (walk (ir-letrec-body e)))
                 (ir-bind-exit (walk (ir-bind-exit-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e))))))
      (when (listp (ir-fn-keys fn))
        (dolist (k (ir-fn-keys fn)) (walk (ir-key-default k))))
      (walk (ir-fn-body fn)))))

;;; Which methods qualify

(defun inlinable-body-p (fn)
  "Is FN's code small and simple enough to copy?"
  (let ((count 0))
    (labels ((walk (e)
               (when (and (typep e '(or ir-call ir-prim ir-if ir-set ir-global-set))
                          (> (incf count) *inline-size*))
                 (return-from inlinable-body-p nil))
               (etypecase e
                 ((or const ivar ir-global-ref))
                 (ir-let (walk (ir-let-value e)) (walk (ir-let-body e)))
                 (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                 ((or ir-call ir-prim ir-global-set ir-set))
                 ((or ir-letrec ir-lambda ir-bind-exit ir-unwind
                      ir-define-variable ir-define-generic ir-define-method
                      ir-define-class ir-define-slot)
                  (return-from inlinable-body-p nil)))))
      (when (listp (ir-fn-keys fn))
        (dolist (k (ir-fn-keys fn)) (walk (ir-key-default k))))
      (walk (ir-fn-body fn))
      t)))

(defun inlinable-fn-p (fn bodies)
  (multiple-value-bind (known found) (gethash fn bodies)
    (if found
        known
        (setf (gethash fn bodies)
              (and (member (ir-fn-kind fn) '(:method :lambda))
                   ;; A function, not a method, checks its specializers.
                   (or (eq (ir-fn-kind fn) :method) (every #'null (ir-fn-specializers fn)))
                   (null (ir-fn-rest fn))
                   (not (uses-next-method-p fn))
                   (null (ir-fn-free-vars fn))
                   (inlinable-body-p fn))))))

(defun inline-target (e bodies)
  "The fn the call E can be replaced by, and its arguments in order."
  (let ((global (global-callee e)))
    (when global
      (return-from inline-target
        (and (eq (ir-fn-kind global) :lambda)
             (= (length (ir-call-arguments e)) (length (ir-fn-required global)))
             (null (ir-fn-keys global))
             (inlinable-fn-p global bodies)
             (not (member global (gethash e *inline-chains*)))
             (< (length (gethash e *inline-chains*)) *inline-depth*)
             (values global (ir-call-arguments e))))))
  (multiple-value-bind (kind g swap) (call-callee e)
    (when (eq kind :generic)
      (let* ((args (if swap (reverse (ir-call-arguments e)) (ir-call-arguments e)))
             (n (length args))
             (m (and (not (resolved-make-class g args))
                     (not (and (<= 1 n 2)
                               (instance-slot-number g (if (= n 1) :getter :setter))))
                     (null (inline-cases g args))
                     (or (let ((sole (sole-method g)))
                           (and sole (= n (gfun-required g)) sole))
                         (resolved-method g args))))
             (fn (and m (eq (gmethod-kind m) :ir) (gmethod-fn m))))
        (when (and fn
                   (inlinable-fn-p fn bodies)
                   (not (member fn (gethash e *inline-chains*)))
                   (< (length (gethash e *inline-chains*)) *inline-depth*))
          (values fn args))))))

;;; Replacing calls

(defun inline-in-fn (fn bodies)
  "Inline the calls in FN's code that qualify; answer whether any was."
  (let ((changed nil))
    (labels ((expr (e)
               ;; E with its calls replaced
               (typecase e
                 (ir-call (multiple-value-bind (callee args) (inline-target e bodies)
                            (if callee
                                (progn (setf changed t)
                                       (inline-copy e callee args fn))
                                e)))
                 (ir-let
                  (let ((value (expr (ir-let-value e)))
                        (body (expr (ir-let-body e))))
                    (splice value (lambda (v) (relocate (make-ir-let (ir-let-var e) v body) e)))))
                 (ir-mv-let
                  (let ((value (expr (ir-mv-let-value e)))
                        (body (expr (ir-mv-let-body e))))
                    (splice value (lambda (v)
                                    (relocate (make-ir-mv-let (ir-mv-let-vars e) (ir-mv-let-rest e)
                                                              v body)
                                              e)))))
                 (ir-seq (relocate (make-ir-seq (mapcar #'expr (ir-seq-forms e))) e))
                 (ir-if
                  (multiple-value-bind (then else) (if-refinements e)
                    (setf (ir-if-then e) (with-refinements (then) (expr (ir-if-then e)))
                          (ir-if-else e) (with-refinements (else) (expr (ir-if-else e)))))
                  e)
                 (ir-letrec (setf (ir-letrec-body e) (expr (ir-letrec-body e))) e)
                 (ir-bind-exit (setf (ir-bind-exit-body e) (expr (ir-bind-exit-body e))) e)
                 (ir-unwind (setf (ir-unwind-protected e) (expr (ir-unwind-protected e))
                                  (ir-unwind-cleanup e) (expr (ir-unwind-cleanup e)))
                            e)
                 (t e))))
      (let ((*refinements* '()))
        (when (listp (ir-fn-keys fn))
          (dolist (k (ir-fn-keys fn))
            (setf (ir-key-default k) (expr (ir-key-default k)))))
        (setf (ir-fn-body fn) (expr (ir-fn-body fn)))))
    changed))

(defun relocate (new old)
  (when (ir-node-p new)
    (setf (ir-node-location new) (ir-node-location old)))
  new)

(defun splice (e k)
  "An expression evaluating E, then (K value): the bindings and effects of
E moved out, so that the value K is given is not a let, mv-let or
sequence, as the IR wants a let's value to be."
  (typecase e
    (ir-let (setf (ir-let-body e) (splice (ir-let-body e) k)) e)
    (ir-mv-let (setf (ir-mv-let-body e) (splice (ir-mv-let-body e) k)) e)
    (ir-seq (let ((forms (ir-seq-forms e)))
              (make-ir-seq (append (butlast forms)
                                   (list (splice (car (last forms)) k))))))
    (t (funcall k e))))

(defun inline-copy (call fn args caller)
  "The code of FN, a method, on ARGS -- what the call CALL in CALLER's code
does."
  (let* ((map (make-hash-table :test #'eq))
         (chain (cons fn (gethash call *inline-chains*)))
         (bindings '()))                ; (var . expression), in order
    (labels ((fresh (var)
               (let ((new (make-ivar (ivar-name var)
                                     (if (member (ivar-kind var) '(:param :key))
                                         :let
                                         (ivar-kind var))
                                     caller)))
                 (setf (ivar-assigned-p new) (ivar-assigned-p var)
                       (gethash var map) new)
                 (let ((declared (ivar-declared-type var)))
                   (when declared
                     (setf (ivar-declared-type new) (gethash declared map declared))))
                 new))
             (atom* (a) (if (ivar-p a) (gethash a map a) a))
             (copy (e)
               (let ((*location* (if (ir-node-p e) (ir-node-location e) *location*)))
                 (etypecase e
                   (const e)
                   (ivar (atom* e))
                   (ir-let (let* ((value (copy (ir-let-value e)))
                                  (var (fresh (ir-let-var e))))
                             (make-ir-let var value (copy (ir-let-body e)))))
                   (ir-mv-let (let* ((value (copy (ir-mv-let-value e)))
                                     (vars (mapcar #'fresh (ir-mv-let-vars e)))
                                     (rest (and (ir-mv-let-rest e) (fresh (ir-mv-let-rest e)))))
                                (make-ir-mv-let vars rest value (copy (ir-mv-let-body e)))))
                   (ir-seq (make-ir-seq (mapcar #'copy (ir-seq-forms e))))
                   (ir-if (make-ir-if (atom* (ir-if-test e)) (copy (ir-if-then e))
                                      (copy (ir-if-else e))))
                   (ir-call (let ((new (make-ir-call (atom* (ir-call-function e))
                                                     (mapcar #'atom* (ir-call-arguments e)))))
                              (setf (gethash new *inline-chains*) chain)
                              new))
                   (ir-prim (make-ir-prim (ir-prim-name e) (mapcar #'atom* (ir-prim-arguments e))))
                   (ir-global-ref (make-ir-global-ref (ir-global-ref-var e)))
                   (ir-global-set (make-ir-global-set (ir-global-set-var e)
                                                      (atom* (ir-global-set-value e))))
                   (ir-set (make-ir-set (atom* (ir-set-var e)) (atom* (ir-set-value e))))))))
      ;; The required parameters: an argument in place of a parameter never
      ;; assigned, else a variable bound to it
      (loop for var in (ir-fn-required fn)
            for a in args
            do (if (ivar-assigned-p var)
                   (push (cons (fresh var) a) bindings)
                   (setf (gethash var map) a)))
      (when (ir-fn-next fn)
        (setf (gethash (ir-fn-next fn) map) (make-const dylan::dylan%false)))
      ;; The keyword parameters: the leftmost value given, else the default
      (when (listp (ir-fn-keys fn))
        (let ((pairs (nthcdr (length (ir-fn-required fn)) args)))
          (dolist (k (ir-fn-keys fn))
            (let ((given (loop for (key value) on pairs by #'cddr
                               when (eql (const-value (constant-atom* key)) (ir-key-keyword k))
                                 return value)))
              (let ((value (if given (atom* given) (copy (ir-key-default k)))))
                (push (cons (fresh (ir-key-var k)) value) bindings))))))
      (let ((body (copy (ir-fn-body fn))))
        (labels ((bind (bindings)
                   (if (null bindings)
                       body
                       (destructuring-bind ((var . value) . more) bindings
                         (splice value (lambda (v)
                                         (let ((*location* (ir-node-location call)))
                                           (make-ir-let var v (bind more)))))))))
          (bind (reverse bindings)))))))
