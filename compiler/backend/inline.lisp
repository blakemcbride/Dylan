;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Inlining.  A call of a generic function that is known to reach one
;;;; small method -- its only method, or the one the classes of the
;;;; arguments choose (calls.lisp) -- is replaced by a copy of the method's
;;;; code, its parameters bound to the arguments; so is a call of a module
;;;; variable that always holds one small function (types.lisp), and a
;;;; call of a closure a let makes of a function with no free variables.
;;;; The C compiler then sees the whole loop that iterates over a range or
;;;; a stretchy vector, not calls into the library, and the copy's types
;;;; follow from the caller's: a test of an argument already known to be an
;;;; integer is dropped, and so on.
;;;;
;;;; A call that passes a known function -- a closure, a function's or a
;;;; generic function's variable -- may copy a larger function
;;;; (*SPECIALIZE-SIZE*): the copy is the library's function specialized
;;;; for the program, its calls of that argument direct or inlined in their
;;;; turn.  So sort with a test: given, say, runs its merge loop with the
;;;; test inline.
;;;;
;;;; Only the program's functions are inlined into, never the library's,
;;;; whose C must not depend on the program.  A function qualifies if it is
;;;; small enough (*INLINE-SIZE*), binds no exits and has no cleanups,
;;;; calls no next method, takes no #rest, and uses no variable of an
;;;; enclosing function -- and, if not a method, specializes none of its
;;;; parameters, which it would check.  The loops and closures it makes are
;;;; copied too.  The calls in a copy may be inlined in their turn, after
;;;; the types are found again, to a depth of *INLINE-DEPTH*, but a
;;;; function is never inlined into a copy of itself.
;;;;
;;;; Nothing the program can see changes: the method run is the one
;;;; dispatch would run, with the same arguments, and its keyword
;;;; arguments' defaults are evaluated as its entry would.

(in-package #:dylan-compiler)

(defparameter *inline-size* 16
  "The most calls, primitives, ifs and assignments a function's code may
have to be inlined.")

(defparameter *specialize-size* 80
  "The size limit when the call passes a known function.")

(defparameter *inline-depth* 3
  "How many times a copy's calls may be inlined in their turn.")

(defvar *inline-chains* nil
  "IR-CALL -> the methods whose inlining made it, innermost first.")

(defvar *toplevel-order* nil
  "Top-level IR-FN -> its place in the program, and module variable -> the
place of the top-level form that defines it.")

(defun inline-calls (ir)
  "Inline the calls of the program's functions that qualify, finding the
types again before each round; leave IR analyzed afresh."
  (let ((*ivar-counter* (reduce #'max (ir-bound-vars ir) :key #'ivar-id :initial-value 0))
        (*fn-counter* (reduce #'max (ir-fns ir) :key #'ir-fn-id :initial-value 0))
        (*inline-chains* (make-hash-table :test #'eq))
        (*toplevel-order* (toplevel-order ir))
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

(defun toplevel-order (ir)
  (let ((order (make-hash-table :test #'eq)))
    (loop for top in (ir-program-toplevels ir)
          for i from 0
          do (setf (gethash top order) i)
             (labels ((walk (e)
                        (typecase e
                          (ir-define-variable
                           (unless (gethash (ir-define-variable-var e) order)
                             (setf (gethash (ir-define-variable-var e) order) i)))
                          (ir-let (walk (ir-let-value e)) (walk (ir-let-body e)))
                          (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                          (ir-seq (mapc #'walk (ir-seq-forms e)))
                          (ir-if (walk (ir-if-then e)) (walk (ir-if-else e))))))
               (walk (ir-fn-body top))))
    order))

(defun defined-before-p (var fn)
  "Is the module variable VAR defined by a top-level form before the one
FN's code is in?  Then FN's code runs only once VAR has its value: the
top-level forms run in order, and FN is made by its own."
  (let ((top fn))
    (loop while (ir-fn-parent top) do (setf top (ir-fn-parent top)))
    (let ((defined (gethash var *toplevel-order*))
          (here (gethash top *toplevel-order*)))
      (and defined here (< defined here)))))

;;; Which methods qualify

(defun code-size (fn)
  "How many calls, primitives, ifs and assignments FN's code has, with the
functions it makes; or NIL if it binds an exit, has a cleanup, or defines
anything."
  (let ((count 0))
    (labels ((walk-fn (f)
               (when (listp (ir-fn-keys f))
                 (dolist (k (ir-fn-keys f)) (walk (ir-key-default k))))
               (walk (ir-fn-body f)))
             (walk (e)
               (when (typep e '(or ir-call ir-prim ir-if ir-set ir-global-set))
                 (incf count))
               (etypecase e
                 ((or const ivar ir-global-ref))
                 (ir-let (walk (ir-let-value e)) (walk (ir-let-body e)))
                 (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                 ((or ir-call ir-prim ir-global-set ir-set))
                 (ir-letrec (mapc #'walk-fn (ir-letrec-fns e)) (walk (ir-letrec-body e)))
                 (ir-lambda (walk-fn (ir-lambda-fn e)))
                 ((or ir-bind-exit ir-unwind
                      ir-define-variable ir-define-generic ir-define-method
                      ir-define-class ir-define-slot)
                  (return-from code-size nil)))))
      (walk-fn fn)
      count)))

(defun inlinable-fn-p (fn bodies limit)
  "Does FN qualify for inlining, its code no larger than LIMIT?"
  (let ((size (multiple-value-bind (known found) (gethash fn bodies)
                (if found
                    known
                    (setf (gethash fn bodies)
                          (and (member (ir-fn-kind fn) '(:method :lambda))
                               ;; A function, not a method, checks its
                               ;; specializers.
                               (or (eq (ir-fn-kind fn) :method)
                                   (every #'null (ir-fn-specializers fn)))
                               (null (ir-fn-rest fn))
                               (not (uses-next-method-p fn))
                               (null (ir-fn-free-vars fn))
                               (code-size fn)))))))
    (and size (<= size limit))))

(defun known-function-atom-p (a)
  "Is A known when compiling to be a particular function: a closure made
of a function with no free variables, a module variable that always holds
one function, or a generic function?"
  (and (ivar-p a)
       (or (let ((v (and (not (ivar-assigned-p a)) (gethash a *let-values*))))
             (and (ir-lambda-p v) (null (ir-fn-free-vars (ir-lambda-fn v)))))
           (let ((v (static-value a)))
             (and (eq (car v) :global)
                  (or (gethash (second v) *gfuns*)
                      (gethash (second v) *global-functions*)))))))

(defun size-limit (args)
  "How large a function a call on ARGS may inline: larger if one of ARGS
is a known function, which the copy may then call directly or inline."
  (if (some #'known-function-atom-p args) *specialize-size* *inline-size*))

(defun inline-allowed-p (e fn)
  "May the call E, made by inlining the fns of its chain, inline FN?"
  (let ((chain (gethash e *inline-chains*)))
    (and (not (member fn chain))
         (< (length chain) *inline-depth*))))

(defun inline-target (e bodies caller)
  "The fn the call E in CALLER's code can be replaced by, and its arguments
in order."
  (let ((global (or (let ((g (global-callee e)))
                      ;; Inlined, the call no longer reads the variable,
                      ;; which signals an error if it has no value yet.
                      (and g (defined-before-p (second (static-value (ir-call-function e)))
                                               caller)
                           g))
                    ;; A closure made by a let, not a local function of a
                    ;; letrec, whose calls are jumps (contify.lisp)
                    (let* ((f (ir-call-function e))
                           (v (and (ivar-p f) (not (ivar-assigned-p f))
                                   (gethash f *let-values*))))
                      (and (ir-lambda-p v) (ir-lambda-fn v))))))
    (when global
      (return-from inline-target
        (and (eq (ir-fn-kind global) :lambda)
             (= (length (ir-call-arguments e)) (length (ir-fn-required global)))
             (null (ir-fn-keys global))
             (inlinable-fn-p global bodies (size-limit (ir-call-arguments e)))
             (inline-allowed-p e global)
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
                   (inlinable-fn-p fn bodies (size-limit args))
                   (inline-allowed-p e fn))
          (values fn args))))))

;;; Replacing calls

(defun inline-in-fn (fn bodies)
  "Inline the calls in FN's code that qualify; answer whether any was."
  (let ((changed nil))
    (labels ((expr (e)
               ;; E with its calls replaced
               (typecase e
                 (ir-call (multiple-value-bind (callee args) (inline-target e bodies fn)
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
         (binder caller)                ; the fn whose code is being copied into
         (bindings '()))                ; (var . expression), in order
    (labels ((fresh (var)
               (let ((new (make-ivar (ivar-name var)
                                     (if (and (eq binder caller)
                                              (member (ivar-kind var) '(:param :key)))
                                         :let
                                         (ivar-kind var))
                                     binder)))
                 (setf (ivar-assigned-p new) (ivar-assigned-p var)
                       (gethash var map) new)
                 (let ((declared (ivar-declared-type var)))
                   (when declared
                     (setf (ivar-declared-type new) (gethash declared map declared))))
                 new))
             (atom* (a) (if (ivar-p a) (gethash a map a) a))
             (copy-fn (f)
               ;; A function made in the code copied, made anew within BINDER
               (let ((new (%make-ir-fn (incf *fn-counter*) (ir-fn-kind f) (ir-fn-name f)
                                       binder (ir-fn-location f)))
                     (outer binder))
                 (setf (ir-fn-specializers new) (mapcar (lambda (s) (and s (atom* s)))
                                                        (ir-fn-specializers f)))
                 (setf binder new)
                 (unwind-protect
                      (progn
                        (setf (ir-fn-required new) (mapcar #'fresh (ir-fn-required f))
                              (ir-fn-next new) (and (ir-fn-next f) (fresh (ir-fn-next f)))
                              (ir-fn-rest new) (and (ir-fn-rest f) (fresh (ir-fn-rest f)))
                              (ir-fn-keys new)
                              (if (listp (ir-fn-keys f))
                                  (mapcar (lambda (k)
                                            (let ((default (copy (ir-key-default k))))
                                              (make-ir-key (ir-key-keyword k)
                                                           (fresh (ir-key-var k))
                                                           default)))
                                          (ir-fn-keys f))
                                  (ir-fn-keys f))
                              (ir-fn-body new) (copy (ir-fn-body f))))
                   (setf binder outer))
                 new))
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
                   (ir-set (make-ir-set (atom* (ir-set-var e)) (atom* (ir-set-value e))))
                   (ir-letrec (let ((vars (mapcar #'fresh (ir-letrec-vars e))))
                                (make-ir-letrec vars (mapcar #'copy-fn (ir-letrec-fns e))
                                                (copy (ir-letrec-body e)))))
                   (ir-lambda (make-ir-lambda (copy-fn (ir-lambda-fn e))))))))
      ;; The required parameters: an argument in place of a parameter,
      ;; where neither is ever assigned, else a variable bound to it
      (loop for var in (ir-fn-required fn)
            for a in args
            do (if (or (ivar-assigned-p var) (and (ivar-p a) (ivar-assigned-p a)))
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
