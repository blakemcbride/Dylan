;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Lowering of exits and cleanups (CompilerPlan.md, Phase 6).
;;;;
;;;; A bind-exit whose exit procedure is only called directly in its own
;;;; function -- or in a local function compiled into it (contify.lisp) --
;;;; stays: the code generator makes each call a jump.  Any other
;;;; needs a frame in the thread's dynamic chain, and so does every
;;;; unwind-protect.  The runtime's functions that establish frames take
;;;; closures, so their forms are moved out into new functions:
;;;;
;;;;   (unwind-protect p c)  =>  (%primitive unwind-protect (method () p)
;;;;                                                         (method () c))
;;;;   (bind-exit (x) b)     =>  (%primitive call-with-exit (method (x) b))
;;;;
;;;; and the IR analyzed again.  The cleanups go first: an exit called from
;;;; inside an unwind-protect is then called from another function, so its
;;;; bind-exit is lowered too, and the jump that skipped the cleanup never
;;;; happens.  Generated code thus never calls setjmp, and can make tail
;;;; calls anywhere.

(in-package #:dylan-compiler)

(defun reanalyze-program (ir)
  "Clear what ANALYZE-PROGRAM found, and analyze IR again."
  (dolist (f (ir-fns ir))
    (setf (ir-fn-free-vars f) '()
          (ir-fn-call-sites f) '()
          (ir-fn-escapes-p f) nil
          (ir-fn-binder f) nil)
    (labels ((walk (e)
               (typecase e
                 (ir-call (setf (ir-call-kind e) :unknown (ir-call-tail-p e) nil))
                 (ir-let (walk (ir-let-value e)) (walk (ir-let-body e)))
                 (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (walk (ir-letrec-body e)))
                 (ir-bind-exit (walk (ir-bind-exit-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e))))))
      (when (listp (ir-fn-keys f))
        (dolist (k (ir-fn-keys f)) (walk (ir-key-default k))))
      (walk (ir-fn-body f))))
  (dolist (v (ir-bound-vars ir))
    (setf (ivar-captured-p v) nil
          (ivar-ref-count v) 0
          (ivar-call-count v) 0
          (ivar-value-fn v) nil
          (ivar-escapes-p v) nil))
  (analyze-program ir))

(defun rebind (e old new)
  "Move the bindings of E (outside the functions it contains) from the fn
OLD to the fn NEW, and make NEW the parent of the functions it contains."
  (labels ((own (var) (when (eq (ivar-binder var) old) (setf (ivar-binder var) new)))
           (adopt (fn) (when (eq (ir-fn-parent fn) old) (setf (ir-fn-parent fn) new)))
           (walk (e)
             (typecase e
               (ir-let (own (ir-let-var e)) (walk (ir-let-value e)) (walk (ir-let-body e)))
               (ir-mv-let (mapc #'own (ir-mv-let-vars e))
                          (when (ir-mv-let-rest e) (own (ir-mv-let-rest e)))
                          (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
               (ir-seq (mapc #'walk (ir-seq-forms e)))
               (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
               (ir-letrec (mapc #'own (ir-letrec-vars e)) (mapc #'adopt (ir-letrec-fns e))
                          (walk (ir-letrec-body e)))
               (ir-lambda (adopt (ir-lambda-fn e)))
               (ir-define-method (adopt (ir-define-method-fn e)))
               (ir-bind-exit (own (ir-bind-exit-var e)) (walk (ir-bind-exit-body e)))
               (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e))))))
    (walk e)))

(defun outline (body parent params)
  "A new fn, a child of PARENT, of PARAMS and BODY, which PARENT's code
contained."
  (let ((g (let ((*fn* parent)) (make-ir-fn :lambda nil))))
    (setf (ir-fn-required g) params
          (ir-fn-specializers g) (make-list (length params) :initial-element nil)
          (ir-fn-body g) body)
    (dolist (p params)
      (setf (ivar-binder p) g
            (ivar-kind p) :param))
    (rebind body parent g)
    g))

(defun lower-in (e fn transform)
  "E with every form TRANSFORM changes changed, innermost first.  TRANSFORM
is called with a form and the fn whose code it is in, and answers its
replacement or NIL."
  (labels ((walk (e)
             (typecase e
               (ir-let (setf (ir-let-value e) (walk (ir-let-value e))
                             (ir-let-body e) (walk (ir-let-body e))))
               (ir-mv-let (setf (ir-mv-let-value e) (walk (ir-mv-let-value e))
                                (ir-mv-let-body e) (walk (ir-mv-let-body e))))
               (ir-seq (setf (ir-seq-forms e) (mapcar #'walk (ir-seq-forms e))))
               (ir-if (setf (ir-if-then e) (walk (ir-if-then e))
                            (ir-if-else e) (walk (ir-if-else e))))
               (ir-letrec (setf (ir-letrec-body e) (walk (ir-letrec-body e))))
               (ir-bind-exit (setf (ir-bind-exit-body e) (walk (ir-bind-exit-body e))))
               (ir-unwind (setf (ir-unwind-protected e) (walk (ir-unwind-protected e))
                                (ir-unwind-cleanup e) (walk (ir-unwind-cleanup e)))))
             (or (and (ir-node-p e) (funcall transform e fn)) e)))
    (walk e)))

(defun lower-everywhere (ir transform)
  (dolist (f (ir-fns ir))
    (call-with-counters
     f (lambda ()
         (when (listp (ir-fn-keys f))
           (dolist (k (ir-fn-keys f))
             (setf (ir-key-default k) (lower-in (ir-key-default k) f transform))))
         (setf (ir-fn-body f) (lower-in (ir-fn-body f) f transform))))))

(defun lower-unwind-protect (e fn)
  (when (ir-unwind-p e)
    (let* ((*location* (ir-node-location e))
           (protected (outline (ir-unwind-protected e) fn '()))
           (cleanup (outline (ir-unwind-cleanup e) fn '()))
           (t1 (make-ivar (make-symbol "protected") :temp fn))
           (t2 (make-ivar (make-symbol "cleanup") :temp fn)))
      (make-ir-let t1 (make-ir-lambda protected)
                   (make-ir-let t2 (make-ir-lambda cleanup)
                                (make-ir-prim :unwind-protect (list t1 t2)))))))

(defun lower-escaping-exit (e fn)
  (when (and (ir-bind-exit-p e) (ivar-escapes-p (ir-bind-exit-var e)))
    (let* ((*location* (ir-node-location e))
           (body (outline (ir-bind-exit-body e) fn (list (ir-bind-exit-var e))))
           (temp (make-ivar (make-symbol "body") :temp fn)))
      (make-ir-let temp (make-ir-lambda body)
                   (make-ir-prim :call-with-exit (list temp))))))

(defun localize-exits (ir)
  "Mark as not escaping each exit only called directly, in the function
that binds it or in local functions contified into that function (whose
code the code generator puts there), and mark the calls in tail position
again: a bind-exit's body may now have some."
  (let ((*groups* (make-hash-table :test #'eq))
        (local-calls (make-hash-table :test #'eq)) ; exit -> local calls of it
        (changed nil))
    (dolist (fn (ir-fns ir))
      (dolist (letrec (letrecs-in (ir-fn-body fn)))
        (decide-contification letrec)))
    (dolist (fn (ir-fns ir))
      (let ((host (loop for f = fn then (ir-fn-parent f)
                        while (gethash f *groups*)
                        finally (return f))))
        (dolist (c (append (direct-calls (ir-fn-body fn))
                           (and (listp (ir-fn-keys fn))
                                (mapcan (lambda (k) (direct-calls (ir-key-default k)))
                                        (ir-fn-keys fn)))))
          (let ((f (ir-call-function (car c))))
            (when (and (ivar-p f) (eq (ivar-kind f) :exit) (eq host (ivar-binder f)))
              (incf (gethash f local-calls 0)))))))
    (loop for exit being the hash-keys of local-calls using (hash-value n)
          when (and (ivar-escapes-p exit) (= n (ivar-ref-count exit)))
            do (setf (ivar-escapes-p exit) nil changed t))
    (when changed
      (dolist (top (ir-program-toplevels ir))
        (mark-tail (ir-fn-body top) t)))))

(defun escaping-exit-p (ir)
  "Is there a bind-exit in IR whose exit escapes?"
  (dolist (fn (ir-fns ir) nil)
    (labels ((walk (e)
               (typecase e
                 (ir-bind-exit (when (ivar-escapes-p (ir-bind-exit-var e))
                                 (return-from escaping-exit-p t))
                               (walk (ir-bind-exit-body e)))
                 (ir-let (walk (ir-let-value e)) (walk (ir-let-body e)))
                 (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (walk (ir-letrec-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e))))))
      (walk (ir-fn-body fn)))))

(defvar *library-counters* nil
  "(ivar-counter . fn-counter) for what lowering the library's forms makes,
or NIL to number everything after the program's.")

(defun lower-control (ir)
  "Lower IR's cleanups and escaping exits; answer IR, analyzed afresh."
  (let ((*ivar-counter* (reduce #'max (ir-bound-vars ir) :key #'ivar-id :initial-value 0))
        (*fn-counter* (reduce #'max (ir-fns ir) :key #'ir-fn-id :initial-value 0))
        (*library-counters* (let ((c (ir-program-library-counters ir)))
                              (and c (cons (car c) (cdr c))))))
    (lower-everywhere ir #'lower-unwind-protect)
    (reanalyze-program ir)
    ;; Outlining a bind-exit's body can move a call of another exit out of
    ;; its function, so until none escapes
    (loop (localize-exits ir)
          (unless (escaping-exit-p ir) (return))
          (lower-everywhere ir #'lower-escaping-exit)
          (reanalyze-program ir))
    (let ((c (ir-program-library-counters ir)))
      (when (and c (or (>= (car *library-counters*) (+ (car c) +library-id-gap+))
                       (>= (cdr *library-counters*) (+ (cdr c) +library-id-gap+))))
        (error "Internal error: lowering the library made too many ids.")))
    ir))

(defun call-with-counters (fn thunk)
  "Call THUNK, numbering what it makes for FN: after the library's, if it
is the library's (*LIBRARY-COUNTERS*)."
  (if (and *library-counters* (ir-fn-library-p fn))
      (let ((*ivar-counter* (car *library-counters*))
            (*fn-counter* (cdr *library-counters*)))
        (multiple-value-prog1 (funcall thunk)
          (setf *library-counters* (cons *ivar-counter* *fn-counter*))))
      (funcall thunk)))
