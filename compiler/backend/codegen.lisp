;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; C code generation: functions, calls and proper tail calls
;;;; (CompilerPlan.md, Phase 4).
;;;;
;;;; Every function of the IR becomes a C function with the uniform entry
;;;; of runtime/c/dylan.h -- unless it is CONTIFIED: a local method that
;;;; does not escape and whose every call is a jump (a loop, most often) is
;;;; compiled as a labelled block of the C function that hosts it, its
;;;; calls as assignments to its parameters and a goto.  A tail call of a
;;;; function from its own body is likewise a goto to its top.  Every
;;;; other tail call is a C tail call the C compiler must make a jump
;;;; (musttail), or, built with -DDY_TRAMPOLINE, a return to a trampoline.
;;;;
;;;; Closures are flat: a closure holds the values of its free variables.
;;;; A variable that is both captured and assigned lives in a cell, which
;;;; the closures share.  A function with no free variables is a static
;;;; object.
;;;;
;;;; A function that known calls reach other than in tail position also
;;;; gets a SPECIALIZED entry, fnN_spec: its required arguments as C
;;;; arguments -- C doubles where unboxed -- and one more for each keyword,
;;;; DY_UNSUPPLIED if not passed, so that such a call's keywords are
;;;; matched when compiling.  Its prototype is not the uniform one, so it
;;;; can make no musttail call.  If its body makes no tail call but jumps,
;;;; it is the function's only body, and the uniform entry just passes it
;;;; its arguments.  Otherwise, if it gains something (keywords or doubles),
;;;; the body is written twice, and the specialized entry's tail calls are
;;;; ordinary calls of uniform entries: it is entered only by calls that
;;;; are not tail calls, so each such call holds at most one more frame.
;;;;
;;;; The generator walks the A-normal form with a DEST saying what to do
;;;; with each value: return it, bind or set a variable with it, keep it as
;;;; the first of several values (:MV), or drop it.  A dest may also name a
;;;; label to go to afterwards, which is how a contified block returns to
;;;; the one place that entered it.

(in-package #:dylan-compiler)

;;; ------------------------------------------------------------ Contification

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

;;; ---------------------------------------------------------- C functions

(defconstant +max-arguments+ 1024 "As DY_MAX_ARGUMENTS in dylan.h.")

(defstruct (cfun (:constructor make-cfun (fn c-name)))
  fn
  c-name
  (env '())                             ; free variables, in env order
  (extra-locals '())                    ; C temporaries: (name . type)
  (needs-mv-reset nil)
  (self-tail-call nil)                  ; did it jump to its own top?
  (spec nil))                           ; its specialized entry: NIL,
                                        ; :ONLY (its only body) or :COPY

(defvar *spec* nil
  "What the specialized entry being generated does with a tail call:
:PROBE, give up (throw to SPEC-TAIL-CALL); :ONLY, none can happen; :COPY,
make it an ordinary call.  NIL for a uniform entry.")

(defun demote-tail-call-p ()
  "Must a tail call about to be generated be made an ordinary call?"
  (ecase *spec*
    ((nil) nil)
    (:probe (throw 'spec-tail-call nil))
    (:only (error "Internal error: a tail call in a specialized entry."))
    (:copy t)))

(defvar *cfuns* nil "IR-FN -> CFUN, for every fn that is a C function.")
(defvar *cfun* nil "The CFUN being generated.")
(defvar *globals* nil "MODULE-VAR -> C name, for the program's own.")
(defvar *used-primitives* nil)
(defvar *used-primitive-entries* nil "C names of primitive entries called directly.")
(defvar *absorbed-uses* nil
  "IVAR -> how many of its uses are as the function of a call made without
its value.")
(defvar *global-functions* nil
  "Module variable -> the IR-FN of the closure it always holds (types.lisp).")
(defvar *stack-rests* nil
  "#rest IVAR -> T, for the lists that never escape (types.lisp).")
(defvar *unboxed* nil "IVAR -> T, for the variables that are C doubles (types.lisp).")
(defvar *lines* nil "The lines of the function being generated, newest first.")
(defvar *indent* 1)
(defvar *label-counter* 0)
(defvar *temp-counter* 0)

(defparameter +line-reset+ "#line DY_RESET"
  "Stands for a #line directive back to the C file itself.")

(defconstant +abi-version+ 11 "As DY_ABI_VERSION in runtime/c/dylan.h.")

(defvar *emit-lines* t
  "Write #line directives, so that the C compiler's messages and debuggers
point to the Dylan source?")
(defvar *line-location* nil
  "(file-name . line) of the last #line written in the function being
generated.")

(defun note-line ()
  "Write a #line directive for *LOCATION* if it is not the last one."
  (let ((location *location*))
    (when (and *emit-lines* (location-p location)
               (stringp (source-file-name (location-file location))))
      (let ((file (source-file-name (location-file location)))
            (line (location-line-and-column location)))
        (unless (and *line-location* (string= (car *line-location*) file)
                     (= (cdr *line-location*) line))
          (setf *line-location* (cons file line))
          (push (format nil "#line ~d ~a" line (c-name-string file)) *lines*))))))

(defun emit (control &rest arguments)
  (note-line)
  (push (concatenate 'string (make-string (* 4 *indent*) :initial-element #\Space)
                     (apply #'format nil control arguments))
        *lines*))

(defmacro indented (&body body)
  `(let ((*indent* (1+ *indent*))) ,@body))

(defun new-label (stem) (format nil "~a_~d" stem (incf *label-counter*)))

(defun new-temp (&optional (type "D"))
  (let ((name (format nil "t~d" (incf *temp-counter*))))
    (push (cons name type) (cfun-extra-locals *cfun*))
    name))

(defun host-fn (fn)
  "The fn whose C function FN's code goes in."
  (let ((group (gethash fn *groups*)))
    (if group
        (host-fn (ir-fn-parent fn))
        fn)))

;;; Variables

(defvar *closed-over* nil
  "The variables some C function holds in its environment: those captured
by a real closure, not just by a contified fn.")

(defun cell-var-p (var)
  (and (ivar-assigned-p var) (gethash var *closed-over*)))
(defun v-name (var) (format nil "v~d" (ivar-id var)))

(defun var-read (var)
  (cond ((cell-var-p var) (format nil "DY_CELL(~a)" (v-name var)))
        ((unboxed-p var) (box-double (v-name var)))
        (t (v-name var))))

(defun emit-bind (var value)
  (cond ((cell-var-p var) (emit "~a = dy_make_cell(~a);" (v-name var) value))
        ((unboxed-p var) (emit "~a = DY_DOUBLE(~a);" (v-name var) value))
        (t (emit "~a = ~a;" (v-name var) value))))

(defun emit-set (var value)
  (if (unboxed-p var)
      (emit "~a = DY_DOUBLE(~a);" (v-name var) value)
      (emit "~a = ~a;" (var-read var) value)))

;;; A C expression standing as an operand where the IR has an atom (calls.lisp)
(defstruct (c-atom (:constructor make-c-atom (expr &optional type double-p)))
  expr
  type                                  ; CLASS-INFO of its value, or NIL
  double-p)                             ; is EXPR a C double?

(defun constant-atom (a)
  "The CONST that the atom A always holds, if any: constant propagation."
  (cond ((const-p a) a)
        ((and (ivar-p a) (not (ivar-assigned-p a)) *let-values*)
         (let ((v (gethash a *let-values*)))
           (and (atom-p v) (constant-atom v))))))

(defun atom-c (a)
  (etypecase a
    (const (c-constant *cdata* (const-value a)))
    (ivar (let ((c (constant-atom a)))
            (if c (c-constant *cdata* (const-value c)) (var-read a))))
    (c-atom (c-atom-expr a))))

(defvar *thread-variables* nil "MODULE-VAR -> index, for thread variables.")

(defun global-c-name (var)
  "The C lvalue holding the module variable VAR: a static variable, or,
for a thread variable, the thread's slot for it."
  (if (module-var-thread-local-p var)
      (format nil "dy_thread->thread_variables[~d]"
              (or (gethash var *thread-variables*)
                  (setf (gethash var *thread-variables*)
                        (hash-table-count *thread-variables*))))
      (or (gethash var *globals*)
          (setf (gethash var *globals*) (format nil "dyg~d" (hash-table-count *globals*))))))

(defun unsupported (node what)
  (let ((*location* (or (and (ir-node-p node) (ir-node-location node)) *location*)))
    (compile-error "~a is not supported yet" what)))

;;; Destinations

(defstruct (dest (:constructor make-dest (kind &optional var label)))
  kind                                  ; :return :bind :set :effect :mv :c
                                        ; :raw (a C double variable)
  var                                   ; :bind/:set: IVAR; :mv/:c: C name
  label)                                ; go here afterwards, if non-NIL

(defun with-label (dest label)
  (make-dest (dest-kind dest) (dest-var dest) label))

(defun deliver (dest expr)
  "Give the C expression EXPR to DEST."
  (ecase (dest-kind dest)
    (:return
     (when (cfun-needs-mv-reset *cfun*) (emit "dy_thread->mv_count = 1;"))
     (emit "return ~a;" expr))
    (:bind (emit-bind (dest-var dest) expr))
    (:set (emit-set (dest-var dest) expr))
    (:effect (unless (pure-c-p expr) (emit "(void)(~a);" expr)))
    (:mv (emit "~a = ~a;" (dest-var dest) expr)
         (emit "dy_thread->mv_count = 1;"))
    (:c (emit "~a = ~a;" (dest-var dest) expr))
    (:raw (emit "~a = DY_DOUBLE(~a);" (dest-var dest) expr)))
  (when (dest-label dest) (emit "goto ~a;" (dest-label dest))))

;;; Expressions

(defun pure-c-p (expr)
  "Is the C expression EXPR a variable or constant, whose value can be
dropped unevaluated?"
  (or (every (lambda (c) (or (alphanumericp c) (char= c #\_))) expr)
      (some (lambda (prefix)
              (and (> (length expr) (length prefix))
                   (string= prefix expr :end2 (length prefix))))
            '("DY_INT(" "DY_CHAR(" "DY_OBJ(&" "DY_CELL(" "dy_make_double_float("))))

(defun gen (e dest)
  (let ((*location* (or (and (ir-node-p e) (ir-node-location e)) *location*)))
    (etypecase e
      ((or const ivar)
       (if (and (member (dest-kind dest) '(:bind :set)) (unboxed-p (dest-var dest)))
           (deliver-double dest (atom-double-c e))
           (deliver dest (atom-c e))))
      (ir-global-ref (deliver dest (global-ref-c e)))
      (ir-global-set
       (let ((var (ir-global-set-var e)))
         (when (module-var-predefined-p var)
           (unsupported e "Assigning a variable of the dylan module"))
         (when (gethash var *constant-globals*)
           (compile-error "~a holds a class or generic function and cannot be assigned"
                          (name-string (module-var-name var))))
         ;; Chapter 7: an error if the variable is not defined when the
         ;; set! is done.
         (emit "(void)dy_check_bound(~a, ~a);" (global-c-name var)
               (c-string-literal (map 'list #'char-code (module-var-string var))))
         (emit "~a = ~a;" (global-c-name var) (atom-c (ir-global-set-value e)))
         (deliver dest (atom-c (ir-global-set-value e)))))
      (ir-set
       (let ((var (ir-set-var e)) (value (ir-set-value e)))
         (if (unboxed-p var)
             (emit "~a = ~a;" (v-name var) (atom-double-c value))
             (emit-set var (atom-c value)))
         (unless (and (eq (dest-kind dest) :effect) (null (dest-label dest)))
           (deliver dest (atom-c value)))))
      (ir-prim (gen-primitive e dest))
      (ir-if
       (let ((test (or (constant-atom (ir-if-test e)) (ir-if-test e))))
         (cond
           ((const-p test)
             (gen (if (false-p (const-value test)) (ir-if-else e) (ir-if-then e)) dest))
           ((never-false-p test)
            ;; Known to be of a class #f is not an instance of
            (gen (ir-if-then e) dest))
           (t
             (progn
               (emit "if (~a != DY_FALSE) {" (atom-c test))
               (indented (gen (ir-if-then e) dest))
               (emit "} else {")
               (indented (gen (ir-if-else e) dest))
               (emit "}"))))))
      (ir-seq
       (loop for (f . more) on (ir-seq-forms e)
             do (gen f (if more (make-dest :effect) dest))))
      (ir-let
       (unless (or (dead-binding-p (ir-let-var e) (ir-let-value e))
                   (useless-binding-p (ir-let-var e) (ir-let-value e)))
         (gen (ir-let-value e) (make-dest :bind (ir-let-var e))))
       (gen (ir-let-body e) dest))
      (ir-mv-let (gen-mv-let e dest))
      (ir-call (gen-call e dest))
      (ir-letrec (gen-letrec e dest))
      (ir-lambda (deliver dest (closure-c (ir-lambda-fn e))))
      (ir-define-variable
       (let ((var (ir-define-variable-var e)))
         (emit "~a = ~a;" (global-c-name var) (atom-c (ir-define-variable-value e)))
         (when (module-var-thread-local-p var)
           ;; The value threads made from now on start with.
           (emit "dy_thread_variable_defaults[~d] = ~a;"
                 (gethash var *thread-variables*) (atom-c (ir-define-variable-value e))))
         (deliver dest (c-constant *cdata* (name-symbol (module-var-name var))))))
      (ir-bind-exit (gen-local-exit e dest))
      (ir-unwind (error "Internal error: an unwind-protect was not lowered."))
      ;; Generic functions and methods are static: nothing to do at run
      ;; time.  A class's slot values are evaluated where it is defined.
      (ir-define-generic (deliver dest (definition-value (ir-define-generic-var e))))
      (ir-define-method (deliver dest (definition-value (ir-define-method-var e))))
      (ir-define-class
       (gen-class-definition e)
       (deliver dest (definition-value (ir-define-class-var e))))
      (ir-define-slot
       (gen-class-definition e)
       (deliver dest (c-constant *cdata* (name-symbol (module-var-name
                                                       (slot-spec-getter (ir-define-slot-slot e))))))))))

(defun never-false-p (atom)
  (let ((type (atom-type atom)))
    (and type (not (subclass-info-p (find-class-info "<boolean>" *classes*) type)))))

(defun useless-binding-p (var value)
  "Dead code: is the let of VAR to VALUE needless, its value a constant
every use of VAR has instead, or something without effect no one uses?"
  (and (not (ivar-assigned-p var))
       (not (gethash var *closed-over*))
       (or (const-p value)
           (and (zerop (ivar-ref-count var))
                (or (atom-p value) (ir-lambda-p value)
                    (and (ir-global-ref-p value)
                         (gethash (ir-global-ref-var value) *constant-globals*)))))))

(defun definition-value (var)
  "A defining form answers the name it defines."
  (c-constant *cdata* (name-symbol (module-var-name var))))

(defun global-ref-c (e)
  (let ((var (ir-global-ref-var e)))
    (cond
      ((gethash var *constant-globals*))
      ;; The library may define one of the dylan module's variables.
      ((and (module-var-predefined-p var) (null (module-var-definitions var)))
        (let ((c (predefined-c-value var *classes*)))
          (unless c
            (if (member (dylan::dylan-symbol->string (name-symbol (module-var-name var)))
                        *static-program-exclusions* :test #'string-equal)
                (compile-error "~a cannot be used: a program's classes and generic functions are fixed when it is compiled"
                               (name-string (module-var-name var)))
                (unsupported e (format nil "~a (it has no definition in the runtime yet)"
                                       (module-var-string var)))))
          (let ((primitive (search "dy_prim_" c)))
            (when primitive
              (pushnew (subseq c (+ primitive) (1- (length c))) *used-primitives*
                       :test #'string=)))
          c))
      (t
       (format nil "dy_check_bound(~a, ~a)" (global-c-name var)
               (c-string-literal (map 'list #'char-code (module-var-string var))))))))

(defun deliver-call (dest expr)
  "Give DEST the values of EXPR, a C call of something that answers
values as a Dylan function does."
  (case (dest-kind dest)
    (:return (emit "return ~a;" expr))
    (:mv (emit "dy_thread->mv_count = 1;")
         (emit "~a = ~a;" (dest-var dest) expr)
         (when (dest-label dest) (emit "goto ~a;" (dest-label dest))))
    (:effect (emit "(void)~a;" expr)
             (when (dest-label dest) (emit "goto ~a;" (dest-label dest))))
    (t (deliver dest expr))))

(defun deliver-values (dest exprs)
  "Give DEST the values EXPRS, C expressions."
  (if (= (length exprs) 1)
      (deliver dest (first exprs))
      (let ((first (or (first exprs) "DY_FALSE")))
        (case (dest-kind dest)
          ((:return :mv)
           (loop for x in exprs for i from 0
                 do (emit "dy_thread->mv[~d] = ~a;" i x))
           (emit "dy_thread->mv_count = ~d;" (length exprs))
           (if (eq (dest-kind dest) :return)
               (emit "return ~a;" first)
               (progn (emit "~a = ~a;" (dest-var dest) first)
                      (when (dest-label dest) (emit "goto ~a;" (dest-label dest))))))
          (t (deliver dest first))))))

;;; Exits that stay in their function (control.lisp lowers the others).
;;; A call of the exit gives its arguments to the bind-exit's destination
;;; and jumps past the bind-exit.

(defvar *local-exits* nil "exit IVAR -> the DEST its calls deliver to.")

(defun gen-local-exit (e dest)
  (let* ((end (and (not (eq (dest-kind dest) :return)) (null (dest-label dest))
                   (new-label "exit")))
         (exit-dest (if end (with-label dest end) dest)))
    (setf (gethash (ir-bind-exit-var e) *local-exits*) exit-dest)
    (gen (ir-bind-exit-body e) dest)
    (when end (emit "~a:;" end))))

(defun gen-primitive (e dest)
  (let ((args (mapcar #'atom-c (ir-prim-arguments e))))
    (case (ir-prim-name e)
      (:call-with-exit (deliver-call dest (format nil "dy_call_with_exit(~a)" (first args))))
      (:unwind-protect
       (deliver-call dest (format nil "dy_unwind_protect(~a, ~a)" (first args) (second args))))
      (:handler-bind
       (deliver-call dest (format nil "dy_handler_bind(~{~a~^, ~})" args)))
      (:check-type
       (emit "dy_check_type(~a, ~a);" (first args) (second args))
       (deliver dest "DY_FALSE"))
      (:pair (deliver dest (format nil "dy_pair(~a, ~a)" (first args) (second args))))
      (:append2 (deliver dest (format nil "dy_append2(~a, ~a)" (first args) (second args))))
      (:list->vector (deliver dest (format nil "dy_list_to_vector(~a)" (first args))))
      (t (unsupported e (format nil "the primitive ~(~a~)" (ir-prim-name e)))))))

(defun gen-mv-let (e dest)
  (let ((primary (new-temp))
        (count (new-temp "intptr_t"))
        (values (new-temp "const D *")))
    (gen (ir-mv-let-value e) (make-dest :mv primary))
    (emit "~a = dy_thread->mv_count;" count)
    (emit "~a = ~a == 1 ? &~a : dy_thread->mv;" values count primary)
    (loop for var in (ir-mv-let-vars e)
          for i from 0
          do (emit-bind var (format nil "~a > ~d ? ~a[~d] : DY_FALSE" count i values i)))
    (when (ir-mv-let-rest e)
      (emit-bind (ir-mv-let-rest e)
                 (format nil "dy_list_of(~a, ~d, ~a)" values
                         (length (ir-mv-let-vars e)) count)))
    (gen (ir-mv-let-body e) dest)))

;;; Calls

(defun call-arguments-c (args)
  "Store the arguments past the fourth in the argument area; answer the
C expressions for a0..a3 and the count."
  (let ((cs (mapcar #'atom-c args)))
    (when (> (length cs) +max-arguments+)
      (compile-error "a call with more than ~d arguments" +max-arguments+))
    (loop for c in (nthcdr 4 cs)
          for i from 0
          do (emit "dy_thread->args[~d] = ~a;" i c))
    (values (loop for i below 4 collect (or (nth i cs) "0"))
            (length cs))))

(defun gen-call (e dest)
  (let* ((f (ir-call-function e))
         (target (or (call-target e) (global-callee e)))
         (group (and target (gethash target *groups*)))
         (tail (and (eq (dest-kind dest) :return) (ir-call-tail-p e))))
    (cond
      ((eq (ir-call-kind e) :exit)
       (deliver-values (or (gethash f *local-exits*)
                           (error "Internal error: an exit with no bind-exit."))
                       (mapcar #'atom-c (ir-call-arguments e))))
      (group (gen-contified-call e target group dest))
      ((and tail target (eq target (cfun-fn *cfun*)) (simple-fn-p target)
            (= (length (ir-call-arguments e)) (length (ir-fn-required target))))
       ;; A tail call of itself: a jump to its top.
       (gen-jump (ir-fn-required target) (ir-call-arguments e))
       (setf (cfun-self-tail-call *cfun*) t)
       (emit "goto top;"))
      ((gen-known-call e dest tail))
      ((and (not tail) target
            (let ((call (spec-call-c target (atom-c f) (ir-call-arguments e))))
              (and call (progn (emit-call dest nil call nil) t)))))
      (t
       (multiple-value-bind (as n) (call-arguments-c (ir-call-arguments e))
         (let* ((direct (and target (gethash target *cfuns*)))
                (fexpr (atom-c f))
                (call (if direct
                          (format nil "DY_CALL_DIRECT(~a, ~a, ~{~a, ~}~d)"
                                  (cfun-c-name direct) fexpr as n)
                          (format nil "DY_CALL(~a, ~{~a, ~}~d)" fexpr as n))))
           (cond ((and tail (not (demote-tail-call-p)))
                  (emit "~a;" (if direct
                                  (format nil "DY_TAIL_CALL_DIRECT(~a, ~a, ~{~a, ~}~d)"
                                          (cfun-c-name direct) fexpr as n)
                                  (format nil "DY_TAIL_CALL(~a, ~{~a, ~}~d)" fexpr as n))))
                 ((eq (dest-kind dest) :return)
                  (emit "return ~a;" call))
                 ((eq (dest-kind dest) :mv)
                  (emit "dy_thread->mv_count = 1;")
                  (emit "~a = ~a;" (dest-var dest) call)
                  (when (dest-label dest) (emit "goto ~a;" (dest-label dest))))
                 ((eq (dest-kind dest) :effect)
                  (emit "(void)~a;" call)
                  (when (dest-label dest) (emit "goto ~a;" (dest-label dest))))
                 (t (deliver dest call)))))))))

(defun spec-call-c (fn fexpr args)
  "A C call of FN's specialized entry, with FEXPR as self, on ARGS, if it
has one and they fit it: its required arguments, then keyword/value pairs
with constant keywords it takes."
  (let ((cfun (gethash fn *cfuns*)))
    (when (and cfun (cfun-spec cfun))
      (let* ((required (ir-fn-required fn))
             (r (length required))
             (keys (ir-fn-keys fn))
             (n (length args)))
        (when (and (>= n r) (evenp (- n r)) (or keys (= n r)))
          (let ((values (make-list (length keys))))
            (loop for (k v) on (nthcdr r args) by #'cddr
                  do (let* ((c (and (not (c-atom-p k)) (constant-atom k)))
                            (i (and c (position (const-value c) keys :key #'ir-key-keyword))))
                       (unless i (return-from spec-call-c nil))
                       ;; The leftmost of a keyword's values
                       (unless (nth i values) (setf (nth i values) (atom-c v)))))
            (format nil "~a_spec(~a~{, ~a~})" (cfun-c-name cfun) fexpr
                    (append (loop for p in required for a in args
                                  collect (if (unboxed-p p) (atom-double-c a) (atom-c a)))
                            (substitute "DY_UNSUPPLIED" nil values)))))))))

(defun gen-jump (params args)
  "Bind PARAMS to ARGS afresh, all at once."
  (let ((temps (mapcar (lambda (p) (new-temp (if (unboxed-p p) "double" "D"))) params)))
    (loop for temp in temps for a in args for p in params
          do (emit "~a = ~a;" temp (if (unboxed-p p) (atom-double-c a) (atom-c a))))
    (loop for p in params for temp in temps
          do (if (unboxed-p p)
                 (emit "~a = ~a;" (v-name p) temp)
                 (emit-bind p temp)))))

(defun gen-contified-call (e target group dest)
  (cond ((eq dest (group-dest group)))
        ((and (eq (group-mode group) :single) (eq e (group-entry group))
              (null (group-dest group)))
         (setf (group-dest group) (with-label dest (new-label "after"))))
        (t (error "Internal error: a contified call with a stray destination.")))
  (gen-jump (ir-fn-required target) (ir-call-arguments e))
  (emit "goto ~a;" (group-label group target))
  (when (eq e (group-entry group))
    (emit "~a:;" (dest-label (group-dest group)))))

(defun group-label (group fn)
  (or (gethash fn (group-labels group))
      (setf (gethash fn (group-labels group))
            (new-label (format nil "~a" (c-identifier (name-string (ir-fn-name fn))))))))

;;; Letrec and closures

(defun fn-info-c (fn)
  (function-info-c (length (ir-fn-required fn)) (ir-fn-rest fn)
                   (let ((k (ir-fn-keys fn))) (if (listp k) (mapcar #'ir-key-keyword k) k))))

(defun closure-c (fn)
  "A C expression for a closure of FN made now."
  (let ((cfun (gethash fn *cfuns*)))
    (if (null (cfun-env cfun))
        (format nil "DY_OBJ(&~a_closure)" (cfun-c-name cfun))
        (let ((temp (new-temp)))
          (emit "~a = dy_make_closure(~a, ~d);" temp (cfun-c-name cfun)
                (length (cfun-env cfun)))
          (emit "((DyFunction *)DY_PTR(~a))->info = ~a;" temp (fn-info-c fn))
          (fill-env temp cfun)
          (record-specializers temp fn)
          temp))))

(defun record-specializers (closure fn)
  "Give the closure CLOSURE of FN, a method made by method, the
specializers it checks, for reflection (method-specializers,
applicable-method?)."
  (when (and (some #'identity (ir-fn-specializers fn)) (not (gethash fn *method-of-fn*)))
    (let ((vector (new-temp)))
      (emit "~a = dy_make_vector(~d, DY_OBJ(&dy_class_object));" vector
            (length (ir-fn-specializers fn)))
      (loop for spec in (ir-fn-specializers fn)
            for i from 0
            when spec
              do (emit "((DyVector *)DY_PTR(~a))->data[~d] = ~a;" vector i (atom-c spec)))
      (emit "((DyFunction *)DY_PTR(~a))->specializers = ~a;" closure vector))))

(defun fill-env (closure cfun)
  (loop for var in (cfun-env cfun)
        for i from 0
        ;; A cell variable's cell, not its value.
        do (emit "((DyFunction *)DY_PTR(~a))->env[~d] = ~a;" closure i (v-name var))))

(defun gen-letrec (e dest)
  (let* ((fns (ir-letrec-fns e))
         (contified (remove-if-not (lambda (f) (gethash f *groups*)) fns))
         (group (and contified (gethash (first contified) *groups*))))
    ;; The closures, all made before any is filled, since they may refer
    ;; to each other.
    (let ((pending '()))
      (loop for var in (ir-letrec-vars e)
            for fn in fns
            unless (member fn contified)
              do (let ((cfun (gethash fn *cfuns*)))
                   (if (null (cfun-env cfun))
                       (emit-bind var (format nil "DY_OBJ(&~a_closure)" (cfun-c-name cfun)))
                       (progn
                         (emit-bind var (format nil "dy_make_closure(~a, ~d)"
                                                (cfun-c-name cfun) (length (cfun-env cfun))))
                         (emit "((DyFunction *)DY_PTR(~a))->info = ~a;" (var-read var) (fn-info-c fn))
                         (push (cons var cfun) pending)))))
      (loop for (var . cfun) in (reverse pending)
            do (fill-env (var-read var) cfun)
               (record-specializers (var-read var) (cfun-fn cfun))))
    (if (null group)
        (gen (ir-letrec-body e) dest)
        (let ((end (and (not (eq (dest-kind dest) :return)) (new-label "end"))))
          (when (eq (group-mode group) :tail)
            (setf (group-dest group) (if end (with-label dest end) dest)))
          (gen (ir-letrec-body e) (if (eq (group-mode group) :tail) (group-dest group) dest))
          (when end (emit "goto ~a;" end))
          (unless (group-dest group)
            ;; Never entered.
            (setf (group-dest group) (make-dest :effect nil end)))
          (dolist (fn contified)
            (emit "~a:;" (group-label group fn))
            (gen (ir-fn-body fn) (group-dest group)))
          (when end (emit "~a:;" end))))))

;;; Functions

(defun owned-vars (fn)
  "The variables a C function for FN declares: those its own code binds,
and those of the fns contified into it."
  (let ((vars '()))
    (labels ((fn-vars (f)
               (dolist (v (ir-fn-required f)) (push v vars))
               (when (ir-fn-next f) (push (ir-fn-next f) vars))
               (when (ir-fn-rest f) (push (ir-fn-rest f) vars))
               (when (listp (ir-fn-keys f))
                 (dolist (k (ir-fn-keys f))
                   (walk (ir-key-default k))
                   (push (ir-key-var k) vars)))
               (walk (ir-fn-body f)))
             (walk (e)
               (typecase e
                 (ir-let (walk (ir-let-value e)) (push (ir-let-var e) vars)
                         (walk (ir-let-body e)))
                 (ir-mv-let (walk (ir-mv-let-value e))
                            (dolist (v (ir-mv-let-vars e)) (push v vars))
                            (when (ir-mv-let-rest e) (push (ir-mv-let-rest e) vars))
                            (walk (ir-mv-let-body e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (loop for v in (ir-letrec-vars e)
                                  for f in (ir-letrec-fns e)
                                  do (if (gethash f *groups*)
                                         (fn-vars f)    ; no closure, no variable
                                         (push v vars)))
                            (walk (ir-letrec-body e)))
                 (ir-bind-exit (push (ir-bind-exit-var e) vars)
                               (walk (ir-bind-exit-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e))))))
      (fn-vars fn))
    (remove-duplicates vars)))

(defun makes-calls-p (fn)
  "Might FN's C function call another function other than in tail
position?  (If so, a call may have changed the count of values.)"
  (labels ((walk (e)
             (typecase e
               (ir-call (and (not (gethash (call-target e) *groups*))
                             (not (ir-call-tail-p e))
                             (not (eq (ir-call-kind e) :exit))))
               ;; These call Dylan functions.
               (ir-prim (member (ir-prim-name e) '(:call-with-exit :unwind-protect :handler-bind)))
               (ir-bind-exit (walk (ir-bind-exit-body e)))
               (ir-let (or (walk (ir-let-value e)) (walk (ir-let-body e))))
               (ir-mv-let (or (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e))))
               (ir-seq (some #'walk (ir-seq-forms e)))
               (ir-if (or (walk (ir-if-then e)) (walk (ir-if-else e))))
               (ir-letrec (or (some (lambda (f) (and (gethash f *groups*) (walk (ir-fn-body f))))
                                    (ir-letrec-fns e))
                              (walk (ir-letrec-body e))))
               (t nil)))
           (fn-walk (f)
             (or (walk (ir-fn-body f))
                 (and (listp (ir-fn-keys f))
                      (some (lambda (k) (walk (ir-key-default k))) (ir-fn-keys f))))))
    (fn-walk fn)))

(defun gen-function (cfun)
  "The C text of CFUN's functions."
  (case (cfun-spec cfun)
    (:only (concatenate 'string (gen-function-entry cfun :only) (gen-spec-wrapper cfun)))
    (:copy (concatenate 'string (gen-function-entry cfun :copy)
                        (gen-function-entry (copy-cfun cfun) nil)))
    (t (gen-function-entry cfun nil))))

(defun spec-parameters (fn)
  "The C parameters of FN's specialized entry after self: (type . name)."
  (append (loop for var in (ir-fn-required fn) for i from 0
                collect (cons (if (unboxed-p var) "double" "D") (format nil "p~d" i)))
          (loop for k in (ir-fn-keys fn) for i from 0
                collect (cons "D" (format nil "k~d" i)))))

(defun spec-prototype (cfun)
  (format nil "static D ~a_spec(D~{, ~a~});" (cfun-c-name cfun)
          (mapcar #'car (spec-parameters (cfun-fn cfun)))))

(defun gen-spec-wrapper (cfun)
  "The uniform entry of CFUN, whose only body is its specialized entry."
  (let* ((fn (cfun-fn cfun))
         (*cfun* (make-cfun fn (cfun-c-name cfun)))
         (*emit-lines* nil)
         (*lines* '())
         (*indent* 1)
         (required (ir-fn-required fn))
         (r (length required))
         (keys (ir-fn-keys fn)))
    (if keys
        (progn
          (when (> r 0)
            (emit "if (DY_UNLIKELY(argc < ~d)) dy_wrong_argument_count(self, argc);" r))
          (gen-keyword-scan fn r))
        (emit "if (DY_UNLIKELY(argc != ~d)) dy_wrong_argument_count(self, argc);" r))
    (emit "return ~a_spec(self~{, ~a~});" (cfun-c-name cfun)
          (append (loop for var in required for i from 0
                        for a = (if (< i 4) (format nil "a~d" i) (format nil "dy_thread->args[~d]" (- i 4)))
                        collect (if (unboxed-p var) (format nil "DY_DOUBLE(~a)" a) a))
                  (mapcar (lambda (k) (v-name (ir-key-var k))) keys)))
    (with-output-to-string (out)
      (format out "static D ~a(D self, D a0, D a1, D a2, D a3, intptr_t argc) {~%"
              (cfun-c-name cfun))
      (format out "    (void)self; (void)a0; (void)a1; (void)a2; (void)a3; (void)argc;~%")
      (when keys
        (format out "    D ~{~a~^, ~};~%" (mapcar (lambda (k) (v-name (ir-key-var k))) keys)))
      (dolist (local (reverse (cfun-extra-locals *cfun*)))
        (format out "    ~a ~a;~%" (cdr local) (car local)))
      (dolist (line (reverse *lines*))
        (format out "~a~%" line))
      (format out "}~%"))))

(defun spec-candidate-p (cfun)
  "Could CFUN's function have a specialized entry?"
  (let ((fn (cfun-fn cfun)))
    (and (not (eq (ir-fn-kind fn) :toplevel))
         (null (ir-fn-rest fn))
         (listp (ir-fn-keys fn))
         (or (null (ir-fn-next fn)) (zerop (ivar-ref-count (ir-fn-next fn))))
         (<= (+ (length (ir-fn-required fn)) (length (ir-fn-keys fn))) 64))))

(defun spec-callees (fns)
  "The fns that some call not in tail position may call through a
specialized entry."
  (let ((found (make-hash-table :test #'eq)))
    (labels ((walk (e)
               (typecase e
                 (ir-call (unless (ir-call-tail-p e)
                            (let ((fn (or (known-callee e)
                                          (multiple-value-bind (kind what) (call-callee e)
                                            (let ((m (and (eq kind :generic) (spec-method what))))
                                              (and m (gmethod-fn m)))))))
                              (when fn (setf (gethash fn found) t)))))
                 (ir-let (walk (ir-let-value e)) (walk (ir-let-body e)))
                 (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (walk (ir-letrec-body e)))
                 (ir-bind-exit (walk (ir-bind-exit-body e)))
                 (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e))))))
      (dolist (fn fns)
        (when (listp (ir-fn-keys fn))
          (dolist (k (ir-fn-keys fn)) (walk (ir-key-default k))))
        (walk (ir-fn-body fn))))
    found))

(defun choose-spec-entries (cfuns fns)
  "Decide which of CFUNS have specialized entries, and of what kind."
  (let ((callees (spec-callees fns)))
    (dolist (cfun cfuns)
      (let ((fn (cfun-fn cfun)))
        (when (and (gethash fn callees) (spec-candidate-p cfun))
          (setf (cfun-spec cfun)
                (cond ((catch 'spec-tail-call
                         (let ((*spec* :probe))
                           (gen-function-entry (copy-cfun cfun) :probe))
                         t)
                       :only)
                      ((or (ir-fn-keys fn) (some #'unboxed-p (ir-fn-required fn)))
                       :copy)))
          ;; The probe may have left its contified groups' destinations.
          (loop for f being the hash-keys of *groups* using (hash-value group)
                when (eq (host-fn f) fn)
                  do (setf (group-dest group) nil)))))))

(defun gen-function-entry (cfun spec)
  "The C text of CFUN's uniform entry with its body, or, if SPEC, its
specialized entry, made with *SPEC* SPEC."
  (let* ((fn (cfun-fn cfun))
         (*spec* spec)
         (*cfun* cfun)
         (*line-location* nil)
         (*lines* '())
         (*indent* 1)
         (*location* (ir-fn-location fn))
         (required (ir-fn-required fn))
         (r (length required))
         (keys (ir-fn-keys fn)))
    (setf (cfun-needs-mv-reset cfun) (makes-calls-p fn))
    ;; Arguments
    (cond (spec
           (loop for var in required
                 for i from 0
                 do (if (unboxed-p var)
                        (emit "~a = p~d;" (v-name var) i)
                        (emit-bind var (format nil "p~d" i)))))
          (t
           (cond ((and (null (ir-fn-rest fn)) (null keys))
                  (emit "if (DY_UNLIKELY(argc != ~d)) dy_wrong_argument_count(self, argc);" r))
                 ((> r 0)
                  (emit "if (DY_UNLIKELY(argc < ~d)) dy_wrong_argument_count(self, argc);" r)))
           (when (> r +max-arguments+) (compile-error "a method with more than ~d parameters"
                                                      +max-arguments+))
           (loop for var in required
                 for i from 0
                 do (emit-bind var (if (< i 4)
                                       (format nil "a~d" i)
                                       (format nil "dy_thread->args[~d]" (- i 4)))))))
    (loop for var in (cfun-env cfun)
          for i from 0
          do (emit "~a = ((DyFunction *)DY_PTR(self))->env[~d];" (v-name var) i))
    (unless (gethash fn *method-of-fn*)
      ;; A method made by method checks its specializers when called
      ;; (define-method's are checked by dispatch, or its direct entry).
      (loop for spec in (ir-fn-specializers fn)
            for var in required
            when spec
              do (emit "dy_check_type(~a, ~a);" (var-read var) (atom-c spec))))
    (cond ((and (ir-fn-next fn) (zerop (ivar-ref-count (ir-fn-next fn)))))
          ((and (ir-fn-next fn) (gethash fn *method-of-fn*))
           ;; Its caller left its next method in the thread.  A next method
           ;; that may be called with no arguments must remember these.
           (let ((temp (new-temp)))
             (emit "~a = dy_thread->next_method;" temp)
             (when (needs-captured-next-p (ir-fn-next fn) fn)
               (emit "if (~a != DY_FALSE) ~a = dy_capture_next(~a, argc, a0, a1, a2, a3);"
                     temp temp temp))
             (emit-bind (ir-fn-next fn) temp)))
          ((ir-fn-next fn)
           ;; Called directly, a method has no next method.
           (emit-bind (ir-fn-next fn) "DY_FALSE")))
    (let ((rest (ir-fn-rest fn)))
      (when rest
        (emit-bind rest (if (stack-rest-p rest)
                            (progn
                              (push (cons (format nil "~a_pairs[DY_STACK_PAIRS]" (v-name rest))
                                          "DyPair")
                                    (cfun-extra-locals *cfun*))
                              (format nil "argc - ~d <= DY_STACK_PAIRS ? dy_collect_arguments_into(~a_pairs, argc, ~d, a0, a1, a2, a3) : dy_collect_arguments(argc, ~d, a0, a1, a2, a3)"
                                      r (v-name rest) r r))
                            (format nil "dy_collect_arguments(argc, ~d, a0, a1, a2, a3)" r))))
      (cond ((and keys spec)
             ;; The keyword arguments, matched by the caller
             (loop for k in keys for i from 0
                   do (emit "~a = k~d;" (v-name (ir-key-var k)) i))
             (gen-keyword-defaults fn))
            (keys
             ;; The keyword arguments, read where they were passed
             (gen-keyword-scan fn r)
             (gen-keyword-defaults fn))))
    (let ((top-index (length *lines*)))
      (gen (ir-fn-body fn) (make-dest :return))
      (when (cfun-self-tail-call cfun)
        ;; The label goes after the prologue.
        (let ((tail (nthcdr (- (length *lines*) top-index) *lines*))
              (head (subseq *lines* 0 (- (length *lines*) top-index))))
          (setf *lines* (append head (list "top:;") tail)))))
    (with-output-to-string (out)
      (if spec
          (format out "static D ~a_spec(D self~:{, ~a ~a~}) {~%    (void)self;~%"
                  (cfun-c-name cfun)
                  (mapcar (lambda (p) (list (car p) (cdr p))) (spec-parameters fn)))
          (progn
            (format out "static D ~a(D self, D a0, D a1, D a2, D a3, intptr_t argc) {~%"
                    (cfun-c-name cfun))
            (format out "    (void)self; (void)a0; (void)a1; (void)a2; (void)a3; (void)argc;~%")))
      (let ((vars (sort (copy-list (append (owned-vars fn) (cfun-env cfun))) #'<
                        :key #'ivar-id)))
        (let ((boxed (remove-if #'unboxed-p vars))
              (doubles (remove-if-not #'unboxed-p vars)))
          (when boxed
            (format out "    D ~{~a~^, ~};~%" (mapcar #'v-name boxed)))
          (when doubles
            (format out "    double ~{~a~^, ~};~%" (mapcar #'v-name doubles)))))
      (dolist (local (reverse (cfun-extra-locals cfun)))
        (format out "    ~a ~a;~%" (cdr local) (car local)))
      (dolist (line (reverse *lines*))
        (format out "~a~%" line))
      (format out "}~%")
      (when *line-location*
        ;; Back to the C file (output.lisp writes the line number).
        (format out "~a~%" +line-reset+)))))

(defun needs-captured-next-p (var fn)
  "Might the next method VAR be called with no arguments, which means
with the arguments FN was given?"
  (or (> (ivar-ref-count var) (ivar-call-count var))  ; used as a value
      (let ((found nil))
        (labels ((walk-fn (f)
                   (when (listp (ir-fn-keys f))
                     (dolist (k (ir-fn-keys f)) (walk (ir-key-default k))))
                   (walk (ir-fn-body f)))
                 (walk (e)
                   (typecase e
                     (ir-call (when (and (eq (ir-call-function e) var)
                                         (null (ir-call-arguments e)))
                                (setf found t)))
                     (ir-let (walk (ir-let-value e)) (walk (ir-let-body e)))
                     (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                     (ir-seq (mapc #'walk (ir-seq-forms e)))
                     (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                     (ir-letrec (mapc #'walk-fn (ir-letrec-fns e)) (walk (ir-letrec-body e)))
                     (ir-lambda (walk-fn (ir-lambda-fn e)))
                     (ir-bind-exit (walk (ir-bind-exit-body e)))
                     (ir-unwind (walk (ir-unwind-protected e)) (walk (ir-unwind-cleanup e))))))
          (walk-fn fn))
        found)))

(defun gen-keyword-scan (fn start)
  ;; Chapter 6: a method accepts the keywords it names, or any if it has
  ;; #rest; called through its generic function, it accepts what that
  ;; has checked.  The arguments from START on are keyword/value pairs.
  (let* ((keys (ir-fn-keys fn))
         (any (or (ir-fn-rest fn) (gethash fn *method-of-fn*)))
         (keys (if (listp keys) keys '()))
         (i (new-temp "intptr_t"))
         (key (new-temp)))
    (dolist (k keys) (emit "~a = DY_UNSUPPLIED;" (v-name (ir-key-var k))))
    (emit "for (~a = ~d; ~a < argc; ~a += 2) {" i start i i)
    (indented
      (emit "if (~a + 1 >= argc) dy_odd_keyword_arguments(self);" i)
      (emit "~a = dy_argument(~a, a0, a1, a2, a3);" key i)
      (if (and (null keys) any)
          nil
          (progn
            (loop for k in keys
                  for first = t then nil
                  do (emit "~:[} else ~;~]if (~a == ~a) {" first key
                           (c-constant *cdata* (ir-key-keyword k)))
                     (indented
                       (emit "if (~a == DY_UNSUPPLIED) ~a = dy_argument(~a + 1, a0, a1, a2, a3);"
                             (v-name (ir-key-var k)) (v-name (ir-key-var k)) i)))
            (cond ((null keys)
                   (emit "dy_unknown_keyword(self, ~a);" key))
                  (any (emit "}"))
                  (t (emit "} else {")
                     (indented (emit "dy_unknown_keyword(self, ~a);" key))
                     (emit "}"))))))
    (emit "}")))

(defun gen-keyword-defaults (fn)
  ;; Defaults, in order: each sees the parameters before it.
  (let ((keys (ir-fn-keys fn)))
    (dolist (k (if (listp keys) keys '()))
      (let ((var (ir-key-var k)))
        (emit "if (~a == DY_UNSUPPLIED) {" (v-name var))
        (indented
          (gen (ir-key-default k) (make-dest :c (v-name var))))
        (emit "}")
        (when (cell-var-p var)
          (emit "~a = dy_make_cell(~a);" (v-name var) (v-name var)))))))

;;; Programs

(defun inert-toplevel-p (fn)
  "Is FN a top-level form that does nothing when run: a define-method or
define-generic-function, whose work the compiler has done?"
  (and (eq (ir-fn-kind fn) :toplevel)
       (labels ((inert (e)
                  (typecase e
                    (ir-let (and (typep (ir-let-value e) '(or const ivar ir-global-ref))
                                 (inert (ir-let-body e))))
                    ((or ir-define-method ir-define-generic) t))))
         (inert (ir-fn-body fn)))))

(defvar *debug* nil
  "Compile for debugging: trust no type found when compiling, so that every
inline operation tests its arguments when run.")

(defun fn-module-name (fn units)
  "The name of the module FN's code was written in, a string."
  (let* ((top (loop for f = fn then (ir-fn-parent f)
                    while (ir-fn-parent f)
                    finally (return f)))
         (location (ir-fn-location top))
         (unit (and location (find (location-file location) units :key #'unit-file))))
    (if unit
        (string-downcase (dylan::dylan-symbol->string (dylan-module-name (unit-module unit))))
        "dylan-user")))

(defun generate-c (ir stream)
  "Write the C program for IR to STREAM.  Answer a table: the C name of
each of its Dylan functions -> the name of the module it was written in."
  (let ((*groups* (make-hash-table :test #'eq))
        (*cfuns* (make-hash-table :test #'eq))
        (*cdata* (make-cdata))
        (*classes* (prepare-classes (built-in-class-infos)))
        (*globals* (make-hash-table :test #'eq))
        (*closed-over* (make-hash-table :test #'eq))
        (*local-exits* (make-hash-table :test #'eq))
        (*thread-variables* (make-hash-table :test #'eq))
        (*used-primitive-entries* '())
        (*absorbed-uses* nil)
        (*unboxed* nil)
        (*stack-rests* nil)
        (*global-functions* (make-hash-table :test #'eq))
        (*dylan-modules* (program-modules (ir-program-source ir)))
        (dylan::*symbol-spellings* (or (program-spellings (ir-program-source ir))
                                       dylan::*symbol-spellings*))
        (*let-values* (make-hash-table :test #'eq))
        (*global-definitions* (make-hash-table :test #'eq))
        (*assigned-globals* (make-hash-table :test #'eq))
        (*gclasses* (make-hash-table :test #'eq))
        (*gfuns* (make-hash-table :test #'eq))
        (*method-of-fn* (make-hash-table :test #'eq))
        (*constant-globals* (make-hash-table :test #'eq))
        (*sdef-of-node* (make-hash-table :test #'eq))
        (*instance-slot-numbers* (make-hash-table :test #'eq))
        (*object-text* '())
        (*object-decls* '())
        (*definition-order* 0)
        (*reflective-methods* t)
        (*sdef-counter* 0)
        (*used-primitives* '())
        (*label-counter* 0)
        (*temp-counter* 0)
        (fns (ir-fns ir)))
    (setf *reflective-methods* (uses-method-reflection-p ir))
    (prepare-object-system ir)
    (dolist (fn fns)
      (dolist (letrec (letrecs-in (ir-fn-body fn)))
        (decide-contification letrec)))
    (let ((cfuns '()))
      (loop for fn in fns
            for i from 0
            unless (or (gethash fn *groups*) (inert-toplevel-p fn))
              do (let ((cfun (make-cfun fn (format nil "fn~d_~a" i
                                                   (if (ir-fn-name fn)
                                                       (c-identifier (name-string (ir-fn-name fn)))
                                                       (string-downcase (ir-fn-kind fn)))))))
                   (setf (cfun-env cfun)
                         (sort (remove-duplicates
                                (append (ir-fn-free-vars fn)
                                        ;; A method's specializers, which it
                                        ;; checks when called.
                                        (and (not (gethash fn *method-of-fn*))
                                             (remove-if-not #'ivar-p (ir-fn-specializers fn)))))
                               #'< :key #'ivar-id))
                   (let ((m (gethash fn *method-of-fn*)))
                     (when m (setf (gmethod-entry-c m) (cfun-c-name cfun))))
                   (setf (gethash fn *cfuns*) cfun)
                   (push cfun cfuns)))
      (setf cfuns (nreverse cfuns))
      (dolist (cfun cfuns)
        (dolist (var (cfun-env cfun))
          (setf (gethash var *closed-over*) t)))
      (count-absorbed-uses fns)
      (unless *debug*
        (infer-types fns)
        (choose-unboxed fns))
      (find-stack-rests fns)
      (choose-spec-entries cfuns fns)
      (let ((texts (mapcar #'gen-function cfuns))
            (closure-lines
              (loop for cfun in cfuns
                    when (null (cfun-env cfun))
                      collect (format nil "static DyFunction ~a_closure = { &dy_wrapper_method, ~a, DY_FALSE, ~a };"
                                      (cfun-c-name cfun) (cfun-c-name cfun)
                                      (if (eq (ir-fn-kind (cfun-fn cfun)) :toplevel)
                                          "DY_FALSE"
                                          (fn-info-c (cfun-fn cfun)))))))
        (finish-object-system)
        (emit-program-data *classes* *cdata* stream :prelude (reverse *object-decls*)
                           :thread-variable-count (hash-table-count *thread-variables*))
        (format stream "~%/* Program */~%~%")
        (format stream "_Static_assert(DY_ABI_VERSION == ~d, \"dylan.h is not the runtime this compiler generates C for\");~%~%"
                +abi-version+)
        (dolist (p *used-primitives*)
          (format stream "extern DyFunction ~a;~%" p))
        (dolist (p *used-primitive-entries*)
          (format stream "D ~a(D, D, D, D, D, intptr_t);~%" p))
        (dolist (cfun cfuns)
          (format stream "static D ~a(D, D, D, D, D, intptr_t);~%" (cfun-c-name cfun))
          (when (cfun-spec cfun)
            (format stream "~a~%" (spec-prototype cfun))))
        (terpri stream)
        (dolist (line closure-lines) (format stream "~a~%" line))
        (terpri stream)
        (loop for var being the hash-keys of *globals* using (hash-value name)
              do (format stream "static D ~a = DY_UNBOUND;   /* ~a */~%"
                         name (module-var-string var)))
        (terpri stream)
        (dolist (text texts) (format stream "~a~%" text))
        (dolist (text (reverse *object-text*)) (format stream "~a~%" text))
        (format stream "int main(void) {~%    dy_init();~%")
        (dolist (top (ir-program-toplevels ir))
          (unless (inert-toplevel-p top)
            (format stream "    DY_CALL(DY_OBJ(&~a_closure), 0, 0, 0, 0, 0);~%"
                    (cfun-c-name (gethash top *cfuns*)))))
        (format stream "    return 0;~%}~%")
        (let ((modules (make-hash-table :test #'equal))
              (units (program-units (ir-program-source ir))))
          (dolist (cfun cfuns)
            (setf (gethash (cfun-c-name cfun) modules)
                  (fn-module-name (cfun-fn cfun) units))
            (when (cfun-spec cfun)
              (setf (gethash (format nil "~a_spec" (cfun-c-name cfun)) modules)
                    (fn-module-name (cfun-fn cfun) units))))
          modules)))))
