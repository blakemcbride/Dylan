;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Calls whose function is known at compile time (CompilerPlan.md, Phase
;;;; 9: dispatch resolution and inlining).
;;;;
;;;; A call's function is known when it is a module variable that holds a
;;;; generic function, or a function the runtime writes in C.  (A module
;;;; variable that always holds one function is called directly by
;;;; codegen.lisp; see types.lisp.)  Then:
;;;;
;;;;   - A call of +, -, *, /, <, > or = with two arguments (one, for - and
;;;;     /) is a call of the binary (or unary) generic function.
;;;;   - Some of the library's methods are done inline, behind a test of
;;;;     the arguments (*method-inlines*): arithmetic and comparison on
;;;;     integers and double floats, and size, element and the iteration
;;;;     protocol on lists, simple object vectors and byte strings -- when
;;;;     no method but the library's could apply to such arguments, which
;;;;     the static program lets the compiler know.  The test is left out
;;;;     for arguments whose class types.lisp has found.  Anything else
;;;;     goes to the generic function.
;;;;   - The getter or setter of an untyped instance slot with no other
;;;;     methods reads or writes the slot inline.
;;;;   - make of a class the program defines, which only the library's
;;;;     method on <class> makes, calls the class's maker.
;;;;   - A generic function with one method, on <object> for every
;;;;     argument, is not dispatched: its method is called.
;;;;   - Any other generic function's dispatch function is called directly.
;;;;   - A function written in C is called directly, and id?, not, car,
;;;;     cdr and instance? of a known class are done inline.
;;;;
;;;; The read of a module variable whose only uses are such calls is
;;;; dropped.

(in-package #:dylan-compiler)

(defparameter *variadic-rewrites*
  '(("+" 2 "binary+") ("*" 2 "binary*") ("-" 2 "binary-") ("-" 1 "unary-")
    ("/" 2 "binary/") ("/" 1 "unary/") ("<" 2 "binary<") ("=" 2 "binary=")
    (">" 2 "binary<" :swap)))

(defparameter *variadic-folds*
  '(("+" "binary+") ("*" "binary*") ("-" "binary-") ("/" "binary/"))
  "With three or more arguments, these combine them in pairs from the left
(chapter 14), as calls of the binary generic function.")


(defparameter *method-inlines*
  `(("binary+" 2
     (("<integer>" "<integer>") "<integer>" "dy_int_add({0}, {1})" :fold ,#'+)
     (("<double-float>" "<double-float>") "<double-float>" "{d0} + {d1}")
     (("<integer>" "<double-float>") "<double-float>" "(double)DY_INT_VALUE({0}) + {d1}")
     (("<double-float>" "<integer>") "<double-float>" "{d0} + (double)DY_INT_VALUE({1})"))
    ("binary-" 2
     (("<integer>" "<integer>") "<integer>" "dy_int_sub({0}, {1})" :fold ,#'-)
     (("<double-float>" "<double-float>") "<double-float>" "{d0} - {d1}")
     (("<integer>" "<double-float>") "<double-float>" "(double)DY_INT_VALUE({0}) - {d1}")
     (("<double-float>" "<integer>") "<double-float>" "{d0} - (double)DY_INT_VALUE({1})"))
    ("binary*" 2
     (("<integer>" "<integer>") "<integer>" "dy_int_mul({0}, {1})" :fold ,#'*)
     (("<double-float>" "<double-float>") "<double-float>" "{d0} * {d1}")
     (("<integer>" "<double-float>") "<double-float>" "(double)DY_INT_VALUE({0}) * {d1}")
     (("<double-float>" "<integer>") "<double-float>" "{d0} * (double)DY_INT_VALUE({1})"))
    ("binary/" 2
     (("<double-float>" "<double-float>") "<double-float>" "{d0} / {d1}"
      :extra "{d1} != 0.0")
     (("<integer>" "<double-float>") "<double-float>" "(double)DY_INT_VALUE({0}) / {d1}"
      :extra "{d1} != 0.0")
     (("<double-float>" "<integer>") "<double-float>" "{d0} / (double)DY_INT_VALUE({1})"
      :extra "{1} != DY_INT(0)"))
    ("binary<" 2
     (("<integer>" "<integer>") "<boolean>" "DY_BOOL((intptr_t)({0}) < (intptr_t)({1}))"
      :fold ,(lambda (a b) (if (< a b) t +false+)))
     (("<double-float>" "<double-float>") "<boolean>" "DY_BOOL({d0} < {d1})"))
    ("binary=" 2
     (("<integer>" "<integer>") "<boolean>" "DY_BOOL(({0}) == ({1}))"
      :fold ,(lambda (a b) (if (= a b) t +false+)))
     (("<double-float>" "<double-float>") "<boolean>" "DY_BOOL({d0} == {d1})"))
    ("unary-" 1
     (("<integer>") "<integer>" "dy_int_negate({0})" :fold ,#'-)
     (("<double-float>") "<double-float>" "-({d0})"))
    ;; The iteration protocol on lists and simple vectors
    ("initial-state" 1
     (("<pair>") "<pair>" "{0}")
     (("<empty-list>") "<boolean>" "DY_FALSE")
     (("<simple-object-vector>") nil "(DY_VECTOR_SIZE({0}) > 0 ? DY_INT(0) : DY_FALSE)"))
    ("next-state" 2
     (("<pair>" "<pair>") nil "(dy_kind_p(dy_tail({1}), DY_KIND_PAIR) ? dy_tail({1}) : DY_FALSE)")
     (("<simple-object-vector>" "<integer>") nil
      "(DY_INT_VALUE({1}) + 1 < DY_VECTOR_SIZE({0}) ? DY_INT(DY_INT_VALUE({1}) + 1) : DY_FALSE)"))
    ("current-element" 2
     (("<pair>" "<pair>") nil "dy_head({1})")
     (("<simple-object-vector>" "<integer>") nil "DY_VECTOR_DATA({0})[DY_INT_VALUE({1})]"
      :extra "(uintptr_t)DY_INT_VALUE({1}) < (uintptr_t)DY_VECTOR_SIZE({0})"))
    ("element" 2
     (("<simple-object-vector>" "<integer>") nil "DY_VECTOR_DATA({0})[DY_INT_VALUE({1})]"
      :extra "(uintptr_t)DY_INT_VALUE({1}) < (uintptr_t)DY_VECTOR_SIZE({0})")
     (("<byte-string>" "<integer>") "<character>"
      "DY_CHAR((unsigned char)DY_STRING_DATA({0})[DY_INT_VALUE({1})])"
      :extra "(uintptr_t)DY_INT_VALUE({1}) < (uintptr_t)DY_STRING_SIZE({0})"))
    ("modulo" 2
     (("<integer>" "<integer>") "<integer>"
      "DY_INT(dy_int_modulo(DY_INT_VALUE({0}), DY_INT_VALUE({1})))"
      :extra "DY_INT_VALUE({1}) != 0"))
    ("remainder" 2
     (("<integer>" "<integer>") "<integer>" "DY_INT(DY_INT_VALUE({0}) % DY_INT_VALUE({1}))"
      :extra "DY_INT_VALUE({1}) != 0"))
    ("=hash" 1 (("<integer>") "<integer>" "DY_INT(DY_INT_VALUE({0}) & DY_INT_MAX)"))
    ("zero?" 1
     (("<integer>") "<boolean>" "DY_BOOL({0} == DY_INT(0))")
     (("<double-float>") "<boolean>" "DY_BOOL({d0} == 0.0)"))
    ("positive?" 1
     (("<integer>") "<boolean>" "DY_BOOL((intptr_t)({0}) > (intptr_t)DY_INT(0))")
     (("<double-float>") "<boolean>" "DY_BOOL({d0} > 0.0)"))
    ("negative?" 1
     (("<integer>") "<boolean>" "DY_BOOL((intptr_t)({0}) < (intptr_t)DY_INT(0))")
     (("<double-float>") "<boolean>" "DY_BOOL({d0} < 0.0)"))
    ("even?" 1 (("<integer>") "<boolean>" "DY_BOOL((DY_INT_VALUE({0}) & 1) == 0)"))
    ("odd?" 1 (("<integer>") "<boolean>" "DY_BOOL((DY_INT_VALUE({0}) & 1) != 0)"))
    ("element-setter" 3
     (("<simple-object-vector>" "<integer>" "<object>") nil
      "(DY_VECTOR_DATA({0})[DY_INT_VALUE({1})] = {2})"
      :extra "(uintptr_t)DY_INT_VALUE({1}) < (uintptr_t)DY_VECTOR_SIZE({0})"))
    ("size" 1
     (("<simple-object-vector>") "<integer>" "DY_INT(DY_VECTOR_SIZE({0}))")
     (("<byte-string>") "<integer>" "DY_INT(DY_STRING_SIZE({0}))")))
  "Methods of the library's generic functions done inline: the generic
function's name, its number of arguments, and cases, each (CLASSES
RESULT-CLASS EXPRESSION &key EXTRA FOLD): the case is for arguments of the
CLASSES, all sealed but <object>, for any argument, which a test of their
representation finds, and, if
given, for which the C condition EXTRA holds.  Its value is of the
RESULT-CLASS, if known, and is the C EXPRESSION -- of type double for a
<double-float>.  {i} stands for argument i, {di} for it as a double.
FOLD, given integer constants, computes the result when compiling.")

(defun case-classes (c) (first c))
(defun case-result (c) (second c))
(defun case-expression (c) (third c))
(defun case-extra (c) (getf (cdddr c) :extra))
(defun case-fold (c) (getf (cdddr c) :fold))

(defun class-test-c (class-name arg)
  "A C test that ARG, a C expression, is an instance of the sealed class
CLASS-NAME."
  (cond ((string= class-name "<integer>") (format nil "DY_IS_INT(~a)" arg))
        ((string= class-name "<double-float>") (format nil "DY_IS_DOUBLE(~a)" arg))
        ((string= class-name "<empty-list>") (format nil "(~a) == DY_EMPTY" arg))
        (t (format nil "dy_kind_p(~a, DY_KIND_~a)" arg
                   (substitute #\_ #\- (string-upcase (string-trim "<>" class-name)))))))

;;; Functions written in C that are done inline: name, arguments, the C
;;; expression -- which may itself fall back to the function -- and
;;; whether it may be used on constants other than integers, which it
;;; does not test, so that a C compiler finds no test always true or false
;;; to warn of: T, or :LAST for the last argument only.
(defparameter *primitive-inlines*
  '(("id?" 2 "DY_BOOL((~a) == (~a) || (DY_IS_POINTER(~2:*~a) && DY_IS_POINTER(~a) && dy_identical(~2:*~a, ~a)))")
    ("not" 1 "DY_BOOL((~a) == DY_FALSE)")
    ("car" 1 "(dy_kind_p(~a, DY_KIND_PAIR) ? dy_head(~:*~a) : ~a)")
    ("cdr" 1 "(dy_kind_p(~a, DY_KIND_PAIR) ? dy_tail(~:*~a) : ~a)")
    ("cons" 2 "dy_pair(~a, ~a)" t)
    ("%set-car!" 2 "(dy_kind_p(~a, DY_KIND_PAIR) ? (((DyPair *)DY_PTR(~:*~a))->head = ~a) : ~a)")
    ("%set-cdr!" 2 "(dy_kind_p(~a, DY_KIND_PAIR) ? (((DyPair *)DY_PTR(~:*~a))->tail = ~a) : ~a)")
    ("%vector-size" 1 "(dy_kind_p(~a, DY_KIND_SIMPLE_OBJECT_VECTOR) ? DY_INT(DY_VECTOR_SIZE(~:*~a)) : ~a)")
    ("%vector-element" 2 "((dy_kind_p(~a, DY_KIND_SIMPLE_OBJECT_VECTOR) && DY_IS_INT(~a) && (uintptr_t)DY_INT_VALUE(~:*~a) < (uintptr_t)DY_VECTOR_SIZE(~2:*~a)) ? DY_VECTOR_DATA(~:*~a)[DY_INT_VALUE(~a)] : ~a)")
    ("%vector-element-setter" 3 "((dy_kind_p(~a, DY_KIND_SIMPLE_OBJECT_VECTOR) && DY_IS_INT(~a) && (uintptr_t)DY_INT_VALUE(~:*~a) < (uintptr_t)DY_VECTOR_SIZE(~2:*~a)) ? (DY_VECTOR_DATA(~:*~a)[DY_INT_VALUE(~a)] = ~a) : ~a)" :last)))

(defparameter *generated-primitives*
  '("slot_initialized_p" "slot_descriptor" "slot_descriptors")
  "Primitives the compiler writes into each program, whose entries are not
dy_prim_NAME_entry.")

(defun call-callee (e)
  "What E calls, if known: (values :generic gfun swap?), (values :fold
gfun), (values :primitive c-name), or NIL."
  (let ((v (static-value (ir-call-function e)))
        (n (length (ir-call-arguments e))))
    (when (eq (car v) :global)
      (let ((var (second v)) (swap nil))
        (when (and (eq (module-var-module var) (dylan-module)) (> n 2))
          (loop for (name target) in *variadic-folds*
                when (and (eq var (dylan-var name)) (gethash (dylan-var target) *gfuns*))
                  do (return-from call-callee
                       (values :fold (gethash (dylan-var target) *gfuns*)))))
        (when (eq (module-var-module var) (dylan-module))
          (loop for (name count target swap?) in *variadic-rewrites*
                when (and (= n count) (eq var (dylan-var name))
                          (gethash (dylan-var target) *gfuns*))
                  do (setf var (dylan-var target) swap swap?)
                     (return)))
        (let ((g (gethash var *gfuns*)))
          (cond ((and g (primitive-generic-c-name var g))
                 (values :primitive (primitive-generic-c-name var g)))
                (g (values :generic g swap))
                ((and (module-var-predefined-p var) (null (module-var-definitions var)))
                 (let ((c (predefined-c-value var *classes*)))
                   (when (and c (search "dy_prim_" c))
                     (values :primitive (subseq c (+ (search "dy_prim_" c) 8)
                                                (1- (length c)))))))))))))

(defun count-absorbed-uses (fns)
  (setf *absorbed-uses* (make-hash-table :test #'eq))
  (dolist (fn fns)
    (labels ((walk (e)
               (typecase e
                 (ir-call (let ((f (ir-call-function e)))
                            (when (and (ivar-p f) (call-callee e))
                              (incf (gethash f *absorbed-uses* 0)))))
                 (ir-let (walk (ir-let-value e)) (walk (ir-let-body e)))
                 (ir-mv-let (walk (ir-mv-let-value e)) (walk (ir-mv-let-body e)))
                 (ir-seq (mapc #'walk (ir-seq-forms e)))
                 (ir-if (walk (ir-if-then e)) (walk (ir-if-else e)))
                 (ir-letrec (walk (ir-letrec-body e)))
                 (ir-bind-exit (walk (ir-bind-exit-body e))))))
      (when (listp (ir-fn-keys fn))
        (dolist (k (ir-fn-keys fn)) (walk (ir-key-default k))))
      (walk (ir-fn-body fn)))))

(defun dead-binding-p (var value)
  "Is the let of VAR to VALUE needless: the value pure, and every use of
VAR absorbed by a known call -- none by a closure, whose environment holds
it?"
  (and (typep value '(or ir-global-ref const))
       (not (cell-var-p var))
       (not (ivar-captured-p var))
       (= (ivar-ref-count var) (gethash var *absorbed-uses* 0))))

;;; Whether a method is the library's

(defun library-method-p (m)
  (let ((location (gmethod-location m)))
    (and location
         (search "/runtime/dylan/" (source-file-name (location-file location))))))

(defun sole-method (g)
  "The method G's calls can go straight to: G's only method, if it takes
any arguments and G takes no others."
  (let ((methods (gfun-methods g)))
    (and (= (length methods) 1)
         (not (gfun-variable-p g))
         (eq (gmethod-kind (first methods)) :ir)
         (every #'object-spec-p (gmethod-specs (first methods)))
         (first methods))))

(defun spec-method (g)
  "The method whose specialized entry G's calls may go straight to: G's
only method, if it takes any arguments.  (A call with keywords goes
there only if the method takes them all, which is all G would check.)"
  (let ((methods (gfun-methods g)))
    (and (= (length methods) 1)
         (eq (gmethod-kind (first methods)) :ir)
         (every #'object-spec-p (gmethod-specs (first methods)))
         (first methods))))

;;; Emitting

(defun emit-call (dest tail call tail-call)
  "Give DEST the values of the C call CALL; TAIL-CALL is the statement that
makes it a tail call, used when TAIL."
  (when (and (or tail (eq (dest-kind dest) :return)) (cfun-needs-mv-reset *cfun*))
    ;; The values of an earlier call may still be counted, and a function
    ;; written in C with one value does not reset the count.
    (emit "dy_thread->mv_count = 1;"))
  (cond ((and tail (not (demote-tail-call-p))) (emit "~a;" tail-call))
        ((eq (dest-kind dest) :return) (emit "return ~a;" call))
        ((eq (dest-kind dest) :mv)
         (emit "dy_thread->mv_count = 1;")
         (emit "~a = ~a;" (dest-var dest) call)
         (when (dest-label dest) (emit "goto ~a;" (dest-label dest))))
        ((eq (dest-kind dest) :effect)
         (emit "(void)~a;" call)
         (when (dest-label dest) (emit "goto ~a;" (dest-label dest))))
        (t (deliver dest call))))

(defun gen-known-call (e dest tail)
  "Generate the call E if its function is known; answer whether it was."
  (multiple-value-bind (kind what swap) (call-callee e)
    (when kind
      (let ((args (if swap (reverse (ir-call-arguments e)) (ir-call-arguments e))))
        (ecase kind
          (:primitive (gen-primitive-call what args dest tail))
          (:generic (gen-generic-call what args dest tail))
          (:fold
           ;; ((a op b) op c) ...: each step's value in a temporary
           (let ((acc (first args)))
             (loop for (arg . more) on (rest args)
                   do (if more
                          (let* ((type (generic-call-type what (list acc arg)))
                                 (double (eq type (find-class-info "<double-float>" *classes*)))
                                 (temp (new-temp (if double "double" "D"))))
                            (gen-generic-call what (list acc arg)
                                              (make-dest (if double :raw :c) temp) nil)
                            (setf acc (make-c-atom temp type double)))
                          (gen-generic-call what (list acc arg) dest tail))))))
        t))))

(defun known-class (atom)
  "The CLASS-INFO of the class ATOM is known to hold, or NIL."
  (let ((v (static-value atom)))
    (and (eq (car v) :global) (global-class (second v)))))

(defun gen-primitive-call (c-name args dest tail)
  (when (and (string= c-name "instance_p") (= (length args) 2) (known-class (second args)))
    ;; instance? of a known class
    (let ((class (known-class (second args)))
          (type (atom-type (first args))))
      (return-from gen-primitive-call
        (deliver-pure dest (if (and type (subclass-info-p type class))
                               "DY_TRUE"
                               (format nil "DY_BOOL(dy_instance_p(~a, &~a))"
                                       (atom-c (first args)) (wrapper-c-name class)))))))
  (when (and (string= c-name "id") (= (length args) 2) (some #'never-number-p args))
    ;; Only a number can be id? to another object it is not ==.
    (return-from gen-primitive-call
      (deliver-pure dest (format nil "DY_BOOL(~a == ~a)"
                                 (atom-c (first args)) (atom-c (second args))))))
  (pushnew (format nil "dy_prim_~a" c-name) *used-primitives* :test #'string=)
  (let* ((n (length args))
         (inline (find-if (lambda (i) (and (eq (dylan-var (first i))
                                                (dylan-var (primitive-dylan-name c-name)))
                                            (= (second i) n)))
                          *primitive-inlines*)))
    (if (and inline (not (member c-name *generated-primitives* :test #'string=))
             (or (eq (fourth inline) t)
                 (notany #'heap-constant-p (if (fourth inline) (butlast args) args))))
        (deliver-pure dest (apply #'format nil (third inline)
                                  (append (mapcar #'atom-c args)
                                          ;; the fallback, for car and cdr
                                          (list (primitive-call-c c-name args)))))
        (multiple-value-bind (as count) (call-arguments-c args)
          (let ((object (format nil "DY_OBJ(&dy_prim_~a)" c-name)))
            (if (member c-name *generated-primitives* :test #'string=)
                (emit-call dest tail
                           (format nil "DY_CALL(~a, ~{~a, ~}~d)" object as count)
                           (format nil "DY_TAIL_CALL(~a, ~{~a, ~}~d)" object as count))
                (let ((entry (format nil "dy_prim_~a_entry" c-name)))
                  (pushnew entry *used-primitive-entries* :test #'string=)
                  (emit-call dest tail
                             (format nil "DY_CALL_DIRECT(~a, ~a, ~{~a, ~}~d)" entry object as count)
                             (format nil "DY_TAIL_CALL_DIRECT(~a, ~a, ~{~a, ~}~d)"
                                     entry object as count)))))))))

(defun never-number-p (a)
  "Is the atom A known not to be a boxed number: a constant other than a
float or ratio, or of a class no boxed number is an instance of?"
  (let ((c (if (const-p a) a (and (ivar-p a) (constant-atom a)))))
    (if c
        (not (and (numberp (const-value c)) (not (integerp (const-value c)))))
        (let ((type (atom-type a))
              (number (find-class-info "<number>" *classes*)))
          (and type
               (or (subclass-info-p type (find-class-info "<integer>" *classes*))
                   (and (not (subclass-info-p type number))
                        (not (subclass-info-p number type)))))))))

(defun primitive-call-c (c-name args)
  "A C expression calling the primitive (of at most four arguments)."
  (pushnew (format nil "dy_prim_~a_entry" c-name) *used-primitive-entries* :test #'string=)
  (format nil "dy_prim_~a_entry(DY_OBJ(&dy_prim_~a), ~{~a, ~}~d)" c-name c-name
          (loop for i below 4 collect (if (nth i args) (atom-c (nth i args)) "0"))
          (length args)))

(defun primitive-dylan-name (c-name)
  (car (rassoc c-name *primitive-functions* :test #'string=)))

(defun deliver-pure (dest expr)
  "Give DEST the single value of EXPR, a C expression that calls no Dylan
function."
  (if (eq (dest-kind dest) :return)
      (progn (when (cfun-needs-mv-reset *cfun*) (emit "dy_thread->mv_count = 1;"))
             (emit "return ~a;" expr))
      (deliver dest expr)))

(defun deliver-double (dest expr)
  "Give DEST the value of EXPR, a C double."
  (case (dest-kind dest)
    (:raw (emit "~a = ~a;" (dest-var dest) expr)
          (when (dest-label dest) (emit "goto ~a;" (dest-label dest))))
    ((:bind :set)
     (if (unboxed-p (dest-var dest))
         (progn (emit "~a = ~a;" (v-name (dest-var dest)) expr)
                (when (dest-label dest) (emit "goto ~a;" (dest-label dest))))
         (deliver-pure dest (box-double expr))))
    (t (deliver-pure dest (box-double expr)))))

(defun box-double (expr) (format nil "dy_make_double_float(~a)" expr))

(defun heap-constant-p (a)
  "Is A a constant other than an integer?  The inline cases are not used
on such constants, whose tests C compilers fold and then warn about the
branches they find dead."
  (and (const-p a) (not (integerp (const-value a)))))

(defun template (string args)
  "STRING with each {i} replaced by the C expression of the atom ARGS[i],
and each {di} by its C double."
  (let ((out string))
    (loop for a in args for i from 0
          do (let ((d (format nil "{d~d}" i))
                   (plain (format nil "{~d}" i)))
               (when (search d out) (setf out (replace-all out d (atom-double-c a))))
               (when (search plain out) (setf out (replace-all out plain (atom-c a))))))
    out))

(defun replace-all (string old new)
  (with-output-to-string (s)
    (loop with start = 0
          for hit = (search old string :start2 start)
          do (write-string string s :start start :end (or hit (length string)))
             (if hit (progn (write-string new s) (setf start (+ hit (length old)))) (return)))))

(defun library-only-for-p (g class-names)
  "Do only the library's methods of G apply to arguments of the classes
CLASS-NAMES?"
  (let ((classes (mapcar (lambda (n) (find-class-info n *classes*)) class-names)))
    (every (lambda (m)
             (or (library-method-p m)
                 (notevery (lambda (spec class)
                             (cond ((string= (class-info-name class) "<object>") t)
                                   ((eq (spec-kind spec) :class)
                                    (subclass-info-p class (spec-class spec)))
                                   ;; Any object, for all the compiler knows
                                   ((runtime-spec-p spec) t)
                                   (t (eq (constant-class (spec-value spec)) class))))
                           (gmethod-specs m) classes)))
           (gfun-methods g))))

(defun instance-slot-number (g kind)
  "If every method of G is the KIND (:getter or :setter) of an instance
slot -- for a setter, untyped, or of a type that is a class known when
compiling -- the program's number for that slot, and for a setter the
class of the type, if any."
  (let* ((methods (gfun-methods g))
         (sd (and methods (gmethod-sdef (first methods))))
         (type (and sd (slot-spec-type (sdef-spec sd)))))
    (and sd
         (or (eq kind :getter) (null type) (known-class type))
         (every (lambda (m)
                  (and (eq (gmethod-kind m) kind)
                       (eq (sdef-allocation (gmethod-sdef m)) :instance)
                       (sdef-class (gmethod-sdef m))
                       (or (eq kind :getter)
                           (null type)
                           (let ((other (slot-spec-type (sdef-spec (gmethod-sdef m)))))
                             (and other (eq (known-class other) (known-class type)))))
                       (or (eq kind :getter)
                           type
                           (null (slot-spec-type (sdef-spec (gmethod-sdef m)))))))
                methods)
         (values (gethash (sdef-getter sd) *instance-slot-numbers*)
                 (and (eq kind :setter) type (known-class type))))))

(defun gen-slot-call (g args dest tail)
  "Generate the call of G, a slot getter or setter, inline if it can be;
answer whether it was."
  (let ((n (length args)))
    (multiple-value-bind (k type)
        (and (notany #'heap-constant-p (if (= n 2) (butlast args) args))
             (instance-slot-number g (case n (1 :getter) (2 :setter))))
      (when k
        (gen-inline-slot-call g args dest tail k type)
        t))))

(defun gen-inline-slot-call (g args dest tail k type)
  "Generate the call of G on ARGS, which reads or writes the instance slot
numbered K, whose type, for a setter, is the class TYPE, if any."
  (let ((n (length args)))
    (progn
      (let ((place (new-temp "D *"))
            (cs (mapcar #'atom-c args)))
        (emit "~a = dy_instance_slot(~a, ~d);" place (first cs) k)
        (if (= n 1)
            (progn (emit "if (~a && *~:*~a != DY_UNBOUND) {" place)
                   (indented (deliver-pure dest (format nil "*~a" place))))
            (progn (emit "if (~a~@[ && ~a~]) {" place
                         (and type
                              (not (and (atom-type (second args))
                                        (subclass-info-p (atom-type (second args)) type)))
                              (format nil "dy_instance_p(~a, &~a)" (second cs)
                                      (wrapper-c-name type))))
                   (indented (emit "*~a = ~a;" place (second cs))
                             (deliver-pure dest (second cs)))))
        (emit "} else {")
        (indented (gen-dispatched-call g args dest tail))
        (emit "}")
        t))))

(defun inline-entry (g n)
  (let* ((var-name (module-var-name (gfun-var g)))
         (name (format nil "~a~:[~;-setter~]"
                       (dylan::dylan-symbol->string (name-symbol var-name))
                       (setter-name-p var-name))))
    (and (eq (module-var-module (gfun-var g)) (dylan-module))
         (find-if (lambda (i) (and (string= (first i) name) (= (second i) n)))
                  *method-inlines*))))

(defun argument-fit (arg class-name)
  "Is ARG known to be an instance of the sealed class CLASS-NAME (:YES),
known not to be (:NO), or neither (:MAYBE)?"
  (let ((type (atom-type arg))
        (class (find-class-info class-name *classes*)))
    (cond ((string= class-name "<object>") :yes)
          ((null type) :maybe)
          ((subclass-info-p type class) :yes)
          ((subclass-info-p class type) :maybe)
          (t :no))))

(defun inline-cases (g args)
  "The cases of G's inline methods that might apply to ARGS, each (CASE
. FITS)."
  (let ((entry (inline-entry g (length args))))
    (loop for c in (cddr entry)
          for fits = (mapcar #'argument-fit args (case-classes c))
          when (and (not (member :no fits)) (library-only-for-p g (case-classes c))
                    ;; nor a divisor known to be 0, which C compilers warn of
                    (not (and (search "!= 0" (or (case-extra c) ""))
                              (let ((d (constant-atom* (second args))))
                                (and d (eql (const-value d) 0))))))
            collect (cons c fits))))

(defun constant-atom* (a)
  (cond ((const-p a) a)
        ((ivar-p a) (constant-atom a))))

(defun certain-case-p (case-and-fits)
  (and (every (lambda (f) (eq f :yes)) (cdr case-and-fits))
       (null (case-extra (car case-and-fits)))))

(defun generic-call-type (g args)
  "The class of the value of the call of G on ARGS, if known."
  (let ((cases (inline-cases g args)))
    (or (and cases
             (every (lambda (f) (eq f :yes)) (cdr (first cases)))
             (case-result (car (first cases)))
             (find-class-info (case-result (car (first cases))) *classes*))
        (and (double-contagion-p g args)
             (find-class-info "<double-float>" *classes*)))))

(defparameter *float-contagious* '("binary+" "binary-" "binary*" "binary/")
  "The library's generic functions whose only methods, on two numbers,
answer a float when either is one (chapter 14), or signal an error.")

(defun may-apply-p (spec class)
  "Might a method specialized by SPEC apply to an instance of CLASS?"
  (or (runtime-spec-p spec)
      (subclass-info-p class (spec-class spec))
      (subclass-info-p (spec-class spec) class)))

(defun double-contagion-p (g args)
  "Is the call of G on ARGS sure to answer a <double-float>, if it answers
at all: G one of *FLOAT-CONTAGIOUS*, one argument known to be a
<double-float>, and no method but the library's able to apply to a
<double-float> in its place?"
  (let ((double (find-class-info "<double-float>" *classes*)))
    (and (= (length args) 2)
         (member (gfun-var g) (mapcar #'dylan-var *float-contagious*))
         (loop for a in args for i from 0
               thereis (and (eq (argument-fit a "<double-float>") :yes)
                            (every (lambda (m)
                                     (or (library-method-p m)
                                         (not (may-apply-p (nth i (gmethod-specs m)) double))))
                                   (gfun-methods g)))))))

(defun fold-call (case args)
  "The constant value of CASE on ARGS, all integer constants, or NIL."
  (let ((fold (case-fold case))
        (args (mapcar (lambda (a) (or (and (not (c-atom-p a)) (constant-atom a)) a)) args)))
    (when (and fold (every (lambda (a) (and (const-p a) (integerp (const-value a)))) args))
      (let ((value (apply fold (mapcar #'const-value args))))
        (and (or (not (integerp value)) (< (- (expt 2 62)) value (expt 2 62)))
             (list value))))))

(defun deliver-case (dest case args)
  (let ((expr (template (case-expression case) args)))
    (if (equal (case-result case) "<double-float>")
        (deliver-double dest expr)
        (deliver-pure dest expr))))

(defun resolved-make-class (g args)
  "If the call of G on ARGS is a make of a class the program defines, and
no method of make but the library's on <class> -- which calls the class's
maker -- applies to it, that class."
  (when (and (eq (gfun-var g) (dylan-var "make")) args)
    (let* ((class (known-class (first args)))
           (metaclass (find-class-info "<class>" *classes*)))
      (when (and class (class-gclass class)
                 (every (lambda (m)
                          (let ((spec (first (gmethod-specs m))))
                            (cond ((runtime-spec-p spec) nil)
                                  ((eq (spec-kind spec) :singleton)
                                   (not (eq (spec-value spec) class)))
                                  ((subclass-info-p metaclass (spec-class spec))
                                   (and (library-method-p m) (eq (spec-class spec) metaclass)))
                                  (t t))))
                        (gfun-methods g)))
        class))))

;;; Methods chosen when compiling.  When the classes of a call's arguments
;;; show which of a generic function's methods apply -- each surely or
;;; surely not -- and one of them is at least as specific as every other at
;;; every argument, that method is first in any order dispatch would find,
;;; so the call can go straight to it.

(defun spec-fit (spec type object)
  "Does a method specialized by SPEC apply to an argument whose class is
TYPE (or NIL), and which is OBJECT if that is not :UNKNOWN: :YES, :NO or
:MAYBE?"
  (flet ((overlap-p (a b)
           (some (lambda (c) (and (subclass-info-p c a) (subclass-info-p c b))) *classes*)))
    (when (and (null type) (not (eq object :unknown)))
      (setf type (ignore-errors (constant-class object))))
    (cond ((runtime-spec-p spec) :maybe)
          ((eq (spec-kind spec) :singleton)
           (cond ((not (eq object :unknown))
                  (if (eql object (spec-value spec)) :yes :no))
                 ((and type (not (overlap-p type (spec-class spec)))) :no)
                 (t :maybe)))
          ((object-spec-p spec) :yes)
          ((null type) :maybe)
          ((subclass-info-p type (spec-class spec)) :yes)
          ((overlap-p type (spec-class spec)) :maybe)
          (t :no))))

(defun spec<= (a b)
  "Is the spec A at least as specific as B, for any argument both apply to?"
  (cond ((eq (spec-kind a) :singleton) t)
        ((eq (spec-kind b) :singleton) nil)
        (t (subclass-info-p (spec-class a) (spec-class b)))))

(defun argument-object (a)
  "The object the atom A always is, if known when compiling, or :UNKNOWN."
  (let ((class (known-class a))
        (c (and (not (c-atom-p a)) (constant-atom* a))))
    (cond (class class)
          ((and c (typep (const-value c) '(or symbol character integer))) (const-value c))
          (t :unknown))))

(defun resolved-method (g args &optional (types (mapcar #'atom-type args)))
  "The method of G a call on ARGS, whose classes are TYPES, goes straight
to, if it is known when compiling: an IR method that does not call
next-method, given the keyword arguments it takes, if any."
  (let* ((r (gfun-required g))
         (n (length args)))
    (when (and (not *reflective-methods*)
               (>= n r)
               (or (= n r) (and (gfun-variable-p g) (evenp (- n r)))))
      (let ((applicable '()))
        (dolist (m (gfun-methods g))
          (case (let ((fits (loop for spec in (gmethod-specs m)
                                  for a in args for type in types
                                  collect (spec-fit spec type (argument-object a)))))
                  (cond ((member :no fits) :no)
                        ((member :maybe fits) :maybe)
                        (t :yes)))
            (:yes (push m applicable))
            (:maybe (return-from resolved-method nil))))
        (let ((m (find-if (lambda (m)
                            (every (lambda (other)
                                     (every #'spec<= (gmethod-specs m) (gmethod-specs other)))
                                   applicable))
                          applicable)))
          (and m
               (eq (gmethod-kind m) :ir)
               (gmethod-fn m)
               (not (uses-next-method-p (gmethod-fn m)))
               (resolved-keywords-p m (nthcdr r args))
               m))))))

(defun resolved-keywords-p (m pairs)
  "Are PAIRS, the keyword arguments of a call, constant keywords M takes?
Then a check of them against every applicable method's would pass."
  (or (null pairs)
      (let ((keys (gmethod-keys m)))
        (and keys
             (loop for (k) on pairs by #'cddr
                   always (let ((c (and (not (c-atom-p k)) (constant-atom* k))))
                            (and c (or (eq keys :any) (gmethod-rest-p m)
                                       (member (const-value c) keys)))))))))

(defun slot-getter-type (g args)
  "If every method of G is the getter of a slot whose type is a class
known when compiling, the same for all, that class: the class of the value
of a call of G on ARGS."
  (let ((methods (gfun-methods g)))
    (and (= (length args) 1) methods
         (let ((types (mapcar (lambda (m)
                                (and (eq (gmethod-kind m) :getter)
                                     (slot-spec-type (sdef-spec (gmethod-sdef m)))
                                     (known-class (slot-spec-type (sdef-spec (gmethod-sdef m))))))
                              methods)))
           (and (first types)
                (every (lambda (type) (eq type (first types))) types)
                (first types))))))

(defun gen-generic-call (g args dest tail)
  (let ((class (resolved-make-class g args)))
    (when class
      ;; Straight to the maker
      (multiple-value-bind (as count) (call-arguments-c args)
        (let ((maker (format nil "dy_maker_~a" (class-info-c-name class)))
              (self (atom-c (first args))))
          (if (and (eq (dest-kind dest) :bind) (stack-object-p (dest-var dest)))
              ;; On the stack (types.lisp), filled as the maker would
              (emit-call dest nil
                         (format nil "dy_fill_~a(dy_init_instance(~a, &~a), ~a, ~{~a, ~}~d)"
                                 (class-info-c-name class)
                                 (stack-buffer "DyInstance" (class-info-size class))
                                 (wrapper-c-name class) self as count)
                         nil)
              (emit-call dest tail
                         (format nil "DY_CALL_DIRECT(~a, ~a, ~{~a, ~}~d)" maker self as count)
                         (format nil "DY_TAIL_CALL_DIRECT(~a, ~a, ~{~a, ~}~d)"
                                 maker self as count)))))
      (return-from gen-generic-call)))
  (when (gen-slot-call g args dest tail)
    (return-from gen-generic-call))
  (let ((cases (inline-cases g args)))
    (cond
      ((null cases) (gen-dispatched-call g args dest tail))
      ((and (certain-case-p (first cases)) (fold-call (car (first cases)) args))
       (deliver-pure dest (c-constant *cdata* (first (fold-call (car (first cases)) args)))))
      ((certain-case-p (first cases))
       (deliver-case dest (car (first cases)) args))
      (t
       (let ((covered nil))
         (loop for (c . fits) in cases
               for first = t then nil
               for tests = (append (loop for f in fits for class in (case-classes c)
                                         for a in args
                                         unless (eq f :yes) collect (class-test-c class (atom-c a)))
                                   (and (case-extra c) (list (template (case-extra c) args))))
               do (if tests
                      (emit "~:[} else ~;~]if (~{~a~^ && ~}) {" first tests)
                      (progn (emit "} else {") (setf covered t)))
                  (indented (deliver-case dest c args))
               until covered)
         (unless covered
           (emit "} else {")
           (indented (gen-dispatched-call g args dest tail))))
       (emit "}")))))

(defun uses-next-method-p (fn)
  (let ((next (ir-fn-next fn)))
    (and next (plusp (ivar-ref-count next)))))

(defun gen-dispatched-call (g args dest tail)
  (let* ((resolved (resolved-method g args))
         (sole (or (sole-method g) resolved))
         (m (or (spec-method g) resolved))
         (spec (and m (not tail)
                    (spec-call-c (gmethod-fn m) (format nil "DY_OBJ(&~a)" (gmethod-object-c m))
                                 args))))
    (if spec
        ;; Straight to the method's specialized entry
        (emit-call dest nil spec nil)
        (multiple-value-bind (as count) (call-arguments-c args)
          (if sole
              ;; Straight to the method, with no next method
              (progn
                (when (uses-next-method-p (gmethod-fn sole))
                  (emit "dy_thread->next_method = DY_FALSE;"))
                (emit-call dest tail
                           (format nil "DY_CALL_DIRECT(~a, DY_OBJ(&~a), ~{~a, ~}~d)"
                                   (gmethod-entry-c sole) (gmethod-object-c sole) as count)
                           (format nil "DY_TAIL_CALL_DIRECT(~a, DY_OBJ(&~a), ~{~a, ~}~d)"
                                   (gmethod-entry-c sole) (gmethod-object-c sole) as count)))
              (let ((dispatch (format nil "~a_dispatch" (gfun-c-name g)))
                    (object (address (gfun-c-name g))))
                (emit-call dest tail
                           (format nil "DY_CALL_DIRECT(~a, ~a, ~{~a, ~}~d)" dispatch object as count)
                           (format nil "DY_TAIL_CALL_DIRECT(~a, ~a, ~{~a, ~}~d)"
                                   dispatch object as count))))))))
