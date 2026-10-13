;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; The DYLAN module's variables that have a value in C so far: the
;;;; built-in classes (classes.lisp), and the provisional functions of
;;;; runtime/c/primitives.c.  Until the library is written (CompilerPlan.md,
;;;; Phase 8), a program can use only these of the DYLAN module.

(in-package #:dylan-compiler)

(defparameter *primitive-functions*
  '(("+" . "add") ("-" . "subtract") ("*" . "multiply") ("/" . "divide")
    ("<" . "less") (">" . "greater") ("<=" . "less_equal")
    (">=" . "greater_equal") ("=" . "equal") ("/=" . "not_equal")
    ("id?" . "id") ("zero?" . "zero_p") ("positive?" . "positive_p")
    ("negative?" . "negative_p") ("even?" . "even_p") ("odd?" . "odd_p")
    ("abs" . "abs") ("max" . "max") ("min" . "min")
    ("not" . "not") ("identity" . "identity")
    ("list" . "list") ("cons" . "cons") ("car" . "car") ("cdr" . "cdr")
    ("vector" . "vector")
    ("values" . "values") ("apply" . "apply")
    ("print" . "print")
    ;; Conditions (chapter 13); the rest are in the library
    ("do-handlers" . "do_handlers")
    ("%signal" . "signal_condition") ("%set-error-hooks" . "set_error_hooks")
    ("%format-message" . "format_message") ("%die" . "die") ("%warn" . "warn")
    ("%break" . "break_message")
    ;; Classes and functions (chapters 10 and 11)
    ("make" . "make") ("object-class" . "object_class")
    ("instance?" . "instance_p") ("subclass?" . "subclass_p")
    ("singleton" . "singleton") ("all-superclasses" . "all_superclasses")
    ("direct-superclasses" . "direct_superclasses")
    ("direct-subclasses" . "direct_subclasses")
    ("generic-function-methods" . "generic_function_methods")
    ("method-specializers" . "method_specializers")
    ("applicable-method?" . "applicable_method_p")
    ("sorted-applicable-methods" . "sorted_applicable_methods")
    ("find-method" . "find_method")
    ("seal" . "seal") ("make-read-only" . "seal"))
  "Dylan name -> the C name of its DyFunction, less the dy_prim_ prefix.")

(defun predefined-c-value (var classes)
  "The C expression for the value of the DYLAN module's variable VAR, or
NIL if it has none yet."
  (let ((name (module-var-name var)))
    (unless (consp name)
      (let* ((string (dylan::dylan-symbol->string name))
             (primitive (cdr (assoc string *primitive-functions* :test #'string-equal)))
             (class (find string classes :key #'class-info-name :test #'string-equal)))
        (cond (primitive (format nil "DY_OBJ(&dy_prim_~a)" primitive))
              ((and class (class-info-exported-p class))
               (format nil "DY_OBJ(&~a)" (class-c-name class))))))))
