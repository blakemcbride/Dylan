;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Write the static data the runtime tests link with:
;;;;
;;;;     sbcl --script tests/runtime/generate.lisp OUTPUT.c
;;;;
;;;; OUTPUT.c holds the built-in classes and symbols as the compiler emits
;;;; them, and two tables for test-runtime.c: whether each class is a
;;;; subclass of each other, computed here from the precedence lists; and
;;;; constants, as the compiler emits them, each with how the runtime's
;;;; debug printer must write it.

(let ((here (directory-namestring (or *load-truename* *default-pathname-defaults*))))
  (let ((*standard-output* (make-broadcast-stream)))
    (load (merge-pathnames "../../compiler/load.lisp" here))))

(in-package #:dylan-compiler)

(defparameter *test-constants*
  ;; (Dylan source . how dy_debug_print writes it)
  '(("12" . "12")
    ("-7" . "-7")
    ("4611686018427387903" . "4611686018427387903")
    ("-4611686018427387904" . "-4611686018427387904")
    ("#\\a" . "#\\a")
    ("#t" . "#t")
    ("#f" . "#f")
    ("()" . "()")
    ("\"say \\\"hi\\\"\"" . "\"say \"hi\"\"")
    ("'(1 (2) . 3)" . "(1 (2) . 3)")
    ("#(1 #t #f ())" . "#(1 #t #f ())")
    ("3/4" . "3/4")
    ("-6/8" . "-3/4")
    ("1.5" . "1.5")
    ("1.5s0" . "1.5s0")
    ("21.0" . "21.0")
    ("1.0e20" . "1.0e20")
    ("-0.1" . "-0.1")
    ("foo:" . "foo:")
    ("'Foo" . "Foo")
    ("'FOO" . "Foo")))

(defun read-dylan-string (text)
  (with-input-from-string (in text)
    (let ((form (dylan::dylan-read in)))
      ;; 'x reads as (quote x); the constant is x.
      (if (and (consp form) (eq (car form) 'quote)) (cadr form) form))))

(let* ((classes (prepare-classes (built-in-class-infos)))
       (data (make-cdata))
       (constants (mapcar (lambda (c) (c-constant data (read-dylan-string (car c))))
                          *test-constants*)))
  (with-open-file (out (second sb-ext:*posix-argv*) :direction :output
                                                    :if-exists :supersede)
    (emit-program-data classes data out)
    ;; Row i, column j: is class i a subclass of class j?
    (format out "~%/* Tests */~%~%const uint8_t test_subclass[] = {~%")
    (dolist (c classes)
      (format out "  ~{~d,~^ ~}~%"
              (mapcar (lambda (d) (if (subclass-info-p c d) 1 0)) classes)))
    (format out "};~%~%D test_constants[] = {~%~{  ~a,~%~}};~%" constants)
    (format out "const char *const test_expected[] = {~%~{  ~a,~%~}};~%"
            (mapcar (lambda (c) (c-string-literal (map 'list #'char-code (cdr c))))
                    *test-constants*))
    (format out "const size_t test_constant_count = ~d;~%" (length *test-constants*))))
