;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Static C data: the program's constants, symbols and classes, in the
;;;; representation of runtime/c/dylan.h.
;;;;
;;;; A CDATA collects definitions as constants are asked for; C-CONSTANT
;;;; answers a C expression of type D for a Dylan constant, defining static
;;;; objects for it as needed.  Strings with the same contents are shared;
;;;; a symbol (or keyword) is defined once whatever the case it is written
;;;; in, with the spelling it first had, and every one is listed in
;;;; dy_static_symbols for the runtime to intern when it starts.
;;;;
;;;; EMIT-PROGRAM-DATA writes the whole: the definitions, then a wrapper and
;;;; a class object for every class, then the tables the runtime reads.

(in-package #:dylan-compiler)

(defvar *cdata* nil "The CDATA of the program being compiled.")

(defstruct (cdata (:constructor make-cdata ()))
  (counter 0)
  (definitions '())                     ; C text, newest first
  (strings (make-hash-table :test #'equal))
  (symbols (make-hash-table :test #'equal))   ; folded name -> C name
  (symbol-list '())                     ; C names, newest first
  (keywords (make-hash-table :test #'equal)))

(defun cdata-name (data prefix)
  (format nil "~a~d" prefix (incf (cdata-counter data))))

(defun define-c (data control &rest arguments)
  (push (apply #'format nil control arguments) (cdata-definitions data)))

(defun address (c-name) (format nil "DY_OBJ(&~a)" c-name))

;;; C literals

(defun c-string-literal (codes)
  "A C string literal holding the bytes CODES (integers below 256)."
  (with-output-to-string (out)
    (write-char #\" out)
    (dolist (c codes)
      (cond ((or (= c 34) (= c 92)) (format out "\\~c" (code-char c)))
            ((<= 32 c 126) (write-char (code-char c) out))
            ;; Always three digits, so a digit after it cannot extend it.
            (t (format out "\\~3,'0o" c))))
    (write-char #\" out)))

(defun c-float-literal (x)
  "X, a finite float, as an exact C hexadecimal floating constant."
  (multiple-value-bind (mantissa exponent sign) (integer-decode-float x)
    (format nil "~:[~;-~]0x~xp~d~:[~;f~]"
            (minusp sign) mantissa exponent (typep x 'single-float))))

;;; Constants

(defun c-constant (data value)
  "A C expression of type D for the Dylan constant VALUE."
  (cond ((integerp value)
         (check-type value (integer #.(- (expt 2 62)) #.(1- (expt 2 62))))
         (format nil "DY_INT(~d)" value))
        ((characterp value) (format nil "DY_CHAR(~d)" (char-code value)))
        ((eq value t) "DY_TRUE")
        ((eq value dylan::dylan%false) "DY_FALSE")
        ((null value) "DY_EMPTY")
        ((stringp value) (address (c-string data value)))
        ((keywordp value)
         (address (c-symbol data (dylan::dylan-symbol->string value) :keyword)))
        ((symbolp value)
         (address (c-symbol data (dylan::dylan-symbol->string value) :symbol)))
        ((consp value) (address (c-pair data value)))
        ((vectorp value) (address (c-vector data (map 'list (lambda (x) (c-constant data x))
                                                      value))))
        ((typep value 'ratio)
         (let ((name (cdata-name data "dy_ratio_")))
           (define-c data "static DyRatio ~a = { &dy_wrapper_ratio, DY_INT(~d), DY_INT(~d) };"
                     name (numerator value) (denominator value))
           (address name)))
        ((typep value 'single-float)
         (let ((name (cdata-name data "dy_float_")))
           (define-c data "static DySingleFloat ~a = { &dy_wrapper_single_float, ~a };"
                     name (c-float-literal value))
           (address name)))
        ((typep value 'double-float)
         (let ((name (cdata-name data "dy_float_")))
           (define-c data "static DyDoubleFloat ~a = { &dy_wrapper_double_float, ~a };"
                     name (c-float-literal value))
           (address name)))
        (t (error "No C representation for the constant ~s." value))))

(defun c-string (data string)
  "The C name of a static string object holding STRING: a <byte-string>
if every character fits in a byte, else a <unicode-string>."
  (or (gethash string (cdata-strings data))
      (setf (gethash string (cdata-strings data))
            (let ((name (cdata-name data "dy_string_"))
                  (codes (map 'list #'char-code string)))
              (if (every (lambda (c) (< c 256)) codes)
                  (define-c data "static struct { DyWrapper *wrapper; D size; char data[~d]; } ~a =~%  { &dy_wrapper_byte_string, DY_INT(~d), ~a };"
                            (1+ (length codes)) name (length codes)
                            (c-string-literal codes))
                  (define-c data "static struct { DyWrapper *wrapper; D size; uint32_t data[~d]; } ~a =~%  { &dy_wrapper_unicode_string, DY_INT(~d), { ~{~du~^, ~} } };"
                            (max 1 (length codes)) name (length codes) codes))
              name))))

(defun c-symbol (data spelling kind)
  "The C name of the static symbol (KIND :SYMBOL) or keyword (:KEYWORD)
named SPELLING."
  (let ((table (if (eq kind :keyword) (cdata-keywords data) (cdata-symbols data)))
        (key (string-downcase spelling)))
    (or (gethash key table)
        (setf (gethash key table)
              (let ((string (c-string data spelling))
                    (name (cdata-name data (if (eq kind :keyword) "dy_keyword_" "dy_symbol_"))))
                (define-c data "DySymbol ~a = { &dy_wrapper_~(~a~), ~a, 0 };"
                          name kind (address string))
                (push name (cdata-symbol-list data))
                name)))))

(defun c-pair (data list)
  (let ((head (c-constant data (car list)))
        (tail (c-constant data (cdr list)))
        (name (cdata-name data "dy_pair_")))
    (define-c data "static DyPair ~a = { &dy_wrapper_pair, ~a, ~a };" name head tail)
    name))

(defun c-vector (data elements)
  "The C name of a static <simple-object-vector> of ELEMENTS, C
expressions."
  (let ((name (cdata-name data "dy_vector_")))
    (define-c data "static struct { DyWrapper *wrapper; D size; D data[~d]; } ~a =~%  { &dy_wrapper_simple_object_vector, DY_INT(~d), { ~{~a~^, ~} } };"
              (max 1 (length elements)) name (length elements) elements)
    name))

;;; Classes

(defun class-c-name (class) (format nil "dy_class_~a" (class-info-c-name class)))
(defun wrapper-c-name (class) (format nil "dy_wrapper_~a" (class-info-c-name class)))

(defun ancestor-bytes (class count)
  (let ((bytes (make-array (ceiling count 8) :initial-element 0)))
    (dolist (a (class-info-cpl class))
      (let ((id (class-info-id a)))
        (setf (aref bytes (floor id 8))
              (logior (aref bytes (floor id 8)) (ash 1 (mod id 8))))))
    (coerce bytes 'list)))

(defun emit-program-data (classes data stream &key prelude (thread-variable-count 0))
  "Write to STREAM the C file defining CLASSES (prepared by
PREPARE-CLASSES) and every constant DATA holds.  PRELUDE is a list of C
declarations to put before the data, which may refer to what they
declare."
  (let ((count (length classes))
        (class-text '()))
    ;; The class objects' own constants go into DATA first.
    (dolist (c classes)
      (let ((name (c-symbol data (class-info-name c) :symbol))
            (supers (c-vector data (mapcar (lambda (s) (address (class-c-name s)))
                                           (class-info-supers c))))
            (cpl (c-vector data (mapcar (lambda (s) (address (class-c-name s)))
                                        (class-info-cpl c)))))
        (push (format nil "static const uint8_t dy_ancestors_~a[] = { ~{~d~^, ~} };~%~
                           ~a~
                           DyClass ~a = { &dy_wrapper_class, &~a, ~a, ~a, ~a, ~a, ~a };~%~
                           DyWrapper ~a = { DY_OBJ(&~a), ~d, ~d, ~d, DY_KIND_~a, ~a, dy_ancestors_~a, ~a };"
                      (class-info-c-name c) (ancestor-bytes c count)
                      (if (class-info-slot-offsets c)
                          (format nil "static const int32_t dy_slot_offsets_~a[] = { ~{~d~^, ~} };~%"
                                  (class-info-c-name c)
                                  (coerce (class-info-slot-offsets c) 'list))
                          "")
                      (class-c-name c) (wrapper-c-name c) (address name)
                      (address supers) (address cpl)
                      (address (c-vector data (mapcar (lambda (s) (address (class-c-name s)))
                                                      (class-info-subclasses c))))
                      (or (class-info-maker c) "NULL")
                      (wrapper-c-name c) (class-c-name c)
                      (class-info-id c) (class-info-subtree-end c) (class-info-size c)
                      (substitute #\_ #\- (string-upcase (class-info-kind c)))
                      (let ((flags (append (and (class-info-range-ok-p c) '("DY_WRAPPER_RANGE_OK"))
                                           (and (class-info-abstract-p c) '("DY_WRAPPER_ABSTRACT"))
                                           (and (class-info-sealed-p c) '("DY_WRAPPER_SEALED")))))
                        (if flags (format nil "~{~a~^ | ~}" flags) "0"))
                      (class-info-c-name c)
                      (if (class-info-slot-offsets c)
                          (format nil "dy_slot_offsets_~a" (class-info-c-name c))
                          "NULL"))
              class-text)))
    (format stream "/* Generated by the Dylan compiler.  Do not edit. */~%~%#include \"dylan.h\"~%~%")
    (dolist (c classes)
      (format stream "extern DyClass ~a;~%extern DyWrapper ~a;~%"
              (class-c-name c) (wrapper-c-name c))
      (when (class-info-maker c)
        (format stream "D ~a(D, D, D, D, D, intptr_t);~%" (class-info-maker c))))
    (terpri stream)
    (dolist (line prelude) (format stream "~a~%" line))
    (terpri stream)
    (dolist (d (reverse (cdata-definitions data)))
      (format stream "~a~%" d))
    (terpri stream)
    (dolist (text (nreverse class-text))
      (format stream "~a~%~%" text))
    (let ((symbols (reverse (cdata-symbol-list data))))
      (format stream "DySymbol *const dy_static_symbols[] = {~%~{  &~a,~%~}};~%"
              symbols)
      (format stream "const size_t dy_static_symbol_count = ~d;~%~%" (length symbols)))
    (format stream "DyClass *const dy_all_classes[] = {~%~{  &~a,~%~}};~%"
            (mapcar #'class-c-name classes))
    (format stream "const size_t dy_class_count = ~d;~%~%" count)
    ;; The program's thread variables (Phase 7)
    (format stream "D dy_thread_variable_defaults[~d] = { ~{~a~^, ~} };~%"
            (max 1 thread-variable-count)
            (make-list (max 1 thread-variable-count) :initial-element "DY_UNBOUND"))
    (format stream "const size_t dy_thread_variable_count = ~d;~%" thread-variable-count)))
