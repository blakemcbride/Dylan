;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Static C data: the program's constants, symbols and classes, in the
;;;; representation of runtime/c/dylan.h.
;;;;
;;;; A CDATA collects definitions as constants are asked for; C-CONSTANT
;;;; answers a C expression of type D for a Dylan constant, defining static
;;;; objects for it as needed.  A literal string, list or vector of the
;;;; program is its own object, as reading it makes it in the interactive
;;;; implementation -- one for each place the program writes it, however
;;;; often that runs -- named for what it holds and where it was written;
;;;; equal constants of the compiler's own are one object, named for what
;;;; they hold (CONTENT-NAME); a symbol (or keyword) is defined once
;;;; whatever the case it is written in, with the spelling it first had,
;;;; and every one is listed in dy_static_symbols for the runtime to intern
;;;; when it starts.
;;;;
;;;; EMIT-PROGRAM-DATA writes the whole: the definitions, then a wrapper and
;;;; a class object for every class, then the tables the runtime reads.

(in-package #:dylan-compiler)

(defvar *cdata* nil "The CDATA of the program being compiled.")

(defstruct (cdata (:constructor make-cdata ()))
  (counter 0)
  (definitions '())                     ; C text, newest first
  (named (make-hash-table :test #'equal)) ; C name -> the contents it was
                                        ; made for (CONTENT-NAME)
  (strings (make-hash-table :test #'equal))
  (symbols (make-hash-table :test #'equal))   ; folded name -> C name
  (symbol-list '())                     ; C names, newest first
  (keywords (make-hash-table :test #'equal)))

(defun cdata-name (data prefix)
  (format nil "~a~d" prefix (incf (cdata-counter data))))

(defun content-hash (string)
  "A hash of STRING that is the same in every run of the compiler: 64-bit
FNV-1a over its character codes, as 16 hex digits."
  (let ((h #xcbf29ce484222325))
    (loop for ch across string
          do (setf h (ldb (byte 64 0) (* (logxor h (char-code ch)) #x100000001b3))))
    (format nil "~(~16,'0x~)" h)))

(defun content-name (data prefix content)
  "The C name of the object PREFIX names for CONTENT, a string saying all
that the object holds; and whether it is already defined.  The name comes
from CONTENT alone, not the order constants are met, so that the library's
C is the same whatever program it is compiled with (Phase 9, separate
compilation)."
  (let* ((name (concatenate 'string prefix (content-hash content)))
         (old (gethash name (cdata-named data))))
    (when (and old (string/= old content))
      (error "Internal error: two constants hash to ~a." name))
    (setf (gethash name (cdata-named data)) content)
    (values name (and old t))))

(defun define-c (data control &rest arguments)
  (push (apply #'format nil control arguments) (cdata-definitions data)))

(defun address (c-name) (format nil "DY_OBJ(&~a)" c-name))

;;; C literals

(defun c-utf8-literal (string)
  "A C string literal holding STRING, as UTF-8: for the runtime's
messages, which it reads as UTF-8, and for file names."
  (c-string-literal (coerce (sb-ext:string-to-octets string :external-format :utf-8)
                            'list)))

(defun c-string-literal (codes)
  "A C string literal holding the bytes CODES (integers below 256)."
  (assert (every (lambda (c) (< c 256)) codes) ()
          "Internal error: a C string of more than bytes.")
  (with-output-to-string (out)
    (write-char #\" out)
    (dolist (c codes)
      ;; ? too, so that no ?? begins a trigraph, which C11 reads.
      (cond ((or (= c 34) (= c 92) (= c 63)) (format out "\\~c" (code-char c)))
            ((<= 32 c 126) (write-char (code-char c) out))
            ;; Always three digits, so a digit after it cannot extend it.
            (t (format out "\\~3,'0o" c))))
    (write-char #\" out)))

(defun c-comment-text (string)
  "STRING, to go inside a C comment: with no */ to end it, and no ?? to
begin a trigraph."
  (with-output-to-string (out)
    (loop for i from 0 below (length string)
          for c = (char string i)
          for next = (and (< (1+ i) (length string)) (char string (1+ i)))
          do (write-char c out)
             (when (or (and (char= c #\*) (eql next #\/))
                       (and (char= c #\?) (eql next #\?)))
               (write-char #\\ out)))))

(defun c-float-literal (x)
  "X, a finite float, as an exact C hexadecimal floating constant."
  (multiple-value-bind (mantissa exponent sign) (integer-decode-float x)
    (format nil "~:[~;-~]0x~xp~d~:[~;f~]"
            (minusp sign) mantissa exponent (typep x 'single-float))))

;;; Constants

(defvar *literal-keys* nil
  "An EQ table: each string, list and vector the program's source holds ->
where it was written, \"file:index\"; or NIL.")

(defun literal-key (object inherited)
  "The key that makes OBJECT its own constant: where it was written, or,
for a part of a literal that has no place of its own (a list's tail),
INHERITED, made from the literal's."
  (or (and *literal-keys* (gethash object *literal-keys*)) inherited))

(defun c-constant (data value &optional key)
  "A C expression of type D for the Dylan constant VALUE.  KEY: as for
LITERAL-KEY."
  (cond ((integerp value)
         (check-type value (integer #.(- (expt 2 62)) #.(1- (expt 2 62))))
         (format nil "DY_INT(~d)" value))
        ((characterp value) (format nil "DY_CHAR(~d)" (char-code value)))
        ((eq value t) "DY_TRUE")
        ((eq value dylan::dylan%false) "DY_FALSE")
        ((null value) "DY_EMPTY")
        ((stringp value) (address (c-string data value (literal-key value key))))
        ((keywordp value)
         (address (c-symbol data (dylan::dylan-symbol->string value) :keyword)))
        ((symbolp value)
         (address (c-symbol data (dylan::dylan-symbol->string value) :symbol)))
        ((consp value) (address (c-pair data value (literal-key value key))))
        ((vectorp value)
         (let ((key (literal-key value key)))
           (address (c-vector data (loop for x across value
                                         for i from 0
                                         collect (c-constant data x (and key (format nil "~a.~d" key i))))
                              key))))
        ((typep value 'ratio)
         (multiple-value-bind (name old)
             (content-name data "dy_ratio_" (format nil "~d/~d" (numerator value) (denominator value)))
           (unless old
             (define-c data "static DyRatio ~a = { &dy_wrapper_ratio, DY_INT(~d), DY_INT(~d) };"
                       name (numerator value) (denominator value)))
           (address name)))
        ((typep value 'single-float)
         (multiple-value-bind (name old)
             (content-name data "dy_float_" (c-float-literal value))
           (unless old
             (define-c data "static DySingleFloat ~a = { &dy_wrapper_single_float, ~a };"
                       name (c-float-literal value)))
           (address name)))
        ((typep value 'double-float)
         (multiple-value-bind (name old)
             (content-name data "dy_double_" (c-float-literal value))
           (unless old
             (define-c data "static DyDoubleFloat ~a = { &dy_wrapper_double_float, ~a };"
                       name (c-float-literal value)))
           (address name)))
        ((dylan::extended-float? value)
         (let ((d (dylan::extended-float-value value)))
           (multiple-value-bind (name old)
               (content-name data "dy_extended_" (c-float-literal d))
             (unless old
               (define-c data "static DyDoubleFloat ~a = { &dy_wrapper_extended_float, ~a };"
                         name (c-float-literal d)))
             (address name))))
        (t (error "No C representation for the constant ~s." value))))

(defun c-string (data string &optional key)
  "The C name of a static string object holding STRING: a <byte-string>
if every character fits in a byte, else a <unicode-string>.  KEY, if not
NIL, makes it the object of the literal written there."
  (or (gethash (cons string key) (cdata-strings data))
      (setf (gethash (cons string key) (cdata-strings data))
            (let* ((codes (map 'list #'char-code string))
                   (name (content-name data "dy_string_"
                                       (format nil "~{~d~^ ~}~@[ @~a~]" codes key))))
              (if (every (lambda (c) (< c 256)) codes)
                  (define-c data "static struct { DyWrapper *wrapper; D size; char data[~d]; } ~a =~%  { &dy_wrapper_byte_string, DY_INT(~d), ~a };"
                            (1+ (length codes)) name (length codes)
                            (c-string-literal codes))
                  (define-c data "static struct { DyWrapper *wrapper; D size; uint32_t data[~d]; } ~a =~%  { &dy_wrapper_unicode_string, DY_INT(~d), { ~{~du~^, ~} } };"
                            (max 1 (length codes)) name (length codes) codes))
              name))))

(defvar *symbol-forms* nil
  "The program's SYMBOL-FORMS: which top-level form first spells each
symbol (front-end.lisp), or NIL.")

(defun c-symbol (data spelling kind)
  "The C name of the static symbol (KIND :SYMBOL) or keyword (:KEYWORD)
named SPELLING."
  (let ((table (if (eq kind :keyword) (cdata-keywords data) (cdata-symbols data)))
        (key (string-downcase spelling)))
    (or (gethash key table)
        (setf (gethash key table)
              (let ((string (c-string data spelling))
                    (name (content-name data (if (eq kind :keyword) "dy_keyword_" "dy_symbol_")
                                        key))
                    ;; The top-level form that makes it, if the program's;
                    ;; one no form spells, a #key parameter's keyword, is
                    ;; never made by the program, and as may make it.
                    (form (if *symbol-forms*
                              (or (gethash (format nil "~:[s~;k~]:~a" (eq kind :keyword) key)
                                           *symbol-forms*)
                                  "INTPTR_MAX")
                              0)))
                (define-c data "DySymbol ~a = { &dy_wrapper_~(~a~), ~a, 0, ~a };"
                          name kind (address string) form)
                (push name (cdata-symbol-list data))
                name)))))

(defun c-pair (data list &optional key)
  "The C name of a static pair holding LIST.  KEY, if not NIL, makes it
the object of the literal written there; equal lists of the compiler's own
are one object."
  ;; A loop down the list, not a recursion, so that a long one does not
  ;; exhaust the stack; and the Nth tail's key is KEY.dN, not KEY.d.d...,
  ;; so that the keys do not grow with the list.
  (let ((pairs '())                     ; (pair . key), the last first
        (base key)
        (n 0))
    (loop for x = list then (cdr x)
          for first = t then nil
          while (consp x)
          do (let ((own (and (not first) *literal-keys* (gethash x *literal-keys*))))
               (when own (setf base own n 0))
               (push (cons x (cond (first key)
                                   (own own)
                                   (base (format nil "~a.d~d" base n))))
                     pairs)
               (incf n)))
    (let ((tail (c-constant data (cdr (car (first pairs)))
                            (and base (format nil "~a.d~d" base n))))
          (name nil))
      (loop for (pair . k) in pairs
            do (let ((head (c-constant data (car pair) (and k (format nil "~a.a" k)))))
                 (multiple-value-bind (pair-name old)
                     (content-name data "dy_pair_" (format nil "~a ~a~@[ @~a~]" head tail k))
                   (unless old
                     (define-c data "static DyPair ~a = { &dy_wrapper_pair, ~a, ~a };"
                               pair-name head tail))
                   (setf name pair-name
                         tail (address pair-name)))))
      name)))

(defun c-vector (data elements &optional key)
  "The C name of a static <simple-object-vector> of ELEMENTS, C
expressions.  KEY, if not NIL, makes it the object of the literal written
there; equal ones of the compiler's own are one object."
  (multiple-value-bind (name old) (content-name data "dy_vector_"
                                                (format nil "~{~a~^ ~}~@[ @~a~]" elements key))
    (unless old
      (define-c data "static struct { DyWrapper *wrapper; D size; D data[~d]; } ~a =~%  { &dy_wrapper_simple_object_vector, DY_INT(~d), { ~{~a~^, ~} } };"
                (max 1 (length elements)) name (length elements) elements))
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
    ;; Every symbol and keyword the program spells, a variable's name too,
    ;; so that as finds it with the program's spelling, as the reader
    ;; would have made it (chapter 15).
    (when *symbol-forms*
      (let ((spelled '()))
        (maphash (lambda (symbol spelling)
                   (let* ((kind (if (keywordp symbol) :keyword :symbol))
                          (form (gethash (format nil "~:[s~;k~]:~a" (eq kind :keyword)
                                                 (string-downcase spelling))
                                         *symbol-forms*)))
                     (when (and (integerp form) (plusp form))
                       (push (list (string-downcase spelling) spelling kind) spelled))))
                 dylan::*symbol-spellings*)
        (loop for (nil spelling kind) in (sort spelled #'string< :key #'first)
              do (c-symbol data spelling kind))))
    ;; The class objects' own constants go into DATA first.
    (dolist (c classes)
      (let ((name (c-symbol data (class-info-name c) :symbol))
            (supers (c-vector data (mapcar (lambda (s) (address (class-c-name s)))
                                           (class-info-supers c))))
            (cpl (c-vector data (mapcar (lambda (s) (address (class-c-name s)))
                                        (class-info-cpl c)))))
        (push (format nil "static const uint8_t dy_ancestors_~a[] = { ~{~d~^, ~} };~%~
                           ~a~
                           DyClass ~a = { &dy_wrapper_class, &~a, ~a, ~a, ~a, ~a, ~a, ~d };~%~
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
                      ;; The manual's sealed classes are sealed from the start
                      ;; (chapter 11: all-superclasses and the rest answer
                      ;; an empty sequence)
                      (if (class-info-sealed-p c) 1 0)
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
        (format stream "D ~a(D, D, D, D, D, intptr_t);~%" (class-info-maker c))
        (format stream "D dy_fill_~a(D, D, D, D, D, D, intptr_t);~%" (class-info-c-name c))))
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
