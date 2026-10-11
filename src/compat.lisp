;;;; -*- Mode: Lisp -*-
;;;;
;;;;              Copyright 1992 Digital Equipment Corporation
;;;;                         All Rights Reserved
;;;;
;;;; Permission to use, copy, and modify this software and its documentation is
;;;; hereby granted only under the following terms and conditions.  Both the
;;;; above copyright notice and this permission notice must appear in all copies
;;;; of the software, derivative works or modified versions, and any portions
;;;; thereof, and both notices must appear in supporting documentation.
;;;;
;;;; Users of this software agree to the terms and conditions set forth herein,
;;;; and hereby grant back to Digital a non-exclusive, unrestricted, royalty-free
;;;; right and license under any changes, enhancements or extensions made to the
;;;; core functions of the software, including but not limited to those affording
;;;; compatibility with other hardware or software environments, but excluding
;;;; applications which incorporate this software.  Users further agree to use
;;;; their best efforts to return to Digital any such changes, enhancements or
;;;; extensions that they make and inform Digital of noteworthy uses of this
;;;; software.  Correspondence should be provided to Digital at:
;;;;
;;;;			Director, Cambridge Research Lab
;;;;			Digital Equipment Corp
;;;;			One Kendall Square, Bldg 700
;;;;			Cambridge MA 02139
;;;;
;;;; This software may be distributed (but not offered for sale or transferred
;;;; for compensation) to third parties, provided such third parties agree to
;;;; abide by the terms and conditions of this notice.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
;;;; WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
;;;; CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;;;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
;;;; ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;;;; SOFTWARE.
;;;;
;;;; The Common Lisp translation is Copyright 2026 Blake McBride.  It is a
;;;; derivative work of Thomas and is governed by the notice above; see
;;;; ../LICENSE.
;;;;
;;;; Facilities that the Scheme implementation of Thomas obtained either
;;;; from the portable/ directory (populations, one-dimensional weak
;;;; tables, records, merge sort, dynamic-wind) or from a Scheme
;;;; implementation-specific kit (kits/MIT/mit-specific.scm and friends).
;;;;
;;;; In Common Lisp most of these are either built in or trivial, so this
;;;; single file replaced all of portable/ and all of the kits, which have
;;;; since been removed.
;;;;
;;;; The Scheme records of portable/record.scm are gone entirely: every
;;;; record type here is statically known, so they have become
;;;; DEFSTRUCTs in the files that used to build them at load time.

(in-package #:dylan)

;;;; Truth
;;;
;;; Thomas was developed on MIT CScheme 7.2, in which #F and '() are the
;;; same object -- generic.scm even remarks on it ("MIT-Scheme bogosity.
;;; (eq? #F '()) => #T!!!").  The class heterarchy has no <boolean>: FALSE
;;; is simply an instance of <empty-list> (see GET-TYPE in runtime-top).
;;; Common Lisp's NIL therefore models the Thomas false/empty-list object
;;; exactly, and T models #T.

;;;; Populations
;;;
;;; A population is a collection of objects with the property that an
;;; object which becomes garbage is no longer a member.  They hold the
;;; direct instances and the direct subclasses of a class.  Members are
;;; kept in a weak EQ hash table, each with a serial number, so adding one
;;; costs constant time; mapping runs newest first, as in the Scheme.
;;; Without weak tables (other than SBCL) members are simply never dropped.

(defstruct (population (:constructor %make-population ())
                       (:print-object print-opaque-object))
  (table #+sbcl (make-hash-table :test 'eq :weakness :key :synchronized nil)
         #-sbcl (make-hash-table :test 'eq))
  (serial 0 :type fixnum))

(defun globally-special-p (symbol)
  "True if SYMBOL is proclaimed special (or global), so that binding it
would not make a lexical variable."
  #+sbcl (and (member (sb-int:info :variable :kind symbol) '(:special :global))
              t)
  #-sbcl (boundp symbol))

(defun make-population* ()
  (%make-population))

(defun population-members (population)
  "The live members, newest first.  The list holds them strongly, so none
can vanish while a caller walks it."
  (let ((pairs '()))
    (maphash (lambda (object serial) (push (cons serial object) pairs))
             (population-table population))
    (mapcar #'cdr (sort pairs #'> :key #'car))))

(defun add-to-population! (population object)
  (let ((table (population-table population)))
    (unless (nth-value 1 (gethash object table))
      (setf (gethash object table) (incf (population-serial population)))))
  population)

(defun remove-from-population! (population object)
  (remhash object (population-table population))
  population)

(defun map-over-population (population procedure)
  "Collect PROCEDURE's values over the live members, as a list."
  (mapcar procedure (population-members population)))

(defun map-over-population! (population procedure)
  "Apply PROCEDURE to each live member for effect."
  (mapc procedure (population-members population))
  t)

;;;; One-dimensional tables
;;;
;;; The Scheme version keyed weakly off arbitrary objects; generic.scm uses
;;; them to associate parameter lists with methods and data with generic
;;; functions, both of which are closures.  A weak EQ hash table does the
;;; same job.

(defun make-oned-table ()
  #+sbcl (make-hash-table :test 'eq :weakness :key :synchronized nil)
  #-sbcl (make-hash-table :test 'eq))

(defun oned-table/get (table key default)
  (gethash key table default))

(defun oned-table/put! (table key value)
  (setf (gethash key table) value)
  nil)

;;;; Escape continuations
;;;
;;; CALL-WITH-CURRENT-CONTINUATION is needed only for escaping: the
;;; BIND-EXIT and HANDLER-CASE special forms, MEMBER?, and the test
;;; harness.  A CATCH/THROW pair provides exactly that, and it composes
;;; correctly with UNWIND-PROTECT (which is what DYNAMIC-WIND became).

(defun call-with-escape-continuation (receiver)
  (let ((tag (list '#:escape)))
    (catch tag
      (funcall receiver
               (lambda (&optional value) (throw tag value))))))

(defmacro dynamic-wind (before thunk after)
  "The Scheme DYNAMIC-WIND, restricted to escaping continuations."
  (let ((b (gensym)) (a (gensym)))
    `(let ((,b ,before) (,a ,after))
       (funcall ,b)
       (unwind-protect (funcall ,thunk)
         (funcall ,a)))))

;;;; Numbers
;;;
;;; The arithmetic inherited from Thomas assumes Scheme's exact/inexact
;;; distinction.  Common Lisp's rationals are the exact numbers and its
;;; floats the inexact ones, so the mapping is direct.

(declaim (inline exact? inexact->exact exact->inexact))
(defun exact? (n) (rationalp n))
(defun inexact->exact (n) (if (floatp n) (rational n) n))
(defun exact->inexact (n) (if (rationalp n) (float n 1d0) n))

;;;; Symbols and strings
;;;
;;; MIT Scheme folded symbols to lower case; Common Lisp folds them to
;;; upper case.  All that matters is that the folding be consistent, so
;;; "canonicalize" means "upcase" here.  DYLAN-SYMBOL->STRING undoes it
;;; again, so that Dylan programs see the lower-case names they wrote.

(defun canonicalize-string-for-symbol (string)
  (string-upcase string))

(defun dylan-symbol->string (symbol)
  (string-downcase (symbol-name symbol)))

;;;; Sorting
;;;
;;; The Scheme SORT was a stable merge sort.  Note that the method
;;; ordering predicate answers true for equally specific methods, so a
;;; merge sort is required: STABLE-SORT is one, and CL:SORT need not be.

(defun scheme-sort (sequence predicate)
  "Non-destructive stable sort, as MIT Scheme's SORT."
  (etypecase sequence
    (list (stable-sort (copy-list sequence) predicate))
    (vector (stable-sort (copy-seq sequence) predicate))))

(defun scheme-sort! (sequence predicate)
  (stable-sort sequence predicate))

;;;; The Dylan printer
;;;
;;; Thomas printed its values with the host Scheme's WRITE and DISPLAY.  In
;;; Common Lisp that would show NIL, T and :FOO where Dylan wants (), #t
;;; and foo:, so this implementation brings a printer of its own.

(defun print-opaque-object (object stream)
  (format stream "#[~(~a~) ~x]"
          (type-of object)
          (logand (sxhash object) #xffffff)))

(defvar *dylan-print-escape* t
  "True for WRITE-style output, false for DISPLAY-style output.")

(defun dylan-print (object &optional (stream *standard-output*))
  (labels ((out (x) (write-string x stream))
           (rec (x) (dylan-print x stream)))
    (cond
      ((null object) (out "()"))
      ((eq object t) (out "#t"))
      ((keywordp object) (format stream "~(~a~):" (symbol-name object)))
      ((symbolp object) (out (dylan-symbol->string object)))
      ((stringp object)
       (if *dylan-print-escape*
           (progn (write-char #\" stream)
                  (loop for ch across object
                        do (when (or (char= ch #\") (char= ch #\\))
                             (write-char #\\ stream))
                           (write-char ch stream))
                  (write-char #\" stream))
           (out object)))
      ((characterp object)
       (if *dylan-print-escape*
           (let ((*print-readably* nil)) (format stream "#\\~a" (char-name-or-self object)))
           (write-char object stream)))
      ;; WRITE abbreviates (quote x) as 'x, and Thomas's output was
      ;; produced by Scheme's WRITE.
      ((and (consp object)
            (member (car object)
                    '(quote quasiquote unquote unquote-splicing))
            (consp (cdr object)) (null (cddr object)))
       (out (ecase (car object)
              (quote "'") (quasiquote "`") (unquote ",")
              (unquote-splicing ",@")))
       (rec (cadr object)))
      ((consp object)
       (write-char #\( stream)
       (loop with first = t
             for tail = object then (cdr tail)
             while (consp tail)
             do (if first (setf first nil) (write-char #\Space stream))
                (rec (car tail))
             finally (unless (null tail)
                       (out " . ")
                       (rec tail)))
       (write-char #\) stream))
      ((and (vectorp object) (not (stringp object)))
       (out "#(")
       (loop for i below (length object)
             do (unless (zerop i) (write-char #\Space stream))
                (rec (aref object i)))
       (write-char #\) stream))
      ((class? object)
       (format stream "#[dylan-class ~a]"
               (dylan-symbol->string (class.debug-name object))))
      ((and (instance? object) (condition-message object))
       ;; Chapter 13: a condition prints as its message.
       (out (condition-message object)))
      ((instance? object)
       (format stream "#[instance-of ~a]"
               (dylan-symbol->string
                (class.debug-name (instance.class object)))))
      ((slot? object)
       (format stream "#[dylan-slot ~a]"
               (let ((n (slot.debug-name object)))
                 (if (symbolp n) (dylan-symbol->string n) n))))
      ((singleton? object)
       (out "#[dylan-singleton ")
       (rec (singleton.object object))
       (write-char #\] stream))
      ((functionp object)
       (cond ((dylan%generic-function? object)
              (format stream "#[generic function ~a]"
                      (let ((n (generic-function.name object)))
                        (if (symbolp n) (dylan-symbol->string n) n))))
             ((dylan%method? object) (out "#[method]"))
             (t (out "#[compiled-procedure]"))))
      ((floatp object) (write-string (dylan-float-string object) stream))
      ((complexp object)
       (out "#c(")
       (rec (realpart object))
       (write-char #\Space stream)
       (rec (imagpart object))
       (write-char #\) stream))
      (t (princ object stream)))
    object))

(declaim (special <simple-error> <simple-warning> <type-error>))

(defun condition-message (object)
  "The message of a simple condition or type error, or NIL."
  (let ((class (instance.class object)))
    (cond ((or (subclass? class <simple-error>)
               (subclass? class <simple-warning>))
           (dylan-format-message
            (dylan-call dylan/condition-format-string object)
            (dylan-call dylan/condition-format-arguments object)))
          ((subclass? class <type-error>)
           (dylan-format-message
            "~s is not an instance of ~a"
            (list (dylan-call dylan/type-error-value object)
                  (let ((type (dylan-call dylan/type-error-expected-type
                                          object)))
                    (if (class? type)
                        (class.debug-name type)
                        type)))))
          (t nil))))

(defun dylan-float-string (x)
  "A float as Common Lisp writes one with double-float the default format:
9.0, 0.5, 1.0e20 -- the form the manual's examples show (9.0 in chapter
10), where Thomas wrote Scheme's 9. and .5.  Other float formats keep
their exponent marker (1.5f0), so that they read back as themselves."
  (let ((*read-default-float-format* 'double-float))
    (prin1-to-string x)))

(defun char-name-or-self (ch)
  "The manual's character names (chapter 3) where there is one."
  (case ch
    (#\Space "space")
    (#\Newline "newline")
    (#\Tab "tab")
    (#\Rubout "rubout")
    (#\Page "page")
    (#\Backspace "backspace")
    (#\Return "return")
    (#\Nul "null")
    (t (if (graphic-char-p ch)
           (string ch)
           (or (char-name ch) (string ch))))))

(defun dylan-write (object &optional (stream *standard-output*))
  (let ((*dylan-print-escape* t)) (dylan-print object stream)))

(defun dylan-display (object &optional (stream *standard-output*))
  (let ((*dylan-print-escape* nil)) (dylan-print object stream)))

(defun dylan-newline (&optional (stream *standard-output*))
  (terpri stream)
  nil)

(defun dylan-write-line (object &optional (stream *standard-output*))
  (terpri stream)
  (dylan-write object stream)
  object)

(defvar *dylan-pprint-dispatch*
  (let ((table (copy-pprint-dispatch nil)))
    ;; So that the pretty printer, which does the line breaking, still
    ;; renders Dylan's own notation for these four.
    (set-pprint-dispatch 'null
                         (lambda (s x) (declare (ignore x))
                           (write-string "()" s))
                         2 table)
    (set-pprint-dispatch '(eql t)
                         (lambda (s x) (declare (ignore x))
                           (write-string "#t" s))
                         2 table)
    (set-pprint-dispatch 'keyword
                         (lambda (s x)
                           (format s "~(~a~):" (symbol-name x)))
                         2 table)
    (set-pprint-dispatch 'symbol
                         (lambda (s x)
                           (write-string (dylan-symbol->string x) s))
                         1 table)
    table)
  "A pretty-print dispatch table that lays out Dylan code the way Dylan
writes it.")

(defun pp (object &optional (stream *standard-output*))
  "Pretty print, for the Dylan-visible PP extension, for the compiler's
file output, and for echoing input in the test harness."
  (let ((*print-case* :downcase)
        (*print-right-margin* 79)
        (*print-readably* nil)
        (*print-pprint-dispatch* *dylan-pprint-dispatch*)
        (*print-pretty* t))
    (pprint object stream))
  object)

;;;; The Dylan reader
;;;
;;; Dylan programs are read with the host reader.  The Scheme could do that
;;; directly; Common Lisp's reader cannot, because a colon is a
;;; package marker whose constituent trait no readtable may change, and
;;; Dylan writes its keywords with a trailing colon (foo:).  Nor does
;;; Common Lisp know #t and #f.  So this file provides a reader.
;;;
;;; The surface syntax is small: lists (dotted lists included), the quote
;;; mark, strings, characters, vectors, #t and #f, numbers, symbols,
;;; keywords, and semicolon comments.  Dylan's foo: is read as the Common
;;; Lisp keyword :FOO; every other identifier is interned in this package
;;; with its case folded up, which is what the Lisp reader would have done.

(defun whitespace-char-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun dylan-delimiter-char-p (ch)
  (or (null ch)
      (whitespace-char-p ch)
      (member ch '(#\( #\) #\" #\; #\' #\` #\,))))

(defun skip-dylan-whitespace (stream)
  "Skip whitespace and comments; return the next character without
consuming it, or NIL at end of file."
  (loop for ch = (peek-char nil stream nil nil)
        do (cond ((null ch) (return nil))
                 ((whitespace-char-p ch) (read-char stream))
                 ((char= ch #\;)
                  (loop for c = (read-char stream nil nil)
                        until (or (null c) (char= c #\Newline))))
                 ((and (char= ch #\#) (eql (peek-dylan-second-char stream) #\|))
                  (read-char stream) (read-char stream)
                  (skip-dylan-block-comment stream))
                 (t (return ch)))))

(defun read-dylan-string (stream)
  (read-char stream)                    ; the opening quote
  (with-output-to-string (out)
    (loop for ch = (read-char stream nil nil)
          do (cond ((null ch)
                    (error "End of file inside a Dylan string constant."))
                   ((char= ch #\") (return))
                   ((char= ch #\\)
                    (let ((next (read-char stream nil nil)))
                      (when (null next)
                        (error "End of file inside a Dylan string constant."))
                      (write-char (case next
                                    (#\n #\Newline)
                                    (#\t #\Tab)
                                    (t next))
                                  out)))
                   (t (write-char ch out))))))

(defun read-dylan-character (stream)
  "Read what follows #\\ -- either a single character or a character name."
  (let ((first (read-char stream nil nil)))
    (when (null first) (error "End of file after #\\ in Dylan input."))
    (if (and (alpha-char-p first)
             (not (dylan-delimiter-char-p (peek-char nil stream nil nil))))
        (let ((name (with-output-to-string (out)
                      (write-char first out)
                      (loop for ch = (peek-char nil stream nil nil)
                            until (dylan-delimiter-char-p ch)
                            do (write-char (read-char stream) out)))))
          (or (name-char name)
              (error "Unknown character name #\\~a in Dylan input." name)))
        first)))

(defun dylan-token-number (token)
  "The number TOKEN denotes, or NIL if it does not denote one."
  (let* ((n (length token))
         (i (if (and (plusp n) (member (char token 0) '(#\+ #\-))) 1 0))
         (digits 0) (dots 0) (slashes 0) (exponents 0))
    (when (>= i n) (return-from dylan-token-number nil))
    (loop while (< i n)
          for ch = (char token i)
          do (cond ((digit-char-p ch) (incf digits) (incf i))
                   ((char= ch #\.) (incf dots) (incf i))
                   ((char= ch #\/) (incf slashes) (incf i))
                   ((and (member ch '(#\e #\E #\s #\S #\f #\F #\d #\D #\l #\L))
                         (plusp digits)
                         (< (+ i 1) n))
                    (incf exponents)
                    (incf i)
                    (when (member (char token i) '(#\+ #\-)) (incf i)))
                   (t (return-from dylan-token-number nil))))
    (when (or (zerop digits) (> dots 1) (> slashes 1) (> exponents 1)
              (and (plusp slashes) (or (plusp dots) (plusp exponents))))
      (return-from dylan-token-number nil))
    (let ((*read-base* 10)
          (*read-default-float-format* 'double-float)
          (*readtable* (copy-readtable nil)))
      (multiple-value-bind (value end)
          (ignore-errors (read-from-string token nil nil))
        (and (numberp value) (eql end n) value)))))

(defun dylan-token-object (token)
  (or (dylan-token-number token)
      (let ((n (length token)))
        (if (and (> n 1) (char= #\: (char token (1- n))))
            (intern (canonicalize-string-for-symbol (subseq token 0 (1- n)))
                    :keyword)
            (dylan-symbol (canonicalize-string-for-symbol token))))))

(defun dylan-symbol (name)
  "The Dylan symbol named NAME (already canonicalized).  T and NIL are
ordinary symbols in Dylan, but in this package they would be Common Lisp's
T and NIL -- #t and #f -- so they get lower-case names of their own."
  (if (member name '("T" "NIL") :test #'string=)
      (intern (string-downcase name) :dylan)
      (intern name :dylan)))

(defun read-dylan-token (stream)
  (let ((token (with-output-to-string (out)
                 (loop for ch = (peek-char nil stream nil nil)
                       until (dylan-delimiter-char-p ch)
                       do (write-char (read-char stream) out)))))
    (when (zerop (length token))
      (error "Unexpected character ~s in Dylan input."
             (peek-char nil stream nil nil)))
    (dylan-token-object token)))

(defun peek-dylan-second-char (stream)
  "The character after the one PEEK-CHAR would return, or NIL."
  (let ((first (read-char stream nil nil)))
    (if (null first)
        nil
        (let ((second (peek-char nil stream nil nil)))
          (unread-char first stream)
          second))))

(defun read-dylan-list (stream close)
  "Read the elements of a list, up to and including CLOSE.  A lone dot
introduces the tail of a dotted list."
  (let ((head '()) (tail nil))
    (loop
      (let ((ch (skip-dylan-whitespace stream)))
        (cond
          ((null ch) (error "End of file inside a Dylan list."))
          ((char= ch close) (read-char stream) (return head))
          ((and (char= ch #\.)
                (dylan-delimiter-char-p (peek-dylan-second-char stream)))
           (read-char stream)
           (unless tail (error "Misplaced dot in a Dylan list."))
           (setf (cdr tail) (read-dylan-object stream))
           (let ((next (skip-dylan-whitespace stream)))
             (unless (and next (char= next close))
               (error "Malformed dotted list in Dylan input."))
             (read-char stream)
             (return head)))
          (t (let ((cell (list (read-dylan-object stream))))
               (if tail (setf (cdr tail) cell) (setf head cell))
               (setf tail cell))))))))

(defun read-dylan-token-string (stream)
  (with-output-to-string (out)
    (loop for ch = (peek-char nil stream nil nil)
          until (dylan-delimiter-char-p ch)
          do (write-char (read-char stream) out))))

(defun skip-dylan-block-comment (stream)
  "Skip the rest of a #| ... |# comment, which may nest (chapter 3)."
  (loop with depth = 1
        for ch = (read-char stream nil nil)
        do (cond ((null ch) (error "End of file inside a #| comment."))
                 ((and (char= ch #\|) (eql (peek-char nil stream nil nil) #\#))
                  (read-char stream)
                  (when (zerop (decf depth)) (return)))
                 ((and (char= ch #\#) (eql (peek-char nil stream nil nil) #\|))
                  (read-char stream)
                  (incf depth)))))

(defun read-dylan-radix-number (stream radix)
  (let* ((token (read-dylan-token-string stream))
         (value (let ((*read-base* radix)
                      (*readtable* (copy-readtable nil)))
                  (and (every (lambda (ch)
                                (or (digit-char-p ch radix)
                                    (member ch '(#\+ #\- #\/))))
                              token)
                       (ignore-errors (read-from-string token))))))
    (if (rationalp value)
        value
        (error "Bad radix ~a number in Dylan input: ~a" radix token))))

(defun read-dylan-sharp (stream)
  (read-char stream)                    ; the #
  (let ((ch (read-char stream nil nil)))
    (case ch
      ((nil) (error "End of file after # in Dylan input."))
      ((#\\) (read-dylan-character stream))
      ((#\() (coerce (read-dylan-list stream #\)) 'simple-vector))
      ((#\x #\X) (read-dylan-radix-number stream 16))
      ((#\o #\O) (read-dylan-radix-number stream 8))
      ((#\b #\B) (read-dylan-radix-number stream 2))
      ((#\c #\C)
       ;; A complex number, written as Common Lisp writes one: #c(1 2).
       (unless (eql (read-char stream nil nil) #\()
         (error "Bad #c syntax in Dylan input."))
       (let ((parts (read-dylan-list stream #\))))
         (unless (and (= (length parts) 2) (every #'realp parts))
           (error "Bad #c syntax in Dylan input."))
         (complex (first parts) (second parts))))
      (t
       ;; #t, #f, and the parameter-list tokens #key, #rest and #next
       ;; (chapter 3), which are read as the !key, !rest and !next that
       ;; the compiler uses for them.
       (let ((name (concatenate 'string (string ch)
                                (read-dylan-token-string stream))))
         (cond ((string-equal name "t") t)
               ((string-equal name "f") nil)
               ((member name '("key" "rest" "next") :test #'string-equal)
                (dylan-symbol (canonicalize-string-for-symbol
                               (concatenate 'string "!" name))))
               (t (error "Unknown # syntax #~a in Dylan input." name))))))))

(defun read-dylan-object (stream)
  (let ((ch (skip-dylan-whitespace stream)))
    (cond ((null ch) (error "End of file in Dylan input."))
          ((char= ch #\() (read-char stream) (read-dylan-list stream #\)))
          ((char= ch #\)) (error "Unbalanced close parenthesis in Dylan input."))
          ((char= ch #\") (read-dylan-string stream))
          ((char= ch #\') (read-char stream)
                          (list 'quote (read-dylan-object stream)))
          ;; Backquote (chapter 3): `x, ,x and ,@x read as (quasiquote x),
          ;; (unquote x) and (unquote-splicing x).
          ((char= ch #\`) (read-char stream)
                          (list 'quasiquote (read-dylan-object stream)))
          ((char= ch #\,) (read-char stream)
                          (if (eql (peek-char nil stream nil nil) #\@)
                              (progn (read-char stream)
                                     (list 'unquote-splicing
                                           (read-dylan-object stream)))
                              (list 'unquote (read-dylan-object stream))))
          ((char= ch #\#) (read-dylan-sharp stream))
          (t (read-dylan-token stream)))))

(defun dylan-read (&optional (stream *standard-input*) (eof-error-p t) eof-value)
  "Read one Dylan expression."
  (let ((ch (skip-dylan-whitespace stream)))
    (cond (ch (read-dylan-object stream))
          (eof-error-p (error "End of file during DYLAN-READ."))
          (t eof-value))))

;;;; Conditions
;;;
;;; The Scheme kits reflected the host condition system into Dylan's; here
;;; the host is Common Lisp.  DYLAN-CONDITION carries a message and a list
;;; of irritants, exactly as MIT Scheme's simple errors did.

;;; An error message is a format string and its arguments (chapter 13's
;;; <simple-error>; the manual's examples use Common Lisp's directives).
;;; ~A and ~S print an argument as Dylan's DISPLAY and WRITE do, ~D as
;;; DISPLAY, ~% and ~& start a line and ~~ is a tilde; any other directive is
;;; left as it is.  Arguments no directive consumes follow the message, as
;;; the runtime's own MIT-style error calls (message, then irritants)
;;; expect.

(defun dylan-format-message (control arguments)
  (with-output-to-string (out)
    (let ((args arguments)
          (n (length control))
          (i 0))
      (loop while (< i n)
            do (let ((ch (char control i)))
                 (if (and (char= ch #\~) (< (1+ i) n))
                     (let ((directive (char-downcase (char control (1+ i)))))
                       (case directive
                         ((#\a #\d) (if args
                                        (dylan-display (pop args) out)
                                        (write-string "~a" out)))
                         ((#\s) (if args
                                    (dylan-write (pop args) out)
                                    (write-string "~s" out)))
                         ((#\%) (terpri out))
                         ((#\&) (fresh-line out))
                         ((#\~) (write-char #\~ out))
                         (t (write-char ch out)
                            (write-char (char control (1+ i)) out)))
                       (incf i 2))
                     (progn (write-char ch out) (incf i)))))
      (dolist (arg args)
        (write-char #\Space out)
        (dylan-write arg out)))))

(define-condition dylan-condition (error)
  ((message :initarg :message :initform "" :reader dylan-condition-message)
   (irritants :initarg :irritants :initform '()
              :reader dylan-condition-irritants))
  (:report (lambda (condition stream)
             (write-string "Error: " stream)
             (write-string (dylan-format-message
                            (dylan-condition-message condition)
                            (dylan-condition-irritants condition))
                           stream))))

(define-condition unhandled-dylan-condition (error)
  ((dylan-condition :initarg :dylan-condition :reader unhandled-dylan-condition-object))
  (:report (lambda (condition stream)
             (write-string "Unhandled Dylan condition: " stream)
             (dylan-write (unhandled-dylan-condition-object condition) stream))))

(defvar *reflected-error-tag* "Lisp condition: "
  "Used, by identity, to recognise a Dylan condition that merely reflects a
native Lisp one.")

;;; The compiler's own error procedure (dylan::error in the Scheme).

(defun dylan%error (string &rest args)
  (error 'dylan-condition :message string
                          :irritants args))

;;;; The implementation-specific interface used by runtime-exceptions

(defvar *dylan-handler-frames* '())

(defun implementation-specific/push-handler (type function test description thunk)
  (let ((*dylan-handler-frames*
          (cons (list type function test description) *dylan-handler-frames*)))
    (funcall thunk)))

(defun implementation-specific/get-dylan-handler-frames ()
  *dylan-handler-frames*)

(defun implementation-specific/enter-debugger (dylan-condition)
  (break "Dylan breakpoint: ~a" (with-output-to-string (s)
                                   (dylan-write dylan-condition s))))

(defun implementation-specific/induce-error (format-string format-args)
  (error 'dylan-condition
         :message format-string
         :irritants format-args))

(defun implementation-specific/induce-type-error (value class-name)
  (error 'dylan-condition
         :message "~s is not an instance of ~a"
         :irritants (list value class-name)))

(defun implementation-specific/return-from-error (dylan-condition)
  "Chapter 13: if a handler returns from ERROR, error bombs directly to the
debugger -- without signalling again, which would only find the same
handlers."
  (invoke-debugger
   (make-condition 'dylan-condition
                   :message "a handler returned from ERROR, which may not return:"
                   :irritants (list dylan-condition))))

(defun implementation-specific/signal-unhandled-dylan-condition (dylan-condition)
  (error 'unhandled-dylan-condition :dylan-condition dylan-condition))

(defun implementation-specific/warning (format-string format-args)
  (warn "~a" (dylan-format-message format-string format-args)))

(defun implementation-specific/catch-all-errors (handler thunk)
  ;; A handler that returns normally declines, and the condition goes on to
  ;; the next Lisp handler -- which is what
  ;; IMPLEMENTATION-SPECIFIC/LET-LISP-HANDLE-IT relies on.
  (handler-bind ((error handler))
    (funcall thunk)))

(defun implementation-specific/get-error-message (condition)
  (typecase condition
    (dylan-condition (dylan-condition-message condition))
    (simple-condition (let ((c (simple-condition-format-control condition)))
                        (if (stringp c) c *reflected-error-tag*)))
    (t *reflected-error-tag*)))

(defun implementation-specific/get-error-arguments (condition)
  (typecase condition
    (dylan-condition (dylan-condition-irritants condition))
    (simple-condition (if (stringp (simple-condition-format-control condition))
                          (simple-condition-format-arguments condition)
                          (list condition)))
    (t (list condition))))

(defun implementation-specific/is-reflected-error? (f-string f-args)
  (and (eq *reflected-error-tag* f-string)
       (consp f-args)
       (typep (car f-args) 'condition)
       (null (cdr f-args))))

(defun implementation-specific/let-lisp-handle-it (serious)
  (declare (ignore serious))
  ;; Returning declines the condition, so the remaining Lisp handlers run.
  nil)

;;;; Evaluation and file generation

(defun implementation-specific/eval (forms)
  "Evaluate the list of compiled top-level FORMS, returning the last value.
Compiled Dylan binds more variables than it always uses -- the two extra
arguments of the Dylan calling convention, for one -- so the style warnings
the host compiler would produce for it are muffled."
  (let ((result nil))
    (handler-bind ((style-warning #'muffle-warning))
      (dolist (form forms result)
        (setf result (eval form))))))

(defun lisp-pp (object &optional (stream *standard-output*))
  "Pretty print OBJECT as Common Lisp, for the compiler's file output.
Unlike PP this uses the standard dispatch table, so that what is written
can be read back."
  (let ((*print-case* :downcase)
        (*print-right-margin* 79)
        (*print-readably* nil)
        (*print-pretty* t)
        (*package* (find-package '#:dylan)))
    (pprint object stream))
  object)

(defun implementation-specific/generate-file (in-exprs out-forms stream)
  (format stream "~&;;;; Input expressions:~%")
  (dolist (expr in-exprs)
    (let ((text (with-output-to-string (s) (pp expr s))))
      (with-input-from-string (s text)
        (loop for line = (read-line s nil) while line
              do (format stream "~:[~;; ~]~a~%" (plusp (length line)) line)))))
  (format stream "~&~%;;;; Compiled output:~%~%")
  (format stream "(in-package #:dylan)~%")
  (dolist (form out-forms)
    (lisp-pp form stream)
    (terpri stream)))


;;;; Language extensions
;;;
;;; The MIT kit added three Dylan-visible procedures of its own (pp,
;;; scheme-variable and scheme-procedure); the Common Lisp equivalents are
;;; defined in runtime-extensions.lisp.  Only the names are needed here,
;;; because compiler.lisp splices this table into
;;; DYLAN%LISP-NAMES-OF-PREDEFINED-NAMES at load time, before the runtime
;;; has been built.

(declaim (special dylan/pp dylan/lisp-variable dylan/lisp-procedure))

(defparameter implementation-specific/additional-dylan-bindings
  '((pp dylan/pp)
    (lisp-variable dylan/lisp-variable)
    (lisp-procedure dylan/lisp-procedure)))
