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
;;; same object.  The Dylan manual says otherwise: #f is the only false
;;; value, and the empty list is true (chapters 3 and 8, footnote 17).  Here
;;; the empty list is Common Lisp's NIL -- Dylan lists are Lisp lists -- and
;;; #f is an object of its own, the value of DYLAN%FALSE.  #t is T.
;;;
;;; So a Dylan value is tested with TRUE?, never by Lisp's own IF, and a
;;; Lisp truth value handed to Dylan is turned into #t or #f with BOOL.
;;; Inside the runtime, Lisp functions that are not seen by Dylan keep
;;; Lisp's NIL as their false.

(defstruct (dylan-false (:constructor %make-dylan-false ())
                        (:predicate nil)
                        (:copier nil)))

(defvar *print-false-for-lisp* nil
  "True while compiled code is written to a file, so that #f is written
as a form that reads back as the same object.")

(defmethod print-object ((object dylan-false) stream)
  (write-string (if *print-false-for-lisp* "#.dylan::dylan%false" "#f")
                stream))

(defvar dylan%false (%make-dylan-false)
  "Dylan's #f.")

(defmethod make-load-form ((object dylan-false) &optional environment)
  (declare (ignore environment))
  'dylan%false)

(declaim (inline true? false? bool))
(defun true? (object)
  "Is the Dylan value OBJECT true -- anything but #f?"
  (not (eq object dylan%false)))
(defun false? (object)
  (eq object dylan%false))
(defun bool (generalized-boolean)
  "Dylan's #t or #f for a Lisp truth value."
  (if generalized-boolean t dylan%false))

(defun or-false (object)
  "OBJECT, or #f for Lisp's NIL -- for a Lisp function answering an object
or NIL for none, where Dylan wants the object or #f."
  (if object object dylan%false))

(defun false->nil (object)
  "NIL for #f, else OBJECT -- for a Dylan value about to be tested by Lisp."
  (if (false? object) nil object))

(defun boolean-function (lisp-predicate)
  "A function answering Dylan's #t or #f where LISP-PREDICATE answers a
Lisp truth value."
  (lambda (&rest arguments) (bool (apply lisp-predicate arguments))))

;;;; Populations
;;;
;;; A population is a collection of objects with the property that an
;;; object which becomes garbage is no longer a member.  They hold the
;;; direct instances and the direct subclasses of a class.  Members are
;;; kept in a weak EQ hash table, each with a serial number, so adding one
;;; costs constant time; mapping runs newest first, as in the Scheme.
;;; Without weak tables (other than SBCL) members are simply never dropped.
;;;
;;; An object known not to be a member yet -- a new instance -- is added
;;; with ADD-NEW-TO-POPULATION!, which skips the table (SBCL locks a weak
;;; table on every access) and pushes (SERIAL . WEAK-POINTER) onto FRESH.
;;; FRESH is cleared of dead entries when it has grown to twice its length
;;; after the last clearing.

(defstruct (population (:constructor %make-population ())
                       (:print-object print-opaque-object))
  (table #+sbcl (make-hash-table :test 'eq :weakness :key :synchronized nil)
         #-sbcl (make-hash-table :test 'eq))
  (serial 0 :type fixnum)
  (fresh '())
  (fresh-count 0 :type fixnum)
  (fresh-limit 1024 :type fixnum))

(defun make-weak-reference (object)
  #+sbcl (sb-ext:make-weak-pointer object)
  #-sbcl object)

(defun weak-reference-value (reference)
  "The object REFERENCE holds, or NIL if it has been collected."
  #+sbcl (values (sb-ext:weak-pointer-value reference))
  #-sbcl reference)

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
    (dolist (entry (population-fresh population))
      (let ((object (weak-reference-value (cdr entry))))
        (when object (push (cons (car entry) object) pairs))))
    (mapcar #'cdr (sort pairs #'> :key #'car))))

(defun add-to-population! (population object)
  (let ((table (population-table population)))
    (unless (nth-value 1 (gethash object table))
      (setf (gethash object table) (incf (population-serial population)))))
  population)

(defun add-new-to-population! (population object)
  "Add OBJECT, which is not a member of POPULATION."
  (push (cons (incf (population-serial population))
              (make-weak-reference object))
        (population-fresh population))
  (when (> (incf (population-fresh-count population))
           (population-fresh-limit population))
    (let ((live (delete-if-not (lambda (entry)
                                 (weak-reference-value (cdr entry)))
                               (population-fresh population))))
      (setf (population-fresh population) live
            (population-fresh-count population) (length live)
            (population-fresh-limit population)
            (max 1024 (* 2 (length live))))))
  population)

(defun remove-from-population! (population object)
  (remhash object (population-table population))
  (when (population-fresh population)
    (setf (population-fresh population)
          (delete-if (lambda (entry)
                       (eq (weak-reference-value (cdr entry)) object))
                     (population-fresh population))
          (population-fresh-count population)
          (length (population-fresh population))))
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
;;; "canonicalize" means "upcase" here.
;;;
;;; Chapter 15: symbols and keywords are not case sensitive, but each
;;; remembers the case it was first entered in, and prints in it.  The
;;; reader and AS record that spelling; DYLAN-SYMBOL->STRING answers it,
;;; or the name in lower case for a symbol Dylan has not seen.

(defun canonicalize-string-for-symbol (string)
  (string-upcase string))

(defvar *symbol-spellings* (make-hash-table :test #'eq))

(defun remember-spelling (symbol spelling)
  (unless (gethash symbol *symbol-spellings*)
    (setf (gethash symbol *symbol-spellings*) spelling))
  symbol)

(defun dylan%remember-spellings (pairs)
  "Record the spellings a compiled file's symbols were first entered in."
  (loop for (symbol . spelling) in pairs
        do (remember-spelling symbol spelling)))

(defun dylan-symbol->string (symbol)
  (or (gethash symbol *symbol-spellings*)
      ;; #key, #rest and #next are read as !key, !rest and !next.
      (and (eq (symbol-package symbol) (find-package '#:dylan))
           (cdr (assoc (symbol-name symbol)
                       '(("!KEY" . "#key") ("!REST" . "#rest")
                         ("!NEXT" . "#next"))
                       :test #'string=)))
      (string-downcase (symbol-name symbol))))

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

;;;; Extended floats
;;;
;;; Chapter 14 has three float classes; Common Lisp, as SBCL has it, two
;;; formats, its LONG-FLOAT being its DOUBLE-FLOAT.  So an <extended-float>
;;; is a double-float in a structure of its own, as in the compiler to C
;;; it is a C double with a class of its own.  The numeric generic
;;; functions take it apart (runtime-extended-float.lisp).

(defstruct (extended-float (:constructor make-extended-float (value))
                           (:predicate extended-float?)
                           (:copier nil))
  (value 0d0 :type double-float))

(defmethod make-load-form ((object extended-float) &optional environment)
  (declare (ignore environment))
  `(make-extended-float ,(extended-float-value object)))

;;;; The Dylan printer
;;;
;;; Thomas printed its values with the host Scheme's WRITE and DISPLAY.  In
;;; Common Lisp that would show NIL, T and :FOO where Dylan wants (), #t
;;; and foo:, so this implementation brings a printer of its own.  It
;;; prints every object as the compiler to C does (runtime/dylan/
;;; 14-printing.dyl), so that a program prints the same in both.
;;;
;;; DYLAN-PRINT writes the predefined forms.  The Dylan-visible printing
;;; functions go through the generic function PRINT-OBJECT, whose method
;;; on <object> calls DYLAN-PRINT, and DYLAN-PRINT then prints the
;;; elements of a collection through PRINT-OBJECT again, so that a
;;; program's own methods are used inside lists too (runtime-printing.lisp).

(defun print-opaque-object (object stream)
  (format stream "#[~(~a~) ~x]"
          (type-of object)
          (logand (sxhash object) #xffffff)))

(defvar *dylan-print-escape* t
  "True for WRITE-style output, false for DISPLAY-style output.")

(defvar *dylan-print-target* nil
  "The Dylan stream DYLAN-PRINT is writing to on behalf of PRINT-OBJECT,
whose elements go through PRINT-OBJECT too; NIL when it is printing on its
own.")

(defvar *dylan-debug-print* nil
  "True while an error message is put together: objects then print as the
compiler's %FORMAT-MESSAGE prints them, without PRINT-OBJECT, and an
instance of a class other than a predefined collection or class shows only
its class.")

;;;; Streams
;;;
;;; A Dylan stream (chapter 13 passes them around; the stream functions
;;; themselves are runtime-printing.lisp's) is one of *STANDARD-OUTPUT* and
;;; *STANDARD-ERROR*, which write to whatever Lisp's standard output and
;;; error output are at the time, a string stream, which collects its
;;; characters, or a wrapper around a Lisp stream.

(defstruct (dylan-stream (:constructor %make-dylan-stream (target))
                         (:predicate dylan-stream?)
                         (:print-object print-opaque-object))
  target                                ; :STDOUT, :STDERR, :STRING or a stream
  (output nil)                          ; for :STRING, a string output stream
  (contents ""))                        ; for :STRING, the characters so far

(defun dylan-stream-lisp (stream)
  "The Lisp stream that writing to the Dylan STREAM writes to."
  (let ((target (dylan-stream-target stream)))
    (case target
      (:stdout *standard-output*)
      (:stderr *error-output*)
      (:string (or (dylan-stream-output stream)
                   (setf (dylan-stream-output stream)
                         (make-string-output-stream))))
      (t target))))

(defun dylan-stream-string (stream)
  "Everything written to the string stream STREAM so far."
  (let ((output (dylan-stream-output stream)))
    (when output
      (setf (dylan-stream-contents stream)
            (concatenate 'string (dylan-stream-contents stream)
                         (get-output-stream-string output))))
    (copy-seq (dylan-stream-contents stream))))

(defun dylan-function-name (name)
  "The Dylan name of a generic function the runtime knows as NAME: the
compiler's Lisp names for (SETTER FOO) -- dylan/setter/foo/, or setter/foo/
for the predefined ones -- and for a variable of module M -- foo@m -- are
shown as Dylan writes them."
  (if (not (symbolp name))
      name
      (let* ((string (dylan-symbol->string name))
             (setter (loop for prefix in '("dylan/setter/" "setter/")
                           when (and (> (length string) (1+ (length prefix)))
                                     (string-equal prefix string
                                                   :end2 (length prefix))
                                     (char= #\/ (char string
                                                      (1- (length string)))))
                             return (subseq string (length prefix)
                                            (1- (length string)))))
             (base (or setter string))
             (at (position #\@ base))
             (base (if (and at (plusp at)) (subseq base 0 at) base)))
        (if setter
            (format nil "(setter ~a)" base)
            base))))


(declaim (special <condition> <simple-error> <simple-warning> <type-error>
                  <range> <table> <sequence> <array>
                  dylan/print-object dylan/initial-state dylan/next-state
                  dylan/current-element dylan/current-key dylan/size))

(defun dylan-class-name (class)
  (dylan-symbol->string (class.debug-name class)))

(defun dylan-print (object &optional (stream *standard-output*))
  (labels ((out (x) (write-string x stream))
           (rec (x)
             ;; An element, through PRINT-OBJECT when printing for it
             (if *dylan-print-target*
                 (dylan-call dylan/print-object x *dylan-print-target*
                             (bool *dylan-print-escape*))
                 (dylan-print x stream)))
           (elements (collection)
             ;; The elements of a collection, separated by spaces
             (loop for state = (dylan-call dylan/initial-state collection)
                     then (dylan-call dylan/next-state collection state)
                   for first = t then nil
                   while (true? state)
                   do (unless first (write-char #\Space stream))
                      (rec (dylan-call dylan/current-element collection
                                       state)))))
    (cond
      ((null object) (out "()"))
      ((eq object t) (out "#t"))
      ((false? object) (out "#f"))
      ((keywordp object) (format stream "~a:" (dylan-symbol->string object)))
      ((symbolp object) (out (dylan-symbol->string object)))
      ((stringp object)
       (cond ((not *dylan-print-escape*) (out object))
             (*dylan-debug-print*
              (write-char #\" stream) (out object) (write-char #\" stream))
             (t
              (write-char #\" stream)
              (loop for ch across object
                    do (when (or (char= ch #\") (char= ch #\\))
                         (write-char #\\ stream))
                       (write-char ch stream))
              (write-char #\" stream))))
      ((characterp object)
       (cond ((not *dylan-print-escape*) (write-char object stream))
             (*dylan-debug-print*
              (if (<= 32 (char-code object) 126)
                  (format stream "#\\~c" object)
                  (format stream "#\\U+~4,'0X" (char-code object))))
             (t (out "#\\") (out (char-name-or-self object)))))
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
      ((or (floatp object) (extended-float? object))
       (out (dylan-float-string object)))
      ((complexp object)
       (out "#c(")
       (rec (realpart object))
       (write-char #\Space stream)
       (rec (imagpart object))
       (write-char #\) stream))
      ((numberp object) (princ object stream))
      ((class? object)
       (format stream "{the class ~a}" (dylan-class-name object)))
      ((dylan-stream? object) (out "{stream}"))
      ((instance? object)
       (let ((class (instance.class object)))
         (cond
           (*dylan-debug-print*
            (format stream "{instance of ~a}" (dylan-class-name class)))
           ;; Chapter 13: a condition prints as its message.
           ((subclass? class <condition>) (out (condition-message object)))
           ((subclass? class <range>)
            (out "{range ")
            (rec (range-start object))
            (out " by ")
            (rec (range-step object))
            (let ((size (range-size object)))
              (when size
                (out " size ")
                (rec size)))
            (write-char #\} stream))
           ((subclass? class <table>)
            (out "{table")
            (loop for state = (dylan-call dylan/initial-state object)
                    then (dylan-call dylan/next-state object state)
                  while (true? state)
                  do (write-char #\Space stream)
                     (rec (dylan-call dylan/current-key object state))
                     (out " => ")
                     (rec (dylan-call dylan/current-element object state)))
            (write-char #\} stream))
           ;; Any other sequence, or an array: its class and its elements
           ((or (subclass? class <sequence>) (subclass? class <array>))
            (format stream "{~a" (dylan-class-name class))
            (let ((size (dylan-call dylan/size object)))
              (cond ((false? size)
                     (out " ..."))
                    ((plusp size)
                     (write-char #\Space stream)
                     (elements object))))
            (write-char #\} stream))
           (t (format stream "{instance of ~a}" (dylan-class-name class))))))
      ((slot? object)
       (if *dylan-debug-print*
           (out "{slot}")
           (format stream "{slot ~a}"
                   (let ((getter (slot.getter object))
                         (n (slot.debug-name object)))
                     (cond ((and (functionp getter)
                                 (dylan%generic-function? getter))
                            (dylan-function-name
                             (generic-function.name getter)))
                           ((symbolp n) (dylan-function-name n))
                           (t n))))))
      ((singleton? object)
       (if *dylan-debug-print*
           (out "{instance of <singleton>}")
           (progn
             (out "{singleton ")
             (let ((*dylan-print-escape* t)) (rec (singleton.object object)))
             (write-char #\} stream))))
      ;; Generic functions and methods as the manual's examples show them
      ;; (chapters 3, 8, 10 and 11)
      ((and (functionp object) (dylan%generic-function? object))
       (let ((name (generic-function-print-name object)))
         (if name
             (format stream "{the generic function ~a}" name)
             (out "{an anonymous generic function}"))))
      ((functionp object)
       (let ((owner (method-owner object))
             (name (predefined-method-name object)))
         (cond (owner
                (format stream "{a method on ~a}"
                        (or (generic-function-print-name owner)
                            "an anonymous generic function")))
               (name (format stream "{method ~a}" name))
               (t (out "{an anonymous method}")))))
      (t (princ object stream)))
    object))

(defun generic-function-print-name (generic-function)
  "The name a generic function prints with, or NIL if it has none."
  (let ((name (generic-function.name generic-function)))
    (and name (not (false? name)) (dylan-function-name name))))

(defvar *predefined-method-names* nil
  "Each predefined method -> the name of the variable that holds it, made
when first needed.")

(defun predefined-method-name (method)
  "The name of the predefined variable whose value is METHOD, or NIL."
  (unless *predefined-method-names*
    (let ((table (make-hash-table :test 'eq)))
      (loop for (name variable) in (symbol-value 'dylan%lisp-names-of-predefined-names)
            do (when (boundp variable)
                 (let ((value (symbol-value variable)))
                   (when (and (functionp value)
                              (not (dylan%generic-function? value))
                              (not (gethash value table)))
                     (setf (gethash value table) (dylan-function-name name))))))
      (setf *predefined-method-names* table)))
  (gethash method *predefined-method-names*))

(defun condition-message (object)
  "The message of a condition, as the compiler's %CONDITION-MESSAGE puts
it together (chapter 13)."
  (let ((class (instance.class object)))
    (cond ((or (subclass? class <simple-error>)
               (subclass? class <simple-warning>))
           (dylan-format-message
            (dylan-call dylan/condition-format-string object)
            (dylan-call dylan/condition-format-arguments object)))
          ((subclass? class <type-error>)
           (dylan-format-message
            "~s is not an instance of ~s"
            (list (dylan-call dylan/type-error-value object)
                  (dylan-call dylan/type-error-expected-type object))))
          (t (dylan-format-message "~s" (list object))))))

(defun dylan-float-string (x)
  "A float as the compiler to C prints one: the fewest digits that read
back as X, in fixed notation when its magnitude is at least 1e-5 and below
1e16 (or it is zero), else with an exponent; a single-float ends with s
and its exponent (1.5s0), a double-float -- the default format, as when
reading -- with e and its exponent only when it has one (9.0, 1.0e20)."
  (cond
    ((extended-float? x)
     (let ((string (dylan-float-string (extended-float-value x))))
       ;; As a double prints, with l and its exponent (1.5l0)
       (let ((e (position #\e string)))
         (cond ((not (find #\. string)) string) ; +nan, +infinity
               (e (concatenate 'string (subseq string 0 e) "l" (subseq string (1+ e))))
               (t (concatenate 'string string "l0"))))))
    #+sbcl
    ((sb-ext:float-nan-p x) "+nan")
    #+sbcl
    ((sb-ext:float-infinity-p x) (if (plusp x) "+infinity" "-infinity"))
    (t
     (let* ((single (typep x 'single-float))
            (magnitude (abs (coerce x 'double-float)))
            (fixed (or (zerop x) (and (>= magnitude 1d-5) (< magnitude 1d16))))
            (exponent 0)
            (body
              (if (zerop x)
                  "0.0"
                  (multiple-value-bind (k digits)
                      #+sbcl (sb-impl::flonum-to-digits (abs x))
                      #-sbcl (let ((s (string-trim "0" (remove #\. (format nil "~,,,,,,'eE" (abs x))))))
                               (values 1 s))
                    (let ((n (length digits)))
                      (cond ((not fixed)
                             (setf exponent (1- k))
                             (format nil "~a.~a" (char digits 0)
                                     (if (> n 1) (subseq digits 1) "0")))
                            ((<= k 0)
                             (format nil "0.~a~a"
                                     (make-string (- k) :initial-element #\0)
                                     digits))
                            ((>= k n)
                             (format nil "~a~a.0" digits
                                     (make-string (- k n) :initial-element #\0)))
                            (t (format nil "~a.~a" (subseq digits 0 k)
                                       (subseq digits k)))))))))
       (format nil "~:[~;-~]~a~a" (minusp (float-sign x)) body
               (cond (single (format nil "s~d" exponent))
                     ((not fixed) (format nil "e~d" exponent))
                     (t "")))))))

(defun char-name-or-self (ch)
  "The name the compiler to C prints a character by, or the character."
  (case ch
    (#\Space "space")
    (#\Newline "newline")
    (#\Tab "tab")
    (#\Return "return")
    (#\Page "page")
    (#\Nul "null")
    (#\Rubout "rubout")
    (#\Backspace "backspace")
    (#\Esc "escape")
    (t (string ch))))

(defun dylan-print-object (object stream escape)
  "Print OBJECT on the Lisp STREAM through the generic function
PRINT-OBJECT, as the Dylan printing functions do."
  (if (and (boundp 'dylan/print-object) dylan/print-object)
      (dylan-call dylan/print-object object (%make-dylan-stream stream)
                  (bool escape))
      (let ((*dylan-print-escape* escape)) (dylan-print object stream)))
  object)

(defun dylan-write (object &optional (stream *standard-output*))
  (dylan-print-object object stream t))

(defun dylan-display (object &optional (stream *standard-output*))
  (dylan-print-object object stream nil))

(defun dylan-newline (&optional (stream *standard-output*))
  (terpri stream)
  dylan%false)

(defvar *dylan-pprint-dispatch*
  (let ((table (copy-pprint-dispatch nil)))
    ;; So that the pretty printer, which does the line breaking, still
    ;; renders Dylan's own notation for these four.
    (set-pprint-dispatch 'null
                         (lambda (s x) (declare (ignore x))
                           (write-string "()" s))
                         2 table)
    (set-pprint-dispatch 'dylan-false
                         (lambda (s x) (declare (ignore x))
                           (write-string "#f" s))
                         2 table)
    (set-pprint-dispatch '(eql t)
                         (lambda (s x) (declare (ignore x))
                           (write-string "#t" s))
                         2 table)
    (set-pprint-dispatch 'keyword
                         (lambda (s x)
                           (write-string (dylan-symbol->string x) s)
                           (write-char #\: s))
                         2 table)
    (set-pprint-dispatch 'symbol
                         (lambda (s x)
                           (write-string (dylan-symbol->string x) s))
                         1 table)
    ;; Every other object but a list or a vector -- a string, a float, an
    ;; instance -- as Dylan's printer writes it.
    (set-pprint-dispatch '(not (or cons (and vector (not string))))
                         (lambda (s x)
                           (let ((*print-pretty* nil)) (dylan-write x s)))
                         0 table)
    table)
  "A pretty-print dispatch table that lays out Dylan code the way Dylan
writes it.")

(defmacro with-dylan-pretty-printer ((table) &body body)
  `(let ((*print-case* :downcase)
         (*print-right-margin* 79)
         (*print-readably* nil)
         (*print-pprint-dispatch* ,table)
         (*print-pretty* t))
     ,@body))

(defun pp (object &optional (stream *standard-output*))
  "Pretty print, for the compiler's file output and for echoing input in
the test harness: on a new line, which it does not end."
  (with-dylan-pretty-printer (*dylan-pprint-dispatch*)
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

;;; Strings.  A <byte-string> is a simple Lisp string; a <unicode-string>,
;;; one that is not simple, so that GET-TYPE can tell them apart.  As in
;;; the compiler to C, a string constant with a character above code 255
;;; is a <unicode-string>.

(defun make-unicode-string (size &optional (fill #\Space))
  (make-array size :element-type 'character :initial-element fill
                   :adjustable t))

(defun unicode-string? (object)
  (and (stringp object) (not (simple-string-p object))))

(defun as-unicode-string (string)
  "A new <unicode-string> of the characters of STRING."
  (replace (make-unicode-string (length string)) string))

(defun read-dylan-string (stream)
  (let ((string (read-dylan-string-characters stream)))
    (if (some (lambda (ch) (> (char-code ch) 255)) string)
        (as-unicode-string string)
        string)))

(defun read-dylan-string-characters (stream)
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
                      ;; Chapter 3: a backslash quotes the next character.
                      (write-char next out)))
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
            (let ((name (subseq token 0 (1- n))))
              (remember-spelling
               (intern (canonicalize-string-for-symbol name) :keyword)
               name))
            (remember-spelling
             (dylan-user-symbol (canonicalize-string-for-symbol token))
             token)))))

(defun dylan-symbol (name)
  "The Dylan symbol named NAME (already canonicalized).  T and NIL are
ordinary symbols in Dylan, but in this package they would be Common Lisp's
T and NIL -- #t and #f -- so they get lower-case names of their own."
  (if (member name '("T" "NIL") :test #'string=)
      (intern (string-downcase name) :dylan)
      (intern name :dylan)))

;;; The compiler names its own variables !X, DYLAN/X, DYLAN%X and X@MODULE.
;;; So that a Dylan program may use any name, a symbol it writes that looks
;;; like one of those is a different symbol here, its name prefixed with
;;; %%; so is one already beginning with %%, which keeps the mapping one
;;; to one.  The symbol still prints as written (chapter 15).  The
;;; exceptions are !key, !rest and !next, which Thomas programs (the book's
;;; examples among them) write for #key, #rest and #next.

(defun dylan-user-symbol (name)
  "The Dylan symbol a program means by NAME (already canonicalized)."
  (dylan-symbol
   (if (or (and (> (length name) 0) (char= (char name 0) #\!)
                (not (member name '("!KEY" "!REST" "!NEXT") :test #'string=)))
           (and (>= (length name) 6)
                (or (string= "DYLAN/" name :end2 6)
                    (string= "DYLAN%" name :end2 6)))
           (and (>= (length name) 2) (string= "%%" name :end2 2))
           (find #\@ name))
       (concatenate 'string "%%" name)
       name)))

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
               ((string-equal name "f") dylan%false)
               ((member name '("key" "rest" "next") :test #'string-equal)
                (dylan-symbol (canonicalize-string-for-symbol
                               (concatenate 'string "!" name))))
               (t (error "Unknown # syntax #~a in Dylan input." name))))))))

(defvar *dylan-source-positions* nil
  "When an EQ hash table, the reader records in it the stream position at
which each list and vector it reads began.  The Dylan-to-C compiler uses
this to report errors by file and line.")

(defun read-dylan-object (stream)
  (if *dylan-source-positions*
      (let* ((ch (skip-dylan-whitespace stream))
             (start (and ch (file-position stream)))
             (object (read-dylan-object-1 stream)))
        (when (and start (or (consp object)
                             (and (vectorp object) (not (stringp object)))))
          (setf (gethash object *dylan-source-positions*) start))
        object)
      (read-dylan-object-1 stream)))

(defun read-dylan-object-1 (stream)
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
;;; <simple-error>; the manual's examples use Common Lisp's directives),
;;; put together as the compiler's %FORMAT-MESSAGE does: ~A and ~D print
;;; an argument plain, ~S escaped, ~% and ~& a newline and ~~ a tilde; any
;;; other directive is left as it is, a directive with no argument left
;;; prints nothing, and arguments no directive consumes follow the message,
;;; each after a space, as the runtime's own MIT-style error calls
;;; (message, then irritants) expect.

(defun dylan-format-message (control arguments)
  (let ((*dylan-debug-print* t)
        (*dylan-print-target* nil))
    ;; Printing plain affects only a string or a character itself, not
    ;; one inside a list, as %FORMAT-MESSAGE's DISPLAY does.
    (flet ((put (object escape out)
             (let ((*dylan-print-escape*
                     (or escape (not (or (stringp object) (characterp object))))))
               (dylan-print object out))))
      (with-output-to-string (out)
        (let ((args arguments))
          (if (not (stringp control))
              (put control nil out)
              (let ((n (length control))
                    (i 0))
                (loop while (< i n)
                      do (let ((ch (char control i)))
                           (if (and (char= ch #\~) (< (1+ i) n))
                               (let ((directive (char-downcase (char control (1+ i)))))
                                 (case directive
                                   ((#\a #\d) (when args (put (pop args) nil out)))
                                   ((#\s) (when args (put (pop args) t out)))
                                   ((#\% #\&) (terpri out))
                                   ((#\~) (write-char #\~ out))
                                   (t (write-char ch out)
                                    (write-char (char control (1+ i)) out)))
                                 (incf i 2))
                               (progn (write-char ch out) (incf i)))))))
          (dolist (arg args)
            (write-char #\Space out)
            (put arg nil out)))))))

(define-condition dylan-condition (error)
  ((offered :initarg :offered :initform nil :reader offered-to-dylan-p)
   (message :initarg :message :initform "" :reader dylan-condition-message)
   (irritants :initarg :irritants :initform '()
              :reader dylan-condition-irritants))
  (:report (lambda (condition stream)
             (write-string "Error: " stream)
             (write-string (dylan-format-message
                            (dylan-condition-message condition)
                            (dylan-condition-irritants condition))
                           stream))))

(define-condition unhandled-dylan-condition (error)
  ((offered :initarg :offered :initform t :reader offered-to-dylan-p)
   (dylan-condition :initarg :dylan-condition :reader unhandled-dylan-condition-object))
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

(declaim (special <restart>))

(defun call-with-dylan-restarts (thunk)
  "Call THUNK -- which enters the Lisp debugger -- with a Lisp restart for
each Dylan restart handler in effect.  Chapter 13: the debugger ought to
offer the user every restart for which a restart handler is applicable.
Choosing one signals an instance of the handler's restart class."
  (labels ((bind (frames)
             (if (null frames)
                 (funcall thunk)
                 (destructuring-bind (type function test description)
                     (car frames)
                   (declare (ignore function test))
                   (if (and (class? type) (subclass? type <restart>))
                       (restart-bind
                           ((dylan-restart
                              (lambda ()
                                (dylan-call dylan/signal
                                            (dylan-call dylan/make type)))
                              :report-function
                              (lambda (stream)
                                (if (stringp description)
                                    (write-string description stream)
                                    (format stream "Signal a Dylan ~a"
                                            (dylan-symbol->string
                                             (class.debug-name type)))))))
                         (bind (cdr frames)))
                       (bind (cdr frames)))))))
    (bind (reverse *dylan-handler-frames*))))

(defun implementation-specific/induce-error (format-string format-args)
  (call-with-dylan-restarts
   (lambda ()
     (error 'dylan-condition
            :offered t
            :message format-string
            :irritants format-args))))

(defun implementation-specific/induce-type-error (value class-name)
  (call-with-dylan-restarts
   (lambda ()
     (error 'dylan-condition
            :offered t
            :message "~s is not an instance of ~a"
            :irritants (list value class-name)))))

(defun implementation-specific/return-from-error (dylan-condition)
  "Chapter 13: if a handler returns from ERROR, error bombs directly to the
debugger -- without signalling again, which would only find the same
handlers."
  (invoke-debugger
   (make-condition 'dylan-condition
                   :message "a handler returned from ERROR, which may not return:"
                   :irritants (list dylan-condition))))

(defun implementation-specific/signal-unhandled-dylan-condition (dylan-condition)
  (call-with-dylan-restarts
   (lambda ()
     (error 'unhandled-dylan-condition :dylan-condition dylan-condition))))

(defun implementation-specific/warning (format-string format-args)
  (warn "~a" (dylan-format-message format-string format-args)))

(defun implementation-specific/catch-all-errors (handler thunk)
  ;; A handler that returns normally declines, and the condition goes on to
  ;; the next Lisp handler -- which is what
  ;; IMPLEMENTATION-SPECIFIC/LET-LISP-HANDLE-IT relies on.
  ;;
  ;; A Lisp error raised by a Dylan default handler has already been
  ;; offered to every Dylan handler, which all declined (chapter 13), so it
  ;; is not reflected back into Dylan a second time.
  (handler-bind ((error (lambda (condition)
                          (unless (and (typep condition
                                              '(or dylan-condition
                                                   unhandled-dylan-condition))
                                       (offered-to-dylan-p condition))
                            (funcall handler condition)))))
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
        (*package* (find-package '#:dylan))
        (*print-false-for-lisp* t))
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
