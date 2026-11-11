;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Run test programs in the interactive Common Lisp implementation:
;;;;
;;;;     sbcl --script tests/interpreter/run.lisp [file.lisp ...] [test ...]
;;;;
;;;; The files are in the form of tests/codegen/runner.lisp -- each case
;;;; (name program expected &key error compile-error compiler-only) -- so
;;;; that the programs the compiler to C is tested on test the interactive
;;;; implementation too, which must agree with it.  By default all of the
;;;; compiler's case files, tests/codegen/cases*.lisp, and this directory's
;;;; cases.lisp, of programs the compiler to C does not support yet.
;;;;
;;;; Each program runs in the module dylan-user, as a compiled program
;;;; does, or in the module its module: header names, and what it defined
;;;; there is forgotten before the next; its output is captured and
;;;; compared with the expected output.  A program
;;;; the compiler would reject, or that would fail when run, must signal an
;;;; error here, whose message contains the expected text; whether it comes
;;;; from compiling or running makes no difference in this implementation,
;;;; which does both a form at a time, and the "Dylan error: " with which a
;;;; compiled program reports an unhandled error is not expected of it.  A
;;;; case marked :compiler-only t tests something the interactive
;;;; implementation does not do by design -- 63-bit integers, a static
;;;; program's restrictions, the compiler's own diagnostics, threads -- and
;;;; is skipped.

;;; The control stack and heap of bin/dylan (src/build-dylan.lisp): 1 GB
;;; and 8 GB at least, so that the programs run as they do there.
(let ((stack (* 1024 1024 1024))
      (heap (max (sb-ext:dynamic-space-size) (* 8 1024 1024 1024))))
  (when (or (< (extern-alien "thread_control_stack_size" unsigned-long) stack)
            (< (sb-ext:dynamic-space-size) heap))
    (sb-ext:exit
     :code (sb-ext:process-exit-code
            (sb-ext:run-program
             sb-ext:*runtime-pathname*
             (append (list "--control-stack-size" (format nil "~dMB" (floor stack (* 1024 1024)))
                           "--dynamic-space-size" (format nil "~dMB" (floor heap (* 1024 1024)))
                           "--script" (sb-ext:native-namestring *load-truename*))
                     (rest sb-ext:*posix-argv*))
             :input t :output t :error t))
     :abort t)))

(load (merge-pathnames "../../src/load-dylan.lisp"
                       (directory-namestring *load-truename*)))

(in-package #:dylan)

(defvar *program-variables* '()
  "The module variables the program being run has made so far.")

(defun header-keyword-line-p (line)
  (let ((colon (position #\: line)))
    (and colon (> colon 0)
         (alpha-char-p (char line 0))
         (every (lambda (c) (or (alphanumericp c) (char= c #\-)))
                (subseq line 0 colon)))))

(defun program-header (text)
  "Design note 33, as the compiler to C takes it: a program may begin with
lines \"keyword: value\", ended by a blank line.  Answer the module: value
as the name of a module, or NIL, and the text of the forms after them."
  (if (not (and (plusp (length text)) (alpha-char-p (char text 0))
                (header-keyword-line-p
                 (subseq text 0 (or (position #\Newline text) (length text))))))
      (values nil text)
      (with-input-from-string (in text)
        (let ((module nil))
          (loop for line = (read-line in nil nil)
                until (or (null line)
                          (string= (string-trim '(#\Space #\Tab #\Return) line) ""))
                do (let ((colon (position #\: line)))
                     (when (and (header-keyword-line-p line)
                                (string-equal (subseq line 0 colon) "module"))
                       (setf module (dylan-read
                                     (make-string-input-stream
                                      (subseq line (1+ colon))))))))
          (values module
                  (with-output-to-string (out)
                    (loop for line = (read-line in nil nil)
                          while line do (write-line line out))))))))

(defun evaluate-program (text)
  "Read, compile and evaluate the forms of the Dylan program TEXT one by
one, as the read-eval-print loop does: each is read after the one before
it has run, which matters to a symbol's spelling (chapter 15: as may make
a symbol before a later form spells it).  A module: header puts the forms
in that module, as it does a compiled program's; the modules the
program's define-module forms declare are defined first, as bin/dylan
defines a file's (PREDEFINE-FILE-MODULES)."
  (multiple-value-bind (module text) (program-header text)
   (with-input-from-string (in text)
     (dylan%predefine-modules (read-module-forms in)))
   (when module
     (in-module module))
   (with-input-from-string (in text)
    (let ((eof (list 'eof))
          (module-variables '()))
      (loop for form = (dylan-read in nil eof)
            until (eq form eof)
            do (compile-expression
                form nil module-variables
                (lambda (new-variables preamble compiled)
                  (implementation-specific/eval (append preamble (list compiled)))
                  (setf module-variables (append new-variables module-variables))
                  (setf *program-variables* (append new-variables *program-variables*)))))))))

;;; The library's generic functions, each with its methods, and the fast
;;; paths' guards, each with its value, as the library left them: a
;;; program's methods for the library's generic functions are forgotten
;;; too.
(defvar *library-methods*
  (loop for (nil variable) in dylan%lisp-names-of-predefined-names
        when (and (boundp variable) (dylan%generic-function? (symbol-value variable)))
          collect (let ((generic-function (symbol-value variable)))
                    (cons generic-function
                          (copy-list (generic-function-data.methods
                                      (generic-function-data generic-function)))))))

(defvar *library-class-slots*
  (mapcar (lambda (class)
            (list class (class.slots class) (copy-seq (class.class-data class))
                  (class.instance-data-size class)))
          (cons <object> (class-descendants <object>)))
  "Each class of the library, with its slots as they are before any
program: a define-slot on it (chapter 6) changes them.")

(defvar *fast-path-guards*
  (let ((guards '()))
    (maphash (lambda (variable symbols)
               (declare (ignore variable))
               (dolist (guard symbols)
                 (pushnew guard guards)))
             *fast-path-variables*)
    (mapcar (lambda (guard) (cons guard (symbol-value guard))) guards)))

(defvar *dylan-user-module* (gethash 'dylan-user *dylan-modules*)
  "The module dylan-user as the implementation defines it, which a program
may define again.")

(defvar *system-spellings*
  (let ((copy (make-hash-table :test #'eq)))
    (maphash (lambda (symbol spelling) (setf (gethash symbol copy) spelling))
             *symbol-spellings*)
    copy)
  "The spellings of the implementation's own names (chapter 15).")

(defun forget-program ()
  "Undo the program's definitions in dylan-user, for the next program:
its module variables lose their values, and are macros and read-only no
longer, and the library's generic functions lose the methods it added."
  (loop for (generic-function . methods) in *library-methods*
        do (let ((data (generic-function-data generic-function)))
             (unless (equal methods (generic-function-data.methods data))
               (setf (generic-function-data.methods data) (copy-list methods)
                     (generic-function-data.cache data) nil))))
  (loop for (guard . value) in *fast-path-guards*
        do (setf (symbol-value guard) value))
  ;; The library's classes lose the slots the program gave them, and
  ;; objects with no storage of their own their values of them
  (loop for (class slots class-data size) in *library-class-slots*
        do (unless (eq (class.slots class) slots)
             (setf (class.slots class) slots
                   (class.class-data class) (copy-seq class-data)
                   (class.instance-data-size class) size
                   (class.required-init-keywords class) nil)))
  (clrhash *native-slot-values*)
  (clrhash *extended-float-slot-values*)
  (dolist (name *program-variables*)
    (let ((variable (name->module-variable name)))
      (makunbound variable)
      (remhash variable *read-only-variables*)))
  ;; The library's variables it assigned
  (reset-program-variables)
  (clrhash *dylan-macros*)              ; a macro's own name is not among them
  ;; Chapter 15: how a symbol was spelled, but for the implementation's
  ;; own names
  (clrhash *symbol-spellings*)
  (maphash (lambda (symbol spelling) (setf (gethash symbol *symbol-spellings*) spelling))
           *system-spellings*)
  (setf (gethash 'dylan-user *dylan-modules*) *dylan-user-module*)
  (setf *program-variables* '())
  ;; A class knows its instances while they live, and a define-slot on a
  ;; class initializes each (chapter 6): the program's objects are
  ;; collected now, so that the next program finds none of them, as when
  ;; it runs alone, whenever the collector happened to run before.
  (sb-ext:gc :full t))

;;; Not CONDITION-MESSAGE, which would replace the runtime's own function of
;;; that name, the message of a Dylan condition, and print conditions as
;;; #[dylan-instance ...].
(defun lisp-condition-message (condition)
  "The message of the Lisp CONDITION on one line, without the Error: prefix:
for the host's errors that are Dylan's too, as bin/dylan reports them."
  (let ((text (string-trim '(#\Space #\Newline #\Tab)
                           (if (typep condition
                                      '(or program-error arithmetic-error type-error))
                               (batch-error-message condition)
                               (format nil "~a" condition)))))
    (if (and (> (length text) 7) (string= "Error: " text :end2 7))
        (subseq text 7)
        text)))

(defun run-program (text)
  "Run the program TEXT in dylan-user; answer its output and the message
of the error that ended it, or NIL.  Lisp's own errors are offered to the
program's handlers, as at the read-eval-print loop."
  (let ((out (make-string-output-stream))
        (message nil))
    (in-module 'dylan-user)
    ;; Some errors go straight to the debugger, as when a handler returns
    ;; from error (chapter 13); here that ends the program, as an unhandled
    ;; error does, instead of the run.  break reports and carries on, as
    ;; when bin/dylan reads a file (*BATCH*).
    (catch 'debugger
      (let ((*batch* t)
            (*error-output* (make-broadcast-stream))
            (sb-ext:*invoke-debugger-hook*
              (lambda (condition hook)
                (declare (ignore hook))
                (setf message (lisp-condition-message condition))
                (throw 'debugger nil))))
        (handler-case
            (let ((*standard-output* out))
              (dylan%catch-all-conditions
               (lambda () (evaluate-program text))))
          (error (e) (setf message (lisp-condition-message e))))))
    (forget-program)
    (values (get-output-stream-string out) message)))

(defun expected-message (text)
  "TEXT less the prefix a compiled program's report of an unhandled error
has, which this implementation's has not."
  (let ((prefix "Dylan error: "))
    (if (and (>= (length text) (length prefix))
             (string= prefix text :end2 (length prefix)))
        (subseq text (length prefix))
        text)))

(defun run-case (case)
  "The failures of CASE, a list of strings, or :SKIPPED."
  (destructuring-bind (name text expected &key error compile-error compiler-only) case
    (if compiler-only
        :skipped
        (let ((wanted (and (or error compile-error)
                           (expected-message (or error compile-error)))))
          (multiple-value-bind (output message) (run-program text)
            (append
             (unless (string= output expected)
               (list (format nil "~(~a~): output~%~a~%expected~%~a" name output expected)))
             (cond ((and wanted (null message))
                    (list (format nil "~(~a~): expected the error ~s, got none" name wanted)))
                   ((and wanted (not (search wanted message)))
                    (list (format nil "~(~a~): expected the error ~s, got ~s" name wanted message)))
                   ((and message (not wanted))
                    (list (format nil "~(~a~): error: ~a" name message))))))))))

(defun read-cases (file)
  (with-open-file (in file)
    (let ((*package* (find-package '#:dylan)))
      (read in))))

(defun run-test-files (default-files arguments)
  (let* ((files (or (remove-if-not (lambda (a) (search ".lisp" a)) arguments)
                    default-files))
         (selected (remove-if (lambda (a) (search ".lisp" a)) arguments))
         (cases (loop for file in files
                      append (remove-if-not
                              (lambda (c)
                                (or (null selected)
                                    (member (string-downcase (symbol-name (first c)))
                                            selected :test #'string=)))
                              (read-cases file))))
         (skipped 0)
         (failures (loop for case in cases
                         for result = (run-case case)
                         if (eq result :skipped) do (incf skipped)
                           else append result)))
    (dolist (f failures) (format t "FAIL ~a~%" f))
    (format t "~d program~:p, ~d failure~:p~[~:;, ~:*~d compiler-only skipped~]~%"
            (- (length cases) skipped) (length failures) skipped)
    (finish-output)
    (sb-ext:exit :code (if failures 1 0) :abort t)))

(let ((here (directory-namestring *load-truename*)))
  (run-test-files (append (directory (merge-pathnames "../codegen/cases*.lisp" here))
                          (list (merge-pathnames "cases.lisp" here)))
                  (rest sb-ext:*posix-argv*)))
