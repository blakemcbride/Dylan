;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Source files, source locations, and compile-time diagnostics.
;;;;
;;;; A file is read whole into a string and parsed with the Dylan reader
;;;; from src/compat.lisp, which records in an EQ table the position at
;;;; which each list and vector began.  While the front end works on a
;;;; form it binds *LOCATION* to the position of the innermost enclosing
;;;; form that has one, so a diagnostic can name the file, line and column
;;;; -- even for forms a macro made, which inherit the location of the
;;;; form they were expanded from.

(in-package #:dylan-compiler)

(defstruct (source-file (:constructor %make-source-file (name text line-starts)))
  name                                  ; as given, for messages
  text                                  ; the whole file
  line-starts                           ; index at which each line begins
  (positions (make-hash-table :test #'eq))) ; form -> index in TEXT

(defun make-source-file (name text)
  (let ((starts (make-array 16 :adjustable t :fill-pointer 0)))
    (vector-push-extend 0 starts)
    (loop for i from 0 below (length text)
          when (char= (char text i) #\Newline)
            do (vector-push-extend (1+ i) starts))
    (%make-source-file name text starts)))

(defstruct (location (:constructor make-location (file index)))
  file                                  ; a SOURCE-FILE
  index)                                ; character index in its text

(defun location-line-and-column (location)
  "The 1-based line and column of LOCATION."
  (let* ((starts (source-file-line-starts (location-file location)))
         (index (location-index location))
         (lo 0)
         (hi (1- (length starts))))
    ;; The last line start not after INDEX.
    (loop while (< lo hi)
          do (let ((mid (ceiling (+ lo hi) 2)))
               (if (<= (aref starts mid) index)
                   (setf lo mid)
                   (setf hi (1- mid)))))
    (values (1+ lo) (1+ (- index (aref starts lo))))))

(defun location-string (location)
  (if (null location)
      "<unknown location>"
      (multiple-value-bind (line column) (location-line-and-column location)
        (format nil "~a:~d:~d"
                (source-file-name (location-file location)) line column))))

(defvar *source-file* nil
  "The SOURCE-FILE whose forms are being processed, or NIL.")

(defvar *location* nil
  "The location of the innermost form being processed that has one.")

(defun form-location (form)
  "Where FORM was read, or NIL if it was not read from *SOURCE-FILE*."
  (when (and *source-file*
             (or (consp form) (and (vectorp form) (not (stringp form)))))
    (let ((index (gethash form (source-file-positions *source-file*))))
      (and index (make-location *source-file* index)))))

(defmacro with-form ((form) &body body)
  "Evaluate BODY with *LOCATION* at FORM, if FORM's location is known."
  `(let ((*location* (or (form-location ,form) *location*)))
     ,@body))

;;; Printing Dylan objects in messages

(defun dylan-string (object)
  "OBJECT as Dylan would write it."
  (if (and (symbolp object) (null (symbol-package object)))
      (symbol-name object)              ; a name the compiler made
      (with-output-to-string (out)
        (let ((dylan::*print-false-for-lisp* nil))
          (dylan::dylan-write object out)))))

(defun message-string (control arguments)
  "CONTROL formatted with ARGUMENTS, each of which except a string is
written as Dylan would write it."
  (apply #'format nil control
         (mapcar (lambda (x) (if (stringp x) x (dylan-string x))) arguments)))

;;; Diagnostics.  An error abandons the top-level form being processed --
;;; the front end then goes on to the next, so that one run reports as
;;; many errors as it can.  A warning is only recorded.

(define-condition compile-error (error)
  ((location :initarg :location :reader compile-error-location)
   (message :initarg :message :reader compile-error-message))
  (:report (lambda (condition stream)
             (format stream "~a: error: ~a"
                     (location-string (compile-error-location condition))
                     (compile-error-message condition)))))

(defun compile-error (control &rest arguments)
  "Signal a compile-time error at *LOCATION*.  Each argument except a
string is shown as Dylan would write it."
  (error 'compile-error :location *location*
                        :message (message-string control arguments)))

(defstruct (diagnostic (:constructor make-diagnostic (kind location message)))
  kind                                  ; :error or :warning
  location
  message)

(defun diagnostic-string (diagnostic)
  (format nil "~a: ~(~a~): ~a"
          (location-string (diagnostic-location diagnostic))
          (diagnostic-kind diagnostic)
          (diagnostic-message diagnostic)))

(defmethod print-object ((d diagnostic) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (d stream)
        (write-string (diagnostic-string d) stream))))

(defvar *diagnostics* nil
  "Where the diagnostics of the current run are collected, newest first.")

(defun compile-warning (control &rest arguments)
  (push (make-diagnostic :warning *location*
                         (message-string control arguments))
        *diagnostics*))

(defun note-error (condition)
  (push (make-diagnostic :error (compile-error-location condition)
                         (compile-error-message condition))
        *diagnostics*))

;;; Reading a file

(defun header-keyword-line-p (line)
  (let ((colon (position #\: line)))
    (and colon (> colon 0)
         (alpha-char-p (char line 0))
         (every (lambda (c) (or (alphanumericp c) (char= c #\-)))
                (subseq line 0 colon)))))

(defun parse-header (text)
  "Design note 33: a file may begin with lines of the form \"keyword:
value\", ended by a blank line.  Answer the value of the module: keyword (or
NIL) and the index at which the forms begin."
  (flet ((line-at (start)
           (let ((end (or (position #\Newline text :start start) (length text))))
             (values (subseq text start end) (min (length text) (1+ end))))))
    (if (not (and (plusp (length text)) (alpha-char-p (char text 0))
                  (header-keyword-line-p (line-at 0))))
        (values nil 0)
        (let ((module nil) (start 0))
          (loop
            (when (>= start (length text)) (return))
            (multiple-value-bind (line next) (line-at start)
              (setf start next)
              (when (string= (string-trim '(#\Space #\Tab #\Return) line) "")
                (return))
              (when (header-keyword-line-p line)
                (let ((colon (position #\: line)))
                  (when (string-equal (subseq line 0 colon) "module")
                    (setf module (string-trim '(#\Space #\Tab #\Return)
                                              (subseq line (1+ colon)))))))))
          (values module start)))))

(defun read-file-text (pathname)
  (with-open-file (in pathname :external-format :utf-8)
    (let* ((text (make-string (file-length in)))
           (end (read-sequence text in)))
      (subseq text 0 end))))

(defun read-source (name text)
  "Read the forms of TEXT, which came from the file NAME.  Answer the
SOURCE-FILE, the module the header names (a string, or NIL), and a list
of (form . location), one for each top-level form.  A reader error is reported as a compile error at the point the
reader had reached."
  (let ((file (make-source-file name text)))
    (multiple-value-bind (module start) (parse-header text)
      ;; Positioned after the header, so that positions count from the
      ;; start of the text
      (let ((stream (let ((s (make-string-input-stream text))) (file-position s start) s))
            (eof (list nil))
            (forms '()))
        (let ((dylan::*dylan-source-positions* (source-file-positions file)))
          (loop
            (let* ((start (progn (dylan::skip-dylan-whitespace stream)
                                 (file-position stream)))
                   (form
                    (handler-case (dylan::dylan-read stream nil eof)
                      (error (e)
                        (error 'compile-error
                               :location (make-location
                                          file
                                          (min (file-position stream)
                                               (max 0 (1- (length text)))))
                               :message (format nil "~a" e))))))
              (when (eq form eof) (return))
              (push (cons form (make-location file start)) forms))))
        (values file module (nreverse forms))))))
