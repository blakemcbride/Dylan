;;;; Copyright 2026 Blake McBride.  See ../LICENSE.
;;;;
;;;; Streams and printing, as the compiler to C has them
;;;; (runtime/dylan/14-printing.dyl).
;;;;
;;;; The manual uses streams -- print takes stream:, a handler's description
;;;; is a function of a stream, and chapter 13's examples call format -- but
;;;; defines none, and it says (chapter 1) that print and format are used in
;;;; its examples but are not part of Dylan.  These are an extension:
;;;;
;;;;   <stream>, *standard-output*, *standard-error*
;;;;   (make <string-stream>) and (stream-contents s), its characters so far
;;;;   (format stream control argument ...): ~a, ~s and ~d write an argument
;;;;     as display, write and display do; ~% and ~& a newline; ~~ a tilde.
;;;;     A STREAM of #t means *standard-output*; of #f, answer a string.
;;;;   (print object #key stream verbose)  as write does, then a newline
;;;;   (write object #key stream)          strings in quotes, characters #\c
;;;;   (display object #key stream)        strings and characters as they are
;;;;   (newline #key stream)  (write-line object #key stream)
;;;;   (pp object #key stream)             as print does, but laid out over
;;;;                                       lines as code; see PRETTY-PRINT
;;;;
;;;; How an object prints is the generic function print-object (object
;;;; stream escape?), to which programs add methods; its method on <object>
;;;; is DYLAN-PRINT (compat.lisp).

(in-package #:dylan)

(defparameter dylan/*standard-output* (%make-dylan-stream :stdout))
(defparameter dylan/*standard-error* (%make-dylan-stream :stderr))

(defun dylan-stream-argument (stream)
  "The stream a printing function writes to for its STREAM argument: #f
(the default) and #t mean *standard-output*."
  (cond ((or (false? stream) (eq stream t)) dylan/*standard-output*)
        ((dylan-stream? stream) stream)
        (t (dylan-call dylan/error "not a stream" stream))))

(defun stream-keyword (keys)
  (dylan-stream-argument (dylan%find-keyword keys :stream
                                             (lambda () dylan%false))))

(add-method
 dylan/make
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <string-stream>))) nil nil :any)
  (lambda (class &rest rest)
    (declare (ignore class rest))
    (%make-dylan-stream :string))))

(defparameter dylan/stream-contents
  (dylan%function->method
   (make-param-list `((stream ,<string-stream>)) nil nil nil)
   #'dylan-stream-string))

;;; print-object

(defparameter dylan/print-object
  (dylan%generic-fn 'print-object
                    (make-param-list `((object ,<object>) (stream ,<object>)
                                       (escape? ,<object>))
                                     nil nil nil)
                    (lambda (object stream escape?)
                      (let ((*dylan-print-escape* (true? escape?))
                            (*dylan-print-target* stream))
                        (dylan-print object (dylan-stream-lisp stream))))))

(defun print-to (object stream escape?)
  "Print OBJECT on the Dylan STREAM through PRINT-OBJECT."
  (dylan-call dylan/print-object object stream (bool escape?)))

;;; Pretty-printing, for pp.  The compiler to C lays an object out the
;;; same way (runtime/dylan/14-printing.dyl, %pretty-print): an object
;;; whose printed form fits on what is left of a line of 79 columns is
;;; printed as print prints it; otherwise a proper list headed by a symbol
;;; has after its head as many of its elements as fit on the line (the
;;; first, laid out, if none does), and then each further element on a
;;; line of its own, indented two columns past the parenthesis; any other proper list
;;; has each element on a line of its own, one column past the
;;; parenthesis; a vector has them two columns past the #; and anything
;;; else is printed as print prints it.

(defconstant +pretty-print-width+ 79)

(defun printed-form (object)
  "OBJECT as PRINT writes it, without the newline."
  (let ((stream (%make-dylan-stream :string)))
    (print-to object stream t)
    (dylan-stream-string stream)))

(defun pretty-print (object stream column)
  "Write OBJECT laid out on the Dylan STREAM, starting at COLUMN."
  (let ((flat (printed-form object))
        (out (dylan-stream-lisp stream)))
    (flet ((lines (elements indent)
             ;; Each of ELEMENTS on a new line at INDENT
             (dolist (element elements)
               (terpri out)
               (write-string (make-string indent :initial-element #\Space) out)
               (pretty-print element stream indent))))
      (cond
        ((<= (+ column (length flat)) +pretty-print-width+)
         (write-string flat out))
        ((and (consp object) (null (cdr (cl:last object))))
         (let ((head (car object)))
           (write-char #\( out)
           (if (and (symbolp head) head (not (eq head t)) (not (keywordp head)))
               (let* ((name (printed-form head))
                      (at (+ column 1 (length name)))
                      (rest (cdr object)))
                 (write-string name out)
                 ;; The elements that fit on the head's line
                 (loop while rest
                       do (let ((next (printed-form (car rest))))
                            (if (<= (+ at 1 (length next)) +pretty-print-width+)
                                (progn (write-char #\Space out)
                                       (write-string next out)
                                       (incf at (+ 1 (length next)))
                                       (pop rest))
                                (return))))
                 (when (and rest (eq rest (cdr object)))
                   (write-char #\Space out)
                   (pretty-print (car rest) stream (+ at 1))
                   (pop rest))
                 (lines rest (+ column 2)))
               (progn
                 (pretty-print head stream (+ column 1))
                 (lines (cdr object) (+ column 1))))
           (write-char #\) out)))
        ((and (simple-vector-p object) (plusp (length object)))
         (write-string "#(" out)
         (pretty-print (aref object 0) stream (+ column 2))
         (lines (cdr (coerce object 'list)) (+ column 2))
         (write-char #\) out))
        (t (write-string flat out))))))

;;; The functions

(defun print-line (object keys)
  (let ((stream (stream-keyword keys)))
    (print-to object stream t)
    (terpri (dylan-stream-lisp stream))
    object))

(defparameter dylan/print
  (dylan%generic-fn 'print
                    (make-param-list `((object ,<object>)) nil nil
                                     '(:stream :verbose))
                    (lambda (object &rest keys) (print-line object keys))))

(flet ((printer (keys function)
         (dylan%function->method
          (make-param-list `((object ,<object>)) nil nil keys)
          function)))
  (defparameter dylan/write
    (printer '(:stream)
             (lambda (object &rest keys)
               (print-to object (stream-keyword keys) t)
               object)))
  (defparameter dylan/display
    (printer '(:stream)
             (lambda (object &rest keys)
               (print-to object (stream-keyword keys) nil)
               object)))
  (defparameter dylan/write-line
    (printer '(:stream) (lambda (object &rest keys) (print-line object keys))))
  (defparameter dylan/pp
    (printer '(:stream)
             (lambda (object &rest keys)
               (let ((stream (stream-keyword keys)))
                 (pretty-print object stream 0)
                 (terpri (dylan-stream-lisp stream))
                 object)))))

(defparameter dylan/newline
  (dylan%function->method
   (make-param-list '() nil nil '(:stream))
   (lambda (&rest keys)
     (terpri (dylan-stream-lisp (stream-keyword keys)))
     dylan%false)))

;;; format

(defparameter dylan/format
  (dylan%function->method
   (make-param-list `((stream ,<object>) (control ,<object>)) nil t nil)
   (lambda (stream control &rest arguments)
     (let* ((target (cond ((false? stream) (%make-dylan-stream :string))
                          ((eq stream t) dylan/*standard-output*)
                          (t (dylan-stream-argument stream))))
            (out (dylan-stream-lisp target))
            (n (if (stringp control)
                   (length control)
                   (dylan-call dylan/error "format: not a string" control))))
       (flet ((next-argument ()
                (if (null arguments)
                    (dylan-call dylan/error "format: too few arguments for ~s"
                                control)
                    (pop arguments))))
         (loop with i = 0
               while (< i n)
               do (let ((c (char control i)))
                    (if (and (char= c #\~) (< (1+ i) n))
                        (let ((d (char-downcase (char control (1+ i)))))
                          (case d
                            ((#\a #\d) (print-to (next-argument) target nil))
                            ((#\s) (print-to (next-argument) target t))
                            ((#\% #\&) (terpri out))
                            ((#\~) (write-char #\~ out))
                            (t (write-char c out)
                             (write-char (char control (1+ i)) out)))
                          (incf i 2))
                        (progn (write-char c out) (incf i))))))
       (if (false? stream) (dylan-stream-string target) dylan%false)))))
