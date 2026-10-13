module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; Streams and printing.
;;;
;;; The manual uses streams -- print takes stream:, a handler's description
;;; is a function of a stream, and chapter 13's examples call format -- but
;;; defines none.  These are an extension (CompilerPlan.md, Phase 8):
;;;
;;;   <stream>, *standard-output*, *standard-error*
;;;   (make <string-stream>) and (stream-contents s), its characters so far
;;;   (format stream control argument ...): ~a, ~s and ~d write an argument
;;;     as display, write and display do; ~% and ~& a newline; ~~ a tilde.
;;;     A STREAM of #t means *standard-output*; of #f, answer a string.
;;;
;;; Printing follows the Common Lisp implementation, which follows Thomas:
;;;
;;;   (print object #key stream verbose)  as write does, then a newline
;;;   (write object #key stream)          strings in quotes, characters #\c
;;;   (display object #key stream)        strings and characters as they are
;;;   (newline #key stream)  (write-line object #key stream)  (pp object)
;;;
;;; How an object prints is the generic function print-object (object
;;; stream escape?), to which programs add methods.

;;; Streams

(define-class <stream> (<object>))

(define-class <%fd-stream> (<stream>)
  (%fd init-keyword: fd:))

(define-class <string-stream> (<stream>)
  (%chars init-function: (method () (make <stretchy-vector>))))

(define *standard-output* (make <%fd-stream> fd: 1))
(define *standard-error* (make <%fd-stream> fd: 2))

;;; Write a string or a character
(define-generic-function %put (stream x))
(define-method %put ((s <%fd-stream>) x) (%write-fd (%fd s) x))
(define-method %put ((s <string-stream>) (c <character>)) (add! (%chars s) c))
(define-method %put ((s <string-stream>) (x <string>))
  (for-each ((c x)) () (add! (%chars s) c)))

(define stream-contents
  (method (s)
    (bind ((chars (%chars s)))
      (concatenate-as (if (every? (method (c) (binary< (as <integer> c) 256)) chars)
                          <byte-string>
                          <unicode-string>)
                      chars))))

(define %stream
  (method (stream) (if (or (not stream) (id? stream #t)) *standard-output* stream)))

;;; print-object

(define-generic-function print-object (object stream escape?))

(define %print-elements
  (method (s stream escape?)
    (for ((state (initial-state s) (next-state s state))
          (first? #t #f))
         ((not state))
      (unless first? (%put stream #\space))
      (print-object (current-element s state) stream escape?))))

(define-method print-object ((x <object>) stream escape?)
  (%put stream "{instance of ")
  (print-object (%class-name (object-class x)) stream #f)
  (%put stream #\}))

(define-method print-object ((x <number>) stream escape?) (%put stream (%number->string x)))
(define-method print-object ((x <symbol>) stream escape?) (%put stream (%symbol-name x)))
(define-method print-object ((x <keyword>) stream escape?)
  (%put stream (%symbol-name x))
  (%put stream #\:))
(define-method print-object ((x <boolean>) stream escape?) (%put stream (if x "#t" "#f")))
(define-method print-object ((x <empty-list>) stream escape?) (%put stream "()"))

(define %character-names
  (list (cons #\space "space") (cons #\newline "newline") (cons (as <character> 9) "tab")
        (cons (as <character> 13) "return") (cons (as <character> 12) "page")
        (cons (as <character> 0) "null") (cons (as <character> 127) "rubout")
        (cons (as <character> 8) "backspace") (cons (as <character> 27) "escape")))

(define-method print-object ((x <character>) stream escape?)
  (if escape?
      (begin
        (%put stream "#\\")
        (bind ((name (find-pair x %character-names)))
          (%put stream (if name (cdr name) x))))
      (%put stream x)))

(define-method print-object ((x <string>) stream escape?)
  (if escape?
      (begin
        (%put stream #\")
        (for-each ((c x)) ()
          (when (or (id? c #\") (id? c #\\)) (%put stream #\\))
          (%put stream c))
        (%put stream #\"))
      (%put stream x)))

;;; A list prints in full: ''+ is (quote +) (chapter 8).
(define-method print-object ((x <pair>) stream escape?)
  (%put stream #\()
  (bind-methods ((loop (l first?)
                   (cond ((%pair? l)
                          (unless first? (%put stream #\space))
                          (print-object (car l) stream escape?)
                          (loop (cdr l) #f))
                         ((id? l '()) #f)
                         (else: (%put stream " . ") (print-object l stream escape?)))))
    (loop x #t))
  (%put stream #\)))

(define-method print-object ((x <simple-object-vector>) stream escape?)
  (%put stream "#(")
  (%print-elements x stream escape?)
  (%put stream #\)))

;;; Other sequences: their class and their elements
(define-method print-object ((x <sequence>) stream escape?)
  (%put stream #\{)
  (print-object (%class-name (object-class x)) stream #f)
  (unless (empty? x)
    (%put stream #\space)
    (if (size x)
        (%print-elements x stream escape?)
        (%put stream "...")))
  (%put stream #\}))

(define-method print-object ((x <range>) stream escape?)
  (%put stream "{range ")
  (print-object (%from x) stream escape?)
  (%put stream " by ")
  (print-object (%by x) stream escape?)
  (when (%size x)
    (%put stream " size ")
    (print-object (%size x) stream escape?))
  (%put stream #\}))

(define-method print-object ((x <table>) stream escape?)
  (%put stream "{table")
  (for ((state (initial-state x) (next-state x state)))
       ((not state))
    (%put stream #\space)
    (print-object (current-key x state) stream escape?)
    (%put stream " => ")
    (print-object (current-element x state) stream escape?))
  (%put stream #\}))

(define-method print-object ((x <class>) stream escape?)
  (%put stream "{the class ")
  (print-object (%class-name x) stream #f)
  (%put stream #\}))

(define-method print-object ((x <generic-function>) stream escape?)
  (%put stream "{the generic function ")
  (print-object (%generic-name x) stream #f)
  (%put stream #\}))

(define-method print-object ((x <method>) stream escape?) (%put stream "{method}"))

(define-method print-object ((x <singleton>) stream escape?)
  (%put stream "{singleton ")
  (print-object (%singleton-object x) stream #t)
  (%put stream #\}))

(define-method print-object ((x <thread>) stream escape?) (%put stream "{thread}"))

(define-method print-object ((x <slot-descriptor>) stream escape?)
  (%put stream "{slot ")
  (print-object (%generic-name (slot-getter x)) stream #f)
  (%put stream #\}))

(define-method print-object ((x <stream>) stream escape?) (%put stream "{stream}"))

;;; A condition prints as its message (chapter 13).
(define-method print-object ((c <condition>) stream escape?)
  (%put stream (%condition-message c)))

;;; The functions

(define write
  (method (object #key stream)
    (print-object object (%stream stream) #t)
    object))

(define display
  (method (object #key stream)
    (print-object object (%stream stream) #f)
    object))

(define newline
  (method (#key stream)
    (%put (%stream stream) #\newline)
    #f))

(define-generic-function print (object #key stream verbose))
(define-method print (object #key stream verbose)
  (bind ((s (%stream stream)))
    (print-object object s #t)
    (%put s #\newline)
    object))

(define write-line
  (method (object #key stream)
    (bind ((s (%stream stream)))
      (print-object object s #t)
      (%put s #\newline)
      object)))

(define pp
  (method (object #key stream)
    (bind ((s (%stream stream)))
      (print-object object s #t)
      (%put s #\newline)
      object)))

;;; format

(define format
  (method (stream control #rest arguments)
    (bind ((s (cond ((not stream) (make <string-stream>))
                    ((id? stream #t) *standard-output*)
                    (else: stream)))
           (n (size control)))
      (bind-methods ((next-argument ()
                       (if (id? arguments '())
                           (error "format: too few arguments for ~s" control)
                           (bind ((a (car arguments)))
                             (set! arguments (cdr arguments))
                             a)))
                     (scan (i)
                       (when (binary< i n)
                         (bind ((c (element control i)))
                           (if (and (id? c #\~) (binary< (binary+ i 1) n))
                               (begin
                                 (bind ((d (as-lowercase (element control (binary+ i 1)))))
                                   (cond ((or (id? d #\a) (id? d #\d))
                                          (print-object (next-argument) s #f))
                                         ((id? d #\s) (print-object (next-argument) s #t))
                                         ((or (id? d #\%) (id? d #\&)) (%put s #\newline))
                                         ((id? d #\~) (%put s #\~))
                                         (else: (%put s c) (%put s d))))
                                 (scan (binary+ i 2)))
                               (begin (%put s c) (scan (binary+ i 1))))))))
        (scan 0))
      (if stream #f (stream-contents s)))))
