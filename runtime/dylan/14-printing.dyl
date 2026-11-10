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
;;; The Common Lisp implementation prints the same way (src/compat.lisp,
;;; src/runtime-printing.lisp):
;;;
;;;   (print object #key stream verbose)  as write does, then a newline
;;;   (write object #key stream)          strings in quotes, characters #\c
;;;   (display object #key stream)        strings and characters as they are
;;;   (newline #key stream)  (write-line object #key stream)
;;;   (pp object #key stream)             as print, laid out over lines
;;;
;;; How an object prints is the generic function print-object (object
;;; stream escape?), to which programs add methods.

;;; Streams

(define-class <stream> (<object>))
(define-method make ((class (singleton <stream>)) #rest arguments #key) (%abstract class))

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
    (unless (id? (%object-class s) <string-stream>)
      (error "stream-contents: not a string stream: ~s" s))
    (bind ((chars (%chars s)))
      (concatenate-as (if (every? (method (c) (binary< (as <integer> c) 256)) chars)
                          <byte-string>
                          <unicode-string>)
                      chars))))

(define %stream
  (method (stream)
    (cond ((or (not stream) (id? stream #t)) *standard-output*)
          ;; Only the library's streams: a program's subclass of
          ;; <stream> has no way to take characters.
          ((or (id? (%object-class stream) <%fd-stream>)
               (id? (%object-class stream) <string-stream>))
           stream)
          (else: (error "not a stream: ~s" stream)))))

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
  (print-object (%class-name (%object-class x)) stream #f)
  (%put stream #\}))

(define-method print-object ((x <number>) stream escape?) (%put stream (%number->string x)))
;;; An instance of a program's subclass of one of the classes whose
;;; instances the runtime makes itself -- <symbol>, <keyword>,
;;; <character>, <class>, <generic-function>, <method>, <singleton> and
;;; <slot-descriptor> -- has none of their contents: it prints as any
;;; other instance does, unless its class has a method of its own.
(define-method print-object ((x <symbol>) stream escape?)
  (if (id? (%object-class x) <symbol>)
      (%put stream (%symbol-name x))
      (next-method)))
(define-method print-object ((x <keyword>) stream escape?)
  (if (id? (%object-class x) <keyword>)
      (begin
        (%put stream (%symbol-name x))
        (%put stream #\:))
      (next-method)))
(define-method print-object ((x <boolean>) stream escape?) (%put stream (if x "#t" "#f")))
(define-method print-object ((x <empty-list>) stream escape?) (%put stream "()"))

(define %character-names
  (list (cons #\space "space") (cons #\newline "newline") (cons (as <character> 9) "tab")
        (cons (as <character> 13) "return") (cons (as <character> 12) "page")
        (cons (as <character> 0) "null") (cons (as <character> 127) "rubout")
        (cons (as <character> 8) "backspace") (cons (as <character> 27) "escape")))

(define-method print-object ((x <character>) stream escape?)
  (cond ((not (id? (%object-class x) <character>)) (next-method))
        (escape?
         (%put stream "#\\")
         (bind ((name (find-pair x %character-names)))
           (%put stream (if name (cdr name) x))))
        (else: (%put stream x))))

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
  (print-object (%class-name (%object-class x)) stream #f)
  (unless (empty? x)
    (%put stream #\space)
    (if (%library-size x)
        (%print-elements x stream escape?)
        (%put stream "...")))
  (%put stream #\}))

;;; An array: <array> and its elements, in row-major order
(define-method print-object ((x <%general-array>) stream escape?)
  (%put stream "{<array>")
  (unless (empty? x)
    (%put stream #\space)
    (%print-elements x stream escape?))
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
  (if (id? (%object-class x) <class>)
      (begin
        (%put stream "{the class ")
        (print-object (%class-name x) stream #f)
        (%put stream #\}))
      (next-method)))

;;; Generic functions and methods print as the manual's examples show them
;;; (chapters 3, 8, 10 and 11).
(define-method print-object ((x <generic-function>) stream escape?)
  (if (id? (%object-class x) <generic-function>)
      (bind ((name (%generic-name x)))
        (if name
            (begin
              (%put stream "{the generic function ")
              (print-object name stream #f)
              (%put stream #\}))
            (%put stream "{an anonymous generic function}")))
      (next-method)))

(define-method print-object ((x <method>) stream escape?)
  (if (id? (%object-class x) <method>)
      (bind ((owner (%method-owner x))
             (name (%method-name x)))
        (cond (owner
               (%put stream "{a method on ")
               (bind ((g (%generic-name owner)))
                 (if g
                     (print-object g stream #f)
                     (%put stream "an anonymous generic function")))
               (%put stream #\}))
              (name
               (%put stream "{method ")
               (print-object name stream #f)
               (%put stream #\}))
              (else: (%put stream "{an anonymous method}"))))
      (next-method)))

(define-method print-object ((x <singleton>) stream escape?)
  (if (id? (%object-class x) <singleton>)
      (begin
        (%put stream "{singleton ")
        (print-object (%singleton-object x) stream #t)
        (%put stream #\}))
      (next-method)))

(define-method print-object ((x <thread>) stream escape?) (%put stream "{thread}"))

(define-method print-object ((x <slot-descriptor>) stream escape?)
  (if (id? (%object-class x) <slot-descriptor>)
      (begin
        (%put stream "{slot ")
        (print-object (%generic-name (%slot-raw-getter x)) stream #f)
        (%put stream #\}))
      (next-method)))

(define-method print-object ((x <stream>) stream escape?) (%put stream "{stream}"))

;;; A condition prints as its message (chapter 13).
(define-method print-object ((c <condition>) stream escape?)
  (%put stream (%condition-message c)))

;;; A condition as display prints it, through any print-object method of
;;; its class: what the debugger of last resort reports.
(define %condition-text
  (method (c)
    (bind ((s (make <string-stream>)))
      (print-object c s #f)
      (stream-contents s))))

;;; A condition's message (chapter 13): CONTROL with ~a and ~d writing an
;;; argument as display does, ~s as write does, ~% and ~& a newline and ~~
;;; a tilde, and any other directive as it is written; the ARGUMENTS no
;;; directive uses follow, each after a space, as display writes them.  A
;;; CONTROL that is not a string is displayed.
(define %message-text
  (method (control arguments)
    (bind ((s (make <string-stream>)))
      (bind-methods ((put (escape?)
                       (unless (id? arguments '())
                         (print-object (car arguments) s escape?)
                         (set! arguments (cdr arguments))))
                     (scan (i n)
                       (when (binary< i n)
                         (bind ((c (%library-element control i %no-default)))
                           (if (and (id? c #\~) (binary< (binary+ i 1) n))
                               (bind ((d (%char-downcase (%library-element control (binary+ i 1) %no-default))))
                                 (cond ((or (id? d #\a) (id? d #\d)) (put #f))
                                       ((id? d #\s) (put #t))
                                       ((or (id? d #\%) (id? d #\&)) (%put s #\newline))
                                       ((id? d #\~) (%put s #\~))
                                       (else: (%put s c) (%put s (%library-element control (binary+ i 1) %no-default))))
                                 (scan (binary+ i 2) n))
                               (begin (%put s c) (scan (binary+ i 1) n)))))))
        (if (%instance? control <string>)
            (scan 0 (%library-size control))
            (print-object control s #f))
        (for-each ((a arguments)) ()
          (%put s #\space)
          (print-object a s #f)))
      (stream-contents s))))

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

;;; pp lays an object out as src/runtime-printing.lisp's PRETTY-PRINT
;;; does: an object whose printed form fits on what is left of a line of
;;; 79 columns is printed as print prints it; otherwise a proper list
;;; headed by a symbol has after its head as many of its elements as fit
;;; on the line (the first, laid out, if none does), and then each further
;;; element on a line of its own, indented two columns past the
;;; parenthesis; any other proper list has each element on a line of its
;;; own, one column past the parenthesis; a vector has them two columns
;;; past the #; and anything else is printed as print prints it.

(define %printed-form
  (method (object)
    (bind ((s (make <string-stream>)))
      (print-object object s #t)
      (stream-contents s))))

(define %proper-list?
  (method (x)
    (bind-methods ((loop (l) (if (%pair? l) (loop (cdr l)) (id? l '()))))
      (and (%pair? x) (loop x)))))

(define %fits?
  (method (column text) (not (binary< 79 (binary+ column (%library-size text))))))

(define %pretty-print
  (method (object s column)
    (bind ((flat (%printed-form object)))
      (bind-methods ((lines (elements indent)
                       ;; Each of ELEMENTS on a new line at INDENT
                       (for-each ((element elements)) ()
                         (%put s #\newline)
                         (for ((i 0 (binary+ i 1))) ((not (binary< i indent)))
                           (%put s #\space))
                         (%pretty-print element s indent))))
        (cond ((%fits? column flat) (%put s flat))
              ((%proper-list? object)
               (bind ((head (car object)))
                 (%put s #\()
                 (if (%instance? head <symbol>)
                     (bind ((name (%printed-form head))
                            (at (binary+ (binary+ column 1) (%library-size name)))
                            (rest (cdr object)))
                       (%put s name)
                       ;; The elements that fit on the head's line
                       (bind-methods ((fill ()
                                        (unless (id? rest '())
                                          (bind ((next (%printed-form (car rest))))
                                            (when (%fits? (binary+ at 1) next)
                                              (%put s #\space)
                                              (%put s next)
                                              (set! at (binary+ at (binary+ 1 (%library-size next))))
                                              (set! rest (cdr rest))
                                              (fill))))))
                         (fill))
                       (when (and (not (id? rest '())) (id? rest (cdr object)))
                         (%put s #\space)
                         (%pretty-print (car rest) s (binary+ at 1))
                         (set! rest (cdr rest)))
                       (lines rest (binary+ column 2)))
                     (begin
                       (%pretty-print head s (binary+ column 1))
                       (lines (cdr object) (binary+ column 1))))
                 (%put s #\))))
              ((and (%instance? object <simple-object-vector>) (not (empty? object)))
               (%put s "#(")
               (%pretty-print (%library-element object 0 %no-default) s (binary+ column 2))
               (lines (cdr (as <list> object)) (binary+ column 2))
               (%put s #\)))
              (else: (%put s flat)))))))

(define pp
  (method (object #key stream)
    (bind ((s (%stream stream)))
      (%pretty-print object s 0)
      (%put s #\newline)
      object)))

;;; format

(define format
  (method (stream control #rest arguments)
    (unless (%instance? control <string>)
      (error "format: not a string: ~s" control))
    (bind ((s (if (not stream) (make <string-stream>) (%stream stream)))
           (n (%library-size control)))
      (bind-methods ((next-argument ()
                       (if (id? arguments '())
                           (error "format: too few arguments for ~s" control)
                           (bind ((a (car arguments)))
                             (set! arguments (cdr arguments))
                             a)))
                     (scan (i)
                       (when (binary< i n)
                         (bind ((c (%library-element control i %no-default)))
                           (if (and (id? c #\~) (binary< (binary+ i 1) n))
                               (begin
                                 (bind ((d (%char-downcase (%library-element control (binary+ i 1) %no-default))))
                                   (cond ((or (id? d #\a) (id? d #\d))
                                          (print-object (next-argument) s #f))
                                         ((id? d #\s) (print-object (next-argument) s #t))
                                         ((or (id? d #\%) (id? d #\&)) (%put s #\newline))
                                         ((id? d #\~) (%put s #\~))
                                         ;; Any other: as it is written
                                         (else: (%put s c)
                                                (%put s (%library-element control (binary+ i 1) %no-default)))))
                                 (scan (binary+ i 2)))
                               (begin (%put s c) (scan (binary+ i 1))))))))
        (scan 0))
      (if stream #f (stream-contents s)))))
