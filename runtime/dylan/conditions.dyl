module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; Conditions (chapter 13).
;;;
;;; This file is part of the library every program is compiled with.  The
;;; dynamic chain of handlers and exits is the runtime's
;;; (runtime/c/control.c): %signal finds and calls a handler, or
;;; default-handler.  The runtime signals the errors it detects itself --
;;; wrong arguments, type errors, overflow and the rest -- through
;;; %runtime-error and %runtime-type-error, which it is given below.

;;; The classes

(define-class <condition> (<object>))
(define-class <serious-condition> (<condition>))
(define-class <error> (<serious-condition>))

(define-class <simple-error> (<error>)
  (getter: condition-format-string setter: #f
   init-keyword: format-string: init-value: "")
  (getter: condition-format-arguments setter: #f
   init-keyword: format-arguments: init-value: '()))

(define-class <type-error> (<error>)
  (getter: type-error-value setter: #f init-keyword: value:)
  (getter: type-error-expected-type setter: #f init-keyword: type:))

(define-class <warning> (<condition>))

(define-class <simple-warning> (<warning>)
  (getter: condition-format-string setter: #f
   init-keyword: format-string: init-value: "")
  (getter: condition-format-arguments setter: #f
   init-keyword: format-arguments: init-value: '()))

;;; condition: is accepted, and ignored by <restart> itself.
(define-class <restart> (<condition>)
  (getter: %restart-condition setter: #f init-keyword: condition: init-value: #f))
(define-class <simple-restart> (<restart>))
(define-class <abort> (<restart>))

;;; Messages: what the debugger of last resort reports.

(define-generic-function %condition-message (condition))

(define-method %condition-message ((c <condition>))
  (%format-message "~s" (list c)))

(define-method %condition-message ((c <simple-error>))
  (%format-message (condition-format-string c) (condition-format-arguments c)))

(define-method %condition-message ((c <simple-warning>))
  (%format-message (condition-format-string c) (condition-format-arguments c)))

(define-method %condition-message ((c <type-error>))
  (%format-message "~s is not an instance of ~s"
                   (list (type-error-value c) (type-error-expected-type c))))

;;; Default handlers

(define-generic-function default-handler (condition))

(define-method default-handler ((c <condition>)) #f)

(define-method default-handler ((c <serious-condition>))
  (%die (%condition-message c)))

(define-method default-handler ((c <warning>))
  (%warn (%condition-message c))
  #f)

(define-method default-handler ((c <restart>))
  (error "no handler was established for the restart ~s" c))

;;; Signalling

(define %make-condition
  (method (class condition arguments)
    (if (instance? condition <condition>)
        condition
        (make class format-string: condition format-arguments: arguments))))

(define signal
  (method (condition #rest arguments)
    (%signal (%make-condition <simple-warning> condition arguments))))

(define error
  (method (condition #rest arguments)
    (bind ((c (%make-condition <simple-error> condition arguments)))
      (%signal c)
      ;; A handler returned, which error does not allow.
      (%die (%format-message "a handler returned from error: ~a"
                             (list (%condition-message c)))))))

(define cerror
  (method (restart-description condition #rest arguments)
    (handler-case (apply error condition arguments)
      ((<simple-restart> description: restart-description) #f))))

(define break
  (method (#rest arguments)
    (%break (if (id? arguments '())
                "break"
                (%condition-message
                 (%make-condition <simple-warning> (car arguments) (cdr arguments)))))))

(define check-type
  (method (value type)
    (if (instance? value type)
        value
        (error (make <type-error> value: value type: type)))))

(define abort
  (method ()
    (error (make <abort>))))

;;; Interactive handling and introspection: there is no interactive
;;; debugger, so these do the least the manual allows.

(define-generic-function restart-query (restart))
(define-method restart-query ((r <restart>)) #f)

(define-generic-function return-query (condition))
(define-method return-query ((c <condition>)) #f)

(define-generic-function return-allowed? (condition))
(define-method return-allowed? ((c <condition>)) #f)

(define-generic-function return-description (condition))
(define-method return-description ((c <condition>)) #f)

;;; The runtime's own errors

(define %runtime-error
  (method (format-string arguments)
    (error (make <simple-error> format-string: format-string
                                format-arguments: arguments))))

(define %runtime-type-error
  (method (value type)
    (error (make <type-error> value: value type: type))))

(%set-error-hooks %runtime-error %runtime-type-error default-handler)
