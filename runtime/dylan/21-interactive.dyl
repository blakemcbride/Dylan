module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; Names whose meaning only the interactive implementation gives (user
;;; manual, chapters 14 and 16).  The dylan module of a compiled program
;;; has them too, so that the two implementations' dylan modules export
;;; the same names, but calling one is an error.
;;;
;;;   (gensym)                    macro bodies call it while compiling;
;;;                               a compiled program cannot
;;;   (lisp-variable symbol)      a compiled program has no host Lisp
;;;   (lisp-procedure symbol)

(define gensym
  (method (#rest arguments)
    (error "gensym: only macros, while compiling, can call gensym")))

(define lisp-variable
  (method (name)
    (error "lisp-variable: a compiled program has no host Lisp")))

(define lisp-procedure
  (method (name)
    (error "lisp-procedure: a compiled program has no host Lisp")))
