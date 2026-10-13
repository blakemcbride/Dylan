module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; Characters (chapter 16), keywords and symbols (chapter 15).

(define-generic-function as-uppercase (object))
(define-method as-uppercase ((c <character>)) (%char-upcase c))

(define-generic-function as-lowercase (object))
(define-method as-lowercase ((c <character>)) (%char-downcase c))

;;; A character's integer is its Unicode code point.

(define-method as ((class (singleton <integer>)) (c <character>))
  (%character->integer c))

(define-method as ((class (singleton <character>)) (n <integer>))
  (%integer->character n))

;;; Symbols and keywords are not case sensitive, but keep the case they
;;; were first written in.

(define-method as ((class (singleton <symbol>)) (s <string>))
  (%intern-symbol (as <byte-string> s)))

(define-method as ((class (singleton <keyword>)) (s <string>))
  (%intern-keyword (as <byte-string> s)))

(define-method as ((class (singleton <string>)) (s <symbol>)) (%symbol-name s))
(define-method as ((class (singleton <string>)) (k <keyword>)) (%symbol-name k))
(define-method as ((class (singleton <byte-string>)) (s <symbol>)) (%symbol-name s))
(define-method as ((class (singleton <byte-string>)) (k <keyword>)) (%symbol-name k))
