;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; The names the DYLAN module exports.
;;;;
;;;; Provisional: these are the predefined names of the Common Lisp
;;;; implementation (src/compiler.lisp), so that the front end can resolve
;;;; and check references to them.  When the library is written in Dylan
;;;; (CompilerPlan.md, Phase 8) its own define-module form replaces this
;;;; list.

(in-package #:dylan-compiler)

(defparameter *dylan-module-names*
  '("*" "+" "-" "/" "/=" "<" "<=" "<abort>" "<array>" "<byte-string>"
    "<character>" "<class>" "<collection>" "<complex>" "<condition>" "<deque>"
    "<double-float>" "<empty-list>" "<error>" "<explicit-key-collection>"
    "<extended-float>" "<float>" "<function>" "<generic-function>" "<integer>"
    "<keyword>" "<list>" "<method>" "<mutable-collection>"
    "<mutable-explicit-key-collection>" "<mutable-sequence>" "<number>"
    "<object>" "<pair>" "<range>" "<ratio>" "<rational>" "<real>"
    "<rectangular-complex>" "<restart>" "<sequence>" "<serious-condition>"
    "<simple-error>" "<simple-object-vector>" "<simple-restart>"
    "<simple-warning>" "<single-float>" "<singleton>" "<slot-descriptor>"
    "<stretchy-vector>" "<string>" "<symbol>" "<table>" "<type-error>"
    "<unicode-string>" "<vector>" "<warning>" "=" "=hash" ">" ">=" "abort"
    "abs" "acos" "acosh" "add" "add!" "add-method" "add-new" "add-new!"
    "add-slot" "all-superclasses" "always" "angle" "any?" "append"
    "applicable-method?" "apply" "aref" "as" "as-lowercase" "as-lowercase!"
    "as-uppercase" "as-uppercase!" "ash" "asin" "asinh" "atan" "atan2" "atanh"
    "binary*" "binary+" "binary-" "binary-gcd" "binary-lcm" "binary/" "binary<"
    "binary=" "break" "caaar" "caadr" "caar" "cadar" "caddr" "cadr" "car"
    "cdaar" "cdadr" "cdar" "cddar" "cdddr" "cddr" "cdr" "ceiling" "ceiling/"
    "cerror" "check-type" "choose" "choose-by" "class-for-copy" "complement"
    "compose" "concatenate" "concatenate-as" "condition-format-arguments"
    "condition-format-string" "conjoin" "cons" "copy-sequence" "copy-state"
    "cos" "cosh" "current-element" "current-key" "curry" "default-handler"
    "denominator" "dimensions" "direct-subclasses" "direct-superclasses"
    "disjoin" "display" "do" "do-handlers" "element" "empty?" "error" "even?"
    "every?" "exp" "expt" "fill!" "final-state" "find-key" "find-method"
    "find-pair" "first" "floor" "floor/" "freeze-methods" "function-arguments"
    "gcd" "generic-function-methods" "id?" "identity" "imag-part"
    "init-function" "init-keyword" "init-value" "initial-state" "initialize"
    "instance?" "integral?" "intersection" "key-sequence" "last" "lcm" "list"
    "list*" "log" "logand" "logandc1" "logandc2" "logbit?" "logeqv" "logior"
    "lognand" "lognor" "lognot" "logorc1" "logorc2" "logxor" "magnitude" "make"
    "make-polar" "make-read-only" "make-rectangular" "map" "map-as" "map-into"
    "max" "member?" "method-specializers" "min" "modulo" "negative?" "newline"
    "next-state" "not" "numerator" "object-class" "odd?" "pop" "pop-last"
    "positive?" "pp" "previous-state" "print" "push" "push-last" "range"
    "rationalize" "rcurry" "real-part" "reduce" "reduce1" "remainder" "remove"
    "remove!" "remove-duplicates" "remove-duplicates!" "remove-key!"
    "remove-method" "remove-slot" "replace-elements!" "replace-subsequence!"
    "restart-query" "return-allowed?" "return-description" "return-query"
    "reverse" "reverse!" "round" "round/" "seal" "second" "shallow-copy"
    "signal" "sin" "singleton" "sinh" "size" "slot-allocation"
    "slot-descriptor" "slot-descriptors" "slot-getter" "slot-initialized?"
    "slot-setter" "slot-type" "slot-value" "sort" "sort!"
    "sorted-applicable-methods" "sqrt" "subclass?" "subsequence-position" "tan"
    "tanh" "third" "truncate" "truncate/" "type-error-expected-type"
    "type-error-value" "unary-" "unary/" "union" "values" "vector" "write-line"
    "zero?")
    "The variables the DYLAN module owns and exports.")

(defparameter *dylan-module-setter-names*
  '("aref" "caaar" "caadr" "caar" "cadar" "caddr" "cadr" "car" "cdaar" "cdadr"
    "cdar" "cddar" "cdddr" "cddr" "cdr" "current-element" "direct-superclasses"
    "element" "first" "last" "second" "size" "slot-value" "third")
    "The names X for which the DYLAN module also owns (setter X).")

(defparameter *dylan-module-extension-names*
  '(;; Threads (CompilerPlan.md, Phase 7)
    "<thread>" "<lock>" "<recursive-lock>" "<condition-variable>"
    "<atomic-integer>" "<thread-error>"
    "make-thread" "join-thread" "current-thread" "thread-yield"
    "thread-error-thread" "thread-error-condition"
    "acquire-lock" "release-lock"
    "wait-condition-variable" "notify-condition-variable"
    "notify-all-condition-variable"
    "atomic-value" "atomic-add!" "atomic-compare-and-swap!"
    ;; Printing and streams (Phase 8)
    "write" "print-object" "format" "<stream>" "<string-stream>"
    "*standard-output*" "*standard-error*" "stream-contents"
    ;; Timing (Phase 9)
    "current-milliseconds")
  "Variables the DYLAN module exports that the manual does not define.")

(defparameter *dylan-module-internal-names*
  '("<boolean>"
    "%shallow-copy" "%write-fd" "%slot-value-setter" "%make" "%signal" "%set-error-hooks" "%format-message" "%die" "%warn"
    "%break" "%make-thread" "%join-thread" "%thread-condition" "%unhandled"
    "%number-add" "%number-sub" "%number-mul" "%number-div" "%number-negate"
    "%number=" "%number<" "%number?" "%divide" "%math" "%float?" "%float"
    "%numerator" "%denominator" "%rationalize" "%logand" "%logior" "%logxor"
    "%lognot" "%logbit?" "%ash" "%gcd" "%character->integer"
    "%integer->character" "%character?" "%char-upcase" "%char-downcase"
    "%vector-size" "%vector-element" "%vector-element-setter" "%make-vector"
    "%make-byte-string" "%make-unicode-string" "%replace-vector!" "%set-car!"
    "%set-cdr!" "%pair?" "%intern-symbol" "%intern-keyword" "%symbol-name"
    "%object-hash" "%number-hash" "%string-hash" "%write-string"
    "%write-char" "%number->string" "%class-name" "%generic-name"
    "%singleton-object" "%function?")
  "Variables the DYLAN module owns, with values in the runtime, but does not
export: for the library's own use.")
