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
;;;; This file contains the Dylan -> Common Lisp compiler and routines
;;;; needed ONLY at compilation time.  Support routines that are also
;;;; needed when Dylan programs run are located in generic.lisp (generic
;;;; operator dispatch), class.lisp (class heterarchy), and support.lisp
;;;; (general support routines).            (was Thomas's src/compiler.scm)
;;;;
;;;; The Scheme original emitted Scheme; this emits Common Lisp.  The
;;;; correspondences are the obvious ones -- BEGIN became PROGN, SET!
;;;; became SETQ, dotted lambda lists became &REST, LETREC became LET plus
;;;; SETQ, and Scheme's CASE (which compares with EQV? and spells its
;;;; default ELSE) became a COND over MEMBER, since Common Lisp's CASE
;;;; would read a clause whose key list is (t) as the default.

(in-package #:dylan)

;;; Normal external entry points for compilation

(defun compile-expression (e multi-value mod-vars continue)
  "E is a single Dylan expression.  MULTI-VALUE is an expression to be
passed as the multi-value vector (or NIL) at runtime.  MOD-VARS is a list
of pre-existing module variables.  CONTINUE is a function that receives:
  a: the list of newly created module variables,
  b: the preamble (definitions of free variables, refs, sets),
  c: the output code."
  (flet ((define-module-variable (name)
           `(defparameter ,(name->module-variable name) ',the-unassigned-value))
         (define-module-getter (name)
           `(defun ,(name->module-getter name) ()
              ,(name->module-variable name)))
         (define-module-setter (name)
           `(defun ,(name->module-setter name) (new-value)
              (setq ,(name->module-variable name) new-value))))
    (really-compile
     e mod-vars '() multi-value
     (lambda (compiled-output free-vars)
       (let* ((need-getter/setters
                (if (null free-vars)
                    '()
                    (set-difference free-vars mod-vars #'member)))
              (need-definition
                (set-difference need-getter/setters
                                dylan%predefined-variables #'member)))
         (funcall continue
                  need-definition
                  `(,@(mapcar #'define-module-variable need-definition)
                    ,@(mapcar #'define-module-getter need-getter/setters)
                    ,@(mapcar #'define-module-setter need-getter/setters))
                  compiled-output))))))

(defparameter dylan%lisp-names-of-predefined-names
  `((* dylan/*)                                     ; Method
    (+ dylan/+)                                     ; Method
    (- dylan/-)                                     ; Method
    (/ dylan//)                                     ; Method
    (/= dylan//=)                                   ; Method
    (< dylan/<)                                     ; Method
    (<= dylan/<=)                                   ; Method
    (<abort> <abort>)                               ; Class
    (<array> <array>)                               ; Class
    (<byte-string> <byte-string>)                   ; Class
    (<character> <character>)                       ; Class
    (<class> <class>)                               ; Class
    (<collection> <collection>)                     ; Class
    (<complex> <complex>)                           ; Class
    (<condition> <condition>)                       ; Class
    (<deque> <deque>)                               ; Class
    (<double-float> <double-float>)                 ; Class
    (<empty-list> <empty-list>)                     ; Class
    (<error> <error>)                               ; Class
    (<explicit-key-collection>                      ; Class
     <explicit-key-collection>)
    (<extended-float> <extended-float>)             ; Class
    (<float> <float>)                               ; Class
    (<function> <function>)                         ; Class
    (<generic-function> <generic-function>)         ; Class
    (<integer> <integer>)                           ; Class
    (<keyword> <keyword>)                           ; Class
    (<list> <list>)                                 ; Class
    (<method> <method>)                             ; Class
    (<mutable-collection> <mutable-collection>)     ; Class
    (<mutable-explicit-key-collection>              ; Class
     <mutable-explicit-key-collection>)
    (<mutable-sequence> <mutable-sequence>)         ; Class
    (<number> <number>)                             ; Class
    (<object> <object>)                             ; Class
    (<pair> <pair>)                                 ; Class
    (<range> <range>)                               ; Class
    (<ratio> <ratio>)                               ; Class
    (<rational> <rational>)                         ; Class
    (<real> <real>)                                 ; Class
    (<rectangular-complex> <rectangular-complex>)   ; Class
    (<restart> <restart>)                           ; Class
    (<sequence> <sequence>)                         ; Class
    (<serious-condition> <serious-condition>)       ; Class
    (<simple-error> <simple-error>)                 ; Class
    (<simple-object-vector> <simple-object-vector>) ; Class
    (<simple-restart> <simple-restart>)             ; Class
    (<simple-warning> <simple-warning>)             ; Class
    (<single-float> <single-float>)                 ; Class
    (<singleton> <singleton>)                       ; Class
    (<slot-descriptor> <slot-descriptor>)           ; Class
    (<stretchy-vector> <stretchy-vector>)           ; Class
    (<string> <string>)                             ; Class
    (<symbol> <symbol>)                             ; Class
    (<table> <table>)                               ; Class
    (<type-error> <type-error>)                     ; Class
    (<unicode-string> <unicode-string>)             ; Class
    (<vector> <vector>)                             ; Class
    (<warning> <warning>)                           ; Class
    (= dylan/=)                                     ; Method
    (=hash dylan/=hash)                             ; Generic-Function
    (> dylan/>)                                     ; Method
    (>= dylan/>=)                                   ; Method
    (id? dylan/id?)                                 ; Method
    (abort dylan/abort)                             ; Sealed-Function
    (abs dylan/abs)                                 ; Generic-Function
    (acos dylan/acos)                               ; Generic-Function
    (acosh dylan/acosh)                             ; Generic-Function
    (add dylan/add)                                 ; Generic-Function
    (add! dylan/add!)                               ; G.F. Method
    (add-method dylan/add-method)                   ; Generic-Function
    (add-new dylan/add-new)                         ; Generic-Function
    (add-new! dylan/add-new!)                       ; Generic-Function
    (add-slot dylan/add-slot)                       ; Generic-Function
    (all-superclasses dylan/all-superclasses)       ; Generic-Function
    (always dylan/always)                           ; Method
    (angle dylan/angle)                             ; Generic-Method
    (any? dylan/any?)                               ; Generic-Function
    (append dylan/append)                           ; Generic-Function
    (applicable-method? dylan/applicable-method?)   ; Generic-Function
    (apply dylan/apply)                             ; Function
    (aref dylan/aref)                               ; Generic-Function
    (as dylan/as)                                   ; Generic-Function
    (as-lowercase dylan/as-lowercase)               ; G.F. Method
    (as-lowercase! dylan/as-lowercase!)             ; G.F. Method
    (as-uppercase dylan/as-uppercase)               ; G.F. Method
    (as-uppercase! dylan/as-uppercase!)             ; G.F. Method
    (ash dylan/ash)                                 ; Generic-Method
    (asin dylan/asin)                               ; Generic-Function
    (asinh dylan/asinh)                             ; Generic-Function
    (atan dylan/atan)                               ; Generic-Function
    (atan2 dylan/atan2)                             ; Generic-Function
    (atanh dylan/atanh)                             ; Generic-Function
    (binary* dylan/binary*)                         ; Generic-Function
    (binary+ dylan/binary+)                         ; Generic-Function
    (binary- dylan/binary-)                         ; Generic-Function
    (binary-gcd dylan/binary-gcd)                   ; Generic-Method
    (binary-lcm dylan/binary-lcm)                   ; Generic-Method
    (binary/ dylan/binary/)                         ; Generic-Function
    (binary< dylan/binary<)                         ; Generic-Function
    (binary= dylan/binary=)                         ; Generic-Function
    (break dylan/break)                             ; Sealed-Function
    (caaar dylan/caaar)                             ; Method
    (caadr dylan/caadr)                             ; Method
    (caar dylan/caar)                               ; Method
    (cadar dylan/cadar)                             ; Method
    (caddr dylan/caddr)                             ; Method
    (cadr dylan/cadr)                               ; Method
    (car dylan/car)                                 ; Method
    (cdaar dylan/cdaar)                             ; Method
    (cdadr dylan/cdadr)                             ; Method
    (cdar dylan/cdar)                               ; Method
    (cddar dylan/cddar)                             ; Method
    (cdddr dylan/cdddr)                             ; Method
    (cddr dylan/cddr)                               ; Method
    (cdr dylan/cdr)                                 ; Method
    (ceiling dylan/ceiling)                         ; Generic-Function
    (ceiling/ dylan/ceiling/)                       ; Generic-Function
    (cerror dylan/cerror)                           ; Sealed-Function
    (check-type dylan/check-type)                   ; Sealed-Function
    (choose dylan/choose)                           ; Generic-Function
    (choose-by dylan/choose-by)                     ; Generic-Function
    (class-for-copy dylan/class-for-copy)           ; Generic-Function
    (complement dylan/complement)                   ; Method
    (compose dylan/compose)                         ; Method
    (concatenate dylan/concatenate)                 ; Generic-Function
    (concatenate-as dylan/concatenate-as)           ; Generic-Function
    (conjoin dylan/conjoin)                         ; Method
    (cons dylan/cons)                               ; Method
    (copy-sequence dylan/copy-sequence)             ; Generic-Function
    (copy-state dylan/copy-state)                   ; Generic-Function
    (cos dylan/cos)                                 ; Generic-Function
    (cosh dylan/cosh)                               ; Generic-Function
    (current-element dylan/current-element)         ; Generic-Function
    (current-key dylan/current-key)                 ; Generic-Function
    (curry dylan/curry)                             ; Method
    (default-handler dylan/default-handler)         ; Generic-Function
    (denominator dylan/denominator)                 ; Generic-Method
    (dimensions dylan/dimensions)                   ; Generic-Function
    (direct-subclasses dylan/direct-subclasses)     ; Generic-Function
    (direct-superclasses dylan/direct-superclasses) ; Generic-Function
    (disjoin dylan/disjoin)                         ; Method
    (do dylan/do)                                   ; Generic-Function
    (do-handlers dylan/do-handlers)                 ; Sealed-Function
    (element dylan/element)                         ; Generic-Function
    (empty? dylan/empty?)                           ; Generic-Function
    (error dylan/error)                             ; Sealed-Function
    (even? dylan/even?)                             ; Generic-Function
    (every? dylan/every?)                           ; Generic-Function
    (exp dylan/exp)                                 ; Generic-Function
    (expt dylan/expt)                               ; Generic-Function
    (fill! dylan/fill!)                             ; Generic-Function
    (final-state dylan/final-state)                 ; G.F. Method
    (find-key dylan/find-key)                       ; Generic-Function
    (find-method dylan/find-method)                 ; Generic-Function
    (find-pair dylan/find-pair)                     ; Generic-Function
    (first dylan/first)                             ; Generic-Function
    (floor dylan/floor)                             ; Generic-Function
    (floor/ dylan/floor/)                           ; Generic-Function
    (freeze-methods dylan/freeze-methods)           ; Generic-Function
    (function-arguments dylan/function-arguments)   ; Generic-Function
    (gcd dylan/gcd)                                 ; Method
    (generic-function-methods                       ; Generic-Function
     dylan/generic-function-methods)
    (identity dylan/identity)                       ; Method
    (imag-part dylan/imag-part)                     ; Generic-Method
    (init-function dylan/init-function)             ; Generic-Function
    (init-keyword dylan/init-keyword)               ; Generic-Function
    (init-value dylan/init-value)                   ; Generic-Function
    (initial-state dylan/initial-state)             ; Generic-Function
    (initialize dylan/initialize)                   ; Generic-Function
    (instance? dylan/instance?)                     ; Generic-Function
    (integral? dylan/integral?)                     ; Generic-Function
    (intersection dylan/intersection)               ; Generic-Function
    (key-sequence dylan/key-sequence)               ; Generic-Function
    (last dylan/last)                               ; Generic-Function
    (lcm dylan/lcm)                                 ; Method
    (list dylan/list)                               ; Method
    (list* dylan/list*)                             ; Method
    (log dylan/log)                                 ; Generic-Function
    (logand dylan/logand)                           ; Generic-Method
    (logandc1 dylan/logandc1)                       ; Generic-Method
    (logandc2 dylan/logandc2)                       ; Generic-Method
    (logbit? dylan/logbit?)                         ; Generic-Method
    (logeqv dylan/logeqv)                           ; Generic-Method
    (logior dylan/logior)                           ; Generic-Method
    (lognand dylan/lognand)                         ; Generic-Method
    (lognor dylan/lognor)                           ; Generic-Method
    (lognot dylan/lognot)                           ; Generic-Method
    (logorc1 dylan/logorc1)                         ; Generic-Method
    (logorc2 dylan/logorc2)                         ; Generic-Method
    (logxor dylan/logxor)                           ; Generic-Method
    (make dylan/make)                               ; Generic-Function
    (make-polar dylan/make-polar)                   ; Generic-Function
    (make-read-only dylan/make-read-only)           ; Generic-Function
    (make-rectangular dylan/make-rectangular)       ; Generic-Function
    (map dylan/map)                                 ; Generic-Function
    (map-as dylan/map-as)                           ; Generic-Function
    (map-into dylan/map-into)                       ; Generic-Function
    (max dylan/max)                                 ; Method
    (member? dylan/member?)                         ; Generic-Function
    (method-specializers dylan/method-specializers) ; Generic-Function
    (min dylan/min)                                 ; Method
    (modulo dylan/modulo)                           ; Generic-Function
    (negative? dylan/negative?)                     ; Generic-Function
    (next-state dylan/next-state)                   ; Generic-Function
    (not dylan/not)                                 ; Function
    (numerator dylan/numerator)                     ; Generic-Method
    (object-class dylan/object-class)               ; Generic-Function
    (odd? dylan/odd?)                               ; Generic-Function
    (pop dylan/pop)                                 ; Generic-Function
    (pop-last dylan/pop-last)                       ; Generic-Function
    (positive? dylan/positive?)                     ; Generic-Function
    (previous-state dylan/previous-state)           ; G.F. Method
    (push dylan/push)                               ; Generic-Function
    (push-last dylan/push-last)                     ; Generic-Function
    (range dylan/range)                             ; Generic-Function
    (rcurry dylan/rcurry)                           ; Method
    (real-part dylan/real-part)                     ; Generic-Method
    (rationalize dylan/rationalize)                 ; Generic-Method
    (reduce dylan/reduce)                           ; Generic-Function
    (reduce1 dylan/reduce1)                         ; Generic-Function
    (remainder dylan/remainder)                     ; Generic-Function
    (remove dylan/remove)                           ; Generic-Function
    (remove! dylan/remove!)                         ; Generic-Function
    (remove-duplicates dylan/remove-duplicates)     ; Generic-Function
    (remove-duplicates! dylan/remove-duplicates!)   ; Generic-Function
    (remove-key! dylan/remove-key!)                 ; Generic-Function
    (remove-method dylan/remove-method)             ; Generic-Function
    (remove-slot dylan/remove-slot)                 ; Generic-Function
    (replace-elements! dylan/replace-elements!)     ; Generic-Function
    (replace-subsequence! dylan/replace-subsequence!) ; Generic-Function
    (restart-query dylan/restart-query)             ; Generic-Function
    (return-allowed? dylan/return-allowed?)         ; Generic-Function
    (return-description dylan/return-description)   ; Generic-Function
    (return-query dylan/return-query)               ; Generic-Function
    (reverse dylan/reverse)                         ; Generic-Function
    (reverse! dylan/reverse!)                       ; Generic-Function
    (round dylan/round)                             ; Generic-Function
    (round/ dylan/round/)                           ; Generic-Function
    (seal dylan/seal)                               ; Generic-Function
    (second dylan/second)                           ; Generic-Function
    (shallow-copy dylan/shallow-copy)               ; Generic-Function
    (signal dylan/signal)                           ; Sealed-Function
    (sin dylan/sin)                                 ; Generic-Function
    (singleton dylan/singleton)                     ; Function
    (sinh dylan/sinh)                               ; Generic-Function
    (size dylan/size)                               ; G.F. Method
    (slot-allocation dylan/slot-allocation)         ; Generic-Function
    (slot-descriptor dylan/slot-descriptor)         ; Generic-Function
    (slot-descriptors dylan/slot-descriptors)       ; Generic-Function
    (slot-getter dylan/slot-getter)                 ; Generic-Function
    (slot-initialized? dylan/slot-initialized?)     ; Generic-Function
    (slot-setter dylan/slot-setter)                 ; Generic-Function
    (slot-type dylan/slot-type)                     ; Generic-Function
    (slot-value dylan/slot-value)                   ; Generic-Function
    (sort dylan/sort)                               ; Generic-Function
    (sort! dylan/sort!)                             ; Generic-Function
    (sorted-applicable-methods                      ; Generic-Function
     dylan/sorted-applicable-methods)
    (sqrt dylan/sqrt)                               ; Generic-Method
    (subclass? dylan/subclass?)                     ; Generic-Function
    (subsequence-position dylan/subsequence-position) ; Generic-Function
    (tan dylan/tan)                                 ; Generic-Function
    (tanh dylan/tanh)                               ; Generic-Function
    (third dylan/third)                             ; Generic-Function
    (truncate dylan/truncate)                       ; Generic-Function
    (truncate/ dylan/truncate/)                     ; Generic-Function
    (unary- dylan/unary-)                           ; Generic-Function
    (unary/ dylan/unary/)                           ; Generic-Function
    (union dylan/union)                             ; Generic-Function
    (values dylan/values)                           ; Function
    (vector dylan/vector)                           ; Method
    (zero? dylan/zero?)                             ; Generic-Function
    ;;;;;;;;;;;;;;; SETTER VARIABLES
    (,(name->setter 'slot-value) dylan/setter/slot-value/)
    (,(name->setter 'element) dylan/setter/element/)
    (,(name->setter 'current-element) dylan/setter/current-element/)
    (,(name->setter 'first) dylan/setter/first/)
    (,(name->setter 'second) dylan/setter/second/)
    (,(name->setter 'third) dylan/setter/third/)
    (,(name->setter 'aref) dylan/setter/aref/)
    (,(name->setter 'car) dylan/setter/car/)
    (,(name->setter 'cdr) dylan/setter/cdr/)
    (,(name->setter 'caar) dylan/setter/caar/)
    (,(name->setter 'cadr) dylan/setter/cadr/)
    (,(name->setter 'cdar) dylan/setter/cdar/)
    (,(name->setter 'cddr) dylan/setter/cddr/)
    (,(name->setter 'caaar) dylan/setter/caaar/)
    (,(name->setter 'caadr) dylan/setter/caadr/)
    (,(name->setter 'cadar) dylan/setter/cadar/)
    (,(name->setter 'caddr) dylan/setter/caddr/)
    (,(name->setter 'cdaar) dylan/setter/cdaar/)
    (,(name->setter 'cdadr) dylan/setter/cdadr/)
    (,(name->setter 'cddar) dylan/setter/cddar/)
    (,(name->setter 'cdddr) dylan/setter/cdddr/)
    ;;;;;;;;;;;;;;; CRL ADDITIONS
    (display dylan/display)
    (newline dylan/newline)
    (write-line dylan/write-line)
    (print dylan/print)
    ,@implementation-specific/additional-dylan-bindings))

(defparameter dylan%predefined-names
  (mapcar #'car dylan%lisp-names-of-predefined-names))

(defparameter dylan%predefined-variables
  (mapcar #'cadr dylan%lisp-names-of-predefined-names))

(defun compile-to-file (file-name &rest expressions)
  "Compile the Dylan EXPRESSIONS, writing the resulting Lisp program to
FILE-NAME."
  (compile-expression
   `(begin ,@expressions) nil '()
   (lambda (new-vars preamble-code compiled)
     (declare (ignore new-vars))        ; Not used
     (with-open-file (out file-name :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
       (format out ";;; Output generated by the CRL Dylan->Lisp compiler.~%")
       (implementation-specific/generate-file
        expressions
        (append preamble-code
                (list `(dylan%catch-all-conditions (lambda () ,compiled))))
        out))
     file-name)))

(defun compile-file (input output)
  "Compile the INPUT file of Dylan expressions into the OUTPUT file."
  (with-open-file (in input)
    (let ((eof (list 'eof))
          (exprs '()))
      (loop for next = (dylan-read in nil eof)
            until (eq next eof)
            do (push next exprs))
      (apply #'compile-to-file output (nreverse exprs)))))

;;; Compile a list of forms, returning a list of Lisp expressions.
;;; ASSUMPTION: multiple-values?, if not NIL, is to be used only for
;;; compiling the last of the forms.

(defun compile-forms (forms module-vars bound-vars really-compile
                      multiple-values? continue)
  (labels ((walk (result forms mod-vars)
             (if (null forms)
                 (funcall continue (reverse result) mod-vars)
                 (funcall really-compile (car forms) mod-vars bound-vars
                          (if (null (cdr forms)) multiple-values? nil)
                          (lambda (compiled mod-vars)
                            (walk (cons compiled result) (cdr forms)
                                  mod-vars))))))
    (walk '() forms module-vars)))

;;; The real compiler.
;;;
;;; Input: E is a form to be compiled
;;;        MODULE-VARS are the module variables already known to exist
;;;        BOUND-VARS are the names of lexically enclosing variables
;;;        MULTIPLE-VALUES? is either NIL, indicating that the current
;;;          expression is being compiled in non-tail position, or has the
;;;          name of an internal variable to be used at runtime to transmit
;;;          the multiple-value returning function along the tail call chain.
;;;        CONTINUE is called with the result of the compilation, and is
;;;          passed the single Lisp form resulting from compiling E and the
;;;          new list of module variables.
;;; Output: always either error exits or tail calls into CONTINUE.

(defun really-compile (e module-vars bound-vars multiple-values? continue)
  (let ((binding (and (consp e) (assoc (car e) compilation-functions))))
    (cond
      ((or (null e) (eq e t) (stringp e) (characterp e) (numberp e))
       (funcall continue e module-vars))
      ((or (keyword? e) (and (vectorp e) (not (stringp e))))
       ;; Keywords self-evaluate in Dylan, but not in Lisp
       (funcall continue `(quote ,e) module-vars))
      ((variable-name? e)
       ;; As in Lisp, but the compiler needs to distinguish bound from free.
       ;; A lexical binding wins over a module variable of the same name:
       ;; the Scheme got that for free, because both compiled to the same
       ;; symbol and the lexical one shadowed the global.  Here the module
       ;; variable has a name of its own, so the shadowing is explicit.
       (let* ((name (variable->name e))
              (lexical (name->lexical name))
              (new-mod-vars (add-variable name bound-vars module-vars)))
         (funcall continue
                  (cond ((member lexical bound-vars) lexical)
                        ((member name new-mod-vars)
                         `(dylan%free-variable-ref
                           ,(name->module-variable name) ',name))
                        (t name))
                  new-mod-vars)))
      ((symbolp e) (dylan%error "illegal Dylan variable" e))
      (binding
       (funcall (cdr binding) (cdr e) module-vars bound-vars
                #'really-compile multiple-values? continue))
      ((and (consp e) (null (cdr (cl:last e))))
       (compile-forms e module-vars bound-vars #'really-compile nil
                      (lambda (forms module-vars)
                        (funcall continue
                                 `(dylan%apply
                                   ,multiple-values?
                                   ,@(mapcar (lambda (x) `(lambda () ,x))
                                             forms))
                                 module-vars))))
      (t (dylan%error "ill-formed expression" e)))))

(defparameter compilation-functions
  '((and            . compile-and-form)
    (begin          . compile-begin-form)
    (bind           . compile-bind-form)
    (bind-exit      . compile-bind-exit-form)
    (bind-methods   . compile-bind-methods-form)
    (case           . compile-case-form)
    (cond           . compile-cond-form)
    (define         . compile-define-form)
    (define-class   . compile-define-class-form)
    (define-generic-function . compile-define-generic-function-form)
    (define-method  . compile-define-method-form)
    (define-slot    . compile-define-slot-form)
    (dotimes        . compile-dotimes-form)
    (for            . compile-for-form)
    (for-each       . compile-for-each-form)
    (handler-bind   . compile-handler-bind-form)
    (handler-case   . compile-handler-case-form)
    (if             . compile-if-form)
    (method         . compile-method-form)
    (or             . compile-or-form)
    (quote          . compile-quote-form)
    (select         . compile-select-form)
    (set!           . compile-set!-form)
    (setter         . compile-setter-form)
    (unless         . compile-unless-form)
    (until          . compile-until-form)
    (unwind-protect . compile-unwind-protect-form)
    (when           . compile-when-form)
    (while          . compile-while-form)))

(defparameter compiled-sharp-f
  (really-compile nil '() '() nil
                  (lambda (compiled free) (declare (ignore free)) compiled)))

(defun compile-and-form (forms module-vars bound-vars really-compile
                         multiple-values? continue)
  (if (null forms)
      (funcall continue t module-vars)  ; (and) is true
      (compile-forms forms module-vars bound-vars really-compile
                     multiple-values?
                     (lambda (code mod-vars)
                       (funcall continue `(and ,@code) mod-vars)))))

(defun compile-begin-form (forms module-vars bound-vars really-compile
                           multiple-values? continue)
  (if (null forms)
      (funcall continue compiled-sharp-f module-vars)
      (compile-forms forms module-vars bound-vars really-compile
                     multiple-values?
                     (lambda (compiled module-vars)
                       (funcall continue `(progn ,@compiled) module-vars)))))

;;; COMPILE-BIND-FORM is in the file comp-class.lisp

(defun compile-bind-exit-form (forms module-vars bound-vars really-compile
                               multiple-values? continue)
  (must-be-list-of-at-least-length forms 1 "BIND-EXIT form invalid")
  (let ((var (car forms))
        (forms (cdr forms)))
    (must-be-list-of-length var 1 "BIND-EXIT bad variable")
    (unless (variable-name? (car var))
      (dylan%error "BIND-EXIT -- bad variable name" var forms))
    (let ((name (variable->lexical (car var))))
      (funcall
       really-compile
       `(begin ,@forms)
       module-vars (cons name bound-vars) multiple-values?
       (lambda (body module-vars)
         (funcall
          continue
          `(dylan%call/cc
            (lambda (!bind-exit)
              (let ((,name
                      (lambda (!multiple-values !next-method &rest values)
                        (declare (ignore !multiple-values !next-method))
                        (funcall !bind-exit
                                 (dylan%lisp-apply
                                  dylan/values ,multiple-values?
                                  next-method/not-generic
                                  values)))))
                ,body)))
          module-vars))))))

;;; COMPILE-BIND-METHODS-FORM is in the file comp-method.lisp
;;; COMPILE-CASE-FORM is in the file comp-sf.lisp
;;; COMPILE-COND-FORM is in the file comp-sf.lisp

(defun compile-define-form (forms module-vars bound-vars really-compile
                            multiple-values? continue)
  (declare (ignore multiple-values?))   ; No reductions
  (must-be-list-of-length forms 2 "Bad DEFINE syntax")
  (let ((name (car forms))
        (value (cadr forms)))
    (unless (variable-name? name)
      (dylan%error "bad DEFINE variable" forms))
    (funcall really-compile value
             (add-module-variable (variable->name name) nil module-vars)
             bound-vars nil
             (lambda (compiled-value new-module-vars)
               (funcall continue
                        `(progn
                           (,(name->module-setter name) ,compiled-value)
                           ',name)
                        new-module-vars)))))

;;; COMPILE-DEFINE-CLASS-FORM is in the file comp-class.lisp
;;; COMPILE-DEFINE-GENERIC-FUNCTION-FORM is in the file comp-class.lisp
;;; COMPILE-DEFINE-METHOD-FORM is in the file comp-method.lisp
;;; COMPILE-DEFINE-SLOT-FORM is in the file comp-class.lisp

(defun compile-dotimes-form (forms module-vars bound-vars really-compile
                             multiple-values? continue)
  (must-be-list-of-at-least-length forms 1 "DOTIMES: bad syntax")
  (let ((v/c/r (car forms))
        (forms (cdr forms)))
    (must-be-list-of-at-least-length v/c/r 2
                                     "DOTIMES: Bad var/count/result list")
    (let ((var (car v/c/r))
          (count-form (cadr v/c/r))
          (result (if (consp (cddr v/c/r)) (caddr v/c/r) nil)))
      (unless (variable-name? var)
        (dylan%error "DOTIMES -- invalid variable" var forms))
      (unless (or (null (cddr v/c/r)) (null (cdddr v/c/r)))
        (dylan%error "DOTIMES -- bad syntax"))
      (let ((name (variable->lexical var)))
        (compile-forms
         forms module-vars (cons name bound-vars) really-compile nil
         (lambda (body-forms module-vars)
           (funcall
            really-compile count-form module-vars bound-vars nil
            (lambda (count-code module-vars)
              ;; As in Common Lisp, the result form sees the variable,
              ;; bound to the number of iterations.
              (funcall
               really-compile result module-vars (cons name bound-vars)
               multiple-values?
               (lambda (result-code module-vars)
                 (funcall continue
                          `(dylan%dotimes ,count-code
                                          (lambda (,name)
                                            (declare (ignorable ,name))
                                            ,result-code)
                                          (lambda (,name) ,@body-forms))
                          module-vars)))))))))))

;;; COMPILE-FOR-FORM is in the file comp-sf.lisp

(defun compile-for-each-form (forms module-vars bound-vars really-compile
                              multiple-values? continue)
  (must-be-list-of-at-least-length forms 2 "FOR-EACH: bad syntax")
  (dolist (binding (car forms))
    (must-be-list-of-length binding 2 "FOR-EACH: bad binding"))
  (let ((names (mapcar (lambda (binding) (variable->lexical (car binding)))
                       (car forms)))
        (collections (mapcar #'cadr (car forms)))
        (end-test-and-return-vals (cadr forms))
        (forms (cddr forms)))
    (compile-forms
     collections module-vars bound-vars really-compile nil
     (lambda (compiled-collections module-vars)
       (compile-forms
        (if (null end-test-and-return-vals) '(nil) end-test-and-return-vals)
        module-vars (append names bound-vars) really-compile multiple-values?
        (lambda (compiled-et module-vars)
          (compile-forms
           forms module-vars (append names bound-vars) really-compile nil
           (lambda (compiled-forms module-vars)
             (funcall
              continue
              ;; When the end test succeeds, the iteration function answers
              ;; a list holding a thunk for the result forms, which
              ;; DYLAN%FOR-EACH calls.  The thunk's parameter is named so
              ;; as not to shadow the caller's multiple-values variable,
              ;; which the result forms are compiled to deliver to.
              `(dylan%for-each
                (lambda (!for-each/multiple-values !dylan/next-method ,@names)
                  (declare (ignorable !for-each/multiple-values
                                      !dylan/next-method))
                  ,(if (null end-test-and-return-vals)
                       `(progn ,@compiled-forms nil)
                       `(if ,(car compiled-et)
                            (dylan%list
                             (lambda ()
                               ,@(if (null (cdr compiled-et))
                                     (list compiled-sharp-f)
                                     (cdr compiled-et))))
                            (progn ,@compiled-forms nil))))
                ,@compiled-collections)
              module-vars)))))))))

;;; COMPILE-HANDLER-BIND-FORM is in the file comp-exc.lisp
;;; COMPILE-HANDLER-CASE-FORM is in the file comp-exc.lisp

(defun compile-if-form (forms module-vars bound-vars really-compile
                        multiple-values? continue)
  (must-be-list-of-length forms 3 "IF: invalid syntax")
  (let ((pred (car forms))
        (conseq (cadr forms))
        (alter (caddr forms)))
    (funcall
     really-compile pred module-vars bound-vars nil
     (lambda (c-pred module-vars)
       (funcall
        really-compile conseq module-vars bound-vars multiple-values?
        (lambda (c-conseq module-vars)
          (funcall
           really-compile alter module-vars bound-vars multiple-values?
           (lambda (c-alter module-vars)
             (funcall continue `(if ,c-pred ,c-conseq ,c-alter)
                      module-vars)))))))))

;;; COMPILE-METHOD-FORM is in the file comp-method.lisp

(defun compile-or-form (forms module-vars bound-vars really-compile
                        multiple-values? continue)
  (compile-forms forms module-vars bound-vars really-compile multiple-values?
                 (lambda (code mod-vars)
                   (funcall continue `(or ,@code) mod-vars))))

(defun compile-quote-form (forms module-vars bound-vars really-compile
                           multiple-values? continue)
  (declare (ignore bound-vars really-compile multiple-values?))
  (must-be-list-of-length forms 1 "QUOTE: invalid syntax")
  (funcall continue `(quote ,@forms) module-vars))

;;; COMPILE-SELECT-FORM is in the file comp-sf.lisp
;;; COMPILE-SET!-FORM is in the file comp-sf.lisp

(defun compile-setter-form (forms module-vars bound-vars really-compile
                            multiple-values? continue)
  (declare (ignore module-vars bound-vars really-compile
                   multiple-values? continue))
  (dylan%error "bad SETTER syntax" forms))

(defun compile-unless-form (forms module-vars bound-vars really-compile
                            multiple-values? continue)
  (must-be-list-of-at-least-length forms 1 "UNLESS: bad syntax")
  (compile-forms forms module-vars bound-vars really-compile
                 (if (null (cdr forms)) nil multiple-values?)
                 (lambda (forms module-vars)
                   (funcall continue
                            `(if (dylan%not ,(car forms))
                                 (progn ,@(if (null (cdr forms))
                                              (list nil)
                                              (cdr forms)))
                                 nil)
                            module-vars))))

(defun compile-until-form (forms module-vars bound-vars really-compile
                           multiple-values? continue)
  (declare (ignore multiple-values?))
  (must-be-list-of-at-least-length forms 1 "UNTIL: bad syntax")
  (compile-forms forms module-vars bound-vars really-compile nil
                 (lambda (forms module-vars)
                   (funcall continue
                            `(dylan%while (lambda () (dylan%not ,(car forms)))
                                          (lambda ()
                                            ,@(if (null (cdr forms))
                                                  (list nil)
                                                  (cdr forms))))
                            module-vars))))

(defun compile-unwind-protect-form (forms module-vars bound-vars really-compile
                                    multiple-values? continue)
  (must-be-list-of-at-least-length forms 1 "UNWIND-PROTECT: bad syntax")
  (funcall
   really-compile (car forms) module-vars bound-vars multiple-values?
   (lambda (c-protect module-vars)
     (funcall
      really-compile `(begin ,@(cdr forms)) module-vars bound-vars nil
      (lambda (c-cleanup module-vars)
        (funcall continue
                 `(dylan%dynamic-wind (lambda () 'done)
                                      (lambda () ,c-protect)
                                      (lambda () ,c-cleanup))
                 module-vars))))))

(defun compile-when-form (forms module-vars bound-vars really-compile
                          multiple-values? continue)
  (must-be-list-of-at-least-length forms 1 "WHEN: bad syntax")
  (compile-forms forms module-vars bound-vars really-compile
                 (if (null (cdr forms)) nil multiple-values?)
                 (lambda (forms module-vars)
                   (funcall continue
                            `(if ,(car forms)
                                 (progn ,@(if (null (cdr forms))
                                              (list nil)
                                              (cdr forms)))
                                 nil)
                            module-vars))))

(defun compile-while-form (forms module-vars bound-vars really-compile
                           multiple-values? continue)
  (must-be-list-of-at-least-length forms 1 "WHILE: bad syntax")
  (compile-forms forms module-vars bound-vars really-compile
                 (if (null (cdr forms)) nil multiple-values?)
                 (lambda (forms module-vars)
                   (funcall continue
                            `(dylan%while (lambda () ,(car forms))
                                          (lambda ()
                                            ,@(if (null (cdr forms))
                                                  (list nil)
                                                  (cdr forms))))
                            module-vars))))
