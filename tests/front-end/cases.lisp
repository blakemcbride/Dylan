;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Test cases for the front end.  Each is
;;;;
;;;;   (name "dylan source" expected)
;;;;
;;;; where EXPECTED is the core forms the source converts to, as UNPARSE
;;;; writes them, one per line -- or (:diagnostics "text" ...), the
;;;; diagnostics the source must produce, in order, each given as a string
;;;; it must contain.  The expected core forms follow the manual's
;;;; description of each form; the chapter is noted where it decides.

(
 ;; Chapter 3: literals evaluate to themselves; () and vectors included.
 (literals
  "\"abc\" 123 1/2 1.5 foo: #\\a #t #f () #(1 2) 'foo '(1 2)"
  "\"abc\"
123
1/2
1.5
foo:
#\\a
#t
#f
()
#(1 2)
'foo
'(1 2)")

 ;; Version 1 has 63-bit integers and no complex numbers.
 (no-bignums
  "4611686018427387903 -4611686018427387904 4611686018427387904"
  (:diagnostics "1:42: error: the integer 4611686018427387904 does not fit"))
 (no-bignum-ratios
  "'(1 1/4611686018427387904)"
  (:diagnostics "1:1: error: the integer 4611686018427387904 does not fit"))
 (no-complex
  "#c(1 2)"
  (:diagnostics "error: complex numbers are not supported"))

 ;; Variables resolve to the module that owns them.
 (variables
  "(define x 1) x + (setter element) (bind ((x 2)) x)"
  "(define x@dylan-user 1)
x@dylan-user
+@dylan
(setter element)@dylan
(bind ((x_1 2)) x_1)")

 (if
  "(if 1 2 3)"
  "(if 1
    2
    3)")
 (if-arity
  "(if 1 2)"
  (:diagnostics "1:1: error: if: needs a test, a consequent and an alternative"))

 ;; Chapter 8: begin of nothing is #f.
 (begin
  "(list (begin) (begin (f) 2) (begin 1 (f)) (begin (f) (g)))"
  "(list@dylan #f (begin (f@dylan-user) 2) (f@dylan-user) (begin (f@dylan-user) (g@dylan-user)))"
  )

 ;; Chapter 7: set! of a variable, and the extended form, which calls the
 ;; setter with the arguments and then the value, and answers the value.
 (set!
  "(define v 1) (set! v 2) (bind ((a 1)) (set! a 3)) (set! (element v 0) 5)"
  "(define v@dylan-user 1)
(set! v@dylan-user 2)
(bind ((a_1 1)) (set! a_1 3))
(bind ((arg_2 v@dylan-user))
 (bind ((value_3 5)) (begin ((setter element)@dylan arg_2 0 value_3) value_3)))")
 (set!-order
  "(set! (element (f) (g)) (h))"
  "(bind ((arg_1 (f@dylan-user)))
 (bind ((arg_2 (g@dylan-user)))
  (bind ((value_3 (h@dylan-user))) (begin ((setter element)@dylan arg_1 arg_2 value_3) value_3))))")
 (set!-bad
  "(set! 3 4)"
  (:diagnostics "error: set!: bad place 3"))

 ;; Chapter 4: bind binds one after another; a typed variable's type is
 ;; evaluated before the init and the value checked against it; #rest
 ;; takes the remaining values.
 (bind
  "(bind ((x 1) (y x)) y)"
  "(bind ((x_1 1)) (bind ((y_2 x_1)) y_2))")
 ;; Without parentheses, a type is just another variable.
 (bind-two
  "(bind ((x <integer> 1)) x)"
  "(bind ((x_1 <integer>_2 1)) x_1)")
 (bind-typed
  "(bind (((x <integer>) y (f))) x)"
  "(bind ((x-type_1 <integer>@dylan))
 (bind ((x_2 y_3 (f@dylan-user))) (begin (%primitive check-type x_2 x-type_1) x_2)))")
 (bind-rest
  "(bind ((a #rest r (f))) r)"
  "(bind ((a_1 #rest r_2 (f@dylan-user))) r_2)")
 (bind-empty
  "(bind () 1) (bind ())"
  "1
#f")

 ;; Chapter 8: bind-exit.
 (bind-exit
  "(bind-exit (done) (done 1) 2)"
  "(bind-exit (done_1) (begin (done_1 1) 2))")

 (unwind-protect
  "(unwind-protect (f) (g) (h))"
  "(unwind-protect (f@dylan-user) (begin (g@dylan-user) (h@dylan-user)))")

 ;; Chapter 6: parameter lists.
 (method
  "(method ((a <integer>) b #rest r #key c (d 4) (e: f (+ d 1))) (list a b r c d f))"
  "(method ((a_1 <integer>@dylan) b_2 #rest r_3 #key (c: c_4 #f) (d: d_5 4) (e: f_6 (+@dylan d_5 1)))
 (list@dylan a_1 b_2 r_3 c_4 d_5 f_6))")
 (method-key-any
  "(method (#key) 1) (method () )"
  "(method (#key) 1)
(method nil #f)")
 (method-next
  "(method (a #next n) (n a))"
  "(method (a_1 #next n_2) (n_2 a_1))")
 (method-duplicate
  "(method (a a) 1)"
  (:diagnostics "error: method: a appears twice"))

 ;; define-method binds next-method unless the parameters name #next.
 (define-method
  "(define-method m ((x <integer>)) (next-method x)) (define-method m (x #next nm) x)"
  "(define-method m@dylan-user ((x_1 <integer>@dylan) #next next-method_2) (next-method_2 x_1))
(define-method m@dylan-user (x_3 #next nm_4) x_3)")

 (define-generic-function
  "(define-generic-function g (a b #rest r #key c d:) foo: 1) (define-generic-function h (a #key))"
  "(define-generic-function g@dylan-user required: 2 rest: #t keys: '(c: d:))
(define-generic-function h@dylan-user required: 1 rest: #f keys: any)")

 (bind-methods
  "(bind-methods ((ev? (n) (if (= n 0) #t (od? (- n 1)))) (od? (n) (if (= n 0) #f (ev? (- n 1))))) (ev? 10))"
  "(bind-methods
 ((ev?_1 (n_3)
   (if (=@dylan n_3 0)
       #t
       (od?_2 (-@dylan n_3 1))))
  (od?_2 (n_4)
   (if (=@dylan n_4 0)
       #f
       (ev?_1 (-@dylan n_4 1)))))
 (ev?_1 10))")

 ;; Chapter 11: slots.
 (define-class
  "(define-class <p> (<object>) x (y init-value: 0 init-keyword: y:) (getter: z setter: #f type: <integer> required-init-keyword: z:) (w allocation: class init-function: f))"
  "(define-class <p>@dylan-user (<object>@dylan) (x@dylan-user setter: (setter x)@dylan-user)
 (y@dylan-user setter: (setter y)@dylan-user init-value: 0 init-keyword: y:)
 (z@dylan-user type: <integer>@dylan required-init-keyword: z:)
 (w@dylan-user setter: (setter w)@dylan-user init-function: f@dylan-user allocation: class))")
 (define-class-bad-slot
  "(define-class <p> (<object>) (x init-value: 1 init-function: f))"
  (:diagnostics "error: slot x: both init-value: and init-function:"))
 (define-slot
  "(define-class <p> (<object>)) (define-slot <p> q init-value: 1)"
  "(define-class <p>@dylan-user (<object>@dylan))
(define-slot <p>@dylan-user q@dylan-user setter: (setter q)@dylan-user init-value: 1)")

 ;; Chapter 8 macros.
 (and-or
  "(and) (and 1) (and 1 2 3) (or) (or 1) (or 1 2)"
  "#t
1
(if 1
    (if 2
        3
        #f)
    #f)
#f
1
(bind ((or_1 1))
 (if or_1
     or_1
     2))")
 (when-unless
  "(when 1 (f) 3) (unless 1 2) (when 1)"
  "(if 1
    (begin (f@dylan-user) 3)
    #f)
(if 1
    #f
    2)
(if 1
    #f
    #f)")
 (cond
  "(cond ((f) 1) ((g)) (else: 2))"
  "(if (f@dylan-user)
    1
    (bind ((test_1 (g@dylan-user)))
     (if test_1
         test_1
         (if else:
             2
             #f))))")
 (case
  "(case (f) ((1 2) 'a) ((x)) (else: 'c))"
  "(bind ((key_1 (f@dylan-user)))
 (if (bind ((or_2 (id?@dylan key_1 1)))
      (if or_2
          or_2
          (id?@dylan key_1 2)))
     'a
     (if (id?@dylan key_1 'x)
         #f
         'c)))")
 (case-no-else
  "(case 1 ((1) 'a))"
  "(bind ((key_1 1))
 (if (id?@dylan key_1 1)
     'a
     (error@dylan \"case: no clause matches ~s\" key_1)))")
 (case-duplicate
  "(case 1 ((1) 'a) ((1) 'b))"
  (:diagnostics "error: case: 1 appears in more than one match list"))
 (case-else-last
  "(case 1 (else: 'a) ((1) 'b))"
  (:diagnostics "error: case: the else: clause must be the last"))
 (select
  "(select (f) instance? ((<integer> <float>) 'n) (#t 'o))"
  "(bind ((target_1 (f@dylan-user)))
 (bind ((test_2 instance?@dylan))
  (if (bind ((or_3 (test_2 target_1 <integer>@dylan)))
       (if or_3
           or_3
           (test_2 target_1 <float>@dylan)))
      'n
      'o)))")
 (while
  "(while (f) (g))"
  "(bind-methods
 ((while_1 nil
   (if (f@dylan-user)
       (begin (g@dylan-user) (while_1))
       #f)))
 (while_1))")
 (until
  "(until (f))"
  "(bind-methods
 ((until_1 nil
   (if (f@dylan-user)
       #f
       (until_1))))
 (until_1))")
 (dotimes
  "(dotimes (i 3 i) (print i))"
  "(bind ((count_1 3))
 (bind-methods
  ((dotimes_2 (i_3)
    (if (<@dylan i_3 count_1)
        (begin (bind ((i_4 i_3)) (print@dylan i_4)) (dotimes_2 (+@dylan i_3 1)))
        (bind ((i_5 i_3)) i_5))))
  (dotimes_2 0)))")
 ;; The factorial of chapter 8.
 (for
  "(for ((i n (- i 1)) (v 1 (* v i))) ((<= i 0) v))"
  "(bind-methods
 ((for_1 (i_2 v_3)
   (if (<=@dylan i_2 0)
       v_3
       (for_1 (-@dylan i_2 1) (*@dylan v_3 i_2)))))
 (for_1 n@dylan-user 1))")
 (for-no-end
  "(for ((i 0 (+ i 1))) () (f i))"
  "(bind-methods ((for_1 (i_2) (begin (f@dylan-user i_2) (for_1 (+@dylan i_2 1))))) (for_1 0))")
 ;; first-even of chapter 8.
 (for-each
  "(for-each ((number s)) ((even? number) number))"
  "(bind ((collection_1 s@dylan-user))
 (bind-methods
  ((for-each_2 (state_3)
    (if state_3
        (bind ((number_4 (current-element@dylan collection_1 state_3)))
         (if (even?@dylan number_4)
             number_4
             (for-each_2 (next-state@dylan collection_1 state_3))))
        #f)))
  (for-each_2 (initial-state@dylan collection_1))))")

 ;; Chapter 13.
 (handler-bind
  "(handler-bind (<error> h test: t description: \"d\") (f))"
  "(%primitive handler-bind <error>@dylan h@dylan-user t@dylan-user \"d\" (method nil (f@dylan-user)))")
 (handler-case
  "(handler-case (f) ((<error> condition: c) (g c)) ((<warning>) 2))"
  "(bind-exit (done_1)
 (bind
  ((thunk_9
    (bind-exit (handle_2)
     (%primitive handler-bind <warning>@dylan
      (method (condition_3 next-handler_4) (handle_2 (method nil 2))) #f #f
      (method nil
       (%primitive handler-bind <error>@dylan
        (method (condition_5 next-handler_6)
         (handle_2 (method nil (bind ((c_7 condition_5)) (g@dylan-user c_7)))))
        #f #f
        (method nil (bind ((#rest values_8 (f@dylan-user))) (apply@dylan done_1 values_8)))))))))
  (thunk_9)))")

 ;; Backquote.
 (quasiquote
  "`(a ,b ,@c d) `(1 2) `#(x ,y) `(a . ,b) `(a `(b ,(c ,d)))"
  "(%primitive pair 'a (%primitive pair b@dylan-user (%primitive append2 c@dylan-user '(d))))
'(1 2)
(%primitive list->vector (%primitive pair 'x (%primitive pair y@dylan-user ())))
(%primitive pair 'a b@dylan-user)
(%primitive pair 'a
 (%primitive pair
  (%primitive pair 'quasiquote
   (%primitive pair
    (%primitive pair 'b
     (%primitive pair
      (%primitive pair 'unquote
       (%primitive pair (%primitive pair 'c (%primitive pair d@dylan-user ())) ()))
      ()))
    ()))
  ()))")
 (unquote-outside
  ",x"
  (:diagnostics "error: unquote (,) outside a backquote"))

 ;; Macros do not capture a program's variables, nor are their library
 ;; references changed by a program's bindings.
 (hygiene
  "(bind ((or_1 5) (id? 6)) (or (f) or_1) (case 1 ((1) id?)))"
  "(bind ((or_1_1 5))
 (bind ((id?_2 6))
  (begin
   (bind ((or_3 (f@dylan-user)))
    (if or_3
        or_3
        or_1_1))
   (bind ((key_4 1))
    (if (id?@dylan key_4 1)
        id?_2
        (error@dylan \"case: no clause matches ~s\" key_4))))))")

 ;; Chapter 5 and design note 32: modules.
 (modules
  "module: n

(define-module m (use dylan) (create x y))
(define-module n (use dylan) (use m import: (x (y => why))))
(define x 1)
(define why 2)
(list x why +)"
  "(define x@m 1)
(define y@m 2)
(list@dylan x@m y@m +@dylan)")
 (module-prefix-and-exclude
  "module: o

(define-module o (use dylan prefix: \"d-\" exclude: (+)) (use n))
(define-module n (use dylan) (export z))
(d-list d-+ z)"
  (:diagnostics "warning: d-+ is not defined (module o)"
                "warning: z is not defined (module n)"))
 (module-cycle
  "(define-module a (use b)) (define-module b (use a))"
  (:diagnostics "error: define-module: modules"))
 (module-not-exported
  "(define-module a (use dylan import: (no-such-thing)))"
  (:diagnostics "error: define-module: no-such-thing is not exported from dylan"))
 (define-imported
  "(define + 3)"
  (:diagnostics "1:1: error: define of +: the variable is imported from module dylan"))

 ;; Every special form, misused, is located where it is written.
 (locations
  "(define x 1)
(define-method f (x)
  (bind ((y x))
    (if y 1)))"
  (:diagnostics "4:5: error: if: needs a test"))

 (undefined
  "(f 1)"
  (:diagnostics "1:1: warning: f is not defined (module dylan-user)"))

 ;; The reader's errors are reported where it stopped.
 (read-error
  "(f 1"
  (:diagnostics "error: End of file inside a Dylan list"))
)
