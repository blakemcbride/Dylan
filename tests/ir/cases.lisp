;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Test cases for the IR (Phase 2).  Each is
;;;;
;;;;   (name "dylan source" "expected IR" check ...)
;;;;
;;;; The expected IR is what IR-STRING writes for each top-level form, one
;;;; per line.  A check is
;;;;
;;;;   (:var "name" property value ...)  -- the first variable so named
;;;;   (:fn "name" property value ...)   -- the fn so named, or
;;;;   (:fn n property value ...)        -- the n'th fn, counting from 0,
;;;;                                        outer before inner
;;;;
;;;; Variable properties: :captured :assigned :refs :calls :escapes (an
;;;; exit's) :fn (bound to a known fn).  Fn properties: :escapes
;;;; :call-sites :tail-call-sites :free (names of its free variables).

(
 ;; Operands are atoms, evaluated left to right; module variables are
 ;; read into temporaries.
 (operands
  "(f (g 1) (h 2))"
  "(let ((t_1 f@dylan-user))
  (let ((t_2 g@dylan-user))
    (let ((t_3 (t_2 1)))
      (let ((t_4 h@dylan-user))
        (let ((t_5 (t_4 2)))
          (tail-call t_1 t_3 t_5))))))")

 ;; An assigned variable is copied when used as an operand, so that a
 ;; later operand's assignment is not seen.
 (assigned-copy
  "(bind ((x 1)) (list x (begin (set! x 5) x)))"
  "(let ((x_1 1))
  (let ((t_2 list@dylan))
    (let ((t_3 x_1))
      (begin (set! x_1 5)
       (let ((t_4 x_1))
         (tail-call t_2 t_3 t_4))))))"
  (:var "x" :assigned t :captured nil :refs 2))

 ;; Tail calls: the last call of a method, not those whose values it
 ;; uses.
 (tail
  "(define-method fact (n) (if (< n 2) 1 (* n (fact (- n 1)))))"
  "(define-method fact@dylan-user (n_1 #next next-method_2)
 (let ((t_3 <@dylan))
   (let ((t_4 (t_3 n_1 2)))
     (if t_4
         1
         (let ((t_5 *@dylan))
           (let ((t_6 fact@dylan-user))
             (let ((t_7 -@dylan))
               (let ((t_8 (t_7 n_1 1)))
                 (let ((t_9 (t_6 t_8)))
                   (tail-call t_5 n_1 t_9))))))))))")

 ;; A loop is a local method calling itself in tail position; all its
 ;; call sites are known.  The first call is not a tail call: n follows
 ;; it.  The method refers to its own variable, so that is free in it.
 (loop
  "(define-method count-down (n) (while (> n 0) (set! n (- n 1))) n)"
  "(define-method count-down@dylan-user (n_1 #next next-method_2)
 (begin
  (letrec
   ((while_3
     (method while nil
      (let ((t_4 >@dylan))
        (let ((t_5 n_1))
          (let ((t_6 (t_4 t_5 0)))
            (if t_6
                (begin
                 (let ((t_7 -@dylan))
                   (let ((t_8 n_1))
                     (let ((t_9 (t_7 t_8 1)))
                       (set! n_1 t_9))))
                 (tail-call while_3))
                #f)))))))
   (while_3))
  n_1))"
  (:fn "while" :escapes nil :call-sites 2 :tail-call-sites 1 :free ("n" "while"))
  (:var "n" :assigned t :captured t))

 ;; An exit called only directly in its own function does not escape,
 ;; and the body's last call stays a tail call.
 (local-exit
  "(bind-exit (done) (if (g) (done 1) (f)))"
  "(bind-exit (done_1)
 (let ((t_2 g@dylan-user))
   (let ((t_3 (t_2)))
     (if t_3
         (exit-to done_1 1)
         (let ((t_4 f@dylan-user))
           (tail-call t_4))))))"
  (:var "done" :escapes nil :calls 1))

 ;; An exit called from a closure escapes; its body's calls are then not
 ;; tail calls.
 (escaping-exit
  "(bind-exit (done) (map (method (x) (if (zero? x) (done x) x)) (g)))"
  "(bind-exit (done_1)
 (let ((t_2 map@dylan))
   (let ((t_6
          (method (x_3)
           (let ((t_4 zero?@dylan))
             (let ((t_5 (t_4 x_3)))
               (if t_5
                   (exit-to done_1 x_3)
                   x_3))))))
     (let ((t_7 g@dylan-user))
       (let ((t_8 (t_7)))
         (t_2 t_6 t_8))))))"
  (:var "done" :escapes t :captured t)
  (:fn 1 :escapes t :free ("done")))

 ;; An exit used as a value escapes.
 (exit-as-value
  "(bind-exit (done) (f done))"
  "(bind-exit (done_1)
 (let ((t_2 f@dylan-user))
   (t_2 done_1)))"
  (:var "done" :escapes t))

 ;; Captured variables are free in every fn between their use and their
 ;; binding.
 (capture
  "(bind ((x 1) (y 2)) (method () (method () x)))"
  "(let ((x_1 1))
  (let ((y_2 2))
    (method nil (method nil x_1))))"
  (:var "x" :captured t :refs 1)
  (:var "y" :captured nil :refs 0)
  (:fn 1 :free ("x"))
  (:fn 2 :free ("x")))

 ;; A fn bound to an unassigned variable and only called is known; used
 ;; as a value, it escapes.
 (known-fn
  "(bind ((f (method (x) x))) (f 1) (f 2))"
  "(let ((f_2 (method (x_1) x_1)))
  (begin (f_2 1) (tail-call f_2 2)))"
  (:var "f" :fn t :calls 2)
  (:fn 1 :escapes nil :call-sites 2 :tail-call-sites 1))
 (escaping-fn
  "(bind ((f (method (x) x))) (list f))"
  "(let ((f_2 (method (x_1) x_1)))
  (let ((t_3 list@dylan))
    (tail-call t_3 f_2)))"
  (:fn 1 :escapes t :call-sites 0))
 (assigned-fn
  "(bind ((f (method () 1))) (set! f g) (f))"
  "(let ((f_1 (method nil 1)))
  (begin
   (let ((t_2 g@dylan-user))
     (set! f_1 t_2))
   (let ((t_3 f_1))
     (tail-call t_3))))"
  (:var "f" :fn nil :assigned t)
  (:fn 1 :escapes nil :call-sites 0))

 ;; Methods of generic functions escape: they are called by dispatch.
 (method-escapes
  "(define-method m (x) x)"
  "(define-method m@dylan-user (x_1 #next next-method_2) x_1)"
  (:fn 1 :escapes t))

 ;; Neither the protected form's calls nor the cleanup's are tail calls.
 (unwind-protect
  "(unwind-protect (f) (g))"
  "(unwind-protect
    (let ((t_1 f@dylan-user))
      (t_1))
  (let ((t_2 g@dylan-user))
    (t_2)))")

 ;; Multiple values.
 (mv-let
  "(bind ((a b #rest r (f))) (list a b r))"
  "(let ((t_1 f@dylan-user))
  (let-values ((a_2 b_3 #rest r_4 (t_1)))
   (let ((t_5 list@dylan))
     (tail-call t_5 a_2 b_3 r_4))))")

 ;; Specializers are evaluated where the method is made; keyword
 ;; defaults inside it, each seeing the parameters before it.
 (specializers-and-keys
  "(method ((x (singleton 3)) #key (y (f x))) (g x y))"
  "(let ((t_1 singleton@dylan))
  (let ((t_2 (t_1 3)))
    (method
     ((x_3 t_2) #key
      (y: y_5
       (let ((t_4 f@dylan-user))
         (t_4 x_3))))
     (let ((t_6 g@dylan-user))
       (tail-call t_6 x_3 y_5)))))"
  (:var "x" :captured nil :refs 2))

 ;; Local methods' specializers are evaluated before any is made.
 (letrec
  "(bind-methods ((a ((x <integer>)) (b x)) (b ((y <float>)) (a y))) (a 1))"
  "(let ((t_1 <integer>@dylan))
  (let ((t_2 <float>@dylan))
    (letrec
     ((a_3 (method a ((x_5 t_1)) (tail-call b_4 x_5)))
      (b_4 (method b ((y_6 t_2)) (tail-call a_3 y_6))))
     (tail-call a_3 1))))"
  (:fn "a" :escapes nil :call-sites 2)
  (:fn "b" :escapes nil :call-sites 1))

 ;; Definitions take atoms.
 (definitions
  "(define-class <p> (<object>) (x init-value: (f) type: <integer>)) (define v (g))"
  "(let ((t_1 <object>@dylan))
  (let ((t_2 <integer>@dylan))
    (let ((t_3 f@dylan-user))
      (let ((t_4 (t_3)))
        (define-class <p>@dylan-user (t_1)
         (x@dylan-user setter: (setter x)@dylan-user type: t_2 init-value: t_4))))))
(let ((t_5 g@dylan-user))
  (let ((t_6 (t_5)))
    (define v@dylan-user t_6)))")

 ;; handler-case's exits: done is applied, handle is called from the
 ;; handler, which is a closure.
 (handler-case
  "(handler-case (f) ((<error>) 1))"
  "(bind-exit (done_1)
 (let ((thunk_12
        (bind-exit (handle_2)
         (let ((t_3 <error>@dylan))
           (let ((t_7
                  (method (condition_4 next-handler_5)
                   (let ((t_6 (method nil 1)))
                     (exit-to handle_2 t_6)))))
             (let ((t_11
                    (method nil
                     (let ((t_8 f@dylan-user))
                       (let-values ((#rest values_9 (t_8)))
                        (let ((t_10 apply@dylan))
                          (tail-call t_10 done_1 values_9)))))))
               (%primitive handler-bind t_3 t_7 #f #f t_11)))))))
   (thunk_12)))"
  (:var "done" :escapes t)
  (:var "handle" :escapes t))
)
