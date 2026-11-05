module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; Lists (chapter 12, "Operations on Lists").  cons, list, car and cdr
;;; are the runtime's.  A list's iteration state is the pair whose car is
;;; the current element.

;;; A list ends in ().  Walking onto any other tail that is not a pair, the
;;; list is improper: design note 16 makes that a <type-error>, of the tail.
(define %improper-list-tail (method (tail) (%runtime-type-error tail <list>)))

(define-method initial-state ((l <list>)) (if (%pair? l) l #f))
(define-method next-state ((l <list>) (state <pair>))
  (bind ((rest (cdr state)))
    (cond ((%pair? rest) rest)
          ((id? rest '()) #f)
          (else: (%improper-list-tail rest)))))
(define-method current-element ((l <list>) (state <pair>)) (car state))
(define-method (setter current-element) ((l <list>) (state <pair>) new-value)
  (%set-car! state new-value))

(define-method class-for-copy ((l <list>)) <list>)

;;; size: #f for a circular list (tortoise and hare).  An improper list
;;; has no size; design note 16 makes it a <type-error>, of the tail that
;;; is not a list.
(define-method size ((l <list>))
  (bind-methods ((end (tail n) (if (id? tail '()) n (%improper-list-tail tail)))
                 (loop (slow fast n)
                   (cond ((not (%pair? fast)) (end fast n))
                         ((not (%pair? (cdr fast))) (end (cdr fast) (binary+ n 1)))
                         ((and (binary< 0 n) (id? (cdr (cdr fast)) (cdr slow))) #f)
                         (else: (loop (cdr slow) (cdr (cdr fast)) (binary+ n 2))))))
    (if (%pair? l)
        (if (id? (cdr l) l) #f (loop l l 0))
        0)))

(define-method empty? ((l <list>)) (not (%pair? l)))

(define-method element ((l <list>) (index <integer>) #key (default %no-default))
  (bind-methods ((walk (l k)
                   (cond ((not (%pair? l))
                          (cond ((not (id? l '())) (%improper-list-tail l))
                                ((id? default %no-default)
                                 (error "element: there is no element at the index ~s" index))
                                (else: default)))
                         ((zero? k) (car l))
                         (else: (walk (cdr l) (binary- k 1))))))
    (if (negative? index)
        (walk '() 0)
        (walk l index))))

(define-method (setter element) ((l <list>) (index <integer>) new-value)
  (bind-methods ((walk (l k)
                   (cond ((not (%pair? l))
                          (if (id? l '())
                              (error "element: there is no element at the index ~s" index)
                              (%improper-list-tail l)))
                         ((zero? k) (%set-car! l new-value))
                         (else: (walk (cdr l) (binary- k 1))))))
    (walk l index)))

(define-method make ((class (singleton <list>)) #key (size 0) (fill #f))
  (unless (and (%instance? size <integer>) (not (negative? size)))
    (error "make: size: must be a non-negative integer: ~s" size))
  (for ((i 0 (binary+ i 1))
        (l '() (cons fill l)))
       ((not (binary< i size)) l)))

(define-method make ((class (singleton <empty-list>)) #rest arguments) '())

(define (setter car) (method (pair object) (%set-car! pair object)))
(define (setter cdr) (method (pair object) (%set-cdr! pair object)))

(define caar (method (l) (car (car l))))
(define cadr (method (l) (car (cdr l))))
(define cdar (method (l) (cdr (car l))))
(define cddr (method (l) (cdr (cdr l))))
(define caaar (method (l) (car (car (car l)))))
(define caadr (method (l) (car (car (cdr l)))))
(define cadar (method (l) (car (cdr (car l)))))
(define caddr (method (l) (car (cdr (cdr l)))))
(define cdaar (method (l) (cdr (car (car l)))))
(define cdadr (method (l) (cdr (car (cdr l)))))
(define cddar (method (l) (cdr (cdr (car l)))))
(define cdddr (method (l) (cdr (cdr (cdr l)))))

(define (setter caar) (method (l v) (%set-car! (car l) v)))
(define (setter cadr) (method (l v) (%set-car! (cdr l) v)))
(define (setter cdar) (method (l v) (%set-cdr! (car l) v)))
(define (setter cddr) (method (l v) (%set-cdr! (cdr l) v)))
(define (setter caaar) (method (l v) (%set-car! (car (car l)) v)))
(define (setter caadr) (method (l v) (%set-car! (car (cdr l)) v)))
(define (setter cadar) (method (l v) (%set-car! (cdr (car l)) v)))
(define (setter caddr) (method (l v) (%set-car! (cdr (cdr l)) v)))
(define (setter cdaar) (method (l v) (%set-cdr! (car (car l)) v)))
(define (setter cdadr) (method (l v) (%set-cdr! (car (cdr l)) v)))
(define (setter cddar) (method (l v) (%set-cdr! (cdr (car l)) v)))
(define (setter cdddr) (method (l v) (%set-cdr! (cdr (cdr l)) v)))

;;; list* : the last argument is the tail.
(define list*
  (method (#rest arguments)
    (bind-methods ((build (l)
                     (if (id? (cdr l) '())
                         (car l)
                         (cons (car l) (build (cdr l))))))
      (if (id? arguments '()) '() (build arguments)))))

;;; append: copies every list but the last, which the result shares.
(define-generic-function append (list #rest more-lists))
(define-method append ((l <list>) #rest more)
  (bind-methods ((join (lists)
                   (cond ((id? lists '()) '())
                         ((id? (cdr lists) '()) (car lists))
                         (else: (copy-onto (car lists) (join (cdr lists))))))
                 (copy-onto (l tail)
                   (cond ((%pair? l) (cons (car l) (copy-onto (cdr l) tail)))
                         ((id? l '()) tail)
                         (else: (%improper-list-tail l)))))
    (join (cons l more))))

(define-generic-function find-pair (item list #key test))
(define-method find-pair (item (l <list>) #key (test id?))
  (bind-methods ((walk (l)
                   (cond ((id? l '()) #f)
                         ((not (%pair? l)) (%improper-list-tail l))
                         ((not (%instance? (car l) <list>))
                          (error "find-pair: ~s is not a list" (car l)))
                         ((and (%pair? (car l)) (test item (car (car l)))) (car l))
                         (else: (walk (cdr l))))))
    (walk l)))

;;; For lists, member? answers the tail whose car matched.
(define-method member? (value (l <list>) #key (test id?))
  (bind-methods ((walk (l)
                   (cond ((id? l '()) #f)
                         ((not (%pair? l)) (%improper-list-tail l))
                         ((test value (car l)) l)
                         (else: (walk (cdr l))))))
    (walk l)))

(define-method add ((l <list>) new-element) (cons new-element l))
(define-method add! ((l <list>) new-element) (cons new-element l))

(define-method remove! ((l <list>) value #key (test id?) count)
  (%remove-elements l value test count))

(define-method reverse ((l <list>))
  (bind-methods ((loop (l r)
                   (cond ((%pair? l) (loop (cdr l) (cons (car l) r)))
                         ((id? l '()) r)
                         (else: (%improper-list-tail l)))))
    (loop l '())))

(define-method reverse! ((l <list>))
  (bind-methods ((loop (l r)
                   (cond ((%pair? l)
                          (bind ((next (cdr l))) (%set-cdr! l r) (loop next l)))
                         ((id? l '()) r)
                         (else: (%improper-list-tail l)))))
    (loop l '())))

;;; Dotted lists are = when their elements and final cdrs are.
(define-method binary= ((x <pair>) (y <pair>))
  (and (= (car x) (car y)) (= (cdr x) (cdr y))))
