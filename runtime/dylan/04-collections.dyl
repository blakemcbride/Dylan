module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; Collections (chapter 12): the protocol, and the default methods that
;;; work for any collection through it.  The concrete classes are in the
;;; files after this one.

;;; Small helpers on lists, which the generic functions below are built
;;; on (and so must not use).

(define %list-map
  (method (f l)
    (if (id? l '()) '() (cons (f (car l)) (%list-map f (cdr l))))))

(define %list-map2
  (method (f l1 l2)
    (if (or (id? l1 '()) (id? l2 '()))
        '()
        (cons (f (car l1) (car l2)) (%list-map2 f (cdr l1) (cdr l2))))))

(define %list-every?
  (method (f l)
    (cond ((id? l '()) #t)
          ((f (car l)) (%list-every? f (cdr l)))
          (else: #f))))

(define %list-reverse
  (method (l)
    (bind-methods ((loop (l r) (if (id? l '()) r (loop (cdr l) (cons (car l) r)))))
      (loop l '()))))

(define %list-length
  (method (l)
    (bind-methods ((loop (l n) (if (id? l '()) n (loop (cdr l) (binary+ n 1)))))
      (loop l 0))))

;;; The iteration protocol

(define-generic-function initial-state (collection))
(define-generic-function next-state (collection state))
(define-generic-function current-element (collection state))
(define-generic-function (setter current-element) (collection state new-value))
(define-generic-function current-key (collection state))
(define-generic-function final-state (collection))
(define-generic-function previous-state (collection state))

(define-generic-function copy-state (collection state))
(define-method copy-state ((c <collection>) state) state)

;;; Keys

(define-generic-function key-sequence (collection))

(define-method key-sequence ((c <explicit-key-collection>))
  (for ((state (initial-state c) (next-state c state))
        (keys '() (cons (current-key c state) keys)))
       ((not state) keys)))

(define-method key-sequence ((c <sequence>))
  (bind ((n (size c)))
    (if n (range up-to: n) (range))))

(define-generic-function element (collection key #key default))

(define-method element ((c <explicit-key-collection>) key #key (default %no-default))
  (for ((state (initial-state c) (next-state c state)))
       ((or (not state) (= (current-key c state) key))
        (cond (state (current-element c state))
              ((id? default %no-default) (error "element: there is no element for the key ~s" key))
              (else: default)))))

(define-method element ((c <sequence>) key #key (default %no-default))
  (for ((state (initial-state c) (next-state c state))
        (k 0 (binary+ k 1)))
       ((or (not state) (= k key))
        (cond (state (current-element c state))
              ((id? default %no-default) (error "element: there is no element at the index ~s" key))
              (else: default)))))

(define-generic-function (setter element) (collection key new-value))

(define-method (setter element) ((c <mutable-explicit-key-collection>) key new-value)
  (for ((state (initial-state c) (next-state c state)))
       ((or (not state) (= (current-key c state) key))
        (if state
            (set! (current-element c state) new-value)
            (error "element: there is no element for the key ~s" key)))))

(define-method (setter element) ((c <mutable-sequence>) key new-value)
  (for ((state (initial-state c) (next-state c state))
        (k 0 (binary+ k 1)))
       ((or (not state) (= k key))
        (if state
            (set! (current-element c state) new-value)
            (error "element: there is no element at the index ~s" key)))))

;;; Size and the like

(define-generic-function size (collection))
(define-method size ((c <collection>))
  (for ((state (initial-state c) (next-state c state))
        (n 0 (binary+ n 1)))
       ((not state) n)))

(define-generic-function empty? (collection))
(define-method empty? ((c <collection>)) (not (initial-state c)))

(define-generic-function class-for-copy (object))
(define-method class-for-copy ((x <object>)) (object-class x))

;;; A new collection of CLASS holding the elements of the list ELEMENTS.

(define %from-list
  (method (class elements)
    (if (subclass? class <list>)
        elements
        (bind ((result (make class size: (%list-length elements))))
          (for ((state (initial-state result) (next-state result state))
                (l elements (cdr l)))
               ((or (not state) (id? l '())) result)
            (set! (current-element result state) (car l)))))))

;;; Mapping (chapter 12, "Collection Alignment").  Sequences are walked
;;; together; any other collections are aligned by the intersection of
;;; their keys.  FUNCTION is called with the list of each group's
;;; elements.

(define %each-group
  (method (collections function)
    (if (%list-every? (method (c) (instance? c <sequence>)) collections)
        (bind-methods ((loop (states)
                         (when (%list-every? (method (s) s) states)
                           (function (%list-map2 current-element collections states))
                           (loop (%list-map2 next-state collections states)))))
          (loop (%list-map initial-state collections)))
        (for-each ((key (%common-keys collections)))
                  ()
          (function (%list-map (method (c) (element c key)) collections))))))

(define %common-keys
  (method (collections)
    (bind-methods ((loop (keys more)
                     (if (id? more '())
                         keys
                         (loop (intersection keys (key-sequence (car more))) (cdr more)))))
      (loop (key-sequence (car collections)) (cdr collections)))))

(define-generic-function do (procedure collection #rest more-collections))

(define-method do ((f <function>) (c <collection>) #rest more)
  (if (id? more '())
      (for ((state (initial-state c) (next-state c state)))
           ((not state) #f)
        (f (current-element c state)))
      (%each-group (cons c more) (method (arguments) (apply f arguments))))
  #f)

(define-generic-function map (procedure collection #rest more-collections))

(define-method map ((f <function>) (c <collection>) #rest more)
  (apply map-as (class-for-copy c) f c more))

(define-generic-function map-as (class procedure collection #rest more-collections))

(define-method map-as ((class <class>) (f <function>) (c <collection>) #rest more)
  (bind ((collections (cons c more)))
    ;; A vector is a sequence, though also (as an array) an explicit-key
    ;; collection: sequences first.
    (if (not (subclass? class <sequence>))
        (bind ((result (make class)))
          (for-each ((key (%common-keys collections)))
                    ()
            (set! (element result key)
                  (apply f (%list-map (method (c) (element c key)) collections))))
          result)
        (bind ((results '()))
          (%each-group collections
                       (method (arguments) (set! results (cons (apply f arguments) results))))
          (%from-list class (%list-reverse results))))))

(define-generic-function map-into (mutable-collection procedure collection #rest more-collections))

(define-method map-into ((target <mutable-collection>) (f <function>) (c <collection>) #rest more)
  (bind ((collections (cons c more)))
    (if (and (instance? target <sequence>)
             (%list-every? (method (c) (instance? c <sequence>)) collections))
        ;; The values first: TARGET may be one of the collections.
        (bind ((results '()))
          (%each-group collections
                       (method (arguments) (set! results (cons (apply f arguments) results))))
          (for ((state (initial-state target) (next-state target state))
                (l (%list-reverse results) (cdr l)))
               ((or (not state) (id? l '())) target)
            (set! (current-element target state) (car l))))
        (bind ((keys (intersection (key-sequence target) (%common-keys collections))))
          (for-each ((key keys))
                    ()
            (set! (element target key)
                  (apply f (%list-map (method (c) (element c key)) collections))))
          target))))

(define-generic-function any? (procedure collection #rest more-collections))

(define-method any? ((f <function>) (c <collection>) #rest more)
  (bind-exit (return)
    (if (id? more '())
        (for ((state (initial-state c) (next-state c state)))
             ((not state) #f)
          (bind ((v (f (current-element c state))))
            (when v (return v))))
        (%each-group (cons c more)
                     (method (arguments)
                       (bind ((v (apply f arguments)))
                         (when v (return v))))))
    #f))

(define-generic-function every? (procedure collection #rest more-collections))

(define-method every? ((f <function>) (c <collection>) #rest more)
  (bind-exit (return)
    (if (id? more '())
        (for ((state (initial-state c) (next-state c state)))
             ((not state) #f)
          (unless (f (current-element c state)) (return #f)))
        (%each-group (cons c more)
                     (method (arguments)
                       (unless (apply f arguments) (return #f)))))
    #t))

(define-generic-function reduce (procedure initial-value collection))

(define-method reduce ((f <function>) initial-value (c <collection>))
  (for ((state (initial-state c) (next-state c state))
        (value initial-value (f value (current-element c state))))
       ((not state) value)))

(define-generic-function reduce1 (procedure collection))

(define-method reduce1 ((f <function>) (c <collection>))
  (bind ((state (initial-state c)))
    (unless state (error "reduce1: the collection is empty"))
    (for ((state (next-state c state) (next-state c state))
          (value (current-element c state) (f value (current-element c state))))
         ((not state) value))))

(define-generic-function member? (value collection #key test))

(define-method member? (value (c <collection>) #key (test id?))
  (for ((state (initial-state c) (next-state c state)))
       ((or (not state) (test value (current-element c state)))
        (if state #t #f))))

(define-generic-function find-key (collection procedure #key skip failure))

(define-method find-key ((c <collection>) (f <function>) #key (skip 0) (failure #f))
  (bind ((sequence? (instance? c <sequence>)))
    (for ((state (initial-state c) (next-state c state))
          (index 0 (binary+ index 1))
          (skip skip (if (f (current-element c state)) (binary- skip 1) skip)))
         ((or (not state)
              (and (f (current-element c state)) (zero? skip)))
          (cond ((not state) failure)
                (sequence? index)
                (else: (current-key c state)))))))

(define-generic-function replace-elements! (mutable-collection predicate new-value-fn #key count))

(define-method replace-elements! ((c <mutable-collection>) (predicate <function>)
                                  (new-value-fn <function>) #key count)
  (for ((state (initial-state c) (next-state c state))
        (left count left))
       ((or (not state) (and left (zero? left))) c)
    (bind ((e (current-element c state)))
      (when (predicate e)
        (set! (current-element c state) (new-value-fn e))
        (when left (set! left (binary- left 1)))))))

(define-generic-function fill! (mutable-collection value #key start end))

(define-method fill! ((c <mutable-collection>) value #key (start 0) end)
  (if (instance? c <sequence>)
      (for ((state (initial-state c) (next-state c state))
            (index 0 (binary+ index 1)))
           ((or (not state) (and end (not (binary< index end)))) c)
        (unless (binary< index start)
          (set! (current-element c state) value)))
      (for ((state (initial-state c) (next-state c state)))
           ((not state) c)
        (set! (current-element c state) value))))

;;; Equality (chapter 9): collections are = when they have the same keys
;;; and the elements of the same key are =.

(define-method binary= ((x <collection>) (y <collection>))
  (and (= (size x) (size y))
       (every? (method (key)
                 (bind ((other (element y key default: %no-default)))
                   (and (not (id? other %no-default)) (= (element x key) other))))
               (key-sequence x))))

(define-method binary= ((x <sequence>) (y <sequence>))
  (bind-exit (return)
    (for ((sx (initial-state x) (next-state x sx))
          (sy (initial-state y) (next-state y sy)))
         ((or (not sx) (not sy)) (and (not sx) (not sy)))
      (unless (= (current-element x sx) (current-element y sy))
        (return #f)))))

(define-method =hash ((s <sequence>))
  (for ((state (initial-state s) (next-state s state))
        (n 0 (binary+ n 1))
        (h 17 (%logand (%logxor (binary* h 31) (=hash (current-element s state)))
                       1152921504606846975)))
       ((or (not state) (= n 8)) h)))

;;; Conversion of collections by as: to a sequence class, the elements in
;;; their natural order.

(define-method as ((class <class>) (c <collection>))
  (cond ((instance? c class) c)
        ((subclass? class <sequence>) (map-as class identity c))
        (else: (error "as: ~s cannot be converted to ~s" c class))))
