module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; <table> (chapter 12, "Operations on Tables"): a hash table, its keys
;;; compared with = and hashed with =hash (chapter 9).  The buckets are a
;;; vector of lists of (key . value) pairs.  The iteration state is a list:
;;; the bucket index, and the rest of that bucket from the current pair on.

(define-class <table> (<mutable-explicit-key-collection>)
  (%buckets type: <simple-object-vector> init-function: (method () (%make-vector 16 '())))
  (%count type: <integer> init-value: 0))

(define-method make ((class (singleton <table>)) #rest arguments) (%make <table>))

(define %bucket-index
  (method (t key) (modulo (=hash key) (%vector-size (%buckets t)))))

(define %table-find
  (method (t key)
    (bind-methods ((walk (l)
                     (cond ((id? l '()) #f)
                           ((%element= (car (car l)) key) (car l))
                           (else: (walk (cdr l))))))
      (walk (%vector-element (%buckets t) (%bucket-index t key))))))

(define %table-grow
  (method (t)
    (bind ((old (%buckets t)))
      (when (binary< (binary* 2 (%vector-size old)) (%count t))
        (bind ((new (%make-vector (binary* 4 (%vector-size old)) '())))
          (set! (%buckets t) new)
          (dotimes (i (%vector-size old))
            (for-each ((entry (%vector-element old i))) ()
              (bind ((k (%bucket-index t (car entry))))
                (%vector-element-setter new k (cons entry (%vector-element new k)))))))))))

(define-method size ((t <table>)) (%count t))

(define %table-element
  (method (t key default)
    (bind ((entry (%table-find t key)))
      (cond (entry (cdr entry))
            ((id? default %no-default) (error "element: the table has no key ~s" key))
            (else: default)))))

(define-method element ((t <table>) key #key (default %no-default))
  (%table-element t key default))

;;; Setting the element of a key the table does not have adds it.
(define %table-element-setter
  (method (t key new-value)
    (bind ((entry (%table-find t key)))
      (if entry
          (%set-cdr! entry new-value)
          (begin
            (bind ((k (%bucket-index t key)))
              (%vector-element-setter (%buckets t) k
                                      (cons (cons key new-value) (%vector-element (%buckets t) k))))
            (set! (%count t) (binary+ (%count t) 1))
            (%table-grow t)
            new-value)))))

(define-method (setter element) ((t <table>) key new-value)
  (%table-element-setter t key new-value))

;;; The library's own (04-collections.dyl), for a direct instance
(define-method %library-size ((t <table>))
  (if (id? (%object-class t) <table>) (%count t) (size t)))
(define-method %library-element ((t <table>) key default)
  (if (id? (%object-class t) <table>) (%table-element t key default) (next-method)))
(define-method %library-element-setter ((t <table>) key new-value)
  (if (id? (%object-class t) <table>) (%table-element-setter t key new-value) (next-method)))

(define-generic-function remove-key! (table key))
(define-method remove-key! ((t <table>) key)
  (bind ((k (%bucket-index t key)))
    (bind ((bucket (%vector-element (%buckets t) k)))
      (when (%table-find t key)
        (%vector-element-setter (%buckets t) k
                                (choose (method (entry) (not (%element= (car entry) key))) bucket))
        (set! (%count t) (binary- (%count t) 1)))
      t)))

;;; Iteration

(define %table-from
  (method (t i)
    (bind ((buckets (%buckets t)))
      (bind-methods ((scan (i)
                       (cond ((not (binary< i (%vector-size buckets))) #f)
                             ((id? (%vector-element buckets i) '()) (scan (binary+ i 1)))
                             (else: (cons i (%vector-element buckets i))))))
        (scan i)))))

(define-method initial-state ((t <table>)) (%table-from t 0))
(define-method next-state ((t <table>) (state <pair>))
  (if (id? (cdr (cdr state)) '())
      (%table-from t (binary+ (car state) 1))
      (cons (car state) (cdr (cdr state)))))
(define-method current-element ((t <table>) (state <pair>)) (cdr (car (cdr state))))
(define-method current-key ((t <table>) (state <pair>)) (car (car (cdr state))))
(define-method (setter current-element) ((t <table>) (state <pair>) new-value)
  (%set-cdr! (car (cdr state)) new-value))

(define-method key-sequence ((t <table>))
  (for ((state (initial-state t) (next-state t state))
        (keys '() (cons (current-key t state) keys)))
       ((not state) keys)))

;;; as a table: the collection's keys, each with its element (chapter 11).
(define-method as ((class (singleton <table>)) (c <collection>))
  (if (%instance? c <table>)
      c
      (bind ((t (make <table>)))
        (for-each ((key (key-sequence c)))
                  ()
          (%library-element-setter t key (%library-element c key %no-default)))
        t)))

;;; map-into a table: the keys of the sources, which the table gains if it
;;; lacks them (chapter 12).
(define-method map-into ((t <table>) (f <function>) (c <collection>) #rest more)
  (bind ((collections (cons c more)))
    (for-each ((key (%common-keys collections)))
              ()
      (%library-element-setter
       t key
       (apply f (%list-map (method (c) (%library-element c key %no-default)) collections))))
    t))

(define-method shallow-copy ((t <table>))
  (bind ((copy (make <table>)))
    (for ((state (initial-state t) (next-state t state)))
         ((not state) copy)
      (%library-element-setter copy (current-key t state) (current-element t state)))))
