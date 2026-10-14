module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; Functions for sequences (chapter 12).  The default methods make their
;;; results instances of the class-for-copy of the (first) sequence
;;; argument, through a list of the elements.

(define %elements
  (method (s)
    (for ((state (initial-state s) (next-state s state))
          (l '() (cons (current-element s state) l)))
         ((not state) (%list-reverse l)))))

(define %copy-like
  (method (s elements)
    (%from-list (class-for-copy s) elements)))

;;; Adding

(define-generic-function add (sequence new-element))
(define-method add ((s <sequence>) new-element)
  (%copy-like s (cons new-element (%elements s))))

(define-generic-function add! (sequence new-element))
(define-method add! ((s <sequence>) new-element) (add s new-element))

;;; The test is called with an element of the sequence first.
(define %member-by?
  (method (test s value)
    (any? (method (e) (test e value)) s)))

(define-generic-function add-new (sequence new-element #key test))
(define-method add-new ((s <sequence>) new-element #key (test id?))
  (if (%member-by? test s new-element) s (add s new-element)))

(define-generic-function add-new! (sequence new-element #key test))
(define-method add-new! ((s <sequence>) new-element #key (test id?))
  (if (%member-by? test s new-element) s (add! s new-element)))

;;; Removing

(define %remove-elements
  (method (elements value test count)
    (bind-methods ((loop (l left result)
                     (cond ((id? l '()) (%list-reverse result))
                           ((and (or (not left) (binary< 0 left)) (test (car l) value))
                            (loop (cdr l) (and left (binary- left 1)) result))
                           (else: (loop (cdr l) left (cons (car l) result))))))
      (loop elements count '()))))

(define-generic-function remove (sequence value #key test count))
(define-method remove ((s <sequence>) value #key (test id?) count)
  (%copy-like s (%remove-elements (%elements s) value test count)))

(define-generic-function remove! (sequence value #key test count))
(define-method remove! ((s <sequence>) value #key (test id?) count)
  (remove s value test: test count: count))

(define-generic-function choose (predicate sequence))
(define-method choose ((predicate <function>) (s <sequence>))
  (%copy-like s (bind-methods ((loop (l r)
                                 (cond ((id? l '()) (%list-reverse r))
                                       ((predicate (car l)) (loop (cdr l) (cons (car l) r)))
                                       (else: (loop (cdr l) r)))))
                  (loop (%elements s) '()))))

(define-generic-function choose-by (predicate test-sequence value-sequence))
(define-method choose-by ((predicate <function>) (tests <sequence>) (values <sequence>))
  (bind ((chosen '()))
    (do (method (t v) (when (predicate t) (set! chosen (cons v chosen)))) tests values)
    (%copy-like values (%list-reverse chosen))))

(define-generic-function intersection (sequence1 sequence2 #key test))
(define-method intersection ((s1 <sequence>) (s2 <sequence>) #key (test id?))
  (choose (method (e) (any? (method (f) (test e f)) s2)) s1))

(define-generic-function union (sequence1 sequence2 #key test))
(define-method union ((s1 <sequence>) (s2 <sequence>) #key (test id?))
  (concatenate s1 (choose (method (f) (not (any? (method (e) (test e f)) s1))) s2)))

(define-generic-function remove-duplicates (sequence #key test))
(define-method remove-duplicates ((s <sequence>) #key (test id?))
  (%copy-like s (bind-methods ((loop (l r)
                                 (cond ((id? l '()) (%list-reverse r))
                                       ((any? (method (kept) (test kept (car l))) r)
                                        (loop (cdr l) r))
                                       (else: (loop (cdr l) (cons (car l) r))))))
                  (loop (%elements s) '()))))

(define-generic-function remove-duplicates! (sequence #key test))
(define-method remove-duplicates! ((s <sequence>) #key (test id?))
  (remove-duplicates s test: test))

;;; Copying and joining

(define-generic-function copy-sequence (source #key start end))
(define-method copy-sequence ((s <sequence>) #key (start 0) end)
  (%copy-like s (for ((state (initial-state s) (next-state s state))
                      (index 0 (binary+ index 1))
                      (l '() (if (binary< index start) l (cons (current-element s state) l))))
                     ((or (not state) (and end (not (binary< index end))))
                      (%list-reverse l)))))

(define-generic-function concatenate-as (class sequence1 #rest more-sequences))
(define-method concatenate-as ((class <class>) (s <sequence>) #rest more)
  (bind ((sequences (cons s more)))
    (bind-methods ((gather (sequences r)
                     (if (id? sequences '())
                         (%list-reverse r)
                         (gather (cdr sequences)
                                 (reduce (method (r e) (cons e r)) r (car sequences))))))
      (if (%vectors-for? class sequences)
          (%concatenate-vectors class sequences)
          (%from-list class (gather sequences '()))))))

;;; Byte strings into a byte string, or vectors and strings into a simple
;;; object vector, are copied directly.
(define %vectors-for?
  (method (class sequences)
    (cond ((id? class <byte-string>)
           (every? (method (s) (instance? s <byte-string>)) sequences))
          ((id? class <simple-object-vector>)
           (every? (method (s) (or (instance? s <simple-object-vector>)
                                   (instance? s <byte-string>)
                                   (instance? s <unicode-string>)))
                   sequences))
          (else: #f))))

(define %concatenate-vectors
  (method (class sequences)
    (bind ((n (for ((l sequences (cdr l))
                    (n 0 (binary+ n (%vector-size (car l)))))
                   ((id? l '()) n)))
           (result (if (id? class <byte-string>)
                       (%make-byte-string n #\space)
                       (%make-vector n #f))))
      (for ((l sequences (cdr l))
            (i 0 (binary+ i (%vector-size (car l)))))
           ((id? l '()) result)
        (%replace-vector! result i (car l) 0 (%vector-size (car l)))))))

(define-generic-function concatenate (sequence1 #rest sequences))
(define-method concatenate ((s <sequence>) #rest more)
  (apply concatenate-as (class-for-copy s) s more))

;;; Byte strings, most often, at once
(define-method concatenate ((s <byte-string>) #rest more)
  (bind ((class (class-for-copy s)))
    (or (and (id? class <byte-string>) (%concatenate-byte-strings s more))
        (apply concatenate-as class s more))))

(define-generic-function replace-subsequence! (mutable-sequence insert-sequence #key start))
(define-method replace-subsequence! ((target <mutable-sequence>) (insert <sequence>)
                                     #key (start 0))
  (bind ((state (for ((state (initial-state target) (next-state target state))
                      (index 0 (binary+ index 1)))
                     ((or (not state) (= index start)) state))))
    (for-each ((e insert))
              ()
      (unless state
        (error "replace-subsequence!: the sequence is too short"))
      (set! (current-element target state) e)
      (set! state (next-state target state)))
    target))

;;; Reversing and sorting

(define-generic-function reverse (sequence))
(define-method reverse ((s <sequence>)) (%copy-like s (%list-reverse (%elements s))))

(define-generic-function reverse! (sequence))
(define-method reverse! ((s <sequence>)) (reverse s))

;;; A stable merge sort of V, a fresh simple object vector: runs of
;;; WIDTH merged into runs twice as long, from one vector to the other, an
;;; element of the first run going first when neither is less.  Answers
;;; the vector sorted.
(define %sort-vector
  (method (v test)
    (bind ((n (size v)) (from v) (to (%make-vector n #f)) (less? (id? test binary<)))
      (for ((width 1 (binary* width 2)))
           ((not (binary< width n)) from)
        (for ((low 0 (binary+ low (binary* 2 width))))
             ((not (binary< low n)))
          (bind ((middle (if (binary< (binary+ low width) n) (binary+ low width) n))
                 (high (if (binary< (binary+ middle width) n) (binary+ middle width) n))
                 (i low)
                 (j middle))
            (for ((k low (binary+ k 1)))
                 ((id? k high))
              (if (and (binary< j high)
                       (or (not (binary< i middle))
                           ;; binary< called directly is done inline on numbers
                           (if less?
                               (binary< (element from j) (element from i))
                               (test (element from j) (element from i)))))
                  (begin (set! (element to k) (element from j)) (set! j (binary+ j 1)))
                  (begin (set! (element to k) (element from i)) (set! i (binary+ i 1)))))))
        (bind ((t from)) (set! from to) (set! to t))))))

;;; The elements of the sequence S in a fresh simple object vector
(define %elements-vector
  (method (s)
    (bind ((l (%elements s))
           (v (%make-vector (%list-length l) #f)))
      (for ((l l (cdr l)) (i 0 (binary+ i 1)))
           ((id? l '()) v)
        (set! (element v i) (car l))))))

(define-generic-function sort (sequence #key test stable))
(define-method sort ((s <sequence>) #key (test <) stable)
  (bind ((v (%sort-vector (%elements-vector s) (if (id? test <) binary< test)))
         (class (class-for-copy s)))
    (if (id? class <simple-object-vector>)
        v
        (%from-list class (for ((i (binary- (size v) 1) (binary- i 1))
                                (l '() (cons (element v i) l)))
                               ((binary< i 0) l))))))

(define-generic-function sort! (sequence #key test stable))
(define-method sort! ((s <sequence>) #key (test <) stable)
  (sort s test: test))

;;; Particular elements

(define-generic-function first (sequence))
(define-method first ((s <sequence>)) (element s 0))
(define-generic-function second (sequence))
(define-method second ((s <sequence>)) (element s 1))
(define-generic-function third (sequence))
(define-method third ((s <sequence>)) (element s 2))

(define-generic-function (setter first) (sequence new-value))
(define-method (setter first) ((s <mutable-sequence>) new-value) (set! (element s 0) new-value))
(define-generic-function (setter second) (sequence new-value))
(define-method (setter second) ((s <mutable-sequence>) new-value) (set! (element s 1) new-value))
(define-generic-function (setter third) (sequence new-value))
(define-method (setter third) ((s <mutable-sequence>) new-value) (set! (element s 2) new-value))

(define-generic-function last (sequence))
(define-method last ((s <sequence>))
  (bind ((state (initial-state s)))
    (unless state (error "last: the sequence is empty"))
    (for ((state state (next-state s state))
          (previous state state))
         ((not state) (current-element s previous)))))

(define-generic-function (setter last) (sequence new-value))
(define-method (setter last) ((s <mutable-sequence>) new-value)
  (set! (element s (binary- (size s) 1)) new-value))

;;; Searching: the index of the COUNTth place PATTERN occurs in BIG.

(define-generic-function subsequence-position (big pattern #key test count))
(define-method subsequence-position ((big <sequence>) (pattern <sequence>)
                                     #key (test id?) (count 1))
  (bind ((b (%elements big))
         (p (%elements pattern)))
    (bind-methods ((matches? (b p)
                     (cond ((id? p '()) #t)
                           ((id? b '()) #f)
                           ((test (car b) (car p)) (matches? (cdr b) (cdr p)))
                           (else: #f)))
                   (search (b index left)
                     (cond ((matches? b p)
                            (if (= left 1) index (search (cdr b) (binary+ index 1) (binary- left 1))))
                           ((id? b '()) #f)
                           (else: (search (cdr b) (binary+ index 1) left)))))
      (search b 0 count))))

;;; A shallow copy of a sequence is a copy of it.
(define-method shallow-copy ((s <sequence>)) (copy-sequence s))

;;; apply's last argument may be any sequence (chapter 8); the runtime
;;; takes lists and simple vectors itself, and asks for the others' elements.
(%set-sequence-hook
  (method (s)
    (if (instance? s <sequence>)
        (as <list> s)
        (error "apply: the last argument is not a sequence: ~s" s))))
