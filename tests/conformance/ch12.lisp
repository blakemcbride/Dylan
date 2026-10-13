;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Chapter 12, "Collections", of the 1992 manual: its examples, with the
;;;; results it gives, and the functions and classes it documents without
;;;; examples, with the results its text specifies.  Where the manual
;;;; leaves an order unspecified (tables, union, intersection), the tests
;;;; sort or test membership.  Collections whose printed form the manual
;;;; does not define (stretchy vectors, deques, ranges, tables) are
;;;; printed as lists or by their elements.

(
 ;; "Functions for Collections": size, empty?, class-for-copy (from the
 ;; specification: size counts the keys; empty? answers whether there are
 ;; elements; class-for-copy of a sequence is a subclass of <sequence>, of
 ;; an explicit-key-collection a subclass of <explicit-key-collection>;
 ;; "Operations on Ranges": the class-for-copy of a range is <list>).
 (ch12-size-empty
  "(print (list (size '(1 2 3)) (size #(a b)) (size \"abcd\") (size '()) (size (vector))))
   (print (list (empty? '()) (empty? '(1)) (empty? #()) (empty? \"\") (empty? \"a\")))
   (print (list (subclass? (class-for-copy (vector 1 2)) <sequence>)
                (subclass? (class-for-copy (list 1 2)) <sequence>)
                (subclass? (class-for-copy (make <table>)) <explicit-key-collection>)
                (id? (class-for-copy (range from: 0 up-to: 3)) <list>)))"
  "(3 2 4 0 0)
(#t #f #t #t #f)
(#t #t #t #t)
")

 ;; "Functions for Collections": do, the manual's example; do on unequal
 ;; sequences stops at the shortest ("Collection Alignment").
 (ch12-do
  "(print (do (method (a b) (print (+ a b)))
              '(100 100 200 200)
              '(1 2 3 4)))
   (print (do (method (a b) (print (list a b))) '(1 2 3) #(x y)))"
  "101
102
203
204
#f
(1 x)
(2 y)
#f
")

 ;; "Functions for Collections": map and map-as, the manual's examples;
 ;; the rest from the specification (map of a vector answers an instance
 ;; of its class-for-copy, a vector; sequences are processed in natural
 ;; order up to the shortest, "Collection Alignment").
 (ch12-map
  "(print (map + '(100 100 200 200) '(1 2 3 4)))
   (print (map-as <vector> + '(100 100 200 200) '(1 2 3 4)))
   (print (map - #(1 2 3)))
   (print (map + '(1 2 3) '(10 20)))
   (print (map-as <list> as-uppercase \"ab\"))
   (bind ((order '()))
     (map (method (x) (set! order (cons x order))) '(a b c))
     (print order))"
  "(101 102 203 204)
#(101 102 203 204)
#(-1 -2 -3)
(11 22)
(#\\A #\\B)
(c b a)
")

 ;; "Functions for Collections": map-into.  The specification only: the
 ;; function receives the elements of collection and more-collections, and
 ;; mutable-collection may itself be one of them.  The manual's example,
 ;; whose result also adds the target's elements, is left out (README.md,
 ;; "Differences from the 1992 manual").
 (ch12-map-into
  "(define x (list 100 100 200 200))
   (print (map-into x + '(1 2 3 4) '(10 20 30 40)))
   (print x)
   (define y (vector 1 2 3))
   (print (id? (map-into y (method (e) (* e e)) y) y))
   (print y)"
  "(11 22 33 44)
(11 22 33 44)
#t
#(1 4 9)
")

 ;; "Functions for Collections": any? and every?, the manual's examples;
 ;; from the specification, any? answers the first true value and stops
 ;; there.
 (ch12-any-every
  "(print (list (any? > '(1 2 3 4) '(5 4 3 2)) (any? even? '(1 3 5 7))))
   (print (list (every? > '(1 2 3 4) '(5 4 3 2)) (every? odd? '(1 3 5 7))))
   (print (any? (method (x) (and (even? x) (* x 10))) '(1 4 6)))
   (bind ((seen '()))
     (any? (method (x) (set! seen (cons x seen)) (> x 2)) '(1 3 5 7))
     (print seen))
   (print (list (every? < '(1 2) '(2 3 0)) (every? even? '()) (any? even? '())))"
  "(#t #f)
(#f #t)
40
(3 1)
(#t #t #f)
")

 ;; "Functions for Collections": reduce and reduce1, the manual's examples;
 ;; from the specification, reduce of an empty collection is the initial
 ;; value, and the procedure is applied in natural order with the
 ;; accumulated value first.
 (ch12-reduce
  "(define high-score 10)
   (print (reduce max high-score '(3 1 4 1 5 9)))
   (print (reduce max high-score '(3 12 9 8 8 6)))
   (print (reduce1 + '(1 2 3 4 5)))
   (print (reduce + 7 '()))
   (print (reduce (method (acc e) (list acc e)) 0 '(1 2 3)))
   (print (reduce1 (method (acc e) (list acc e)) '(1 2 3)))"
  "10
12
15
7
(((0 1) 2) 3)
((1 2) 3)
")

 ;; "Functions for Collections": member?, the manual's example; from the
 ;; specification, test is called with value first.
 (ch12-member
  "(define flavors #(chocolate pistachio pumpkin))
   (print (member? 'chocolate flavors))
   (print (member? 'banana flavors))
   (print (if (member? 2 #(1 2.0 3) test: =) 'yes 'no))
   (print (if (member? 2 #(1 2.0 3)) 'yes 'no))
   (print (if (member? 2 #(1 5 3) test: (method (v e) (< v e))) 'yes 'no))"
  "#t
#f
yes
no
yes
")

 ;; "Functions for Collections": find-key, the manual's example (has-nuts?
 ;; is not defined there); skip: and failure: from the specification.
 (ch12-find-key
  "(define flavors #(chocolate pistachio pumpkin))
   (define has-nuts? (method (f) (id? f 'pistachio)))
   (print (find-key flavors has-nuts?))
   (print (element flavors 1))
   (print (find-key #(1 2 3 4 6) even? skip: 1))
   (print (find-key #(1 2 3 4) even? skip: 2))
   (print (find-key #(1 3) even? failure: 'none))
   (print (find-key '(a b c) (method (e) (id? e 'c))))"
  "1
pistachio
3
#f
none
2
")

 ;; "Functions for Collections": replace-elements!, the manual's example
 ;; (double is not defined there); count: from the specification.
 (ch12-replace-elements
  "(define double (method (n) (* 2 n)))
   (define numbers (list 10 13 16 19))
   (print (replace-elements! numbers odd? double))
   (define v (vector 1 3 5 7))
   (replace-elements! v odd? double count: 2)
   (print v)"
  "(10 26 16 38)
#(2 6 5 7)
")

 ;; "Functions for Collections": fill!, the manual's example; end: and the
 ;; whole-collection case from the specification.
 (ch12-fill
  "(define x (list 'a 'b 'c 'd 'e 'f))
   (print (fill! x 3 start: 2))
   (define v (vector 1 2 3))
   (fill! v 0)
   (print v)
   (define l (list 1 2 3 4))
   (fill! l 'x start: 1 end: 3)
   (print l)"
  "(a b 3 3 3 3)
#(0 0 0)
(1 x x 4)
")

 ;; "Functions for Sequences": add and add!, the manual's examples; from
 ;; the specification, add answers a sequence one larger and leaves the
 ;; argument unmodified.
 (ch12-add
  "(define numbers '(3 4 5))
   (print (add numbers 1))
   (print numbers)
   (define numbers2 (list 3 4 5))
   (print (add! numbers2 1))
   (define v #(1 2))
   (define v2 (add v 3))
   (print (list (size v2) (if (member? 3 v2) #t #f) v))"
  "(1 3 4 5)
(3 4 5)
(1 3 4 5)
(3 #t #(1 2))
")

 ;; "Functions for Sequences": add-new and add-new!, the manual's examples;
 ;; test: from the specification (called with an element first).
 (ch12-add-new
  "(print (add-new '(3 4 5) 1))
   (print (add-new '(3 4 5) 4))
   (print (add-new! (list 3 4 5) 1))
   (print (add-new! (list 3 4 5) 4))
   (print (add-new '(3 4 5) 4.0 test: =))
   (print (size (add-new '(3 4 5) 4.0)))"
  "(1 3 4 5)
(3 4 5)
(1 3 4 5)
(3 4 5)
(3 4 5)
4
")

 ;; "Functions for Sequences": remove and remove!, the manual's examples;
 ;; test: and count: from the specification.
 (ch12-remove
  "(print (remove '(3 1 4 1 5 9) 1))
   (print (remove! (list 3 1 4 1 5 9) 1))
   (define l '(1 2 1 3 1))
   (print (remove l 1 count: 2))
   (print l)
   (print (remove '(1 2.0 3) 2 test: =))
   (print (remove '(1 5 2 6) 4 test: (method (e v) (> e v))))
   (print (remove! (list 1 2 1 3) 1 count: 1))"
  "(3 4 5 9)
(3 4 5 9)
(2 3 1)
(1 2 1 3 1)
(1 3)
(1 2)
(2 1 3)
")

 ;; "Functions for Sequences": choose and choose-by, the manual's examples.
 (ch12-choose
  "(print (choose even? '(3 1 4 1 5 9)))
   (print (choose-by even? (range from: 1) '(a b c d e f g h i)))
   (print (choose odd? #(1 2 3)))"
  "(4)
(b d f h)
#(1 3)
")

 ;; "Functions for Sequences": intersection and union, the manual's
 ;; examples; the order of the results is unspecified, so they are sorted.
 ;; test: from the specification.
 (ch12-intersection-union
  "(define by-name (method (a b) (< (as <string> a) (as <string> b))))
   (print (sort (intersection '(john paul george ringo)
                              '(richard george edward charles john))
                test: by-name))
   (print (sort (union '(butter flour sugar salt eggs)
                       '(eggs butter mushrooms onions salt))
                test: by-name))
   (print (sort (intersection '(1 2 3) '(2.0 3.0) test: =)))
   (print (size (union '(1 2) '(2.0 3) test: =)))"
  "(george john)
(butter eggs flour mushrooms onions salt sugar)
(2 3)
3
")

 ;; "Functions for Sequences": remove-duplicates and remove-duplicates!,
 ;; the manual's examples; test: from the specification.
 (ch12-remove-duplicates
  "(define s '(spam eggs spam sausage spam spam spam))
   (print (remove-duplicates s))
   (print s)
   (print (remove-duplicates! (list 'spam 'eggs 'spam 'sausage 'spam 'spam)))
   (print (size (remove-duplicates '(1 2 1.0 3) test: =)))"
  "(spam eggs sausage)
(spam eggs spam sausage spam spam spam)
(spam eggs sausage)
3
")

 ;; "Functions for Sequences": copy-sequence, the manual's examples; the
 ;; vector and string cases from the specification.
 (ch12-copy-sequence
  "(define hamlet '(to be or not to be))
   (print (id? hamlet (copy-sequence hamlet)))
   (print (copy-sequence hamlet start: 2 end: 4))
   (print (copy-sequence hamlet start: 4))
   (print (copy-sequence #(1 2 3) end: 2))
   (print (copy-sequence \"abcdef\" start: 1 end: 3))"
  "#f
(or not)
(to be)
#(1 2)
\"bc\"
")

 ;; "Functions for Sequences": concatenate-as and concatenate, the manual's
 ;; examples.  The manual's first concatenate-as example misprints the last
 ;; character #\t as #t; its result "nonfat" shows #\t is meant.
 (ch12-concatenate
  "(print (concatenate-as <string> '(#\\n #\\o #\\n) '(#\\f #\\a #\\t)))
   (print (concatenate-as <vector> '(0 1 2) '(3 4 5) '(6 7 8)))
   (print (concatenate \"low-\" \"calorie\"))
   (print (concatenate '(0 1 2) '(3 4 5) '(6 7 8)))
   (print (concatenate-as <list> #(1) \"a\"))"
  "\"nonfat\"
#(0 1 2 3 4 5 6 7 8)
\"low-calorie\"
(0 1 2 3 4 5 6 7 8)
(1 #\\a)
")

 ;; "Functions for Sequences": replace-subsequence!, the manual's example;
 ;; from the specification, the default start: is 0, and an error is
 ;; signaled when there is no room for the inserted elements.
 (ch12-replace-subsequence
  "(define phrase \"I hate oatmeal.\")
   (print (replace-subsequence! phrase \"like\" start: 2))
   (print (replace-subsequence! (list 1 2 3 4) '(a b)))
   (print (handler-case (replace-subsequence! (vector 1 2 3) '(a b) start: 2)
            ((<error>) 'error)))"
  "\"I like oatmeal.\"
(a b 3 4)
error
")

 ;; "Functions for Sequences": reverse and reverse!, the manual's examples;
 ;; the vector and string cases from the specification.
 (ch12-reverse
  "(define x '(bim bam boom))
   (print (reverse x))
   (print x)
   (print (reverse! '(bim bam boom)))
   (print (reverse #(1 2 3)))
   (print (reverse \"abc\"))"
  "(boom bam bim)
(bim bam boom)
(boom bam bim)
#(3 2 1)
\"cba\"
")

 ;; "Functions for Sequences": sort and sort!, the manual's examples; test:
 ;; and stable: from the specification.
 (ch12-sort
  "(define numbers '(3 1 4 1 5 9))
   (print (sort numbers))
   (print numbers)
   (print (sort! '(3 1 4 1 5 9)))
   (print (sort #(3 1 2) test: >))
   (print (sort '((b . 1) (a . 1) (c . 0) (d . 1) (e . 0))
                test: (method (x y) (< (cdr x) (cdr y))) stable: #t))
   (print (sort! (vector 2 3 1)))"
  "(1 1 3 4 5 9)
(3 1 4 1 5 9)
(1 1 3 4 5 9)
#(3 2 1)
((c . 0) (e . 0) (b . 1) (a . 1) (d . 1))
#(1 2 3)
")

 ;; "Functions for Sequences": first, second, third, their setters, and
 ;; last; the manual's example for last, the rest from the specification
 ;; (the setters answer the new value).
 (ch12-first-last
  "(print (list (first '(a b c)) (second '(a b c)) (third '(a b c))))
   (print (list (first #(1 2 3)) (second \"xyz\") (third #(1 2 3))))
   (print (last '(emperor of china)))
   (print (last #(1 2 3)))
   (define v (vector 1 2 3))
   (print ((setter first) v 'one))
   (print ((setter second) v 'two))
   (print ((setter third) v 'three))
   (print v)
   (define l (list 1 2 3))
   (set! (second l) 'b)
   (print l)"
  "(a b c)
(1 #\\y 3)
china
3
one
two
three
#(one two three)
(1 b 3)
")

 ;; "Functions for Sequences": subsequence-position, the manual's example;
 ;; count:, test: and the failing search from the specification.
 (ch12-subsequence-position
  "(print (subsequence-position \"Ralph Waldo Emerson\" \"Waldo\"))
   (print (subsequence-position \"abcabc\" \"bc\" count: 2))
   (print (subsequence-position \"abc\" \"x\"))
   (print (subsequence-position '(1 2 3 4) '(3 4)))
   (print (subsequence-position '(1 2 3 4) '(3.0 4.0) test: =))"
  "6
4
#f
2
2
")

 ;; "The Instantiable Collection Classes" and the class heterarchy diagram
 ;; (Picture 1): the place of each class.
 (ch12-classes
  "(print (list (subclass? <explicit-key-collection> <collection>)
                (subclass? <mutable-collection> <collection>)
                (subclass? <sequence> <collection>)
                (subclass? <mutable-explicit-key-collection> <explicit-key-collection>)
                (subclass? <mutable-explicit-key-collection> <mutable-collection>)
                (subclass? <mutable-sequence> <mutable-collection>)
                (subclass? <mutable-sequence> <sequence>)))
   (print (list (subclass? <array> <mutable-explicit-key-collection>)
                (subclass? <array> <mutable-sequence>)
                (subclass? <vector> <mutable-sequence>)
                (subclass? <table> <mutable-explicit-key-collection>)
                (subclass? <vector> <array>)
                (subclass? <stretchy-vector> <vector>)
                (subclass? <string> <mutable-sequence>)
                (subclass? <deque> <mutable-sequence>)
                (subclass? <range> <sequence>)
                (subclass? <range> <mutable-collection>)))
   (print (list (subclass? <simple-object-vector> <vector>)
                (subclass? <unicode-string> <vector>)
                (subclass? <unicode-string> <string>)
                (subclass? <byte-string> <vector>)
                (subclass? <byte-string> <string>)
                (subclass? <list> <mutable-sequence>)
                (subclass? <pair> <list>)
                (subclass? <empty-list> <list>)))
   (print (list (instance? '() <empty-list>) (instance? '(1) <pair>)
                (instance? \"abc\" <string>) (instance? #(1) <simple-object-vector>)
                (instance? (make <table>) <table>)))"
  "(#t #t #t #t #t #t #t)
(#t #f #t #t #t #t #t #t #t #f)
(#t #t #t #t #t #t #t #t)
(#t #t #t #t #t)
")

 ;; "The Instantiable Collection Classes": make of each class, with size:
 ;; and fill: and their defaults, from the specification.  make of <vector>
 ;; answers a <simple-object-vector>; make of <string> answers an instance
 ;; of a concrete subclass; <unicode-string> fills with #\space by default.
 (ch12-make
  "(print (make <list> size: 3 fill: 'x))
   (print (make <list> size: 2))
   (print (make <list>))
   (print (make <simple-object-vector> size: 2 fill: 'a))
   (print (make <simple-object-vector> size: 2))
   (print (make <simple-object-vector>))
   (define v (make <vector> size: 3))
   (print (list (instance? v <simple-object-vector>) v))
   (print (make <byte-string> size: 3 fill: #\\a))
   (print (make <byte-string>))
   (print (make <unicode-string> size: 2))
   (print (make <unicode-string> size: 2 fill: #\\z))
   (define s (make <string> size: 2 fill: #\\x))
   (print (list (instance? s <string>) (not (id? (object-class s) <string>)) s))
   (define sv (make <stretchy-vector> size: 2))
   (print (list (size sv) (as <list> sv)))
   (print (as <list> (make <stretchy-vector> size: 2 fill: 7)))
   (print (size (make <stretchy-vector>)))
   (define d (make <deque> size: 2))
   (print (list (size d) (as <list> d)))
   (print (as <list> (make <deque> size: 3 fill: 0)))
   (print (size (make <deque>)))"
  "(x x x)
(#f #f)
()
#(a a)
#(#f #f)
#()
(#t #(#f #f #f))
\"aaa\"
\"\"
\"  \"
\"zz\"
(#t #t \"xx\")
(2 (#f #f))
(7 7)
0
(2 (#f #f))
(0 0 0)
0
")

 ;; "Operations on Arrays": aref and (setter aref), the manual's examples.
 ;; The manual shows #(7 5 9) as the value of the set!, but its text says
 ;; (setter aref) returns new-value, so the test prints the value and then
 ;; the array.  An error is signaled for an index out of bounds or the
 ;; wrong number of indices.
 (ch12-aref
  "(print (aref #(7 8 9) 1))
   (define v (vector 7 8 9))
   (print (set! (aref v 1) 5))
   (print v)
   (define w (vector 7 8 9))
   (print ((setter aref) w 1 5))
   (print w)
   (print (handler-case (aref #(7 8 9) 3) ((<error>) 'error)))
   (print (handler-case (aref #(7 8 9) 0 0) ((<error>) 'error)))
   (print (handler-case (set! (aref (vector 1) 1) 0) ((<error>) 'error)))
   (print (handler-case ((setter aref) (vector 1)) ((<error>) 'error)))"
  "8
5
#(7 5 9)
5
#(7 5 9)
error
error
error
error
")

 ;; "Operations on Arrays": dimensions, the manual's example; make of
 ;; <array> with fill: (default #f), aref of a two-dimensional array, and
 ;; element and (setter element) with an index sequence, from the
 ;; specification.
 (ch12-arrays
  "(print (dimensions (make <array> dimensions: '(4 4))))
   (define a (make <array> dimensions: '(2 3) fill: 0))
   (print (list (aref a 0 0) (aref a 1 2)))
   (print (set! (aref a 1 2) 7))
   (print (list (aref a 1 2) (element a '(1 2)) (aref a 0 2) (aref a 1 1)))
   (print (set! (element a '(0 1)) 'x))
   (print (aref a 0 1))
   (print (element a '(2 0) default: 'none))
   (print (handler-case (element a '(2 0)) ((<error>) 'error)))
   (print (handler-case (set! (element a '(0 3)) 1) ((<error>) 'error)))
   (print (handler-case (aref a 0 3) ((<error>) 'error)))
   (print (handler-case (aref a 0) ((<error>) 'error)))
   (print (aref (make <array> dimensions: '(2 2)) 1 1))
   (print (as <list> (dimensions (make <vector> size: 3))))"
  "(4 4)
(0 0)
7
(7 7 0 0)
x
x
none
error
error
error
error
#f
(3)
")

 ;; "Operations on Deques": push, pop, push-last, pop-last, add! (the same
 ;; as push, id? to the deque) and remove! (id? to the deque), from the
 ;; specification.
 (ch12-deque
  "(define d (make <deque>))
   (print (id? (push d 2) d))
   (push d 1)
   (print (id? (push-last d 3) d))
   (push-last d 4)
   (print (as <list> d))
   (print (pop d))
   (print (pop-last d))
   (print (as <list> d))
   (print (id? (add! d 0) d))
   (print (as <list> d))
   (push-last d 2)
   (print (id? (remove! d 2) d))
   (print (as <list> d))
   (print (list (size d) (element d 0) (element d 1)))
   (set! (element d 1) 'y)
   (print (as <list> d))"
  "#t
#t
(1 2 3 4)
1
4
(2 3)
#t
(0 2 3)
#t
(0 3)
(2 0 3)
(0 y)
")

 ;; "Operations on Deques" and "Operations on Vectors": final-state and
 ;; previous-state iterate backward, from the specification.
 (ch12-backward-iteration
  "(define backward
     (method (c)
       (for ((state (final-state c) (previous-state c state))
             (acc '() (cons (current-element c state) acc)))
            ((not state) (reverse acc)))))
   (define d (make <deque>))
   (push-last d 'a) (push-last d 'b) (push-last d 'c)
   (print (backward d))
   (print (backward #(1 2 3)))
   (print (backward \"xyz\"))
   (print (backward (vector)))"
  "(c b a)
(3 2 1)
(#\\z #\\y #\\x)
()
")

 ;; "Operations on Lists": cons, list, list*, car, cdr and their setters,
 ;; the manual's examples.
 (ch12-list-basics
  "(print (cons 1 2))
   (print (cons 1 '(2 3 4 5)))
   (print (list 1 2 3))
   (print (list (+ 4 3) (- 4 3)))
   (print (list* 1 2 3 '(4 5 6)))
   (print (car '(4 5 6)))
   (print (car '()))
   (print (cdr '(4 5 6)))
   (print (cdr '()))
   (define x (list 4 5 6))
   (print (set! (car x) 9))
   (print x)
   (define y (list 4 5 6))
   (print (set! (cdr y) '(a b c)))
   (print y)
   (print (list))"
  "(1 . 2)
(1 2 3 4 5)
(1 2 3)
(7 1)
(1 2 3 4 5 6)
4
()
(5 6)
()
9
(9 5 6)
(a b c)
(4 a b c)
()
")

 ;; "Operations on Lists": caar through cdddr and their setters, from the
 ;; specification: (caddr x) is (car (cdr (cdr x))).
 (ch12-cxr
  "(define x '((1 2) (3 4) 5 6))
   (print (list (caar x) (cadr x) (cdar x) (cddr x)))
   (define tr '(((a b) c) (d e) f g))
   (print (list (caaar tr) (caadr tr) (cadar tr) (caddr tr)))
   (print (list (cdaar tr) (cdadr tr) (cddar tr) (cdddr tr)))
   (define y (list (list 1 2) 3 4 5))
   (set! (caar y) 'a)
   (set! (cadr y) 'b)
   (set! (cdar y) '(c))
   (print y)
   (print (set! (caddr y) 'third))
   (set! (cdddr y) '(end))
   (print y)
   (define z (list (list (list 1) 2) (list 3 4) 5))
   (set! (caaar z) 'p)
   (set! (caadr z) 'q)
   (set! (cadar z) 'r)
   (set! (cdaar z) '(s))
   (set! (cdadr z) '(t))
   (set! (cddar z) '(u))
   (set! (cddr z) '(v))
   (print z)"
  "(1 (3 4) (2) (5 6))
(a d c f)
((b) (e) () (g))
((a c) b 4 5)
third
((a c) b third end)
(((p s) r u) (q t) v)
")

 ;; "Operations on Lists": add! (cons, sharing structure, not id?),
 ;; remove!, member? (the tail or #f), size (#f for a circular list),
 ;; append (sharing the last list) and find-pair, from the specification.
 (ch12-list-operations
  "(define l (list 2 3))
   (define l2 (add! l 1))
   (print (list l2 (id? l2 l) (id? (cdr l2) l)))
   (print (remove! (list 1 2 1 3) 1))
   (print (member? 2 '(1 2 3)))
   (print (member? 4 '(1 2 3)))
   (print (member? 2.0 '(1 2 3) test: =))
   (define circular (list 1 2 3))
   (set! (cdr (cddr circular)) circular)
   (print (list (size circular) (size '(1 2 3)) (size '())))
   (define tail (list 3 4))
   (define joined (append '(1) '(2) tail))
   (print joined)
   (print (id? (cddr joined) tail))
   (print (find-pair 'b '((a . 1) (b . 2) (b . 3))))
   (print (find-pair 'z '((a . 1))))
   (print (find-pair 2.0 '((1 . a) (2 . b)) test: =))
   (print (handler-case (find-pair 'z '((a . 1) 2)) ((<error>) 'error)))"
  "((1 2 3) #f #t)
(2 3)
(2 3)
#f
(2 3)
(#f 3 0)
(1 2 3 4)
#t
(b . 2)
#f
(2 . b)
error
")

 ;; "Operations on Ranges": range, from the specification: from (default
 ;; 0), by (default 1), up-to exclusive, through inclusive, size, the
 ;; smallest of the bounds, and negative by.
 (ch12-range
  "(print (as <list> (range up-to: 4)))
   (print (as <list> (range from: 1 up-to: 5)))
   (print (as <list> (range from: 1 through: 5)))
   (print (as <list> (range from: 1 size: 3)))
   (print (as <list> (range by: 2 size: 3)))
   (print (as <list> (range from: 0 through: 6 by: 2)))
   (print (as <list> (range from: 0 up-to: 6 by: 2)))
   (print (as <list> (range from: 1 up-to: 10 size: 3)))
   (print (as <list> (range from: 1 up-to: 3 size: 10)))
   (print (as <list> (range from: 1 through: 3 up-to: 3)))
   (print (as <list> (range from: 10 through: 4 by: -3)))
   (print (as <list> (range from: 10 up-to: 4 by: -3)))
   (print (as <list> (range from: 5 up-to: 5)))
   (print (list (size (range from: 1 up-to: 5)) (size (range from: 1))))
   (print (list (element (range from: 3 by: 2) 100) (first (range from: 7))))
   (print (instance? (range up-to: 3) <range>))"
  "(0 1 2 3)
(1 2 3 4)
(1 2 3 4 5)
(1 2 3)
(0 2 4)
(0 2 4 6)
(0 2 4)
(1 2 3)
(1 2)
(1 2)
(10 7 4)
(10 7)
()
(4 #f)
(203 7)
#t
")

 ;; "Operations on Ranges": member?, copy-sequence, binary=, reverse,
 ;; reverse! and intersection on ranges, from the specification: member?
 ;; and binary= always terminate; copy-sequence, reverse and intersection
 ;; answer ranges; reverse! answers its argument; an unbounded range
 ;; cannot be reversed.
 (ch12-range-operations
  "(define odds (range from: 1 by: 2))
   (print (list (member? 1000001 odds) (member? 4 odds) (member? -1 odds)))
   (define c (copy-sequence (range up-to: 5) start: 1 end: 3))
   (print (list (instance? c <range>) (as <list> c)))
   (print (list (= (range from: 0) (range from: 0)) (= (range from: 0) (range from: 1))
                (= (range up-to: 3) (range through: 2)) (= (range up-to: 3) (range up-to: 4))))
   (define r (reverse (range from: 1 through: 3)))
   (print (list (instance? r <range>) (as <list> r)))
   (define r2 (range from: 1 through: 3))
   (print (id? (reverse! r2) r2))
   (print (handler-case (reverse (range from: 1)) ((<error>) 'error)))
   (define sixes (intersection (range from: 0 by: 2) (range from: 0 by: 3)))
   (print (list (instance? sixes <range>) (size sixes) (member? 12 sixes) (member? 4 sixes)))
   (define both (intersection (range from: 0 through: 10) (range from: 5 through: 20)))
   (print (list (instance? both <range>) (sort (as <list> both))))"
  "(#t #f #f)
(#t (1 2))
(#t #f #t #f)
(#t (3 2 1))
#t
error
(#t #f #t #f)
(#t (5 6 7 8 9 10))
")

 ;; "Operations on Stretchy Vector": add! adds at the end and answers the
 ;; stretchy vector; remove! answers the stretchy vector, from the
 ;; specification.
 (ch12-stretchy-vector
  "(define sv (make <stretchy-vector>))
   (print (id? (add! sv 'a) sv))
   (add! sv 'b)
   (add! sv 'c)
   (add! sv 'b)
   (print (list (size sv) (as <list> sv)))
   (print (id? (remove! sv 'b) sv))
   (print (as <list> sv))
   (set! (element sv 1) 'z)
   (print (list (element sv 0) (element sv 1) (size sv)))
   (print (instance? sv <vector>))"
  "#t
(4 (a b c b))
#t
(a c)
(a z 2)
#t
")

 ;; "Operations on Strings": binary< compares lexicographically, a strict
 ;; prefix being smaller, from the specification; as-lowercase,
 ;; as-lowercase!, as-uppercase and as-uppercase!, the manual's examples
 ;; (its as-uppercase! example calls as-uppercase; the ! forms modify the
 ;; string, per the specification).
 (ch12-strings
  "(print (list (< \"abc\" \"abd\") (< \"abd\" \"abc\") (< \"ab\" \"abc\") (< \"abc\" \"ab\")
                (< \"abc\" \"abc\") (binary< \"a\" \"b\") (> \"b\" \"abc\")))
   (define x \"Van Gogh\")
   (print (as-lowercase x))
   (print x)
   (print (as-uppercase x))
   (print x)
   (define y (copy-sequence \"Van Gogh\"))
   (print (as-lowercase! y))
   (print y)
   (define z (copy-sequence \"Van Gogh\"))
   (print (as-uppercase! z))
   (print z)"
  "(#t #f #t #f #f #t #t)
\"van gogh\"
\"Van Gogh\"
\"VAN GOGH\"
\"Van Gogh\"
\"van gogh\"
\"van gogh\"
\"VAN GOGH\"
\"VAN GOGH\"
")

 ;; "Operations on Tables" and "Collection Keys": (setter element) adds a
 ;; key, element with and without default:, remove-key!, size, key-sequence
 ;; (in an unspecified order, so sorted), from the specification.
 (ch12-table
  "(define tb (make <table>))
   (print (set! (element tb 'a) 1))
   (set! (element tb 'b) 2)
   (set! (element tb 3) 'three)
   (print (list (size tb) (element tb 'a) (element tb 3)))
   (set! (element tb 'a) 10)
   (print (list (size tb) (element tb 'a)))
   (print (element tb 'zz default: 'none))
   (print (handler-case (element tb 'zz) ((<error>) 'error)))
   (print (id? (remove-key! tb 'b) tb))
   (print (list (size tb) (element tb 'b default: 'gone)))
   (define ks (key-sequence tb))
   (print (list (size ks) (if (member? 'a ks) #t #f) (if (member? 3 ks) #t #f) (if (member? 'b ks) #t #f)))
   (print (empty? (make <table>)))"
  "1
(3 1 three)
(3 10)
none
error
#t
(2 gone)
(2 #t #t #f)
#t
")

 ;; "Operations on Tables": map-into a table stores each result under its
 ;; key, stretching the table, and leaves its other keys undisturbed, from
 ;; the specification.
 (ch12-table-map-into
  "(define tb (make <table>))
   (set! (element tb 'z) 0)
   (print (id? (map-into tb (method (x) (* x 10)) #(1 2 3)) tb))
   (print (list (size tb) (element tb 0) (element tb 1) (element tb 2) (element tb 'z)))"
  "#t
(4 10 20 30 0)
")

 ;; "Collection Alignment": operations on several collections, some of
 ;; them tables, use the intersection of their key sequences, from the
 ;; specification.
 (ch12-alignment
  "(define tb (make <table>))
   (set! (element tb 0) 'a)
   (set! (element tb 5) 'b)
   (print (do (method (x y) (print (list x y))) tb #(p q r)))
   (print (every? (method (x y) (= x y)) tb #(a)))
   (define u (make <table>))
   (set! (element u 0) 1)
   (set! (element u 5) 2)
   (set! (element u 9) 3)
   (define v (make <table>))
   (set! (element v 5) 10)
   (set! (element v 9) 20)
   (set! (element v 7) 30)
   (define sums (map + u v))
   (print (list (size sums) (element sums 5) (element sums 9)))"
  "(a p)
#f
#t
(2 12 23)
")

 ;; "Operations on Vectors": vector, element with and without default:,
 ;; (setter element), from the specification.
 (ch12-vector
  "(print (vector 1 'a \"s\" #\\c))
   (print (vector))
   (define v (vector 1 2 3))
   (print (list (element v 0) (element v 2) (element v 5 default: 'none)))
   (print (handler-case (element v 3) ((<error>) 'error)))
   (print (set! (element v 1) 'two))
   (print v)
   (print (list (element \"abc\" 1) (element '(a b c) 2)))
   (print (handler-case (element '(a b c) 3) ((<error>) 'error)))"
  "#(1 a \"s\" #\\c)
#()
(1 3 none)
error
two
#(1 two 3)
(#\\b c)
error
")

 ;; "The Iteration Protocol": initial-state, next-state, current-element
 ;; and copy-state; the manual's do1 example; an empty collection's
 ;; initial state is #f; a copied state is unaffected by next-state on the
 ;; original.
 (ch12-iteration-protocol
  "(define-method do1 (f (c <collection>))
     (for ((state (initial-state c) (next-state c state)))
          ((not state) #f)
       (f (current-element c state))))
   (print (do1 print '(a b)))
   (print (do1 print #(1 2)))
   (do1 print \"hi\")
   (print (list (initial-state '()) (initial-state #()) (initial-state \"\")
                (initial-state (make <table>))))
   (define l '(x y z))
   (define s (initial-state l))
   (define saved (copy-state l s))
   (set! s (next-state l s))
   (set! s (next-state l s))
   (print (list (current-element l s) (current-element l saved)))
   (print (next-state l s))"
  "a
b
#f
1
2
#f
#\\h
#\\i
(#f #f #f #f)
(z x)
#f
")

 ;; "Collection Keys": key-sequence, current-key, and the manual's
 ;; do-with-keys methods for <explicit-key-collection> and <sequence>.
 (ch12-keys
  "(define-method do-with-keys (f (c <explicit-key-collection>))
     (for ((state (initial-state c) (next-state c state)))
          ((not state) #f)
       (f (current-key c state) (current-element c state))))
   (define-method do-with-keys (f (c <sequence>))
     (for ((state (initial-state c) (next-state c state))
           (key 0 (+ key 1)))
          ((not state) #f)
       (f key (current-element c state))))
   (print (do-with-keys (method (k e) (print (list k e))) '(a b c)))
   (define tb (make <table>))
   (set! (element tb 1) 'one)
   (set! (element tb 2) 'two)
   (define pairs '())
   (do-with-keys (method (k e) (set! pairs (cons (list k e) pairs))) tb)
   (print (sort pairs test: (method (x y) (< (first x) (first y)))))
   (print (as <list> (key-sequence #(a b c))))
   (print (as <list> (key-sequence \"\")))
   (print (sort (as <list> (key-sequence tb))))
   (define keys '())
   (for ((state (initial-state tb) (next-state tb state)))
        ((not state) #f)
     (set! keys (cons (current-key tb state) keys)))
   (print (sort keys))"
  "(0 a)
(1 b)
(2 c)
#f
((1 one) (2 two))
(0 1 2)
()
(1 2)
(1 2)
")

 ;; "Mutability": (setter current-element) changes the element at the
 ;; state without disturbing the iteration; the default (setter element)
 ;; of a mutable sequence signals an error for a key that is not there
 ;; (the chapter's default method, and design note 10, which it cites).
 (ch12-mutability
  "(define l (list 1 2 3))
   (for ((state (initial-state l) (next-state l state)))
        ((not state) #f)
     (set! (current-element l state) (* 10 (current-element l state))))
   (print l)
   (define tb (make <table>))
   (set! (element tb 'k) 1)
   (bind ((state (initial-state tb)))
     (print (set! (current-element tb state) 'new)))
   (print (element tb 'k))
   (define v (vector 1 2 3))
   (bind ((state (next-state v (initial-state v))))
     (set! (current-element v state) 'b))
   (print v)
   (define m (list 1 2))
   (print (set! (element m 1) 'x))
   (print m)
   (print (handler-case (set! (element m 5) 'y) ((<error>) 'error)))"
  "(10 20 30)
new
new
#(1 b 3)
x
(1 x)
error
")
)
