module: dylan

;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;
;;; Strings (chapter 12, "Operations on Strings"): <byte-string> and
;;; <unicode-string>, vectors of characters.

;;; <byte-string>
(define-method initial-state ((v <byte-string>)) (%vector-initial-state v))
(define-method next-state ((v <byte-string>) (i <integer>)) (%vector-next-state v i))
(define-method current-element ((v <byte-string>) (i <integer>)) (%vector-element v i))
(define-method (setter current-element) ((v <byte-string>) (i <integer>) new-value)
  (%vector-element-setter v i new-value))
(define-method final-state ((v <byte-string>)) (%vector-final-state v))
(define-method previous-state ((v <byte-string>) (i <integer>)) (%vector-previous-state v i))
(define-method size ((v <byte-string>)) (%vector-size v))
(define-method empty? ((v <byte-string>)) (zero? (%vector-size v)))
(define-method element ((v <byte-string>) index #key (default %no-default))
  (%vector-ref v index default))
(define-method (setter element) ((v <byte-string>) index new-value)
  (%vector-element-setter v index new-value))
(define-method copy-sequence ((v <byte-string>) #key (start 0) end)
  (%copy-vector v start (or end (%vector-size v))))
(define-method =hash ((s <byte-string>)) (%string-hash s))

;;; <unicode-string>
(define-method initial-state ((v <unicode-string>)) (%vector-initial-state v))
(define-method next-state ((v <unicode-string>) (i <integer>)) (%vector-next-state v i))
(define-method current-element ((v <unicode-string>) (i <integer>)) (%vector-element v i))
(define-method (setter current-element) ((v <unicode-string>) (i <integer>) new-value)
  (%vector-element-setter v i new-value))
(define-method final-state ((v <unicode-string>)) (%vector-final-state v))
(define-method previous-state ((v <unicode-string>) (i <integer>)) (%vector-previous-state v i))
(define-method size ((v <unicode-string>)) (%vector-size v))
(define-method empty? ((v <unicode-string>)) (zero? (%vector-size v)))
(define-method element ((v <unicode-string>) index #key (default %no-default))
  (%vector-ref v index default))
(define-method (setter element) ((v <unicode-string>) index new-value)
  (%vector-element-setter v index new-value))
(define-method copy-sequence ((v <unicode-string>) #key (start 0) end)
  (%copy-vector v start (or end (%vector-size v))))
(define-method =hash ((s <unicode-string>)) (%string-hash s))

(define-method make ((class (singleton <byte-string>)) #key (size 0) (fill #\space))
  (%make-byte-string size fill))

(define-method make ((class (singleton <unicode-string>)) #key (size 0) (fill #\space))
  (%make-unicode-string size fill))

;;; <string> has no direct instances: make of it makes a <byte-string>.
(define-method make ((class (singleton <string>)) #key (size 0) (fill #\space))
  (%make-byte-string size fill))

;;; Lexicographic order, by binary< on the characters; a strict prefix is
;;; the lesser.
(define-method binary< ((x <string>) (y <string>))
  (bind ((m (size x)) (n (size y)))
    (bind-methods ((compare (i)
                     (cond ((= i m) (binary< i n))
                           ((= i n) #f)
                           (else:
                            (bind ((a (element x i)) (b (element y i)))
                              (cond ((binary< a b) #t)
                                    ((binary< b a) #f)
                                    (else: (compare (binary+ i 1)))))))))
      (compare 0))))

(define-method as-lowercase ((s <string>)) (map as-lowercase s))
(define-method as-uppercase ((s <string>)) (map as-uppercase s))

(define-generic-function as-lowercase! (string))
(define-method as-lowercase! ((s <string>)) (map-into s as-lowercase s))

(define-generic-function as-uppercase! (string))
(define-method as-uppercase! ((s <string>)) (map-into s as-uppercase s))
