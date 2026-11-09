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
;;;; Specializations for the string and byte-string types.
;;;;                        (was Thomas's src/runtime-collections-string.scm)

(in-package #:dylan)

(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <byte-string>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (declare (ignore class))
    (if (true? (dylan-call dylan/instance? collection <byte-string>))
        collection
        (let* ((size (collection-size collection))
               (new-string (make-string size)))
          (do ((state (initial-state collection)
                      (next-state collection state))
               (index 0 (+ index 1)))
              ((not state) new-string)
            ;; Its elements must be characters, as for element-setter.
            (let ((character (current-element collection state)))
              (unless (and (characterp character) (< (char-code character) 256))
                (dylan-call dylan/error
                            "a <byte-string> holds only 8-bit characters: ~s"
                            character))
              (setf (char new-string index) character))))))))

;;; Chapter 12: <unicode-string> (see MAKE-UNICODE-STRING in compat.lisp)
(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <unicode-string>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (declare (ignore class))
    (if (unicode-string? collection)
        collection
        ;; Its characters directly, not by way of a <byte-string>, which
        ;; cannot hold those above code 255.
        (let ((characters (iterate->list
                           (lambda (element)
                             (unless (characterp element)
                               (dylan-call dylan/error
                                           "a string holds only characters: ~s"
                                           element))
                             element)
                           collection)))
          ;; (REPLACE's value may be the string's simple data vector.)
          (let ((string (make-unicode-string (length characters))))
            (replace string characters)
            string))))))

(defun check-byte-character (string character &optional (prefix ""))
  "Signal an error, as the compiler to C does, if CHARACTER is to go into
STRING and is not a character, or STRING is a <byte-string> and its code is
above 255."
  (cond ((and (simple-string-p string)
              (not (and (characterp character) (< (char-code character) 256))))
         (dylan-call dylan/error
                     (concatenate 'string prefix
                                  "a <byte-string> holds only 8-bit characters: ~s")
                     character))
        ((not (characterp character))
         (dylan-call dylan/error
                     (concatenate 'string prefix "a string holds only characters: ~s")
                     character))))

(defun string-like (original result)
  "The string RESULT, made from ORIGINAL, as a string of ORIGINAL's class."
  (if (unicode-string? original) (as-unicode-string result) result))

(add-method
 dylan/as
 (dylan%function->method
  (make-param-list `((class ,(dylan%make-singleton <string>))
                     (collection ,<collection>)) nil nil nil)
  (lambda (class collection)
    (declare (ignore class))
    (if (true? (dylan-call dylan/instance? collection <string>))
        collection
        (dylan-call dylan/as <byte-string> collection)))))

;;;
;;; BYTE-STRING MAKE yields a Lisp string
;;;
;;; size keyword overrides inherited dimensions keyword (which is not used)
;;;
(defun make-dylan-string (unicode? next-method rest)
  "make of a <byte-string>, or with UNICODE? a <unicode-string>, given
the keyword arguments REST."
  (dylan%keyword-validate next-method rest '(:size :fill))
  (let* ((size (dylan%find-keyword rest :size (lambda () 0)))
         (have-fill? t)
         (fill (dylan%find-keyword rest :fill
                                   (lambda () (setf have-fill? nil) nil))))
    (when (or (not (integerp size)) (minusp size))
      (dylan-call dylan/error
                  "make: size: must be a non-negative integer: ~s" size))
    (check-room size (if unicode? 4 1))
    (when have-fill?
      (if unicode?
          (unless (characterp fill)
            (dylan-call dylan/error "make: fill: must be a character: ~s" fill))
          (check-byte-character "" fill "make: ")))
    ;; As in the compiler to C, the fill is a space unless given.
    (let ((fill (if have-fill? fill #\Space)))
      (if unicode?
          (make-unicode-string size fill)
          (make-string size :initial-element fill)))))

(add-method
 dylan/make
 (dylan%dylan-callable->method
  (make-param-list `((string ,(dylan%make-singleton <byte-string>)))
                   nil nil '(:size :fill))
  (lambda (multiple-values next-method class &rest rest)
    (declare (ignore multiple-values class))
    (make-dylan-string nil next-method rest))))

;;;
;;; STRING SPECIALIZED MAKE
;;; And we'll make <string> turn into <byte-string>
;;;
(add-method
 dylan/make
 (dylan%function->method
  (make-param-list `((string ,(dylan%make-singleton <string>))) nil nil :any)
  (lambda (class &rest rest)
    (declare (ignore class))
    (dylan-apply dylan/make <byte-string> rest))))

;;;
;;; UNICODE-STRING SPECIALIZED MAKE, whose fill: may be any character
;;;
(add-method
 dylan/make
 (dylan%dylan-callable->method
  (make-param-list `((unicode ,(dylan%make-singleton <unicode-string>)))
                   nil nil '(:size :fill))
  (lambda (multiple-values next-method class &rest rest)
    (declare (ignore multiple-values class))
    (make-dylan-string t next-method rest))))

;;;
;;; FUNCTIONS FOR COLLECTIONS (page 99)
;;;
(add-method dylan/size (one-arg 'string <byte-string> #'length))
(add-method dylan/size (one-arg 'string <unicode-string> #'length))

(add-method dylan/class-for-copy
            (dylan%function->method
             (make-param-list `((string ,<unicode-string>)) nil nil nil)
             (lambda (x)
               (declare (ignore x))
               <unicode-string>)))

(add-method dylan/class-for-copy
            (dylan%function->method one-string
                                    (lambda (x)
                                      (declare (ignore x))
                                      <byte-string>)))

;;;;
;;;; Functions for Sequences (page 104)
;;;;
(dolist (class (list <byte-string> <unicode-string>))
  (add-method
   dylan/add
   (dylan%function->method
    (make-param-list `((string ,class) (object ,<object>)) nil nil nil)
    (lambda (the-string new-element)
      (check-byte-character the-string new-element)
      (string-like the-string
                   (concatenate 'string (string new-element) the-string))))))

(add-method
 dylan/concatenate
 (dylan%function->method
  (make-param-list `((byte-string ,<byte-string>)) nil 'rest nil)
  (lambda (string-1 &rest rest)
    ;; A new string, even of one argument (chapter 12)
    (let ((result (copy-seq string-1)))
      (dolist (s (mapcar (lambda (seq) (dylan-call dylan/as <byte-string> seq))
                         rest)
               result)
        (setf result (concatenate 'string result s)))))))

(add-method
 dylan/concatenate
 (dylan%function->method
  (make-param-list `((unicode-string ,<unicode-string>)) nil 'rest nil)
  (lambda (string-1 &rest rest)
    (as-unicode-string
     (apply #'concatenate 'string string-1
            (mapcar (lambda (seq) (dylan-call dylan/as <unicode-string> seq)) rest))))))

;;; Any other <string>, of a class of the program's own
(add-method
 dylan/concatenate
 (dylan%function->method
  (make-param-list `((string ,<string>)) nil 'rest nil)
  (lambda (string-1 &rest rest)
    (list->sequence-like string-1
                         (mapcan #'sequence->list (cons string-1 rest))))))

(dolist (class (list <byte-string> <unicode-string>))
  (add-method
   dylan/reverse
   (dylan%function->method
    (make-param-list `((string ,class)) nil nil nil)
    (lambda (string-1)
      (let ((result (make-string (length string-1))))
        (do ((from (- (length string-1) 1) (- from 1))
             (to 0 (+ to 1)))
            ((< from 0) (string-like string-1 result))
          (setf (char result to) (char string-1 from))))))))

(dolist (class (list <byte-string> <unicode-string>))
  (add-method
   dylan/reverse!
   (dylan%function->method
    (make-param-list `((string ,class)) nil nil nil)
    (lambda (string-1)
      (do ((from (- (length string-1) 1) (- from 1))
           (to 0 (+ to 1)))
          ((<= from to) string-1)
        (rotatef (char string-1 to) (char string-1 from)))))))

;;;; Operations on Strings (page 119)

;;; Chapter 12: strings compare "lexicographically, using binary< on
;;; corresponding elements", as the compiled library does: by CL's STRING<
;;; while binary< has only its predefined methods on characters.
(defun string-binary< (a b)
  (if (and *fast-character-compare* (stringp a) (stringp b))
      (bool (string< a b))
      (let ((m (library-size a))
            (n (library-size b)))
        (loop for i from 0
              do (cond ((= i m) (return (bool (< i n))))
                       ((= i n) (return (bool nil)))
                       (t (let ((x (library-element a i))
                                (y (library-element b i)))
                            (cond ((true? (dylan-call dylan/binary< x y))
                                   (return (bool t)))
                                  ((true? (dylan-call dylan/binary< y x))
                                   (return (bool nil)))))))))))

(add-method dylan/binary<
            (dylan%function->method two-strings #'string-binary<))

;;; The generic function DYLAN/AS-LOWERCASE is defined in
;;; runtime-methods.lisp.

;;; Chapter 12: as-lowercase of a string "is equivalent to (map
;;; as-lowercase string)", and as-lowercase! to (map-into string
;;; as-lowercase string); so for as-uppercase.  CL's CHAR-DOWNCASE and
;;; CHAR-UPCASE do it while the two have only their predefined methods on
;;; characters (runtime-fast-paths.lisp).
(defvar *fast-character-case* nil)

(dolist (class (list <byte-string> <unicode-string>))
  (add-method
   dylan/as-lowercase
   (dylan%function->method
    (make-param-list `((byte-string ,class)) nil nil nil)
    (lambda (string)
      (if *fast-character-case*
          (string-like string (map 'string #'char-downcase string))
          (dylan-call dylan/map dylan/as-lowercase string))))))

(defparameter dylan/as-lowercase!
  (dylan%generic-fn 'as-lowercase! one-string nil))

(dolist (class (list <byte-string> <unicode-string>))
  (add-method
   dylan/as-lowercase!
   (dylan%function->method
    (make-param-list `((byte-string ,class)) nil nil nil)
    (lambda (string)
      (if *fast-character-case*
          (do ((index 0 (+ index 1)))
              ((>= index (length string)) string)
            (setf (char string index) (char-downcase (char string index))))
          (dylan-call dylan/map-into string dylan/as-lowercase string))))))

;;; The generic function DYLAN/AS-UPPERCASE is defined in
;;; runtime-methods.lisp.

(dolist (class (list <byte-string> <unicode-string>))
  (add-method
   dylan/as-uppercase
   (dylan%function->method
    (make-param-list `((byte-string ,class)) nil nil nil)
    (lambda (string)
      ;; A <byte-string> holds only 8-bit characters, and the upper case of
      ;; y with diaeresis (code 255) is not one: an error, as in a compiled
      ;; program.
      (if *fast-character-case*
          (string-like string
                       (map 'string
                            (lambda (c)
                              (let ((upper (char-upcase c)))
                                (check-byte-character string upper)
                                upper))
                            string))
          (dylan-call dylan/map dylan/as-uppercase string))))))

(defparameter dylan/as-uppercase!
  (dylan%generic-fn 'as-uppercase! one-string nil))

(dolist (class (list <byte-string> <unicode-string>))
  (add-method
   dylan/as-uppercase!
   (dylan%function->method
    (make-param-list `((byte-string ,class)) nil nil nil)
    (lambda (string)
      (if *fast-character-case*
          (do ((index 0 (+ index 1)))
              ((>= index (length string)) string)
            (let ((upper (char-upcase (char string index))))
              (check-byte-character string upper)
              (setf (char string index) upper)))
          (dylan-call dylan/map-into string dylan/as-uppercase string))))))

;;; PREVIOUS-STATE for strings is the array method: string states are
;;; index vectors.  (The Scheme had a method here specialized on integer
;;; states, which no string state ever is.)

;;;
;;; Collection Keys
;;;
(dolist (class (list <byte-string> <unicode-string>))
  (add-method
   dylan/element
   (dylan%dylan-callable->method
    (make-param-list `((byte-string ,class) (index ,<integer>))
                     nil nil '(:default))
    (lambda (multiple-values next-method string-value index &rest rest)
      (declare (ignore multiple-values))
      (dylan%keyword-validate next-method rest '(:default))
      (let ((size (length string-value)))
        (if (and (>= index 0) (< index size))
            (char string-value index)
            ;; N.B. the Scheme passed the LIST (default:) as the keyword to
            ;; look for rather than the keyword itself, so the default: value
            ;; was never found; fixed.
            (dylan%find-keyword
             rest :default
             (lambda ()
               (dylan-call
                dylan/error
                "element: there is no element at the index ~s"
                index)))))))))

;;;
;;; Mutable Collections
;;;
(dolist (class (list <byte-string> <unicode-string>))
  (add-method
   dylan/setter/current-element/
   (dylan%function->method
    (make-param-list `((byte-string ,class) (state ,<object>)
                       (new-value ,<object>))
                     nil nil nil)
    (lambda (the-string state new-value)
      (check-byte-character the-string new-value)
      (setf (char the-string (aref state 0)) new-value)
      new-value))))

(dolist (class (list <byte-string> <unicode-string>))
  (add-method
   dylan/setter/element/
   (dylan%function->method
    (make-param-list `((byte-string ,class) (index ,<object>)
                       (new-value ,<object>))
                     nil nil nil)
    (lambda (string key new-value)
      (let ((index (index-key key)))
       (unless (and index (< -1 index (length string)))
        (dylan-call dylan/error
                    "element: there is no element at the index ~s"
                    key))
      (check-byte-character string new-value)
      (setf (char string index) new-value)
      new-value)))))
