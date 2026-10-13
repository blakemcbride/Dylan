;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Writing the C files (CompilerPlan.md, Phase 10).
;;;;
;;;; The code generator writes a program as one C text.  For a program
;;;; whose C is NAME.c, that becomes
;;;;
;;;;   NAME.c          the static data, the object system's functions
;;;;                   (dispatch, slot accessors, make), and main
;;;;   NAME-MODULE.c   the functions written in the module MODULE, one file
;;;;                   for each module (the library's is dylan)
;;;;   NAME.h          what they share: an extern declaration of each
;;;;                   object NAME.c defines and a prototype of each function
;;;;
;;;; so every definition loses its `static'.  NAME.c does not include NAME.h:
;;;; a static object of an unnamed struct type may not be declared twice in
;;;; one file.  The text is cut into its top-level definitions by following
;;;; braces, strings and comments, which is all the generated C needs.
;;;;
;;;; Each #line DY_RESET line the code generator writes after a function
;;;; becomes a #line directive back to the C file itself.

(in-package #:dylan-compiler)

;;; Cutting the text

(defun top-level-items (text)
  "TEXT cut into its top-level items, each a string: a preprocessor
line, a comment, a declaration or definition up to its `;', or a function
from its head to its closing brace."
  (let ((items '())
        (start 0)
        (depth 0)
        (i 0)
        (n (length text)))
    (flet ((finish (end)
             (let ((item (string-trim '(#\Newline #\Space) (subseq text start end))))
               (when (plusp (length item)) (push item items)))
             (setf start end)))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond
                   ;; A preprocessor line at the top level
                   ((and (zerop depth) (char= c #\#)
                         (or (zerop i) (char= (char text (1- i)) #\Newline))
                         (every (lambda (ch) (member ch '(#\Space #\Newline)))
                                (subseq text start i)))
                    (let ((end (or (position #\Newline text :start i) n)))
                      (setf start i)
                      (finish end)
                      (setf i end)))
                   ;; A comment
                   ((and (char= c #\/) (< (1+ i) n) (char= (char text (1+ i)) #\*))
                    (let ((end (+ 2 (search "*/" text :start2 (+ i 2)))))
                      (when (and (zerop depth)
                                 (every (lambda (ch) (member ch '(#\Space #\Newline)))
                                        (subseq text start i)))
                        (setf start i)
                        (finish end))
                      (setf i end)))
                   ;; A string or character literal
                   ((member c '(#\" #\'))
                    (incf i)
                    (loop until (char= (char text i) c)
                          do (incf i (if (char= (char text i) #\\) 2 1)))
                    (incf i))
                   ((char= c #\{) (incf depth) (incf i))
                   ((char= c #\})
                    (decf depth) (incf i)
                    ;; The end of a function body
                    (when (and (zerop depth)
                               (let ((next (position-if-not (lambda (ch) (char= ch #\Space))
                                                            text :start i)))
                                 (or (null next) (char= (char text next) #\Newline))))
                      (finish i)))
                   ((and (char= c #\;) (zerop depth))
                    (incf i)
                    (finish i))
                   (t (incf i)))))
      (finish n))
    (nreverse items)))

(defun strip-static (item)
  (if (and (>= (length item) 7) (string= "static " item :end2 7))
      (subseq item 7)
      item))

(defun function-definition-p (item)
  "Is ITEM a function definition (its body in braces)?"
  (let ((paren (position #\( item))
        (brace (position #\{ item))
        (equals (position #\= item)))
    (and paren brace (< paren brace)
         (or (null equals) (< brace equals))
         (char= (char item (1- (length item))) #\}))))

(defun defined-name (item)
  "The identifier ITEM declares or defines."
  (let* ((end (or (and (function-definition-p item) (position #\( item))
                  (let ((equals (search " =" item)))
                    (and equals (or (position #\[ item :end equals) equals)))
                  (or (position #\[ item) (position #\; item))))
         (head (string-right-trim " " (subseq item 0 end)))
         (start (1+ (or (position-if-not (lambda (c) (or (alphanumericp c) (char= c #\_)))
                                         head :from-end t)
                        -1))))
    (subseq head start)))

(defun extern-declaration (item)
  "An extern declaration of what the data definition ITEM defines."
  (let* ((item (strip-static item))
         (equals (search " =" item))
         (head (string-right-trim " ;" (if equals (subseq item 0 equals) item))))
    (if (and (>= (length head) 7) (string= "extern " head :end2 7))
        (format nil "~a;" head)
        (format nil "extern ~a;" head))))

;;; Writing

(defun module-file-stem (module)
  (substitute-if #\_ (lambda (c) (not (or (alphanumericp c) (char= c #\-)))) module))

(defun write-c-program (text c-file modules)
  "Write TEXT, the C of a program, as C-FILE and the files that go with it
(above); MODULES maps the C name of each Dylan function to its module.
Answer the C files, C-FILE first."
  (let* ((c-file (pathname c-file))
         (header (make-pathname :type "h" :defaults c-file))
         (main '())                     ; items of C-FILE, in order
         (shared '())                   ; lines of the header
         (by-module (make-hash-table :test #'equal)) ; module -> items
         (module-order '()))
    (dolist (item (top-level-items text))
      (cond
        ;; Written after each function that needs it, below
        ((string= item +line-reset+))
        ((char= (char item 0) #\#)
         (if (search "#include" item)
             (push item shared)
             (push item main)))
        ((string= "_Static_assert" item :end2 (min 14 (length item))) (push item main))
        ((string= "/*" item :end2 (min 2 (length item))) (push item main))
        ((function-definition-p item)
         (let* ((name (defined-name item))
                (module (gethash name modules))
                (definition (if (search (format nil "~%#line ") item)
                                (format nil "~a~%~a" (strip-static item) +line-reset+)
                                (strip-static item))))
           (cond ((string= name "main") (push definition main))
                 (t (push (format nil "~a;" (subseq definition 0 (position #\{ definition)))
                          shared)
                    (if module
                        (progn
                          (unless (member module module-order :test #'string=)
                            (push module module-order))
                          (push definition (gethash module by-module)))
                        (push definition main))))))
        ;; A prototype
        ((and (position #\( item) (not (search " =" item))
              (< (position #\( item) (or (position #\{ item) (length item))))
         (let ((prototype (strip-static item)))
           (push prototype shared)
           (push prototype main)))
        ;; Data
        (t (push (extern-declaration item) shared)
           (push (strip-static item) main))))
    (let ((files '()))
      (write-c-file header
                    (append (list "/* Generated by the Dylan compiler.  Do not edit. */")
                            (remove-duplicates (reverse shared) :test #'string= :from-end t)))
      (write-c-file c-file (cons "/* Generated by the Dylan compiler.  Do not edit. */"
                                 (cons "#include \"dylan.h\"" (remove "#include \"dylan.h\""
                                                                      (reverse main)
                                                                      :test #'string=))))
      (dolist (module (reverse module-order))
        (let ((file (make-pathname :name (format nil "~a-~a" (pathname-name c-file)
                                                 (module-file-stem module))
                                   :defaults c-file)))
          (write-c-file file
                        (list* (format nil "/* Generated by the Dylan compiler.  Do not edit.~%   The functions of the module ~a. */"
                                       module)
                               (format nil "#include \"~a.h\"" (pathname-name c-file))
                               (reverse (gethash module by-module))))
          (push file files)))
      (cons c-file (reverse files)))))

(defun write-c-file (file items)
  "Write ITEMS, each followed by a blank line, to FILE, replacing each
#line DY_RESET by a #line directive back to FILE."
  (with-open-file (out file :direction :output :if-exists :supersede)
    (let ((line 1)
          (name (c-name-string (namestring file))))
      (dolist (item items)
        (with-input-from-string (in item)
          (loop for text = (read-line in nil)
                while text
                do (if (string= text +line-reset+)
                       (format out "#line ~d ~a~%" (1+ line) name)
                       (format out "~a~%" text))
                   (incf line)))
        (terpri out)
        (incf line)))))
