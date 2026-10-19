;;;; Copyright 2026 Blake McBride.  See ../LICENSE.
;;;;
;;;; Compare interpreted Dylan, Dylan compiled to C, and plain C on the
;;;; benchmarks, as one table:
;;;;
;;;;     $ sbcl --script bench/compare.lisp [name ...]      (or: make bench)
;;;;
;;;; It runs run.lisp (the Common Lisp implementation) and run-c.lisp (the
;;;; compiler to C, and reference.c) and reads their times, in
;;;; milliseconds.  The ratio is the compiled Dylan's time to the C's,
;;;; marked ~ when the C takes under 10 ms, too little to measure well.
;;;; BENCH_SCALE, if set, multiplies every size, as in both.

(defparameter *bench-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defun run-driver (script names)
  "The lines run.lisp or run-c.lisp prints, given the benchmark NAMES."
  (with-output-to-string (out)
    (sb-ext:run-program "sbcl"
                        (append (list "--script"
                                      (namestring (merge-pathnames script *bench-directory*)))
                                (and names (cons "--" names)))
                        :search t :output out :error nil)))

(defun words (line)
  (let ((words '()) (start nil))
    (loop for i from 0 to (length line)
          for c = (if (< i (length line)) (char line i) #\Space)
          do (cond ((member c '(#\Space #\Tab))
                    (when start (push (subseq line start i) words) (setf start nil)))
                   ((null start) (setf start i))))
    (nreverse words)))

(defun number-of (word)
  "WORD as milliseconds: \"116\", \"1.682s\" (seconds), or NIL."
  (let* ((seconds (and (plusp (length word)) (char= (char word (1- (length word))) #\s)))
         (text (if seconds (subseq word 0 (1- (length word))) word))
         (value (ignore-errors
                 (let ((*read-eval* nil))
                   (read-from-string text)))))
    (and (realp value) (if seconds (* 1000 value) value))))

(defun times (text column)
  "An alist: benchmark name -> its time in COLUMN (from 0) of TEXT's
table, in milliseconds."
  (with-input-from-string (in text)
    (loop for line = (read-line in nil)
          while line
          for w = (words line)
          for time = (and (> (length w) column) (number-of (nth column w)))
          when (and time (not (string= (first w) "benchmark")))
            collect (cons (first w) time))))

(defparameter *names* (cdr (member "--" sb-ext:*posix-argv* :test #'string=)))

(format *error-output* "Running the interpreted benchmarks...~%")
(defparameter *interpreted* (times (run-driver "run.lisp" *names*) 2))
(format *error-output* "Running the compiled and C benchmarks...~%")
(defparameter *compiled-text* (run-driver "run-c.lisp" *names*))
(defparameter *compiled* (times *compiled-text* 1))
(defparameter *c* (times *compiled-text* 2))

(defun ms (x) (if x (format nil "~:d" (round x)) "-"))

(defun ratio-text (compiled c)
  (cond ((or (null compiled) (null c) (zerop c)) "-")
        (t (let ((r (/ compiled c)))
             (concatenate 'string
                          (if (< c 10) "~" "")
                          (if (>= r 10) (format nil "~d" (round r)) (format nil "~,1f" r))
                          (string (code-char #x00D7)))))))

(let* ((headers '("Benchmark" "Interpreted Dylan" "Compiled Dylan" "C" "Ratio"))
       (rows (loop for (name . compiled) in *compiled*
                   collect (list name
                                 (ms (cdr (assoc name *interpreted* :test #'string=)))
                                 (ms compiled)
                                 (ms (cdr (assoc name *c* :test #'string=)))
                                 (ratio-text compiled (cdr (assoc name *c* :test #'string=))))))
       (widths (loop for i from 0 below (length headers)
                     collect (+ 2 (reduce #'max (mapcar (lambda (r) (length (nth i r)))
                                                        (cons headers rows))))))
       (out (sb-sys:make-fd-stream 1 :output t :external-format :utf-8 :buffering :full)))
  (flet ((rule (left middle right)
           (format out "~a~{~a~^~}~a~%" left
                   (loop for w in widths for first = t then nil
                         unless first collect middle
                         collect (make-string w :initial-element (code-char #x2500)))
                   right))
         (row (cells &optional header)
           (format out "~c" (code-char #x2502))
           (loop for cell in cells for w in widths for i from 0
                 do (format out "~a~c"
                            (cond (header
                                   ;; Centered
                                   (let* ((pad (- w (length cell))) (left (floor pad 2)))
                                     (format nil "~v@a~v@a" (+ left (length cell)) cell
                                             (- pad left) "")))
                                  ((zerop i) (format nil " ~va " (- w 2) cell))
                                  (t (format nil " ~v@a " (- w 2) cell)))
                            (code-char #x2502)))
           (terpri out)))
    (if (null rows)
        (format *error-output* "No benchmarks ran.~%")
        (progn
          (rule (code-char #x250C) (code-char #x252C) (code-char #x2510))
          (row headers t)
          (loop for (r . more) on rows
                do (rule (code-char #x251C) (code-char #x253C) (code-char #x2524))
                   (row r))
          (rule (code-char #x2514) (code-char #x2534) (code-char #x2518)))))
  (finish-output out))
