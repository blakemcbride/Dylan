;;;; Run the benchmarks compiled to C by the Dylan-to-C compiler, against
;;;; the hand-written C of reference.c and the plain Common Lisp of
;;;; reference.lisp.
;;;;
;;;;     $ sbcl --script bench/run-c.lisp [name ...]
;;;;
;;;; benchmarks.dyl is compiled with a driver that runs each benchmark three
;;;; times and prints its value and best time; reference.c, built with
;;;; cc -O2, does the same, and the reference functions are timed the same
;;;; way here.  Each line gives the three times, the ratios of the Dylan->C
;;;; time to the C and Lisp times ("-" when the other time is under a
;;;; millisecond), and MISMATCH if the three answered differently.  The
;;;; builds go in bench/build/.

(defparameter *bench-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defparameter *benchmarks*
  '(("table" 200000) ("sort" 100000) ("string" 200000) ("deque" 300000)
    ("stretchy" 300000) ("range" 1000000) ("exit" 300000) ("condition" 30000)
    ("next-method" 300000) ("tak" 300) ("float" 300)))

(defun bench-file (name) (merge-pathnames name *bench-directory*))

(let ((*standard-output* (make-broadcast-stream)))
  (load (merge-pathnames "../compiler/load.lisp" *bench-directory*)))

(defparameter *selected*
  (let ((names (cdr (member "--" sb-ext:*posix-argv* :test #'string=))))
    (if names
        (remove-if-not (lambda (b) (member (first b) names :test #'string=)) *benchmarks*)
        *benchmarks*)))

(ensure-directories-exist (bench-file "build/"))
(with-open-file (out (bench-file "build/driver.dyl") :direction :output :if-exists :supersede)
  (format out "(define run (method (name f n)
  (bind ((best #f) (value #f))
    (dotimes (i 3)
      (bind ((start (current-milliseconds)))
        (set! value (f n))
        (bind ((elapsed (- (current-milliseconds) start)))
          (when (or (not best) (< elapsed best)) (set! best elapsed)))))
    (format #t \"~~a ~~s ~~a~~%\" name value best))))~%")
  (dolist (b *selected*)
    (format out "(run ~s bench-~a ~d)~%" (first b) (first b) (second b))))

(unless (funcall (intern "BUILD" "DYLAN-COMPILER")
                 (list (bench-file "benchmarks.dyl") (bench-file "build/driver.dyl"))
                 (namestring (bench-file "build/benchmarks")))
  (format t "The benchmarks did not build.~%")
  (sb-ext:exit :code 1))

(unless (zerop (sb-ext:process-exit-code
                (sb-ext:run-program "cc" (list "-O2" "-o" (namestring (bench-file "build/reference-c"))
                                               (namestring (bench-file "reference.c")))
                                    :search t :output *standard-output* :error *standard-output*)))
  (format t "reference.c did not build.~%")
  (sb-ext:exit :code 1))

(load (compile-file (bench-file "reference.lisp")
                    :output-file (bench-file "build/reference.fasl")))

(defun best-time (function size)
  (let ((best nil) (value nil))
    (dotimes (i 3)
      (let ((start (get-internal-real-time)))
        (setf value (funcall function size))
        (let ((ms (/ (- (get-internal-real-time) start)
                     (/ internal-time-units-per-second 1000.0))))
          (setf best (if best (min best ms) ms)))))
    (values value best)))

;;; The lines a benchmark program prints, each read as (NAME VALUE MS).
(defun program-results (program arguments)
  (with-input-from-string
      (in (with-output-to-string (out)
            (sb-ext:run-program (namestring (bench-file program))
                                arguments :output out)))
    (loop for line = (read-line in nil) while line
          collect (with-input-from-string (l line) (list (read l) (read l) (read l))))))

;;; The ratio of DYLAN-MS to OTHER-MS, or "-" when OTHER-MS is too small.
(defun time-ratio (dylan-ms other-ms)
  (if (< other-ms 0.5) "-" (format nil "~,2f" (/ dylan-ms other-ms))))

(let ((dylan (program-results "build/benchmarks" '()))
      (c (program-results "build/reference-c" (mapcar #'first *selected*))))
  (format t "~&~12a ~12@a ~8@a ~8@a ~10@a ~8@a~%"
          "benchmark" "Dylan->C ms" "C ms" "D/C" "Lisp ms" "D/Lisp")
  (loop for (name size) in *selected*
        for (nil value ms) in dylan
        for (nil c-value c-ms) in c
        do (multiple-value-bind (lisp-value lisp-ms)
               (best-time (symbol-function
                           (find-symbol (format nil "BENCH-~:@(~a~)" name)
                                        "DYLAN-BENCH-REFERENCE"))
                          size)
             (format t "~12a ~12d ~8d ~8@a ~10,1f ~8@a~:[  MISMATCH ~s / ~s / ~s~;~]~%"
                     name ms c-ms (time-ratio ms c-ms) lisp-ms (time-ratio ms lisp-ms)
                     (and (equal value c-value) (equal value lisp-value))
                     value c-value lisp-value))))
