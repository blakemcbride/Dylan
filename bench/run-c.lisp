;;;; Run the benchmarks compiled to C by the Dylan-to-C compiler, against
;;;; the plain Common Lisp of reference.lisp.
;;;;
;;;;     $ sbcl --script bench/run-c.lisp [name ...]
;;;;
;;;; benchmarks.dyl is compiled with a driver that runs each benchmark three
;;;; times and prints its value and best time; the reference functions are
;;;; timed the same way here.  Each line gives both times, their ratio, and
;;;; MISMATCH if the two answered differently.  The build goes in
;;;; bench/build/.

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

(let ((lines (with-input-from-string
                 (in (with-output-to-string (out)
                       (sb-ext:run-program (namestring (bench-file "build/benchmarks")) '()
                                           :output out)))
               (loop for line = (read-line in nil) while line collect line))))
  (format t "~&~12a ~12a ~12a ~8a~%" "benchmark" "Dylan->C ms" "Lisp ms" "ratio")
  (loop for (name size) in *selected*
        for line in lines
        do (destructuring-bind (dname value ms)
               (with-input-from-string (in line) (list (read in) (read in) (read in)))
             (declare (ignore dname))
             (multiple-value-bind (lisp-value lisp-ms)
                 (best-time (symbol-function
                             (find-symbol (format nil "BENCH-~:@(~a~)" name)
                                          "DYLAN-BENCH-REFERENCE"))
                            size)
               (format t "~12a ~12d ~12,1f ~8,2f~:[  MISMATCH ~s vs ~s~;~]~%"
                       name ms lisp-ms (/ ms (max lisp-ms 0.1))
                       (equal value lisp-value) value lisp-value)))))
