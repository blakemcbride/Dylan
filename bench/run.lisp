;;;; Run the benchmarks: each BENCH-<NAME> of benchmarks.dyl against its
;;;; plain Common Lisp counterpart in reference.lisp.
;;;;
;;;;     $ sbcl --script bench/run.lisp [name ...]
;;;;
;;;; With names (table, sort, ...), only those run.  For each it prints the
;;;; best of three times for Dylan and for Lisp, their ratio, and whether
;;;; the two answered the same.  The compiled benchmarks go in bench/build/.

(defparameter *bench-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defparameter *benchmarks*
  ;; (name size)
  '(("table" 200000)
    ("sort" 100000)
    ("string" 200000)
    ("deque" 300000)
    ("stretchy" 300000)
    ("range" 1000000)
    ("exit" 300000)
    ("condition" 30000)
    ("next-method" 300000)
    ("tak" 300)
    ("float" 300)))

(defun bench-file (name)
  (merge-pathnames name *bench-directory*))

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (load (merge-pathnames "../src/load-dylan.lisp" *bench-directory*))
  (ensure-directories-exist (bench-file "build/"))
  (funcall (intern "COMPILE-FILE" "DYLAN")
           (namestring (bench-file "benchmarks.dyl"))
           (namestring (bench-file "build/benchmarks.lisp")))
  (load (compile-file (bench-file "build/benchmarks.lisp")))
  (load (compile-file (bench-file "reference.lisp")
                      :output-file (bench-file "build/reference.fasl"))))

(defun best-time (function size)
  "The value of FUNCTION on SIZE, and the shortest of three runs' times."
  (let ((best nil) (value nil))
    (dotimes (i 3)
      (let ((start (get-internal-real-time)))
        (setf value (funcall function size))
        (let ((seconds (/ (- (get-internal-real-time) start)
                          internal-time-units-per-second 1.0)))
          (setf best (if best (min best seconds) seconds)))))
    (values value best)))

(defun dylan-benchmark (name)
  (let ((function (symbol-value
                   (find-symbol (format nil "!MODULE/BENCH-~:@(~a~)/" name)
                                "DYLAN"))))
    (lambda (size)
      (funcall (intern "DYLAN-CALL" "DYLAN") function size))))

(defun lisp-benchmark (name)
  (symbol-function (find-symbol (format nil "BENCH-~:@(~a~)" name)
                                "DYLAN-BENCH-REFERENCE")))

(let ((names (cdr sb-ext:*posix-argv*)))
  (format t "~&~12a ~9@a ~10@a ~10@a ~8@a~%"
          "benchmark" "size" "dylan" "lisp" "ratio")
  (dolist (benchmark *benchmarks*)
    (destructuring-bind (name size) benchmark
      (when (or (null names) (member name names :test #'string=))
        (multiple-value-bind (dylan-value dylan-time)
            (best-time (dylan-benchmark name) size)
          (multiple-value-bind (lisp-value lisp-time)
              (best-time (lisp-benchmark name) size)
            (format t "~12a ~9d ~9,3fs ~9,3fs ~7,1fx~@[  MISMATCH: ~s~]~%"
                    name size dylan-time lisp-time
                    (/ dylan-time (max lisp-time 0.001))
                    (unless (equal dylan-value lisp-value)
                      (list dylan-value lisp-value)))
            (finish-output)))))))
