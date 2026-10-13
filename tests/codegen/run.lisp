;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Run the code generator's tests (Phase 4):
;;;;
;;;;     sbcl --script tests/codegen/run.lisp [quick]
;;;;
;;;; Each program in the cases*.lisp files is compiled to C once, then built and run
;;;; in four configurations -- gcc and clang, each with musttail and with
;;;; trampolines -- or, given quick, in the first only.  Every run has a
;;;; 256 KB stack.

(let ((here (directory-namestring (or *load-truename* *default-pathname-defaults*))))
  (let ((*standard-output* (make-broadcast-stream)))
    (load (merge-pathnames "../../compiler/load.lisp" here)))
  (defparameter cl-user::*codegen-test-directory* here))

(in-package #:dylan-compiler)

(defparameter *build-directory*
  (merge-pathnames "../../runtime/build/tests/" cl-user::*codegen-test-directory*))

(defparameter *configurations*
  (let ((all '(("gcc" nil) ("gcc" t) ("clang" nil) ("clang" t))))
    (if (member "quick" sb-ext:*posix-argv* :test #'string=) (list (first all)) all)))

(defun run-captured (command)
  "Run COMMAND with sh; answer its exit code, standard output and error."
  (let* ((out (make-string-output-stream))
         (err (make-string-output-stream))
         (process (sb-ext:run-program "/bin/sh" (list "-c" command)
                                      :output out :error err)))
    (values (sb-ext:process-exit-code process)
            (get-output-stream-string out)
            (get-output-stream-string err))))

(defun run-case (case)
  "A list of failure descriptions for CASE."
  (destructuring-bind (name source expected &key error compile-error) case
    (let* ((stem (merge-pathnames (string-downcase (symbol-name name)) *build-directory*))
           (c-file (format nil "~a.c" (namestring stem)))
           (diagnostics (make-string-output-stream)))
      (ensure-directories-exist stem)
      (let ((ok (let ((*error-output* diagnostics))
                  (compile-program (front-end-string source :library t) c-file)))
            (text (get-output-stream-string diagnostics)))
        (cond (compile-error
               (return-from run-case
                 (unless (and (not ok) (search compile-error text))
                   (list (format nil "~(~a~): expected the compile error ~s, got:~%~a"
                                 name compile-error text)))))
              ((not ok)
               (return-from run-case
                 (list (format nil "~(~a~): does not compile:~%~a" name text))))))
      (loop for (cc trampoline) in *configurations*
            for exe = (format nil "~a-~a~:[~;-trampoline~]" (namestring stem) cc trampoline)
            for config = (format nil "~(~a~) [~a~:[~;, trampoline~]]" name cc trampoline)
            append (let ((*standard-output* (make-broadcast-stream)))
                     (if (not (build-c c-file exe :trampoline trampoline :cc cc))
                         (list (format nil "~a: the C compiler failed on ~a" config c-file))
                         (multiple-value-bind (status out err)
                             (run-captured (format nil "ulimit -s 256; exec ~a" exe))
                           (append
                            (unless (string= out expected)
                              (list (format nil "~a: output~%~a~%expected~%~a" config out expected)))
                            (if error
                                (unless (and (= status 1) (search error err))
                                  (list (format nil "~a: expected error ~s, got status ~d: ~a"
                                                config error status err)))
                                (unless (zerop status)
                                  (list (format nil "~a: status ~d: ~a" config status err))))))))))))

(defun run-tests ()
  (let* ((cases (loop for file in (directory (merge-pathnames "cases*.lisp"
                                                             cl-user::*codegen-test-directory*))
                      append (with-open-file (in file)
                               (let ((*package* (find-package '#:dylan-compiler)))
                                 (read in)))))
         (failures (loop for case in cases append (run-case case))))
    (dolist (f failures) (format t "FAIL ~a~%" f))
    (format t "~d program~:p in ~d configuration~:p, ~d failure~:p~%"
            (length cases) (length *configurations*) (length failures))
    (null failures)))

(sb-ext:exit :code (if (run-tests) 0 1))
