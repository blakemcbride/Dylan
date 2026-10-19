;;;; Copyright 2026 Blake McBride.  See ../../LICENSE.
;;;;
;;;; Running test programs, for tests/codegen/run.lisp and
;;;; tests/conformance/run.lisp.  A file of tests holds a list of
;;;;
;;;;   (name "dylan source" "expected standard output" [:error "text"]
;;;;                                                    [:compile-error "text"])
;;;;
;;;; Each program is compiled to C once, then built and run in each
;;;; configuration of a MODE, which the command line names (the default
;;;; is all):
;;;;
;;;;   all        gcc and clang, each with musttail and with trampolines;
;;;;              only the first optimized
;;;;   quick      the first of those only
;;;;   whole      as quick, with the library compiled with the program and
;;;;              tree shaken (shake.lisp), not apart
;;;;   gc-stress  gcc, musttail and trampolines, a collection every 1000
;;;;              allocations (CompilerPlan.md, Phase 11)
;;;;   address    AddressSanitizer and UndefinedBehaviorSanitizer, gcc and
;;;;              clang, in debug builds
;;;;   thread     ThreadSanitizer, gcc, in a debug build
;;;;
;;;; Every run has a 256 KB stack, so a tail call that is not a jump
;;;; overflows it.  The output must be exactly the expected text; with
;;;; :error, the program must instead stop with an error whose message
;;;; contains the text, after writing the expected output; with
;;;; :compile-error, the compiler must report an error containing the text.
;;;; The other command-line arguments name the test files or tests to run.

(let ((*standard-output* (make-broadcast-stream)))
  (load (merge-pathnames "../../compiler/load.lisp"
                         (directory-namestring *load-truename*))))

(in-package #:dylan-compiler)

(defparameter *build-directory*
  (merge-pathnames "../../runtime/build/tests/" (directory-namestring *load-truename*)))

(defparameter *modes*
  ;; mode -> configurations, each (cc trampoline optimize variant), and
  ;; whether the C is generated for debugging
  '(("all" nil ("gcc" nil "-O2" nil) ("gcc" t "-O0" nil) ("clang" nil "-O0" nil)
     ("clang" t "-O0" nil))
    ("quick" nil ("gcc" nil "-O2" nil))
    ("whole" nil ("gcc" nil "-O2" nil))
    ("gc-stress" nil ("gcc" nil "-O2" :gc-stress) ("gcc" t "-O2" :gc-stress))
    ("address" t ("gcc" nil "-O1" :address) ("clang" nil "-O1" :address))
    ("thread" t ("gcc" nil "-O1" :thread))))

(defun run-captured (command)
  "Run COMMAND with sh; answer its exit code, standard output and error."
  (let* ((out (make-string-output-stream))
         (err (make-string-output-stream))
         (process (sb-ext:run-program "/bin/sh" (list "-c" command)
                                      :output out :error err)))
    (values (sb-ext:process-exit-code process)
            (get-output-stream-string out)
            (get-output-stream-string err))))

(defparameter *run-environment*
  "ulimit -s 256; export DYLAN_GC_STRESS=1000 ASAN_OPTIONS=detect_stack_use_after_return=0:detect_leaks=0 UBSAN_OPTIONS=halt_on_error=1:print_stacktrace=1 TSAN_OPTIONS=halt_on_error=1"
  "What each test program runs with.  The variables are exported, since
assignments on their own, before the semicolon, would stay the shell's.
AddressSanitizer's fake stack would hide references from the collectors:
the runtime turns it off itself (object.c), and this says so too.")

(defun run-case (case configurations debug mode)
  "A list of failure descriptions for CASE, built for MODE."
  (destructuring-bind (name source expected &key error compile-error) case
    (let* ((stem (merge-pathnames (format nil "~:[~*~;~a/~]~(~a~)" (string/= mode "all") mode
                                          (symbol-name name))
                                  *build-directory*))
           (c-file (format nil "~a.c" (namestring stem)))
           (diagnostics (make-string-output-stream))
           (c-files nil))
      (ensure-directories-exist stem)
      (let ((ok (let ((*error-output* diagnostics)
                      (*debug* debug))
                  (setf c-files
                        (compile-program (front-end-string source :library t) c-file))))
            (text (get-output-stream-string diagnostics)))
        (cond (compile-error
               (return-from run-case
                 (unless (and (not ok) (search compile-error text))
                   (list (format nil "~(~a~): expected the compile error ~s, got:~%~a"
                                 name compile-error text)))))
              ((not ok)
               (return-from run-case
                 (list (format nil "~(~a~): does not compile:~%~a" name text))))))
      (loop for (cc trampoline optimize variant) in configurations
            for exe = (format nil "~a-~a~:[~;-trampoline~]~@[-~(~a~)~]"
                              (namestring stem) cc trampoline variant)
            for config = (format nil "~(~a~) [~a~:[~;, trampoline~]~@[, ~(~a~)~]]"
                                 name cc trampoline variant)
            append (let ((*standard-output* (make-broadcast-stream)))
                     (if (not (build-c c-files exe :trampoline trampoline :cc cc
                                                   :optimize optimize :variant variant))
                         (list (format nil "~a: the C compiler failed on ~a" config c-file))
                         (multiple-value-bind (status out err)
                             (run-captured (format nil "~a; ~:[~;export GC_MARKERS=1; ~]exec ~a" *run-environment*
                                                   ;; Parallel marking, collecting
                                                   ;; this often, only spins.
                                                   (eq variant :gc-stress) exe))
                           (append
                            (unless (string= out expected)
                              (list (format nil "~a: output~%~a~%expected~%~a" config out expected)))
                            (if error
                                (unless (and (= status 1) (search error err))
                                  (list (format nil "~a: expected error ~s, got status ~d: ~a"
                                                config error status err)))
                                (unless (zerop status)
                                  (list (format nil "~a: status ~d: ~a" config status err))))))))))))

(defun read-cases (file)
  (with-open-file (in file)
    (let ((*package* (find-package '#:dylan-compiler)))
      (read in))))

(defun run-test-files (default-files arguments)
  "Run the tests of DEFAULT-FILES, or of the files and tests the command
line ARGUMENTS name, in the mode they name.  Exit with the status."
  (let* ((mode (or (find-if (lambda (a) (assoc a *modes* :test #'string=)) arguments) "all"))
         (entry (cdr (assoc mode *modes* :test #'string=)))
         (names (remove mode arguments :test #'string=))
         (files (or (remove-if-not (lambda (a) (search ".lisp" a)) names) default-files))
         (selected (remove-if (lambda (a) (search ".lisp" a)) names))
         (cases (loop for file in files
                      append (remove-if-not
                              (lambda (c)
                                (or (null selected)
                                    (member (string-downcase (symbol-name (first c))) selected
                                            :test #'string=)))
                              (read-cases file))))
         (failures (let ((*separate-library* (string/= mode "whole")))
                     (loop for case in cases
                           append (run-case case (rest entry) (first entry) mode)))))
    (dolist (f failures) (format t "FAIL ~a~%" f))
    (format t "~d program~:p in ~d configuration~:p, ~d failure~:p~%"
            (length cases) (length (rest entry)) (length failures))
    (sb-ext:exit :code (if failures 1 0))))
