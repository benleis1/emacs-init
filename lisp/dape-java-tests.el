;; -*- lexical-binding: t; -*-
;; ERT tests for the pure/deterministic pieces of dape-java.el: the
;; RemoteTestRunner wire-protocol parser, the JUnit results tree it
;; feeds, and a few small standalone helpers. Anything that requires a
;; live eglot/jdtls connection (classpath resolution, launch argument
;; construction, the test-bundle fetcher, ...) is out of scope here --
;; it needs a real Java project to exercise meaningfully.
(require 'ert)

;; In the normal CI flow (see .github/workflows/test.yml) `init.el' is
;; loaded first, which pulls in real `eglot'/`dape' and, via
;; `with-eval-after-load', `dape-java.el' itself -- so the functions
;; under test are already defined by the time this file loads. This
;; fallback just makes the file independently runnable too (e.g.
;; `emacs -Q --batch -l ert -l dape-java-tests.el -f
;; ert-run-tests-batch-and-exit'), without requiring a real
;; eglot/dape/jdtls setup -- none of the functions exercised below
;; call into eglot at all.
;; Likewise `dape--repl-insert'/`dape--repl-insert-error' -- real
;; `dape.el' defines these, but standalone they're only used here to
;; mirror wire-protocol lines to the REPL, which none of these tests
;; assert on, so a no-op fallback is enough to let the parser run.
(unless (fboundp 'dape--repl-insert)
  (defun dape--repl-insert (string) (ignore string)))
(unless (fboundp 'dape--repl-insert-error)
  (defun dape--repl-insert-error (string) (ignore string)))

;; Same story for `dape-buffer-default'/`dape-cwd', used by
;; `dape-java--junit-resolve-filepath'.
(unless (fboundp 'dape-buffer-default)
  (defun dape-buffer-default () (file-relative-name (buffer-file-name) default-directory)))
(unless (fboundp 'dape-cwd)
  (defun dape-cwd () default-directory))

;; `dape-java.el' pushes onto `tab-line-exclude-modes', which is only
;; bound once `tab-line' (an Emacs built-in) is loaded -- normally
;; already the case by the time `dape-java.el' loads via `init.el',
;; but not in a bare `-Q' batch session.
(require 'tab-line)

(unless (fboundp 'dape-java--junit-handle-line)
  (load (expand-file-name "dape-java.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name default-directory)))))

(defun dape-java-tests--fake-process ()
  "Return a lightweight process object usable with `process-get'/
`process-put', without spawning a real subprocess."
  (make-pipe-process :name "dape-java-tests" :filter #'ignore :noquery t))

(defmacro dape-java-tests--with-process (var &rest body)
  "Bind VAR to a fake process for BODY, deleting it afterwards."
  (declare (indent 1))
  `(let ((,var (dape-java-tests--fake-process)))
     (unwind-protect (progn ,@body)
       (delete-process ,var))))

;;; `dape-java--junit-test-name'

(ert-deftest dape-java-test/junit-test-name-strips-id-prefix ()
  (should (equal (dape-java--junit-test-name "1,com.foo.BarTest#testOne")
                  "com.foo.BarTest#testOne")))

(ert-deftest dape-java-test/junit-test-name-passthrough-without-id ()
  (should (equal (dape-java--junit-test-name "no-id-here") "no-id-here")))

;;; `dape-java--junit-split-test-name'

(ert-deftest dape-java-test/split-test-name-junit4-paren-style ()
  (should (equal (dape-java--junit-split-test-name "testFoo(com.acme.BarTest)")
                  (cons "com.acme.BarTest" "testFoo"))))

(ert-deftest dape-java-test/split-test-name-hash-style ()
  (should (equal (dape-java--junit-split-test-name "com.acme.BarTest#testFoo")
                  (cons "com.acme.BarTest" "testFoo"))))

(ert-deftest dape-java-test/split-test-name-double-colon-style ()
  (should (equal (dape-java--junit-split-test-name "com.acme.BarTest::testFoo")
                  (cons "com.acme.BarTest" "testFoo"))))

(ert-deftest dape-java-test/split-test-name-unrecognized-falls-back-to-nil-class ()
  (should (equal (dape-java--junit-split-test-name "justAName")
                  (cons nil "justAName"))))

;;; `dape-java--junit-simple-class-name'

(ert-deftest dape-java-test/simple-class-name-strips-package ()
  (should (equal (dape-java--junit-simple-class-name "com.acme.pkg.BarTest") "BarTest")))

(ert-deftest dape-java-test/simple-class-name-no-package-is-unchanged ()
  (should (equal (dape-java--junit-simple-class-name "BarTest") "BarTest")))

(ert-deftest dape-java-test/simple-class-name-nil-is-nil ()
  (should (null (dape-java--junit-simple-class-name nil))))

;;; `dape-java--junit-rewrite-port'

(ert-deftest dape-java-test/rewrite-port-replaces-value-after-flag ()
  (should (equal (dape-java--junit-rewrite-port '("-jar" "x.jar" "-port" "0" "-quiet") 5005)
                  '("-jar" "x.jar" "-port" "5005" "-quiet"))))

(ert-deftest dape-java-test/rewrite-port-no-op-without-flag ()
  (should (equal (dape-java--junit-rewrite-port '("-jar" "x.jar") 5005)
                  '("-jar" "x.jar"))))

(ert-deftest dape-java-test/rewrite-port-does-not-mutate-input ()
  (let ((original '("-port" "0")))
    (dape-java--junit-rewrite-port original 5005)
    (should (equal original '("-port" "0")))))

;;; `dape-java--gradle-project-script'

(ert-deftest dape-java-test/gradle-project-script-detects-groovy ()
  (let ((dir (make-temp-file "dape-java-test-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "build.gradle" dir))
          (should (equal (dape-java--gradle-project-script dir) "build.gradle")))
      (delete-directory dir t))))

(ert-deftest dape-java-test/gradle-project-script-detects-kotlin-dsl ()
  (let ((dir (make-temp-file "dape-java-test-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "build.gradle.kts" dir))
          (should (equal (dape-java--gradle-project-script dir) "build.gradle.kts")))
      (delete-directory dir t))))

(ert-deftest dape-java-test/gradle-project-script-nil-when-neither ()
  (let ((dir (make-temp-file "dape-java-test-" t)))
    (unwind-protect
        (should (null (dape-java--gradle-project-script dir)))
      (delete-directory dir t))))

;;; `dape-java--junit-flatten-items'

(ert-deftest dape-java-test/flatten-items-depth-first ()
  (let ((items `((:name "A" :children ((:name "A1") (:name "A2")))
                 (:name "B"))))
    (should (equal (mapcar (lambda (it) (plist-get it :name))
                           (dape-java--junit-flatten-items items))
                   '("A" "A1" "A2" "B")))))

(ert-deftest dape-java-test/flatten-items-empty ()
  (should (null (dape-java--junit-flatten-items nil))))

;;; Item accessors

(ert-deftest dape-java-test/junit-item-testlevel ()
  (should (eql (dape-java-junit-item-testlevel '(:testLevel 6)) 6)))

(ert-deftest dape-java-test/junit-item-line ()
  (should (eql (dape-java-junit-item-line '(:range (:start (:line 41)))) 41)))

;;; `dape-java--junit-resolve-filepath'
;;
;; `dape-restart', once a run's one-shot JVM has already exited, falls
;; back to re-evaluating the raw `dape-configs' template from
;; `dape-history' -- from whatever buffer is current when the
;; toolbar's restart button is clicked, i.e. `*dape-repl*', which has
;; no file of its own. These pin down the fallback that fixes that.

(ert-deftest dape-java-test/resolve-filepath-remembers-current-buffer-file ()
  (let* ((dape-java--last-junit-filepath nil)
         (file (make-temp-file "dape-java-test-")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (should (equal (dape-java--junit-resolve-filepath) (expand-file-name file)))
          (should (equal dape-java--last-junit-filepath (expand-file-name file))))
      (when-let* ((buf (get-file-buffer file))) (kill-buffer buf))
      (delete-file file))))

(ert-deftest dape-java-test/resolve-filepath-falls-back-when-buffer-has-no-file ()
  (let ((dape-java--last-junit-filepath "/tmp/remembered/BarTest.java"))
    (with-temp-buffer
      (should (equal (dape-java--junit-resolve-filepath) "/tmp/remembered/BarTest.java")))))

(ert-deftest dape-java-test/resolve-filepath-errors-without-file-or-fallback ()
  (let ((dape-java--last-junit-filepath nil))
    (with-temp-buffer
      (should-error (dape-java--junit-resolve-filepath) :type 'user-error))))

;;; RemoteTestRunner wire protocol end-to-end, via `dape-java--junit-handle-line'

(ert-deftest dape-java-test/handle-line-tallies-pass-fail-error-skip ()
  (dape-java-tests--with-process proc
    (dolist (line '("%TESTC  4 v2"
                     "%TESTS 1,testOne(com.foo.BarTest)"
                     "%TESTE 1,testOne(com.foo.BarTest)"
                     "%TESTS 2,testTwo(com.foo.BarTest)"
                     "%FAILED"
                     "%TESTE 2,testTwo(com.foo.BarTest)"
                     "%TESTS 3,testThree(com.foo.BarTest)"
                     "%ERROR"
                     "%TESTE 3,testThree(com.foo.BarTest)"
                     "%TESTS 4,testFour(com.foo.BarTest)"
                     "%TESTI"
                     "%TESTE 4,testFour(com.foo.BarTest)"))
      (dape-java--junit-handle-line proc line))
    (should (eql (process-get proc :junit-pass) 1))
    (should (eql (process-get proc :junit-fail) 1))
    (should (eql (process-get proc :junit-error) 1))
    (should (eql (process-get proc :junit-skip) 1))))

(ert-deftest dape-java-test/handle-line-records-results-in-order ()
  (dape-java-tests--with-process proc
    (dolist (line '("%TESTC  2 v2"
                     "%TESTS 1,testOne(com.foo.BarTest)"
                     "%TESTE 1,testOne(com.foo.BarTest)"
                     "%TESTS 2,testTwo(com.foo.BarTest)"
                     "%FAILED"
                     "%TESTE 2,testTwo(com.foo.BarTest)"))
      (dape-java--junit-handle-line proc line))
    (should (equal (reverse (process-get proc :junit-results))
                    '(("testOne(com.foo.BarTest)" . pass)
                      ("testTwo(com.foo.BarTest)" . fail))))))

(ert-deftest dape-java-test/handle-line-testc-resets-tally-and-results ()
  (dape-java-tests--with-process proc
    (dolist (line '("%TESTC  1 v2"
                     "%TESTS 1,testOne(com.foo.BarTest)"
                     "%FAILED"
                     "%TESTE 1,testOne(com.foo.BarTest)"))
      (dape-java--junit-handle-line proc line))
    (should (eql (process-get proc :junit-fail) 1))
    ;; A second run should start from a clean slate.
    (dape-java--junit-handle-line proc "%TESTC  1 v2")
    (should (eql (process-get proc :junit-pass) 0))
    (should (eql (process-get proc :junit-fail) 0))
    (should (null (process-get proc :junit-results)))))

(ert-deftest dape-java-test/results-tree-groups-by-class-strips-package-omits-skips ()
  (dape-java-tests--with-process proc
    (dolist (line '("%TESTC  4 v2"
                     "%TESTS 1,testOne(com.foo.BarTest)"
                     "%TESTE 1,testOne(com.foo.BarTest)"
                     "%TESTS 2,testTwo(com.foo.BarTest)"
                     "%FAILED"
                     "%TESTE 2,testTwo(com.foo.BarTest)"
                     "%TESTS 3,testThree(com.foo.BarTest)"
                     "%TESTI"
                     "%TESTE 3,testThree(com.foo.BarTest)"
                     "%TESTS 4,testAlpha(com.foo.other.BazTest)"
                     "%TESTE 4,testAlpha(com.foo.other.BazTest)"))
      (dape-java--junit-handle-line proc line))
    (with-current-buffer (dape-java--junit-results-buffer)
      (let ((text (buffer-string)))
        ;; Package-qualified names never appear -- only the simple class name.
        (should-not (string-match-p "com\\.foo" text))
        (should (string-match-p "^BarTest$" text))
        (should (string-match-p "^BazTest$" text))
        ;; Pass/fail marks, method-only names.
        (should (string-match-p "✔ testOne" text))
        (should (string-match-p "✘ testTwo" text))
        (should (string-match-p "✔ testAlpha" text))
        ;; The skipped method is omitted from the tree entirely.
        (should-not (string-match-p "testThree" text))))))

(ert-deftest dape-java-test/results-summary-omits-zero-counts ()
  (dape-java-tests--with-process proc
    (dolist (line '("%TESTC  1 v2"
                     "%TESTS 1,testOne(com.foo.BarTest)"
                     "%TESTE 1,testOne(com.foo.BarTest)"))
      (dape-java--junit-handle-line proc line))
    (with-current-buffer (dape-java--junit-results-buffer)
      (let ((text (buffer-string)))
        (should (string-match-p "^1 passed$" text))
        (should-not (string-match-p "failed" text))
        (should-not (string-match-p "error" text))))))

(ert-deftest dape-java-test/results-summary-includes-each-nonzero-count-on-own-line ()
  (dape-java-tests--with-process proc
    (dolist (line '("%TESTC  3 v2"
                     "%TESTS 1,testOne(com.foo.BarTest)"
                     "%TESTE 1,testOne(com.foo.BarTest)"
                     "%TESTS 2,testTwo(com.foo.BarTest)"
                     "%FAILED"
                     "%TESTE 2,testTwo(com.foo.BarTest)"
                     "%TESTS 3,testThree(com.foo.BarTest)"
                     "%ERROR"
                     "%TESTE 3,testThree(com.foo.BarTest)"))
      (dape-java--junit-handle-line proc line))
    (with-current-buffer (dape-java--junit-results-buffer)
      (let ((text (buffer-string)))
        (should (string-match-p "^1 passed$" text))
        (should (string-match-p "^1 failed$" text))
        (should (string-match-p "^1 errors$" text))))))

;;; `dape-java--junit-results-close'

(ert-deftest dape-java-test/results-close-kills-buffer-and-window ()
  (dape-java--junit-results-buffer)
  (should (get-buffer dape-java-junit-results-buffer-name))
  (dape-java--junit-results-close)
  (should-not (get-buffer dape-java-junit-results-buffer-name)))

(ert-deftest dape-java-test/results-close-is-a-noop-when-buffer-absent ()
  (when-let* ((buf (get-buffer dape-java-junit-results-buffer-name)))
    (kill-buffer buf))
  (should-not (get-buffer dape-java-junit-results-buffer-name))
  ;; Must not signal with nothing to clean up.
  (dape-java--junit-results-close))

(ert-deftest dape-java-test/results-close-also-kills-leftover-compilation-buffer ()
  "The Gradle classpath-resolution `*compilation*' buffer (see
`dape-java--junit-gradle-compile-command') is a one-shot precondition
for the launch, not worth keeping around once the session ends."
  (get-buffer-create "*compilation*")
  (should (get-buffer "*compilation*"))
  (dape-java--junit-results-close)
  (should-not (get-buffer "*compilation*")))

(ert-deftest dape-java-test/results-close-fires-when-repl-buffer-is-killed ()
  "Mirrors the `dape-repl-mode-hook' wiring in `dape-java.el': the
results buffer's death is tied to the repl buffer's own
`kill-buffer-hook', so any way of killing `*dape-repl*' -- the
toolbar's quit button or `q', both `dape-quit' -- takes it down too."
  (dape-java--junit-results-buffer)
  (should (get-buffer dape-java-junit-results-buffer-name))
  (let ((repl (get-buffer-create "*dape-repl*")))
    (unwind-protect
        (progn
          (with-current-buffer repl
            (add-hook 'kill-buffer-hook #'dape-java--junit-results-close nil t))
          (kill-buffer repl)
          (should-not (get-buffer dape-java-junit-results-buffer-name)))
      (when (buffer-live-p repl) (kill-buffer repl)))))

;;; Line-buffering filter/sentinel

(ert-deftest dape-java-test/listener-filter-buffers-partial-lines ()
  (dape-java-tests--with-process proc
    (dape-java--junit-listener-filter proc "%TESTC  1 v2\n%TESTS 1,testOne(com.foo")
    ;; Nothing dispatched yet for the still-incomplete second line, and
    ;; the first line's already been consumed.
    (should (eql (process-get proc :junit-pass) 0))
    (should (equal (process-get proc :junit-pending) "%TESTS 1,testOne(com.foo"))
    (dape-java--junit-listener-filter proc ".BarTest)\n%TESTE 1,testOne(com.foo.BarTest)\n")
    (should (eql (process-get proc :junit-pass) 1))
    (should (equal (process-get proc :junit-pending) ""))))

(ert-deftest dape-java-test/listener-sentinel-flushes-trailing-unterminated-line ()
  (dape-java-tests--with-process proc
    ;; `%RUNTIME's final message famously arrives with no trailing newline.
    (dape-java--junit-listener-filter proc "%TESTC  1 v2\n%RUNTIME12")
    (should (equal (process-get proc :junit-pending) "%RUNTIME12"))
    (delete-process proc)
    (dape-java--junit-listener-sentinel proc "finished\n")
    (should (equal (process-get proc :junit-pending) ""))))

(provide 'dape-java-tests)
