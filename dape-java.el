;; -*- lexical-binding: t; -*-

;;       .o8
;;      "888
;;  .oooo888   .oooo.   oo.ooooo.   .ooooo.
;; d88' `888  `P  )88b   888' `88b d88' `88b
;; 888   888   .oP"888   888   888 888ooo888
;; 888   888  d8(  888   888   888 888    .o
;; `Y8bod88P" `Y888""8o  888bod8P' `Y8bod8P'
;;                       888
;;                      o888o

;; First phase in refactoring dape - jdtls support
;; TODO
;;  1. add some sanity tests.

;; # Prerequisites:

;; JUnit test debugging via jdtls needs two extra artifacts that aren't
;; part of a normal jdtls/vscode-java install:

;; 1. The vscode-java-test OSGi bundle jars, expected in
;;    `dape-java-test-bundles-dir' (default ~/.emacs.d/jdtls-bundles/).
;;    They aren't published to Maven Central -- they ship inside the
;;    `vscjava.vscode-java-test' VS Code extension's `extension/server/'
;;    folder. `M-x dape-java-fetch-test-bundle' automates fetching and
;;    unpacking them (see `dape-java-test-bundle-version' for which
;;    version); equivalently, by hand:
;;      curl -L -o /tmp/vscode-java-test.vsix \
;;        "https://open-vsx.org/api/vscjava/vscode-java-test/<version>/file/vscjava.vscode-java-test-<version>.vsix"
;;      unzip /tmp/vscode-java-test.vsix -d /tmp/vjt
;;      mkdir -p ~/.emacs.d/jdtls-bundles
;;      cp /tmp/vjt/extension/server/*.jar ~/.emacs.d/jdtls-bundles/
;;    Pick the current version from
;;    https://open-vsx.org/extension/vscjava/vscode-java-test -- this
;;    is the only place still serving prebuilt versioned vsix files
;;    for this extension (GitHub Releases stopped attaching them as
;;    of 0.39.1, and they were never on Maven Central or a p2 site).

;; 2. The microsoft java debug plugin jar, expected at
;;    `dape-java-ms-debug-plugin' (default under
;;    ~/.m2/repository/com/microsoft/java/com.microsoft.java.debug.plugin/).
;;    Unlike the test bundle, it is published to Maven Central, so
;;    fetching it into the local repo is a plain:
;;      mvn dependency:get \
;;        -Dartifact=com.microsoft.java:com.microsoft.java.debug.plugin:<version>
;;    Pick the current version from
;;    https://mvnrepository.com/artifact/com.microsoft.java/com.microsoft.java.debug.plugin,
;;    then update `dape-java-ms-debug-plugin' to match if it differs from
;;    the default above.

;; # General Usage:

;; 1. load this file which will setup dape configurations for both running all
;;    the tests or a single test at the cursor.  This can then be invoked by
;;    calling the regular `dape` command. The configuration names are
;;    jdtls-junit and jdtls-junit-method.  A test summary listener is hooked up
;;    by default.

;; 2. additional support is included for potential UI integration like margin
;;    icons for tests.  See dape-java-junit-fetch-items-async which can retrieve
;;    all the lines with tests.

;; 3. `dape-java-diagnostics` is provided for troubleshooting if dape doesn't work.

;; 4. `dape-java-get-test-bundle-vector` is meant for use in the jdtls bundle setup.
;;    `dape-java-fetch-test-bundle` automates populating
;;    `dape-java-test-bundles-dir` in the first place -- see Prerequisites above.

;; Example usage:
;; (add-to-list 'eglot-server-programs
;;              `((java-mode java-ts-mode)
;;                . ,(lambda (&optional _interactive project)
;;                     (let* ((cache-dir (my-jdtls-cache-dir project)))
;;                       (list "jdtls"
;;                             "--jvm-arg=-Djava.import.generatesMetadataFilesAtProjectRoot=false"
;;                             "-data" cache-dir
;;                             :initializationOptions
;;                             (list :settings my-jdtls-settings
;; 				    :bundles (dape-java-get-test-bundle-vector)
;; 				    ))))))

;; Custom variables of note:
;;1. dape-java-ms-debug-plugin supplies the path to the ms java debug plugin
;; jar.
;;2. dape-java-use-gradle-for-classpath. If set to t use the slower but
;; more reliable gradle mechanism for classpath determination
;;3. dape-java-test-bundles-dir. Is set to to jdtls-bundles under the emacs config directory
;;    and is where the microsoft jdtls debug bundle is expected to be installed.
(defgroup dape-java nil
  "Dape configs and support for debugging/testing Java via jdtls."
  :group 'dape)

(defun dape-java-jdtls-support-p ()
  "Return non-nil if the current buffer's jdtls bundles the java-test
plugin and the needed functions."
  (let ((commands (eglot-server-capable :executeCommandProvider :commands)))
    (and (seq-contains-p commands "vscode.java.test.junit.argument" #'equal)
         (seq-contains-p commands dape-java--junit-search-command #'equal))))

;; Run various diagnostics for debugging if things go wrong.
(defun dape-java-diagnostics ()
  (interactive)
  (if (eglot-current-server)
      (condition-case err
	  ;; Everything that touches eglot/the buffer being diagnosed has to
	  ;; happen here, before switching into *dape-diagnotics* below --
	  ;; `eglot-current-server' and `buffer-file-name' are buffer-local,
	  ;; so once inside `with-current-buffer' they'd be asking about the
	  ;; scratch output buffer instead of the Java buffer.
	  (let* ((server (eglot-current-server))
		 (jdtls-support (dape-java-jdtls-support-p))
		 (uri (eglot-path-to-uri (buffer-file-name)))
		 (test-file-p (eglot-execute-command server "java.project.isTestFile" (vector uri)))
		 (result (eglot-execute-command server "java.project.getClasspaths"
						 (vector uri (json-serialize '(:scope "test")))))
		 (test-items (dape-java--junit-flatten-items
			      (eglot-execute-command server dape-java--junit-search-command (vector uri)))))
	    (with-current-buffer (get-buffer-create "*dape-diagnostics*")
	      (erase-buffer)
	      (insert (format "Bundle is loaded and debug functions are available in jdtls: %s\n" jdtls-support))
	      (insert (format "Current buffer is considered a test file: %s\n" test-file-p))
	      (insert (format "project root: %s\n\n" (plist-get result :projectRoot)))
	      (insert (format "classpaths (%d):\n" (length (plist-get result :classpaths))))
	      (seq-each (lambda (e) (insert "  " e "\n")) (plist-get result :classpaths))
	      (insert (format "\nmodulepaths (%d):\n" (length (plist-get result :modulepaths))))
	      (seq-each (lambda (e) (insert "  " e "\n")) (plist-get result :modulepaths))
	      (insert (format "\ntests found (%d):\n" (length test-items)))
	      (seq-each (lambda (it)
			  (insert (format "  [%s] %s\n"
					  (pcase (dape-java-junit-item-testlevel it)
					    (5 "CLASS") (6 "METHOD")
					    (level (format "%s" level)))
					  (plist-get it :fullName))))
			test-items)
	      (goto-char (point-min))
	      (special-mode)
	      (pop-to-buffer "*dape-diagnostics*")))

	;; Message for any jsonrpc issues that occur along the way.
	(jsonrpc-error
	 (message "dape-java-diagnostics: jdtls request failed -- %s"
		   (or (alist-get 'jsonrpc-error-message (cddr err)) (cadr err)))))

	;; else
	(message "eglot is not running in this buffer"))
  )


(defconst dape-java--junit-search-command "vscode.java.test.findTestTypesAndMethods"
  "Command to ask jdtls for the JUnit classes/methods in a file.
The older `vscode.java.test.search.codelens' was removed in
vscode-java-test 0.31.0 no bundle worth installing still has it")

(defun dape-java--junit-flatten-items (items)
  "Flatten the ITEMS tree (classes with method/nested-class :children)
depth-first into a single list."
  (seq-mapcat (lambda (it) (cons it (dape-java--junit-flatten-items (plist-get it :children))))
              items))

(defun dape-java-junit-item-testlevel (item)
  "Return ITEM's TestLevel (5 = CLASS, 6 = METHOD)."
  (plist-get item :testLevel))

(defun dape-java--find-tests (server file-uri)
  "Ask SERVER for the JUnit test class item in FILE-URI."
  (let ((items (dape-java--junit-flatten-items
                (eglot-execute-command
                 server dape-java--junit-search-command (vector file-uri)))))
    (or (seq-find (lambda (it) (eql (dape-java-junit-item-testlevel it) 5)) items)
        (user-error "No JUnit test class found in %s" file-uri))))

(defun dape-java--gradle-project-script (module-dir)
  "Return the build file name either build.gradle or build.gradle.kts if
MODULE-DIR is a Gradle module or nil if not"
  (if (file-exists-p (expand-file-name "build.gradle" module-dir))
      "build.gradle"
    (if (file-exists-p (expand-file-name "build.gradle.kts" module-dir))
	"build.gradle.kts"
      nil)))

(defvar dape-java--junit-gradle-classpath-cache (make-hash-table :test #'equal)
    "Cache of MODULE-DIR -> (BUILD-GRADLE-MTIME . CLASSPATH-VECTOR).
See `dape-java--junit-gradle-classpath-cached'.")

;; The contents of the temporary gradle file to compute the classpath.
;; `dependsOn' each sourceSet's own compile task so that running this
;; task always rebuilds anything Gradle considers stale first.
(defconst dape-java--gradle-file-contents
  "allprojects { proj ->
    proj.plugins.withType(org.gradle.api.plugins.JavaBasePlugin) {
        proj.tasks.register('myDapeClasspath') {
            dependsOn proj.sourceSets.collect { ss -> ss.compileJavaTaskName }
            doLast {
                def files = [] as LinkedHashSet
                proj.sourceSets.each { ss -> files.addAll(ss.runtimeClasspath.files) }
                def cp = files.collect { it.absolutePath }.join(File.pathSeparator)
                println 'MYDAPE_CLASSPATH_BEGIN'
                println cp
                println 'MYDAPE_CLASSPATH_END'
                if (proj.hasProperty('dapeClasspathOut')) {
                    new File(proj.property('dapeClasspathOut')).text = cp
                }
            }
        }
    }
 }")

(defcustom dape-java-use-gradle-for-classpaths nil
  "If true then use gradle to resolve the classpath for tests rather
than relying on jdtls which has issues with some more complex gradle
projects."
  :type 'boolean
  :group 'dape-java)

(defcustom dape-java-junit-gradle-jvm-args "-Xmx2g"
  "Extra JVM args for the Gradle daemon. Currently bumping the heap space
to 2gig by default"
  :type '(choice (const :tag "Use Gradle's own default" nil) string)
  :group 'dape-java)

(defun dape-java--junit-gradle-classpath-file (module-dir)
  "Path of the file the `myDapeClasspath' task writes MODULE-DIR's
classpath to. See `dape-java--junit-gradle-compile-command'."
  (expand-file-name (format "dape-junit-classpath-%s.txt"
                             (secure-hash 'sha1 (expand-file-name module-dir)))
                     temporary-file-directory))

(defun dape-java--junit-gradle-classpath-cached (module-dir)
  "Return MODULE-DIR's Gradle classpath if it's already been resolved,
else nil.

Checked on every `dape-java--junit-fn' pass. A nil return means the
`myDapeClasspath' task started by
`dape-java--junit-gradle-compile-command' either hasn't run yet or
hasn't finished -- the caller is expected to gate the launch behind
that as a `compile' step rather than blocking here.

Cached per MODULE-DIR, keyed on its build file  mtime -- clear
`dape-java--junit-gradle-classpath-cache' to force a refresh."
  (let* ((gradle-file (dape-java--gradle-project-script module-dir))
	 (build-file (expand-file-name gradle-file module-dir))
         (mtime (file-attribute-modification-time (file-attributes build-file)))
         (cached (gethash module-dir dape-java--junit-gradle-classpath-cache)))
    (if (and cached (equal (car cached) mtime))
        (cdr cached)
      (let ((out-file (dape-java--junit-gradle-classpath-file module-dir)))
        (when (file-exists-p out-file)
          (let ((classpath (vconcat (split-string
                                      (with-temp-buffer
                                        (insert-file-contents out-file)
                                        (buffer-string))
                                      path-separator t))))
            ;; Consumed -- a stale leftover must never be mistaken for a
            ;; fresh result by a later cold-cache check.
            (delete-file out-file)
            (puthash module-dir (cons mtime classpath) dape-java--junit-gradle-classpath-cache)
            classpath))))))

(defun dape-java--junit-gradle-compile-command (module-dir)
  "Shell command that resolves MODULE-DIR's Gradle classpath asynchronously.

Meant to be used as a config's `compile' value: `dape' runs it through
`dape-compile-function' (by default plain `compile', so the output
lands in a normal read-only, `q'-to-quit `compilation-mode' buffer)
and only re-invokes `fn' once that finishes -- at which point
`dape-java--junit-gradle-classpath-cached' will find MODULE-DIR's
classpath already written and cached.

Shells out to Gradle through a throwaway `--init-script' (so the
project's own build files aren't touched) to collect every
sourceSet's `runtimeClasspath' -- this covers the module's own
compiled output/resources plus all resolved dependencies, including
sibling modules pulled in via composite builds, which jdtls's own
classpath resolution doesn't reliably surface for this project."
  (let* ((module-dir (expand-file-name module-dir))
         (gradlew-dir (or (locate-dominating-file module-dir "gradlew")
                           (user-error "No `gradlew' found above %s" module-dir)))
         (gradlew (expand-file-name "gradlew" gradlew-dir))
         (init-file (make-temp-file "dape-junit-classpath-" nil ".gradle"
                                     dape-java--gradle-file-contents))
         (out-file (dape-java--junit-gradle-classpath-file module-dir)))
    (format "rm -f %s; cd %s && %s --console=plain --init-script %s -PdapeClasspathOut=%s%s myDapeClasspath; ec=$?; rm -f %s; exit $ec"
            (shell-quote-argument out-file)
            (shell-quote-argument module-dir)
            (shell-quote-argument gradlew)
            (shell-quote-argument init-file)
            (shell-quote-argument out-file)
            (if dape-java-junit-gradle-jvm-args
                (concat " " (shell-quote-argument
                             (format "-Dorg.gradle.jvmargs=%s" dape-java-junit-gradle-jvm-args)))
              "")
            (shell-quote-argument init-file))))

(defun dape-java--junit-launch-arguments (server item)
  "Resolve the java launch arguments (mainClass, classpath, ...) for ITEM.
ITEM may be a CLASS-level or METHOD-level item. A METHOD item's
`:jdtHandler' is what `vscode.java.test.junit.argument' wants to scope
the launch to just that method.

The returned `:classpath' is only the runner's own scaffolding
classpath (test-runner + junit4/5 runtime jars) -- `dape-java--junit-fn'
still has to merge in the module's Gradle classpath before this is
launch-ready."
  (let* ((testlevel (dape-java-junit-item-testlevel item))
         (method-p (eql testlevel 6))
         (argument
          (json-serialize
           `((projectName . ,(plist-get item :projectName))
             (testLevel . ,testlevel)
             (testKind . ,(plist-get item :testKind))
             (testNames . ,(vector (if method-p
                                       (plist-get item :jdtHandler)
                                     (plist-get item :fullName))))))))
    (plist-get (eglot-execute-command server "vscode.java.test.junit.argument" (vector argument))
               :body)))

(defun dape-java--junit-test-name (payload)
  "Extract the human-readable test name from a \"id,name\" PAYLOAD."
  (if (string-match "\\`[0-9]+,\\(.*\\)\\'" payload)
      (match-string 1 payload)
    payload))

(defun dape-java--junit-report-test (proc)
  "Print PROC's just-finished test (name/status/trace) to the REPL."
  (let ((name (or (process-get proc :junit-cur) "?"))
        (status (or (process-get proc :junit-status) 'pass))
        (trace (nreverse (process-get proc :junit-trace))))
    (pcase status
      ('pass (process-put proc :junit-pass (1+ (process-get proc :junit-pass)))
             (dape--repl-insert (format "  PASS   %s\n" name)))
      ('skip (process-put proc :junit-skip (1+ (process-get proc :junit-skip)))
             (dape--repl-insert (format "  SKIP   %s\n" name)))
      (_ (let ((key (if (eq status 'error) :junit-error :junit-fail)))
           (process-put proc key (1+ (process-get proc key))))
         (dape--repl-insert-error
          (format "  %s  %s\n%s"
                  (if (eq status 'error) "ERROR " "FAILED")
                  name
                  (if trace
                      (concat (mapconcat (lambda (l) (concat "    " l)) trace "\n") "\n")
                    "")))))))

(defun dape-java--junit-report-summary (proc elapsed-ms)
  "Print PROC's final pass/fail/error/skip tally, ELAPSED-MS long, to the REPL."
  (dape--repl-insert
   (format "--- JUnit run finished in %sms: %d passed, %d failed, %d errors, %d skipped ---\n"
           (string-trim elapsed-ms)
           (process-get proc :junit-pass) (process-get proc :junit-fail)
           (process-get proc :junit-error) (process-get proc :junit-skip))))

(defun dape-java--junit-handle-line (proc line)
  "Decode one LINE of RemoteTestRunner's wire protocol for PROC.
See `org.eclipse.jdt.internal.junit.runner.MessageIds' for the message
format this parses: fixed \"%WORD\" markers, optionally followed by a
payload, with stack traces/diffs sent as raw lines wrapped between a
\"%..S\"/\"%..E\" start/end marker pair."
  (cond
   ((process-get proc :junit-in-raw)
    (if (string-match "\\`%\\(?:TRACEE\\|RTRACEE\\|EXPECTE\\|ACTUALE\\)\\b" line)
        (process-put proc :junit-in-raw nil)
      (process-put proc :junit-trace (cons line (process-get proc :junit-trace)))))
   ((string-match "\\`%\\([A-Za-z]+\\) *\\(.*\\)\\'" line)
    (let ((id (match-string 1 line)) (payload (match-string 2 line)))
      (cond
       ((member id '("TRACES" "RTRACES" "EXPECTS" "ACTUALS"))
        (process-put proc :junit-in-raw t))
       ((equal id "TESTC")
        (process-put proc :junit-pass 0) (process-put proc :junit-fail 0)
        (process-put proc :junit-error 0) (process-put proc :junit-skip 0)
        (dape--repl-insert (format "\n--- JUnit run: %s test(s) ---\n" (car (split-string payload)))))
       ((equal id "TESTS")
        (process-put proc :junit-cur (dape-java--junit-test-name payload))
        (process-put proc :junit-status 'pass)
        (process-put proc :junit-trace nil))
       ((equal id "FAILED") (process-put proc :junit-status 'fail))
       ((equal id "ERROR") (process-put proc :junit-status 'error))
       ((equal id "TESTI") (process-put proc :junit-status 'skip))
       ((equal id "TESTE") (dape-java--junit-report-test proc))
       ((equal id "RUNTIME") (dape-java--junit-report-summary proc payload))
       ((equal id "TESTSTP") (dape--repl-insert-error "--- JUnit run stopped ---\n")))))))

(defun dape-java--junit-listener-filter (proc string)
  "Buffer PROC's incoming STRING into lines and hand each to
`dape-java--junit-handle-line'."
  (process-put proc :junit-pending (concat (process-get proc :junit-pending) string))
  (let ((pending (process-get proc :junit-pending)) (start 0) pos)
    ;; Capture the newline position from `string-match's return value
    ;; rather than `(match-end 0)' -- `dape-java--junit-handle-line' runs
    ;; its own `string-match' calls, which clobber the global match
    ;; data this loop would otherwise be reading after the fact.
    (while (setq pos (string-match "\n" pending start))
      (dape-java--junit-handle-line
       proc (string-trim-right (substring pending start pos) "\r"))
      (setq start (1+ pos)))
    (process-put proc :junit-pending (substring pending start))))

(defun dape-java--junit-listener-sentinel (proc _event)
  "Flush PROC's trailing, non-newline-terminated line once it closes.
RemoteTestRunner's final `%RUNTIME' message has no trailing newline,
so `dape-java--junit-listener-filter' never treats it as a complete
line while the connection is open."
  (unless (process-live-p proc)
    (let ((pending (process-get proc :junit-pending)))
      (when (and pending (not (string-empty-p pending)))
        (process-put proc :junit-pending "")
        (dape-java--junit-handle-line proc pending)))))

;; Verify should be in format #'callback.
(defcustom dape-java-junit-listener-callback #'dape-java--junit-listener-filter
  "An optional function hook that will be passed every line that the
listener returns back."
  :type '(choice (const :tag "Disabled" nil) function)
  :group 'dape-java)

(defcustom dape-java-junit-listener-sentinel #'dape-java--junit-listener-sentinel
  "An optional process sentinel paired with `dape-java-junit-listener-callback'.
RemoteTestRunner's final message (the elapsed-time summary) is not
newline-terminated, so a line-buffering callback never sees it as a
complete line while the connection is open. This sentinel is called
with the same PROC/EVENT-STRING arguments Emacs passes to any process
sentinel, and should flush whatever the callback left buffered once
the connection closes."
  :type '(choice (const :tag "Disabled" nil) function)
  :group 'dape-java)

(defvar dape-java--junit-listener nil
  "Server process that listens for test results.
See `dape-java--junit-start-listener'.")

(defun dape-java--junit-start-listener ()
  "(Re)start a throwaway socket listener and return its port.
RemoteTestRunner insists on connecting to the `-port' it's given and
reporting results over that socket; `dape-java--junit-listener-filter'
decodes that stream into a running pass/fail summary in the REPL."
  (when (process-live-p dape-java--junit-listener)
    (delete-process dape-java--junit-listener))
  (setq dape-java--junit-listener
        (make-network-process :name "dape-junit-listener"
                              :server t :service t :family 'ipv4
                              :filter dape-java-junit-listener-callback
                              :sentinel dape-java-junit-listener-sentinel
                              :noquery t))
  (cadr (process-contact dape-java--junit-listener)))

(defun dape-java--junit-rewrite-port (program-arguments port)
  "Replace the `-port' value in PROGRAM-ARGUMENTS with PORT."
  (let* ((args (append program-arguments nil))
         (pos (seq-position args "-port" #'equal)))
    (when pos
      (setf (nth (1+ pos) args) (number-to-string port)))
    args))

(defun dape-java--junit-fn (config item-fn)
  "Shared `fn' for the `jdtls-junit' and `jdtls-junit-method' configs.
ITEM-FN is called with SERVER and FILE-URI to resolve the codelens
item to launch -- the whole class, or the method at point.

While the module's Gradle classpath is still cold, this only sets
`compile' (see `dape-java--junit-gradle-compile-command') and returns
early, without starting the JUnit listener or a jdtls debug session --
`dape' reruns `fn' from scratch once that compile buffer reports
success, and by then `dape-java--junit-gradle-classpath-cached' is
warm, so this function falls through to the real launch below.

Maven modules have no custom classpath-resolution path they skip the
Gradle compile/merge step entirely and launch with just jdtls's own
launch classpath."
  (with-current-buffer (find-file-noselect (dape-config-get config :filePath))
    (let* ((server (eglot-current-server))
           (file-uri (eglot-path-to-uri (buffer-file-name)))
           (item (funcall item-fn server file-uri))
           (launch (dape-java--junit-launch-arguments server item))
           (root (plist-get launch :workingDirectory))
           (gradle-p (dape-java--gradle-project-script root))
           (gradle-classpath (and gradle-p (dape-java--junit-gradle-classpath-cached root))))
      ;; early exit if we need to run compile and generate a classpath - that
      ;; will pick setting up the config up when its done.
      (if (and gradle-p (not gradle-classpath) dape-java-use-gradle-for-classpaths)
          (plist-put config 'compile (dape-java--junit-gradle-compile-command root))
        (let* ((junit-port (and dape-java-junit-listener-callback (dape-java--junit-start-listener)))
               (program-args (if junit-port
                                  (dape-java--junit-rewrite-port
                                   (plist-get launch :programArguments) junit-port)
                                (plist-get launch :programArguments)))
               (debug-port (eglot-execute-command server "vscode.java.startDebugSession" nil)))
          (thread-first config
                        (plist-put 'port debug-port)
                        (plist-put 'compile nil)
                        (plist-put :mainClass (plist-get launch :mainClass))
                        (plist-put :projectName (plist-get launch :projectName))
                        (plist-put :classPaths (vconcat gradle-classpath
                                                        (plist-get launch :classpath)))
                        (plist-put :modulePaths (plist-get launch :modulepath))
                        (plist-put :cwd (plist-get launch :workingDirectory))
                        (plist-put :vmArgs (mapconcat #'identity
                                                      (append (plist-get launch :vmArguments) nil)
                                                      " "))
                        (plist-put :args (mapconcat #'identity program-args " "))))))))

(defun dape-java--junit-ensure (config)
  "Shared `ensure' for the `jdtls-junit' and `jdtls-junit-method' configs."
  (let ((file (dape-config-get config :filePath)))
    (unless (and (stringp file) (file-exists-p file))
      (user-error "Unable to locate :filePath `%s'" file))
    (with-current-buffer (find-file-noselect file)
      (unless (and (featurep 'eglot) (eglot-current-server))
        (user-error "No eglot instance active in buffer %s" (current-buffer)))
      (unless (dape-java-jdtls-support-p)
        (user-error "Jdtls instance does not expose `%s' -- is the vscode-java-test bundle installed?"
                    dape-java--junit-search-command)))))

(defun dape-java-junit-item-line (item)
  "Return ITEM's declaration line (LSP, zero-origin)."
  (plist-get (plist-get (plist-get item :range) :start) :line))

(defun dape-java--junit-method-item-at-point (server file-uri)
  "Ask SERVER for the JUnit test METHOD item enclosing point in FILE-URI.
Codelens locations only span the method name token, not the method
body, and the items aren't returned in source order -- so the
enclosing method is taken to be the METHOD item with the highest
declaration line at or before point."
  (let* ((point-line (plist-get (eglot--pos-to-lsp-position) :line))
         (items (dape-java--junit-flatten-items
                 (eglot-execute-command
                  server dape-java--junit-search-command (vector file-uri))))
         (candidates (seq-filter
                      (lambda (it) (and (eql (dape-java-junit-item-testlevel it) 6)
                                        (<= (dape-java-junit-item-line it) point-line)))
                      items)))
    (or (car (last (seq-sort-by #'dape-java-junit-item-line #'< candidates)))
        (user-error "No JUnit test method at point in %s" file-uri))))

(with-eval-after-load 'dape
  ;; dape config for all tests in the file
  (add-to-list
   'dape-configs
   `(jdtls-junit
     modes (java-mode java-ts-mode)
     ensure dape-java--junit-ensure
     :filePath ,(lambda () (expand-file-name (dape-buffer-default) (dape-cwd)))
     fn (lambda (config) (dape-java--junit-fn config #'dape-java--find-tests))
     :stopOnEntry nil
     :type "java"
     :request "launch"
     :console "integratedConsole"
     :internalConsoleOptions "neverOpen"))

  ;; dape config for a single test method (at point)
  (add-to-list
   'dape-configs
   `(jdtls-junit-method
     modes (java-mode java-ts-mode)
     ensure dape-java--junit-ensure
     :filePath ,(lambda () (expand-file-name (dape-buffer-default) (dape-cwd)))
     fn (lambda (config) (dape-java--junit-fn config #'dape-java--junit-method-item-at-point))
     :stopOnEntry nil
     :type "java"
     :request "launch"
     :console "integratedConsole"
     :internalConsoleOptions "neverOpen")))

(defconst  dape-java-junit-backoff-initial-delay 2
  "Seconds before the first retry in `dape-java-junit-fetch-items-async'.")

(defconst  dape-java-junit-backoff-factor 1.5
  "Multiplier applied to the delay on each successive retry.")

(defconst  dape-java-junit-backoff-max-delay 60
  "Cap on the backoff delay -- once reached, keeps retrying at this
interval indefinitely rather than growing without bound. There's no
retry limit/timeout: a project that's slow enough to still be
importing a minute in is unusual but not wrong, and giving up would
just leave the gutters permanently empty until the next save.")

(defun dape-java-junit-fetch-items-async (server file-uri callback &optional delay)
  "Asynchronously ask SERVER for FILE-URI's JUnit items, then call
CALLBACK with the flattened list (see `dape-java--junit-flatten-items').

Async so a jdtls that's still busy importing the project (this query can
only answer once the file's project is indexed) doesn't block Emacs for
however long that takes.

Retries on error, with gradually increasing backoff and no cutoff,
rather than trying to guess when the project is done importing."
  (let* ((delay (or delay dape-java-junit-backoff-initial-delay))
         (retry (lambda (&rest _)
                  (run-with-timer
                   delay nil
                   (lambda (buf)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (dape-java-junit-fetch-items-async
                          server file-uri callback
                          (min (* delay dape-java-junit-backoff-factor)
                               dape-java-junit-backoff-max-delay)))))
                   (current-buffer)))))
    (eglot--async-request
     server :workspace/executeCommand
     `(:command ,dape-java--junit-search-command :arguments ,(vector file-uri))
     :success-fn (lambda (items) (funcall callback (dape-java--junit-flatten-items items)))
     ;; We need to retry on both errors and timeouts
     :error-fn retry
     :timeout-fn retry
     :hint dape-java--junit-search-command)))

(defcustom dape-java-ms-debug-plugin
  (expand-file-name
  "~/.m2/repository/com/microsoft/java/com.microsoft.java.debug.plugin/0.53.2/com.microsoft.java.debug.plugin-0.53.2.jar")
  "The path to the microsoft java debug plugin jar"
  :group 'dape-java
  :type 'string
  )

(defcustom dape-java-test-bundles-dir
  (expand-file-name "jdtls-bundles/" user-emacs-directory)
  "Directory holding the vscode-java-test OSGi bundle jars used by jdtls.
See the Prerequisites section at the top of this file for how to
populate it, or `dape-java-fetch-test-bundle' to do it automatically."
  :type 'directory
  :group 'dape-java)

(defcustom dape-java-test-bundle-version "0.40.1"
  "Version of the `vscjava.vscode-java-test' extension
`dape-java-fetch-test-bundle' fetches.

Check https://open-vsx.org/extension/vscjava/vscode-java-test for the
current release and bump this to match. There's no Maven-Central-style
version-alignment metadata tying this to a jdtls/`com.microsoft.java.debug.plugin'
version -- if jdtls starts rejecting the bundle after an upgrade,
that's the first thing to try changing."
  :type 'string
  :group 'dape-java)

(defun dape-java-fetch-test-bundle (&optional version)
  "Download and install the vscode-java-test OSGi bundle jars into
`dape-java-test-bundles-dir', fetching VERSION (default
`dape-java-test-bundle-version') from open-vsx.org.

Automates the VSIX-unzip procedure in the Prerequisites section at
the top of this file. open-vsx.org is, as of this writing, the only
place still serving prebuilt versioned vsix files for this extension
-- microsoft/vscode-java-test stopped attaching them to GitHub
Releases as of 0.39.1, and the jars were never published to Maven
Central or any p2 update site.).
Requires `unzip' on PATH."
  (interactive
   (list (read-string "vscode-java-test version: " dape-java-test-bundle-version)))
  (let* ((version (or version dape-java-test-bundle-version))
         (url (format
               "https://open-vsx.org/api/vscjava/vscode-java-test/%s/file/vscjava.vscode-java-test-%s.vsix"
               version version))
         (vsix-file (make-temp-file "dape-java-test-bundle-" nil ".vsix"))
         (extract-dir (make-temp-file "dape-java-test-bundle-" t)))
    (unwind-protect
        (progn
          (message "dape-java-fetch-test-bundle: downloading %s" url)
          (url-copy-file url vsix-file t)
          (unless (zerop (call-process "unzip" nil nil nil "-q" "-o" vsix-file "-d" extract-dir))
            (user-error "dape-java-fetch-test-bundle: `unzip' failed on %s" vsix-file))
          (make-directory dape-java-test-bundles-dir t)
          (dolist (jar (directory-files
                        (expand-file-name "extension/server" extract-dir)
                        t "\\.jar\\'"))
            (copy-file jar (expand-file-name (file-name-nondirectory jar)
                                              dape-java-test-bundles-dir)
                       t))
          (message "dape-java-fetch-test-bundle: installed to %s" dape-java-test-bundles-dir))
      (delete-file vsix-file)
      (delete-directory extract-dir t))))

(defun dape-java-get-test-bundle-vector ()
  "Return back the vector of debug jar names to supply to jdtls that will enable DAP"
  (let ((debug-plugin dape-java-ms-debug-plugin)
	(bundle-dir dape-java-test-bundles-dir))

    (unless (file-exists-p debug-plugin)
      (user-error "com.microsoft.java.debug.plugin is missing at: %s" debug-plugin))

    (when (directory-empty-p bundle-dir)
      (user-error "The microsoft test bundle dir is empty at: %s" bundle-dir))

    (vconcat
     (vector debug-plugin)
     (seq-remove
      (lambda (f) (string-match-p "test\\.runner\\|org\\.objectweb\\.asm\\|jacocoagent" f))
      (file-expand-wildcards (concat bundle-dir  "*.jar"))))))
