;; -*- lexical-binding: t; -*-
;; ERT tests for speedbar-config.el: that its customizations are installed once speedbar loads,
;; and the helper functions behind them (file type text, project root and tracked-file filtering,
;; git state parsing and coloring, dedicated mode line pieces, the tag advices).  Anything that
;; needs a real speedbar window or frame is out of scope here.
(require 'ert)
(require 'cl-lib)
(require 'project)

;; In the normal CI flow (see .github/workflows/test.yml) `init.el' is loaded first, which loads
;; `modeline.el' and `speedbar-config.el' itself.  This fallback makes the file runnable on its
;; own (e.g. `emacs -Q --batch -l ert -l speedbar-config-tests.el -f ert-run-tests-batch-and-exit').
(eval-and-compile
  (let ((dir (file-name-directory
              (or load-file-name (bound-and-true-p byte-compile-current-file)
                  buffer-file-name default-directory))))
    (unless (fboundp 'my-modeline-dedicated-line)
      (require 'modeline (expand-file-name "modeline.el" dir)))
    (unless (fboundp 'my-speedbar--file-type)
      (require 'speedbar-config (expand-file-name "speedbar-config.el" dir)))))

;; Loading speedbar runs the `with-eval-after-load' body in speedbar-config.el.  This also makes
;; speedbar's variables special before the `let' bindings below.
(require 'speedbar)

;; Declared so the `let' bindings below stay dynamic when this file is byte-compiled on its own.
(defvar my-speedbar-project-view-p)
(defvar my-speedbar--project-root)
(defvar my-speedbar--tracked)
(defvar my-speedbar--git-status)
(defvar my-speedbar-update-speed)
(defvar my-modeline-format)
(defvar my-modeline-dedicated-window-modes)
(defvar my-modeline-dedicated-label-functions)
(defvar my-modeline-dedicated-help-functions)

;;; Helpers

(defmacro speedbar-config-tests--with-repo (vars &rest body)
  "Run BODY with VARS bound to a throwaway git repo's root, with files made.
VARS is (ROOT).  The repo has tracked a.txt and sub/b.txt (committed) and an
untracked c.txt.  Skips the test when git is missing."
  (declare (indent 1))
  `(progn
     (skip-unless (executable-find "git"))
     (let* ((,(car vars) (file-name-as-directory
                          (file-truename (make-temp-file "speedbar-config-test" t))))
            (process-environment (append '("GIT_CONFIG_GLOBAL=/dev/null"
                                           "GIT_CONFIG_NOSYSTEM=1")
                                         process-environment))
            (my-speedbar--tracked nil)
            (my-speedbar--git-status nil))
       (unwind-protect
           (progn
             (speedbar-config-tests--git ,(car vars) "init" "-q")
             (make-directory (expand-file-name "sub" ,(car vars)))
             (dolist (f '("a.txt" "sub/b.txt" "c.txt"))
               (with-temp-file (expand-file-name f ,(car vars)) (insert "one\n")))
             (speedbar-config-tests--git ,(car vars) "add" "a.txt" "sub/b.txt")
             (speedbar-config-tests--git ,(car vars) "commit" "-q" "-m" "init")
             ,@body)
         (delete-directory ,(car vars) t)))))

(defun speedbar-config-tests--git (dir &rest args)
  "Run git with ARGS in DIR and signal an error if it fails."
  (let ((default-directory dir))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil
                            "-c" "user.name=t" "-c" "user.email=t@example.com"
                            "-c" "commit.gpgsign=false" args))
        (error "git %s failed: %s" args (buffer-string))))))

(defun speedbar-config-tests--insert-rows (&rest names)
  "Insert a speedbar-style file row for each of NAMES in the current buffer."
  (dolist (name names)
    (insert "0: [ ] ")
    (insert (propertize name 'speedbar-function 'speedbar-find-file 'speedbar-text name))
    (insert "\n")))

;;; Installation after speedbar loads

(ert-deftest speedbar-config-test/settings-applied ()
  (should speedbar-show-unknown-files)
  (should speedbar-prefer-window)
  (should-not speedbar-obj-do-check)
  (should-not speedbar-vc-do-check))

(ert-deftest speedbar-config-test/keys-bound ()
  (should (eq (keymap-lookup speedbar-mode-map "q") #'my-speedbar-quit))
  (should (eq (keymap-lookup speedbar-mode-map "h") #'my-speedbar-describe-keymap))
  (should (eq (keymap-lookup speedbar-mode-map "P") #'my-speedbar-project-view))
  (should (eq (keymap-lookup speedbar-mode-map "b") #'my-speedbar-show-buffers))
  (should (eq (keymap-lookup speedbar-mode-map "f") #'my-speedbar-show-files))
  (should (eq (keymap-lookup speedbar-mode-map "r") #'my-speedbar-show-previous-display)))

(ert-deftest speedbar-config-test/o-only-bound-in-project-view ()
  (let ((my-speedbar-project-view-p nil))
    (should-not (keymap-lookup speedbar-mode-map "o")))
  (let ((my-speedbar-project-view-p t))
    (should (eq (keymap-lookup speedbar-mode-map "o") #'my-speedbar-sync-to-last-window))))

(ert-deftest speedbar-config-test/advice-installed ()
  (dolist (pair '((speedbar-item-info-file-helper . my-speedbar-item-info-file)
                  (speedbar-insert-image-button-maybe . my-speedbar-glyph)
                  (speedbar-file-lists . my-speedbar-project-file-lists)
                  (speedbar-update-directory-contents . my-speedbar-project-update-contents)
                  (speedbar-update-directory-contents . my-speedbar--colorize)
                  (speedbar-update-directory-contents . my-speedbar--reset-top)
                  (speedbar-update-localized-contents . my-speedbar-project-update-localized)
                  (speedbar-update-current-file . my-speedbar--reveal)
                  (speedbar-update-current-file . my-speedbar--colorize)
                  (speedbar-set-mode-line-format . my-speedbar-project-mode-line)
                  (speedbar-set-mode-line-format . my-speedbar--strip-arrows)
                  (speedbar-change-initial-expansion-list . my-speedbar-project-view-exit)
                  (speedbar-refresh . my-speedbar-project-refresh)
                  (speedbar-expand-line . my-speedbar--colorize)
                  (speedbar-directory-buttons . my-speedbar--root-face)
                  (speedbar-set-timer . my-speedbar--update-speed)
                  (speedbar-fetch-dynamic-imenu . my-speedbar--no-tags-imenu)
                  (speedbar-fetch-dynamic-tags . my-speedbar--no-tags-dynamic)))
    (should (advice-member-p (cdr pair) (car pair)))))

(ert-deftest speedbar-config-test/dedicated-modeline-registered ()
  (should (equal (alist-get 'speedbar-mode my-modeline-dedicated-window-modes)
                 'my-speedbar-quit))
  (should (equal (alist-get 'speedbar-mode my-modeline-dedicated-label-functions)
                 'my-speedbar-modeline-label))
  (should (equal (alist-get 'speedbar-mode my-modeline-dedicated-help-functions)
                 'my-speedbar-modeline-help)))

(ert-deftest speedbar-config-test/markdown-is-a-supported-extension ()
  (should (seq-some (lambda (re) (string-match-p re "notes.md"))
                    speedbar-supported-extension-expressions)))

;;; `my-speedbar--file-type'

(ert-deftest speedbar-config-test/file-type-directory ()
  (should (equal (my-speedbar--file-type temporary-file-directory) "directory")))

(ert-deftest speedbar-config-test/file-type-from-major-mode ()
  (should (equal (my-speedbar--file-type "/nowhere/foo.el") "Emacs Lisp"))
  (should (equal (my-speedbar--file-type "/nowhere/foo.py") "Python")))

(ert-deftest speedbar-config-test/file-type-falls-back-to-extension ()
  (should (equal (my-speedbar--file-type "/nowhere/foo.zzqx") ".zzqx file")))

(ert-deftest speedbar-config-test/file-type-unknown ()
  (should (equal (my-speedbar--file-type "/nowhere/zzqx") "unknown")))

;;; `my-speedbar-help-echo'

(ert-deftest speedbar-config-test/help-echo-appends-type ()
  (cl-letf (((symbol-function 'speedbar-item-info) (lambda () "info text"))
            ((symbol-function 'speedbar-line-file) (lambda (&rest _) "/nowhere/foo.el")))
    (should (equal (my-speedbar-help-echo) "info text\nType: Emacs Lisp"))))

(ert-deftest speedbar-config-test/help-echo-without-file-is-just-info ()
  (cl-letf (((symbol-function 'speedbar-item-info) (lambda () "info text"))
            ((symbol-function 'speedbar-line-file) (lambda (&rest _) nil)))
    (should (equal (my-speedbar-help-echo) "info text"))))

;;; `my-speedbar-describe-keymap'

(ert-deftest speedbar-config-test/describe-keymap-lists-local-bindings ()
  (let ((map (make-sparse-keymap)))
    (keymap-set map "x" #'ignore)
    (keymap-set map "C-c y" #'forward-char)
    (unwind-protect
        (with-temp-buffer
          (use-local-map map)
          (my-speedbar-describe-keymap)
          (let ((text (with-current-buffer "*Speedbar Keys*" (buffer-string))))
            (should (string-match-p "^x +ignore" text))
            (should (string-match-p "^C-c y +forward-char" text))))
      (when (get-buffer "*Speedbar Keys*")
        (kill-buffer "*Speedbar Keys*")))))

;;; Named display commands

(ert-deftest speedbar-config-test/show-displays-pick-the-right-list ()
  (let (seen)
    (cl-letf (((symbol-function 'speedbar-change-initial-expansion-list)
               (lambda (name) (push name seen))))
      (let ((speedbar-previously-used-expansion-list-name "quick buffers"))
        (my-speedbar-show-buffers)
        (my-speedbar-show-files)
        (my-speedbar-show-previous-display)))
    (should (equal (nreverse seen) '("quick buffers" "files" "quick buffers")))))

;;; `my-speedbar-item-info-file'

(ert-deftest speedbar-config-test/item-info-file-shows-vc-state ()
  (speedbar-config-tests--with-repo (root)
    (let ((file (expand-file-name "a.txt" root))
          shown)
      (cl-letf (((symbol-function 'dframe-message)
                 (lambda (fmt &rest args) (setq shown (apply #'format fmt args))))
                ;; `vc-state' resolves the file against `default-directory'.
                (default-directory root))
        (my-speedbar-item-info-file file))
      (should (string-match-p (regexp-quote file) shown))
      (should (string-match-p "\\[vc: up-to-date\\]" shown)))))

(ert-deftest speedbar-config-test/item-info-file-no-vc-state-outside-repo ()
  (let ((file (make-temp-file "speedbar-config-test"))
        shown)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'dframe-message)
                     (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
            (my-speedbar-item-info-file file))
          (should (string-match-p (regexp-quote file) shown))
          (should-not (string-match-p "\\[vc:" shown)))
      (delete-file file))))

;;; `my-speedbar-glyph'

(ert-deftest speedbar-config-test/glyph-defers-for-unknown-marker ()
  (with-temp-buffer
    (insert "xyz")
    (let (called)
      (my-speedbar-glyph (lambda (start length) (setq called (list start length))) 1 3)
      (should (equal called '(1 3)))
      (should-not (get-text-property 1 'display)))))

(ert-deftest speedbar-config-test/glyph-replaces-directory-marker ()
  (skip-unless (require 'nerd-icons nil t))
  (with-temp-buffer
    (insert "<+>")
    (my-speedbar-glyph (lambda (&rest _) (ert-fail "should not defer")) 1 3)
    (should (get-text-property 1 'display))))

;;; Project root and tracked files

(ert-deftest speedbar-config-test/root-of-project ()
  (speedbar-config-tests--with-repo (root)
    (should (equal (my-speedbar--root-of root) root))
    (should (equal (my-speedbar--root-of (expand-file-name "sub/" root)) root))))

(ert-deftest speedbar-config-test/root-of-nil-for-non-project ()
  (should-not (my-speedbar--root-of nil))
  (should-not (my-speedbar--root-of "/ssh:nowhere:/tmp/"))
  (should-not (my-speedbar--root-of (file-name-as-directory
                                     (file-truename (make-temp-file "speedbar-config-test" t))))))

(ert-deftest speedbar-config-test/tracked-lists-files-and-directories ()
  (speedbar-config-tests--with-repo (root)
    (let ((tracked (my-speedbar--tracked root)))
      (should (equal (car tracked) root))
      (should (gethash (concat root "a.txt") (nth 1 tracked)))
      (should (gethash (concat root "sub/b.txt") (nth 1 tracked)))
      (should-not (gethash (concat root "c.txt") (nth 1 tracked)))
      (should (gethash (concat root "sub/") (nth 2 tracked)))
      (should-not (gethash root (nth 2 tracked))))))

(ert-deftest speedbar-config-test/tracked-nil-without-tracked-files ()
  (skip-unless (executable-find "git"))
  (let ((root (file-name-as-directory (file-truename (make-temp-file "speedbar-config-test" t))))
        (process-environment (cons "GIT_CONFIG_GLOBAL=/dev/null" process-environment))
        (my-speedbar--tracked nil))
    (unwind-protect
        (progn
          (speedbar-config-tests--git root "init" "-q")
          (should-not (my-speedbar--tracked root)))
      (delete-directory root t))))

(ert-deftest speedbar-config-test/file-lists-filters-in-project-view ()
  (speedbar-config-tests--with-repo (root)
    (let ((my-speedbar-project-view-p t)
          (my-speedbar--project-root root)
          (orig (lambda (_dir) (list '("sub" "ignored") '("a.txt" "c.txt")))))
      (should (equal (my-speedbar-project-file-lists orig root)
                     '(("sub") ("a.txt")))))))

(ert-deftest speedbar-config-test/file-lists-untouched-outside-project-view ()
  (let ((my-speedbar-project-view-p nil)
        (orig (lambda (_dir) (list '("sub" "ignored") '("a.txt" "c.txt")))))
    (should (equal (my-speedbar-project-file-lists orig "/nowhere/")
                   '(("sub" "ignored") ("a.txt" "c.txt"))))))

;;; `my-speedbar--project-switch-root'

(ert-deftest speedbar-config-test/switch-root-cases ()
  (speedbar-config-tests--with-repo (root)
    (let ((default-directory root))
      (let ((my-speedbar--project-root nil)
            (speedbar-shown-directories nil))
        (should (equal (my-speedbar--project-switch-root) root)))
      (let ((my-speedbar--project-root root)
            (speedbar-shown-directories (list root)))
        (should-not (my-speedbar--project-switch-root)))
      (let ((my-speedbar--project-root root)
            (speedbar-shown-directories nil))
        (should (equal (my-speedbar--project-switch-root) root)))
      (let ((my-speedbar--project-root "/some/other/project/")
            (speedbar-shown-directories (list "/some/other/project/")))
        (should (equal (my-speedbar--project-switch-root) root))))
    (let ((default-directory temporary-file-directory))
      (should-not (my-speedbar--project-switch-root)))))

;;; Project view redraw advice

(ert-deftest speedbar-config-test/update-contents-passes-through-outside-project-view ()
  (let ((my-speedbar-project-view-p nil)
        (called nil))
    (my-speedbar-project-update-contents (lambda (&rest args) (setq called args)) 1 2)
    (should (equal called '(1 2)))))

(ert-deftest speedbar-config-test/update-contents-keeps-speedbar-in-project ()
  (speedbar-config-tests--with-repo (root)
    (with-temp-buffer
      (let ((speedbar-buffer (current-buffer))
            (my-speedbar-project-view-p t)
            (my-speedbar--project-root root)
            (default-directory temporary-file-directory)
            drawn-in)
        (my-speedbar-project-update-contents (lambda () (setq drawn-in default-directory)))
        (should (equal drawn-in root))
        (should (equal default-directory root))))))

(ert-deftest speedbar-config-test/update-contents-draws-the-buffers-project ()
  (speedbar-config-tests--with-repo (root)
    (let ((speedbar-buffer (generate-new-buffer " *speedbar-test*"))
          (my-speedbar-project-view-p t)
          (my-speedbar--project-root "/some/other/project/")
          (speedbar-shown-directories nil)
          (default-directory root)
          drawn-in)
      (unwind-protect
          (progn
            (my-speedbar-project-update-contents (lambda () (setq drawn-in default-directory)))
            (should (equal drawn-in root))
            (should (equal my-speedbar--project-root root)))
        (kill-buffer speedbar-buffer)))))

(ert-deftest speedbar-config-test/update-localized-ignores-same-project ()
  (speedbar-config-tests--with-repo (root)
    (let ((speedbar-buffer (generate-new-buffer " *speedbar-test*"))
          (my-speedbar-project-view-p t)
          (my-speedbar--project-root root)
          (speedbar-shown-directories (list root))
          (default-directory root)
          (called nil))
      (unwind-protect
          (progn
            (my-speedbar-project-update-localized (lambda () (setq called t)))
            (should-not called)
            (setq speedbar-shown-directories nil)
            (my-speedbar-project-update-localized (lambda () (setq called t)))
            (should called))
        (kill-buffer speedbar-buffer)))))

(ert-deftest speedbar-config-test/view-exit-and-refresh-reset-state ()
  (let ((my-speedbar-project-view-p t)
        (speedbar-full-text-cache '(stale))
        (speedbar-shown-directories '("/x/"))
        (my-speedbar--tracked '("/x/" nil nil))
        (my-speedbar--git-status '("/x/")))
    (my-speedbar-project-refresh)
    (should-not my-speedbar--tracked)
    (should-not my-speedbar--git-status)
    (my-speedbar-project-view-exit)
    (should-not my-speedbar-project-view-p)
    (should-not speedbar-full-text-cache)
    (should-not speedbar-shown-directories)))

(ert-deftest speedbar-config-test/project-view-requires-speedbar-buffer ()
  (with-temp-buffer
    (should-error (my-speedbar-project-view) :type 'user-error)))

;;; Git state

(ert-deftest speedbar-config-test/git-status-modified-added-and-clean ()
  (speedbar-config-tests--with-repo (root)
    (with-temp-file (expand-file-name "a.txt" root) (insert "changed\n"))
    (with-temp-file (expand-file-name "new.txt" root) (insert "new\n"))
    (speedbar-config-tests--git root "add" "new.txt")
    (let ((states (my-speedbar--git-status root)))
      (should (eq (gethash (concat root "a.txt") states) 'modified))
      (should (eq (gethash (concat root "new.txt") states) 'added))
      (should-not (gethash (concat root "sub/b.txt") states))
      (should-not (gethash (concat root "c.txt") states)))))

(ert-deftest speedbar-config-test/git-status-rename-consumes-old-name ()
  (speedbar-config-tests--with-repo (root)
    (speedbar-config-tests--git root "mv" "a.txt" "renamed.txt")
    (let ((states (my-speedbar--git-status root)))
      (should (eq (gethash (concat root "renamed.txt") states) 'modified))
      (should-not (gethash (concat root "a.txt") states)))))

(ert-deftest speedbar-config-test/git-status-conflict ()
  (speedbar-config-tests--with-repo (root)
    (speedbar-config-tests--git root "checkout" "-q" "-b" "other")
    (with-temp-file (expand-file-name "a.txt" root) (insert "other\n"))
    (speedbar-config-tests--git root "commit" "-q" "-a" "-m" "other")
    (speedbar-config-tests--git root "checkout" "-q" "-")
    (with-temp-file (expand-file-name "a.txt" root) (insert "main\n"))
    (speedbar-config-tests--git root "commit" "-q" "-a" "-m" "main")
    (ignore-errors (speedbar-config-tests--git root "merge" "other"))
    (should (eq (gethash (concat root "a.txt") (my-speedbar--git-status root)) 'conflict))))

(ert-deftest speedbar-config-test/git-status-is-cached-per-root ()
  (speedbar-config-tests--with-repo (root)
    (let ((first (my-speedbar--git-status root)))
      (with-temp-file (expand-file-name "a.txt" root) (insert "changed\n"))
      (should (eq (my-speedbar--git-status root) first))
      (setq my-speedbar--git-status nil)
      (should (eq (gethash (concat root "a.txt") (my-speedbar--git-status root)) 'modified)))))

;;; `my-speedbar--colorize'

(ert-deftest speedbar-config-test/colorize-marks-changed-files-only ()
  (speedbar-config-tests--with-repo (root)
    (with-temp-file (expand-file-name "a.txt" root) (insert "changed\n"))
    (with-temp-buffer
      (let ((speedbar-buffer (current-buffer))
            (my-speedbar-project-view-p t)
            (my-speedbar--project-root root)
            (default-directory root))
        (speedbar-config-tests--insert-rows "a.txt" "c.txt")
        (my-speedbar--colorize)
        (let ((overlays (seq-filter (lambda (ov) (overlay-get ov 'my-speedbar-status))
                                    (overlays-in (point-min) (point-max)))))
          (should (= (length overlays) 1))
          (should (eq (overlay-get (car overlays) 'face) 'my-speedbar-file-modified))
          (should (equal (buffer-substring-no-properties (overlay-start (car overlays))
                                                         (overlay-end (car overlays)))
                         "a.txt")))
        ;; A second run replaces the overlays rather than stacking them.
        (my-speedbar--colorize)
        (should (= (length (seq-filter (lambda (ov) (overlay-get ov 'my-speedbar-status))
                                       (overlays-in (point-min) (point-max))))
                   1))))))

(ert-deftest speedbar-config-test/colorize-does-nothing-outside-project-view ()
  (with-temp-buffer
    (let ((speedbar-buffer (current-buffer))
          (my-speedbar-project-view-p nil))
      (speedbar-config-tests--insert-rows "a.txt")
      (my-speedbar--colorize)
      (should-not (overlays-in (point-min) (point-max))))))

;;; Mode line pieces

(ert-deftest speedbar-config-test/project-mode-line-names-the-project ()
  (let ((my-speedbar--project-root "/some/where/my-proj/"))
    (let ((line (my-speedbar--project-mode-line)))
      (should (string-match-p "project: my-proj" (substring-no-properties line)))
      (should (string-match-p "close the window" (get-text-property 0 'help-echo line)))
      (should (get-text-property 0 'local-map line)))))

(ert-deftest speedbar-config-test/mode-line-advice-installs-project-line ()
  (with-temp-buffer
    (let ((speedbar-buffer (current-buffer))
          (my-speedbar--project-root "/some/where/my-proj/")
          (orig-called nil))
      (let ((my-speedbar-project-view-p t))
        (my-speedbar-project-mode-line (lambda () (setq orig-called t))))
      (should-not orig-called)
      (should (equal mode-line-format '(:eval (my-speedbar--project-mode-line))))
      (let ((my-speedbar-project-view-p nil))
        (my-speedbar-project-mode-line (lambda () (setq orig-called t))))
      (should orig-called))))

(ert-deftest speedbar-config-test/strip-arrows-from-other-displays ()
  (with-temp-buffer
    (let ((speedbar-buffer (current-buffer))
          (speedbar-initial-expansion-list-name "quick buffers")
          (my-speedbar-project-view-p nil))
      (setq-local mode-line-format '("<< Quick Buffers >>" (:eval foo)))
      (my-speedbar--strip-arrows)
      (should (equal mode-line-format '("Quick Buffers" (:eval foo)))))))

(ert-deftest speedbar-config-test/files-display-gets-dedicated-modeline ()
  (with-temp-buffer
    (let ((speedbar-buffer (current-buffer))
          (speedbar-initial-expansion-list-name "files")
          (my-speedbar-project-view-p nil))
      (my-speedbar--strip-arrows)
      (should (equal mode-line-format my-modeline-format)))))

(ert-deftest speedbar-config-test/strip-arrows-leaves-project-view-alone ()
  (with-temp-buffer
    (let ((speedbar-buffer (current-buffer))
          (speedbar-initial-expansion-list-name "files")
          (my-speedbar-project-view-p t))
      (setq-local mode-line-format '("keep"))
      (my-speedbar--strip-arrows)
      (should (equal mode-line-format '("keep"))))))

(ert-deftest speedbar-config-test/file-count-counts-file-rows-and-caches ()
  (with-temp-buffer
    (speedbar-config-tests--insert-rows "a.txt" "b.txt")
    (insert "0: <+> dir\n")
    (should (= (my-speedbar--file-count) 2))
    (speedbar-config-tests--insert-rows "c.txt")
    (should (= (my-speedbar--file-count) 3))))

(ert-deftest speedbar-config-test/modeline-label-counts-files ()
  (with-temp-buffer
    (let ((speedbar-update-flag t))
      (speedbar-config-tests--insert-rows "a.txt")
      (should (string-match-p "1 file\\'" (my-speedbar-modeline-label)))
      (speedbar-config-tests--insert-rows "b.txt")
      (should (string-match-p "2 files\\'" (my-speedbar-modeline-label))))))

(ert-deftest speedbar-config-test/modeline-help-reports-state ()
  (let ((default-directory temporary-file-directory))
    (let ((speedbar-update-flag t))
      (should (string-match-p "Auto-refresh: on" (my-speedbar-modeline-help))))
    (let ((speedbar-update-flag nil))
      (should (string-match-p "Auto-refresh: off" (my-speedbar-modeline-help))))
    (should (string-match-p "Project root: none" (my-speedbar-modeline-help)))))

(ert-deftest speedbar-config-test/modeline-help-names-project-root ()
  (speedbar-config-tests--with-repo (root)
    (let ((default-directory root))
      (should (string-match-p (regexp-quote (abbreviate-file-name root))
                              (my-speedbar-modeline-help))))))

;;; Small advices

(ert-deftest speedbar-config-test/update-speed-replaces-the-interval ()
  (let ((my-speedbar-update-speed 0.2))
    (should (equal (my-speedbar--update-speed '(5)) '(0.2)))
    (should (equal (my-speedbar--update-speed '(nil)) '(nil)))))

(ert-deftest speedbar-config-test/root-face-layers-over-drawn-text ()
  (with-temp-buffer
    (my-speedbar--root-face (lambda () (insert "/some/dir/")))
    (should (memq 'my-speedbar-root-face
                  (ensure-list (get-text-property 1 'face))))))

(ert-deftest speedbar-config-test/no-tags-imenu-treats-rescan-as-empty ()
  (should-not (my-speedbar--no-tags-imenu '(("*Rescan*"))))
  (should-not (my-speedbar--no-tags-imenu '(nil ("*Rescan*"))))
  (should-not (my-speedbar--no-tags-imenu nil))
  (should (equal (my-speedbar--no-tags-imenu '(("fn" . 1))) '(("fn" . 1)))))

(ert-deftest speedbar-config-test/no-tags-dynamic-messages-when-empty ()
  (let (messages)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (should-not (my-speedbar--no-tags-dynamic (lambda (_f) '(insert-fn)) "/x/foo.el"))
      (should-not (my-speedbar--no-tags-dynamic (lambda (_f) nil) "/x/foo.el"))
      (should (equal (my-speedbar--no-tags-dynamic (lambda (_f) '(insert-fn ("tag" . 1)))
                                                   "/x/foo.el")
                     '(insert-fn ("tag" . 1)))))
    (should (equal messages '("No tags in foo.el" "No tags in foo.el")))))

(ert-deftest speedbar-config-test/reset-top-scrolls-after-redraw-only ()
  (with-temp-buffer
    (let ((speedbar-buffer (current-buffer)))
      (insert (make-string 200 ?x))
      (goto-char (point-max))
      ;; A redraw: the first character is replaced, so point returns to the top.
      (my-speedbar--reset-top (lambda () (erase-buffer) (insert (make-string 200 ?y))))
      (should (= (point) (point-min)))
      (goto-char (point-max))
      ;; An in-place expansion keeps the first character, so point stays put.
      (my-speedbar--reset-top (lambda () (goto-char (point-max)) (insert "more")))
      (should (= (point) (point-max))))))

(provide 'speedbar-config-tests)
