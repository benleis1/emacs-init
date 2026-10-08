;; -*- lexical-binding: t; -*-
(require 'ert)

;; Minimal viability.
(ert-deftest my/test-config-loading ()
  "Test that the user configuration loads without errors."
  (should
   (load (expand-file-name "init.el" user-emacs-directory) t)))

;; Regression test for the org-timegrid startup bug: a `use-package' with
;; `:commands'/`:defer' had a real function call in `:init', which forced
;; org-agenda's require chain to load on every startup instead of only when
;; the agenda is actually opened. Generalized to every `:defer t'/`:commands'
;; use-package block in init.el (see the matching feature list there) --
;; loading init.el fresh should never pull any of these in. Keep this list in
;; sync when adding/removing a `:defer'/`:commands' use-package; a feature
;; symbol usually matches the package name but isn't guaranteed to (check
;; with `featurep' if unsure).
(defconst my/deferred-package-features
  '(nerd-icons-dired diff-hl markdown-mode stripe-buffer markdown-toc
    org-modern org-timegrid org-timegrid-agenda org-agenda transient magit
    treesit-fold sqlformat excorporate yasnippet consult-yasnippet wikimode)
  "Features that must stay unloaded after a bare `init.el' load.")

(ert-deftest my/test-deferred-packages-stay-deferred ()
  "Test that none of `my/deferred-package-features' load merely from init.el."
  (load (expand-file-name "init.el" user-emacs-directory) t)
  (let ((loaded (seq-filter #'featurep my/deferred-package-features)))
    (should (null loaded))))

;; Crude safeguard against startup-time regressions like the org-timegrid one
;; above: spawn a genuinely fresh Emacs process (this test file's own load of
;; init.el, above, only measures a warm reload) and fail if a cold start
;; takes too long. Threshold is 2.5s rather than a tight 1s: local batch
;; timings cluster around 0.93-1.0s, but GitHub's shared ubuntu-latest
;; runners are noticeably slower and noisier than a local machine -- 1.5s
;; still flaked there. 2.5s keeps enough headroom for that CI variance while
;; still catching a regression the size of the org-timegrid one (which took
;; cold start from ~1.1s to ~1.5s).
(ert-deftest my/test-startup-time-under-threshold ()
  "Test that loading early-init.el + init.el in a fresh Emacs takes < 2.5s.

Elpaca clones and builds every `:ensure'd package on first use, and on a
cold CI runner (no cache between jobs) that alone can take minutes -- time
that has nothing to do with actual startup speed. So we run an untimed
warm-up load first to force elpaca to finish installing/building
everything on disk, then only time a second, now genuinely warm, load."
  (let* ((emacs (or (executable-find "emacs") "emacs"))
         (early (expand-file-name "early-init.el" user-emacs-directory))
         (init (expand-file-name "init.el" user-emacs-directory))
         (warmup-form (format "(load %S) (load %S)" early init))
         (timed-form (format "(let ((start (current-time))) (load %S) (load %S) \
(princ (format \"MY-ELAPSED %%s\" (float-time (time-subtract (current-time) start)))))"
                       early init)))
    (call-process emacs nil nil nil "--batch" "--eval" warmup-form)
    (let* ((output (with-temp-buffer
                      (call-process emacs nil t nil "--batch" "--eval" timed-form)
                      (buffer-string)))
           (elapsed (and (string-match "MY-ELAPSED \\([0-9.]+\\)" output)
                         (string-to-number (match-string 1 output)))))
      (should elapsed)
      (should (< elapsed 2.5)))))

;; CI runs an older Emacs than the one used day to day, so a call to an
;; Emacs 31-only function that isn't guarded only blows up there. Statically
;; scan the config files for these symbols instead of relying on the running
;; Emacs. A use counts as guarded when it sits in the body of a `when'/`if'
;; (then branch)/`and' whose test requires Emacs >= 31 or `fboundp's/`boundp's
;; the symbol. Add symbols here as new Emacs 31 features are adopted.
(defconst my/emacs31-only-symbols
  '(;; Modes
    mouse-shift-adjust-mode find-function-mode prettify-special-glyphs-mode
    delete-selection-local-mode center-line-mode delete-trailing-whitespace-mode
    system-taskbar-mode icalendar-mode conf-npmrc-mode mhtml-ts-mode
    go-work-ts-mode
    ;; Commands
    copy-theme-options unix-word-rubout unix-filename-rubout unfill-paragraph
    fill-paragraph-semlf fill-region-as-paragraph-semlf
    shell-command-do-open native-compile-directory
    ;; Functions
    garbage-collect-heapsize char-displayable-on-frame-p frame-initial-p
    set-local plusp minusp oddp evenp drop-while take-while
    hash-table-contains-p color-blend dom-inner-text truncate-string-pixelwise
    remove-display-text-property completion-table-with-metadata
    multiple-command-partition-arguments ensure-proper-list
    buffer-local-toplevel-value set-buffer-local-toplevel-value
    ;; Macros
    static-when static-unless setopt-local incf decf with-work-buffer cond*)
  "Symbols that only exist in Emacs 31 and later (from etc/NEWS of Emacs 31).
Deliberately omits generic names like `all' and `any' that would false-positive.")

(defun my/emacs31-guard-p (test sym)
  "Return non-nil if TEST guarantees Emacs 31+ or that SYM is defined."
  (cond
   ((not (consp test)) nil)
   ((and (memq (car test) '(>= > <= <))
         (eq (cadr test) 'emacs-major-version)
         (integerp (nth 2 test)))
    (pcase (car test)
      ('>= (>= (nth 2 test) 31))
      ('> (>= (nth 2 test) 30))
      (_ nil)))
   ((and (memq (car test) '(fboundp boundp))
         (equal (cadr test) `(quote ,sym)))
    t)
   ((eq (car test) 'and)
    (seq-some (lambda (x) (my/emacs31-guard-p x sym)) (cdr test)))
   (t nil)))

(defun my/find-unguarded-symbol (form sym &optional guarded)
  "Return non-nil if FORM uses SYM outside a guard (GUARDED non-nil if inside one)."
  (cond
   ((eq form sym) (not guarded))
   ((not (consp form)) nil)
   ((not (proper-list-p form))
    (or (my/find-unguarded-symbol (car form) sym guarded)
        (my/find-unguarded-symbol (cdr form) sym guarded)))
   ((and (eq (car form) 'quote) (eq (cadr form) sym)) (not guarded))
   ((and (memq (car form) '(when if and)) (cdr form))
    (let* ((test (cadr form))
           (inner (or guarded (my/emacs31-guard-p test sym)))
           (rest (cddr form)))
      (or (and (not inner) (my/find-unguarded-symbol test sym guarded))
          (and (eq (car form) 'if) rest
               (or (my/find-unguarded-symbol (car rest) sym inner)
                   (seq-some (lambda (x) (my/find-unguarded-symbol x sym guarded))
                             (cdr rest))))
          (and (not (eq (car form) 'if))
               (seq-some (lambda (x) (my/find-unguarded-symbol x sym inner))
                         rest)))))
   (t (seq-some (lambda (x) (my/find-unguarded-symbol x sym guarded)) form))))

(defun my/read-all-forms (file)
  "Return the list of top-level forms in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (let (forms)
      (goto-char (point-min))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(ert-deftest my/test-no-unguarded-emacs31-functions ()
  "Test that Emacs 31-only symbols are only used behind a version/fboundp guard."
  (let (offenders)
    (dolist (file (directory-files user-emacs-directory t "\\.el\\'"))
      (unless (string-match-p "tests?\\.el\\'" file)
        (dolist (form (my/read-all-forms file))
          (dolist (sym my/emacs31-only-symbols)
            (when (my/find-unguarded-symbol form sym)
              (push (format "%s: %s" (file-name-nondirectory file) sym)
                    offenders))))))
    (should (null offenders))))
