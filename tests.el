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
;; takes too long. Threshold is 1.5s rather than a tight 1s: local batch
;; timings cluster around 0.93-1.0s already, so 1.0s flat would flake on a
;; slower CI runner; 1.5s still catches a regression the size of the
;; org-timegrid one (which took cold start from ~1.1s to ~1.5s).
(ert-deftest my/test-startup-time-under-threshold ()
  "Test that loading early-init.el + init.el in a fresh Emacs takes < 1.5s.

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
      (should (< elapsed 1.5)))))
