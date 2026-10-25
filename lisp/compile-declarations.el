;;; compile-declarations.el --- Byte-compile predeclarations for init.el -*- lexical-binding: t -*-

;;; Commentary:

;; Everything that would otherwise trigger "free variable" or "function not known to be defined"
;; byte-compile warnings in init.el lives here, so init.el stays warning-clean without suppressing
;; the warnings globally.  A bare `defvar' only tells the compiler the variable is special (and
;; does nothing at load time); `declare-function' likewise only informs the compiler.  Most of
;; these are for packages that are loaded lazily, so they aren't available when init.el is
;; compiled.  When a new warning shows up, add it here under the owning package.  Packages that
;; are on the `load-path' during compilation (flymake gets elpaca's build directories via
;; `elisp-flymake-byte-compile-load-path') need no entry for what they define at load time; a bare
;; `emacs -Q' compile without those directories will show warnings for the pruned ones.
;;
;; This file is not loaded or required.  Those declarations only apply to the file being compiled,
;; so init.el splices these forms in at macroexpansion time with `my-include-declarations'.

;;; Code:

;; Elpaca macros (`elpaca') need to be known at compile time to expand rather than being compiled
;; as function calls.  Only done while compiling: loading elpaca before init.el's bootstrap has
;; defined `elpaca-installer-version' triggers elpaca's "installer version does not match" warning.
;; This is a no-op when elpaca isn't on the `load-path'.
(eval-when-compile
  (when (bound-and-true-p byte-compile-current-file)
    (add-to-list 'load-path (expand-file-name "elpaca/builds/elpaca" user-emacs-directory))
    (require 'elpaca nil t)
    ;; A bare `emacs -Q' compile (flymake) has no elpaca-use-package, so elpaca-style :ensure specs
    ;; fail to parse and package.el refreshes the archives over the network.  Already active when
    ;; compiling from the running session.
    (when (and (featurep 'elpaca) (not (bound-and-true-p elpaca-use-package-mode)))
      (require 'use-package)
      (when (require 'elpaca-use-package nil t)
        (elpaca-use-package-mode)
        (with-no-warnings (setq use-package-always-ensure t))))))
(eval-when-compile (require 'frameset))
;; Load the packages init.el configures lazily, at compile time only, so the compiler sees their
;; variables and functions without a declaration here.  Costs nothing at startup.  Load order
;; matters (`dape-java' needs `tab-line' first) and failures are ignored.
(eval-when-compile
  (dolist (feature '(tab-line ispell flyspell eglot jsonrpc flymake ediff ediff-util ediff-wind org org-agenda
                     org-clock org-element dape dape-java treesit treesit-fold pulsar diff-hl
                     markdown-mode imenu desktop dired diary-lib cl-lib zone
                     pixel-scroll profiler modus-themes excorporate wikimode ghostel ghostel-links
                     consult vertico marginalia yasnippet gcmh nerd-icons))
    (condition-case nil (require feature nil t) (error nil))))

;; elpaca
(declare-function elpaca--expand-declaration "elpaca")
(declare-function elpaca-process-queues "elpaca")
(declare-function elpaca-generate-autoloads "elpaca")
(declare-function elpaca-wait "elpaca")
(defvar native-comp-async-env-modifier-form)
(declare-function use-package-report "use-package-core")

;; Built-in packages
(declare-function cl-mapcar "cl-extra")
(declare-function widget-create-child-and-convert "wid-edit")
(declare-function widget-field-value-create "wid-edit")
(declare-function widget-value-set "wid-edit")
(defvar c-basic-offset)
(declare-function profiler-report "profiler")
(declare-function profiler-stop "profiler")
(declare-function dired-mouse-find-file "dired")
(declare-function diary-sort-entries "diary-lib")
(declare-function imenu--subalist-p "imenu")
(declare-function zone-when-idle "zone")
(declare-function treesit-buffer-root-node "treesit")
(declare-function treesit-node-children "treesit")
(declare-function ediff-next-difference "ediff-util")
(declare-function ediff-previous-difference "ediff-util")
(declare-function ediff-setup-windows-plain "ediff-wind")

;; org
(declare-function org-element--property "org-element-ast")
(defvar org-capture-templates)
(declare-function org-agenda-maybe-redo "org-agenda")
(declare-function org-clock-into-drawer "org-clock")
(declare-function org-duration-from-minutes "org-duration")
(declare-function org-element-timestamp-parser "org-element")
(declare-function org-get-heading "org")
(declare-function org-map-entries "org")
(declare-function org-self-insert-command "org")
(declare-function org-time-stamp-format "org")
(declare-function org-timestamp-has-time-p "org")
(declare-function org-timestamp-to-time "org")
(declare-function org-update-all-dblocks "org")

;; eglot, jsonrpc, treesit-fold
(declare-function eglot--project "eglot")
(declare-function eglot-code-actions "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-path-to-uri "eglot")
(declare-function eglot-shutdown "eglot")
(declare-function jsonrpc-request "jsonrpc")
(declare-function treesit-fold--cons-add "treesit-fold")
(declare-function treesit-fold--foldable-node-at-pos "treesit-fold")
(declare-function treesit-fold-close "treesit-fold")
(declare-function treesit-fold-mode "treesit-fold")
(declare-function treesit-fold-summary-javadoc "treesit-fold")
(declare-function treesit-fold-usable-mode-p "treesit-fold")

;; dape and dape-java
(declare-function dape "dape")
(declare-function dape--config-eval "dape")
(declare-function dape-java-get-test-bundle-vector "dape-java")
(declare-function dape-java-jdtls-support-p "dape-java")
(declare-function dape-java-junit-fetch-items-async "dape-java")
(declare-function dape-java-junit-item-line "dape-java")
(declare-function dape-java-junit-item-testlevel "dape-java")

;; ghostel
(declare-function ghostel "ghostel")
(declare-function ghostel-buffer-name-by-title "ghostel")

;; Other third-party packages
(defvar imenu-list-mode-line-format)
(declare-function mixed-pitch-mode "mixed-pitch")
(defvar modus-operandi-tinted-palette-overrides)
(defvar modus-vivendi-palette-overrides)
(defvar folio-theme-palette-overrides)
(defvar nano-like-palette-overrides)
(declare-function modus-themes--background-p "modus-themes")
(declare-function modus-themes-get-color-value "modus-themes")
(declare-function global-diff-hl-mode "diff-hl")
(declare-function diff-hl-flydiff-mode "diff-hl-flydiff")
(declare-function markdown--browse-url "markdown-mode")
(declare-function markdown-imenu-create-nested-index "markdown-mode")
(declare-function markdown-link-at-pos "markdown-mode")
(declare-function markdown-link-p "markdown-mode")
(declare-function excorporate "excorporate")
(declare-function wikimode-project-tags "wikimode")
(declare-function exco-org-show-day "excorporate-org")
(declare-function ilist-plus--build-mode-line-format "ilist-plus")
(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function nerd-icons-octicon "nerd-icons")

;; Files under lisp/
(declare-function my-modeline-mode "modeline")
(declare-function my-modeline--icon-safe "modeline")
(declare-function my-modeline--keyboard-icon "modeline")
(declare-function my-play-appt-sound "macos")
(declare-function tab2-register-buffer-name-function "tab-config")
(defvar my-modeline-dedicated-window-map)
(defvar my-default-fixed-pitch-font)

;; Variables owned by this file but defined or first set inside lazy forms
(defvar my-calendar-init)
(defvar my-jdtls-settings)
(defvar my-ghostel-escape-timeout)

;; Functions defined by this file inside `use-package' / `with-eval-after-load' bodies, and
;; advice generated by `define-advice' there.  The compiler can't see definitions nested in those
;; lambdas.
(declare-function my-ghostel--set-input-needed "init")
(declare-function my-ghostel-notify "init")
(declare-function my-ghostel-click-link "init")
(declare-function my-ghostel-escape "init")
(declare-function my-ghostel-kill-range "init")
(declare-function my-ghostel-kill-and-send "init")
(declare-function ghostel--on-user-input@my-clear-input-needed "init")
(declare-function ghostel@my-start-in-project-root "init")
(declare-function marginalia--buffer-status@my-ghostel-state "init")
(declare-function byte-compile-file@my/per-file-log "init")
(declare-function my-dape-toolbar-header-line-string "init")
(declare-function my-dape-toolbar-update-repl-header "init")
(declare-function my-dape--gutter-arrow-overlay "init")
(declare-function my-dape--gutter-ensure-margin "init")
(declare-function my-dape--junit-gutter-refresh-when-ready "init")
(declare-function my-dape-run-gutter-mode "init")
(declare-function my-class-run-gutter-clear "init")
(declare-function my-class-run-gutter-refresh "init")
(declare-function my-junit-test-gutter-clear "init")
(declare-function my-junit-test-gutter-refresh "init")
(declare-function simple-mode-line-buffer "init")
(declare-function my-get-buffer-state "init")
(declare-function my-restore-buffer-state "init")
(declare-function my-ediff-nav-button "init")
(declare-function my-ediff-nav-string "init")
(declare-function my-ediff-install-nav-overlay "init")

;;; compile-declarations.el ends here
