;; -*- lexical-binding: t; -*-

;;; Commentary:
;; This runs before init.el, before package.el initializes, and before the
;; first frame is created. Used for startup GC tuning and frame parameters
;; that need to be set before the first frame exists.

;;; Code:

;; GC during startup is pure waste -- loading ~70 packages trips the
;; default 800KB `gc-cons-threshold' constantly (measured: 48 GCs,
;; accounting for ~36% of init time on this config). Raise it for the
;; duration of startup; `gcmh-mode' (see init.el) takes over adaptive
;; steady-state management once packages are loaded.
(setq gc-cons-threshold most-positive-fixnum)

;; `file-name-handler-alist' is consulted on every `require'/`load'/
;; `file-exists-p' call (tramp, jka-compr, epa, ...), each doing a regexp
;; match against the path. Loading ~70 packages through elpaca means
;; thousands of these lookups for no benefit during startup. Blank it out
;; here and restore the original value once init has finished.
(defvar my--file-name-handler-alist-backup file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my--file-name-handler-alist-backup)))

;; Packages are managed by elpaca (see init.el) now, not package.el. Emacs
;; normally auto-activates package.el (and its previously-installed
;; ~/.emacs.d/elpa packages) before init.el is even loaded; left on, that
;; races elpaca's own bootstrap and use-package integration.
(setq package-enable-at-startup nil)

;; Disable the tool bar before the first frame is created. This is cheaper
;; than disabling it later.
(push '(tool-bar-lines . 0) default-frame-alist)

;; Set the real startup font (family + size) before the first frame is
;; created instead of via `set-face-attribute' in init.el. Doing it here
;; means the frame is born the right size instead of being resized right
;; after. `frame-inhibit-implied-resize' below covers the remaining cases
;; where init.el still changes fonts after startup (e.g. the nano-like
;; theme's family swap on theme change).
;; This is also the single definition of the fixed-pitch family used by
;; init.el. The font must exist: a missing font here makes Emacs abort the
;; GUI frame and fall back to a terminal frame, and it can't be probed
;; from early-init (`find-font' returns nil before the first GUI frame).
(defvar my-default-fixed-pitch-font "DejaVuSansM Nerd Font"
  "Default fixed-pitch font family.")
(defvar my-default-font-size 16
  "Startup font size in points.")
(push `(font . ,(format "%s-%d" my-default-fixed-pitch-font my-default-font-size))
      default-frame-alist)
(setq frame-inhibit-implied-resize t)

;; Font-cache compaction runs after every GC and gets noticeably slower
;; once a lot of glyphs from an icon font (nerd-icons, used in the
;; mode-line and dired) are in play. Skip it.
(setq inhibit-compacting-font-caches t)

;; Turn off the file path in the  title bar
(setq frame-title-format nil)

;;; early-init.el ends here
