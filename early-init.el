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

;; Packages are managed by elpaca (see init.el) now, not package.el. Emacs
;; normally auto-activates package.el (and its previously-installed
;; ~/.emacs.d/elpa packages) before init.el is even loaded; left on, that
;; races elpaca's own bootstrap and use-package integration.
(setq package-enable-at-startup nil)

;; Disable the tool bar before the first frame is created. This is cheaper
;; than disabling it later.
(push '(tool-bar-lines . 0) default-frame-alist)

;;; early-init.el ends here
