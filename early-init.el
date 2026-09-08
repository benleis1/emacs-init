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

;; Disable the tool bar before the first frame is created. Calling
;; `tool-bar-mode' in init.el (after the frame already has a tool bar)
;; measured ~104ms, presumably from a native AppKit window-resize round
;; trip to remove it. Setting this via `default-frame-alist' means the
;; frame is simply created without one -- no removal needed, and
;; `tool-bar-mode' in init.el becomes a free no-op safety net.
(push '(tool-bar-lines . 0) default-frame-alist)

;;; early-init.el ends here
