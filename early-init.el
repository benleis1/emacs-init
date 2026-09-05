;; -*- lexical-binding: t; -*-

;;; Commentary:
;; This runs before init.el, before package.el initializes, and before the
;; first frame is created. Currently only used for startup GC tuning.

;;; Code:

;; GC during startup is pure waste -- loading ~70 packages trips the
;; default 800KB `gc-cons-threshold' constantly (measured: 48 GCs,
;; accounting for ~36% of init time on this config). Raise it for the
;; duration of startup, then drop to a saner standing value once startup
;; is done so GC pauses move to a sane cadence during editing instead of
;; just being deferred.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 32 1024 1024))))

;;; early-init.el ends here
