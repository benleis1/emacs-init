;;; macos.el --- mac specific config   -*- lexical-binding: t; -*-

;;; Commentary:

;; macOS-specific configuration, loaded only when `system-type' is `darwin'. See init.el for the
;; load point and rationale on ordering.

;;; Code:

(eval-when-compile (require 'use-package))

;; GUI Emacs on macOS is launched by launchd, not a login shell, so it only
;; gets a minimal PATH/exec-path -- Homebrew-installed tools like aspell,
;; jdtls and pgformatter aren't visible to `executable-find' without this.
;; But exec-path-from-shell is relatively expensive so as compromise
;; just add homebrew onto the path as needed
(unless (member "/opt/homebrew/bin" exec-path)
  (add-to-list 'exec-path "/opt/homebrew/bin"))

;; Deal with dark/light mode macos ui elements like the scrollbar
(declare-function ns-auto-titlebar-mode "ns-auto-titlebar")
(use-package ns-auto-titlebar
  :ensure t
  :config
  (ns-auto-titlebar-mode 1))

;; Copy to clipboard functions for terminal mode
;; copy the current region directly
(defun pbcopy-region ()
  "Invoke pbcopy."
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbcopy-kill-ring (&optional _xpush)
  "Copy the latest kill run entry."
  (interactive)
  (let ((process-connection-type nil)
	(text (current-kill 0)))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; Final version hook into interprogram-cut-function instead
;; for terminal mode cut to system clipboard
(defun paste-for-osx (text &optional _push)
  "Hook to make pasting of TEXT work."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(unless window-system
  (setq interprogram-cut-function 'paste-for-osx))

;; Appointment alert sound; the advice that uses it lives in init.el.
(defun play-mac-sound (sound-name)
  "Play a macOS system SOUND-NAME asynchronously."
  (let ((sound-path (format "/System/Library/Sounds/%s.aiff" sound-name)))
    (if (file-exists-p sound-path)
        (start-process "mac-sound" nil "afplay" sound-path)
      (message "Sound file not found: %s" sound-path))))

(defun my-play-appt-sound ()
  "Appointment alert sound, called by `my-appt-glass-bell' in init.el."
  (play-mac-sound "Glass"))

;; Frame transparency + blur parameters, applied by `my-alpha-blend--apply'
;; in init.el. Needs the emacs-plus `frame-transparency' community patch (see
;; ~/.config/emacs-plus/build.yml); on an unpatched build these parameters are
;; simply ignored.
(defvar my-alpha-frame-params)
(defvar my-alpha-frame-reset-params)
(setq my-alpha-frame-params '((alpha-background . 0.60)
                              (ns-background-blur . 20)
                              (ns-alpha-elements ns-alpha-all))
      my-alpha-frame-reset-params '((alpha-background . 1.0)
                                    (ns-background-blur . 0)
                                    (ns-alpha-elements)))

(provide 'macos)
;;; macos.el ends here
