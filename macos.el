;; -*- lexical-binding: t; -*-
;; macOS-specific configuration, loaded only when `system-type' is `darwin'.
;; See init.el for the load point and rationale on ordering.

;; GUI Emacs on macOS is launched by launchd, not a login shell, so it only
;; gets a minimal PATH/exec-path -- Homebrew-installed tools like aspell,
;; jdtls and pgformatter aren't visible to `executable-find' without this.
;; But exec-path-from-shell is relatively expensive so as compromise
;; just add homebrew onto the path as needed
(unless (member "/opt/homebrew/bin" exec-path)
  (add-to-list 'exec-path "/opt/homebrew/bin"))

;; Deal with dark/light mode macos ui elements like the scrollbar
(use-package ns-auto-titlebar
  :ensure t
  :config
  (ns-auto-titlebar-mode 1))

;; Copy to clipboard functions for terminal mode
;; copy the current region directly
(defun pbcopy-region ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

;; copy the latest kill ring
(defun pbcopy-kill-ring (&optional _xpush)
  (interactive)
  (let ((process-connection-type nil)
	(text (current-kill 0)))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

;; Final version hook into interprogram-cut-function instead
;; for terminal mode cut to system clipboard
(defun paste-for-osx (text &optional _push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(unless window-system
  (setq interprogram-cut-function 'paste-for-osx))

;; Appointment alert sound; the advice that uses it lives in init.el.
(defun play-mac-sound (sound-name)
  "Play a macOS system sound asynchronously."
  (let ((sound-path (format "/System/Library/Sounds/%s.aiff" sound-name)))
    (if (file-exists-p sound-path)
        (start-process "mac-sound" nil "afplay" sound-path)
      (message "Sound file not found: %s" sound-path))))

;; Appointment alert sound, called by `my-appt-glass-bell' in init.el.
(defun my-play-appt-sound ()
  (play-mac-sound "Glass"))
