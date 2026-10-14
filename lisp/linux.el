;; -*- lexical-binding: t; -*-
;; Linux-specific configuration (Ubuntu + Wayland/niri), loaded only when
;; `system-type' is `gnu/linux'. See init.el for the load point.
;; Needs: wl-clipboard (wl-copy) for terminal-mode clipboard, and
;; pipewire-bin (pw-play) or pulseaudio-utils (paplay) for the appt sound.

(let ((local-bin (expand-file-name "~/.local/bin")))
  (unless (member local-bin exec-path)
    (add-to-list 'exec-path local-bin)))

;; Terminal-mode kills go to the Wayland clipboard. GUI (pgtk) Emacs handles
;; the clipboard natively.
(defun my-wl-copy (text &optional _push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(when (and (not window-system)
           (getenv "WAYLAND_DISPLAY")
           (executable-find "wl-copy"))
  (setq interprogram-cut-function #'my-wl-copy))

(defun my-play-appt-sound ()
  "Play the appointment alert sound asynchronously."
  (let ((sound-path "/usr/share/sounds/freedesktop/stereo/complete.oga")
        (player (or (executable-find "pw-play") (executable-find "paplay"))))
    (cond ((not player) (message "No sound player (pw-play/paplay) found"))
          ((not (file-exists-p sound-path))
           (message "Sound file not found: %s" sound-path))
          (t (start-process "appt-sound" nil player sound-path)))))
