;;; package --- early-init     -*- lexical-binding: t; -*-

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

;;; Startup profiling
;; `emacs --profile-init' profiles startup with the built-in profiler and shows the reports once
;; elpaca has finished init. It starts here to capture as much of startup as possible. The no-op
;; switch handler keeps emacs from complaining about an unknown option.
;;
;; `my-toggle-profiler' (adapted from Doom Emacs): the first call starts sampling CPU and memory,
;; the second shows the report and stops.
;;
;; With `--profile-init' this file does the first call, sets `my-profile-init-p' and runs
;; `my-profile-init-finish' once elpaca has finished init. That also shows the time per section of
;; init.el (recorded by the `my-profile-mark' calls there) and the use-package report.

(defvar my-profile-init-p nil
  "Non-nil when Emacs was started with --profile-init.")
(defvar my-profiler-running nil
  "Non-nil while the profiler started by `my-toggle-profiler' is sampling.")
(defvar my-profile-marks nil
  "Reverse list of (NAME TIME PACKAGE-COUNT) recorded by `my-profile-mark'.")

(defvar use-package-compute-statistics)
(defvar use-package-statistics)
(declare-function use-package-report "use-package-core")
(declare-function profiler-report "profiler")
(declare-function profiler-stop "profiler")
(eval-when-compile (require 'cl-lib))

(defun my-toggle-profiler ()
  "Toggle the Emacs profiler.  Run it again to see the profiling report."
  (interactive)
  (if my-profiler-running
      (progn (profiler-report) (profiler-stop))
    (profiler-start 'cpu+mem))
  (setq my-profiler-running (not my-profiler-running)))

(defun my-profile-mark (name)
  "Record that the init section NAME starts now, if --profile-init was given."
  (when my-profile-init-p
    (push (list name (float-time)
                (if (boundp 'use-package-statistics) (hash-table-count use-package-statistics) 0))
          my-profile-marks)))

;; The wall column is the time between consecutive marks, so a section containing an `elpaca-wait'
;; absorbs the processing of every use-package queued before it. The use-package column sums the
;; time use-package measured for the packages declared in the section. Its :use-package-secs
;; already includes the nested :init and :config times.
(defun my-profile-report-phases ()
  "Show per-section init timing recorded by `my-profile-mark' in *init-phases*."
  (let ((marks (cons (list "startup before init.el" (float-time before-init-time) 0)
                     (reverse my-profile-marks)))
        (pkg-secs nil)
        (buf (get-buffer-create "*init-phases*")))
    (when (boundp 'use-package-statistics)
      (maphash (lambda (_name stats)
                 (push (float-time (gethash :use-package-secs stats '(0 0 0 0))) pkg-secs))
               use-package-statistics)
      (setq pkg-secs (nreverse pkg-secs)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%-42s %10s %17s\n" "Section" "wall (ms)" "use-package (ms)"))
        (cl-loop for (mark next) on marks while next
                 do (insert (format "%-42s %10.0f %17.0f\n" (nth 0 mark)
                                    (* 1000 (- (nth 1 next) (nth 1 mark)))
                                    (* 1000 (apply #'+ (seq-subseq pkg-secs (nth 2 mark)
                                                                   (nth 2 next)))))))
        (insert (format "\nTotal wall: %.0f ms\n"
                        (* 1000 (- (nth 1 (car my-profile-marks)) (nth 1 (car marks))))))
        (special-mode)))
    (pop-to-buffer buf)))

(defun my-profile-init-finish ()
  "Show the init section timings, `use-package' report and profiler report."
  (my-profile-mark "init done")
  (my-profile-report-phases)
  (use-package-report)
  (my-toggle-profiler))

(when (member "--profile-init" command-line-args)
  (add-to-list 'command-switch-alist '("--profile-init" . ignore))
  (require 'profiler)
  (profiler-start 'cpu+mem)
  (setq my-profiler-running t
        my-profile-init-p t
        ;; Per package timing. This has to be on before use-package is loaded.
        use-package-compute-statistics t)
  (add-hook 'elpaca-after-init-hook #'my-profile-init-finish))

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

;; Turn off the file path in the  title bar and the proxy icon
(setq frame-title-format nil)
(defvar ns-use-proxy-icon) ; only exists in NS builds; avoids a byte-compile warning elsewhere
(when (eq system-type 'darwin)
  (setq ns-use-proxy-icon nil))

;; Guardrail for load to always use newer .el files over .elc ones.
(setq load-prefer-newer t)

;;; early-init.el ends here
