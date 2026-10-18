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

;; Runs in a fresh Emacs rather than this one: the features it checks can be
;; loaded by other tests (org-agenda by the meeting-log tests) or as a side
;; effect of a failure elsewhere (ERT opens a failed test's source file, which
;; fires `find-file-hook' and enables diff-hl in a git checkout).
(ert-deftest my/test-deferred-packages-stay-deferred ()
  "Test that none of `my/deferred-package-features' load merely from init.el."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (init (expand-file-name "init.el" user-emacs-directory))
         (form (format "(progn (load %S t) (princ (format \"MY-LOADED %%S\" (seq-filter #'featurep '%S))))"
                       init my/deferred-package-features))
         (output (with-temp-buffer
                   (call-process emacs nil t nil "-Q" "--batch" "--eval" form)
                   (buffer-string))))
    (should (string-match "MY-LOADED \\(.*\\)" output))
    (should (null (read (match-string 1 output))))))

;; Crude safeguard against startup-time regressions like the org-timegrid one
;; above: spawn a genuinely fresh Emacs process (this test file's own load of
;; init.el, above, only measures a warm reload) and fail if a cold start
;; takes too long. Threshold is 2.5s rather than a tight 1s: local batch
;; timings cluster around 0.93-1.0s, but GitHub's shared ubuntu-latest
;; runners are noticeably slower and noisier than a local machine -- 1.5s
;; still flaked there. 2.5s keeps enough headroom for that CI variance while
;; still catching a regression the size of the org-timegrid one (which took
;; cold start from ~1.1s to ~1.5s).
;; TODO: on github this is much slower so disabling for local only for now.
(defun  my/test-startup-time-under-threshold ()
  "Test that loading early-init.el + init.el in a fresh Emacs takes < 2.5s.

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
      (should (< elapsed 2.5)))))

;; CI runs an older Emacs than the one used day to day, so a call to an
;; Emacs 31-only function that isn't guarded only blows up there. Statically
;; scan the config files for these symbols instead of relying on the running
;; Emacs. A use counts as guarded when it sits in the body of a `when'/`if'
;; (then branch)/`and' whose test requires Emacs >= 31 or `fboundp's/`boundp's
;; the symbol. Add symbols here as new Emacs 31 features are adopted.
(defconst my/emacs31-only-symbols
  '(;; Modes
    mouse-shift-adjust-mode find-function-mode prettify-special-glyphs-mode
    delete-selection-local-mode center-line-mode delete-trailing-whitespace-mode
    system-taskbar-mode icalendar-mode conf-npmrc-mode mhtml-ts-mode
    go-work-ts-mode
    ;; Commands
    copy-theme-options unix-word-rubout unix-filename-rubout unfill-paragraph
    fill-paragraph-semlf fill-region-as-paragraph-semlf
    shell-command-do-open native-compile-directory
    ;; Functions
    garbage-collect-heapsize char-displayable-on-frame-p frame-initial-p
    set-local plusp minusp oddp evenp drop-while take-while
    hash-table-contains-p color-blend dom-inner-text truncate-string-pixelwise
    remove-display-text-property completion-table-with-metadata
    multiple-command-partition-arguments ensure-proper-list
    buffer-local-toplevel-value set-buffer-local-toplevel-value
    ;; Macros
    static-when static-unless setopt-local incf decf with-work-buffer cond*)
  "Symbols that only exist in Emacs 31 and later (from etc/NEWS of Emacs 31).
Deliberately omits generic names like `all' and `any' that would false-positive.")

(defun my/emacs31-guard-p (test sym)
  "Return non-nil if TEST guarantees Emacs 31+ or that SYM is defined."
  (cond
   ((not (consp test)) nil)
   ((and (memq (car test) '(>= > <= <))
         (eq (cadr test) 'emacs-major-version)
         (integerp (nth 2 test)))
    (pcase (car test)
      ('>= (>= (nth 2 test) 31))
      ('> (>= (nth 2 test) 30))
      (_ nil)))
   ((and (memq (car test) '(fboundp boundp))
         (equal (cadr test) `(quote ,sym)))
    t)
   ((eq (car test) 'and)
    (seq-some (lambda (x) (my/emacs31-guard-p x sym)) (cdr test)))
   (t nil)))

(defun my/find-unguarded-symbol (form sym &optional guarded)
  "Return non-nil if FORM uses SYM outside a guard (GUARDED non-nil if inside one)."
  (cond
   ((eq form sym) (not guarded))
   ((not (consp form)) nil)
   ((not (proper-list-p form))
    (or (my/find-unguarded-symbol (car form) sym guarded)
        (my/find-unguarded-symbol (cdr form) sym guarded)))
   ((and (eq (car form) 'quote) (eq (cadr form) sym)) (not guarded))
   ((and (memq (car form) '(when if and)) (cdr form))
    (let* ((test (cadr form))
           (inner (or guarded (my/emacs31-guard-p test sym)))
           (rest (cddr form)))
      (or (and (not inner) (my/find-unguarded-symbol test sym guarded))
          (and (eq (car form) 'if) rest
               (or (my/find-unguarded-symbol (car rest) sym inner)
                   (seq-some (lambda (x) (my/find-unguarded-symbol x sym guarded))
                             (cdr rest))))
          (and (not (eq (car form) 'if))
               (seq-some (lambda (x) (my/find-unguarded-symbol x sym inner))
                         rest)))))
   (t (seq-some (lambda (x) (my/find-unguarded-symbol x sym guarded)) form))))

(defun my/read-all-forms (file)
  "Return the list of top-level forms in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (let (forms)
      (goto-char (point-min))
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(ert-deftest my/test-no-unguarded-emacs31-functions ()
  "Test that Emacs 31-only symbols are only used behind a version/fboundp guard."
  (let (offenders)
    (dolist (file (append (directory-files user-emacs-directory t "\\.el\\'")
                          (directory-files (expand-file-name "lisp" user-emacs-directory) t "\\.el\\'")))
      (unless (string-match-p "tests?\\.el\\'" file)
        (dolist (form (my/read-all-forms file))
          (dolist (sym my/emacs31-only-symbols)
            (when (my/find-unguarded-symbol form sym)
              (push (format "%s: %s" (file-name-nondirectory file) sym)
                    offenders))))))
    (should (null offenders))))

;;; Appointments, meeting log and jsonrpc filters (init.el)
;; These run against temp buffers/files and rebind the global state they touch;
;; nothing reads or writes ~/org.
(require 'cl-lib)

(defmacro my/tests--at-time (hour minute &rest body)
  "Run BODY with the current time of day, per `decode-time', set to HOUR:MINUTE.
The date stays real: `time-to-days' is built on `decode-time', so a made-up
date would disagree with `my-appt-dismissed-day' and trigger its daily reset."
  (declare (indent 2))
  (let ((orig (make-symbol "orig")))
    `(let ((,orig (symbol-function 'decode-time)))
       (cl-letf (((symbol-function 'decode-time)
                  (lambda (&optional time &rest args)
                    (let ((decoded (apply ,orig time args)))
                      (if time
                          decoded
                        (append (list 0 ,minute ,hour) (nthcdr 3 decoded)))))))
         ,@body))))

(defmacro my/tests--with-appt-state (&rest body)
  "Run BODY with the appointment mode-line state reset and colors pinned."
  `(let ((appt-time-msg-list nil)
         (appt-mode-string nil)
         (appt-message-warning-time 10)
         (my-appt-dismissed-keys nil)
         (my-appt-overdue-list nil)
         (my-appt-beeped-keys nil)
         (my-appt--pre-check-due nil)
         (my-appt-dismissed-day (time-to-days nil))
         (my-appt-mode-line-color-far "#000000")
         (my-appt-mode-line-color-near "#ff0000"))
     ,@body))

(defun my/tests--appt (minutes title &optional warn)
  "An `appt-time-msg-list' entry for TITLE at MINUTES since midnight."
  (list (list minutes) title nil warn))

(defun my/tests--face-prop (string prop)
  "PROP from the `face' plist on the first character of STRING."
  (plist-get (get-text-property 0 'face string) prop))

;;;; `my-appt-mode-line-intensity'

(ert-deftest my/test-appt-intensity-is-zero-at-start-of-window ()
  (should (= 0.0 (my-appt-mode-line-intensity 10 10))))

(ert-deftest my/test-appt-intensity-is-one-when-due ()
  (should (= 1.0 (my-appt-mode-line-intensity 0 10))))

(ert-deftest my/test-appt-intensity-is-eased-quadratically ()
  (should (= 0.25 (my-appt-mode-line-intensity 5 10))))

(ert-deftest my/test-appt-intensity-clamps-outside-window ()
  (should (= 0.0 (my-appt-mode-line-intensity 99 10))))

(ert-deftest my/test-appt-intensity-tolerates-zero-warn-time ()
  (should (= 1.0 (my-appt-mode-line-intensity 0 0))))

;;;; `my-appt-mode-line-color'

(ert-deftest my/test-appt-color-interpolates-between-endpoints ()
  (my/tests--with-appt-state
   (should (equal "#000000" (my-appt-mode-line-color 0.0)))
   (should (equal "#ff0000" (my-appt-mode-line-color 1.0)))
   (should (equal "#7f0000" (my-appt-mode-line-color 0.5)))))

(ert-deftest my/test-appt-color-falls-back-to-near-color-when-unresolvable ()
  (my/tests--with-appt-state
   (let ((my-appt-mode-line-color-far "not-a-color"))
     (should (equal "#ff0000" (my-appt-mode-line-color 0.5))))))

;;;; `my-appt-due-list'

(ert-deftest my/test-appt-due-list-includes-appointments-inside-the-window ()
  (my/tests--with-appt-state
   (let ((appt-time-msg-list (list (my/tests--appt 605 "Standup"))))
     (my/tests--at-time 10 0
       (should (equal '((5 "Standup" 10 (605 . "Standup")))
                      (my-appt-due-list)))))))

(ert-deftest my/test-appt-due-list-window-edges-are-inclusive ()
  (my/tests--with-appt-state
   (let ((appt-time-msg-list (list (my/tests--appt 600 "now")
                                   (my/tests--appt 610 "edge"))))
     (my/tests--at-time 10 0
       (should (equal '("now" "edge") (mapcar #'cadr (my-appt-due-list))))))))

(ert-deftest my/test-appt-due-list-excludes-past-and-far-future ()
  (my/tests--with-appt-state
   (let ((appt-time-msg-list (list (my/tests--appt 599 "past")
                                   (my/tests--appt 611 "far"))))
     (my/tests--at-time 10 0
       (should (null (my-appt-due-list)))))))

(ert-deftest my/test-appt-due-list-honors-per-appointment-warn-time ()
  (my/tests--with-appt-state
   (let ((appt-time-msg-list (list (my/tests--appt 630 "early heads-up" 30))))
     (my/tests--at-time 10 0
       (should (equal '((30 "early heads-up" 30 (630 . "early heads-up")))
                      (my-appt-due-list)))))))

;;;; `my-appt-glass-bell'

(ert-deftest my/test-appt-bell-rings-once-for-a-newly-due-appointment ()
  (my/tests--with-appt-state
   (let ((appt-time-msg-list (list (my/tests--appt 605 "Standup")))
         first second)
     (my/tests--at-time 10 0
       (my-appt-glass-bell (lambda (&rest _) (setq first ring-bell-function)))
       (my-appt-glass-bell (lambda (&rest _) (setq second ring-bell-function)))
       (should (eq first #'my-play-appt-sound))
       (should (eq second #'ignore))
       (should (equal '((605 . "Standup")) my-appt-beeped-keys))))))

(ert-deftest my/test-appt-bell-is-silent-for-a-dismissed-appointment ()
  (my/tests--with-appt-state
   (let ((appt-time-msg-list (list (my/tests--appt 605 "Standup")))
         (my-appt-dismissed-keys '((605 . "Standup")))
         seen)
     (my/tests--at-time 10 0
       (my-appt-glass-bell (lambda (&rest _) (setq seen ring-bell-function)))
       (should (eq seen #'ignore))
       (should (null my-appt-beeped-keys))))))

(ert-deftest my/test-appt-bell-snapshots-due-list-for-the-update-hook ()
  (my/tests--with-appt-state
   (let ((appt-time-msg-list (list (my/tests--appt 605 "Standup"))))
     (my/tests--at-time 10 0
       (my-appt-glass-bell #'ignore)
       (should (equal '((5 "Standup" 10 (605 . "Standup")))
                      my-appt--pre-check-due))))))

(ert-deftest my/test-appt-bell-passes-arguments-and-result-through ()
  (my/tests--with-appt-state
   (should (equal '(a b) (my-appt-glass-bell (lambda (&rest args) args) 'a 'b)))))

;;;; `my-appt-mode-line-update'

(ert-deftest my/test-appt-update-shows-countdown-for-due-appointment ()
  (my/tests--with-appt-state
   (let ((appt-time-msg-list (list (my/tests--appt 605 "Standup"))))
     (my/tests--at-time 10 0
       (my-appt-mode-line-update)
       (should (string-prefix-p "⏰ 5m" appt-mode-string))
       (should (string-match-p "Standup" (get-text-property 0 'help-echo appt-mode-string)))
       (should (eq 'normal (my/tests--face-prop appt-mode-string :weight)))))))

(ert-deftest my/test-appt-update-says-now-at-zero-minutes-and-turns-bold ()
  (my/tests--with-appt-state
   (let ((appt-time-msg-list (list (my/tests--appt 600 "Standup"))))
     (my/tests--at-time 10 0
       (my-appt-mode-line-update)
       (should (string-prefix-p "⏰ now" appt-mode-string))
       (should (eq 'bold (my/tests--face-prop appt-mode-string :weight)))))))

(ert-deftest my/test-appt-update-uses-the-soonest-of-several-appointments ()
  (my/tests--with-appt-state
   (let ((appt-time-msg-list (list (my/tests--appt 608 "Later")
                                   (my/tests--appt 603 "Sooner"))))
     (my/tests--at-time 10 0
       (my-appt-mode-line-update)
       (should (string-prefix-p "⏰ 3m" appt-mode-string))
       (let ((echo (get-text-property 0 'help-echo appt-mode-string)))
         (should (string-match-p "Later" echo))
         (should (string-match-p "Sooner" echo)))))))

(ert-deftest my/test-appt-update-hides-dismissed-appointments ()
  (my/tests--with-appt-state
   (let ((appt-time-msg-list (list (my/tests--appt 605 "Standup")))
         (my-appt-dismissed-keys '((605 . "Standup"))))
     (my/tests--at-time 10 0
       (my-appt-mode-line-update)
       (should (null appt-mode-string))))))

(ert-deftest my/test-appt-update-marks-a-matured-appointment-overdue ()
  (my/tests--with-appt-state
   ;; `appt-check' already removed the appointment from `appt-time-msg-list'.
   (let ((my-appt--pre-check-due '((0 "Standup" 10 (600 . "Standup")))))
     (my/tests--at-time 10 0
       (my-appt-mode-line-update)
       (should (equal '(("Standup" . (600 . "Standup"))) my-appt-overdue-list))
       (should (string-prefix-p "⏰ Due" appt-mode-string))
       (should (string-match-p "Standup" (get-text-property 0 'help-echo appt-mode-string)))
       ;; A second update must not add it again.
       (my-appt-mode-line-update)
       (should (= 1 (length my-appt-overdue-list)))))))

(ert-deftest my/test-appt-update-does-not-mark-a-dismissed-appointment-overdue ()
  (my/tests--with-appt-state
   (let ((my-appt--pre-check-due '((0 "Standup" 10 (600 . "Standup"))))
         (my-appt-dismissed-keys '((600 . "Standup"))))
     (my/tests--at-time 10 0
       (my-appt-mode-line-update)
       (should (null my-appt-overdue-list))
       (should (null appt-mode-string))))))

(ert-deftest my/test-appt-update-ignores-an-appointment-that-was-not-yet-due ()
  (my/tests--with-appt-state
   (let ((my-appt--pre-check-due '((3 "Standup" 10 (603 . "Standup")))))
     (my/tests--at-time 10 0
       (my-appt-mode-line-update)
       (should (null my-appt-overdue-list))))))

(ert-deftest my/test-appt-update-prefers-countdown-over-overdue ()
  (my/tests--with-appt-state
   (let ((appt-time-msg-list (list (my/tests--appt 605 "Next")))
         (my-appt-overdue-list '(("Old" . (550 . "Old")))))
     (my/tests--at-time 10 0
       (my-appt-mode-line-update)
       (should (string-prefix-p "⏰ 5m" appt-mode-string))
       (should (equal '(("Old" . (550 . "Old"))) my-appt-overdue-list))))))

(ert-deftest my/test-appt-update-drops-overdue-entries-that-were-dismissed ()
  (my/tests--with-appt-state
   (let ((my-appt-overdue-list '(("Old" . (550 . "Old"))))
         (my-appt-dismissed-keys '((550 . "Old"))))
     (my/tests--at-time 10 0
       (my-appt-mode-line-update)
       (should (null my-appt-overdue-list))
       (should (null appt-mode-string))))))

(ert-deftest my/test-appt-update-resets-state-on-a-new-day ()
  (my/tests--with-appt-state
   (let ((my-appt-dismissed-day 0)
         (my-appt-dismissed-keys '((605 . "Standup")))
         (my-appt-beeped-keys '((605 . "Standup")))
         (my-appt-overdue-list '(("Old" . (550 . "Old")))))
     (my/tests--at-time 10 0
       (my-appt-mode-line-update)
       (should (null my-appt-dismissed-keys))
       (should (null my-appt-beeped-keys))
       (should (null my-appt-overdue-list))
       (should (= (time-to-days nil) my-appt-dismissed-day))))))

(ert-deftest my/test-appt-update-keeps-state-within-the-same-day ()
  (my/tests--with-appt-state
   (let ((my-appt-dismissed-keys '((605 . "Standup"))))
     (my/tests--at-time 10 0
       (my-appt-mode-line-update)
       (should (equal '((605 . "Standup")) my-appt-dismissed-keys))))))

;;;; `my-appt-mode-line-dismiss'

(ert-deftest my/test-appt-dismiss-logs-each-appointment-once ()
  (my/tests--with-appt-state
   (let ((appt-time-msg-list (list (my/tests--appt 605 "Standup")))
         (my-appt-overdue-list '(("Old" . (550 . "Old"))))
         logged)
     (cl-letf (((symbol-function 'my-meeting-log-record-dismissed-appointments)
                (lambda (keys) (push keys logged))))
       (my/tests--at-time 10 0
         (my-appt-mode-line-dismiss nil)
         (should (equal '(((605 . "Standup") (550 . "Old"))) logged))
         (should (member '(605 . "Standup") my-appt-dismissed-keys))
         (should (member '(550 . "Old") my-appt-dismissed-keys))
         (should (null my-appt-overdue-list))
         (should (null appt-mode-string))
         ;; A repeat click logs nothing new.
         (my-appt-mode-line-dismiss nil)
         (should (equal '(nil ((605 . "Standup") (550 . "Old"))) logged)))))))

;;;; Calendar parsing

(defmacro my/tests--with-org-buffer (text &rest body)
  "Run BODY in a temp Org buffer holding TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (let ((org-mode-hook nil)) (org-mode))
     ,@body))

(defun my/tests--fmt (time)
  (format-time-string "%F %R" time))

(ert-deftest my/test-meeting-log-parse-returns-minutes-title-and-times ()
  (my/tests--with-org-buffer "* Standup                :meeting:\n<2026-03-02 Mon 10:00-10:30>\n"
    (let ((entries (my-meeting-log--parse-calendar-entries)))
      (should (= 1 (length entries)))
      (pcase-let ((`(,minutes ,title ,start ,end) (car entries)))
        (should (= 600 minutes))
        (should (equal "Standup" title))
        (should (equal "2026-03-02 10:00" (my/tests--fmt start)))
        (should (equal "2026-03-02 10:30" (my/tests--fmt end)))))))

(ert-deftest my/test-meeting-log-parse-keeps-calendar-order ()
  (my/tests--with-org-buffer
      (concat "* First\n<2026-03-02 Mon 09:15-10:00>\n"
              "* Second\n<2026-03-02 Mon 13:45-14:15>\n")
    (let ((entries (my-meeting-log--parse-calendar-entries)))
      (should (equal '("First" "Second") (mapcar #'cadr entries)))
      (should (equal '(555 825) (mapcar #'car entries))))))

(ert-deftest my/test-meeting-log-parse-uses-link-description-as-title ()
  (my/tests--with-org-buffer
      "* [[https://teams.example.com/abc][Weekly Sync]] :meeting:\n<2026-03-02 Mon 11:00-11:30>\n"
    (should (equal '("Weekly Sync")
                   (mapcar #'cadr (my-meeting-log--parse-calendar-entries))))))

(ert-deftest my/test-meeting-log-parse-skips-declined-untimed-and-undated-entries ()
  (my/tests--with-org-buffer
      (concat "* Declined: Offsite\n<2026-03-02 Mon 10:00-11:00>\n"
              "* All day thing\n<2026-03-02 Mon>\n"
              "* No timestamp at all\n"
              "* Kept\n<2026-03-02 Mon 15:00-15:30>\n")
    (should (equal '("Kept")
                   (mapcar #'cadr (my-meeting-log--parse-calendar-entries))))))

(ert-deftest my/test-meeting-log-parse-reads-the-given-buffer ()
  (let ((other (generate-new-buffer " *calendar*")))
    (unwind-protect
        (progn
          (with-current-buffer other
            (insert "* Elsewhere\n<2026-03-02 Mon 08:00-08:30>\n")
            (let ((org-mode-hook nil)) (org-mode)))
          (with-temp-buffer
            (should (equal '("Elsewhere")
                           (mapcar #'cadr (my-meeting-log--parse-calendar-entries other))))))
      (kill-buffer other))))

;;;; `my-meeting-log--match-entry'

(defconst my/tests--entries
  '((600 "Standup" start-a end-a)
    (600 "Design review for the new API" start-b end-b)
    (660 "Standup" start-c end-c))
  "Entries shaped like `my-meeting-log--parse-calendar-entries' output.")

(ert-deftest my/test-meeting-log-match-requires-the-same-start-minute ()
  (should (equal '(660 "Standup" start-c end-c)
                 (my-meeting-log--match-entry 660 "Standup" my/tests--entries))))

(ert-deftest my/test-meeting-log-match-prefers-first-matching-entry ()
  (should (equal '(600 "Standup" start-a end-a)
                 (my-meeting-log--match-entry 600 "Standup" my/tests--entries))))

(ert-deftest my/test-meeting-log-match-accepts-title-as-substring-of-heading ()
  (should (equal "Design review for the new API"
                 (cadr (my-meeting-log--match-entry 600 "Design review" my/tests--entries)))))

(ert-deftest my/test-meeting-log-match-accepts-heading-as-substring-of-title ()
  (should (equal '(660 "Standup" start-c end-c)
                 (my-meeting-log--match-entry 660 "Standup (room 4)" my/tests--entries))))

(ert-deftest my/test-meeting-log-match-nil-for-other-minute-or-title ()
  (should (null (my-meeting-log--match-entry 601 "Standup" my/tests--entries)))
  (should (null (my-meeting-log--match-entry 600 "Retro" my/tests--entries)))
  (should (null (my-meeting-log--match-entry 600 "Standup" nil))))

(ert-deftest my/test-meeting-log-match-does-not-treat-title-as-a-regexp ()
  (should (null (my-meeting-log--match-entry 600 "Stand.p" my/tests--entries))))

;;;; `my-meeting-log--ensure-week-summary'

(defun my/tests--summary (week)
  (format "* Clock Summary %s\n#+BEGIN: clocktable :scope file :block %s\n#+END:\n" week week))

(ert-deftest my/test-meeting-log-summary-inserted-into-empty-buffer ()
  (with-temp-buffer
    (my-meeting-log--ensure-week-summary "2026-W10")
    (should (equal (my/tests--summary "2026-W10") (buffer-string)))))

(ert-deftest my/test-meeting-log-summary-is-idempotent ()
  (with-temp-buffer
    (my-meeting-log--ensure-week-summary "2026-W10")
    (my-meeting-log--ensure-week-summary "2026-W10")
    (should (equal (my/tests--summary "2026-W10") (buffer-string)))))

(ert-deftest my/test-meeting-log-summary-added-for-each-new-week ()
  (with-temp-buffer
    (my-meeting-log--ensure-week-summary "2026-W10")
    (my-meeting-log--ensure-week-summary "2026-W11")
    (should (equal (concat (my/tests--summary "2026-W10") (my/tests--summary "2026-W11"))
                   (buffer-string)))))

(ert-deftest my/test-meeting-log-summary-starts-on-its-own-line ()
  (with-temp-buffer
    (insert "* Existing entry")
    (my-meeting-log--ensure-week-summary "2026-W10")
    (should (equal (concat "* Existing entry\n" (my/tests--summary "2026-W10"))
                   (buffer-string)))))

;;;; `my-meeting-log--append-clock'

(defun my/tests--time (year month day hour minute)
  (encode-time (list 0 minute hour day month year nil -1 nil)))

(defmacro my/tests--with-temp-org-file (var &rest body)
  "Run BODY with VAR bound to a fresh temp .org file, cleaned up afterwards."
  (declare (indent 1))
  `(let ((,var (make-temp-file "my-tests-" nil ".org")))
     (unwind-protect
         (progn ,@body)
       (when-let* ((buf (find-buffer-visiting ,var)))
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf))
       (delete-file ,var))))

(defun my/tests--file-string (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defmacro my/tests--with-clock-settings (&rest body)
  "Pin the Org clock/duration options `my-meeting-log--append-clock' reads.
Loads the libraries first: binding their variables before they are declared
special would make the `let' lexical."
  `(let ((_ (progn (require 'org-clock) (require 'org-duration)))
         (org-clock-into-drawer t)
         (org-duration-format '((special . h:mm))))
     ,@body))

(ert-deftest my/test-meeting-log-append-clock-writes-a-clocked-heading ()
  (my/tests--with-temp-org-file log
    (let ((my-meeting-log-file log))
      (my/tests--with-clock-settings
       (my-meeting-log--append-clock "Standup"
                                     (my/tests--time 2026 3 2 10 0)
                                     (my/tests--time 2026 3 2 10 30))
       (let ((text (my/tests--file-string log)))
         (should (string-match-p "^\\* Clock Summary 2026-W10$" text))
         (should (string-match-p "^\\* Standup$" text))
         (should (string-match-p ":LOGBOOK:" text))
         (should (string-match-p
                  "CLOCK: \\[2026-03-02 Mon 10:00\\]--\\[2026-03-02 Mon 10:30\\] =>  0:30"
                  text)))))))

(ert-deftest my/test-meeting-log-append-clock-omits-drawer-when-disabled ()
  (my/tests--with-temp-org-file log
    (let ((my-meeting-log-file log))
      (my/tests--with-clock-settings
       (let ((org-clock-into-drawer nil))
         (my-meeting-log--append-clock "Standup"
                                       (my/tests--time 2026 3 2 10 0)
                                       (my/tests--time 2026 3 2 11 30)))
       (let ((text (my/tests--file-string log)))
         (should-not (string-match-p ":LOGBOOK:" text))
         (should (string-match-p "=>  1:30" text)))))))

(ert-deftest my/test-meeting-log-append-clock-refreshes-the-clocktable ()
  (my/tests--with-temp-org-file log
    (let ((my-meeting-log-file log))
      (my/tests--with-clock-settings
       (my-meeting-log--append-clock "Standup"
                                     (my/tests--time 2026 3 2 10 0)
                                     (my/tests--time 2026 3 2 10 30))
       (should (string-match-p "\\*Total time\\* +| +\\*0:30\\*"
                               (my/tests--file-string log)))))))

(ert-deftest my/test-meeting-log-append-clock-shares-one-summary-per-week ()
  (my/tests--with-temp-org-file log
    (let ((my-meeting-log-file log))
      (my/tests--with-clock-settings
       (my-meeting-log--append-clock "One" (my/tests--time 2026 3 2 10 0) (my/tests--time 2026 3 2 10 30))
       (my-meeting-log--append-clock "Two" (my/tests--time 2026 3 3 10 0) (my/tests--time 2026 3 3 10 30))
       (my-meeting-log--append-clock "Three" (my/tests--time 2026 3 10 10 0) (my/tests--time 2026 3 10 10 30))
       (let ((text (my/tests--file-string log)))
         (should (= 2 (cl-count-if (lambda (l) (string-prefix-p "* Clock Summary" l))
                                   (split-string text "\n"))))
         (should (string-match-p "^\\* Clock Summary 2026-W10$" text))
         (should (string-match-p "^\\* Clock Summary 2026-W11$" text))
         (dolist (title '("One" "Two" "Three"))
           (should (string-match-p (format "^\\* %s$" title) text))))))))

(ert-deftest my/test-meeting-log-append-clock-uses-the-iso-week-year ()
  (my/tests--with-temp-org-file log
    (let ((my-meeting-log-file log))
      (my/tests--with-clock-settings
       ;; Mon 2025-12-29 belongs to ISO week 1 of 2026, not week 53 of 2025.
       (my-meeting-log--append-clock "Year end"
                                     (my/tests--time 2025 12 29 10 0)
                                     (my/tests--time 2025 12 29 10 30))
       (should (string-match-p "^\\* Clock Summary 2026-W01$" (my/tests--file-string log)))))))

(ert-deftest my/test-meeting-log-append-clock-never-logs-negative-time ()
  (my/tests--with-temp-org-file log
    (let ((my-meeting-log-file log))
      (my/tests--with-clock-settings
       (my-meeting-log--append-clock "Backwards"
                                     (my/tests--time 2026 3 2 11 0)
                                     (my/tests--time 2026 3 2 10 0))
       (should (string-match-p "=>  0:00" (my/tests--file-string log)))))))

;;;; `my-meeting-log-record-dismissed-appointments'

(defun my/tests--calendar-text (day-time &rest entries)
  "Org text with one heading per (TITLE START END) in ENTRIES on DAY-TIME's date."
  (mapconcat (lambda (e)
               (format "* %s :meeting:\n%s\n" (nth 0 e)
                       (format-time-string
                        (format "<%%Y-%%m-%%d %%a %s-%s>" (nth 1 e) (nth 2 e))
                        day-time)))
             entries ""))

(defmacro my/tests--with-calendar (text &rest body)
  "Run BODY with `my-meetings-file' holding TEXT and a fresh `my-meeting-log-file'.
Messages are collected into the variable `messages'."
  (declare (indent 1))
  `(my/tests--with-temp-org-file cal
     (my/tests--with-temp-org-file log
       (with-temp-file cal (insert ,text))
       (let ((my-meetings-file cal)
             (my-meeting-log-file log)
             (org-mode-hook nil)
             messages)
         (my/tests--with-clock-settings
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
            ,@body))))))

(ert-deftest my/test-meeting-log-record-logs-todays-matching-appointment ()
  (my/tests--with-calendar
      (my/tests--calendar-text nil '("Standup" "10:00" "10:30"))
    (my-meeting-log-record-dismissed-appointments '((600 . "Standup")))
    (let ((text (my/tests--file-string my-meeting-log-file)))
      (should (string-match-p "^\\* Standup$" text))
      (should (string-match-p "=>  0:30" text)))
    (should-not (cl-some (lambda (m) (string-prefix-p "my-meeting-log:" m)) messages))))

(ert-deftest my/test-meeting-log-record-logs-every-key ()
  (my/tests--with-calendar
      (my/tests--calendar-text nil '("Standup" "10:00" "10:30") '("Review" "14:00" "15:00"))
    (my-meeting-log-record-dismissed-appointments '((600 . "Standup") (840 . "Review")))
    (let ((text (my/tests--file-string my-meeting-log-file)))
      (should (string-match-p "^\\* Standup$" text))
      (should (string-match-p "^\\* Review$" text))
      (should (string-match-p "=>  1:00" text)))))

(ert-deftest my/test-meeting-log-record-reports-an-unmatched-appointment ()
  (my/tests--with-calendar
      (my/tests--calendar-text nil '("Standup" "10:00" "10:30"))
    (my-meeting-log-record-dismissed-appointments '((601 . "Standup")))
    (should (cl-some (lambda (m) (string-match-p "no calendar.org match" m)) messages))
    (should (equal "" (my/tests--file-string my-meeting-log-file)))))

(ert-deftest my/test-meeting-log-record-ignores-appointments-from-other-days ()
  (my/tests--with-calendar
      (my/tests--calendar-text (time-subtract nil (* 24 60 60))
                               '("Standup" "10:00" "10:30"))
    (my-meeting-log-record-dismissed-appointments '((600 . "Standup")))
    (should (cl-some (lambda (m) (string-match-p "no calendar.org match" m)) messages))
    (should (equal "" (my/tests--file-string my-meeting-log-file)))))

(ert-deftest my/test-meeting-log-record-never-logs-declined-meetings ()
  (my/tests--with-calendar
      (my/tests--calendar-text nil '("Declined: Standup" "10:00" "10:30"))
    (my-meeting-log-record-dismissed-appointments '((600 . "Standup")))
    (should (equal "" (my/tests--file-string my-meeting-log-file)))))

(ert-deftest my/test-meeting-log-record-isolates-failures-between-keys ()
  (my/tests--with-calendar
      (my/tests--calendar-text nil '("Standup" "10:00" "10:30") '("Review" "14:00" "15:00"))
    (let ((real (symbol-function 'my-meeting-log--append-clock)))
      (cl-letf (((symbol-function 'my-meeting-log--append-clock)
                 (lambda (title &rest args)
                   (if (equal title "Standup")
                       (error "disk on fire")
                     (apply real title args)))))
        (my-meeting-log-record-dismissed-appointments '((600 . "Standup") (840 . "Review")))))
    (should (cl-some (lambda (m) (string-match-p "couldn't log \"Standup\": disk on fire" m))
                     messages))
    (let ((text (my/tests--file-string my-meeting-log-file)))
      (should-not (string-match-p "^\\* Standup$" text))
      (should (string-match-p "^\\* Review$" text)))))

;;;; jsonrpc log filters

(defconst my/tests--big-text (make-string 200 ?x))

(ert-deftest my/test-jsonrpc-elide-text-document-replaces-only-the-text ()
  (let* ((params (list :textDocument (list :uri "file:///a.java" :version 3
                                           :text my/tests--big-text)))
         (result (my-jsonrpc-elide-text-document params))
         (td (plist-get result :textDocument)))
    (should (equal "<elided>" (plist-get td :text)))
    (should (equal "file:///a.java" (plist-get td :uri)))
    (should (equal 3 (plist-get td :version)))))

(ert-deftest my/test-jsonrpc-elide-text-document-does-not-mutate-its-input ()
  (let ((params (list :textDocument (list :uri "file:///a.java" :text my/tests--big-text))))
    (my-jsonrpc-elide-text-document params)
    (should (equal my/tests--big-text
                   (plist-get (plist-get params :textDocument) :text)))))

(ert-deftest my/test-jsonrpc-elide-text-document-keeps-other-params ()
  (let ((result (my-jsonrpc-elide-text-document
                 (list :textDocument (list :text "t") :position '(:line 1)))))
    (should (equal '(:line 1) (plist-get result :position)))))

(ert-deftest my/test-jsonrpc-elide-text-document-returns-params-unchanged-when-nothing-to-elide ()
  (let ((no-text (list :textDocument (list :uri "file:///a.java")))
        (no-doc (list :position '(:line 1))))
    (should (eq no-text (my-jsonrpc-elide-text-document no-text)))
    (should (eq no-doc (my-jsonrpc-elide-text-document no-doc)))
    (should (null (my-jsonrpc-elide-text-document nil)))
    (should (equal "str" (my-jsonrpc-elide-text-document "str")))))

(ert-deftest my/test-jsonrpc-elide-token-replaces-the-token ()
  (let ((result (my-jsonrpc-elide-token (list :token "abc-123" :value '(:kind "report")))))
    (should (equal "<elided>" (plist-get result :token)))
    (should (equal '(:kind "report") (plist-get result :value)))))

(ert-deftest my/test-jsonrpc-elide-token-does-not-mutate-its-input ()
  (let ((params (list :token "abc-123")))
    (my-jsonrpc-elide-token params)
    (should (equal "abc-123" (plist-get params :token)))))

(ert-deftest my/test-jsonrpc-elide-token-returns-params-unchanged-without-token ()
  (let ((params (list :value 1)))
    (should (eq params (my-jsonrpc-elide-token params)))
    (should (null (my-jsonrpc-elide-token nil)))))

(ert-deftest my/test-jsonrpc-log-text-summarizes-progress-reports ()
  (should (equal "report: Building workspace"
                 (my-jsonrpc-log-text
                  '(:method "$/progress"
                    :params (:token "t" :value (:kind "report" :message "Building workspace")))))))

(ert-deftest my/test-jsonrpc-log-text-tolerates-sparse-progress-messages ()
  (should (equal ": " (my-jsonrpc-log-text '(:method "$/progress" :params (:value nil)))))
  (should (equal "report: " (my-jsonrpc-log-text
                             '(:method "$/progress" :params (:value (:kind "report")))))))

(ert-deftest my/test-jsonrpc-log-text-shows-params-without-the-file-contents ()
  (let ((text (my-jsonrpc-log-text
               (list :method "textDocument/didOpen"
                     :params (list :textDocument (list :uri "file:///a.java"
                                                       :text my/tests--big-text))))))
    (should (string-match-p "file:///a.java" text))
    (should (string-match-p "<elided>" text))
    (should-not (string-match-p my/tests--big-text text))))

(ert-deftest my/test-jsonrpc-log-text-empty-for-a-method-without-params ()
  (should (equal "" (my-jsonrpc-log-text '(:method "initialized")))))

(ert-deftest my/test-jsonrpc-log-text-formats-results-and-errors ()
  (should (equal "=> (:ok t)" (my-jsonrpc-log-text '(:id 1 :result (:ok t)))))
  (should (equal "ERROR (:code -32601)" (my-jsonrpc-log-text '(:id 1 :error (:code -32601))))))

(ert-deftest my/test-jsonrpc-log-text-nil-for-an-unrecognized-message ()
  (should (null (my-jsonrpc-log-text '(:id 1)))))

(ert-deftest my/test-jsonrpc-skip-message-suppresses-progress-begin-and-end ()
  (dolist (kind '("begin" "end"))
    (should (my-jsonrpc-skip-message-p
             `(:method "$/progress" :params (:value (:kind ,kind)))))))

(ert-deftest my/test-jsonrpc-skip-message-keeps-everything-else ()
  (should-not (my-jsonrpc-skip-message-p
               '(:method "$/progress" :params (:value (:kind "report")))))
  (should-not (my-jsonrpc-skip-message-p '(:method "$/progress" :params (:value nil))))
  (should-not (my-jsonrpc-skip-message-p
               '(:method "window/logMessage" :params (:value (:kind "begin")))))
  (should-not (my-jsonrpc-skip-message-p '(:id 1 :result nil)))
  (should-not (my-jsonrpc-skip-message-p nil)))

(ert-deftest my/test-jsonrpc-log-event-advice-is-installed ()
  "The advice targets a private jsonrpc function; fail loudly if it goes away."
  (require 'jsonrpc)
  (should (fboundp 'jsonrpc--log-event))
  (should (advice--p (symbol-function 'jsonrpc--log-event))))

;;; Config-wide checks: byte-compilation and references

(defconst my/repo-root
  (locate-dominating-file (or load-file-name buffer-file-name default-directory) "install.sh")
  "Checkout root (where install.sh and the docs live), or nil if not found.")

(defun my/config-source-files ()
  "The config's own non-test Lisp files, from the checkout when there is one.
`user-emacs-directory' is the fallback because on a real install its lisp/
also holds machine-local files (local.el and the like) that aren't ours."
  (let ((root (or my/repo-root user-emacs-directory)))
    (seq-filter (lambda (f) (and (file-exists-p f) (not (string-match-p "tests?\\.el\\'" f))))
                (append (list (expand-file-name "early-init.el" root)
                              (expand-file-name "init.el" root))
                        (directory-files (expand-file-name "lisp" root) t "\\.el\\'")))))

;;;; Byte-compile cleanliness
;; Catches typos, wrong arity, unknown functions and the like in every config
;; file, which also guards the byte-compile cleanups in the commit history.

(defun my/byte-compile-warnings (file)
  "Byte-compile FILE into a temp dir and return its warning/error lines."
  (let* ((tmpdir (make-temp-file "my-bytecomp" t))
         (log (generate-new-buffer-name "*my-compile-log*"))
         (byte-compile-log-buffer log)
         (byte-compile-dest-file-function
          (lambda (f) (expand-file-name (concat (file-name-nondirectory f) "c") tmpdir)))
         warnings)
    (unwind-protect
        (progn
          (unless (byte-compile-file file)
            (push (format "%s: byte-compile-file failed" (file-name-nondirectory file))
                  warnings))
          (when (get-buffer log)
            (with-current-buffer log
              (goto-char (point-min))
              ;; Only FILE's own diagnostics: loading other code (e.g. a
              ;; machine-specific local.el) can log into the same buffer.
              (while (re-search-forward
                      (format "^%s:.*\\(?:Warning\\|Error\\): .*$"
                              (regexp-quote (file-name-nondirectory file)))
                      nil t)
                (push (match-string 0) warnings)))))
      (when (get-buffer log) (kill-buffer log))
      (delete-directory tmpdir t))
    (nreverse warnings)))

(ert-deftest my/test-lisp-byte-compile-helper-reports-problems ()
  "The checker must actually fail on a bad file, or the test below proves nothing."
  (let ((file (make-temp-file "my-bad-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";; -*- lexical-binding: t; -*-\n"
                    "(defun my-bad-fn (x) (my-undefined-function-xyz x 1))\n"
                    "(defun my-bad-fn2 () (car))\n"))
          (let ((warnings (my/byte-compile-warnings file)))
            (should (cl-some (lambda (w) (string-match-p "my-undefined-function-xyz" w)) warnings))
            (should (cl-some (lambda (w) (string-match-p "called with 0 arguments" w)) warnings))))
      (delete-file file))))

(ert-deftest my/test-lisp-byte-compile-helper-is-quiet-on-a-clean-file ()
  (let ((file (make-temp-file "my-good-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ";; -*- lexical-binding: t; -*-\n(defun my-good-fn (x) (1+ x))\n"))
          (should (null (my/byte-compile-warnings file))))
      (delete-file file))))

(ert-deftest my/test-lisp-files-byte-compile-without-warnings ()
  "Every config file byte-compiles without warnings."
  (should (null (apply #'append (mapcar #'my/byte-compile-warnings
                                        (my/config-source-files))))))

;;;; Functions named in hooks, advice and key bindings must exist
;; A typo in `(add-hook 'x #'my-fn)' or a `:bind' entry only shows up when the
;; hook or key fires. Statically collect the function symbols the config
;; hands to hooks/advice/keymaps/`use-package' and check any in this config's
;; own namespaces are defined somewhere in it (or at least `fboundp').

(defconst my/own-symbol-regexp "\\`\\(?:my[-/]\\|tab2-\\|dape-java-\\)"
  "Function names that belong to this config rather than a package.")

(defconst my/function-ref-arg-positions
  '((add-hook . 2) (remove-hook . 2) (advice-add . 3) (advice-remove . 2)
    (define-key . 3) (keymap-set . 3) (keymap-global-set . 2) (keymap-local-set . 2)
    (global-set-key . 2) (local-set-key . 2) (bind-key . 2))
  "Calls that take a function at argument position N (0 is the call's name).")

(defun my/walk-form (fn form)
  "Call FN on FORM and every list nested in it, tolerating dotted lists."
  (when (consp form)
    (funcall fn form)
    (let ((x form))
      (while (consp x)
        (my/walk-form fn (car x))
        (setq x (cdr x))))))

(defun my/function-ref (x &optional bare-ok)
  "The function symbol X names, if X is #'SYM or 'SYM (or a bare symbol if BARE-OK)."
  (cond ((and (consp x) (memq (car x) '(function quote)) (symbolp (cadr x))
              (cadr x) (not (keywordp (cadr x))))
         (cadr x))
        ((and bare-ok (symbolp x) x (not (eq x t)) (not (keywordp x))) x)))

(defun my/bind-spec-refs (spec)
  "Function symbols in a `use-package' :bind SPEC."
  (cond ((not (consp spec)) nil)
        ((or (stringp (car spec)) (vectorp (car spec)))
         (let ((f (my/function-ref (cdr spec) t))) (and f (list f))))
        ((eq (car spec) :map) (my/bind-spec-refs (cddr spec)))
        (t (apply #'append (mapcar #'my/bind-spec-refs spec)))))

(defun my/hook-spec-refs (spec)
  "Function symbols in a `use-package' :hook SPEC of (HOOK . FUNCTION) pairs."
  (cond ((not (consp spec)) nil)
        ((and (symbolp (car spec)) (cdr spec) (symbolp (cdr spec)))
         (list (cdr spec)))
        ((and (consp (car spec)) (cdr spec) (symbolp (cdr spec)))
         (list (cdr spec)))
        ((proper-list-p spec) (apply #'append (mapcar #'my/hook-spec-refs spec)))))

(defun my/collect-function-refs (forms)
  "Return (SYMBOL . CALL) for each function symbol FORMS give to a hook/binding."
  (let (refs)
    (dolist (top forms)
      (my/walk-form
       (lambda (form)
         (let ((head (car form)))
           (cond
            ((and (symbolp head) (assq head my/function-ref-arg-positions))
             (when-let* ((sym (my/function-ref
                               (nth (cdr (assq head my/function-ref-arg-positions)) form))))
               (push (cons sym head) refs)))
            ((eq head 'use-package)
             (let (kw)
               (dolist (e (cddr form))
                 (if (keywordp e)
                     (setq kw e)
                   (dolist (sym (pcase kw
                                  ((or :bind :bind*) (my/bind-spec-refs e))
                                  (:hook (my/hook-spec-refs e))))
                     (push (cons sym (intern (format "use-package%s" kw))) refs)))))))))
       top))
    (nreverse refs)))

(defun my/collect-defined-functions (forms)
  "Names defined with defun/defmacro/defalias/etc. anywhere in FORMS."
  (let (defined)
    (dolist (top forms)
      (my/walk-form
       (lambda (form)
         (when (memq (car form) '(defun defmacro defsubst defalias cl-defun cl-defmacro
                                   cl-defgeneric cl-defmethod define-minor-mode
                                   define-globalized-minor-mode define-derived-mode))
           (let ((name (my/function-ref (cadr form) t)))
             (when name (push name defined)))))
       top))
    defined))

(ert-deftest my/test-function-refs-collects-hooks-advice-and-bindings ()
  (let ((refs (my/collect-function-refs
               '((add-hook 'foo-hook #'my-hooked)
                 (add-hook 'foo-hook (lambda () (my-inside-lambda)))
                 (advice-add 'bar :around #'my-advice)
                 (define-key map "k" 'my-cmd)
                 (keymap-global-set "C-c x" #'my-global)
                 (with-eval-after-load 'org (add-hook 'org-mode-hook #'my-nested))
                 (use-package pkg
                   :bind (("a" . my-bound) (:map pkg-map ("b" . my-mapped)))
                   :hook (prog-mode . my-use-package-hook)
                   :config (add-hook 'baz-hook #'my-config-hook))
                 (use-package other :bind ("c" . my-single))))))
    (should (equal '(my-advice my-bound my-cmd my-config-hook my-global my-hooked my-mapped
                     my-nested my-single my-use-package-hook)
                   (sort (mapcar #'car refs) #'string<)))
    (should-not (assq 'my-inside-lambda refs))))

(ert-deftest my/test-function-refs-collects-definitions-at-any-depth ()
  (should (equal '(my-a my-b my-c my-d)
                 (sort (my/collect-defined-functions
                        '((defun my-a ())
                          (with-eval-after-load 'x (defun my-b ()))
                          (defalias 'my-c #'ignore)
                          (define-minor-mode my-d "doc")))
                       #'string<))))

(ert-deftest my/test-hooked-and-bound-functions-are-defined ()
  (let* ((forms (apply #'append (mapcar #'my/read-all-forms (my/config-source-files))))
         (defined (my/collect-defined-functions forms))
         offenders)
    (dolist (ref (my/collect-function-refs forms))
      (let ((sym (car ref)))
        (when (and (string-match-p my/own-symbol-regexp (symbol-name sym))
                   (not (memq sym defined))
                   (not (fboundp sym)))
          (push (format "%s (via %s)" sym (cdr ref)) offenders))))
    (should (null (delete-dups (nreverse offenders))))))

;;;; Docs and install.sh point at files that exist

(defun my/doc-link-targets (file)
  "Relative file targets of the Markdown links/images in FILE."
  (let (targets)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "\\]([ \t]*\\([^) \t\n]+\\)" nil t)
        (let ((target (car (split-string (match-string 1) "#"))))
          (when (and (not (string-empty-p target))
                     (not (string-match-p "\\`[a-z]+:" target))
                     (string-match-p "\\.\\(?:png\\|jpe?g\\|gif\\|svg\\|md\\|el\\|org\\|sh\\)\\'" target))
            (push target targets)))))
    (nreverse targets)))

(ert-deftest my/test-doc-link-targets-extracts-relative-files-only ()
  (let ((file (make-temp-file "my-doc-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "![shot](./images/a.png) [doc](../docs/b.md#sec) "
                    "[web](https://x.example/c.png) [anchor](#top) [t](url \"title\") "
                    "[img](../images/d.png \"caption\")"))
          (should (equal '("./images/a.png" "../docs/b.md" "../images/d.png")
                         (my/doc-link-targets file))))
      (delete-file file))))

(ert-deftest my/test-doc-links-and-images-resolve ()
  "Markdown (and the init.el comments the readme is generated from) link to real files."
  (skip-unless my/repo-root)
  (let (missing)
    (dolist (file (append (list (expand-file-name "readme.md" my/repo-root)
                                (expand-file-name "changelog.md" my/repo-root)
                                (expand-file-name "init.el" my/repo-root))
                          (directory-files (expand-file-name "docs" my/repo-root) t "\\.md\\'")))
      (when (file-exists-p file)
        ;; init.el's comments become readme.md, so its links are relative to the root.
        (let ((base (if (string-suffix-p ".el" file) my/repo-root (file-name-directory file))))
          (dolist (target (my/doc-link-targets file))
            (unless (file-exists-p (expand-file-name target base))
              (push (format "%s -> %s" (file-relative-name file my/repo-root) target) missing))))))
    (should (null (nreverse missing)))))

(ert-deftest my/test-install-script-copies-files-that-exist ()
  "Every file install.sh links or copies out of the checkout is present."
  (skip-unless my/repo-root)
  (let ((script (expand-file-name "install.sh" my/repo-root))
        missing)
    (with-temp-buffer
      (insert-file-contents script)
      (goto-char (point-min))
      (while (re-search-forward
              "for f in \\([^;\n]+\\); do[ \t\n]+\\(?:cp\\|ln [^\"]*\\)\"\\$src/\\(?:\\([a-z]+\\)/\\)?\\$f\""
              nil t)
        (let ((dir (or (match-string 2) "")))
          (dolist (name (split-string (match-string 1)))
            (let ((path (if (string-empty-p dir) name (concat dir "/" name))))
              (unless (file-exists-p (expand-file-name path my/repo-root))
                (push path missing)))))))
    (should (null (nreverse missing)))))

(ert-deftest my/test-install-script-links-every-lisp-file-that-init-loads ()
  "Each non-test, non-theme lisp/*.el is `load'ed or `require'd by init.el."
  (skip-unless my/repo-root)
  (let ((init (with-temp-buffer
                (insert-file-contents (expand-file-name "init.el" my/repo-root))
                (buffer-string)))
        orphans)
    (dolist (file (directory-files (expand-file-name "lisp" my/repo-root) nil "\\.el\\'"))
      (let ((name (file-name-sans-extension file)))
        (unless (or (string-match-p "tests?\\'" name) (string-suffix-p "-theme" name)
                    (string-match-p (format "(\\(?:load\\|require\\)[ \t]+['\"]%s\\(?:\\.el\\)?[\"') ]"
                                            (regexp-quote name))
                                    init))
          (push file orphans))))
    (should (null (nreverse orphans)))))

;;; ghostel dependencies
;; The config reaches into ghostel internals (`ghostel--*'); these tests fail
;; loudly after a ghostel update that renames or drops one of them.

(defun my/tests--ghostel-symbols-used ()
  "Every `ghostel-*' symbol referenced by init.el and lisp/*.el (not the tests)."
  (let (syms)
    (dolist (file (cons (expand-file-name "init.el" my/repo-root)
                        (seq-remove (lambda (f) (string-match-p "tests?\\.el\\'" f))
                                    (directory-files (expand-file-name "lisp" my/repo-root)
                                                     t "\\.el\\'"))))
      (with-temp-buffer
        (insert-file-contents file)
        (while (re-search-forward "\\_<\\(ghostel-[-a-z0-9]+\\)\\_>" nil t)
          (push (intern (match-string 1)) syms))))
    (delete-dups syms)))

(defun my/tests--ghostel-available-p ()
  (and (require 'ghostel nil t) (require 'ghostel-prompt nil t)))

(ert-deftest my/test-ghostel-symbols-used-by-config-exist ()
  "Every ghostel symbol the config references is still defined by ghostel."
  (skip-unless my/repo-root)
  (skip-unless (my/tests--ghostel-available-p))
  (let ((missing (seq-remove (lambda (s) (or (boundp s) (fboundp s) (featurep s)))
                             (my/tests--ghostel-symbols-used))))
    (should (null missing))))

(ert-deftest my/test-ghostel-command-running-tracks-osc133 ()
  "`ghostel--command-running' is nil in the start hook, t after it, nil after finish.
The busy indicators rely on this ordering: the hooks fire before the flag
changes, which is why the refresh is deferred to a timer."
  (skip-unless (my/tests--ghostel-available-p))
  (skip-unless (fboundp 'ghostel--osc133-marker))
  (with-temp-buffer
    (let (seen-start seen-finish)
      (setq-local ghostel-command-start-functions
                  (list (lambda (_buf) (setq seen-start (list ghostel--command-running)))))
      (setq-local ghostel-command-finish-functions
                  (list (lambda (_buf _exit) (setq seen-finish (list ghostel--command-running)))))
      (setq-local ghostel--command-running nil)
      (ghostel--osc133-marker "C" nil)
      (should (equal seen-start '(nil)))
      (should ghostel--command-running)
      (ghostel--osc133-marker "D" "0")
      (should (equal seen-finish '(t)))
      (should-not ghostel--command-running))))
