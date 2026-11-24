;; -*- lexical-binding: t; -*-
;; ERT tests for the pure/deterministic pieces of tab-plus.el: the
;; view list bookkeeping (`tab-plus-views'), the generic `tab-plus--find-first'
;; helper, the buffer/group filtering predicates, and the
;; persist-format round trip. Anything that renders an actual tab
;; line, moves real windows around, or depends on `vc'/project state
;; (`tab-plus--get-tabs', `tab-plus--format-tab', the mouse-drag commands, ...)
;; is out of scope here.
(require 'ert)
(require 'cl-lib)
(require 'tab-line)

;; In the normal CI flow (see .github/workflows/test.yml) `init.el' is
;; loaded first, which loads `tab-plus.el' itself -- so the
;; functions under test are already defined by the time this file
;; loads. This fallback just makes the file independently runnable too
;; (e.g. `emacs -Q --batch -l ert -l tab-plus-tests.el -f
;; ert-run-tests-batch-and-exit'), since `tab-plus.el' has no
;; `provide' of its own to key a `require' off of.
(unless (fboundp 'tab-plus--find-first)
  (load (expand-file-name "tab-plus.el"
                          (file-name-directory
                           (or load-file-name buffer-file-name default-directory)))))

;; `tab-plus-views'/the frame's `tab-line-sel-view' parameter are global
;; state that the real tab line reads and mutates continuously --
;; every test below runs against a throwaway view list instead of the
;; live one, restoring both afterwards so it can't bleed into other
;; tests (in either direction).
(defmacro tab-plus-tests--with-view-list (views start-pos &rest body)
  "Run BODY with `tab-plus-views' bound to VIEWS and the selected frame's
`tab-line-sel-view' parameter set to START-POS, restoring both
afterwards."
  (declare (indent 2))
  `(let ((tab-plus-tests--old-views tab-plus-views)
         (tab-plus-tests--old-sel (frame-parameter nil 'tab-line-sel-view)))
     (unwind-protect
         (progn
           (setq tab-plus-views ,views)
           (set-frame-parameter nil 'tab-line-sel-view ,start-pos)
           ,@body)
       (setq tab-plus-views tab-plus-tests--old-views)
       (set-frame-parameter nil 'tab-line-sel-view tab-plus-tests--old-sel))))

;;; `tab-plus--find-first'

(ert-deftest tab-plus-test/find-first-returns-first-match ()
  (should (equal (tab-plus--find-first (lambda (x) (> x 2)) '(1 2 3 4)) 3)))

(ert-deftest tab-plus-test/find-first-nil-when-no-match ()
  (should (null (tab-plus--find-first (lambda (x) (> x 10)) '(1 2 3)))))

(ert-deftest tab-plus-test/find-first-nil-on-empty-list ()
  (should (null (tab-plus--find-first #'identity nil))))

;;; View list bookkeeping: `tab-plus--get-view-by-name', `tab-plus-list-views',
;;; `tab-plus-num-views'

(ert-deftest tab-plus-test/get-view-by-name-finds-match ()
  (tab-plus-tests--with-view-list
   (list (make-tab-plus-view :name "default") (make-tab-plus-view :name "work")) 0
   (should (equal (tab-plus-view-name (tab-plus--get-view-by-name "work")) "work"))))

(ert-deftest tab-plus-test/get-view-by-name-nil-when-absent ()
  (tab-plus-tests--with-view-list (list (make-tab-plus-view :name "default")) 0
				    (should (null (tab-plus--get-view-by-name "nope")))))

(ert-deftest tab-plus-test/list-views-returns-names-in-order ()
  (tab-plus-tests--with-view-list
   (list (make-tab-plus-view :name "default") (make-tab-plus-view :name "work")
         (make-tab-plus-view :name "play"))
   0
   (should (equal (tab-plus-list-views) '("default" "work" "play")))))

(ert-deftest tab-plus-test/num-views-counts-views ()
  (tab-plus-tests--with-view-list
   (list (make-tab-plus-view :name "default") (make-tab-plus-view :name "work")) 0
   (should (eql (tab-plus-num-views) 2))))

;;; `tab-plus-new-view'

(ert-deftest tab-plus-test/new-view-errors-on-duplicate-name ()
  (tab-plus-tests--with-view-list (list (make-tab-plus-view :name "default")) 0
				    (should-error (tab-plus-new-view "default"))))

;;; `tab-plus-close-view-by-name'

(ert-deftest tab-plus-test/close-view-errors-on-default ()
  (tab-plus-tests--with-view-list (list (make-tab-plus-view :name "default")) 0
				    (should-error (tab-plus-close-view-by-name "default"))))

(ert-deftest tab-plus-test/close-view-errors-when-name-not-found ()
  (tab-plus-tests--with-view-list (list (make-tab-plus-view :name "default")) 0
				    (should-error (tab-plus-close-view-by-name "nope"))))

;;; `tab-plus-next-view'/`tab-plus-prev-view' wraparound

(ert-deftest tab-plus-test/next-view-wraps-from-last-to-first ()
  (tab-plus-tests--with-view-list
   (list (make-tab-plus-view :name "default") (make-tab-plus-view :name "work")) 1
   (tab-plus-next-view)
   (should (eql (frame-parameter nil 'tab-line-sel-view) 0))))

(ert-deftest tab-plus-test/prev-view-wraps-from-first-to-last ()
  (tab-plus-tests--with-view-list
   (list (make-tab-plus-view :name "default") (make-tab-plus-view :name "work")) 0
   (tab-plus-prev-view)
   (should (eql (frame-parameter nil 'tab-line-sel-view) 1))))

;;; `tab-plus--buffer-mode'/`tab-plus--buffer-filter'

(ert-deftest tab-plus-test/buffer-mode-returns-major-mode ()
  (with-temp-buffer
    (text-mode)
    (should (eq (tab-plus--buffer-mode (current-buffer)) 'text-mode))))

(ert-deftest tab-plus-test/buffer-filter-true-for-file-buffer ()
  (let* ((file (make-temp-file "tab-plus-test-")))
    (unwind-protect
        (let ((buf (find-file-noselect file)))
          (unwind-protect
              (should (tab-plus--buffer-filter buf))
            (kill-buffer buf)))
      (delete-file file))))

(ert-deftest tab-plus-test/buffer-filter-true-for-whitelisted-mode ()
  (with-temp-buffer
    (text-mode)
    (should (tab-plus--buffer-filter (current-buffer)))))

(ert-deftest tab-plus-test/buffer-filter-false-otherwise ()
  (with-temp-buffer
    (fundamental-mode)
    (should-not (tab-plus--buffer-filter (current-buffer)))))

;;; `tab-plus--filter-buffers-by-group'

(ert-deftest tab-plus-test/filter-buffers-by-group-nil-group-passes-through ()
  (with-temp-buffer
    (let ((bufs (list (current-buffer))))
      (should (equal (tab-plus--filter-buffers-by-group bufs nil) bufs)))))

(ert-deftest tab-plus-test/filter-buffers-by-group-files-keeps-only-file-buffers ()
  (let* ((file (make-temp-file "tab-plus-test-")))
    (unwind-protect
        (let ((file-buf (find-file-noselect file)))
          (unwind-protect
              (with-temp-buffer
                (let ((no-file-buf (current-buffer)))
                  (should (equal (tab-plus--filter-buffers-by-group
                                  (list file-buf no-file-buf) "Files")
                                 (list file-buf)))))
            (kill-buffer file-buf)))
      (delete-file file))))

(ert-deftest tab-plus-test/filter-buffers-by-group-modified-keeps-only-modified ()
  (let* ((file (make-temp-file "tab-plus-test-")))
    (unwind-protect
        (let ((file-buf (find-file-noselect file)))
          (unwind-protect
              (progn
                (with-current-buffer file-buf (insert "x"))
                (with-temp-buffer
                  (let ((unmodified-buf (current-buffer)))
                    (should (equal (tab-plus--filter-buffers-by-group
                                    (list file-buf unmodified-buf) "Modified")
                                   (list file-buf))))))
            (with-current-buffer file-buf (set-buffer-modified-p nil))
            (kill-buffer file-buf)))
      (delete-file file))))

;;; `tab-plus--convert-to-persist-format'/`tab-plus--convert-from-persist-format'

(ert-deftest tab-plus-test/persist-format-round-trips-name-and-buffers ()
  (with-temp-buffer
    (rename-buffer "tab-plus-test-persist-buf" t)
    (let* ((buf (current-buffer))
           (view (make-tab-plus-view :name "work" :buffers (list buf)))
           (persisted (tab-plus--convert-to-persist-format view))
           (restored (tab-plus--convert-from-persist-format persisted)))
      (should (equal (tab-plus-persist-view-name persisted) "work"))
      (should (equal (tab-plus-persist-view-buffernames persisted)
                     '("tab-plus-test-persist-buf")))
      (should (equal (tab-plus-view-name restored) "work"))
      (should (equal (tab-plus-view-buffers restored) (list buf))))))

;;; Tear off: `tab-plus--leave-window', `tab-plus--tear-off-buffer',
;;; `tab-plus--close-tear-off-view'

(ert-deftest tab-plus-test/leave-window-drops-tab-and-switches-away ()
  (let ((other (generate-new-buffer "tab-plus-test-other"))
        (torn (generate-new-buffer "tab-plus-test-torn")))
    (unwind-protect
        (save-window-excursion
          (tab-plus-tests--with-view-list
           (list (make-tab-plus-view :name "default" :buffers (list other torn)))
           0
           (switch-to-buffer other)
           (switch-to-buffer torn)
           (tab-plus--leave-window torn (selected-window))
           (should-not (memq torn (tab-plus--get-buffer-list)))
           (should (memq other (tab-plus--get-buffer-list)))
           (should-not (eq (window-buffer) torn))
           (should (buffer-live-p torn))))
      (kill-buffer other)
      (kill-buffer torn))))

(ert-deftest tab-plus-test/leave-window-resyncs-tab-group-to-new-buffer ()
  ;; Redisplay hasn't run yet when a tab is torn off, so the window's tab
  ;; group would otherwise still be the torn-off buffer's and hide the
  ;; buffer that replaced it. A sentinel stands in for the stale group.
  (let ((term (generate-new-buffer "tab-plus-test-term"))
        (torn (generate-new-buffer "tab-plus-test-torn")))
    (unwind-protect
        (save-window-excursion
          (tab-plus-tests--with-view-list
           (list (make-tab-plus-view :name "default" :buffers (list term torn)))
           0
           (with-current-buffer term (text-mode))
           (switch-to-buffer term)
           (switch-to-buffer torn)
           (set-window-parameter nil 'tab-line-group "stale-group")
           ;; Only the explicit resync may update the group here, not the
           ;; hook that redisplay would normally run.
           (let ((window-buffer-change-functions nil)
                 (window-selection-change-functions nil))
             (tab-plus--leave-window torn (selected-window)))
           (should (eq (window-buffer) term))
           (should (equal (window-parameter nil 'tab-line-group)
                          (tab-line-tabs-buffer-group-name term)))))
      (set-window-parameter nil 'tab-line-group nil)
      (kill-buffer term)
      (kill-buffer torn))))

(ert-deftest tab-plus-test/tear-off-buffer-creates-numbered-views ()
  ;; Batch Emacs can't create a second frame, so `make-frame' is stubbed
  ;; to hand back the selected one.
  (let ((one (generate-new-buffer "tab-plus-test-one"))
        (two (generate-new-buffer "tab-plus-test-two")))
    (unwind-protect
        (save-window-excursion
          (tab-plus-tests--with-view-list
           (list (make-tab-plus-view :name "default" :buffers (list one two)))
           0
           (cl-letf (((symbol-function 'make-frame) (lambda (&rest _) (selected-frame)))
                     ((symbol-function 'select-frame-set-input-focus) #'ignore)
                     ((symbol-function 'redisplay) #'ignore)
                     ((symbol-function 'tab-plus--leave-window) #'ignore))
             (tab-plus--tear-off-buffer one (selected-window))
             (should (equal (frame-parameter nil 'tab-plus-tear-off-view) "tear off[1]"))
             (tab-plus--tear-off-buffer two (selected-window))
             (should (equal (frame-parameter nil 'tab-plus-tear-off-view) "tear off[2]")))
           (should (equal (tab-plus-list-views) '("default" "tear off[1]" "tear off[2]")))
           (should (equal (tab-plus-view-buffers (tab-plus--get-view-by-name "tear off[1]"))
                          (list one)))
           (should (equal (tab-plus-view-buffers (tab-plus--get-view-by-name "tear off[2]"))
                          (list two)))
           (should (eql (frame-parameter nil 'tab-line-sel-view) 2))))
      (set-frame-parameter nil 'tab-plus-tear-off-view nil)
      (kill-buffer one)
      (kill-buffer two))))

(ert-deftest tab-plus-test/tear-off-buffer-reuses-lowest-free-number ()
  (let ((buf (generate-new-buffer "tab-plus-test-buf")))
    (unwind-protect
        (save-window-excursion
          (tab-plus-tests--with-view-list
           (list (make-tab-plus-view :name "default")
                 (make-tab-plus-view :name "tear off[1]")
                 (make-tab-plus-view :name "tear off[3]"))
           0
           (cl-letf (((symbol-function 'make-frame) (lambda (&rest _) (selected-frame)))
                     ((symbol-function 'select-frame-set-input-focus) #'ignore)
                     ((symbol-function 'redisplay) #'ignore)
                     ((symbol-function 'tab-plus--leave-window) #'ignore))
             (tab-plus--tear-off-buffer buf (selected-window)))
           (should (member "tear off[2]" (tab-plus-list-views)))))
      (set-frame-parameter nil 'tab-plus-tear-off-view nil)
      (kill-buffer buf))))

(ert-deftest tab-plus-test/close-tear-off-view-removes-the-frames-view ()
  (tab-plus-tests--with-view-list
   (list (make-tab-plus-view :name "default")
         (make-tab-plus-view :name "tear off[1]")
         (make-tab-plus-view :name "tear off[2]"))
   0
   (unwind-protect
       (progn
         (set-frame-parameter nil 'tab-plus-tear-off-view "tear off[1]")
         (tab-plus--close-tear-off-view (selected-frame))
         (should (equal (tab-plus-list-views) '("default" "tear off[2]"))))
     (set-frame-parameter nil 'tab-plus-tear-off-view nil))))

(ert-deftest tab-plus-test/close-tear-off-view-ignores-ordinary-frames ()
  (tab-plus-tests--with-view-list
   (list (make-tab-plus-view :name "default")
         (make-tab-plus-view :name "work"))
   0
   (set-frame-parameter nil 'tab-plus-tear-off-view nil)
   (tab-plus--close-tear-off-view (selected-frame))
   (should (equal (tab-plus-list-views) '("default" "work")))))

(ert-deftest tab-plus-test/close-tear-off-view-is-a-delete-frame-hook ()
  (unwind-protect
      (progn
        (tab-plus-mode 1)
        (should (memq 'tab-plus--close-tear-off-view delete-frame-functions)))
    (tab-plus-mode -1)))

(ert-deftest tab-plus-test/mode-installs-and-restores-settings ()
  (let* ((was-on tab-plus-mode)
         ;; init.el turns the mode on; capture the stock settings, not ours.
         (_ (when was-on (tab-plus-mode -1)))
         (old-tabs-fn tab-line-tabs-function)
         (old-skip switch-to-prev-buffer-skip)
         (old-exclude tab-line-exclude-modes)
         (old-global global-tab-line-mode)
         (old-drag (keymap-lookup tab-line-tab-map tab-plus--drag-key nil t)))
    (unwind-protect
        (progn
          (tab-plus-mode 1)
          (should (eq tab-line-tabs-function #'tab-plus--get-tabs))
          (should (eq switch-to-prev-buffer-skip #'tab-plus--skip-foreign-buffer))
          ;; dape modes are appended on top of the configured list.
          (should (cl-subsetp tab-plus-exclude-modes tab-line-exclude-modes))
          (should global-tab-line-mode)
          (should (advice-member-p #'tab-plus--close-tab-advice 'tab-line-close-tab))
          (should (eq (keymap-lookup tab-line-tab-map tab-plus--drag-key)
                      #'tab-plus-mouse-move-tab))
          (tab-plus-mode -1)
          (should (eq tab-line-tabs-function old-tabs-fn))
          (should (eq switch-to-prev-buffer-skip old-skip))
          (should (equal tab-line-exclude-modes old-exclude))
          (should (eq global-tab-line-mode old-global))
          (should-not (advice-member-p #'tab-plus--close-tab-advice 'tab-line-close-tab))
          (should-not (memq 'tab-plus--close-tear-off-view delete-frame-functions))
          (should (eq (keymap-lookup tab-line-tab-map tab-plus--drag-key nil t) old-drag)))
      (tab-plus-mode (if was-on 1 -1)))))

;; `tab-plus--git-state' runs from every tab-line render, so it must only read a
;; cache and start an asynchronous `git status'; the sentinel fills the cache in.
(defun tab-plus-tests--wait-git (buffer)
  "Wait for the git process of BUFFER to finish."
  (let ((deadline (+ (float-time) 10)))
    (while (and (buffer-local-value 'tab-plus--git-process buffer)
                (process-live-p (buffer-local-value 'tab-plus--git-process buffer))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    ;; let the sentinel run
    (accept-process-output nil 0.05)))

(defmacro tab-plus-tests--with-git-repo (&rest body)
  "Run BODY with `repo' bound to a fresh git repository with one committed file."
  (declare (indent 0))
  `(let* ((repo (file-name-as-directory (make-temp-file "tab-plus-git-" t)))
          (default-directory repo))
     (unwind-protect
         (progn
           (dolist (cmd '(("init" "-q") ("config" "user.email" "t@example.com")
                          ("config" "user.name" "t") ("config" "commit.gpgsign" "false")))
             (apply #'call-process "git" nil nil nil cmd))
           (with-temp-file (expand-file-name "a.txt" repo) (insert "one\n"))
           (call-process "git" nil nil nil "add" "a.txt")
           (call-process "git" nil nil nil "commit" "-q" "-m" "init")
           ,@body)
       (delete-directory repo t))))

(defun tab-plus-tests--git-state-of (file)
  "Visit FILE, wait for the async check, and return its git state."
  (let ((buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf
          (setq tab-plus-git-state-cache nil)
          (should (null (tab-plus--git-state buf)))
          (tab-plus-tests--wait-git buf)
          (tab-plus--git-state buf))
      (kill-buffer buf))))

(ert-deftest tab-plus-test/parse-git-status ()
  (should (eq (tab-plus--parse-git-status "") 'up-to-date))
  (should (eq (tab-plus--parse-git-status " M a.txt\n") 'edited))
  (should (eq (tab-plus--parse-git-status "M  a.txt\n") 'edited))
  (should (eq (tab-plus--parse-git-status "A  a.txt\n") 'edited))
  (should (eq (tab-plus--parse-git-status "UU a.txt\n") 'needs-merge))
  (should (eq (tab-plus--parse-git-status "AA a.txt\n") 'needs-merge))
  (should (eq (tab-plus--parse-git-status "DU a.txt\n") 'needs-merge))
  (should (null (tab-plus--parse-git-status "?? a.txt\n")))
  (should (null (tab-plus--parse-git-status "!! a.txt\n"))))

(ert-deftest tab-plus-test/git-state-up-to-date-and-edited ()
  (tab-plus-tests--with-git-repo
    (let ((file (expand-file-name "a.txt" repo)))
      (should (eq (tab-plus-tests--git-state-of file) 'up-to-date))
      (with-temp-file file (insert "two\n"))
      (should (eq (tab-plus-tests--git-state-of file) 'edited)))))

(ert-deftest tab-plus-test/git-state-untracked-is-nil ()
  (tab-plus-tests--with-git-repo
    (let ((file (expand-file-name "new.txt" repo)))
      (with-temp-file file (insert "x\n"))
      (should (null (tab-plus-tests--git-state-of file))))))

(ert-deftest tab-plus-test/git-state-needs-merge-on-conflict ()
  (tab-plus-tests--with-git-repo
    (let ((file (expand-file-name "a.txt" repo))
          (base (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
      (call-process "git" nil nil nil "checkout" "-q" "-b" "other")
      (with-temp-file file (insert "other\n"))
      (call-process "git" nil nil nil "commit" "-q" "-am" "other")
      (call-process "git" nil nil nil "checkout" "-q" base)
      (with-temp-file file (insert "base\n"))
      (call-process "git" nil nil nil "commit" "-q" "-am" "base")
      (call-process "git" nil nil nil "merge" "-q" "other")
      (should (eq (tab-plus-tests--git-state-of file) 'needs-merge)))))

(ert-deftest tab-plus-test/git-state-nil-outside-repo ()
  (let* ((dir (file-name-as-directory (make-temp-file "tab-plus-norepo-" t)))
         (file (expand-file-name "a.txt" dir))
         (process-environment (cons "GIT_CEILING_DIRECTORIES=/" process-environment)))
    (unwind-protect
        (progn (with-temp-file file (insert "x\n"))
               (should (null (tab-plus-tests--git-state-of file))))
      (delete-directory dir t))))

(ert-deftest tab-plus-test/git-state-nil-when-directory-deleted ()
  (let ((dir (make-temp-file "tab-plus-tests-" t)))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "a.txt" dir))
          (setq default-directory (file-name-as-directory dir))
          (delete-directory dir t)
          (should (null (tab-plus--git-state (current-buffer))))
          (should (null tab-plus--git-process)))
      (when (file-exists-p dir) (delete-directory dir t)))))

(ert-deftest tab-plus-test/git-state-cached-for-ttl ()
  (let ((starts 0))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "a.txt" temporary-file-directory))
      (setq tab-plus-git-state-cache (cons 'edited (float-time)))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _) (cl-incf starts) nil)))
        (should (eq (tab-plus--git-state (current-buffer)) 'edited))
        (should (= starts 0))
        ;; Stale entries are still returned while a refresh starts.
        (setq tab-plus-git-state-cache
              (cons 'edited (- (float-time) tab-plus-git-state-cache-ttl 1)))
        (should (eq (tab-plus--git-state (current-buffer)) 'edited))
        (should (= starts 1))))))

(ert-deftest tab-plus-test/git-refresh-after-save-updates-state ()
  (tab-plus-tests--with-git-repo
    (let* ((file (expand-file-name "a.txt" repo))
           (buf (find-file-noselect file)))
      (unwind-protect
          (with-current-buffer buf
            (setq tab-plus-git-state-cache nil)
            (tab-plus--git-state buf)
            (tab-plus-tests--wait-git buf)
            (should (eq (car tab-plus-git-state-cache) 'up-to-date))
            (add-hook 'after-save-hook #'tab-plus--git-refresh-after-save nil t)
            (erase-buffer) (insert "changed\n")
            (save-buffer)
            (should (process-live-p tab-plus--git-process))
            (tab-plus-tests--wait-git buf)
            (should (eq (car tab-plus-git-state-cache) 'edited)))
        (kill-buffer buf)))))

(ert-deftest tab-plus-test/git-sentinel-ignores-superseded-process ()
  (with-temp-buffer
    (let ((buf (current-buffer))
          (old (make-pipe-process :name "tab-plus-test-old" :noquery t))
          (new (make-pipe-process :name "tab-plus-test-new" :noquery t)))
      (unwind-protect
          (progn
            (setq tab-plus-git-state-cache (cons 'edited 0)
                  tab-plus--git-process new)
            (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil))
                      ((symbol-function 'process-buffer) (lambda (_) nil)))
              (tab-plus--git-sentinel old buf))
            (should (eq (car tab-plus-git-state-cache) 'edited))
            (should (eq tab-plus--git-process new)))
        (delete-process old) (delete-process new)))))

(ert-deftest tab-plus-test/git-sentinel-redraws-only-on-change ()
  (let ((redraws 0))
    (with-temp-buffer
      (let ((buf (current-buffer))
            (proc (make-pipe-process :name "tab-plus-test-dummy" :noquery t)))
        (unwind-protect
            (cl-letf (((symbol-function 'force-mode-line-update)
                       (lambda (&rest _) (cl-incf redraws)))
                      ((symbol-function 'process-live-p) (lambda (_) nil))
                      ((symbol-function 'process-status) (lambda (_) 'exit))
                      ((symbol-function 'process-exit-status) (lambda (_) 0)))
              (setq tab-plus-git-state-cache nil
                    tab-plus--git-process proc)
              (let ((pbuf (generate-new-buffer " *t*" t)))
                (with-current-buffer pbuf (insert " M a.txt\n"))
                (cl-letf (((symbol-function 'process-buffer) (lambda (_) pbuf)))
                  (tab-plus--git-sentinel proc buf)))
              (should (eq (car tab-plus-git-state-cache) 'edited))
              (should (= redraws 1))
              (setq tab-plus--git-process proc)
              (let ((pbuf (generate-new-buffer " *t*" t)))
                (with-current-buffer pbuf (insert " M a.txt\n"))
                (cl-letf (((symbol-function 'process-buffer) (lambda (_) pbuf)))
                  (tab-plus--git-sentinel proc buf)))
              (should (= redraws 1)))
          (delete-process proc))))))

(provide 'tab-plus-tests)
