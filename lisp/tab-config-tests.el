;; -*- lexical-binding: t; -*-
;; ERT tests for the pure/deterministic pieces of tab-config.el: the
;; view list bookkeeping (`tab2-views'), the generic `find-first'
;; helper, the buffer/group filtering predicates, and the
;; persist-format round trip. Anything that renders an actual tab
;; line, moves real windows around, or depends on `vc'/project state
;; (`tab2-get-tabs', `tab2-format-tab', the mouse-drag commands, ...)
;; is out of scope here.
(require 'ert)
(require 'cl-lib)
(require 'tab-line)

;; In the normal CI flow (see .github/workflows/test.yml) `init.el' is
;; loaded first, which loads `tab-config.el' itself -- so the
;; functions under test are already defined by the time this file
;; loads. This fallback just makes the file independently runnable too
;; (e.g. `emacs -Q --batch -l ert -l tab-config-tests.el -f
;; ert-run-tests-batch-and-exit'), since `tab-config.el' has no
;; `provide' of its own to key a `require' off of.
(unless (fboundp 'find-first)
  (load (expand-file-name "tab-config.el"
                           (file-name-directory
                            (or load-file-name buffer-file-name default-directory)))))

;; `tab2-views'/the frame's `tab-line-sel-view' parameter are global
;; state that the real tab line reads and mutates continuously --
;; every test below runs against a throwaway view list instead of the
;; live one, restoring both afterwards so it can't bleed into other
;; tests (in either direction).
(defmacro tab-config-tests--with-view-list (views start-pos &rest body)
  "Run BODY with `tab2-views' bound to VIEWS and the selected frame's
`tab-line-sel-view' parameter set to START-POS, restoring both
afterwards."
  (declare (indent 2))
  `(let ((tab-config-tests--old-views tab2-views)
         (tab-config-tests--old-sel (frame-parameter nil 'tab-line-sel-view)))
     (unwind-protect
         (progn
           (setq tab2-views ,views)
           (set-frame-parameter nil 'tab-line-sel-view ,start-pos)
           ,@body)
       (setq tab2-views tab-config-tests--old-views)
       (set-frame-parameter nil 'tab-line-sel-view tab-config-tests--old-sel))))

;;; `find-first'

(ert-deftest tab-config-test/find-first-returns-first-match ()
  (should (equal (find-first (lambda (x) (> x 2)) '(1 2 3 4)) 3)))

(ert-deftest tab-config-test/find-first-nil-when-no-match ()
  (should (null (find-first (lambda (x) (> x 10)) '(1 2 3)))))

(ert-deftest tab-config-test/find-first-nil-on-empty-list ()
  (should (null (find-first #'identity nil))))

;;; View list bookkeeping: `tab2-get-view-by-name', `tab2-list-views',
;;; `tab2-num-views'

(ert-deftest tab-config-test/get-view-by-name-finds-match ()
  (tab-config-tests--with-view-list
      (list (make-tab2-view :name "default") (make-tab2-view :name "work")) 0
    (should (equal (tab2-view-name (tab2-get-view-by-name "work")) "work"))))

(ert-deftest tab-config-test/get-view-by-name-nil-when-absent ()
  (tab-config-tests--with-view-list (list (make-tab2-view :name "default")) 0
    (should (null (tab2-get-view-by-name "nope")))))

(ert-deftest tab-config-test/list-views-returns-names-in-order ()
  (tab-config-tests--with-view-list
      (list (make-tab2-view :name "default") (make-tab2-view :name "work")
            (make-tab2-view :name "play"))
      0
    (should (equal (tab2-list-views) '("default" "work" "play")))))

(ert-deftest tab-config-test/num-views-counts-views ()
  (tab-config-tests--with-view-list
      (list (make-tab2-view :name "default") (make-tab2-view :name "work")) 0
    (should (eql (tab2-num-views) 2))))

;;; `tab2-new-view'

(ert-deftest tab-config-test/new-view-errors-on-duplicate-name ()
  (tab-config-tests--with-view-list (list (make-tab2-view :name "default")) 0
    (should-error (tab2-new-view "default"))))

;;; `tab2-close-view-by-name'

(ert-deftest tab-config-test/close-view-errors-on-default ()
  (tab-config-tests--with-view-list (list (make-tab2-view :name "default")) 0
    (should-error (tab2-close-view-by-name "default"))))

(ert-deftest tab-config-test/close-view-errors-when-name-not-found ()
  (tab-config-tests--with-view-list (list (make-tab2-view :name "default")) 0
    (should-error (tab2-close-view-by-name "nope"))))

;;; `tab2-next-view'/`tab2-prev-view' wraparound

(ert-deftest tab-config-test/next-view-wraps-from-last-to-first ()
  (tab-config-tests--with-view-list
      (list (make-tab2-view :name "default") (make-tab2-view :name "work")) 1
    (tab2-next-view)
    (should (eql (frame-parameter nil 'tab-line-sel-view) 0))))

(ert-deftest tab-config-test/prev-view-wraps-from-first-to-last ()
  (tab-config-tests--with-view-list
      (list (make-tab2-view :name "default") (make-tab2-view :name "work")) 0
    (tab2-prev-view)
    (should (eql (frame-parameter nil 'tab-line-sel-view) 1))))

;;; `tab2-buffer-mode'/`tab2-buffer-filter'

(ert-deftest tab-config-test/buffer-mode-returns-major-mode ()
  (with-temp-buffer
    (text-mode)
    (should (eq (tab2-buffer-mode (current-buffer)) 'text-mode))))

(ert-deftest tab-config-test/buffer-filter-true-for-file-buffer ()
  (let* ((file (make-temp-file "tab-config-test-")))
    (unwind-protect
        (let ((buf (find-file-noselect file)))
          (unwind-protect
              (should (tab2-buffer-filter buf))
            (kill-buffer buf)))
      (delete-file file))))

(ert-deftest tab-config-test/buffer-filter-true-for-whitelisted-mode ()
  (with-temp-buffer
    (text-mode)
    (should (tab2-buffer-filter (current-buffer)))))

(ert-deftest tab-config-test/buffer-filter-false-otherwise ()
  (with-temp-buffer
    (fundamental-mode)
    (should-not (tab2-buffer-filter (current-buffer)))))

;;; `tab2-filter-buffers-by-group'

(ert-deftest tab-config-test/filter-buffers-by-group-nil-group-passes-through ()
  (with-temp-buffer
    (let ((bufs (list (current-buffer))))
      (should (equal (tab2-filter-buffers-by-group bufs nil) bufs)))))

(ert-deftest tab-config-test/filter-buffers-by-group-files-keeps-only-file-buffers ()
  (let* ((file (make-temp-file "tab-config-test-")))
    (unwind-protect
        (let ((file-buf (find-file-noselect file)))
          (unwind-protect
              (with-temp-buffer
                (let ((no-file-buf (current-buffer)))
                  (should (equal (tab2-filter-buffers-by-group
                                  (list file-buf no-file-buf) "Files")
                                 (list file-buf)))))
            (kill-buffer file-buf)))
      (delete-file file))))

(ert-deftest tab-config-test/filter-buffers-by-group-modified-keeps-only-modified ()
  (let* ((file (make-temp-file "tab-config-test-")))
    (unwind-protect
        (let ((file-buf (find-file-noselect file)))
          (unwind-protect
              (progn
                (with-current-buffer file-buf (insert "x"))
                (with-temp-buffer
                  (let ((unmodified-buf (current-buffer)))
                    (should (equal (tab2-filter-buffers-by-group
                                    (list file-buf unmodified-buf) "Modified")
                                   (list file-buf))))))
            (with-current-buffer file-buf (set-buffer-modified-p nil))
            (kill-buffer file-buf)))
      (delete-file file))))

;;; `tab2-convert-to-persist-format'/`tab2-convert-from-persist-format'

(ert-deftest tab-config-test/persist-format-round-trips-name-and-buffers ()
  (with-temp-buffer
    (rename-buffer "tab-config-test-persist-buf" t)
    (let* ((buf (current-buffer))
           (view (make-tab2-view :name "work" :buffers (list buf)))
           (persisted (tab2-convert-to-persist-format view))
           (restored (tab2-convert-from-persist-format persisted)))
      (should (equal (tab2-persist-view-name persisted) "work"))
      (should (equal (tab2-persist-view-buffernames persisted)
                      '("tab-config-test-persist-buf")))
      (should (equal (tab2-view-name restored) "work"))
      (should (equal (tab2-view-buffers restored) (list buf))))))

(provide 'tab-config-tests)
