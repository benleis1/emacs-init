;; -*- lexical-binding: t; -*-
;; ERT tests that every stock menu entry manipulated by menu-config.el exists in a pristine Emacs.
;; menu-config.el removes and moves entries by key and `define-key' silently ignores keys that
;; are absent, so an Emacs upgrade that renames one would leave a stale item with no error.
;;
;; The loaded config has already rearranged its menus, so the check runs in a separate
;; `emacs -Q --batch' process that sees the stock menus.  When adding a key to a removal list or
;; anchor in menu-config.el, add it to `menu-config-tests--stock-keys' as well.
(require 'ert)

(defconst menu-config-tests--stock-keys
  `(((menu-bar file)
     new-file insert-file recover-session Open\ Recent open-file project-open-file
     separator-window separator-tab separator-frame
     new-window-below new-window-on-right one-window
     make-frame make-frame-on-display make-frame-on-monitor delete-this-frame
     undelete-last-deleted-frame undelete-frame-mode make-tab close-tab)
    ((menu-bar buffer)
     next-buffer previous-buffer)
    ((menu-bar edit)
     mark-whole-buffer separator-search ,@(when (featurep 'ns) '(spell)) fill search
     execute-extended-command)
    ((menu-bar tools)
     rgrep grep games separator-games gnus rmail compose-mail directory-search browse-web
     separator-net encryption-decryption separator-encryption-decryption ede semantic
     shell-commands project)
    ((menu-bar tools project)
     project-or-external-find-regexp project-find-regexp))
  "Alist of (MENU-PATH . KEYS): stock entries menu-config.el removes, replaces or anchors on.\nThe Edit menu has a `spell' entry on NS builds only.")

(defconst menu-config-tests--stock-text-labels
  '("Center Line" "Center Region" "Center Paragraph" "Paragraph Indent" "Auto Fill")
  "Labels of `text-mode-menu' entries that menu-config.el removes or anchors on.")

(defun menu-config-tests--stock-missing ()
  "Return the stock menu entries that menu-config.el needs but a pristine Emacs lacks."
  (let* ((form
          `(progn
             (require 'recentf)
             (recentf-mode 1)
             (require 'text-mode)
             (let (missing)
               (dolist (spec ',menu-config-tests--stock-keys)
                 (let ((map (lookup-key global-map (vconcat (car spec)))))
                   (dolist (key (cdr spec))
                     (unless (and (keymapp map) (assq key (cdr map)))
                       (push (format "%S %S" (car spec) key) missing)))))
               (dolist (label ',menu-config-tests--stock-text-labels)
                 (unless (seq-some
                          (lambda (entry)
                            (and (consp entry)
                                 (equal label (if (eq (cadr entry) 'menu-item)
                                                  (nth 2 entry)
                                                (and (stringp (cadr entry)) (cadr entry))))))
                          (cdr text-mode-menu))
                   (push (format "text-mode-menu %S" label) missing)))
               (unless (assq nil (cdr text-mode-menu))
                 (push "text-mode-menu separator" missing))
               (princ (format "\nMISSING:%S\n" (nreverse missing))))))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (call-process emacs nil t nil "-Q" "--batch" "--eval" (prin1-to-string form))
      (goto-char (point-min))
      (if (re-search-forward "^MISSING:\\(.*\\)$" nil t)
          (car (read-from-string (match-string 1)))
        (list (concat "checker did not report: " (buffer-string)))))))

(ert-deftest menu-config-test/stock-menu-entries-exist ()
  (should-not (menu-config-tests--stock-missing)))

(defconst menu-config-tests--library
  (expand-file-name "menu-config.el" (file-name-directory (or load-file-name buffer-file-name)))
  "Path of menu-config.el, resolved when this file is loaded.")

(defun menu-config-tests--run-batch (form)
  "Evaluate FORM in a pristine `emacs -Q --batch' that has loaded menu-config.el.
Return the text FORM prints after a RESULT: marker."
  (let ((emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (call-process emacs nil t nil "-Q" "--batch" "--load" menu-config-tests--library
                    "--eval" (prin1-to-string `(progn (require 'recentf) (recentf-mode 1)
                                                      (my-menu-config)
                                                      ,form)))
      (goto-char (point-min))
      (if (re-search-forward "^RESULT:\\(.*\\)$" nil t)
          (car (read-from-string (match-string 1)))
        (list 'no-result (buffer-string))))))

(ert-deftest menu-config-test/falls-back-without-consult-and-ghostel ()
  (should (equal (menu-config-tests--run-batch
                  '(princ (format "\nRESULT:%S\n"
                                  (list (and (assq 'Open\ Recent (cdr (lookup-key global-map [menu-bar file]))) t)
                                        (and (assq 'rgrep (cdr (lookup-key global-map [menu-bar tools]))) t)
                                        (lookup-key global-map [menu-bar buffer select-named-buffer])
                                        (lookup-key global-map [menu-bar tools ghostel])
                                        (lookup-key global-map [menu-bar edit search project-search])))))
                 '(t t switch-to-buffer nil project-find-regexp))))

(ert-deftest menu-config-test/help-menu-commands-exist ()
  (should-not
   (menu-config-tests--run-batch
    '(let (bad)
       (require 'cl-lib)
       (cl-labels ((walk (map)
                     (dolist (e (cdr map))
                       (when (and (consp e) (not (stringp e)))
                         (let* ((b (cdr e))
                                (cmd (cond ((eq (car-safe b) 'menu-item) (nth 2 b))
                                           ((stringp (car-safe b)) (cdr b))
                                           (t b))))
                           (cond ((keymapp cmd) (walk cmd))
                                 ((and cmd (not (commandp cmd))) (push cmd bad))))))))
         (walk menu-bar-help-menu))
       (princ (format "\nRESULT:%S\n" (nreverse bad)))))))

(ert-deftest menu-config-test/help-menu-is-the-stock-keymap ()
  (should (equal (menu-config-tests--run-batch
                  '(princ (format "\nRESULT:%S\n"
                                  (eq menu-bar-help-menu (lookup-key global-map [menu-bar help-menu])))))
                 t)))
