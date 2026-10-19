;;; config-diagnostics.el --- Check this config's external dependencies -*- lexical-binding: t -*-

;;; Commentary:

;; `M-x my-config-diagnostics' reports whether the executables, fonts and
;; grammars the config relies on are available.  This file has no dependency on
;; the rest of the config (or elpaca), so install.sh can run it standalone:
;;
;;   emacs --batch -l lisp/config-diagnostics.el -f my-config-diagnostics
;;
;; Checks that need the running config (the GUI font, `my-java-home', the dape
;; Java paths) are skipped in batch mode.

;;; Code:

(require 'seq)
(require 'subr-x)

(defvar my-default-fixed-pitch-font)
(defvar my-java-home)
(defvar dape-java-ms-debug-plugin)
(defvar dape-java-test-bundles-dir)

;;;###autoload
(defun my-config-diagnostics ()
  "Check the external dependencies this config relies on and show the status.
Each line is OK, MISSING (required) or WARN (optional feature unavailable).
Interactively the results go to a buffer; in batch mode they are printed to
stdout and the checks that need the running config are skipped."
  (interactive)
  (let* ((exe (lambda (&rest names) (seq-some #'executable-find names)))
         (jar-count (lambda (dir)
                      (and (file-directory-p dir)
                           (length (directory-files dir nil "\\.jar\\'")))))
         (java-bin (and (boundp 'my-java-home)
                        (expand-file-name "bin/java" my-java-home)))
         (debug-plugin (and (boundp 'dape-java-ms-debug-plugin) dape-java-ms-debug-plugin))
         (bundles-dir (and (boundp 'dape-java-test-bundles-dir) dape-java-test-bundles-dir))
         ;; Each entry: (LABEL REQUIRED-P DETAIL-OR-NIL-IF-MISSING HINT)
         (checks
          (append
           (delq
            nil
            (list
             (list "Emacs >= 30" t
                   (and (>= emacs-major-version 30) emacs-version)
                   "install Emacs 30 or later")
             (list "git" t (funcall exe "git")
                   "elpaca needs it to clone packages")
             (unless noninteractive
               (list (format "Font %s" my-default-fixed-pitch-font) t
                     (cond ((not (display-graphic-p)) "skipped (not a GUI frame)")
                           ((find-font (font-spec :family my-default-fixed-pitch-font))
                            "found"))
                     "install the DejaVuSansM Nerd Font (see install.sh)"))
             (list "aspell / ispell" nil (funcall exe "aspell" "ispell")
                   "flyspell needs one of them")
             (list "pg_format" nil (funcall exe "pg_format")
                   "SQL formatting needs pgformatter")
             (list "jdtls" nil (funcall exe "jdtls")
                   "eglot Java support needs jdtls on PATH")
             (unless noninteractive
               (list (format "my-java-home (%s)" my-java-home) nil
                     (and java-bin (file-executable-p java-bin) java-bin)
                     "set `my-java-home' to a JDK directory"))
             (unless noninteractive
               (list "dape Java debug plugin jar" nil
                     (and debug-plugin (file-exists-p debug-plugin) debug-plugin)
                     (if debug-plugin
                         "see `dape-java-ms-debug-plugin'"
                       "dape-java not loaded yet; load dape and re-run")))
             (unless noninteractive
               (list "dape JUnit test bundle jars" nil
                     (let ((n (and bundles-dir (funcall jar-count bundles-dir))))
                       (and n (> n 0) (format "%d jars in %s" n bundles-dir)))
                     (if bundles-dir
                         "run M-x dape-java-fetch-test-bundle"
                       "dape-java not loaded yet; load dape and re-run")))
             (list "unzip" nil (funcall exe "unzip")
                   "dape-java-fetch-test-bundle needs it")
             (list "tree-sitter Java grammar" nil
                   (and (treesit-available-p) (treesit-language-available-p 'java)
                        "installed")
                   "run M-x treesit-install-language-grammar RET java")
             (list "Dynamic module support (for ghostel)" nil
                   (and (bound-and-true-p module-file-suffix) module-file-suffix)
                   "ghostel needs Emacs built with --with-modules")
             (list "C compiler (for tree-sitter grammars)" nil
                   (funcall exe "cc" "gcc" "clang")
                   "needed to build tree-sitter grammars")
             (list "Python language server" nil
                   (funcall exe "pylsp" "pyright-langserver" "jedi-language-server"
                            "ruff" "ty")
                   "pip install python-lsp-server (or pyright)")))
           (pcase system-type
             ('darwin
              (list (list "pbcopy" nil (funcall exe "pbcopy")
                          "terminal clipboard support")
                    (list "afplay" nil (funcall exe "afplay")
                          "appointment sound")))
             ('gnu/linux
              (list (list "wl-copy" nil (funcall exe "wl-copy")
                          "terminal clipboard support")
                    (list "pw-play / paplay" nil (funcall exe "pw-play" "paplay")
                          "appointment sound")))))))
    (if noninteractive
        (progn
          (princ "Config diagnostics\n\n")
          (dolist (check checks)
            (pcase-let ((`(,label ,required ,found ,hint) check))
              (princ (format "%s %s%s\n"
                             (cond (found "[ OK    ]")
                                   (required "[MISSING]")
                                   (t "[WARN   ]"))
                             label
                             (if found (format "  -> %s" found) (format "  (%s)" hint)))))))
      (with-current-buffer (get-buffer-create "*my-config-diagnostics*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "Config diagnostics\n\n" 'face 'bold))
          (dolist (check checks)
            (pcase-let ((`(,label ,required ,found ,hint) check))
              (insert (if found
                          (propertize "[ OK    ]" 'face 'success)
                        (if required
                            (propertize "[MISSING]" 'face 'error)
                          (propertize "[WARN   ]" 'face 'warning)))
                      " " label
                      (if found (format "  -> %s" found) (format "  (%s)" hint))
                      "\n")))
          (goto-char (point-min))
          (special-mode))
        (display-buffer (current-buffer))))))

(provide 'config-diagnostics)
;;; config-diagnostics.el ends here
